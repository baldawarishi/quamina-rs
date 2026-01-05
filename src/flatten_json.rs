//! Streaming JSON flattener with segment-based field skipping.
//!
//! This is a port of Go's flattenJSON which provides significant performance
//! improvements by:
//! - Only parsing fields that appear in patterns (using SegmentsTree)
//! - Early termination when all needed fields are found
//! - Zero-copy field values as slices of the original event bytes
//! - Reusable state with reset() pattern (like Go's flattenJSON)

use crate::segments_tree::SegmentsTree;
use crate::QuaminaError;

/// Represents a field's position within an array in the event.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayPos {
    pub array: i32,
    pub pos: i32,
}

/// A flattened field from a JSON event.
#[derive(Clone, Debug)]
pub struct Field<'a> {
    /// Full path (e.g., "context\nuser\nid") - owned because it comes from SegmentsTree
    pub path: Vec<u8>,
    /// Value bytes from the event
    pub val: FieldValue<'a>,
    /// Array position tracking
    pub array_trail: Vec<ArrayPos>,
    /// True if the value is a JSON number
    pub is_number: bool,
}

/// Field value - either a slice of the original event or an owned string
/// for values containing escape sequences.
#[derive(Clone, Debug)]
pub enum FieldValue<'a> {
    /// Borrowed slice from original event (zero-copy)
    Borrowed(&'a [u8]),
    /// Owned bytes (for escaped strings)
    Owned(Vec<u8>),
}

/// Member name - either borrowed or owned if it contains escapes.
enum MemberName<'a> {
    Borrowed(&'a [u8]),
    Owned(Vec<u8>),
}

impl MemberName<'_> {
    fn as_bytes(&self) -> &[u8] {
        match self {
            MemberName::Borrowed(b) => b,
            MemberName::Owned(v) => v.as_slice(),
        }
    }
}

impl FieldValue<'_> {
    pub fn as_bytes(&self) -> &[u8] {
        match self {
            FieldValue::Borrowed(s) => s,
            FieldValue::Owned(v) => v,
        }
    }
}

/// Reusable JSON flattener state.
///
/// This struct holds the working buffers that can be reused across multiple
/// flatten calls, following Go's reset() pattern for reduced allocations.
/// The fields capacity is tracked and grows as needed.
pub struct FlattenJsonState {
    /// Working array position trail (reused between calls)
    array_trail: Vec<ArrayPos>,
    /// Typical fields count (learned from previous calls for pre-allocation)
    fields_hint: usize,
}

impl Default for FlattenJsonState {
    fn default() -> Self {
        Self::new()
    }
}

impl FlattenJsonState {
    /// Create a new reusable flattener state.
    pub fn new() -> Self {
        Self {
            array_trail: Vec::with_capacity(8),
            fields_hint: 32,
        }
    }

    /// Reset internal state for reuse.
    #[inline]
    fn reset(&mut self) {
        self.array_trail.clear();
    }

    /// Flatten an event using this reusable state.
    ///
    /// This is the primary API for high-performance event processing.
    /// The state is automatically reset before each call.
    pub fn flatten<'a>(
        &mut self,
        event: &'a [u8],
        tree: &SegmentsTree,
    ) -> Result<Vec<Field<'a>>, QuaminaError> {
        self.reset();

        let mut ctx = FlattenContext {
            event,
            index: 0,
            fields: Vec::with_capacity(self.fields_hint),
            skipping: 0,
            array_trail: &mut self.array_trail,
            array_count: 0,
        };

        let result = ctx.flatten_impl(tree);

        // Update hint for next call
        self.fields_hint = self.fields_hint.max(ctx.fields.capacity());

        result.map(|()| ctx.fields)
    }
}

/// Internal context for a single flatten operation.
/// Borrows the reusable array_trail from FlattenJsonState.
struct FlattenContext<'a, 'b> {
    event: &'a [u8],
    index: usize,
    fields: Vec<Field<'a>>,
    skipping: i32,
    array_trail: &'b mut Vec<ArrayPos>,
    array_count: i32,
}

impl<'a> FlattenContext<'a, '_> {
    fn flatten_impl(&mut self, tree: &SegmentsTree) -> Result<(), QuaminaError> {
        if self.event.is_empty() {
            return Err(QuaminaError::InvalidJson("empty event".into()));
        }

        // Find the opening brace
        loop {
            let ch = self.ch();
            if ch == b'{' {
                match self.read_object(tree) {
                    Ok(()) => {}
                    Err(FlattenError::EarlyStop) => return Ok(()),
                    Err(FlattenError::Error(e)) => return Err(e),
                }
                // Eat trailing whitespace
                self.index += 1;
                while self.index < self.event.len() {
                    let ch = self.event[self.index];
                    if !is_whitespace(ch) {
                        return Err(self.error(&format!(
                            "garbage char '{}' after top-level object",
                            ch as char
                        )));
                    }
                    self.index += 1;
                }
                return Ok(());
            } else if is_whitespace(ch) {
                self.index += 1;
                if self.index >= self.event.len() {
                    return Err(self.error("unexpected end of event"));
                }
            } else {
                return Err(self.error("not a JSON object"));
            }
        }
    }

    /// Read a JSON object, recursing into nested objects as needed.
    fn read_object(&mut self, tree: &SegmentsTree) -> Result<(), FlattenError> {
        // index points at {
        self.step()?;

        let mut fields_count = tree.fields_count();
        let mut nodes_count = tree.nodes_count();

        // Snapshot array trail for object member fields
        let array_trail = if self.skipping == 0 {
            self.array_trail.clone()
        } else {
            Vec::new()
        };

        let mut member_name: MemberName<'a> = MemberName::Borrowed(&[]);
        let mut member_is_used = false;
        let mut state = ObjectState::InObject;

        loop {
            // Early termination: all needed fields found
            if nodes_count == 0 && fields_count == 0 {
                if tree.is_root() {
                    return Err(FlattenError::EarlyStop);
                } else {
                    return self.leave_object();
                }
            }

            let ch = self.ch();

            match state {
                ObjectState::InObject => {
                    if is_whitespace(ch) {
                        // skip
                    } else if ch == b'"' {
                        member_name = self.read_member_name()?;
                        member_is_used =
                            self.skipping == 0 && tree.is_segment_used(member_name.as_bytes());
                        state = ObjectState::SeekingColon;
                    } else if ch == b'}' {
                        return Ok(());
                    } else {
                        return Err(FlattenError::Error(self.error(&format!(
                            "illegal character '{}' in JSON object",
                            ch as char
                        ))));
                    }
                }

                ObjectState::SeekingColon => {
                    if is_whitespace(ch) {
                        // skip
                    } else if ch == b':' {
                        state = ObjectState::MemberValue;
                    } else {
                        return Err(FlattenError::Error(self.error(&format!(
                            "illegal character '{}' while looking for colon",
                            ch as char
                        ))));
                    }
                }

                ObjectState::MemberValue => {
                    // Skip whitespace before value
                    let mut ch = ch;
                    while is_whitespace(ch) {
                        self.step()?;
                        ch = self.ch();
                    }

                    let mut val: Option<FieldValue<'a>> = None;
                    let mut is_number = false;
                    let mut is_leaf = false;

                    match ch {
                        b'"' => {
                            if self.skipping > 0 || !member_is_used {
                                self.skip_string_value()?;
                            } else {
                                val = Some(self.read_string_value()?);
                            }
                            is_leaf = true;
                        }
                        b't' => {
                            self.read_literal(b"true")?;
                            if self.skipping == 0 && member_is_used {
                                val = Some(FieldValue::Borrowed(b"true"));
                            }
                            is_leaf = true;
                        }
                        b'f' => {
                            self.read_literal(b"false")?;
                            if self.skipping == 0 && member_is_used {
                                val = Some(FieldValue::Borrowed(b"false"));
                            }
                            is_leaf = true;
                        }
                        b'n' => {
                            self.read_literal(b"null")?;
                            if self.skipping == 0 && member_is_used {
                                val = Some(FieldValue::Borrowed(b"null"));
                            }
                            is_leaf = true;
                        }
                        b'-' | b'0'..=b'9' => {
                            let num_val = self.read_number()?;
                            if self.skipping == 0 && member_is_used {
                                val = Some(num_val);
                                is_number = true;
                            }
                            is_leaf = true;
                        }
                        b'[' => {
                            let segment_used = tree.is_segment_used(member_name.as_bytes());
                            if !segment_used {
                                self.skipping += 1;
                            }

                            if self.skipping > 0 || !member_is_used {
                                self.skip_block(b'[', b']')?;
                            } else {
                                let array_tree = tree.get(member_name.as_bytes()).unwrap_or(tree);
                                let path = tree
                                    .path_for_segment(member_name.as_bytes())
                                    .map(|p| p.to_vec());
                                self.read_array(path, array_tree)?;
                            }

                            if !segment_used {
                                self.skipping -= 1;
                            }
                        }
                        b'{' => {
                            let segment_used = tree.is_segment_used(member_name.as_bytes());
                            if !segment_used {
                                self.skipping += 1;
                            }

                            if self.skipping > 0 || !member_is_used {
                                self.skip_block(b'{', b'}')?;
                            } else if let Some(child_tree) = tree.get(member_name.as_bytes()) {
                                nodes_count = nodes_count.saturating_sub(1);
                                self.read_object(child_tree)?;
                            } else {
                                // No child tree - skip the block
                                self.skip_block(b'{', b'}')?;
                            }

                            if !segment_used {
                                self.skipping -= 1;
                            }
                        }
                        _ => {
                            return Err(FlattenError::Error(self.error(&format!(
                                "illegal character '{}' after field name",
                                ch as char
                            ))));
                        }
                    }

                    if is_leaf {
                        if let Some(v) = val {
                            if member_is_used {
                                if let Some(path) = tree.path_for_segment(member_name.as_bytes()) {
                                    self.fields.push(Field {
                                        path: path.to_vec(),
                                        val: v,
                                        array_trail: array_trail.clone(),
                                        is_number,
                                    });
                                    fields_count = fields_count.saturating_sub(1);
                                }
                            }
                        }
                    }

                    state = ObjectState::AfterValue;
                }

                ObjectState::AfterValue => {
                    if is_whitespace(ch) {
                        // skip
                    } else if ch == b',' {
                        state = ObjectState::InObject;
                    } else if ch == b'}' {
                        return Ok(());
                    } else {
                        return Err(FlattenError::Error(
                            self.error(&format!("illegal character '{}' in object", ch as char)),
                        ));
                    }
                }
            }

            self.step()?;
        }
    }

    /// Read a JSON array, recursing into elements as needed.
    fn read_array(
        &mut self,
        path: Option<Vec<u8>>,
        tree: &SegmentsTree,
    ) -> Result<(), FlattenError> {
        // index points at [
        self.step()?;

        if self.skipping == 0 {
            self.enter_array();
        }

        let mut state = ArrayState::InArray;

        loop {
            let mut ch = self.ch();

            match state {
                ArrayState::InArray => {
                    // Skip whitespace
                    while is_whitespace(ch) {
                        self.step()?;
                        ch = self.ch();
                    }

                    let mut val: Option<FieldValue<'a>> = None;
                    let mut is_number = false;
                    let mut is_leaf = false;

                    match ch {
                        b'"' => {
                            val = Some(self.read_string_value()?);
                            is_leaf = true;
                        }
                        b't' => {
                            self.read_literal(b"true")?;
                            val = Some(FieldValue::Borrowed(b"true"));
                            is_leaf = true;
                        }
                        b'f' => {
                            self.read_literal(b"false")?;
                            val = Some(FieldValue::Borrowed(b"false"));
                            is_leaf = true;
                        }
                        b'n' => {
                            self.read_literal(b"null")?;
                            val = Some(FieldValue::Borrowed(b"null"));
                            is_leaf = true;
                        }
                        b'-' | b'0'..=b'9' => {
                            val = Some(self.read_number()?);
                            is_number = true;
                            is_leaf = true;
                        }
                        b'{' => {
                            if self.skipping == 0 {
                                self.step_array_element();
                            }
                            self.read_object(tree)?;
                        }
                        b'[' => {
                            if self.skipping == 0 {
                                self.step_array_element();
                            }
                            self.read_array(path.clone(), tree)?;
                        }
                        b']' => {
                            if self.skipping == 0 {
                                self.leave_array();
                            }
                            return Ok(());
                        }
                        _ => {
                            return Err(FlattenError::Error(
                                self.error(&format!("illegal character '{}' in array", ch as char)),
                            ));
                        }
                    }

                    if is_leaf {
                        if let Some(v) = val {
                            if self.skipping == 0 {
                                self.step_array_element();
                                if let Some(ref p) = path {
                                    self.fields.push(Field {
                                        path: p.clone(),
                                        val: v,
                                        array_trail: self.array_trail.clone(),
                                        is_number,
                                    });
                                }
                            }
                        }
                    }

                    state = ArrayState::AfterValue;
                }

                ArrayState::AfterValue => {
                    if is_whitespace(ch) {
                        // skip
                    } else if ch == b']' {
                        if self.skipping == 0 {
                            self.leave_array();
                        }
                        return Ok(());
                    } else if ch == b',' {
                        state = ArrayState::InArray;
                    } else {
                        return Err(FlattenError::Error(
                            self.error(&format!("illegal character '{}' in array", ch as char)),
                        ));
                    }
                }
            }

            self.step()?;
        }
    }

    /// Skip remaining content until we exit the current object.
    fn leave_object(&mut self) -> Result<(), FlattenError> {
        while self.index < self.event.len() {
            let ch = self.event[self.index];
            match ch {
                b'"' => self.skip_string_value()?,
                b'{' | b'[' => {
                    let close = if ch == b'{' { b'}' } else { b']' };
                    self.skip_block(ch, close)?;
                }
                b'}' => return Ok(()),
                _ => {}
            }
            self.index += 1;
        }
        Err(FlattenError::Error(self.error("truncated block")))
    }

    /// Skip a block (object or array) quickly without parsing.
    fn skip_block(&mut self, open: u8, close: u8) -> Result<(), FlattenError> {
        let mut level = 0;

        while self.index < self.event.len() {
            let ch = self.event[self.index];

            match ch {
                b'"' => self.skip_string_value()?,
                c if c == open => level += 1,
                c if c == close => {
                    level -= 1;
                    if level == 0 {
                        return Ok(());
                    }
                }
                _ => {}
            }

            self.index += 1;
        }

        Err(FlattenError::Error(self.error("truncated block")))
    }

    /// Skip a string value quickly.
    fn skip_string_value(&mut self) -> Result<(), FlattenError> {
        self.step()?; // skip opening "

        while self.index < self.event.len() {
            let ch = self.event[self.index];

            // Handle escape sequences
            if ch == b'\\' && self.index + 1 < self.event.len() {
                let next = self.event[self.index + 1];
                if next == b'\\' || next == b'"' {
                    self.index += 2;
                    continue;
                }
            }

            if ch == b'"' {
                return Ok(());
            }

            self.index += 1;
        }

        Err(FlattenError::Error(self.error("truncated string")))
    }

    /// Read a member name (the part between quotes).
    /// Returns borrowed bytes if no escapes, or owned decoded bytes if escapes present.
    fn read_member_name(&mut self) -> Result<MemberName<'a>, FlattenError> {
        // Skip opening "
        self.step()?;
        let start = self.index;

        while self.index < self.event.len() {
            let ch = self.event[self.index];
            if ch == b'"' {
                return Ok(MemberName::Borrowed(&self.event[start..self.index]));
            } else if ch == b'\\' {
                // Has escapes - need to decode
                return self.read_member_name_with_escapes(start);
            } else if ch <= 0x1f {
                return Err(FlattenError::Error(
                    self.error(&format!("illegal byte {:02x} in field name", ch)),
                ));
            } else {
                self.index += 1;
            }
        }

        Err(FlattenError::Error(self.error("premature end of event")))
    }

    /// Read a member name that contains escape sequences.
    fn read_member_name_with_escapes(
        &mut self,
        start: usize,
    ) -> Result<MemberName<'a>, FlattenError> {
        let mut name = Vec::new();
        // Copy content before the escape
        name.extend_from_slice(&self.event[start..self.index]);

        while self.index < self.event.len() {
            let ch = self.event[self.index];
            if ch == b'"' {
                return Ok(MemberName::Owned(name));
            } else if ch == b'\\' {
                self.index += 1;
                if self.index >= self.event.len() {
                    return Err(FlattenError::Error(self.error("premature end in escape")));
                }
                let escaped = self.event[self.index];
                match escaped {
                    b'"' => name.push(b'"'),
                    b'\\' => name.push(b'\\'),
                    b'/' => name.push(b'/'),
                    b'b' => name.push(0x08),
                    b'f' => name.push(0x0c),
                    b'n' => name.push(b'\n'),
                    b'r' => name.push(b'\r'),
                    b't' => name.push(b'\t'),
                    b'u' => {
                        self.index += 1;
                        let code = self.read_hex_4()?;
                        if code < 0x80 {
                            name.push(code as u8);
                        } else if code < 0x800 {
                            name.push(0xC0 | ((code >> 6) as u8));
                            name.push(0x80 | ((code & 0x3F) as u8));
                        } else if (0xD800..=0xDBFF).contains(&code) {
                            // High surrogate - check for low surrogate
                            if self.index + 5 < self.event.len()
                                && self.event[self.index] == b'\\'
                                && self.event[self.index + 1] == b'u'
                            {
                                self.index += 2;
                                let low = self.read_hex_4()?;
                                if (0xDC00..=0xDFFF).contains(&low) {
                                    let full = 0x10000 + ((code - 0xD800) << 10) + (low - 0xDC00);
                                    name.push(0xF0 | ((full >> 18) as u8));
                                    name.push(0x80 | (((full >> 12) & 0x3F) as u8));
                                    name.push(0x80 | (((full >> 6) & 0x3F) as u8));
                                    name.push(0x80 | ((full & 0x3F) as u8));
                                    // Don't decrement here - the decrement at end of b'u' case handles it
                                }
                            }
                        } else {
                            name.push(0xE0 | ((code >> 12) as u8));
                            name.push(0x80 | (((code >> 6) & 0x3F) as u8));
                            name.push(0x80 | ((code & 0x3F) as u8));
                        }
                        self.index -= 1;
                    }
                    _ => {
                        return Err(FlattenError::Error(
                            self.error("malformed escape in field name"),
                        ));
                    }
                }
            } else if ch <= 0x1f {
                return Err(FlattenError::Error(
                    self.error(&format!("illegal byte {:02x} in field name", ch)),
                ));
            } else {
                name.push(ch);
            }
            self.index += 1;
        }

        Err(FlattenError::Error(self.error("premature end of event")))
    }

    /// Read a string value (including quotes).
    fn read_string_value(&mut self) -> Result<FieldValue<'a>, FlattenError> {
        let val_start = self.index;
        self.step()?; // skip opening "

        while self.index < self.event.len() {
            let ch = self.event[self.index];
            if ch == b'"' {
                return Ok(FieldValue::Borrowed(&self.event[val_start..=self.index]));
            } else if ch == b'\\' {
                // Has escapes - need to unescape
                return self.read_string_with_escapes(val_start);
            } else if ch <= 0x1f {
                return Err(FlattenError::Error(
                    self.error(&format!("illegal byte {:02x} in string value", ch)),
                ));
            }
            self.index += 1;
        }

        Err(FlattenError::Error(self.error("event truncated in string")))
    }

    /// Read a string value that contains escape sequences.
    fn read_string_with_escapes(
        &mut self,
        val_start: usize,
    ) -> Result<FieldValue<'a>, FlattenError> {
        let mut val = vec![b'"'];
        // Copy content from after opening quote to current position (the backslash)
        val.extend_from_slice(&self.event[val_start + 1..self.index]);

        while self.index < self.event.len() {
            let ch = self.event[self.index];
            if ch == b'"' {
                val.push(b'"');
                return Ok(FieldValue::Owned(val));
            } else if ch == b'\\' {
                self.index += 1;
                if self.index >= self.event.len() {
                    return Err(FlattenError::Error(self.error("premature end in escape")));
                }
                let escaped = self.event[self.index];
                match escaped {
                    b'"' => val.push(b'"'),
                    b'\\' => val.push(b'\\'),
                    b'/' => val.push(b'/'),
                    b'b' => val.push(0x08),
                    b'f' => val.push(0x0c),
                    b'n' => val.push(b'\n'),
                    b'r' => val.push(b'\r'),
                    b't' => val.push(b'\t'),
                    b'u' => {
                        // Unicode escape - parse 4 hex digits
                        self.index += 1;
                        let code = self.read_hex_4()?;
                        // Simple case: BMP character
                        if code < 0x80 {
                            val.push(code as u8);
                        } else if code < 0x800 {
                            val.push(0xC0 | ((code >> 6) as u8));
                            val.push(0x80 | ((code & 0x3F) as u8));
                        } else if (0xD800..=0xDBFF).contains(&code) {
                            // High surrogate - check for low surrogate
                            if self.index + 5 < self.event.len()
                                && self.event[self.index] == b'\\'
                                && self.event[self.index + 1] == b'u'
                            {
                                self.index += 2;
                                let low = self.read_hex_4()?;
                                if (0xDC00..=0xDFFF).contains(&low) {
                                    let full = 0x10000 + ((code - 0xD800) << 10) + (low - 0xDC00);
                                    val.push(0xF0 | ((full >> 18) as u8));
                                    val.push(0x80 | (((full >> 12) & 0x3F) as u8));
                                    val.push(0x80 | (((full >> 6) & 0x3F) as u8));
                                    val.push(0x80 | ((full & 0x3F) as u8));
                                    // Don't decrement here - the decrement at end of b'u' case handles it
                                }
                            }
                        } else {
                            val.push(0xE0 | ((code >> 12) as u8));
                            val.push(0x80 | (((code >> 6) & 0x3F) as u8));
                            val.push(0x80 | ((code & 0x3F) as u8));
                        }
                        self.index -= 1; // will be incremented at end of loop
                    }
                    _ => {
                        return Err(FlattenError::Error(self.error("malformed escape in text")));
                    }
                }
            } else if ch <= 0x1f {
                return Err(FlattenError::Error(
                    self.error(&format!("illegal byte {:02x} in string value", ch)),
                ));
            } else {
                val.push(ch);
            }
            self.index += 1;
        }

        Err(FlattenError::Error(self.error("premature end of event")))
    }

    /// Read 4 hex digits for a \uXXXX escape.
    fn read_hex_4(&mut self) -> Result<u32, FlattenError> {
        let mut value = 0u32;
        for _ in 0..4 {
            if self.index >= self.event.len() {
                return Err(FlattenError::Error(self.error("truncated unicode escape")));
            }
            let ch = self.event[self.index];
            let digit = match ch {
                b'0'..=b'9' => ch - b'0',
                b'a'..=b'f' => ch - b'a' + 10,
                b'A'..=b'F' => ch - b'A' + 10,
                _ => {
                    return Err(FlattenError::Error(
                        self.error("invalid hex digit in unicode escape"),
                    ))
                }
            };
            value = value * 16 + digit as u32;
            self.index += 1;
        }
        Ok(value)
    }

    /// Read a JSON number.
    fn read_number(&mut self) -> Result<FieldValue<'a>, FlattenError> {
        let start = self.index;

        // Optional minus
        if self.ch() == b'-' {
            self.index += 1;
        }

        // Integer part
        while self.index < self.event.len() {
            let ch = self.event[self.index];
            if !ch.is_ascii_digit() {
                break;
            }
            self.index += 1;
        }

        // Fractional part
        if self.index < self.event.len() && self.event[self.index] == b'.' {
            self.index += 1;
            while self.index < self.event.len() && self.event[self.index].is_ascii_digit() {
                self.index += 1;
            }
        }

        // Exponent
        if self.index < self.event.len()
            && (self.event[self.index] == b'e' || self.event[self.index] == b'E')
        {
            self.index += 1;
            if self.index < self.event.len()
                && (self.event[self.index] == b'+' || self.event[self.index] == b'-')
            {
                self.index += 1;
            }
            while self.index < self.event.len() && self.event[self.index].is_ascii_digit() {
                self.index += 1;
            }
        }

        self.index -= 1; // back up so caller can advance
        Ok(FieldValue::Borrowed(&self.event[start..=self.index]))
    }

    /// Read a literal (true, false, null).
    fn read_literal(&mut self, expected: &[u8]) -> Result<(), FlattenError> {
        for &b in expected {
            if self.ch() != b {
                return Err(FlattenError::Error(self.error("unknown literal")));
            }
            self.step()?;
        }
        self.index -= 1; // back up so caller can advance
        Ok(())
    }

    /// Get current byte.
    #[inline]
    fn ch(&self) -> u8 {
        self.event[self.index]
    }

    /// Advance to next byte.
    #[inline]
    fn step(&mut self) -> Result<(), FlattenError> {
        self.index += 1;
        if self.index < self.event.len() {
            Ok(())
        } else {
            Err(FlattenError::Error(self.error("premature end of event")))
        }
    }

    /// Enter an array.
    fn enter_array(&mut self) {
        self.array_count += 1;
        self.array_trail.push(ArrayPos {
            array: self.array_count,
            pos: 0,
        });
    }

    /// Leave an array.
    fn leave_array(&mut self) {
        self.array_trail.pop();
    }

    /// Step to next array element.
    fn step_array_element(&mut self) {
        if let Some(last) = self.array_trail.last_mut() {
            last.pos += 1;
        }
    }

    /// Create an error with location info.
    fn error(&self, message: &str) -> QuaminaError {
        let mut line_num = 1;
        let mut last_line_start = 0;
        for (i, &b) in self.event.iter().enumerate() {
            if i >= self.index {
                break;
            }
            if b == b'\n' {
                line_num += 1;
                last_line_start = i;
            }
        }
        QuaminaError::InvalidJson(format!(
            "at line {} col {}: {}",
            line_num,
            self.index - last_line_start,
            message
        ))
    }
}
#[derive(Clone, Copy)]
enum ObjectState {
    InObject,
    SeekingColon,
    MemberValue,
    AfterValue,
}

#[derive(Clone, Copy)]
enum ArrayState {
    InArray,
    AfterValue,
}

enum FlattenError {
    EarlyStop,
    Error(QuaminaError),
}

impl From<QuaminaError> for FlattenError {
    fn from(e: QuaminaError) -> Self {
        FlattenError::Error(e)
    }
}

/// Whitespace lookup table - O(1) check vs match statement.
/// Index by byte value, true if whitespace (space, tab, newline, carriage return).
const IS_WHITESPACE: [bool; 256] = {
    let mut table = [false; 256];
    table[b' ' as usize] = true;
    table[b'\t' as usize] = true;
    table[b'\n' as usize] = true;
    table[b'\r' as usize] = true;
    table
};

#[inline]
fn is_whitespace(b: u8) -> bool {
    IS_WHITESPACE[b as usize]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_tree(paths: &[&str]) -> SegmentsTree {
        let mut tree = SegmentsTree::new();
        for path in paths {
            tree.add(path);
        }
        tree
    }

    #[test]
    fn test_simple_object() {
        let event = br#"{"status": "active", "count": 42}"#;
        let tree = make_tree(&["status"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_slice(), b"status");
        assert_eq!(fields[0].val.as_bytes(), b"\"active\"");
    }

    #[test]
    fn test_nested_object() {
        let event = br#"{"context": {"user": {"id": "123"}}}"#;
        let tree = make_tree(&["context\nuser\nid"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_slice(), b"context\nuser\nid");
        assert_eq!(fields[0].val.as_bytes(), b"\"123\"");
    }

    #[test]
    fn test_skips_unused_fields() {
        let event = br#"{"a": 1, "b": 2, "c": 3, "d": 4, "e": 5}"#;
        let tree = make_tree(&["c"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_slice(), b"c");
        assert_eq!(fields[0].val.as_bytes(), b"3");
    }

    #[test]
    fn test_number_value() {
        let event = br#"{"price": 99.99}"#;
        let tree = make_tree(&["price"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert!(fields[0].is_number);
        assert_eq!(fields[0].val.as_bytes(), b"99.99");
    }

    #[test]
    fn test_array_simple() {
        let event = br#"{"tags": ["a", "b", "c"]}"#;
        let tree = make_tree(&["tags"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 3);
        // Each element should have different array position
        assert_eq!(fields[0].array_trail[0].pos, 1);
        assert_eq!(fields[1].array_trail[0].pos, 2);
        assert_eq!(fields[2].array_trail[0].pos, 3);
    }

    #[test]
    fn test_early_termination() {
        // With a large object, early termination should stop after finding needed fields
        let event = br#"{"first": 1, "second": 2, "third": 3, "fourth": 4, "fifth": 5}"#;
        let tree = make_tree(&["first"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_slice(), b"first");
    }

    #[test]
    fn test_escape_sequences() {
        let event = br#"{"msg": "hello\nworld"}"#;
        let tree = make_tree(&["msg"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        // Value should be unescaped
        assert_eq!(fields[0].val.as_bytes(), b"\"hello\nworld\"");
    }

    #[test]
    fn test_unicode_escape() {
        let event = br#"{"char": "\u0041"}"#;
        let tree = make_tree(&["char"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), b"\"A\"");
    }

    #[test]
    fn test_empty_object() {
        let event = br#"{}"#;
        let tree = make_tree(&["anything"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 0);
    }

    #[test]
    fn test_skip_nested_object() {
        let event = br#"{"skip": {"nested": {"deep": 1}}, "keep": "value"}"#;
        let tree = make_tree(&["keep"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_slice(), b"keep");
    }

    #[test]
    fn test_skip_array() {
        let event = br#"{"skip": [1, 2, [3, 4]], "keep": "value"}"#;
        let tree = make_tree(&["keep"]);
        let mut state = FlattenJsonState::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_slice(), b"keep");
    }

    #[test]
    fn test_state_reuse() {
        // Test that the state can be reused across multiple flatten calls
        let tree = make_tree(&["status"]);
        let mut state = FlattenJsonState::new();

        let event1 = br#"{"status": "active"}"#;
        let fields1 = state.flatten(event1, &tree).unwrap();
        assert_eq!(fields1.len(), 1);
        assert_eq!(fields1[0].val.as_bytes(), b"\"active\"");

        let event2 = br#"{"status": "pending"}"#;
        let fields2 = state.flatten(event2, &tree).unwrap();
        assert_eq!(fields2.len(), 1);
        assert_eq!(fields2[0].val.as_bytes(), b"\"pending\"");
    }
}
