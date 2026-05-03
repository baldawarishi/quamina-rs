//! Streaming JSON flattener with segment-based field skipping.
//!
//! This is a port of Go's flattenJSON which provides significant performance
//! improvements by:
//! - Only parsing fields that appear in patterns (using SegmentsTree)
//! - Early termination when all needed fields are found
//! - Zero-copy field values as slices of the original event bytes
//! - Reusable state with reset() pattern (like Go's flattenJSON)
//!
//! # Safety
//! This module uses unsafe for:
//! - `from_utf8_unchecked`: JSON field names are guaranteed valid UTF-8 by spec
//! - `transmute`: Lifetime extension for borrowed fields (verified by Miri)
#![allow(unsafe_code)]

use crate::QuaminaError;
use crate::segments_tree::{SegmentEntry, SegmentsTree};
use smallvec::SmallVec;
use std::sync::Arc;

/// Represents a field's position within an array in the event.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayPos {
    pub array: i32,
    pub pos: i32,
}

/// Type alias for path storage - Arc for O(1) cloning (paths are shared from SegmentsTree)
pub type PathArc = Arc<[u8]>;
/// Legacy type alias for path storage - inline up to 64 bytes to avoid heap allocation
pub type PathVec = SmallVec<[u8; 64]>;
/// Type alias for array trail storage - inline up to 4 elements
pub type ArrayTrailVec = SmallVec<[ArrayPos; 4]>;

/// A flattened field from a JSON event.
#[derive(Clone, Debug)]
pub struct Field<'a> {
    /// Full path (e.g., "context\nuser\nid") - Arc for O(1) cloning
    pub path: PathArc,
    /// Value bytes from the event
    pub val: FieldValue<'a>,
    /// Array position tracking - uses SmallVec to avoid heap allocation
    pub array_trail: ArrayTrailVec,
    /// True if the value is a JSON number
    pub is_number: bool,
}

impl Field<'_> {
    /// Returns the path as a string slice.
    ///
    /// # Safety
    /// This uses unsafe conversion because JSON field names are guaranteed
    /// to be valid UTF-8 by the JSON specification.
    #[inline]
    #[must_use]
    pub fn path_str(&self) -> &str {
        // SAFETY: JSON field names are valid UTF-8 per JSON spec (RFC 8259).
        unsafe { std::str::from_utf8_unchecked(&self.path) }
    }

    /// Returns the value as raw bytes, preserving surrounding quotes for strings.
    ///
    /// String values retain their quotes (e.g., `"hello"`) so that the automaton
    /// can distinguish strings from numbers with identical digit content.
    /// This mirrors Go's design where quotes act as an implicit type tag.
    #[inline]
    #[must_use]
    pub fn value_bytes(&self) -> &[u8] {
        self.val.as_bytes()
    }

    /// Returns the array trail as a slice.
    #[inline]
    #[must_use]
    pub fn array_trail_slice(&self) -> &[ArrayPos] {
        &self.array_trail
    }
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
    const fn as_bytes(&self) -> &[u8] {
        match self {
            MemberName::Borrowed(b) => b,
            MemberName::Owned(v) => v.as_slice(),
        }
    }
}

impl FieldValue<'_> {
    #[must_use]
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
/// Like Go's flattenJSON, we reuse the fields slice between calls to avoid
/// reallocating the underlying array.
pub struct State {
    /// Working array position trail (reused between calls)
    array_trail: ArrayTrailVec,
    /// Reusable fields storage. We use 'static as a placeholder lifetime;
    /// the actual borrows come from the event passed to flatten().
    /// This is safe because:
    /// 1. We clear the vec before each flatten call
    /// 2. We only expose fields with the correct event lifetime
    /// 3. The mutable borrow of self prevents concurrent access
    fields: Vec<Field<'static>>,
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    /// Create a new reusable flattener state.
    #[must_use]
    pub fn new() -> Self {
        Self {
            array_trail: ArrayTrailVec::new(),
            fields: Vec::with_capacity(32),
        }
    }

    /// Reset internal state for reuse.
    /// Like Go's reset(), this clears the fields slice but keeps capacity.
    #[inline]
    fn reset(&mut self) {
        self.array_trail.clear();
        self.fields.clear();
    }

    /// Flatten an event using this reusable state.
    ///
    /// Returns a mutable slice of fields that can be sorted in place.
    /// The slice borrows from both self and the event, preventing reuse
    /// until the caller is done with the fields.
    ///
    /// This is the primary API for high-performance event processing.
    /// The state is automatically reset before each call.
    pub fn flatten<'a>(
        &mut self,
        event: &'a [u8],
        tree: &SegmentsTree,
    ) -> Result<&mut [Field<'a>], QuaminaError> {
        self.reset();

        let mut ctx = FlattenContext {
            event,
            index: 0,
            fields: &mut self.fields,
            skipping: 0,
            array_trail: &mut self.array_trail,
            array_count: 0,
        };

        ctx.flatten_impl(tree)?;

        // SAFETY: The Fields in self.fields contain borrows from `event` which has lifetime 'a.
        // We store them with 'static lifetime internally, but return a slice with the correct
        // 'a lifetime. This is safe because:
        // 1. We cleared the vec at the start, so all borrows are from this event
        // 2. The returned mutable slice borrows self, preventing concurrent flatten calls
        // 3. The caller cannot use the slice after event is dropped (enforced by 'a)
        let fields_slice: &mut [Field<'a>] =
            unsafe { std::mem::transmute(self.fields.as_mut_slice()) };

        Ok(fields_slice)
    }
}

/// Internal context for a single flatten operation.
/// Borrows the reusable fields vec and array_trail from State.
struct FlattenContext<'a, 'b> {
    event: &'a [u8],
    index: usize,
    /// Borrowed mutable reference to the fields storage.
    /// We store Field<'static> internally but the actual borrows are from event.
    fields: &'b mut Vec<Field<'static>>,
    skipping: i32,
    array_trail: &'b mut ArrayTrailVec,
    array_count: i32,
}

impl<'a> FlattenContext<'a, '_> {
    /// Push a field to the storage, transmuting the lifetime.
    #[inline]
    fn push_field(&mut self, field: Field<'a>) {
        // SAFETY: Field<'a> and Field<'static> have the same layout.
        // The actual borrows are from self.event, and the caller will
        // receive a slice with the correct 'a lifetime.
        let static_field: Field<'static> = unsafe { std::mem::transmute(field) };
        self.fields.push(static_field);
    }
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
    #[allow(clippy::too_many_lines)] // single-pass JSON object reader; the state machine reads better as one function
    fn read_object(&mut self, tree: &SegmentsTree) -> Result<(), FlattenError> {
        // index points at {
        self.step()?;

        let mut fields_count = tree.fields_count();
        let mut nodes_count = tree.nodes_count();

        // Snapshot array trail for object member fields
        let array_trail: ArrayTrailVec = if self.skipping == 0 {
            self.array_trail.clone()
        } else {
            ArrayTrailVec::new()
        };

        // These are written in ObjectState::InObject (when we read a `"key"`) and
        // consumed later in ObjectState::MemberValue (when we parse the value).
        // Rust can't prove the state-machine ordering, so we must initialize them
        // here — but the initial values are never actually read.
        #[allow(unused_assignments)]
        let mut member_name: MemberName<'a> = MemberName::Borrowed(&[]);
        #[allow(unused_assignments)]
        let mut member_is_used = false;
        let mut member_entry: Option<&SegmentEntry> = None;
        let mut state = ObjectState::InObject;

        loop {
            // Early termination: all needed fields found
            if nodes_count == 0 && fields_count == 0 {
                if tree.is_root() {
                    return Err(FlattenError::EarlyStop);
                }
                return self.leave_object();
            }

            let ch = self.ch();

            match state {
                ObjectState::InObject => {
                    if is_whitespace(ch) {
                        // skip
                    } else if ch == b'"' {
                        member_name = self.read_member_name()?;
                        // Single fused lookup replaces separate is_segment_used + get + path_arc_for_segment
                        member_entry = if self.skipping == 0 {
                            tree.lookup(member_name.as_bytes())
                        } else {
                            None
                        };
                        member_is_used = member_entry.is_some();
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
                            if !member_is_used {
                                self.skipping += 1;
                            }

                            if self.skipping > 0 {
                                self.skip_block(b'[', b']')?;
                            } else {
                                let array_tree =
                                    member_entry.and_then(|e| e.node()).unwrap_or(tree);
                                let path = member_entry.and_then(|e| e.field()).cloned();
                                self.read_array(path, array_tree)?;
                            }

                            if !member_is_used {
                                self.skipping -= 1;
                            }
                        }
                        b'{' => {
                            if !member_is_used {
                                self.skipping += 1;
                            }

                            if self.skipping > 0 {
                                self.skip_block(b'{', b'}')?;
                            } else if let Some(child_tree) = member_entry.and_then(|e| e.node()) {
                                nodes_count = nodes_count.saturating_sub(1);
                                self.read_object(child_tree)?;
                            } else {
                                // No child tree - skip the block
                                self.skip_block(b'{', b'}')?;
                            }

                            if !member_is_used {
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

                    if is_leaf
                        && let Some(v) = val
                        && member_is_used
                        && let Some(path) = member_entry.and_then(|e| e.field()).cloned()
                    {
                        self.push_field(Field {
                            path,
                            val: v,
                            array_trail: array_trail.clone(),
                            is_number,
                        });
                        fields_count = fields_count.saturating_sub(1);
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
        path: Option<PathArc>,
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

                    if is_leaf
                        && let Some(v) = val
                        && self.skipping == 0
                    {
                        self.step_array_element();
                        if let Some(ref p) = path {
                            self.push_field(Field {
                                path: p.clone(),
                                val: v,
                                array_trail: self.array_trail.clone(),
                                is_number,
                            });
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
                    self.error(&format!("illegal byte {ch:02x} in field name")),
                ));
            }
            self.index += 1;
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
                    self.error(&format!("illegal byte {ch:02x} in field name")),
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
                    self.error(&format!("illegal byte {ch:02x} in string value")),
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
                    self.error(&format!("illegal byte {ch:02x} in string value")),
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
                    ));
                }
            };
            value = value * 16 + u32::from(digit);
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
            if self.index >= self.event.len() {
                return Err(FlattenError::Error(
                    self.error("number truncated after minus"),
                ));
            }
        }

        // Integer part - must have at least one digit
        let digit_start = self.index;
        while self.index < self.event.len() {
            let ch = self.event[self.index];
            if !ch.is_ascii_digit() {
                break;
            }
            self.index += 1;
        }

        // Validate we read at least one digit
        if self.index == digit_start {
            let ch = if self.index < self.event.len() {
                self.event[self.index] as char
            } else {
                '?'
            };
            return Err(FlattenError::Error(
                self.error(&format!("illegal character '{ch}' in number")),
            ));
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
        Self::Error(e)
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
const fn is_whitespace(b: u8) -> bool {
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
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_ref(), b"status");
        assert_eq!(fields[0].val.as_bytes(), b"\"active\"");
    }

    #[test]
    fn test_nested_object() {
        let event = br#"{"context": {"user": {"id": "123"}}}"#;
        let tree = make_tree(&["context\nuser\nid"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_ref(), b"context\nuser\nid");
        assert_eq!(fields[0].val.as_bytes(), b"\"123\"");
    }

    #[test]
    fn test_skips_unused_fields() {
        let event = br#"{"a": 1, "b": 2, "c": 3, "d": 4, "e": 5}"#;
        let tree = make_tree(&["c"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_ref(), b"c");
        assert_eq!(fields[0].val.as_bytes(), b"3");
    }

    #[test]
    fn test_number_value() {
        let event = br#"{"price": 99.99}"#;
        let tree = make_tree(&["price"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert!(fields[0].is_number);
        assert_eq!(fields[0].val.as_bytes(), b"99.99");
    }

    #[test]
    fn test_array_simple() {
        let event = br#"{"tags": ["a", "b", "c"]}"#;
        let tree = make_tree(&["tags"]);
        let mut state = State::new();
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
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_ref(), b"first");
    }

    #[test]
    fn test_escape_sequences() {
        let event = br#"{"msg": "hello\nworld"}"#;
        let tree = make_tree(&["msg"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        // Value should be unescaped
        assert_eq!(fields[0].val.as_bytes(), b"\"hello\nworld\"");
    }

    #[test]
    fn test_unicode_escape() {
        let event = br#"{"char": "\u0041"}"#;
        let tree = make_tree(&["char"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), b"\"A\"");
    }

    #[test]
    fn test_empty_object() {
        let event = br"{}";
        let tree = make_tree(&["anything"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 0);
    }

    #[test]
    fn test_skip_nested_object() {
        let event = br#"{"skip": {"nested": {"deep": 1}}, "keep": "value"}"#;
        let tree = make_tree(&["keep"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_ref(), b"keep");
    }

    #[test]
    fn test_skip_array() {
        let event = br#"{"skip": [1, 2, [3, 4]], "keep": "value"}"#;
        let tree = make_tree(&["keep"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path.as_ref(), b"keep");
    }

    #[test]
    fn test_state_reuse() {
        // Test that the state can be reused across multiple flatten calls.
        // The Vec storage is reused (capacity preserved), avoiding reallocation.
        let tree = make_tree(&["status"]);
        let mut state = State::new();

        // First call
        let event1 = br#"{"status": "active"}"#;
        {
            let fields1 = state.flatten(event1, &tree).unwrap();
            assert_eq!(fields1.len(), 1);
            assert_eq!(fields1[0].val.as_bytes(), b"\"active\"");
        }

        // Second call - state is reused, Vec capacity preserved
        let event2 = br#"{"status": "pending"}"#;
        {
            let fields2 = state.flatten(event2, &tree).unwrap();
            assert_eq!(fields2.len(), 1);
            assert_eq!(fields2[0].val.as_bytes(), b"\"pending\"");
        }
    }

    #[test]
    fn test_trailing_garbage_after_close_brace() {
        // Use a non-existent field to force full parse without early termination.
        let tree = make_tree(&["nonexistent"]);
        let mut state = State::new();

        // Valid: simple object with trailing whitespace
        let result = state.flatten(br#"{"status": "ok"}  "#, &tree);
        assert!(
            result.is_ok(),
            "Valid JSON with trailing spaces should succeed"
        );

        // Valid: single space after close brace
        let result = state.flatten(br#"{"status": "ok"} "#, &tree);
        assert!(
            result.is_ok(),
            "Valid JSON with single trailing space should succeed"
        );

        // Invalid: garbage character after close brace
        let result = state.flatten(br#"{"status": "ok"}x"#, &tree);
        assert!(
            result.is_err(),
            "JSON with garbage char after close brace should fail"
        );

        // Invalid: newline and then garbage
        let result = state.flatten(
            br#"{"status": "ok"}
x"#,
            &tree,
        );
        assert!(
            result.is_err(),
            "JSON with newline then garbage should fail"
        );
    }

    #[test]
    fn test_whitespace_before_opening_brace() {
        let tree = make_tree(&["x"]);
        let mut state = State::new();

        // Valid: spaces before open brace
        let result = state.flatten(b"  {\"x\": 1}", &tree);
        assert!(result.is_ok(), "Spaces before open brace should be skipped");

        // Valid: tabs and newlines before open brace
        let result = state.flatten(b" \t\n {\"x\": 1}", &tree);
        assert!(
            result.is_ok(),
            "Mixed whitespace before open brace should work"
        );

        // Invalid: EOF during whitespace skip
        let result = state.flatten(b"   ", &tree);
        assert!(
            result.is_err(),
            "Whitespace-only input should fail with EOF"
        );

        // Invalid: non-{ after whitespace
        let result = state.flatten(b" [1,2,3]", &tree);
        assert!(result.is_err(), "Non-object after whitespace should fail");
    }

    // Error handling tests - based on Go quamina's TestFJErrorCases
    #[test]
    fn test_error_truncated_object() {
        let tree = make_tree(&["a", "b"]);
        let mut state = State::new();

        let bad_cases = [
            r#"{"a"#,        // Truncated key
            r#"{"a""#,       // Missing colon
            r#"{"a":"#,      // Missing value
            r#"{"a": "#,     // Missing value after space
            r"{",            // Just open brace
            r#"{"a": 1"#,    // Missing close brace
            r#"{"a": 2 2}"#, // Double value
        ];

        for bad in &bad_cases {
            let result = state.flatten(bad.as_bytes(), &tree);
            assert!(result.is_err(), "Should reject truncated JSON: {bad}");
        }
    }

    #[test]
    fn test_error_truncated_array() {
        let tree = make_tree(&["a"]);
        let mut state = State::new();

        let bad_cases = [
            r#"{"a": ["#,    // Just open bracket
            r#"{"a": [  "#,  // Open bracket with space
            r#"{"a": [1, "#, // Truncated after comma
        ];

        for bad in &bad_cases {
            let result = state.flatten(bad.as_bytes(), &tree);
            assert!(result.is_err(), "Should reject truncated array: {bad}");
        }
    }

    #[test]
    fn test_error_truncated_string() {
        let tree = make_tree(&["k"]);
        let mut state = State::new();

        let bad_cases = [
            r#"{"k": ""#,  // Unterminated string
            r#"{"k": "t"#, // Unterminated string
            r#"{"k": "\"#, // Unterminated escape
        ];

        for bad in &bad_cases {
            let result = state.flatten(bad.as_bytes(), &tree);
            assert!(result.is_err(), "Should reject truncated string: {bad}");
        }
    }

    #[test]
    fn test_error_invalid_value() {
        let tree = make_tree(&["a"]);
        let mut state = State::new();

        let bad_cases = [
            r#"{"a": xx}"#,    // Invalid value
            r#"{"a": tru}"#,   // Truncated true
            r#"{"a": truse}"#, // Invalid boolean
            r#"{"a": -z}"#,    // Invalid negative number (Go TestFJErrorCases)
        ];

        for bad in &bad_cases {
            let result = state.flatten(bad.as_bytes(), &tree);
            assert!(result.is_err(), "Should reject invalid value: {bad}");
        }
    }

    #[test]
    fn test_error_invalid_json_structure() {
        let tree = make_tree(&["a"]);
        let mut state = State::new();

        let bad_cases = [
            r#""xx""#,            // Not an object at top level
            "",                   // Empty input
            r#"{"a" : [ foo ]}"#, // Invalid array element
        ];

        for bad in &bad_cases {
            let result = state.flatten(bad.as_bytes(), &tree);
            assert!(result.is_err(), "Should reject invalid structure: {bad}");
        }
    }

    #[test]
    fn test_error_invalid_nested_object() {
        // Based on Go TestFJErrorCases - need to track nested field "a\nx" to force
        // parsing of nested object (otherwise the object is skipped without validation)
        let tree = make_tree(&["a", "a\nx"]);
        let mut state = State::new();

        let bad = r#"{"a": { x }}"#; // Invalid: x is not a valid JSON value
        let result = state.flatten(bad.as_bytes(), &tree);
        assert!(
            result.is_err(),
            "Should reject invalid nested object: {bad}"
        );
    }

    #[test]
    fn test_array_with_booleans_and_null() {
        let event = br#"{"items": [true, false, null, 42]}"#;
        let tree = make_tree(&["items"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 4);
        assert_eq!(fields[0].val.as_bytes(), b"true");
        assert_eq!(fields[1].val.as_bytes(), b"false");
        assert_eq!(fields[2].val.as_bytes(), b"null");
        assert_eq!(fields[3].val.as_bytes(), b"42");
    }

    #[test]
    fn test_array_with_nested_objects_and_arrays() {
        // Nested objects in arrays should get proper array position tracking.
        let event = br#"{"data": [{"id": 1}, {"id": 2}]}"#;
        let tree = make_tree(&["data", "data\nid"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();

        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].val.as_bytes(), b"1");
        assert_eq!(fields[1].val.as_bytes(), b"2");
        // Array positions must differ
        assert_ne!(fields[0].array_trail[0].pos, fields[1].array_trail[0].pos);

        // Nested arrays: [[1, 2], [3, 4]]
        // Values from different inner arrays must have distinct outer array positions
        let event2 = br#"{"matrix": [[1, 2], [3, 4]]}"#;
        let tree2 = make_tree(&["matrix"]);
        let mut state2 = State::new();
        let fields2 = state2.flatten(event2, &tree2).unwrap();
        assert_eq!(fields2.len(), 4);
        // Values 1,2 should share one outer array pos; values 3,4 a different one
        assert_eq!(fields2[0].array_trail[0].pos, fields2[1].array_trail[0].pos);
        assert_eq!(fields2[2].array_trail[0].pos, fields2[3].array_trail[0].pos);
        assert_ne!(fields2[0].array_trail[0].pos, fields2[2].array_trail[0].pos);
        // All values should have exactly 2 levels of array trail
        for f in fields2.iter() {
            assert_eq!(
                f.array_trail.len(),
                2,
                "nested array should produce 2-level trail"
            );
        }

        // Empty array followed by non-empty: empty [] must clean up its trail entry.
        let event3 = br#"{"items": [[], [1, 2]]}"#;
        let tree3 = make_tree(&["items"]);
        let mut state3 = State::new();
        let fields3 = state3.flatten(event3, &tree3).unwrap();
        assert_eq!(fields3.len(), 2);
        // Values should have exactly 2 levels (outer + inner), not 3
        for f in fields3.iter() {
            assert_eq!(
                f.array_trail.len(),
                2,
                "empty array must not leave stale trail entry"
            );
        }
    }

    #[test]
    fn test_skip_strings_containing_brackets() {
        // Strings containing } or ] must be properly skipped, not treated as delimiters.
        let event = br#"{"skip": {"key": "val}ue"}, "keep": "ok"}"#;
        let tree = make_tree(&["keep"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""ok""#);

        // String with ] inside a skipped array
        let event2 = br#"{"skip": ["a]b", "c]d"], "keep": "yes"}"#;
        let fields2 = state.flatten(event2, &tree).unwrap();
        assert_eq!(fields2.len(), 1);
        assert_eq!(fields2[0].val.as_bytes(), br#""yes""#);

        // String with { and [ inside a skipped block
        let event3 = br#"{"skip": {"k": "v{a[l"}, "keep": "go"}"#;
        let fields3 = state.flatten(event3, &tree).unwrap();
        assert_eq!(fields3.len(), 1);
        assert_eq!(fields3[0].val.as_bytes(), br#""go""#);
    }

    #[test]
    fn test_leave_object_skips_strings_with_braces() {
        // When early termination exits a nested object, remaining strings with }
        // must be properly skipped. A second outer field prevents double early-termination.
        let event = br#"{"outer": {"wanted": "got", "leftover": "has}brace"}, "after": "ok"}"#;
        let tree = make_tree(&["outer\nwanted", "after"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 2);
        assert_eq!(fields[0].val.as_bytes(), br#""got""#);
        assert_eq!(fields[1].val.as_bytes(), br#""ok""#);

        // Also with nested object inside the leftover
        let event2 = br#"{"outer": {"wanted": "ok", "extra": {"deep": "val}ue"}}, "after": "yes"}"#;
        let tree2 = make_tree(&["outer\nwanted", "after"]);
        let fields2 = state.flatten(event2, &tree2).unwrap();
        assert_eq!(fields2.len(), 2);
        assert_eq!(fields2[0].val.as_bytes(), br#""ok""#);
        assert_eq!(fields2[1].val.as_bytes(), br#""yes""#);
    }

    #[test]
    fn test_skip_string_with_escaped_quote() {
        // A skipped string with \" must not end the string at the escaped quote.
        let event = br#"{"skip": "has \" quote", "keep": "ok"}"#;
        let tree = make_tree(&["keep"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""ok""#);

        // Also test escaped backslash before quote: \\" means actual \ then end-of-string
        let event2 = br#"{"skip": "end\\", "keep": "yes"}"#;
        let fields2 = state.flatten(event2, &tree).unwrap();
        assert_eq!(fields2.len(), 1);
        assert_eq!(fields2[0].val.as_bytes(), br#""yes""#);

        // Other escape sequences in skipped strings must not confuse the skip logic.
        let event3 = br#"{"skip": "a\tb", "keep": "z"}"#;
        let fields3 = state.flatten(event3, &tree).unwrap();
        assert_eq!(fields3.len(), 1);
        assert_eq!(fields3[0].val.as_bytes(), br#""z""#);

        let event4 = br#"{"skip": "test\n", "keep": "w"}"#;
        let fields4 = state.flatten(event4, &tree).unwrap();
        assert_eq!(fields4.len(), 1);
        assert_eq!(fields4[0].val.as_bytes(), br#""w""#);
    }

    #[test]
    fn test_member_name_escape_sequences() {
        // Each JSON escape sequence in a member name must be correctly decoded.
        let mut state = State::new();

        // \t in member name
        let tree_t = make_tree(&["key\twith\ttab"]);
        let event_t = br#"{"key\twith\ttab": "val_t"}"#;
        let fields = state.flatten(event_t, &tree_t).unwrap();
        assert_eq!(fields.len(), 1, "\\t escape in member name");
        assert_eq!(fields[0].val.as_bytes(), br#""val_t""#);

        // \r in member name (can't test \n since it's the path separator)
        let tree_r = make_tree(&["key\rwith\rreturn"]);
        let event_r = br#"{"key\rwith\rreturn": "val_r"}"#;
        let fields = state.flatten(event_r, &tree_r).unwrap();
        assert_eq!(fields.len(), 1, "\\r escape in member name");

        // \b (backspace) in member name
        let tree_b = make_tree(&["key\x08val"]);
        let event_b = br#"{"key\bval": "val_b"}"#;
        let fields = state.flatten(event_b, &tree_b).unwrap();
        assert_eq!(fields.len(), 1, "\\b escape in member name");

        // \f (form feed) in member name
        let tree_f = make_tree(&["key\x0cval"]);
        let event_f = br#"{"key\fval": "val_f"}"#;
        let fields = state.flatten(event_f, &tree_f).unwrap();
        assert_eq!(fields.len(), 1, "\\f escape in member name");

        // \/ (forward slash) in member name
        let tree_slash = make_tree(&["key/val"]);
        let event_slash = br#"{"key\/val": "val_slash"}"#;
        let fields = state.flatten(event_slash, &tree_slash).unwrap();
        assert_eq!(fields.len(), 1, "\\/ escape in member name");

        // \\ (backslash) in member name
        let tree_bs = make_tree(&["key\\val"]);
        let event_bs = br#"{"key\\val": "val_bs"}"#;
        let fields = state.flatten(event_bs, &tree_bs).unwrap();
        assert_eq!(fields.len(), 1, "\\\\ escape in member name");

        // \" (quote) in member name
        let tree_q = make_tree(&["key\"val"]);
        let event_q = br#"{"key\"val": "val_q"}"#;
        let fields = state.flatten(event_q, &tree_q).unwrap();
        assert_eq!(fields.len(), 1, "\\\" escape in member name");

        // \n conflicts with the path separator, so just verify parsing succeeds.
        let tree_after = make_tree(&["after"]);
        let event_n = br#"{"key\nval": 1, "after": "found"}"#;
        let fields = state.flatten(event_n, &tree_after).unwrap();
        assert_eq!(fields.len(), 1, "\\n escape in member name must parse");
        assert_eq!(fields[0].val.as_bytes(), br#""found""#);
    }

    #[test]
    fn test_unicode_escape_uppercase_hex() {
        // Unicode escapes with uppercase hex digits (A-F) must work.
        let tree = make_tree(&["ch"]);
        let mut state = State::new();

        // \u00AB uses uppercase A and B
        let event = br#"{"ch": "\u00AB"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        // U+00AB is «, encoded as UTF-8: 0xC2 0xAB
        assert_eq!(fields[0].val.as_bytes(), &[b'"', 0xC2, 0xAB, b'"']);

        // Also test in a member name
        let tree2 = make_tree(&["\u{00FF}key"]);
        let event2 = br#"{"\u00FFkey": "found"}"#;
        let fields2 = state.flatten(event2, &tree2).unwrap();
        assert_eq!(
            fields2.len(),
            1,
            "Uppercase hex in member name unicode escape"
        );
    }

    #[test]
    fn test_number_with_exponent_sign() {
        let tree = make_tree(&["val"]);
        let mut state = State::new();

        let event_plus = br#"{"val": 1e+2}"#;
        let fields = state.flatten(event_plus, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), b"1e+2");
        assert!(fields[0].is_number);

        let event_minus = br#"{"val": 3.14e-10}"#;
        let fields = state.flatten(event_minus, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), b"3.14e-10");
        assert!(fields[0].is_number);
    }

    #[test]
    fn test_unicode_escape_2byte_in_name() {
        // \u00E9 = é, 2-byte UTF-8: 0xC3 0xA9
        let tree = make_tree(&["é"]);
        let mut state = State::new();
        let event = br#"{"\u00E9": "yes"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""yes""#);
    }

    #[test]
    fn test_unicode_escape_boundary_0080_in_name() {
        // U+0080 sits at the 1-byte / 2-byte UTF-8 boundary
        let tree = make_tree(&["\u{0080}"]);
        let mut state = State::new();
        let event = br#"{"\u0080": "boundary"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""boundary""#);
    }

    #[test]
    fn test_unicode_escape_boundary_0800_in_name() {
        // U+0800 sits at the 2-byte / 3-byte UTF-8 boundary
        let tree = make_tree(&["\u{0800}"]);
        let mut state = State::new();
        let event = br#"{"\u0800": "boundary2"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""boundary2""#);
    }

    #[test]
    fn test_unicode_escape_3byte_in_name() {
        // \u4E2D = 中, 3-byte UTF-8: 0xE4 0xB8 0xAD
        let tree = make_tree(&["中"]);
        let mut state = State::new();
        let event = br#"{"\u4E2D": "ok"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""ok""#);
    }

    #[test]
    fn test_unicode_escape_surrogate_pair_in_name() {
        // \uD83D\uDE00 = 😀, surrogate pair, 4-byte UTF-8: 0xF0 0x9F 0x98 0x80
        let tree = make_tree(&["😀"]);
        let mut state = State::new();
        let event = br#"{"\uD83D\uDE00": "smile"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""smile""#);
    }

    #[test]
    fn test_unicode_escape_2byte_in_value() {
        // 2-byte unicode escape in a string value
        let tree = make_tree(&["v"]);
        let mut state = State::new();
        let event = br#"{"v": "\u00E9"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        // é = 0xC3 0xA9
        assert_eq!(fields[0].val.as_bytes(), &[b'"', 0xC3, 0xA9, b'"']);
    }

    #[test]
    fn test_unicode_escape_boundary_0080_in_value() {
        // U+0080 sits at the 1-byte / 2-byte UTF-8 boundary
        let tree = make_tree(&["v"]);
        let mut state = State::new();
        let event = br#"{"v": "\u0080"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        // U+0080 = 0xC2 0x80 in UTF-8
        assert_eq!(fields[0].val.as_bytes(), &[b'"', 0xC2, 0x80, b'"']);
    }

    #[test]
    fn test_unicode_escape_boundary_0800_in_value() {
        // U+0800 sits at the 2-byte / 3-byte UTF-8 boundary
        let tree = make_tree(&["v"]);
        let mut state = State::new();
        let event = br#"{"v": "\u0800"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        // U+0800 = 0xE0 0xA0 0x80 in UTF-8
        assert_eq!(fields[0].val.as_bytes(), &[b'"', 0xE0, 0xA0, 0x80, b'"']);
    }

    #[test]
    fn test_unicode_escape_3byte_in_value() {
        let tree = make_tree(&["v"]);
        let mut state = State::new();
        let event = br#"{"v": "\u4E2D"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        // 中 = 0xE4 0xB8 0xAD
        assert_eq!(fields[0].val.as_bytes(), &[b'"', 0xE4, 0xB8, 0xAD, b'"']);
    }

    #[test]
    fn test_unicode_escape_surrogate_pair_in_value() {
        let tree = make_tree(&["v"]);
        let mut state = State::new();
        let event = br#"{"v": "\uD83D\uDE00"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        // 😀 = 0xF0 0x9F 0x98 0x80
        assert_eq!(
            fields[0].val.as_bytes(),
            &[b'"', 0xF0, 0x9F, 0x98, 0x80, b'"']
        );
    }

    #[test]
    fn test_skip_string_with_unicode_escape() {
        // Unicode escapes in skipped strings must not confuse the skip logic
        let tree = make_tree(&["keep"]);
        let mut state = State::new();
        let event = br#"{"skip": "\uD83D\uDE00end", "keep": "found"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""found""#);
    }

    #[test]
    fn test_unused_field_with_unicode_name() {
        // Field with unicode-escaped name that isn't in the tree must be skipped
        let tree = make_tree(&["wanted"]);
        let mut state = State::new();
        let event = br#"{"\uD83D\uDE00": "ignored", "wanted": "yes"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""yes""#);
    }

    #[test]
    fn test_surrogate_pair_codepoint_arithmetic_in_name() {
        // \uD83D\uDE00 = 😀; pattern uses literal UTF-8, event uses escape.
        // Wrong (low - 0xDC00) arithmetic produces different bytes and fails the lookup.
        let tree = make_tree(&["😀"]);
        let mut state = State::new();
        let event = br#"{"\uD83D\uDE00": "match"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""match""#);
    }

    #[test]
    fn test_surrogate_pair_codepoint_arithmetic_in_value() {
        // Same arithmetic check for string values rather than field names.
        let tree = make_tree(&["v"]);
        let mut state = State::new();
        let event = br#"{"v": "\uD83D\uDE00"}"#;
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        // 😀 = 0xF0 0x9F 0x98 0x80
        assert_eq!(
            fields[0].val.as_bytes(),
            &[b'"', 0xF0, 0x9F, 0x98, 0x80, b'"']
        );
    }

    #[test]
    fn test_skip_array_in_nested_object() {
        // Array under an unmatched sibling key inside a nested object must be skipped.
        let event = br#"{"outer": {"unused_array": [1, 2, 3], "wanted": 42}}"#;
        let tree = make_tree(&["outer\nwanted"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), b"42");
    }

    #[test]
    fn test_null_in_skipped_context() {
        // Null field under a skipped nested object must not be captured.
        let event = br#"{"skipped_object": {"a": null}, "wanted": "ok"}"#;
        let tree = make_tree(&["wanted"]);
        let mut state = State::new();
        let fields = state.flatten(event, &tree).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].val.as_bytes(), br#""ok""#);
    }

    #[test]
    fn test_error_skipping_never_ending_string() {
        // Tests from Go TestFJSkippingErrors
        let tree = make_tree(&["non_existing_value"]);
        let mut state = State::new();

        let bad_cases = [
            r#"{ "a": { "v": "hello"#, // Block with string that never ends
            r#"{ "a": ["hello"#,       // Array with string that never ends
            r#"{ "k": ""#,             // String that never ends
            r#"{ "k": { "a":"#,        // Truncated block
            r#"{ "k": {"#,             // Truncated block
            r#"{ "k": [1, "#,          // Truncated array
            r#"{ "k": ["#,             // Truncated array
        ];

        for bad in &bad_cases {
            let result = state.flatten(bad.as_bytes(), &tree);
            assert!(result.is_err(), "Should reject never-ending string: {bad}");
        }
    }
}
