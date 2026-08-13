//! MessagePack event flattener.
//!
//! Decodes MessagePack-encoded events (<https://github.com/msgpack/msgpack/blob/master/spec.md>)
//! into the same [`OwnedField`] shape the JSON flattener produces: paths
//! joined by `\n`, string values wrapped in `"`, numbers canonicalized to
//! minimal decimal text, and array trails allocated in positive preorder.
//!
//! # Policies
//!
//! MessagePack can express values JSON cannot (raw binary, extension types,
//! timestamps, non-text map keys). Each of those is governed by an explicit,
//! named policy on [`MessagePackFlattener`] so the decoder never has to guess
//! how to represent something. Defaults reject anything JSON has no
//! equivalent for; callers opt in to a specific, collision-free
//! representation via [`MessagePackFlattener::builder`].
//!
//! # Validation
//!
//! Every value in the document is decoded and validated (type policy,
//! numeric range, resource limits) regardless of whether the current
//! [`SegmentsTreeTracker`] considers it relevant to any pattern. Tracking
//! only controls whether a scalar is materialized into an [`OwnedField`] and
//! whether an array position is allocated an id; it never skips structural
//! or policy validation, so a hostile payload cannot hide behind an
//! unreferenced field.

use crate::{
    ArrayPos, BinaryValuePolicy, CanonicalValue, DuplicateKeyPolicy, EventFormat, EventLimits,
    Flattener, MapKeyPolicy, NumericPolicy, OwnedField, QuaminaError, RootValuePolicy,
    SegmentsTreeTracker,
};
use rustc_hash::FxHashSet;

/// How unrecognized MessagePack extension types (`ext`/`fixext`, other than
/// the timestamp extension governed by [`MessagePackTimestampPolicy`]) are
/// handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ExtensionValuePolicy {
    /// Reject any event containing an extension type this decoder does not
    /// have a named, stable representation for.
    #[default]
    RejectUnknown,
}

/// How the MessagePack timestamp extension (type -1) is represented.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MessagePackTimestampPolicy {
    /// Represent the timestamp as an RFC 3339 UTC string, e.g.
    /// `"1970-01-01T00:00:01Z"`. All three encodings (32-bit seconds-only
    /// fixext4, 64-bit fixext8 packing nanoseconds and seconds, and the
    /// 12-byte ext8 form) canonicalize the same way.
    #[default]
    CanonicalRfc3339,
}

/// Builder for [`MessagePackFlattener`].
///
/// Construct via [`MessagePackFlattener::builder`], chain the policy setters
/// that need to differ from their defaults, and finish with
/// [`build`](Self::build).
#[derive(Debug, Clone, Copy, Default)]
pub struct MessagePackFlattenerBuilder {
    numbers: NumericPolicy,
    binary_values: BinaryValuePolicy,
    map_keys: MapKeyPolicy,
    duplicate_keys: DuplicateKeyPolicy,
    extensions: ExtensionValuePolicy,
    timestamps: MessagePackTimestampPolicy,
    root_values: RootValuePolicy,
    limits: EventLimits,
}

impl MessagePackFlattenerBuilder {
    /// Set the numeric canonicalization policy.
    #[must_use]
    pub const fn numbers(mut self, policy: NumericPolicy) -> Self {
        self.numbers = policy;
        self
    }

    /// Set the binary-value policy.
    #[must_use]
    pub const fn binary_values(mut self, policy: BinaryValuePolicy) -> Self {
        self.binary_values = policy;
        self
    }

    /// Set the map-key policy.
    #[must_use]
    pub const fn map_keys(mut self, policy: MapKeyPolicy) -> Self {
        self.map_keys = policy;
        self
    }

    /// Set the duplicate-key policy.
    #[must_use]
    pub const fn duplicate_keys(mut self, policy: DuplicateKeyPolicy) -> Self {
        self.duplicate_keys = policy;
        self
    }

    /// Set the unknown-extension policy.
    #[must_use]
    pub const fn extensions(mut self, policy: ExtensionValuePolicy) -> Self {
        self.extensions = policy;
        self
    }

    /// Set the timestamp-extension policy.
    #[must_use]
    pub const fn timestamps(mut self, policy: MessagePackTimestampPolicy) -> Self {
        self.timestamps = policy;
        self
    }

    /// Set the root-value policy.
    #[must_use]
    pub const fn root_values(mut self, policy: RootValuePolicy) -> Self {
        self.root_values = policy;
        self
    }

    /// Set the resource limits enforced while decoding.
    #[must_use]
    pub const fn limits(mut self, limits: EventLimits) -> Self {
        self.limits = limits;
        self
    }

    /// Finish building the flattener.
    #[must_use]
    pub const fn build(self) -> MessagePackFlattener {
        MessagePackFlattener {
            numbers: self.numbers,
            binary_values: self.binary_values,
            map_keys: self.map_keys,
            duplicate_keys: self.duplicate_keys,
            extensions: self.extensions,
            timestamps: self.timestamps,
            root_values: self.root_values,
            limits: self.limits,
        }
    }
}

/// A [`Flattener`] that decodes MessagePack events.
///
/// Use [`new`](Self::new) for default policies, or
/// [`builder`](Self::builder) to select non-default policies. See the
/// [module docs](self) for how MessagePack values map onto the JSON scalar
/// representation Quamina's matcher expects.
#[derive(Debug, Clone, Copy)]
pub struct MessagePackFlattener {
    numbers: NumericPolicy,
    binary_values: BinaryValuePolicy,
    map_keys: MapKeyPolicy,
    duplicate_keys: DuplicateKeyPolicy,
    extensions: ExtensionValuePolicy,
    timestamps: MessagePackTimestampPolicy,
    root_values: RootValuePolicy,
    limits: EventLimits,
}

impl MessagePackFlattener {
    /// Create a flattener with every policy at its default.
    #[must_use]
    pub fn new() -> Self {
        Self::builder().build()
    }

    /// Start a [`MessagePackFlattenerBuilder`] to select non-default policies.
    #[must_use]
    pub fn builder() -> MessagePackFlattenerBuilder {
        MessagePackFlattenerBuilder::default()
    }
}

impl Default for MessagePackFlattener {
    fn default() -> Self {
        Self::new()
    }
}

impl Flattener for MessagePackFlattener {
    fn flatten(
        &mut self,
        event: &[u8],
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        let mut decoder = Decoder::new(event, *self);
        decoder.decode_root(tracker)?;
        Ok(decoder.fields)
    }

    fn copy(&self) -> Box<dyn Flattener> {
        Box::new(*self)
    }
}

// =============================================================================
// Error helpers
// =============================================================================

fn invalid_event(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::invalid_event(EventFormat::MessagePack, message)
}

fn limit_exceeded(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::limit_exceeded(EventFormat::MessagePack, message)
}

fn unsupported_value(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_value(EventFormat::MessagePack, message)
}

fn unsupported_map_key(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_map_key(EventFormat::MessagePack, message)
}

fn unsupported_feature(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_feature(EventFormat::MessagePack, message)
}

fn duplicate_field() -> QuaminaError {
    crate::decoder_errors::duplicate_field(EventFormat::MessagePack)
}

/// Largest integer magnitude that round-trips exactly through `f64`, i.e. 2^53.
const MAX_SAFE_INT: i128 = 9_007_199_254_740_992;

// =============================================================================
// Decoder
// =============================================================================

/// Recursive-descent MessagePack decoder.
///
/// Every value is decoded and validated regardless of whether it is
/// referenced by any pattern; the `field_path`/`child_tracker` parameters
/// threaded through the recursive `decode_*` methods only decide whether a
/// scalar is materialized into an [`OwnedField`] and whether an array
/// position is allocated an id, mirroring [`JsonFlattener`](crate::JsonFlattener)'s
/// tracker-driven skipping.
struct Decoder<'a> {
    data: &'a [u8],
    pos: usize,
    numbers: NumericPolicy,
    binary_values: BinaryValuePolicy,
    map_keys: MapKeyPolicy,
    duplicate_keys: DuplicateKeyPolicy,
    extensions: ExtensionValuePolicy,
    timestamps: MessagePackTimestampPolicy,
    root_values: RootValuePolicy,
    limits: EventLimits,
    fields: Vec<OwnedField>,
    field_count: usize,
    allocated_bytes: usize,
    next_array_id: i32,
    array_trail: Vec<ArrayPos>,
}

impl<'a> Decoder<'a> {
    const fn new(data: &'a [u8], flattener: MessagePackFlattener) -> Self {
        Self {
            data,
            pos: 0,
            numbers: flattener.numbers,
            binary_values: flattener.binary_values,
            map_keys: flattener.map_keys,
            duplicate_keys: flattener.duplicate_keys,
            extensions: flattener.extensions,
            timestamps: flattener.timestamps,
            root_values: flattener.root_values,
            limits: flattener.limits,
            fields: Vec::new(),
            field_count: 0,
            allocated_bytes: 0,
            next_array_id: 1,
            array_trail: Vec::new(),
        }
    }

    // -- top level -----------------------------------------------------

    fn decode_root(&mut self, tracker: &dyn SegmentsTreeTracker) -> Result<(), QuaminaError> {
        if self.data.is_empty() {
            return Err(invalid_event("empty MessagePack event").at_byte_offset(0));
        }
        let marker = self.data[0];
        let is_map = matches!(marker, 0x80..=0x8F | 0xDE | 0xDF);
        match self.root_values {
            RootValuePolicy::MapOnly => {
                if !is_map {
                    return Err(
                        unsupported_value("root MessagePack value must be a map").at_byte_offset(0)
                    );
                }
            }
        }
        self.decode_map(Some(tracker), 1)?;
        if self.pos != self.data.len() {
            return Err(
                invalid_event("trailing bytes after top-level MessagePack value")
                    .at_byte_offset(self.pos),
            );
        }
        Ok(())
    }

    // -- containers ------------------------------------------------------

    fn decode_map(
        &mut self,
        tracker: Option<&dyn SegmentsTreeTracker>,
        depth: usize,
    ) -> Result<(), QuaminaError> {
        let offset = self.pos;
        let len = self.read_map_header()?;
        self.check_container_len(len, offset)?;
        let mut seen_keys: FxHashSet<Vec<u8>> = FxHashSet::default();
        for _ in 0..len {
            let key_offset = self.pos;
            let key = self.read_map_key()?;
            match self.duplicate_keys {
                DuplicateKeyPolicy::Reject => {
                    if !seen_keys.insert(key.clone()) {
                        return Err(duplicate_field().at_byte_offset(key_offset));
                    }
                }
            }
            let (field_path, child_tracker) = match tracker {
                Some(t) => (t.path_for_segment(&key).map(<[u8]>::to_vec), t.get(&key)),
                None => (None, None),
            };
            self.decode_value(field_path, child_tracker, depth)?;
        }
        Ok(())
    }

    fn decode_array(
        &mut self,
        field_path: Option<Vec<u8>>,
        child_tracker: Option<&dyn SegmentsTreeTracker>,
        depth: usize,
    ) -> Result<(), QuaminaError> {
        let offset = self.pos;
        let len = self.read_array_header()?;
        self.check_container_len(len, offset)?;
        let used = field_path.is_some() || child_tracker.is_some();
        if used {
            let array_id = self.alloc_array_id(offset)?;
            self.array_trail.push(ArrayPos {
                array: array_id,
                pos: 0,
            });
            for i in 0..len {
                let pos_i32 = i32::try_from(i + 1)
                    .map_err(|_| limit_exceeded("array position exceeds i32 range"))
                    .map_err(|e| e.at_byte_offset(offset))?;
                if let Some(last) = self.array_trail.last_mut() {
                    last.pos = pos_i32;
                }
                self.decode_value(field_path.clone(), child_tracker, depth)?;
            }
            self.array_trail.pop();
        } else {
            for _ in 0..len {
                self.decode_value(None, None, depth)?;
            }
        }
        Ok(())
    }

    // -- dispatch ----------------------------------------------------------

    fn decode_value(
        &mut self,
        field_path: Option<Vec<u8>>,
        child_tracker: Option<&dyn SegmentsTreeTracker>,
        depth: usize,
    ) -> Result<(), QuaminaError> {
        let marker_offset = self.pos;
        let marker = self.peek_u8()?;
        match marker {
            0x80..=0x8F | 0xDE | 0xDF => {
                let new_depth = depth + 1;
                self.check_depth(new_depth, marker_offset)?;
                self.decode_map(child_tracker, new_depth)?;
            }
            0x90..=0x9F | 0xDC | 0xDD => {
                let new_depth = depth + 1;
                self.check_depth(new_depth, marker_offset)?;
                self.decode_array(field_path, child_tracker, new_depth)?;
            }
            0x00..=0x7F | 0xE0..=0xFF | 0xCC..=0xD3 => {
                let raw = self.read_integer()?;
                let value = self.canonical_int(raw, marker_offset)?;
                self.emit_scalar(field_path, value, marker_offset)?;
            }
            0xCA => {
                self.take_u8()?;
                let bits = self.take_u32()?;
                let v = f64::from(f32::from_bits(bits));
                let value = CanonicalValue::from_f64(v)
                    .map_err(|_| unsupported_value("non-finite float32 value"))
                    .map_err(|e| e.at_byte_offset(marker_offset))?;
                self.emit_scalar(field_path, value, marker_offset)?;
            }
            0xCB => {
                self.take_u8()?;
                let bits = self.take_u64()?;
                let v = f64::from_bits(bits);
                let value = CanonicalValue::from_f64(v)
                    .map_err(|_| unsupported_value("non-finite float64 value"))
                    .map_err(|e| e.at_byte_offset(marker_offset))?;
                self.emit_scalar(field_path, value, marker_offset)?;
            }
            0xA0..=0xBF | 0xD9..=0xDB => {
                let text = self.read_str_value()?;
                self.emit_scalar(field_path, CanonicalValue::String(text), marker_offset)?;
            }
            0xC4..=0xC6 => match self.binary_values {
                BinaryValuePolicy::Reject => {
                    return Err(unsupported_value("binary values are rejected by policy")
                        .at_byte_offset(marker_offset));
                }
                BinaryValuePolicy::TaggedBase64 => {
                    let len = self.read_bin_len()?;
                    let bytes = self.take_bytes(len)?.to_vec();
                    let text = format!("base64:{}", crate::base64::encode(&bytes));
                    self.emit_scalar(field_path, CanonicalValue::String(text), marker_offset)?;
                }
            },
            0xC7..=0xC9 | 0xD4..=0xD8 => {
                let value = self.read_ext(marker, marker_offset)?;
                self.emit_scalar(field_path, value, marker_offset)?;
            }
            0xC0 => {
                self.take_u8()?;
                self.emit_scalar(field_path, CanonicalValue::Null, marker_offset)?;
            }
            0xC2 => {
                self.take_u8()?;
                self.emit_scalar(field_path, CanonicalValue::Bool(false), marker_offset)?;
            }
            0xC3 => {
                self.take_u8()?;
                self.emit_scalar(field_path, CanonicalValue::Bool(true), marker_offset)?;
            }
            0xC1 => {
                return Err(invalid_event(
                    "0xC1 is a reserved MessagePack marker and is never used",
                )
                .at_byte_offset(marker_offset));
            }
        }
        Ok(())
    }

    // -- scalar emission ---------------------------------------------------

    fn emit_scalar(
        &mut self,
        field_path: Option<Vec<u8>>,
        value: CanonicalValue,
        offset: usize,
    ) -> Result<(), QuaminaError> {
        self.field_count += 1;
        if self.field_count > self.limits.max_fields {
            return Err(limit_exceeded("max_fields exceeded").at_byte_offset(offset));
        }
        let bytes = value.matcher_bytes();
        self.allocated_bytes = self.allocated_bytes.saturating_add(bytes.len());
        if self.allocated_bytes > self.limits.max_total_allocated_bytes {
            return Err(limit_exceeded("max_total_allocated_bytes exceeded").at_byte_offset(offset));
        }
        if let Some(path) = field_path {
            if path.len() > self.limits.max_path_bytes {
                return Err(limit_exceeded("max_path_bytes exceeded").at_byte_offset(offset));
            }
            self.allocated_bytes = self.allocated_bytes.saturating_add(path.len());
            self.fields.push(OwnedField {
                is_number: value.is_number(),
                path,
                val: bytes,
                array_trail: self.array_trail.clone(),
            });
        }
        Ok(())
    }

    fn canonical_int(&self, value: i128, offset: usize) -> Result<CanonicalValue, QuaminaError> {
        match self.numbers {
            NumericPolicy::LosslessQuamina => {
                if !(-MAX_SAFE_INT..=MAX_SAFE_INT).contains(&value) {
                    return Err(unsupported_value("integer exceeds lossless numeric range")
                        .at_byte_offset(offset));
                }
                // Range-checked above: value fits in i64.
                #[allow(clippy::cast_possible_truncation)]
                Ok(CanonicalValue::from_i64(value as i64))
            }
        }
    }

    // -- resource limits -----------------------------------------------

    fn check_container_len(&self, len: usize, offset: usize) -> Result<(), QuaminaError> {
        crate::decoder_limits::check_container_len(
            &self.limits,
            EventFormat::MessagePack,
            len,
            offset,
        )
    }

    fn check_scalar_len(&self, len: usize, offset: usize) -> Result<(), QuaminaError> {
        crate::decoder_limits::check_scalar_len(&self.limits, EventFormat::MessagePack, len, offset)
    }

    fn check_depth(&self, new_depth: usize, offset: usize) -> Result<(), QuaminaError> {
        crate::decoder_limits::check_depth(
            &self.limits,
            EventFormat::MessagePack,
            new_depth,
            offset,
        )
    }

    fn alloc_array_id(&mut self, offset: usize) -> Result<i32, QuaminaError> {
        crate::decoder_limits::alloc_array_id(
            &mut self.next_array_id,
            EventFormat::MessagePack,
            offset,
        )
    }

    // -- map keys ------------------------------------------------------

    fn read_map_key(&mut self) -> Result<Vec<u8>, QuaminaError> {
        let offset = self.pos;
        let marker = self.peek_u8()?;
        match self.map_keys {
            MapKeyPolicy::TextOnly => match marker {
                0xA0..=0xBF | 0xD9 | 0xDA | 0xDB => {
                    let text = self.read_str_value()?;
                    let bytes = text.into_bytes();
                    self.allocated_bytes = self.allocated_bytes.saturating_add(bytes.len());
                    if self.allocated_bytes > self.limits.max_total_allocated_bytes {
                        return Err(limit_exceeded("max_total_allocated_bytes exceeded")
                            .at_byte_offset(offset));
                    }
                    Ok(bytes)
                }
                _ => Err(unsupported_map_key("map keys must be MessagePack strings")
                    .at_byte_offset(offset)),
            },
        }
    }

    // -- strings / binary ------------------------------------------------

    fn read_str_len(&mut self) -> Result<usize, QuaminaError> {
        let offset = self.pos;
        let marker = self.take_u8()?;
        let len = match marker {
            0xA0..=0xBF => usize::from(marker & 0x1F),
            0xD9 => usize::from(self.take_u8()?),
            0xDA => usize::from(self.take_u16()?),
            0xDB => self.take_u32()? as usize,
            _ => unreachable!("caller filtered to string markers"),
        };
        self.check_scalar_len(len, offset)?;
        Ok(len)
    }

    fn read_str_value(&mut self) -> Result<String, QuaminaError> {
        let offset = self.pos;
        let len = self.read_str_len()?;
        let bytes = self.take_bytes(len)?;
        String::from_utf8(bytes.to_vec())
            .map_err(|_| invalid_event("string is not valid UTF-8").at_byte_offset(offset))
    }

    fn read_bin_len(&mut self) -> Result<usize, QuaminaError> {
        let offset = self.pos;
        let marker = self.take_u8()?;
        let len = match marker {
            0xC4 => usize::from(self.take_u8()?),
            0xC5 => usize::from(self.take_u16()?),
            0xC6 => self.take_u32()? as usize,
            _ => unreachable!("caller filtered to binary markers"),
        };
        self.check_scalar_len(len, offset)?;
        Ok(len)
    }

    // -- extensions ------------------------------------------------------

    fn read_ext(&mut self, marker: u8, offset: usize) -> Result<CanonicalValue, QuaminaError> {
        self.take_u8()?; // consume the marker byte itself
        let len = match marker {
            0xD4 => 1,
            0xD5 => 2,
            0xD6 => 4,
            0xD7 => 8,
            0xD8 => 16,
            0xC7 => usize::from(self.take_u8()?),
            0xC8 => usize::from(self.take_u16()?),
            0xC9 => self.take_u32()? as usize,
            _ => unreachable!("caller filtered to extension markers"),
        };
        self.check_scalar_len(len, offset)?;
        let ext_type = self.take_i8()?;
        let data = self.take_bytes(len)?;
        if ext_type == -1 {
            match self.timestamps {
                MessagePackTimestampPolicy::CanonicalRfc3339 => decode_timestamp(data, offset),
            }
        } else {
            match self.extensions {
                ExtensionValuePolicy::RejectUnknown => {
                    Err(unsupported_feature("unknown MessagePack extension type")
                        .at_byte_offset(offset))
                }
            }
        }
    }

    // -- header readers ------------------------------------------------

    fn read_map_header(&mut self) -> Result<usize, QuaminaError> {
        let offset = self.pos;
        let marker = self.take_u8()?;
        match marker {
            0x80..=0x8F => Ok(usize::from(marker & 0x0F)),
            0xDE => Ok(usize::from(self.take_u16()?)),
            0xDF => Ok(self.take_u32()? as usize),
            _ => Err(invalid_event("expected a MessagePack map").at_byte_offset(offset)),
        }
    }

    fn read_array_header(&mut self) -> Result<usize, QuaminaError> {
        let offset = self.pos;
        let marker = self.take_u8()?;
        match marker {
            0x90..=0x9F => Ok(usize::from(marker & 0x0F)),
            0xDC => Ok(usize::from(self.take_u16()?)),
            0xDD => Ok(self.take_u32()? as usize),
            _ => Err(invalid_event("expected a MessagePack array").at_byte_offset(offset)),
        }
    }

    // -- integers ------------------------------------------------------

    fn read_integer(&mut self) -> Result<i128, QuaminaError> {
        let marker = self.take_u8()?;
        let value = match marker {
            0x00..=0x7F => i128::from(marker),
            0xE0..=0xFF => i128::from(marker.cast_signed()),
            0xCC => i128::from(self.take_u8()?),
            0xCD => i128::from(self.take_u16()?),
            0xCE => i128::from(self.take_u32()?),
            0xCF => i128::from(self.take_u64()?),
            0xD0 => i128::from(self.take_i8()?),
            0xD1 => i128::from(self.take_i16()?),
            0xD2 => i128::from(self.take_i32()?),
            0xD3 => i128::from(self.take_i64()?),
            _ => unreachable!("caller filtered to integer markers"),
        };
        Ok(value)
    }

    // -- byte-level primitives ------------------------------------------

    fn peek_u8(&self) -> Result<u8, QuaminaError> {
        self.data
            .get(self.pos)
            .copied()
            .ok_or_else(|| invalid_event("unexpected end of event").at_byte_offset(self.pos))
    }

    fn take_u8(&mut self) -> Result<u8, QuaminaError> {
        let byte = self.peek_u8()?;
        self.pos += 1;
        Ok(byte)
    }

    fn take_bytes(&mut self, len: usize) -> Result<&'a [u8], QuaminaError> {
        let start = self.pos;
        let end = start
            .checked_add(len)
            .ok_or_else(|| invalid_event("length overflow").at_byte_offset(start))?;
        let slice = self
            .data
            .get(start..end)
            .ok_or_else(|| invalid_event("unexpected end of event").at_byte_offset(start))?;
        self.pos = end;
        Ok(slice)
    }

    fn take_u16(&mut self) -> Result<u16, QuaminaError> {
        let b = self.take_bytes(2)?;
        Ok(u16::from_be_bytes([b[0], b[1]]))
    }

    fn take_u32(&mut self) -> Result<u32, QuaminaError> {
        let b = self.take_bytes(4)?;
        Ok(u32::from_be_bytes([b[0], b[1], b[2], b[3]]))
    }

    fn take_u64(&mut self) -> Result<u64, QuaminaError> {
        let b = self.take_bytes(8)?;
        Ok(u64::from_be_bytes([
            b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
        ]))
    }

    fn take_i8(&mut self) -> Result<i8, QuaminaError> {
        Ok(self.take_u8()?.cast_signed())
    }

    fn take_i16(&mut self) -> Result<i16, QuaminaError> {
        Ok(self.take_u16()?.cast_signed())
    }

    fn take_i32(&mut self) -> Result<i32, QuaminaError> {
        Ok(self.take_u32()?.cast_signed())
    }

    fn take_i64(&mut self) -> Result<i64, QuaminaError> {
        Ok(self.take_u64()?.cast_signed())
    }
}

// =============================================================================
// Timestamp extension (type -1) decoding
// =============================================================================

/// Decode the MessagePack timestamp extension payload (4, 8, or 12 bytes;
/// see <https://github.com/msgpack/msgpack/blob/master/timestamp-spec.md>)
/// into an RFC 3339 UTC string.
fn decode_timestamp(data: &[u8], offset: usize) -> Result<CanonicalValue, QuaminaError> {
    let (seconds, nanos): (i64, u32) = match data.len() {
        4 => (
            i64::from(u32::from_be_bytes([data[0], data[1], data[2], data[3]])),
            0,
        ),
        8 => {
            let combined = u64::from_be_bytes([
                data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
            ]);
            let nanos = ((combined >> 34) & 0x3FFF_FFFF) as u32;
            // Masked to the low 34 bits above, which always fits in i64.
            #[allow(clippy::cast_possible_wrap)]
            let secs = (combined & 0x0000_0003_FFFF_FFFF) as i64;
            (secs, nanos)
        }
        12 => {
            let nanos = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
            let secs = i64::from_be_bytes([
                data[4], data[5], data[6], data[7], data[8], data[9], data[10], data[11],
            ]);
            (secs, nanos)
        }
        _ => {
            return Err(
                invalid_event("MessagePack timestamp extension has an unsupported length")
                    .at_byte_offset(offset),
            );
        }
    };
    if nanos >= 1_000_000_000 {
        return Err(
            invalid_event("MessagePack timestamp nanoseconds out of range").at_byte_offset(offset),
        );
    }
    Ok(CanonicalValue::String(crate::civil_date::format_rfc3339(
        seconds, nanos,
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::segments_tree::SegmentsTree;

    fn tree(paths: &[&str]) -> SegmentsTree {
        let mut t = SegmentsTree::new();
        for p in paths {
            t.add(p);
        }
        t
    }

    #[test]
    fn rfc3339_epoch_and_one_second_after() {
        assert_eq!(
            crate::civil_date::format_rfc3339(0, 0),
            "1970-01-01T00:00:00Z"
        );
        assert_eq!(
            crate::civil_date::format_rfc3339(1, 0),
            "1970-01-01T00:00:01Z"
        );
        assert_eq!(
            crate::civil_date::format_rfc3339(86_400, 0),
            "1970-01-02T00:00:00Z"
        );
    }

    #[test]
    fn simple_map_decodes_expected_field() {
        let wire = [0x81, 0xa1, b'x', 0x2a]; // {"x": 42}
        let t = tree(&["x"]);
        let mut flattener = MessagePackFlattener::new();
        let fields = flattener.flatten(&wire, &t).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path, b"x");
        assert_eq!(fields[0].val, b"42");
        assert!(fields[0].is_number);
    }

    #[test]
    fn empty_event_errors_without_panicking() {
        let t = tree(&["x"]);
        let mut flattener = MessagePackFlattener::new();
        assert!(flattener.flatten(&[], &t).is_err());
    }
}
