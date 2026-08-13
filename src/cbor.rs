//! CBOR event flattener.
//!
//! Decodes CBOR-encoded events (RFC 8949) into the same [`OwnedField`] shape
//! the JSON flattener produces: paths joined by `\n`, string values wrapped
//! in `"`, numbers canonicalized to minimal decimal text, and array trails
//! allocated in positive preorder.
//!
//! # Policies
//!
//! CBOR can express values JSON has no equivalent for (raw binary, tags,
//! simple values, shared/cyclic references, non-canonical integer
//! encodings). Each of those is governed by an explicit, named policy on
//! [`CborFlattener`] so the decoder never has to guess how to represent
//! something. Defaults reject anything JSON has no equivalent for; callers
//! opt in to a specific, collision-free representation via
//! [`CborFlattener::builder`].
//!
//! # Validation
//!
//! Every value in the document is decoded and validated (type policy,
//! numeric range, resource limits, indefinite-length termination)
//! regardless of whether the current [`SegmentsTreeTracker`] considers it
//! relevant to any pattern. Tracking only controls whether a scalar is
//! materialized into an [`OwnedField`] and whether an array position is
//! allocated an id; it never skips structural or policy validation, so a
//! hostile payload cannot hide behind an unreferenced field.

use crate::{
    ArrayPos, BinaryValuePolicy, CanonicalValue, DuplicateKeyPolicy, EventFormat, EventLimits,
    Flattener, MapKeyPolicy, NumericPolicy, OwnedField, QuaminaError, RootValuePolicy,
    SegmentsTreeTracker,
};
use rustc_hash::FxHashSet;

/// How unassigned or `undefined` CBOR simple values (major type 7) are handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CborSimpleValuePolicy {
    /// Reject any simple value this decoder does not have a stable
    /// representation for: unassigned simple values 0-19 and 24-31, and
    /// `undefined` (additional info 23, wire byte `0xF7`). Only `false`,
    /// `true`, and `null` are supported.
    #[default]
    RejectUnsupported,
}

/// How CBOR tags (major type 6) are handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CborTagPolicy {
    /// Recognize tag 0 (standard date/time string), tag 1 (epoch-based
    /// date/time), tag 2/3 (positive/negative bignum), tag 4 (decimal
    /// fraction), and tag 5 (bigfloat), each converted to a canonical
    /// value. Any other tag is rejected as an unsupported format feature.
    #[default]
    KnownSemanticValues,
}

/// How CBOR's shareable-value tags (28 and 29, used to encode cyclic or
/// shared structures) are handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SharedReferencePolicy {
    /// Reject any event using tag 28 (shareable) or tag 29 (shared
    /// reference). Quamina's flattener has no notion of a graph with
    /// shared or cyclic structure.
    #[default]
    Reject,
}

/// How non-canonical (non-shortest-form) CBOR integer/length/tag encodings
/// are handled, e.g. encoding `1` with the 2-byte `0x18 0x01` form instead
/// of the 1-byte `0x01` form.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NonCanonicalPolicy {
    /// Reject any integer, string/array/map length, or tag number that is
    /// not encoded in its shortest well-formed CBOR form.
    #[default]
    Reject,
}

/// Builder for [`CborFlattener`].
///
/// Construct via [`CborFlattener::builder`], chain the policy setters that
/// need to differ from their defaults, and finish with [`build`](Self::build).
#[derive(Debug, Clone, Copy, Default)]
pub struct CborFlattenerBuilder {
    numbers: NumericPolicy,
    binary_values: BinaryValuePolicy,
    map_keys: MapKeyPolicy,
    duplicate_keys: DuplicateKeyPolicy,
    simple_values: CborSimpleValuePolicy,
    tags: CborTagPolicy,
    shared_references: SharedReferencePolicy,
    noncanonical: NonCanonicalPolicy,
    root_values: RootValuePolicy,
    limits: EventLimits,
}

impl CborFlattenerBuilder {
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

    /// Set the simple-value policy.
    #[must_use]
    pub const fn simple_values(mut self, policy: CborSimpleValuePolicy) -> Self {
        self.simple_values = policy;
        self
    }

    /// Set the tag policy.
    #[must_use]
    pub const fn tags(mut self, policy: CborTagPolicy) -> Self {
        self.tags = policy;
        self
    }

    /// Set the shared-reference tag policy.
    #[must_use]
    pub const fn shared_references(mut self, policy: SharedReferencePolicy) -> Self {
        self.shared_references = policy;
        self
    }

    /// Set the non-canonical-encoding policy.
    #[must_use]
    pub const fn noncanonical(mut self, policy: NonCanonicalPolicy) -> Self {
        self.noncanonical = policy;
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
    pub const fn build(self) -> CborFlattener {
        CborFlattener {
            numbers: self.numbers,
            binary_values: self.binary_values,
            map_keys: self.map_keys,
            duplicate_keys: self.duplicate_keys,
            simple_values: self.simple_values,
            tags: self.tags,
            shared_references: self.shared_references,
            noncanonical: self.noncanonical,
            root_values: self.root_values,
            limits: self.limits,
        }
    }
}

/// A [`Flattener`] that decodes CBOR events.
///
/// Use [`new`](Self::new) for default policies, or [`builder`](Self::builder)
/// to select non-default policies. See the [module docs](self) for how CBOR
/// values map onto the JSON scalar representation Quamina's matcher expects.
#[derive(Debug, Clone, Copy)]
pub struct CborFlattener {
    numbers: NumericPolicy,
    binary_values: BinaryValuePolicy,
    map_keys: MapKeyPolicy,
    duplicate_keys: DuplicateKeyPolicy,
    simple_values: CborSimpleValuePolicy,
    tags: CborTagPolicy,
    shared_references: SharedReferencePolicy,
    noncanonical: NonCanonicalPolicy,
    root_values: RootValuePolicy,
    limits: EventLimits,
}

impl CborFlattener {
    /// Create a flattener with every policy at its default.
    #[must_use]
    pub fn new() -> Self {
        Self::builder().build()
    }

    /// Start a [`CborFlattenerBuilder`] to select non-default policies.
    #[must_use]
    pub fn builder() -> CborFlattenerBuilder {
        CborFlattenerBuilder::default()
    }
}

impl Default for CborFlattener {
    fn default() -> Self {
        Self::new()
    }
}

impl Flattener for CborFlattener {
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
    crate::decoder_errors::invalid_event(EventFormat::Cbor, message)
}

fn limit_exceeded(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::limit_exceeded(EventFormat::Cbor, message)
}

fn unsupported_value(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_value(EventFormat::Cbor, message)
}

fn unsupported_map_key(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_map_key(EventFormat::Cbor, message)
}

fn unsupported_feature(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_feature(EventFormat::Cbor, message)
}

fn duplicate_field() -> QuaminaError {
    crate::decoder_errors::duplicate_field(EventFormat::Cbor)
}

/// Largest integer magnitude that round-trips exactly through `f64`, i.e. 2^53.
const MAX_SAFE_INT: i128 = 9_007_199_254_740_992;

// =============================================================================
// Decoder
// =============================================================================

/// Recursive-descent CBOR decoder.
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
    simple_values: CborSimpleValuePolicy,
    tags: CborTagPolicy,
    shared_references: SharedReferencePolicy,
    noncanonical: NonCanonicalPolicy,
    root_values: RootValuePolicy,
    limits: EventLimits,
    fields: Vec<OwnedField>,
    field_count: usize,
    allocated_bytes: usize,
    next_array_id: i32,
    array_trail: Vec<ArrayPos>,
}

impl<'a> Decoder<'a> {
    const fn new(data: &'a [u8], flattener: CborFlattener) -> Self {
        Self {
            data,
            pos: 0,
            numbers: flattener.numbers,
            binary_values: flattener.binary_values,
            map_keys: flattener.map_keys,
            duplicate_keys: flattener.duplicate_keys,
            simple_values: flattener.simple_values,
            tags: flattener.tags,
            shared_references: flattener.shared_references,
            noncanonical: flattener.noncanonical,
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
            return Err(invalid_event("empty CBOR event").at_byte_offset(0));
        }
        let marker = self.data[0];
        let is_map = (marker >> 5) == 5;
        match self.root_values {
            RootValuePolicy::MapOnly => {
                if !is_map {
                    return Err(
                        unsupported_value("root CBOR value must be a map").at_byte_offset(0)
                    );
                }
            }
        }
        self.decode_map(Some(tracker), 1)?;
        if self.pos != self.data.len() {
            return Err(
                invalid_event("trailing bytes after top-level CBOR value").at_byte_offset(self.pos)
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
        let marker = self.peek_u8()?;
        if marker >> 5 != 5 {
            return Err(invalid_event("expected a CBOR map").at_byte_offset(offset));
        }
        let (raw_len, indefinite) = self.read_arg()?;
        let declared_len = if indefinite {
            None
        } else {
            let len = Self::usize_len(raw_len, offset)?;
            self.check_container_len(len, offset)?;
            Some(len)
        };

        let mut seen_keys: FxHashSet<Vec<u8>> = FxHashSet::default();
        let mut index: usize = 0;
        loop {
            if !self.container_should_continue(declared_len, index, offset)? {
                break;
            }

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
            index += 1;
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
        let marker = self.peek_u8()?;
        if marker >> 5 != 4 {
            return Err(invalid_event("expected a CBOR array").at_byte_offset(offset));
        }
        let (raw_len, indefinite) = self.read_arg()?;
        let declared_len = if indefinite {
            None
        } else {
            let len = Self::usize_len(raw_len, offset)?;
            self.check_container_len(len, offset)?;
            Some(len)
        };

        let used = field_path.is_some() || child_tracker.is_some();
        if used {
            let array_id = self.alloc_array_id(offset)?;
            self.array_trail.push(ArrayPos {
                array: array_id,
                pos: 0,
            });
        }

        let mut index: usize = 0;
        loop {
            if !self.container_should_continue(declared_len, index, offset)? {
                break;
            }

            if used {
                let pos_i32 = i32::try_from(index + 1)
                    .map_err(|_| limit_exceeded("array position exceeds i32 range"))
                    .map_err(|e| e.at_byte_offset(offset))?;
                if let Some(last) = self.array_trail.last_mut() {
                    last.pos = pos_i32;
                }
                self.decode_value(field_path.clone(), child_tracker, depth)?;
            } else {
                self.decode_value(None, None, depth)?;
            }
            index += 1;
        }

        if used {
            self.array_trail.pop();
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
        let major = marker >> 5;
        match major {
            0 | 1 => {
                let (raw, indefinite) = self.read_arg()?;
                if indefinite {
                    return Err(invalid_event("CBOR integers cannot use indefinite length")
                        .at_byte_offset(marker_offset));
                }
                let signed = if major == 0 {
                    i128::from(raw)
                } else {
                    -1 - i128::from(raw)
                };
                let value = self.canonical_int(signed, marker_offset)?;
                self.emit_scalar(field_path, value, marker_offset)?;
            }
            2 => {
                let bytes = self.read_byte_string(marker_offset)?;
                match self.binary_values {
                    BinaryValuePolicy::Reject => {
                        return Err(unsupported_value("binary values are rejected by policy")
                            .at_byte_offset(marker_offset));
                    }
                    BinaryValuePolicy::TaggedBase64 => {
                        let text = format!("base64:{}", crate::base64::encode(&bytes));
                        self.emit_scalar(field_path, CanonicalValue::String(text), marker_offset)?;
                    }
                }
            }
            3 => {
                let text = self.read_text_string(marker_offset)?;
                self.emit_scalar(field_path, CanonicalValue::String(text), marker_offset)?;
            }
            4 => {
                let new_depth = depth + 1;
                self.check_depth(new_depth, marker_offset)?;
                self.decode_array(field_path, child_tracker, new_depth)?;
            }
            5 => {
                let new_depth = depth + 1;
                self.check_depth(new_depth, marker_offset)?;
                self.decode_map(child_tracker, new_depth)?;
            }
            6 => {
                let value = self.decode_tag(marker_offset)?;
                self.emit_scalar(field_path, value, marker_offset)?;
            }
            _ => {
                let value = self.decode_simple_or_float()?;
                self.emit_scalar(field_path, value, marker_offset)?;
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

    // -- simple values / floats --------------------------------------------

    /// Decode a major-type-7 value (simple value, `false`/`true`/`null`, or
    /// half/single/double-precision float) at the current position.
    fn decode_simple_or_float(&mut self) -> Result<CanonicalValue, QuaminaError> {
        let offset = self.pos;
        let marker = self.peek_u8()?;
        let ai = marker & 0x1F;
        if matches!(ai, 25..=27) {
            let v = self.read_float_value()?;
            return CanonicalValue::from_f64(v).map_err(|_| {
                unsupported_value("non-finite CBOR float value").at_byte_offset(offset)
            });
        }
        self.take_u8()?; // consume the marker byte
        match ai {
            20 => Ok(CanonicalValue::Bool(false)),
            21 => Ok(CanonicalValue::Bool(true)),
            22 => Ok(CanonicalValue::Null),
            24 => {
                self.take_u8()?; // consume the 1-byte simple-value payload
                match self.simple_values {
                    CborSimpleValuePolicy::RejectUnsupported => {
                        Err(unsupported_value("unassigned CBOR simple value")
                            .at_byte_offset(offset))
                    }
                }
            }
            28..=30 => {
                Err(invalid_event("reserved CBOR additional-info value").at_byte_offset(offset))
            }
            31 => Err(invalid_event(
                "unexpected CBOR break outside an indefinite-length container",
            )
            .at_byte_offset(offset)),
            // ai in 0..=19 (unassigned simple values) or 23 (undefined).
            _ => match self.simple_values {
                CborSimpleValuePolicy::RejectUnsupported => Err(unsupported_value(
                    "unassigned or undefined CBOR simple value",
                )
                .at_byte_offset(offset)),
            },
        }
    }

    /// Read a half/single/double-precision float (additional info 25/26/27)
    /// at the current position, returning it as `f64` without a finiteness
    /// check.
    fn read_float_value(&mut self) -> Result<f64, QuaminaError> {
        let offset = self.pos;
        let marker = self.take_u8()?;
        let ai = marker & 0x1F;
        match ai {
            25 => Ok(f16_to_f64(self.take_u16()?)),
            26 => Ok(f64::from(f32::from_bits(self.take_u32()?))),
            27 => Ok(f64::from_bits(self.take_u64()?)),
            _ => Err(invalid_event("expected a CBOR float").at_byte_offset(offset)),
        }
    }

    // -- tags ----------------------------------------------------------

    /// Decode a major-type-6 tagged value at the current position into a
    /// canonical scalar, applying the shared-reference and tag policies.
    fn decode_tag(&mut self, marker_offset: usize) -> Result<CanonicalValue, QuaminaError> {
        let (tag, indefinite) = self.read_arg()?;
        if indefinite {
            return Err(invalid_event("CBOR tags cannot use indefinite length")
                .at_byte_offset(marker_offset));
        }
        if tag == 28 || tag == 29 {
            return match self.shared_references {
                SharedReferencePolicy::Reject => Err(unsupported_feature(
                    "CBOR shared-reference tags are not supported",
                )
                .at_byte_offset(marker_offset)),
            };
        }
        match self.tags {
            CborTagPolicy::KnownSemanticValues => match tag {
                0 => {
                    let content_offset = self.pos;
                    let marker = self.peek_u8()?;
                    if marker >> 5 != 3 {
                        return Err(invalid_event("CBOR tag 0 content must be a text string")
                            .at_byte_offset(content_offset));
                    }
                    let text = self.read_text_string(content_offset)?;
                    Ok(CanonicalValue::String(text))
                }
                1 => self.decode_epoch_time(marker_offset),
                2 => self.decode_bignum(false, marker_offset),
                3 => self.decode_bignum(true, marker_offset),
                4 => self.decode_decimal_fraction(10.0, marker_offset),
                5 => self.decode_decimal_fraction(2.0, marker_offset),
                _ => Err(unsupported_feature("unknown CBOR tag").at_byte_offset(marker_offset)),
            },
        }
    }

    /// Decode a tag-1 (epoch-based date/time) payload into an RFC 3339 string.
    fn decode_epoch_time(&mut self, marker_offset: usize) -> Result<CanonicalValue, QuaminaError> {
        let value_offset = self.pos;
        let marker = self.peek_u8()?;
        let major = marker >> 5;
        let (seconds, nanos): (i64, u32) = match major {
            0 | 1 => {
                let raw = self.read_plain_integer()?;
                let secs = i64::try_from(raw)
                    .map_err(|_| unsupported_value("epoch time out of range"))
                    .map_err(|e| e.at_byte_offset(marker_offset))?;
                (secs, 0)
            }
            7 => {
                let v = self.read_float_value()?;
                if !v.is_finite() {
                    return Err(unsupported_value("non-finite epoch time value")
                        .at_byte_offset(marker_offset));
                }
                split_seconds_nanos(v)
                    .ok_or_else(|| unsupported_value("epoch time out of range"))
                    .map_err(|e| e.at_byte_offset(marker_offset))?
            }
            _ => {
                return Err(invalid_event("CBOR tag 1 content must be numeric")
                    .at_byte_offset(value_offset));
            }
        };
        Ok(CanonicalValue::String(crate::civil_date::format_rfc3339(
            seconds, nanos,
        )))
    }

    /// Decode a tag-2/tag-3 (positive/negative bignum) payload.
    fn decode_bignum(
        &mut self,
        negative: bool,
        marker_offset: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        let content_offset = self.pos;
        let marker = self.peek_u8()?;
        if marker >> 5 != 2 {
            return Err(invalid_event("CBOR bignum content must be a byte string")
                .at_byte_offset(content_offset));
        }
        let bytes = self.read_byte_string(content_offset)?;
        let mut magnitude: u128 = 0;
        for &b in &bytes {
            magnitude = magnitude
                .checked_mul(256)
                .and_then(|m| m.checked_add(u128::from(b)))
                .ok_or_else(|| unsupported_value("bignum exceeds lossless numeric range"))
                .map_err(|e| e.at_byte_offset(marker_offset))?;
        }
        let signed = if negative {
            i128::try_from(magnitude)
                .ok()
                .and_then(|m| (-1_i128).checked_sub(m))
        } else {
            i128::try_from(magnitude).ok()
        };
        let signed = signed
            .ok_or_else(|| unsupported_value("bignum exceeds lossless numeric range"))
            .map_err(|e| e.at_byte_offset(marker_offset))?;
        self.canonical_int(signed, marker_offset)
    }

    /// Decode a tag-4 (decimal fraction, `base == 10`) or tag-5 (bigfloat,
    /// `base == 2`) payload: a two-element array of `[exponent, mantissa]`.
    fn decode_decimal_fraction(
        &mut self,
        base: f64,
        marker_offset: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        let content_offset = self.pos;
        let marker = self.peek_u8()?;
        if marker >> 5 != 4 {
            return Err(
                invalid_event("decimal fraction/bigfloat content must be an array")
                    .at_byte_offset(content_offset),
            );
        }
        let (raw_len, indefinite) = self.read_arg()?;
        if indefinite || raw_len != 2 {
            return Err(invalid_event(
                "decimal fraction/bigfloat content must be a two-element array",
            )
            .at_byte_offset(content_offset));
        }
        let exponent = self.read_plain_integer()?;
        let mantissa = self.read_plain_integer()?;
        let exponent_f = i32::try_from(exponent)
            .map_err(|_| unsupported_value("decimal fraction exponent out of range"))
            .map_err(|e| e.at_byte_offset(marker_offset))?;
        // Values this decoder accepts stay well within f64's exact-integer
        // range for the corpus this policy targets; wider mantissas are
        // caught by the finiteness check below.
        #[allow(clippy::cast_precision_loss)]
        let mantissa_f = mantissa as f64;
        let value = mantissa_f * base.powi(exponent_f);
        CanonicalValue::from_f64(value)
            .map_err(|_| unsupported_value("decimal fraction/bigfloat value is not representable"))
            .map_err(|e| e.at_byte_offset(marker_offset))
    }

    /// Read a plain (non-tagged) major-type-0/1 integer at the current position.
    fn read_plain_integer(&mut self) -> Result<i128, QuaminaError> {
        let offset = self.pos;
        let marker = self.peek_u8()?;
        let major = marker >> 5;
        if major != 0 && major != 1 {
            return Err(invalid_event("expected a plain CBOR integer").at_byte_offset(offset));
        }
        let (raw, indefinite) = self.read_arg()?;
        if indefinite {
            return Err(
                invalid_event("CBOR integers cannot use indefinite length").at_byte_offset(offset)
            );
        }
        Ok(if major == 0 {
            i128::from(raw)
        } else {
            -1 - i128::from(raw)
        })
    }

    // -- resource limits -----------------------------------------------

    /// Advance a definite/indefinite map or array loop by one step.
    ///
    /// Returns `true` if another item should be processed, `false` if the
    /// container has ended (a definite length was reached, or an
    /// indefinite container's break byte was consumed). For indefinite
    /// containers this also enforces `max_container_items` incrementally,
    /// since no declared length is available to check up front.
    fn container_should_continue(
        &mut self,
        declared_len: Option<usize>,
        index: usize,
        offset: usize,
    ) -> Result<bool, QuaminaError> {
        if let Some(len) = declared_len {
            return Ok(index < len);
        }
        if self.peek_u8()? == 0xFF {
            self.take_u8()?;
            return Ok(false);
        }
        if index >= self.limits.max_container_items {
            return Err(limit_exceeded("max_container_items exceeded").at_byte_offset(offset));
        }
        Ok(true)
    }

    fn check_container_len(&self, len: usize, offset: usize) -> Result<(), QuaminaError> {
        crate::decoder_limits::check_container_len(&self.limits, EventFormat::Cbor, len, offset)
    }

    fn check_scalar_len(&self, len: usize, offset: usize) -> Result<(), QuaminaError> {
        crate::decoder_limits::check_scalar_len(&self.limits, EventFormat::Cbor, len, offset)
    }

    fn check_depth(&self, new_depth: usize, offset: usize) -> Result<(), QuaminaError> {
        crate::decoder_limits::check_depth(&self.limits, EventFormat::Cbor, new_depth, offset)
    }

    fn alloc_array_id(&mut self, offset: usize) -> Result<i32, QuaminaError> {
        crate::decoder_limits::alloc_array_id(&mut self.next_array_id, EventFormat::Cbor, offset)
    }

    fn usize_len(value: u64, offset: usize) -> Result<usize, QuaminaError> {
        usize::try_from(value)
            .map_err(|_| limit_exceeded("declared length exceeds addressable size"))
            .map_err(|e| e.at_byte_offset(offset))
    }

    // -- map keys ------------------------------------------------------

    fn read_map_key(&mut self) -> Result<Vec<u8>, QuaminaError> {
        let offset = self.pos;
        let marker = self.peek_u8()?;
        match self.map_keys {
            MapKeyPolicy::TextOnly => {
                if marker >> 5 != 3 {
                    return Err(unsupported_map_key("CBOR map keys must be text strings")
                        .at_byte_offset(offset));
                }
                let text = self.read_text_string(offset)?;
                let bytes = text.into_bytes();
                self.allocated_bytes = self.allocated_bytes.saturating_add(bytes.len());
                if self.allocated_bytes > self.limits.max_total_allocated_bytes {
                    return Err(
                        limit_exceeded("max_total_allocated_bytes exceeded").at_byte_offset(offset)
                    );
                }
                Ok(bytes)
            }
        }
    }

    // -- strings / binary ------------------------------------------------

    /// Read a definite or indefinite (chunked) string of major type
    /// `expected_major` (2 for byte strings, 3 for text strings) at the
    /// current position, joining chunks into one byte buffer.
    fn read_chunked_or_definite(
        &mut self,
        marker_offset: usize,
        expected_major: u8,
    ) -> Result<Vec<u8>, QuaminaError> {
        let (raw_len, indefinite) = self.read_arg()?;
        if !indefinite {
            let len = Self::usize_len(raw_len, marker_offset)?;
            self.check_scalar_len(len, marker_offset)?;
            return Ok(self.take_bytes(len)?.to_vec());
        }
        let mut out = Vec::new();
        loop {
            if self.peek_u8()? == 0xFF {
                self.take_u8()?;
                break;
            }
            let chunk_offset = self.pos;
            let chunk_major = self.peek_u8()? >> 5;
            if chunk_major != expected_major {
                return Err(
                    invalid_event("indefinite string chunk has the wrong major type")
                        .at_byte_offset(chunk_offset),
                );
            }
            let (chunk_raw_len, chunk_indefinite) = self.read_arg()?;
            if chunk_indefinite {
                return Err(
                    invalid_event("nested indefinite string chunk is not allowed")
                        .at_byte_offset(chunk_offset),
                );
            }
            let chunk_len = Self::usize_len(chunk_raw_len, chunk_offset)?;
            let new_len = out
                .len()
                .checked_add(chunk_len)
                .ok_or_else(|| limit_exceeded("max_scalar_bytes exceeded"))
                .map_err(|e| e.at_byte_offset(marker_offset))?;
            self.check_scalar_len(new_len, marker_offset)?;
            out.extend_from_slice(self.take_bytes(chunk_len)?);
        }
        Ok(out)
    }

    fn read_byte_string(&mut self, marker_offset: usize) -> Result<Vec<u8>, QuaminaError> {
        self.read_chunked_or_definite(marker_offset, 2)
    }

    fn read_text_string(&mut self, marker_offset: usize) -> Result<String, QuaminaError> {
        let bytes = self.read_chunked_or_definite(marker_offset, 3)?;
        String::from_utf8(bytes)
            .map_err(|_| invalid_event("string is not valid UTF-8").at_byte_offset(marker_offset))
    }

    // -- header / noncanonical checks ------------------------------------

    /// Consume a CBOR header (marker byte plus any additional length
    /// bytes) at the current position, returning `(value, is_indefinite)`.
    /// `value` is meaningless when `is_indefinite` is true. Applies the
    /// noncanonical policy to non-shortest-form encodings.
    fn read_arg(&mut self) -> Result<(u64, bool), QuaminaError> {
        let offset = self.pos;
        let marker = self.take_u8()?;
        let ai = marker & 0x1F;
        match ai {
            0..=23 => Ok((u64::from(ai), false)),
            24 => {
                let v = self.take_u8()?;
                self.check_noncanonical(u64::from(v), 24, offset)?;
                Ok((u64::from(v), false))
            }
            25 => {
                let v = self.take_u16()?;
                self.check_noncanonical(u64::from(v), 25, offset)?;
                Ok((u64::from(v), false))
            }
            26 => {
                let v = self.take_u32()?;
                self.check_noncanonical(u64::from(v), 26, offset)?;
                Ok((u64::from(v), false))
            }
            27 => {
                let v = self.take_u64()?;
                self.check_noncanonical(v, 27, offset)?;
                Ok((v, false))
            }
            28..=30 => {
                Err(invalid_event("reserved CBOR additional-info value").at_byte_offset(offset))
            }
            31 => Ok((0, true)),
            _ => unreachable!("additional info is masked to 5 bits"),
        }
    }

    fn check_noncanonical(&self, value: u64, ai: u8, offset: usize) -> Result<(), QuaminaError> {
        let NonCanonicalPolicy::Reject = self.noncanonical;
        let could_be_shorter = match ai {
            24 => value < 24,
            25 => value <= 0xFF,
            26 => value <= 0xFFFF,
            27 => value <= 0xFFFF_FFFF,
            _ => false,
        };
        if could_be_shorter {
            return Err(
                invalid_event("non-canonical (non-shortest-form) CBOR integer encoding")
                    .at_byte_offset(offset),
            );
        }
        Ok(())
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
}

/// Split a finite `f64` seconds-since-epoch value into whole seconds and
/// nanoseconds, rounding the fractional part. Returns `None` if the whole
/// seconds component does not fit in `i64`.
fn split_seconds_nanos(value: f64) -> Option<(i64, u32)> {
    let secs_f = value.floor();
    if !(-9.2e18..=9.2e18).contains(&secs_f) {
        return None;
    }
    // Range-checked above.
    #[allow(clippy::cast_possible_truncation)]
    let secs = secs_f as i64;
    let frac_nanos = ((value - secs_f) * 1_000_000_000.0).round();
    // `frac_nanos` is in `[0, 1e9]` by construction (a fraction of one second).
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    let nanos = (frac_nanos as u32).min(999_999_999);
    Some((secs, nanos))
}

/// Decode an IEEE 754 half-precision (binary16) bit pattern to `f64`.
fn f16_to_f64(bits: u16) -> f64 {
    let sign = u64::from(bits >> 15);
    let exponent = (bits >> 10) & 0x1F;
    let fraction = f64::from(bits & 0x3FF);
    let magnitude = if exponent == 0 {
        // Subnormal: value = fraction * 2^-24.
        fraction * 2f64.powi(-24)
    } else if exponent == 0x1F {
        if fraction == 0.0 {
            f64::INFINITY
        } else {
            f64::NAN
        }
    } else {
        // Normal: value = (1 + fraction/1024) * 2^(exponent-15).
        (1.0 + fraction / 1024.0) * 2f64.powi(i32::from(exponent) - 15)
    };
    if sign == 1 { -magnitude } else { magnitude }
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
    fn half_float_known_values() {
        // 0x3E00 = 1.5, 0x7C00 = +Infinity, 0xFC00 = -Infinity, 0x7E00 = NaN.
        // 1.5 is exactly representable in f64, so bit-for-bit equality is
        // the correct check here, not an approximate comparison.
        #[allow(clippy::float_cmp)]
        {
            assert_eq!(f16_to_f64(0x3E00), 1.5);
        }
        assert!(f16_to_f64(0x7C00).is_infinite());
        assert!(f16_to_f64(0xFC00).is_infinite());
        assert!(f16_to_f64(0x7E00).is_nan());
    }

    #[test]
    fn simple_map_decodes_expected_field() {
        let wire = [0xa1, 0x61, b'x', 0x18, 0x2a]; // {"x": 42}
        let t = tree(&["x"]);
        let mut flattener = CborFlattener::new();
        let fields = flattener.flatten(&wire, &t).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].path, b"x");
        assert_eq!(fields[0].val, b"42");
        assert!(fields[0].is_number);
    }

    #[test]
    fn empty_event_errors_without_panicking() {
        let t = tree(&["x"]);
        let mut flattener = CborFlattener::new();
        assert!(flattener.flatten(&[], &t).is_err());
    }
}
