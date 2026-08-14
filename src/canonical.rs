//! Shared decoder boundary: format-neutral field paths, canonicalized
//! scalar values, array-trail bookkeeping, and tracker-aware field
//! construction used by non-JSON flatteners.
//!
//! This module is the proposed shared foundation described in
//! `tests/contracts/README.md`. It is validated directly by the
//! `core-boundary` contract; individual format flatteners are free to build
//! [`OwnedField`](crate::OwnedField) values directly when that is simpler,
//! reusing the canonicalization helpers here where useful.

use crate::decoder_errors::{
    ambiguous_event_path, conflicting_array_id, duplicate_field, invalid_canonical_field,
    invalid_event_path, limit_exceeded,
};
use crate::{ErrorLocation, EventFormat, EventLimits, QuaminaError};
use rustc_hash::{FxHashMap, FxHashSet};

/// An ordered sequence of UTF-8 path segments identifying a field, distinct
/// from the legacy newline-joined byte path used internally by the matcher.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FieldPath {
    segments: Vec<String>,
}

impl FieldPath {
    /// Build a path from an ordered sequence of segments.
    #[must_use]
    pub fn from_segments<'a, I: IntoIterator<Item = &'a str>>(segments: I) -> Self {
        Self {
            segments: segments.into_iter().map(str::to_owned).collect(),
        }
    }

    /// Iterate the path's segments in order.
    pub fn segments(&self) -> impl Iterator<Item = &str> {
        self.segments.iter().map(String::as_str)
    }

    /// Number of segments in this path.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.segments.len()
    }

    /// True if this path has no segments.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }

    /// Encode this path unambiguously for the matcher: segments are joined
    /// by a raw newline separator, while any backslash or newline byte
    /// embedded within a segment is escaped first. This keeps a two-segment
    /// path distinct from a one-segment path containing a literal newline.
    #[must_use]
    pub fn matcher_path_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        for (i, segment) in self.segments.iter().enumerate() {
            if i > 0 {
                out.push(b'\n');
            }
            for &byte in segment.as_bytes() {
                match byte {
                    b'\\' => {
                        out.push(b'\\');
                        out.push(b'\\');
                    }
                    b'\n' => {
                        out.push(b'\\');
                        out.push(b'n');
                    }
                    other => out.push(other),
                }
            }
        }
        out
    }
}

/// A canonicalized scalar value: the durable representation decoders
/// construct instead of hand-writing matcher bytes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CanonicalValue {
    /// Decoded Unicode text.
    String(String),
    /// A boolean value.
    Bool(bool),
    /// An explicit null/absent-but-present value.
    Null,
    /// A finite, losslessly canonicalized number, stored as its shortest
    /// canonical decimal text.
    Number(String),
}

impl CanonicalValue {
    /// Parse and canonicalize a decimal/exponent numeric string. Integer,
    /// decimal, and exponent spellings of the same value canonicalize to
    /// identical bytes. Non-finite values and malformed text are rejected.
    ///
    /// # Errors
    /// Returns [`QuaminaError::InvalidCanonicalField`] if `text` is not a
    /// well-formed finite decimal number.
    pub fn number(text: &str) -> Result<Self, QuaminaError> {
        validate_number_syntax(text)?;
        let value: f64 = text.parse().map_err(|_| invalid_number(text))?;
        if !value.is_finite() {
            return Err(invalid_number(text));
        }
        Ok(Self::Number(canonicalize_number(value)))
    }

    /// Build a canonical number directly from a finite `f64`.
    ///
    /// # Errors
    /// Returns [`QuaminaError::InvalidCanonicalField`] if `value` is not finite.
    pub fn from_f64(value: f64) -> Result<Self, QuaminaError> {
        if !value.is_finite() {
            return Err(invalid_number(&value.to_string()));
        }
        Ok(Self::Number(canonicalize_number(value)))
    }

    /// Build a canonical number directly from an `i64`, which is always lossless.
    #[must_use]
    pub fn from_i64(value: i64) -> Self {
        Self::Number(value.to_string())
    }

    /// The matcher-compatible bytes for this value: quoted decoded content
    /// for strings, `true`/`false`/`null` literals, or canonical numeric text.
    #[must_use]
    pub fn matcher_bytes(&self) -> Vec<u8> {
        match self {
            Self::String(text) => {
                let mut bytes = Vec::with_capacity(text.len() + 2);
                bytes.push(b'"');
                bytes.extend_from_slice(text.as_bytes());
                bytes.push(b'"');
                bytes
            }
            Self::Bool(true) => b"true".to_vec(),
            Self::Bool(false) => b"false".to_vec(),
            Self::Null => b"null".to_vec(),
            Self::Number(text) => text.as_bytes().to_vec(),
        }
    }

    /// True if this value is a canonical number.
    #[must_use]
    pub const fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }

    /// The string content, if this value is a [`CanonicalValue::String`].
    #[must_use]
    pub const fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(text) => Some(text.as_str()),
            Self::Bool(_) | Self::Null | Self::Number(_) => None,
        }
    }
}

fn invalid_number(text: &str) -> QuaminaError {
    QuaminaError::InvalidCanonicalField {
        format: EventFormat::Custom("canonical-value"),
        location: ErrorLocation::default(),
        message: format!("{text:?} is not a finite, canonical number"),
    }
}

/// Consume a run of one or more ASCII digits from `bytes` starting at `*i`,
/// advancing `*i` past them. Returns `false` (leaving `*i` unmoved) if there
/// is no digit at the starting position.
fn consume_digits(bytes: &[u8], i: &mut usize) -> bool {
    let count = bytes[*i..]
        .iter()
        .take_while(|b| b.is_ascii_digit())
        .count();
    *i += count;
    count > 0
}

/// Validate that `text` matches a strict `-?digit+(.digit+)?([eE][+-]?digit+)?`
/// grammar, rejecting `NaN`/`inf`/trailing garbage/empty input outright
/// before any numeric parsing is attempted.
fn validate_number_syntax(text: &str) -> Result<(), QuaminaError> {
    let bytes = text.as_bytes();
    let mut i = 0;
    if bytes.is_empty() {
        return Err(invalid_number(text));
    }
    if bytes[i] == b'-' {
        i += 1;
    }
    if !consume_digits(bytes, &mut i) {
        return Err(invalid_number(text));
    }
    if i < bytes.len() && bytes[i] == b'.' {
        i += 1;
        if !consume_digits(bytes, &mut i) {
            return Err(invalid_number(text));
        }
    }
    if i < bytes.len() && (bytes[i] == b'e' || bytes[i] == b'E') {
        i += 1;
        if i < bytes.len() && (bytes[i] == b'+' || bytes[i] == b'-') {
            i += 1;
        }
        if !consume_digits(bytes, &mut i) {
            return Err(invalid_number(text));
        }
    }
    if i != bytes.len() {
        return Err(invalid_number(text));
    }
    Ok(())
}

fn canonicalize_number(value: f64) -> String {
    if value.fract() == 0.0 && value.abs() < 1e15 {
        // Range-checked above: magnitudes under 1e15 fit losslessly in i64.
        #[allow(clippy::cast_possible_truncation)]
        let integer = value as i64;
        format!("{integer}")
    } else {
        format!("{value}")
    }
}

/// A single canonicalized field: a validated path paired with a validated value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CanonicalField {
    path: FieldPath,
    value: CanonicalValue,
}

impl CanonicalField {
    /// Pair a path with a value.
    #[must_use]
    pub const fn new(path: FieldPath, value: CanonicalValue) -> Self {
        Self { path, value }
    }

    /// The field's path.
    #[must_use]
    pub const fn path(&self) -> &FieldPath {
        &self.path
    }

    /// The field's value.
    #[must_use]
    pub const fn value(&self) -> &CanonicalValue {
        &self.value
    }

    /// True if this field's value is a canonical number.
    #[must_use]
    pub const fn is_number(&self) -> bool {
        self.value.is_number()
    }

    /// The matcher-compatible bytes for this field's value.
    #[must_use]
    pub fn matcher_bytes(&self) -> Vec<u8> {
        self.value.matcher_bytes()
    }
}

/// A handle to one array allocated by an [`ArrayTrailBuilder`]. Opaque and
/// cheap to copy; use [`id`](Self::id) only for diagnostics or interop with
/// the legacy `array_trail` representation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ArrayHandle {
    id: i32,
}

impl ArrayHandle {
    /// The array's allocated id.
    #[must_use]
    pub const fn id(&self) -> i32 {
        self.id
    }
}

fn array_trail_error(message: impl Into<String>) -> QuaminaError {
    QuaminaError::InvalidCanonicalField {
        format: EventFormat::Custom("array-trail"),
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Allocates unique array ids and tracks the current nesting stack of
/// `(array id, position)` pairs while a decoder walks a nested event.
pub struct ArrayTrailBuilder {
    next_id: u64,
    stack: Vec<(ArrayHandle, i32)>,
}

impl ArrayTrailBuilder {
    /// Start a new builder with ids allocated from 1.
    #[must_use]
    pub const fn new() -> Self {
        Self::with_next_id(1)
    }

    /// Start a new builder with the next allocated id set explicitly, used
    /// to construct id-overflow scenarios in tests.
    #[must_use]
    pub const fn with_next_id(next_id: u64) -> Self {
        Self {
            next_id,
            stack: Vec::new(),
        }
    }

    fn allocate(&mut self) -> Result<ArrayHandle, QuaminaError> {
        let id = self.next_id;
        if id == 0 || id > i32::MAX as u64 {
            return Err(array_trail_error("array id allocation overflowed i32"));
        }
        self.next_id += 1;
        // Range-checked above: id fits in i32.
        #[allow(clippy::cast_possible_truncation)]
        let id = id as i32;
        Ok(ArrayHandle { id })
    }

    /// Enter a new nested array, allocating a fresh id.
    ///
    /// # Errors
    /// Returns an error if allocating another id would overflow `i32`.
    pub fn enter_array(&mut self) -> Result<ArrayHandle, QuaminaError> {
        let handle = self.allocate()?;
        self.stack.push((handle, 0));
        Ok(handle)
    }

    /// Leave the current array level and enter a fresh sibling at the same
    /// nesting depth, allocating a new id.
    ///
    /// # Errors
    /// Returns an error if allocating another id would overflow `i32`.
    pub fn enter_sibling_array(&mut self) -> Result<ArrayHandle, QuaminaError> {
        self.stack.pop();
        self.enter_array()
    }

    /// Record the one-based position of `handle` within its array.
    ///
    /// # Errors
    /// Returns an error if `pos` is not a positive (one-based) position, or
    /// if `handle` is not currently open.
    pub fn set_position(&mut self, handle: ArrayHandle, pos: i32) -> Result<(), QuaminaError> {
        if pos < 1 {
            return Err(array_trail_error(format!(
                "array position {pos} is not one-based"
            )));
        }
        for (entry_handle, entry_pos) in &mut self.stack {
            if *entry_handle == handle {
                *entry_pos = pos;
                return Ok(());
            }
        }
        Err(array_trail_error("array handle is not currently open"))
    }

    /// Leave the array identified by `handle`, popping it (and anything
    /// still nested inside it) off the stack.
    ///
    /// # Errors
    /// Returns an error if `handle` is not currently open.
    pub fn leave_array(&mut self, handle: ArrayHandle) -> Result<(), QuaminaError> {
        if let Some(index) = self.stack.iter().position(|(h, _)| *h == handle) {
            self.stack.truncate(index);
            Ok(())
        } else {
            Err(array_trail_error("array handle is not currently open"))
        }
    }

    /// Snapshot the current outer-to-inner stack of `(array id, position)` pairs.
    #[must_use]
    pub fn snapshot(&self) -> ArraySnapshot {
        ArraySnapshot {
            positions: self
                .stack
                .iter()
                .map(|(handle, pos)| (handle.id, *pos))
                .collect(),
        }
    }
}

impl Default for ArrayTrailBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// A snapshot of the array trail in effect when a field was emitted.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ArraySnapshot {
    positions: Vec<(i32, i32)>,
}

impl ArraySnapshot {
    /// The outer-to-inner `(array id, position)` pairs in this snapshot.
    #[must_use]
    pub fn positions(&self) -> Vec<(i32, i32)> {
        self.positions.clone()
    }

    /// True if `self` and `other` share an array id at different positions,
    /// meaning fields carrying these two snapshots cannot both belong to
    /// the same array element.
    #[must_use]
    pub fn conflicts_with(&self, other: &Self) -> bool {
        for &(id, pos) in &self.positions {
            for &(other_id, other_pos) in &other.positions {
                if id == other_id && pos != other_pos {
                    return true;
                }
            }
        }
        false
    }
}

/// Tracks which fully-qualified field paths appear in at least one pattern,
/// so a decoder can skip constructing fields that could never match.
pub struct PatternFieldTracker {
    paths: FxHashSet<FieldPath>,
}

impl PatternFieldTracker {
    /// Build a tracker from every path referenced by any pattern.
    #[must_use]
    pub fn from_paths<I: IntoIterator<Item = FieldPath>>(paths: I) -> Self {
        Self {
            paths: paths.into_iter().collect(),
        }
    }

    /// True if `path` is referenced by at least one tracked pattern.
    #[must_use]
    pub fn is_referenced(&self, path: &FieldPath) -> bool {
        self.paths.contains(path)
    }
}

/// The output of a [`FieldSetBuilder`]: canonical fields sorted by matcher path.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct FieldSetOutput {
    fields: Vec<CanonicalField>,
}

impl FieldSetOutput {
    /// Number of retained fields.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.fields.len()
    }

    /// True if no fields were retained.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.fields.is_empty()
    }

    /// Iterate the retained fields in sorted order.
    pub fn iter(&self) -> impl Iterator<Item = &CanonicalField> {
        self.fields.iter()
    }

    /// True if `path` (as exact segments) is present in the retained set.
    #[must_use]
    pub fn contains_path<'a, I: IntoIterator<Item = &'a str>>(&self, path: I) -> bool {
        let target = FieldPath::from_segments(path);
        self.fields.iter().any(|field| field.path == target)
    }

    /// The value of the field at `path`, if present.
    #[must_use]
    pub fn value<'a, I: IntoIterator<Item = &'a str>>(&self, path: I) -> Option<&CanonicalValue> {
        let target = FieldPath::from_segments(path);
        self.fields
            .iter()
            .find(|field| field.path == target)
            .map(CanonicalField::value)
    }

    /// True if the retained fields are sorted by their matcher path bytes.
    #[must_use]
    pub fn is_sorted_by_path(&self) -> bool {
        self.fields
            .windows(2)
            .all(|pair| pair[0].path.matcher_path_bytes() <= pair[1].path.matcher_path_bytes())
    }
}

/// Builds a [`FieldSetOutput`], consulting a [`PatternFieldTracker`] before
/// retaining each candidate field and enforcing [`EventLimits::max_fields`].
pub struct FieldSetBuilder<'a> {
    tracker: &'a PatternFieldTracker,
    limits: EventLimits,
    fields: Vec<CanonicalField>,
}

impl<'a> FieldSetBuilder<'a> {
    /// Start a new builder against `tracker`, bounded by `limits`.
    #[must_use]
    pub const fn new(tracker: &'a PatternFieldTracker, limits: EventLimits) -> Self {
        Self {
            tracker,
            limits,
            fields: Vec::new(),
        }
    }

    /// Offer a candidate field. Retained only if `path` is referenced by
    /// the tracker; otherwise silently discarded.
    ///
    /// # Errors
    /// Returns [`QuaminaError::EventLimitExceeded`] if retaining this field
    /// would exceed [`EventLimits::max_fields`].
    pub fn emit<'b, I: IntoIterator<Item = &'b str>>(
        &mut self,
        path: I,
        value: CanonicalValue,
    ) -> Result<(), QuaminaError> {
        let path = FieldPath::from_segments(path);
        if !self.tracker.is_referenced(&path) {
            return Ok(());
        }
        if self.fields.len() >= self.limits.max_fields {
            return Err(QuaminaError::EventLimitExceeded {
                format: EventFormat::Custom("field-set"),
                location: ErrorLocation::default(),
                message: "max_fields exceeded".to_owned(),
            });
        }
        self.fields.push(CanonicalField::new(path, value));
        Ok(())
    }

    /// Finish building, sorting the retained fields by matcher path.
    ///
    /// # Errors
    /// This currently always returns `Ok`; the fallible signature is
    /// reserved for future cross-field validation.
    pub fn finish(mut self) -> Result<FieldSetOutput, QuaminaError> {
        self.fields.sort_by(|a, b| {
            a.path
                .matcher_path_bytes()
                .cmp(&b.path.matcher_path_bytes())
        });
        Ok(FieldSetOutput {
            fields: self.fields,
        })
    }
}

/// One array position entry in a [`RawField`]'s trail, exactly as a decoder
/// observed it, before conflict/range validation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RawArrayPos {
    id: i32,
    pos: i32,
}

impl RawArrayPos {
    /// Pair a raw array id with a raw (unvalidated) position.
    #[must_use]
    pub const fn new(id: i32, pos: i32) -> Self {
        Self { id, pos }
    }
}

/// An unvalidated field exactly as a low-level decoder produced it.
///
/// A single flat path (no embedded segment separator), raw matcher-shaped
/// value bytes, a numeric-tag flag, and a raw array trail.
/// [`DecoderBoundary::validate`] turns a batch of these into
/// [`OwnedField`](crate::OwnedField)s or a deterministic error.
#[derive(Clone, Debug)]
pub struct RawField {
    path: Vec<u8>,
    value: Vec<u8>,
    is_number: bool,
    array_trail: Vec<RawArrayPos>,
}

impl RawField {
    /// Construct a raw field from its path, matcher-shaped value bytes,
    /// numeric-tag flag, and array trail.
    #[must_use]
    pub const fn new(
        path: Vec<u8>,
        value: Vec<u8>,
        is_number: bool,
        array_trail: Vec<RawArrayPos>,
    ) -> Self {
        Self {
            path,
            value,
            is_number,
            array_trail,
        }
    }
}

fn validate_scalar(value: &[u8], is_number: bool) -> bool {
    if is_number {
        return std::str::from_utf8(value).is_ok_and(|text| CanonicalValue::number(text).is_ok());
    }
    value == b"true" || value == b"null" || value == b"false" || is_quoted_scalar(value)
}

/// A quoted-string scalar, either in the ordinary `"..."` matcher form or
/// the backslash-quoted `\"...\"` form some raw fixtures use.
fn is_quoted_scalar(value: &[u8]) -> bool {
    let plain = value.len() >= 2 && value[0] == b'"' && value[value.len() - 1] == b'"';
    let backslash_quoted = value.len() >= 4
        && value[0] == b'\\'
        && value[1] == b'"'
        && value[value.len() - 2] == b'\\'
        && value[value.len() - 1] == b'"';
    plain || backslash_quoted
}

/// Validates batches of [`RawField`]s against a format and [`EventLimits`].
///
/// Converts them into [`OwnedField`](crate::OwnedField)s or a
/// deterministic, panic-free error. This is the shared safety boundary
/// hostile custom fields must cross before reaching the matcher.
pub struct DecoderBoundary {
    format: EventFormat,
    limits: EventLimits,
}

impl DecoderBoundary {
    /// Build a boundary for `format`, enforcing `limits`.
    #[must_use]
    pub const fn new(format: EventFormat, limits: EventLimits) -> Self {
        Self { format, limits }
    }

    fn error(&self, message: impl Into<String>) -> QuaminaError {
        limit_exceeded(self.format, message)
    }

    fn check_array_conflicts(&self, raw: &[RawField]) -> Result<(), QuaminaError> {
        // For every array id, remember the depth it was first seen at and
        // the path prefix (split on the legacy '\n' separator) of that
        // length. A later use of the same id at a different depth, or under
        // a different owning prefix, means the id was reused for two
        // structurally different arrays.
        let mut seen: FxHashMap<i32, (usize, Vec<&[u8]>)> = FxHashMap::default();
        for field in raw {
            let segments: Vec<&[u8]> = split_on_newline(&field.path);
            for (depth, entry) in field.array_trail.iter().enumerate() {
                let prefix_len = (depth + 1).min(segments.len());
                let prefix: Vec<&[u8]> = segments[..prefix_len].to_vec();
                match seen.get(&entry.id) {
                    None => {
                        seen.insert(entry.id, (depth, prefix));
                    }
                    Some((seen_depth, seen_prefix)) => {
                        if *seen_depth != depth || seen_prefix != &prefix {
                            return Err(conflicting_array_id(self.format, entry.id));
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Validate one raw field against `self`'s limits and encoding rules,
    /// recording it in `seen_fields` for the batch-wide duplicate check.
    fn validate_one_field(
        &self,
        index: usize,
        field: RawField,
        seen_fields: &mut FxHashSet<(Vec<u8>, Vec<RawArrayPos>)>,
    ) -> Result<crate::OwnedField, QuaminaError> {
        if field.path.len() > self.limits.max_path_bytes {
            return Err(self.error("max_path_bytes exceeded").at_field_index(index));
        }
        if field.value.len() > self.limits.max_scalar_bytes {
            return Err(self
                .error("max_scalar_bytes exceeded")
                .at_field_index(index));
        }
        if field.array_trail.len() > self.limits.max_depth {
            return Err(self.error("max_depth exceeded").at_field_index(index));
        }
        let path_str = std::str::from_utf8(&field.path).map_err(|_| {
            invalid_event_path(self.format, "field path is not valid UTF-8").at_field_index(index)
        })?;
        if path_str.contains('\n') {
            return Err(ambiguous_event_path(self.format).at_field_index(index));
        }
        if !validate_scalar(&field.value, field.is_number) {
            return Err(invalid_canonical_field(
                self.format,
                "scalar bytes do not match the declared numeric/string tag",
            )
            .at_field_index(index));
        }
        let key = (field.path.clone(), field.array_trail.clone());
        if !seen_fields.insert(key) {
            return Err(duplicate_field(self.format).at_field_index(index));
        }
        Ok(crate::OwnedField {
            path: field.path,
            val: field.value,
            array_trail: field
                .array_trail
                .iter()
                .map(|p| crate::ArrayPos {
                    array: p.id,
                    pos: p.pos,
                })
                .collect(),
            is_number: field.is_number,
        })
    }

    /// Validate a batch of raw fields, returning either the equivalent
    /// [`OwnedField`](crate::OwnedField)s or the first deterministic error found.
    ///
    /// # Errors
    /// Returns a format-neutral [`QuaminaError`] describing the first
    /// structural, encoding, or resource-limit violation found. Never panics.
    pub fn validate(&self, raw: Vec<RawField>) -> Result<Vec<crate::OwnedField>, QuaminaError> {
        if raw.len() > self.limits.max_fields {
            return Err(self.error("max_fields exceeded"));
        }
        self.check_array_conflicts(&raw)?;

        let total_bytes: usize = raw.iter().map(|f| f.path.len() + f.value.len()).sum();
        if total_bytes > self.limits.max_total_allocated_bytes {
            return Err(self.error("max_total_allocated_bytes exceeded"));
        }

        let mut seen_fields: FxHashSet<(Vec<u8>, Vec<RawArrayPos>)> = FxHashSet::default();
        let mut out = Vec::with_capacity(raw.len());
        for (index, field) in raw.into_iter().enumerate() {
            out.push(self.validate_one_field(index, field, &mut seen_fields)?);
        }
        Ok(out)
    }
}

fn split_on_newline(path: &[u8]) -> Vec<&[u8]> {
    path.split(|&b| b == b'\n').collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    // -- CanonicalValue::number / validate_number_syntax / consume_digits --

    #[test]
    fn number_accepts_integer_decimal_exponent_and_negative_forms() {
        assert_eq!(
            CanonicalValue::number("0").unwrap(),
            CanonicalValue::Number("0".to_owned())
        );
        assert!(CanonicalValue::number("123").is_ok());
        assert!(CanonicalValue::number("-123").is_ok());
        assert!(CanonicalValue::number("1.5").is_ok());
        assert!(CanonicalValue::number("-1.5").is_ok());
        assert!(CanonicalValue::number("1e10").is_ok());
        assert!(CanonicalValue::number("1E10").is_ok());
        assert!(CanonicalValue::number("1e+10").is_ok());
        assert!(CanonicalValue::number("1e-10").is_ok());
        assert!(CanonicalValue::number("1.5e10").is_ok());
    }

    #[test]
    fn number_rejects_empty_and_sign_only_input() {
        assert!(CanonicalValue::number("").is_err());
        assert!(CanonicalValue::number("-").is_err());
    }

    #[test]
    fn number_rejects_a_decimal_point_with_no_fractional_digits() {
        assert!(CanonicalValue::number("1.").is_err());
        assert!(CanonicalValue::number("1.e5").is_err());
    }

    #[test]
    fn number_rejects_an_exponent_marker_with_no_exponent_digits() {
        assert!(CanonicalValue::number("1e").is_err());
        assert!(CanonicalValue::number("1e+").is_err());
        assert!(CanonicalValue::number("1e-").is_err());
    }

    #[test]
    fn number_rejects_trailing_garbage_and_non_numeric_text() {
        assert!(CanonicalValue::number("1.2.3").is_err());
        assert!(CanonicalValue::number("123abc").is_err());
        assert!(CanonicalValue::number("abc").is_err());
        assert!(CanonicalValue::number("NaN").is_err());
        assert!(CanonicalValue::number("inf").is_err());
        assert!(CanonicalValue::number("Infinity").is_err());
        assert!(CanonicalValue::number(" 1").is_err());
        assert!(CanonicalValue::number("1 ").is_err());
    }

    #[test]
    fn number_rejects_a_leading_digit_missing_before_the_decimal_point() {
        // consume_digits must require at least one digit for the integer
        // part; a bare ".5" has no leading digit run at all.
        assert!(CanonicalValue::number(".5").is_err());
    }

    #[test]
    fn from_f64_round_trips_through_canonical_number_text() {
        assert_eq!(
            CanonicalValue::from_f64(2.0).unwrap(),
            CanonicalValue::Number("2".to_owned())
        );
        assert!(CanonicalValue::from_f64(f64::NAN).is_err());
        assert!(CanonicalValue::from_f64(f64::INFINITY).is_err());
    }

    // -- DecoderBoundary::validate / check_array_conflicts --

    fn field(path: &str, value: &str, is_number: bool, trail: Vec<(i32, i32)>) -> RawField {
        RawField::new(
            path.as_bytes().to_vec(),
            value.as_bytes().to_vec(),
            is_number,
            trail
                .into_iter()
                .map(|(id, pos)| RawArrayPos::new(id, pos))
                .collect(),
        )
    }

    fn boundary(limits: EventLimits) -> DecoderBoundary {
        DecoderBoundary::new(EventFormat::Custom("test"), limits)
    }

    #[test]
    fn validate_accepts_a_well_formed_batch() {
        let raw = vec![
            field("a", "\"hi\"", false, vec![]),
            field("b", "1", true, vec![]),
        ];
        let out = boundary(EventLimits::default()).validate(raw).unwrap();
        assert_eq!(out.len(), 2);
    }

    #[test]
    fn validate_rejects_a_scalar_that_does_not_match_its_declared_tag() {
        // Tagged as a number but not valid canonical number text.
        let raw = vec![field("a", "not-a-number", true, vec![])];
        let err = boundary(EventLimits::default()).validate(raw).unwrap_err();
        assert!(matches!(err, QuaminaError::InvalidCanonicalField { .. }));
    }

    #[test]
    fn validate_rejects_a_path_embedding_the_segment_separator() {
        let raw = vec![field("a\nb", "\"x\"", false, vec![])];
        let err = boundary(EventLimits::default()).validate(raw).unwrap_err();
        assert!(matches!(err, QuaminaError::AmbiguousEventPath { .. }));
    }

    #[test]
    fn validate_rejects_a_duplicate_path_and_array_trail() {
        let raw = vec![
            field("a", "\"x\"", false, vec![]),
            field("a", "\"y\"", false, vec![]),
        ];
        let err = boundary(EventLimits::default()).validate(raw).unwrap_err();
        assert!(matches!(err, QuaminaError::DuplicateEventField { .. }));
    }

    #[test]
    fn validate_accepts_a_path_at_exactly_max_path_bytes_and_rejects_one_byte_over() {
        let limits = EventLimits {
            max_path_bytes: 4,
            ..EventLimits::default()
        };
        let at_limit = vec![field("abcd", "\"x\"", false, vec![])];
        assert!(boundary(limits).validate(at_limit).is_ok());

        let over_limit = vec![field("abcde", "\"x\"", false, vec![])];
        let err = boundary(limits).validate(over_limit).unwrap_err();
        assert!(matches!(err, QuaminaError::EventLimitExceeded { .. }));
    }

    #[test]
    fn validate_accepts_a_batch_of_exactly_max_fields_and_rejects_one_more() {
        let limits = EventLimits {
            max_fields: 2,
            ..EventLimits::default()
        };
        let at_limit = vec![
            field("a", "\"x\"", false, vec![]),
            field("b", "\"x\"", false, vec![]),
        ];
        assert!(boundary(limits).validate(at_limit).is_ok());

        let over_limit = vec![
            field("a", "\"x\"", false, vec![]),
            field("b", "\"x\"", false, vec![]),
            field("c", "\"x\"", false, vec![]),
        ];
        let err = boundary(limits).validate(over_limit).unwrap_err();
        assert!(matches!(err, QuaminaError::EventLimitExceeded { .. }));
    }

    #[test]
    fn validate_accepts_the_same_array_id_reused_at_the_same_depth_and_prefix() {
        let raw = vec![
            field("a", "1", true, vec![(1, 0)]),
            field("a", "2", true, vec![(1, 1)]),
        ];
        assert!(boundary(EventLimits::default()).validate(raw).is_ok());
    }

    #[test]
    fn validate_rejects_the_same_array_id_reused_at_a_different_depth() {
        // Array id 1 first seen at depth 0 under "a", then reused at depth 1
        // under "b\nc" -- a structurally different array reusing an id.
        let raw = vec![
            field("a", "1", true, vec![(1, 0)]),
            field("b\nc", "2", true, vec![(1, 0), (1, 0)]),
        ];
        let err = boundary(EventLimits::default()).validate(raw).unwrap_err();
        assert!(matches!(
            err,
            QuaminaError::ConflictingArrayId { id: 1, .. }
        ));
    }

    #[test]
    fn validate_rejects_a_field_path_that_is_not_utf8() {
        let raw = vec![RawField::new(
            vec![0xFF, 0xFE],
            b"\"x\"".to_vec(),
            false,
            vec![],
        )];
        let err = boundary(EventLimits::default()).validate(raw).unwrap_err();
        assert!(matches!(err, QuaminaError::InvalidEventPath { .. }));
    }
}
