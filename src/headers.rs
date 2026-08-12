//! Transport-headers ("envelope") flattener.
//!
//! Decodes an [`Envelope`]'s headers (HTTP or Kafka) into the same
//! [`OwnedField`] shape the JSON flattener produces, under the reserved
//! `headers` namespace (configurable via [`HeadersFlattenerBuilder::namespace`]).
//! Header names are ASCII-lowercased; repeated headers with the same name
//! stay distinct, ordered array values unless an explicit HTTP list policy
//! asks for RFC 9110 comma-list folding. See `tests/contracts/README.md`'s
//! "Policy decisions" section for the full rationale.
//!
//! # Validation
//!
//! Every header is validated (name encoding, resource limits, duplicate
//! `Content-Type`, namespace collisions) regardless of whether the current
//! [`SegmentsTreeTracker`] considers it relevant to any pattern, mirroring
//! the other format flatteners in this crate: tracking only controls
//! whether a decoded value is materialized into an [`OwnedField`].

use crate::{
    ArrayPos, ArrayTrailBuilder, CanonicalValue, Envelope, EnvelopeFlattener, ErrorLocation,
    EventFormat, OwnedField, QuaminaError, SegmentsTreeTracker, Transport,
};
use rustc_hash::FxHashMap;

// =============================================================================
// Policies
// =============================================================================

/// How a raw transport header name is normalized into a field path segment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HeaderNamePolicy {
    /// Lowercase ASCII letters (`A`-`Z` become `a`-`z`); other bytes are
    /// left as-is. This makes header lookups case-insensitive, matching
    /// HTTP and Kafka header-name semantics.
    #[default]
    AsciiLowercase,
}

/// How a decoded header value's raw bytes become a matcher-compatible string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HeaderValuePolicy {
    /// Header values must be valid UTF-8 text. A header value with invalid
    /// UTF-8 bytes is rejected.
    #[default]
    Utf8Strings,
    /// Represent every header value as the string `base64:<standard base64>`,
    /// e.g. bytes `[0xff, 0x00]` become `"base64:/wA="`. Collision-free with
    /// [`Utf8Strings`](Self::Utf8Strings) output since it always applies the
    /// `base64:` tag, regardless of whether the raw bytes happen to be valid
    /// UTF-8.
    TaggedBase64,
}

/// How multiple header lines sharing the same (case-insensitive) name are
/// represented.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RepeatedHeaderPolicy {
    /// Every occurrence becomes its own array element, in the order the
    /// headers arrived on the wire. Values are never deduplicated or
    /// comma-joined by this policy alone.
    #[default]
    DistinctArrayValuesInWireOrder,
}

/// How an HTTP header value that looks like an RFC 9110 comma-separated
/// list is represented. Never consulted for Kafka headers, which have no
/// field-line folding concept.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HttpListValuePolicy {
    /// Each header line is kept as one whole value, comma and all. This is
    /// the default: Quamina never assumes a header is a comma-joined list
    /// without an explicit policy.
    #[default]
    DistinctFieldLines,
    /// Split each header line on `,` (per RFC 9110 field-line folding,
    /// under which repeating a header with a comma-joined value is
    /// equivalent to repeating the header line itself), trimming
    /// surrounding whitespace from each element and treating every element
    /// as a distinct array value in wire order.
    CommaSeparatedValues,
}

/// How an HTTP header value is decoded before being turned into a
/// matcher-compatible string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HttpValueDecoding {
    /// Use the header value's bytes as-is (after only the configured
    /// [`WhitespacePolicy`] trimming).
    #[default]
    Raw,
    /// Decode a leading/trailing optionally-whitespace-padded quoted
    /// string: backslash-escapes (`\X` becomes `X`) are resolved first, and
    /// if the result is wrapped in `"`...`"`, the quotes are stripped and
    /// `%XX` percent-escapes inside are decoded as UTF-8 bytes. A value
    /// that is not quoted after backslash-unescaping is used unescaped but
    /// otherwise unchanged.
    QuotedStringAndPercent,
}

/// Whether optional surrounding whitespace is trimmed from a header value
/// (and from each element of an HTTP comma-separated list) before decoding.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum WhitespacePolicy {
    /// Trim leading/trailing ASCII whitespace.
    #[default]
    TrimOptionalWhitespace,
}

/// How a repeated `Content-Type` header (case-insensitive) is handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ContentTypeDuplicatePolicy {
    /// Reject the envelope if `Content-Type` appears more than once, since
    /// HTTP and Kafka consumers disagree on how to resolve multiple
    /// content-type declarations.
    #[default]
    Reject,
}

/// How a header path that would collide with the reserved metadata
/// namespace, or with a separator byte, is handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum HeaderCollisionPolicy {
    /// Reject the envelope. Metadata never silently overwrites, or is
    /// overwritten by, payload-shaped data.
    #[default]
    Reject,
}

/// Resource limits enforced while flattening an envelope's headers, so a
/// hostile or malformed envelope cannot exhaust memory before an error is
/// returned.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeaderLimits {
    /// Maximum number of header entries (counting repeats) an envelope may carry.
    pub max_count: usize,
    /// Maximum length in bytes of any one header name.
    pub max_name_bytes: usize,
    /// Maximum length in bytes of any one header value.
    pub max_value_bytes: usize,
    /// Maximum total bytes (summed name and value lengths) across every header.
    pub max_aggregate_bytes: usize,
}

impl Default for HeaderLimits {
    fn default() -> Self {
        Self {
            max_count: 1_000,
            max_name_bytes: 256,
            max_value_bytes: 65_536,
            max_aggregate_bytes: 1_000_000,
        }
    }
}

// =============================================================================
// Builder
// =============================================================================

/// Builder for [`HeadersFlattener`].
///
/// Construct via [`HeadersFlattener::builder`], chain the policy setters
/// that need to differ from their defaults, and finish with
/// [`build`](Self::build).
#[derive(Debug, Clone)]
pub struct HeadersFlattenerBuilder {
    namespace: String,
    names: HeaderNamePolicy,
    values: HeaderValuePolicy,
    repeated_headers: RepeatedHeaderPolicy,
    http_lists: HttpListValuePolicy,
    http_value_decoding: HttpValueDecoding,
    whitespace: WhitespacePolicy,
    duplicate_content_type: ContentTypeDuplicatePolicy,
    limits: HeaderLimits,
    collisions: HeaderCollisionPolicy,
}

impl Default for HeadersFlattenerBuilder {
    fn default() -> Self {
        Self {
            namespace: "headers".to_owned(),
            names: HeaderNamePolicy::default(),
            values: HeaderValuePolicy::default(),
            repeated_headers: RepeatedHeaderPolicy::default(),
            http_lists: HttpListValuePolicy::default(),
            http_value_decoding: HttpValueDecoding::default(),
            whitespace: WhitespacePolicy::default(),
            duplicate_content_type: ContentTypeDuplicatePolicy::default(),
            limits: HeaderLimits::default(),
            collisions: HeaderCollisionPolicy::default(),
        }
    }
}

impl HeadersFlattenerBuilder {
    /// Set the reserved top-level namespace header fields are nested under.
    /// Defaults to `"headers"`.
    #[must_use]
    pub fn namespace(mut self, namespace: &str) -> Self {
        self.namespace = namespace.to_owned();
        self
    }

    /// Set the header-name normalization policy.
    #[must_use]
    pub const fn names(mut self, policy: HeaderNamePolicy) -> Self {
        self.names = policy;
        self
    }

    /// Set the header-value decoding policy.
    #[must_use]
    pub const fn values(mut self, policy: HeaderValuePolicy) -> Self {
        self.values = policy;
        self
    }

    /// Set the repeated-header representation policy.
    #[must_use]
    pub const fn repeated_headers(mut self, policy: RepeatedHeaderPolicy) -> Self {
        self.repeated_headers = policy;
        self
    }

    /// Set the HTTP comma-separated list policy.
    #[must_use]
    pub const fn http_lists(mut self, policy: HttpListValuePolicy) -> Self {
        self.http_lists = policy;
        self
    }

    /// Set the HTTP quoted-string/percent decoding policy.
    #[must_use]
    pub const fn http_value_decoding(mut self, policy: HttpValueDecoding) -> Self {
        self.http_value_decoding = policy;
        self
    }

    /// Set the surrounding-whitespace policy.
    #[must_use]
    pub const fn whitespace(mut self, policy: WhitespacePolicy) -> Self {
        self.whitespace = policy;
        self
    }

    /// Set the duplicate-`Content-Type` policy.
    #[must_use]
    pub const fn duplicate_content_type(mut self, policy: ContentTypeDuplicatePolicy) -> Self {
        self.duplicate_content_type = policy;
        self
    }

    /// Set the resource limits enforced while decoding.
    #[must_use]
    pub const fn limits(mut self, limits: HeaderLimits) -> Self {
        self.limits = limits;
        self
    }

    /// Set the namespace/separator collision policy.
    #[must_use]
    pub const fn collisions(mut self, policy: HeaderCollisionPolicy) -> Self {
        self.collisions = policy;
        self
    }

    /// Finish building the flattener.
    #[must_use]
    pub fn build(self) -> HeadersFlattener {
        HeadersFlattener {
            namespace: self.namespace,
            names: self.names,
            values: self.values,
            repeated_headers: self.repeated_headers,
            http_lists: self.http_lists,
            http_value_decoding: self.http_value_decoding,
            whitespace: self.whitespace,
            duplicate_content_type: self.duplicate_content_type,
            limits: self.limits,
            collisions: self.collisions,
        }
    }
}

// =============================================================================
// HeadersFlattener
// =============================================================================

/// An [`EnvelopeFlattener`] that decodes transport headers (HTTP or Kafka)
/// under a reserved namespace.
///
/// Use [`new`](Self::new) for default policies, or [`builder`](Self::builder)
/// to select non-default policies. See the [module docs](self) for how
/// header values map onto the JSON scalar representation Quamina's matcher
/// expects.
#[derive(Debug, Clone)]
pub struct HeadersFlattener {
    namespace: String,
    names: HeaderNamePolicy,
    values: HeaderValuePolicy,
    repeated_headers: RepeatedHeaderPolicy,
    http_lists: HttpListValuePolicy,
    http_value_decoding: HttpValueDecoding,
    whitespace: WhitespacePolicy,
    duplicate_content_type: ContentTypeDuplicatePolicy,
    limits: HeaderLimits,
    collisions: HeaderCollisionPolicy,
}

impl HeadersFlattener {
    /// Create a flattener with every policy at its default.
    #[must_use]
    pub fn new() -> Self {
        Self::builder().build()
    }

    /// Start a [`HeadersFlattenerBuilder`] to select non-default policies.
    #[must_use]
    pub fn builder() -> HeadersFlattenerBuilder {
        HeadersFlattenerBuilder::default()
    }

    /// Flatten `envelope`'s headers directly, without going through a
    /// [`Quamina`](crate::Quamina) instance or a pattern tracker. Every
    /// header is decoded and emitted, useful for comparing two flattener
    /// configurations' output.
    ///
    /// # Errors
    /// Returns an error if the envelope's headers cannot be interpreted
    /// under this flattener's configured policies (see the [module
    /// docs](self)).
    pub fn flatten_headers(&self, envelope: &Envelope) -> Result<Vec<OwnedField>, QuaminaError> {
        self.build_fields(envelope, None)
    }

    fn build_fields(
        &self,
        envelope: &Envelope,
        tracker: Option<&dyn SegmentsTreeTracker>,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        let headers = envelope.headers();
        if headers.len() > self.limits.max_count {
            return Err(limit_exceeded("header count exceeds configured max_count"));
        }

        let mut groups: Vec<(String, Vec<&[u8]>)> = Vec::new();
        let mut index: FxHashMap<String, usize> = FxHashMap::default();
        let mut aggregate_bytes: usize = 0;
        let mut content_type_seen = false;

        for (name, value) in headers.iter() {
            aggregate_bytes = aggregate_bytes
                .saturating_add(name.len())
                .saturating_add(value.len());
            if aggregate_bytes > self.limits.max_aggregate_bytes {
                return Err(limit_exceeded(
                    "aggregate header bytes exceed configured max_aggregate_bytes",
                ));
            }
            if name.len() > self.limits.max_name_bytes {
                return Err(limit_exceeded(
                    "header name exceeds configured max_name_bytes",
                ));
            }
            if value.len() > self.limits.max_value_bytes {
                return Err(limit_exceeded(
                    "header value exceeds configured max_value_bytes",
                ));
            }

            let lowered = self.normalize_name(name)?;

            if lowered == "content-type" {
                match self.duplicate_content_type {
                    ContentTypeDuplicatePolicy::Reject => {
                        if content_type_seen {
                            return Err(conflicting_headers(
                                "duplicate Content-Type header is not allowed",
                            ));
                        }
                        content_type_seen = true;
                    }
                }
            }

            if let Some(&i) = index.get(&lowered) {
                groups[i].1.push(value);
            } else {
                index.insert(lowered.clone(), groups.len());
                groups.push((lowered, vec![value]));
            }
        }

        if matches!(self.collisions, HeaderCollisionPolicy::Reject)
            && let Some(body) = envelope.body()
            && body_has_top_level_key(body, &self.namespace)
        {
            return Err(path_collision(
                "payload body has a top-level field matching the reserved headers namespace",
            ));
        }

        // The only defined policy is wire-order, distinct array values,
        // which is how `items` below are always assembled; matching here
        // keeps this policy consulted (rather than dead weight) and gives a
        // future alternative a place to branch.
        match self.repeated_headers {
            RepeatedHeaderPolicy::DistinctArrayValuesInWireOrder => {}
        }

        let namespace_bytes = self.namespace.as_bytes();
        let mut fields = Vec::new();
        let mut array_builder = ArrayTrailBuilder::new();
        let mut first_group = true;

        for (lowered_name, raw_values) in &groups {
            let handle = if first_group {
                array_builder.enter_array()
            } else {
                array_builder.enter_sibling_array()
            }
            .map_err(|_| limit_exceeded("array id allocation overflowed"))?;
            first_group = false;

            let used_path = Self::used_path(tracker, namespace_bytes, lowered_name.as_bytes());
            let items = self.decode_group_items(envelope.transport(), raw_values);

            for (offset, item) in items.into_iter().enumerate() {
                let pos = i32::try_from(offset + 1)
                    .map_err(|_| limit_exceeded("array position exceeds i32 range"))?;
                array_builder
                    .set_position(handle, pos)
                    .map_err(|_| limit_exceeded("array position overflow"))?;
                let value = self.encode_value(item)?;
                if let Some(path) = &used_path {
                    fields.push(OwnedField {
                        path: path.clone(),
                        val: value.matcher_bytes(),
                        array_trail: array_builder
                            .snapshot()
                            .positions()
                            .into_iter()
                            .map(|(array, pos)| ArrayPos { array, pos })
                            .collect(),
                        is_number: false,
                    });
                }
            }
        }

        Ok(fields)
    }

    fn normalize_name(&self, name: &[u8]) -> Result<String, QuaminaError> {
        if name.contains(&b'\n') {
            return Err(invalid_envelope(
                "name",
                "header name contains an embedded newline, which would corrupt the matcher path",
            ));
        }
        let text = std::str::from_utf8(name)
            .map_err(|_| invalid_envelope("name", "header name is not valid UTF-8"))?;
        match self.names {
            HeaderNamePolicy::AsciiLowercase => Ok(text.to_ascii_lowercase()),
        }
    }

    fn used_path(
        tracker: Option<&dyn SegmentsTreeTracker>,
        namespace: &[u8],
        name: &[u8],
    ) -> Option<Vec<u8>> {
        let used = match tracker {
            None => true,
            Some(t) => t.get(namespace).is_some_and(|ns| ns.is_segment_used(name)),
        };
        if !used {
            return None;
        }
        let mut path = namespace.to_vec();
        path.push(b'\n');
        path.extend_from_slice(name);
        Some(path)
    }

    /// Turn one header name's raw wire values into the decoded byte items
    /// that will become array elements, honoring the HTTP list, decoding,
    /// and whitespace policies.
    fn decode_group_items(&self, transport: Transport, raw_values: &[&[u8]]) -> Vec<Vec<u8>> {
        let mut items = Vec::new();
        for raw in raw_values {
            let split_by_comma = transport == Transport::Http
                && matches!(self.http_lists, HttpListValuePolicy::CommaSeparatedValues);
            if split_by_comma {
                match std::str::from_utf8(raw) {
                    Ok(text) => {
                        for part in text.split(',') {
                            items.push(self.decode_item(part.as_bytes()));
                        }
                    }
                    Err(_) => items.push(self.decode_item(raw)),
                }
            } else {
                items.push(self.decode_item(raw));
            }
        }
        items
    }

    fn decode_item(&self, raw: &[u8]) -> Vec<u8> {
        let trimmed = match self.whitespace {
            WhitespacePolicy::TrimOptionalWhitespace => trim_ascii_whitespace(raw),
        };
        match self.http_value_decoding {
            HttpValueDecoding::Raw => trimmed.to_vec(),
            HttpValueDecoding::QuotedStringAndPercent => decode_http_quoted_percent(trimmed),
        }
    }

    fn encode_value(&self, bytes: Vec<u8>) -> Result<CanonicalValue, QuaminaError> {
        match self.values {
            HeaderValuePolicy::Utf8Strings => {
                let text = String::from_utf8(bytes)
                    .map_err(|_| unsupported_value("header value is not valid UTF-8"))?;
                Ok(CanonicalValue::String(text))
            }
            HeaderValuePolicy::TaggedBase64 => Ok(CanonicalValue::String(format!(
                "base64:{}",
                base64_encode(&bytes)
            ))),
        }
    }
}

impl Default for HeadersFlattener {
    fn default() -> Self {
        Self::new()
    }
}

impl EnvelopeFlattener for HeadersFlattener {
    fn flatten_envelope(
        &mut self,
        envelope: &Envelope,
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        self.build_fields(envelope, Some(tracker))
    }

    fn copy(&self) -> Box<dyn EnvelopeFlattener> {
        Box::new(self.clone())
    }
}

// =============================================================================
// Error helpers
// =============================================================================

fn invalid_envelope(attribute: &'static str, message: impl Into<String>) -> QuaminaError {
    QuaminaError::InvalidEnvelope {
        format: EventFormat::Headers,
        location: ErrorLocation::default(),
        attribute,
        message: message.into(),
    }
}

fn conflicting_headers(message: impl Into<String>) -> QuaminaError {
    QuaminaError::ConflictingEnvelopeHeaders {
        format: EventFormat::Headers,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

fn path_collision(message: impl Into<String>) -> QuaminaError {
    QuaminaError::EnvelopePathCollision {
        format: EventFormat::Headers,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

fn limit_exceeded(message: impl Into<String>) -> QuaminaError {
    QuaminaError::EventLimitExceeded {
        format: EventFormat::Headers,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

fn unsupported_value(message: impl Into<String>) -> QuaminaError {
    QuaminaError::UnsupportedEventValue {
        format: EventFormat::Headers,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

// =============================================================================
// Byte-level helpers
// =============================================================================

/// Trim leading/trailing ASCII whitespace, never panicking on empty input.
fn trim_ascii_whitespace(bytes: &[u8]) -> &[u8] {
    let start = bytes
        .iter()
        .position(|b| !b.is_ascii_whitespace())
        .unwrap_or(bytes.len());
    let end = bytes
        .iter()
        .rposition(|b| !b.is_ascii_whitespace())
        .map_or(start, |p| p + 1);
    &bytes[start..end]
}

/// Resolve `\X` backslash escapes (any escaped byte becomes itself,
/// dropping the backslash) throughout `bytes`.
fn backslash_unescape(bytes: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'\\' && i + 1 < bytes.len() {
            out.push(bytes[i + 1]);
            i += 2;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    out
}

const fn hex_val(b: u8) -> Option<u8> {
    match b {
        b'0'..=b'9' => Some(b - b'0'),
        b'a'..=b'f' => Some(b - b'a' + 10),
        b'A'..=b'F' => Some(b - b'A' + 10),
        _ => None,
    }
}

/// Decode `%XX` percent-escapes into raw bytes; a malformed escape (missing
/// or non-hex digits) is left as literal bytes rather than erroring, since
/// this only ever runs on already-untrusted wire content.
fn percent_decode(bytes: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%'
            && i + 2 < bytes.len()
            && let (Some(hi), Some(lo)) = (hex_val(bytes[i + 1]), hex_val(bytes[i + 2]))
        {
            out.push((hi << 4) | lo);
            i += 3;
            continue;
        }
        out.push(bytes[i]);
        i += 1;
    }
    out
}

/// Decode a leading/trailing-quoted, backslash-escaped, percent-escaped HTTP
/// header value. `trimmed` is expected to already have surrounding
/// whitespace removed. A value that is not wrapped in `"`...`"` after
/// backslash-unescaping is returned unescaped but otherwise unchanged.
fn decode_http_quoted_percent(trimmed: &[u8]) -> Vec<u8> {
    let unescaped = backslash_unescape(trimmed);
    if unescaped.len() >= 2 && unescaped[0] == b'"' && unescaped[unescaped.len() - 1] == b'"' {
        let inner = &unescaped[1..unescaped.len() - 1];
        percent_decode(inner)
    } else {
        unescaped
    }
}

/// A minimal, best-effort scan for whether `body` is a JSON-object-shaped
/// byte string with a top-level (depth-1) key exactly equal to `key`. Never
/// panics on malformed input; a scan that cannot make sense of `body`
/// simply reports no match rather than erroring, since `HeadersFlattener`
/// does not otherwise decode the payload.
fn body_has_top_level_key(body: &[u8], key: &str) -> bool {
    let mut i = 0;
    while i < body.len() && body[i].is_ascii_whitespace() {
        i += 1;
    }
    if body.get(i) != Some(&b'{') {
        return false;
    }
    i += 1;
    let mut depth: i32 = 1;
    let key_bytes = key.as_bytes();
    while i < body.len() {
        match body[i] {
            b'"' => {
                let str_start = i + 1;
                let mut j = str_start;
                while j < body.len() && body[j] != b'"' {
                    if body[j] == b'\\' && j + 1 < body.len() {
                        j += 1;
                    }
                    j += 1;
                }
                if j >= body.len() {
                    return false;
                }
                let content = &body[str_start..j];
                i = j + 1;
                if depth == 1 {
                    let mut k = i;
                    while k < body.len() && body[k].is_ascii_whitespace() {
                        k += 1;
                    }
                    if body.get(k) == Some(&b':') && content == key_bytes {
                        return true;
                    }
                }
            }
            b'{' | b'[' => {
                depth += 1;
                i += 1;
            }
            b'}' | b']' => {
                depth -= 1;
                i += 1;
                if depth <= 0 {
                    return false;
                }
            }
            _ => i += 1,
        }
    }
    false
}

// =============================================================================
// Base64 (standard alphabet, padded) — used only by `HeaderValuePolicy::TaggedBase64`
// =============================================================================

const BASE64_ALPHABET: &[u8; 64] =
    b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn base64_encode(data: &[u8]) -> String {
    let mut out = String::with_capacity(data.len().div_ceil(3) * 4);
    let mut chunks = data.chunks_exact(3);
    for chunk in &mut chunks {
        let n = (u32::from(chunk[0]) << 16) | (u32::from(chunk[1]) << 8) | u32::from(chunk[2]);
        push_sextets(&mut out, n, 4);
    }
    let rem = chunks.remainder();
    match rem.len() {
        1 => {
            let n = u32::from(rem[0]) << 16;
            push_sextets(&mut out, n, 2);
            out.push_str("==");
        }
        2 => {
            let n = (u32::from(rem[0]) << 16) | (u32::from(rem[1]) << 8);
            push_sextets(&mut out, n, 3);
            out.push('=');
        }
        _ => {}
    }
    out
}

fn push_sextets(out: &mut String, n: u32, count: u8) {
    for i in 0..count {
        let shift = 18 - 6 * u32::from(i);
        let sextet = (n >> shift) & 0x3F;
        out.push(BASE64_ALPHABET[sextet as usize] as char);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base64_matches_known_vectors() {
        assert_eq!(base64_encode(&[0x00, 0xff]), "AP8=");
        assert_eq!(base64_encode(b"foobar"), "Zm9vYmFy");
    }

    #[test]
    fn percent_decode_handles_utf8_sequences() {
        assert_eq!(percent_decode(b"Gr%C3%BC%C3%9Fe"), "Grüße".as_bytes());
    }

    #[test]
    fn body_key_scan_finds_top_level_key_only() {
        assert!(body_has_top_level_key(
            br#"{"headers":{"x-role":"payload"}}"#,
            "headers"
        ));
        assert!(!body_has_top_level_key(
            br#"{"outer":{"headers":1}}"#,
            "headers"
        ));
        assert!(!body_has_top_level_key(b"not json", "headers"));
        assert!(!body_has_top_level_key(b"", "headers"));
    }
}
