//! quamina-rs: Fast pattern-matching library for filtering JSON events

#![deny(missing_docs)]
#![cfg_attr(miri, allow(warnings))]

// Internal modules exposed as `pub` only for benchmarks (benches/matching.rs).
// Not part of the public API — use `Quamina` instead.
#[doc(hidden)]
pub mod automaton;
#[cfg(feature = "avro")]
pub mod avro;
#[cfg(any(
    feature = "messagepack",
    feature = "cbor",
    feature = "protobuf",
    feature = "avro",
    feature = "headers"
))]
mod base64;
pub mod canonical;
mod case_folding;
#[cfg(any(
    feature = "messagepack",
    feature = "cbor",
    feature = "protobuf",
    feature = "avro",
    feature = "headers"
))]
mod decoder_errors;
pub mod envelope;
#[doc(hidden)]
pub mod flatten_json;
mod flattener;
pub mod format_policies;
#[cfg(feature = "headers")]
pub mod headers;
#[doc(hidden)]
pub mod json;
#[cfg(feature = "messagepack")]
pub mod messagepack;
#[doc(hidden)]
pub mod numbits;
#[cfg(feature = "protobuf")]
pub mod protobuf;
#[doc(hidden)]
pub mod regexp;
#[doc(hidden)]
pub mod segments_tree;
mod unicode_categories;
#[cfg(any(feature = "protobuf", feature = "avro"))]
mod zigzag;

#[cfg(test)]
mod regexp_samples;

#[cfg(kani)]
mod kani_proofs;

// Re-export flattener types for custom implementations
pub use crate::flatten_json::ArrayPos;
pub use crate::flattener::{Flattener, JsonFlattener, OwnedField, SegmentsTreeTracker};

// Re-export the Avro flattener (avro contract).
#[cfg(feature = "avro")]
pub use crate::avro::{
    AvroBuilder, AvroCodecPolicy, AvroFlattener, AvroInput, AvroSchemaGraph, AvroUnionPolicy,
    FingerprintResolver, LogicalTypeContract, LogicalTypePolicy,
};
// Re-export the shared decoder boundary (core-boundary contract).
pub use crate::canonical::{
    ArrayHandle, ArraySnapshot, ArrayTrailBuilder, CanonicalField, CanonicalValue, DecoderBoundary,
    FieldPath, FieldSetBuilder, FieldSetOutput, PatternFieldTracker, RawArrayPos, RawField,
};
// Re-export transport envelope support (headers/CloudEvents contracts).
pub use crate::envelope::{Envelope, EnvelopeBuilder, EnvelopeFlattener, Headers, Transport};
// Re-export format policies shared by more than one binary flattener.
pub use crate::format_policies::{
    BinaryValuePolicy, DuplicateKeyPolicy, MapKeyPolicy, NumericPolicy, RootValuePolicy,
};
// Re-export the MessagePack flattener (messagepack contract).
#[cfg(feature = "messagepack")]
pub use crate::messagepack::{
    ExtensionValuePolicy, MessagePackFlattener, MessagePackFlattenerBuilder,
    MessagePackTimestampPolicy,
};
// Re-export the headers envelope flattener (headers contract).
#[cfg(feature = "headers")]
pub use crate::headers::{
    ContentTypeDuplicatePolicy, HeaderCollisionPolicy, HeaderLimits, HeaderNamePolicy,
    HeaderValuePolicy, HeadersFlattener, HeadersFlattenerBuilder, HttpListValuePolicy,
    HttpValueDecoding, RepeatedHeaderPolicy, WhitespacePolicy,
};
// Re-export the Protobuf flattener (protobuf contract).
#[cfg(feature = "protobuf")]
pub use crate::protobuf::{
    ContractFields, DecoderState, EnumSymbol, EnumValuePolicy, MapPolicy, OneofPolicy,
    ProtobufFieldName, ProtobufFlattener, ProtobufInput, ProtobufPresence, SchemaGraph,
    UnknownFieldPolicy, WellKnownTypeContract, WellKnownTypePolicy,
};

use automaton::{NfaBuffers, ThreadSafeCoreMatcher};
use json::Matcher;
use parking_lot::Mutex;
use rustc_hash::{FxHashMap, FxHashSet};
use segments_tree::SegmentsTree;
use std::cell::RefCell;
use std::fmt;
use std::hash::Hash;
use std::sync::atomic::{AtomicU64, Ordering};

thread_local! {
    /// Thread-local JSON flattener state, avoiding per-call Mutex overhead.
    static TL_FLATTENER: RefCell<flatten_json::State> = RefCell::new(flatten_json::State::new());
    /// Thread-local NFA traversal buffers, avoiding per-call Mutex overhead.
    static TL_NFA_BUFS: RefCell<NfaBuffers> = RefCell::new(NfaBuffers::new());
}

/// Statistics for pruner rebuilding decisions
#[derive(Debug, Default)]
pub struct PrunerStats {
    /// Count of patterns emitted (returned after filtering) since last rebuild
    emitted: AtomicU64,
    /// Count of patterns filtered out (deleted) since last rebuild
    filtered: AtomicU64,
}

impl PrunerStats {
    fn new() -> Self {
        Self::default()
    }

    fn reset(&self) {
        self.emitted.store(0, Ordering::Relaxed);
        self.filtered.store(0, Ordering::Relaxed);
    }

    fn add_emitted(&self, count: u64) {
        self.emitted.fetch_add(count, Ordering::Relaxed);
    }

    fn add_filtered(&self, count: u64) {
        self.filtered.fetch_add(count, Ordering::Relaxed);
    }

    /// Count of live-pattern matches returned since the last rebuild.
    pub fn emitted(&self) -> u64 {
        self.emitted.load(Ordering::Relaxed)
    }

    /// Count of deleted-pattern matches suppressed since the last rebuild.
    pub fn filtered(&self) -> u64 {
        self.filtered.load(Ordering::Relaxed)
    }

    /// Check if rebuild should be triggered (Go uses 0.2 ratio, 1000 minimum)
    fn should_rebuild(&self) -> bool {
        let emitted = self.emitted.load(Ordering::Relaxed);
        let filtered = self.filtered.load(Ordering::Relaxed);

        // Minimum activity threshold
        if emitted + filtered < 1000 {
            return false;
        }

        // Avoid division by zero
        if emitted == 0 {
            return false;
        }

        // Rebuild when filtered/emitted > 0.2, which we'd rather check
        // without an f64 detour: 5*filtered > emitted is the same condition
        // in integers. `checked_mul` returning None means filtered is so
        // huge (≥ 2^61) we're well overdue for a rebuild anyway, so we
        // treat it as "yes, rebuild".
        filtered.checked_mul(5).is_none_or(|fx5| fx5 > emitted)
    }
}

impl Clone for PrunerStats {
    fn clone(&self) -> Self {
        Self {
            emitted: AtomicU64::new(self.emitted.load(Ordering::Relaxed)),
            filtered: AtomicU64::new(self.filtered.load(Ordering::Relaxed)),
        }
    }
}

/// Resource-consumption statistics for a matcher tree, as reported by
/// [`Quamina::matcher_stats`].
///
/// `bytes` is the headline figure: an estimate of the memory consumed by the
/// matcher's data structures. The other fields describe the automaton's
/// shape, which drives traversal cost.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct MatcherStats {
    /// Total automaton states across all value matchers.
    pub states: usize,
    /// Estimated bytes consumed by the matcher's data structures.
    /// Capacity-based: counts allocated buffer space, not just occupied space.
    pub bytes: usize,
    /// Sum of the stored epsilon-closure sizes of all states.
    /// Self-only closures use the zero-length sentinel and contribute zero.
    pub fanouts: usize,
    /// Size of the largest stored epsilon closure.
    /// Zero means every closure is self-only.
    pub max_fanout: usize,
}

/// Pattern definition: field matchers
type PatternDef = FxHashMap<String, Vec<Matcher>>;

/// A stored pattern definition, stamped with the position of the
/// [`add_pattern`](Quamina::add_pattern) call that produced it.
///
/// Merge order decides an automaton's size and layout, not which events it
/// accepts, so rebuilds and clones replay in add order to reproduce the
/// matcher the caller's adds built.
#[derive(Clone)]
struct StoredPattern {
    added_at: usize,
    fields: PatternDef,
}

/// The wire format an event, envelope, or error is associated with.
///
/// Used by non-JSON flatteners and the shared decoder boundary to report
/// which decoder produced a given error, independent of the error variant.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EventFormat {
    /// JSON events, handled by [`JsonFlattener`].
    Json,
    /// MessagePack events.
    MessagePack,
    /// CBOR events.
    Cbor,
    /// Protocol Buffers events.
    Protobuf,
    /// Apache Avro events.
    Avro,
    /// Transport headers (HTTP/Kafka) carried by an [`Envelope`].
    Headers,
    /// CloudEvents in binary content mode.
    CloudEventsBinary,
    /// A caller-defined format, named for diagnostics and test harnesses.
    Custom(&'static str),
}

/// The location an error occurred at, when the decoder can identify one.
///
/// Both fields are independent: a byte-oriented decoder reports a byte
/// offset, while the shared field-boundary validator reports the index of
/// the offending field in the raw field list. Either, both, or neither may
/// be known for a given error.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ErrorLocation {
    field_index: Option<usize>,
    byte_offset: Option<usize>,
}

impl ErrorLocation {
    /// The index of the offending field within the raw field list, if known.
    #[must_use]
    pub const fn field_index(&self) -> Option<usize> {
        self.field_index
    }

    /// The byte offset into the source event where the error occurred, if known.
    #[must_use]
    pub const fn byte_offset(&self) -> Option<usize> {
        self.byte_offset
    }
}

/// Boxed source error retained by format-neutral [`QuaminaError`] variants.
type BoxedSource = Box<dyn std::error::Error + Send + Sync + 'static>;

/// Limits every non-JSON decoder enforces on the shape and size of the raw
/// event it decodes, so a hostile or malformed input cannot exhaust memory
/// or the call stack before an error is returned.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EventLimits {
    /// Maximum nesting depth (objects/arrays) the decoder will descend into.
    pub max_depth: usize,
    /// Maximum number of fields the decoder will retain in one event.
    pub max_fields: usize,
    /// Maximum length in bytes of any one field path.
    pub max_path_bytes: usize,
    /// Maximum length in bytes of any one scalar value.
    pub max_scalar_bytes: usize,
    /// Maximum number of items (array elements/map entries) in one container.
    pub max_container_items: usize,
    /// Maximum total bytes the decoder will allocate for one event.
    pub max_total_allocated_bytes: usize,
}

impl Default for EventLimits {
    fn default() -> Self {
        Self {
            max_depth: 64,
            max_fields: 10_000,
            max_path_bytes: 4_096,
            max_scalar_bytes: 1_000_000,
            max_container_items: 100_000,
            max_total_allocated_bytes: 64_000_000,
        }
    }
}

impl EventLimits {
    /// A deliberately tight preset used by contract tests to trip every
    /// resource limit with small, hand-written payloads.
    #[must_use]
    pub const fn strict() -> Self {
        Self {
            max_depth: 4,
            max_fields: 8,
            max_path_bytes: 64,
            max_scalar_bytes: 64,
            max_container_items: 8,
            max_total_allocated_bytes: 512,
        }
    }
}

/// Errors that can occur during pattern matching
#[derive(Debug)]
pub enum QuaminaError {
    /// The event JSON was syntactically invalid.
    InvalidJson(String),
    /// The pattern JSON was malformed or used unsupported syntax.
    InvalidPattern(String),
    /// The input contained invalid UTF-8.
    InvalidUtf8,
    /// The requested media type is not supported (only `application/json`).
    UnsupportedMediaType(String),
    /// The pattern exceeded configured complexity limits (see [`PatternLimits`]).
    PatternTooComplex(String),
    /// The raw event bytes were malformed for the reported format.
    InvalidEvent {
        /// The format that rejected the event.
        format: EventFormat,
        /// Where in the event the problem was found, if known.
        location: ErrorLocation,
        /// A human-readable description of the problem.
        message: String,
        /// The underlying decoder error, if any.
        source: Option<BoxedSource>,
    },
    /// A field path could not be validated (e.g. invalid UTF-8).
    InvalidEventPath {
        /// The format that rejected the path.
        format: EventFormat,
        /// Where the invalid path was found, if known.
        location: ErrorLocation,
        /// A human-readable description of the problem.
        message: String,
    },
    /// A raw field path embedded the segment separator instead of being
    /// constructed as distinct segments, making its meaning ambiguous.
    AmbiguousEventPath {
        /// The format that rejected the path.
        format: EventFormat,
        /// Where the ambiguous path was found, if known.
        location: ErrorLocation,
    },
    /// A raw scalar's bytes did not match its declared numeric/string tag.
    InvalidCanonicalField {
        /// The format that rejected the field.
        format: EventFormat,
        /// Where the invalid field was found, if known.
        location: ErrorLocation,
        /// A human-readable description of the problem.
        message: String,
    },
    /// The same array id was used for two structurally different arrays.
    ConflictingArrayId {
        /// The format that rejected the event.
        format: EventFormat,
        /// Where the conflict was found, if known.
        location: ErrorLocation,
        /// The array id that was reused.
        id: i32,
    },
    /// The same field path and array trail were emitted more than once.
    DuplicateEventField {
        /// The format that rejected the event.
        format: EventFormat,
        /// Where the duplicate was found, if known.
        location: ErrorLocation,
    },
    /// A configured [`EventLimits`] bound was exceeded.
    EventLimitExceeded {
        /// The format that rejected the event.
        format: EventFormat,
        /// Where the limit was exceeded, if known.
        location: ErrorLocation,
        /// A human-readable description of which limit was exceeded.
        message: String,
    },
    /// The event contained a value this decoder's policy does not support
    /// (e.g. a non-finite float, unsupported binary data, or an unknown enum).
    UnsupportedEventValue {
        /// The format that rejected the value.
        format: EventFormat,
        /// Where the value was found, if known.
        location: ErrorLocation,
        /// A human-readable description of the problem.
        message: String,
    },
    /// A map/object key did not satisfy the configured key policy.
    UnsupportedMapKey {
        /// The format that rejected the key.
        format: EventFormat,
        /// Where the key was found, if known.
        location: ErrorLocation,
        /// A human-readable description of the problem.
        message: String,
    },
    /// The event used a recognized but unimplemented/disabled format feature.
    UnsupportedFormatFeature {
        /// The format that rejected the feature.
        format: EventFormat,
        /// Where the feature was found, if known.
        location: ErrorLocation,
        /// A human-readable description of the unsupported feature.
        message: String,
    },
    /// A schema or descriptor used to construct a flattener was invalid.
    InvalidSchema {
        /// The format the schema was for.
        format: EventFormat,
        /// A human-readable description of the problem.
        message: String,
    },
    /// A schema needed to decode an event (e.g. by fingerprint) was not available.
    MissingEventSchema {
        /// The format that needed the schema.
        format: EventFormat,
        /// Where the missing schema was needed, if known.
        location: ErrorLocation,
        /// A human-readable description of the problem.
        message: String,
    },
    /// A transport envelope (headers, CloudEvents attributes) was invalid.
    InvalidEnvelope {
        /// The format that rejected the envelope.
        format: EventFormat,
        /// Where the invalid attribute was found, if known.
        location: ErrorLocation,
        /// The name of the offending attribute, if applicable.
        attribute: &'static str,
        /// A human-readable description of the problem.
        message: String,
    },
    /// Transport headers conflicted (e.g. duplicate `Content-Type`, or a
    /// case-insensitive header name repeated with different values).
    ConflictingEnvelopeHeaders {
        /// The format that rejected the envelope.
        format: EventFormat,
        /// Where the conflict was found, if known.
        location: ErrorLocation,
        /// A human-readable description of the conflict.
        message: String,
    },
    /// A header or attribute path collided with a reserved namespace or a
    /// payload-derived path.
    EnvelopePathCollision {
        /// The format that rejected the envelope.
        format: EventFormat,
        /// Where the collision was found, if known.
        location: ErrorLocation,
        /// A human-readable description of the collision.
        message: String,
    },
}

impl QuaminaError {
    /// Start building a generic "malformed event bytes" error for `format`.
    ///
    /// Chain [`at_byte_offset`](Self::at_byte_offset), [`at_field_index`](Self::at_field_index),
    /// [`with_message`](Self::with_message), and [`with_source`](Self::with_source) to attach detail.
    #[must_use]
    pub const fn invalid_event(format: EventFormat) -> Self {
        Self::InvalidEvent {
            format,
            location: ErrorLocation {
                field_index: None,
                byte_offset: None,
            },
            message: String::new(),
            source: None,
        }
    }

    /// The format that produced this error.
    ///
    /// Variants that predate format tracking (`InvalidJson` and friends)
    /// report [`EventFormat::Json`], since they only ever arise from the
    /// default JSON path.
    #[must_use]
    pub const fn format(&self) -> EventFormat {
        match self {
            Self::InvalidEvent { format, .. }
            | Self::InvalidEventPath { format, .. }
            | Self::AmbiguousEventPath { format, .. }
            | Self::InvalidCanonicalField { format, .. }
            | Self::ConflictingArrayId { format, .. }
            | Self::DuplicateEventField { format, .. }
            | Self::EventLimitExceeded { format, .. }
            | Self::UnsupportedEventValue { format, .. }
            | Self::UnsupportedMapKey { format, .. }
            | Self::UnsupportedFormatFeature { format, .. }
            | Self::InvalidSchema { format, .. }
            | Self::MissingEventSchema { format, .. }
            | Self::InvalidEnvelope { format, .. }
            | Self::ConflictingEnvelopeHeaders { format, .. }
            | Self::EnvelopePathCollision { format, .. } => *format,
            Self::InvalidJson(_)
            | Self::InvalidPattern(_)
            | Self::InvalidUtf8
            | Self::UnsupportedMediaType(_)
            | Self::PatternTooComplex(_) => EventFormat::Json,
        }
    }

    /// Where in the event this error occurred, if the decoder recorded one.
    #[must_use]
    pub const fn location(&self) -> ErrorLocation {
        match self {
            Self::InvalidEvent { location, .. }
            | Self::InvalidEventPath { location, .. }
            | Self::AmbiguousEventPath { location, .. }
            | Self::InvalidCanonicalField { location, .. }
            | Self::ConflictingArrayId { location, .. }
            | Self::DuplicateEventField { location, .. }
            | Self::EventLimitExceeded { location, .. }
            | Self::UnsupportedEventValue { location, .. }
            | Self::UnsupportedMapKey { location, .. }
            | Self::UnsupportedFormatFeature { location, .. }
            | Self::MissingEventSchema { location, .. }
            | Self::InvalidEnvelope { location, .. }
            | Self::ConflictingEnvelopeHeaders { location, .. }
            | Self::EnvelopePathCollision { location, .. } => *location,
            Self::InvalidSchema { .. }
            | Self::InvalidJson(_)
            | Self::InvalidPattern(_)
            | Self::InvalidUtf8
            | Self::UnsupportedMediaType(_)
            | Self::PatternTooComplex(_) => ErrorLocation {
                field_index: None,
                byte_offset: None,
            },
        }
    }

    const fn location_mut(&mut self) -> Option<&mut ErrorLocation> {
        match self {
            Self::InvalidEvent { location, .. }
            | Self::InvalidEventPath { location, .. }
            | Self::AmbiguousEventPath { location, .. }
            | Self::InvalidCanonicalField { location, .. }
            | Self::ConflictingArrayId { location, .. }
            | Self::DuplicateEventField { location, .. }
            | Self::EventLimitExceeded { location, .. }
            | Self::UnsupportedEventValue { location, .. }
            | Self::UnsupportedMapKey { location, .. }
            | Self::UnsupportedFormatFeature { location, .. }
            | Self::MissingEventSchema { location, .. }
            | Self::InvalidEnvelope { location, .. }
            | Self::ConflictingEnvelopeHeaders { location, .. }
            | Self::EnvelopePathCollision { location, .. } => Some(location),
            _ => None,
        }
    }

    /// Attach a byte offset into the source event to this error.
    #[must_use]
    pub const fn at_byte_offset(mut self, offset: usize) -> Self {
        if let Some(location) = self.location_mut() {
            location.byte_offset = Some(offset);
        }
        self
    }

    /// Attach the index of the offending field (within a raw field list) to this error.
    #[must_use]
    pub const fn at_field_index(mut self, index: usize) -> Self {
        if let Some(location) = self.location_mut() {
            location.field_index = Some(index);
        }
        self
    }

    /// Attach a human-readable message to this error, replacing any existing one.
    #[must_use]
    pub fn with_message(mut self, message: impl Into<String>) -> Self {
        match &mut self {
            Self::InvalidEvent { message: m, .. }
            | Self::InvalidEventPath { message: m, .. }
            | Self::InvalidCanonicalField { message: m, .. }
            | Self::EventLimitExceeded { message: m, .. }
            | Self::UnsupportedEventValue { message: m, .. }
            | Self::UnsupportedMapKey { message: m, .. }
            | Self::UnsupportedFormatFeature { message: m, .. }
            | Self::InvalidSchema { message: m, .. }
            | Self::MissingEventSchema { message: m, .. }
            | Self::InvalidEnvelope { message: m, .. }
            | Self::ConflictingEnvelopeHeaders { message: m, .. }
            | Self::EnvelopePathCollision { message: m, .. } => *m = message.into(),
            _ => {}
        }
        self
    }

    /// Attach the underlying decoder error that caused this error.
    #[must_use]
    pub fn with_source(mut self, source: impl std::error::Error + Send + Sync + 'static) -> Self {
        if let Self::InvalidEvent { source: s, .. } = &mut self {
            *s = Some(Box::new(source));
        }
        self
    }
}

impl fmt::Display for QuaminaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidJson(msg) => write!(f, "invalid JSON: {msg}"),
            Self::InvalidPattern(msg) => write!(f, "invalid pattern: {msg}"),
            Self::InvalidUtf8 => write!(f, "invalid UTF-8"),
            Self::UnsupportedMediaType(mt) => {
                write!(f, "media type \"{mt}\" is not supported by Quamina")
            }
            Self::PatternTooComplex(msg) => {
                write!(f, "pattern too complex: {msg}")
            }
            Self::InvalidEvent {
                format, message, ..
            } => write!(f, "invalid {format:?} event: {message}"),
            Self::InvalidEventPath {
                format, message, ..
            } => write!(f, "invalid {format:?} event path: {message}"),
            Self::AmbiguousEventPath { format, .. } => {
                write!(f, "ambiguous {format:?} event path")
            }
            Self::InvalidCanonicalField {
                format, message, ..
            } => write!(f, "invalid {format:?} canonical field: {message}"),
            Self::ConflictingArrayId { format, id, .. } => {
                write!(f, "conflicting array id {id} in {format:?} event")
            }
            Self::DuplicateEventField { format, .. } => {
                write!(f, "duplicate field in {format:?} event")
            }
            Self::EventLimitExceeded {
                format, message, ..
            } => write!(f, "{format:?} event limit exceeded: {message}"),
            Self::UnsupportedEventValue {
                format, message, ..
            } => write!(f, "unsupported {format:?} event value: {message}"),
            Self::UnsupportedMapKey {
                format, message, ..
            } => write!(f, "unsupported {format:?} map key: {message}"),
            Self::UnsupportedFormatFeature {
                format, message, ..
            } => write!(f, "unsupported {format:?} feature: {message}"),
            Self::InvalidSchema { format, message } => {
                write!(f, "invalid {format:?} schema: {message}")
            }
            Self::MissingEventSchema {
                format, message, ..
            } => write!(f, "missing {format:?} schema: {message}"),
            Self::InvalidEnvelope {
                format, message, ..
            } => write!(f, "invalid {format:?} envelope: {message}"),
            Self::ConflictingEnvelopeHeaders {
                format, message, ..
            } => write!(f, "conflicting {format:?} envelope headers: {message}"),
            Self::EnvelopePathCollision {
                format, message, ..
            } => write!(f, "{format:?} envelope path collision: {message}"),
        }
    }
}

/// Limits on pattern complexity to prevent OOM and stack exhaustion.
///
/// Four complementary limits, each catching a different attack vector:
/// - **Nesting depth**: prevents stack exhaustion and deep-nesting attacks
/// - **Field count**: prevents wide patterns with hundreds of fields
/// - **Arena byte budget**: per-field backstop that catches automaton complexity whatever produced it
/// - **State count**: prevents exponential field-matcher blowup from mixed-type matchers
///
/// # Defaults
/// - `max_pattern_depth`: 256 (jq precedent)
/// - `max_fields_per_pattern`: 256
/// - `arena_byte_budget`: 10 MB (regex crate precedent)
/// - `max_states_per_pattern`: 1024
#[derive(Debug, Clone)]
pub struct PatternLimits {
    /// Maximum nesting depth of a pattern (default: 256)
    pub max_pattern_depth: usize,
    /// Maximum number of fields per pattern (default: 256)
    pub max_fields_per_pattern: usize,
    /// Maximum arena byte size for any one value matcher (default: 10 MB).
    /// A pattern is rejected when the arena for one of its fields would exceed
    /// this; the matcher as a whole holds one such arena per field it matches on.
    pub arena_byte_budget: usize,
    /// Maximum number of field-matcher states during pattern construction (default: 1024).
    ///
    /// When a field has N mixed-type matchers (e.g. exact + prefix), the state count
    /// multiplies by N for each such field. With K fields of N matchers each, states
    /// grow as N^K. This limit caps the product to prevent exponential memory blowup.
    /// All-exact fields use a bulk optimization that doesn't multiply states, so this
    /// limit only affects patterns mixing matcher types on the same field.
    pub max_states_per_pattern: usize,
}

impl Default for PatternLimits {
    fn default() -> Self {
        Self {
            max_pattern_depth: 256,
            max_fields_per_pattern: 256,
            arena_byte_budget: 10 * 1024 * 1024, // 10 MB
            max_states_per_pattern: 1024,
        }
    }
}

impl std::error::Error for QuaminaError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::InvalidEvent {
                source: Some(source),
                ..
            } => Some(source.as_ref()),
            _ => None,
        }
    }
}

/// Controls how Quamina builds matchers for wildcard and regexp patterns,
/// trading `add_pattern` cost against `matches_for_event` speed. The default is
/// [`BuiltForComfort`](Self::BuiltForComfort).
///
/// Wildcard and regexp patterns compile to nondeterministic automata (NFAs).
/// [`BuiltForComfort`](Self::BuiltForComfort) keeps them as NFAs: matchers stay
/// small and adds stay cheap, but `matches_for_event` slows down about linearly
/// as you add more such patterns.
///
/// [`BuiltForSpeed`](Self::BuiltForSpeed) converts the NFAs to deterministic
/// automata (DFAs) at the next freeze after an add. Matching then no longer
/// scales with pattern count. But some pattern sets make the DFA — and the
/// freeze — grow explosively, up to O(2ⁿ). Call
/// [`matcher_stats`](Quamina::matcher_stats) to track the size for your
/// patterns.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[repr(u8)]
pub enum MatcherBuildMode {
    /// Keep wildcard and regexp matchers as NFAs: compact and cheap to build,
    /// with matching that slows down roughly linearly in the number of such
    /// patterns. This is the default.
    #[default]
    BuiltForComfort = 0,
    /// Convert wildcard and regexp matchers to DFAs at freeze time: near
    /// constant-time matching regardless of pattern count, at the cost of
    /// slower adds and possible explosive growth in matcher size.
    BuiltForSpeed = 1,
}

/// Builder for configuring a Quamina instance
///
/// This provides a Go-compatible builder pattern for creating Quamina instances
/// with custom configuration options.
///
/// # Example
/// ```
/// use quamina::QuaminaBuilder;
///
/// let q = QuaminaBuilder::<String>::new()
///     .with_media_type("application/json")
///     .unwrap()
///     .with_auto_rebuild(true)
///     .build()
///     .unwrap();
/// ```
pub struct QuaminaBuilder<X: Clone + Eq + Hash + Send + Sync = String> {
    /// Whether auto-rebuild is enabled (default: true)
    auto_rebuild_enabled: bool,
    /// Media type (only "application/json" supported)
    media_type_validated: bool,
    /// Custom flattener (if provided, replaces default JSON flattener)
    custom_flattener: Option<Box<dyn flattener::Flattener>>,
    /// Envelope flattener (if provided, event matching goes through
    /// `matches_for_envelope` instead of `matches_for_event`)
    envelope_flattener: Option<Box<dyn envelope::EnvelopeFlattener>>,
    /// Pattern complexity limits
    pattern_limits: PatternLimits,
    /// PhantomData to carry the X type parameter
    _phantom: std::marker::PhantomData<X>,
}

impl<X: Clone + Eq + Hash + Send + Sync> QuaminaBuilder<X> {
    /// Create a new QuaminaBuilder with default settings
    #[must_use]
    pub fn new() -> Self {
        Self {
            auto_rebuild_enabled: true,
            media_type_validated: false,
            custom_flattener: None,
            envelope_flattener: None,
            pattern_limits: PatternLimits::default(),
            _phantom: std::marker::PhantomData,
        }
    }

    /// Specify an envelope flattener, for formats (transport headers,
    /// CloudEvents) that need metadata alongside or instead of an event
    /// body. When set, match with [`Quamina::matches_for_envelope`] instead
    /// of [`Quamina::matches_for_event`].
    #[must_use]
    pub fn with_envelope_flattener(
        mut self,
        flattener: Box<dyn envelope::EnvelopeFlattener>,
    ) -> Self {
        self.envelope_flattener = Some(flattener);
        self
    }

    /// Specify the media type for event parsing
    ///
    /// Currently only "application/json" is supported.
    ///
    /// # Errors
    /// Returns `QuaminaError::UnsupportedMediaType` if the media type is not supported.
    ///
    /// # Example
    /// ```
    /// use quamina::QuaminaBuilder;
    ///
    /// // Valid media type
    /// let builder = QuaminaBuilder::<String>::new()
    ///     .with_media_type("application/json")
    ///     .unwrap();
    ///
    /// // Invalid media type
    /// let result = QuaminaBuilder::<String>::new()
    ///     .with_media_type("text/html");
    /// assert!(result.is_err());
    /// ```
    pub fn with_media_type(mut self, media_type: &str) -> Result<Self, QuaminaError> {
        // Check for conflict with custom flattener
        if self.custom_flattener.is_some() {
            return Err(QuaminaError::InvalidPattern(
                "flattener already specified".into(),
            ));
        }
        match media_type {
            "application/json" => {
                self.media_type_validated = true;
                Ok(self)
            }
            other => Err(QuaminaError::UnsupportedMediaType(other.to_string())),
        }
    }

    /// Specify a custom flattener for event parsing.
    ///
    /// This allows using custom parsers for non-JSON formats (CBOR, Protocol Buffers, etc.).
    /// When a custom flattener is provided, the default JSON flattener is replaced.
    ///
    /// This option cannot be combined with `with_media_type()`.
    ///
    /// # Errors
    /// Returns an error if `with_media_type()` has already been called.
    ///
    /// # Example
    /// ```
    /// use quamina::{QuaminaBuilder, Flattener, SegmentsTreeTracker, OwnedField, QuaminaError};
    ///
    /// struct MyFlattener;
    ///
    /// impl Flattener for MyFlattener {
    ///     fn flatten(
    ///         &mut self,
    ///         event: &[u8],
    ///         tracker: &dyn SegmentsTreeTracker,
    ///     ) -> Result<Vec<OwnedField>, QuaminaError> {
    ///         // Custom parsing logic
    ///         Ok(vec![])
    ///     }
    ///
    ///     fn copy(&self) -> Box<dyn Flattener> {
    ///         Box::new(MyFlattener)
    ///     }
    /// }
    ///
    /// let q = QuaminaBuilder::<String>::new()
    ///     .with_flattener(Box::new(MyFlattener))
    ///     .unwrap()
    ///     .build()
    ///     .unwrap();
    /// ```
    pub fn with_flattener(
        mut self,
        flattener: Box<dyn flattener::Flattener>,
    ) -> Result<Self, QuaminaError> {
        // Check for conflict with media type
        if self.media_type_validated {
            return Err(QuaminaError::InvalidPattern(
                "media-type already specified".into(),
            ));
        }
        if self.custom_flattener.is_some() {
            return Err(QuaminaError::InvalidPattern(
                "flattener specified more than once".into(),
            ));
        }
        self.custom_flattener = Some(flattener);
        Ok(self)
    }

    /// Set the maximum nesting depth for patterns (default: 256).
    ///
    /// # Panics
    /// Panics if `depth` is 0.
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<&str>::new()
    ///     .with_max_pattern_depth(1)
    ///     .build()?;
    /// let err = q.add_pattern("deep", r#"{"a": {"b": ["v"]}}"#).unwrap_err();
    /// assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn with_max_pattern_depth(mut self, depth: usize) -> Self {
        assert!(depth > 0, "max_pattern_depth must be at least 1");
        self.pattern_limits.max_pattern_depth = depth;
        self
    }

    /// Set the maximum number of fields per pattern (default: 256).
    ///
    /// # Panics
    /// Panics if `count` is 0.
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<&str>::new()
    ///     .with_max_fields_per_pattern(1)
    ///     .build()?;
    /// let err = q.add_pattern("wide", r#"{"a": ["1"], "b": ["2"]}"#).unwrap_err();
    /// assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn with_max_fields_per_pattern(mut self, count: usize) -> Self {
        assert!(count > 0, "max_fields_per_pattern must be at least 1");
        self.pattern_limits.max_fields_per_pattern = count;
        self
    }

    /// Set the arena byte budget, the cap on how large the automaton for any
    /// one field may grow (default: 10 MB).
    ///
    /// This bounds pattern complexity, not the matcher's total memory: a
    /// pattern is rejected when the arena for one of its fields would exceed
    /// the budget, and a matcher holds one such arena per field it matches on,
    /// so patterns spread across fields add up past it. Admission measures the
    /// arena's flat buffers, which is cheap but leaves out per-state transition
    /// tables, so an accepted arena reports more than this in
    /// [`matcher_stats`](Quamina::matcher_stats). What the budget does hold
    /// exactly is the memory
    /// [`BuiltForSpeed`](MatcherBuildMode::BuiltForSpeed) would add on top.
    ///
    /// # Panics
    /// Panics if `budget` is 0.
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<&str>::new()
    ///     .with_arena_byte_budget(1)
    ///     .build()?;
    /// let err = q.add_pattern("p", r#"{"x": [{"prefix": "a"}]}"#).unwrap_err();
    /// assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// Each field is budgeted on its own, so a matcher can hold much more than
    /// the budget in total:
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<String>::new()
    ///     .with_arena_byte_budget(4_000)
    ///     .build()?;
    /// for i in 0..3 {
    ///     q.add_pattern(format!("p{i}"), &format!(r#"{{"f{i}": [{{"prefix": "abcdefghij{i}"}}]}}"#))?;
    /// }
    /// assert!(q.matcher_stats().bytes > 4_000);
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn with_arena_byte_budget(mut self, budget: usize) -> Self {
        assert!(budget > 0, "arena_byte_budget must be at least 1");
        self.pattern_limits.arena_byte_budget = budget;
        self
    }

    /// Set the maximum field-matcher states per pattern (default: 1024).
    ///
    /// # Panics
    /// Panics if `max_states` is 0.
    ///
    /// ```
    /// # use quamina::{QuaminaBuilder, QuaminaError};
    /// # fn main() -> Result<(), QuaminaError> {
    /// let mut q = QuaminaBuilder::<&str>::new()
    ///     .with_max_states_per_pattern(1)
    ///     .build()?;
    /// let err = q.add_pattern("p", r#"{"a": ["x", {"prefix": "y"}]}"#).unwrap_err();
    /// assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn with_max_states_per_pattern(mut self, max_states: usize) -> Self {
        assert!(max_states > 0, "max_states_per_pattern must be at least 1");
        self.pattern_limits.max_states_per_pattern = max_states;
        self
    }

    /// Enable or disable automatic pruner rebuilding
    ///
    /// When enabled (default), the matcher will automatically rebuild its internal
    /// data structures when the ratio of deleted to active patterns exceeds a threshold.
    /// This helps maintain matching performance after many deletions.
    ///
    /// # Example
    /// ```
    /// use quamina::QuaminaBuilder;
    ///
    /// // Disable auto-rebuild for manual control
    /// let q = QuaminaBuilder::<String>::new()
    ///     .with_auto_rebuild(false)
    ///     .build()
    ///     .unwrap();
    /// ```
    #[must_use]
    pub const fn with_auto_rebuild(mut self, enabled: bool) -> Self {
        self.auto_rebuild_enabled = enabled;
        self
    }

    /// Build the Quamina instance.
    ///
    /// # Example
    /// ```
    /// use quamina::QuaminaBuilder;
    ///
    /// let q = QuaminaBuilder::<String>::new()
    ///     .build()
    ///     .unwrap();
    /// ```
    ///
    /// # Errors
    ///
    /// The current implementation always returns `Ok`. The fallible signature is
    /// reserved so future builder options (e.g. validating a custom flattener or
    /// pattern storage backend) can surface configuration errors without a
    /// breaking API change.
    pub fn build(self) -> Result<Quamina<X>, QuaminaError> {
        Ok(Quamina {
            automaton: ThreadSafeCoreMatcher::with_limits(
                self.pattern_limits.arena_byte_budget,
                self.pattern_limits.max_states_per_pattern,
            ),
            pattern_defs: FxHashMap::default(),
            next_add_position: 0,
            deleted_patterns: FxHashSet::default(),
            automaton_is_stale: false,
            segments_tree: SegmentsTree::new(),
            custom_flattener: self.custom_flattener.map(Mutex::new),
            envelope_flattener: self.envelope_flattener.map(Mutex::new),
            pruner_stats: PrunerStats::new(),
            auto_rebuild_enabled: self.auto_rebuild_enabled,
            pattern_limits: self.pattern_limits,
        })
    }
}

impl<X: Clone + Eq + Hash + Send + Sync> Default for QuaminaBuilder<X> {
    fn default() -> Self {
        Self::new()
    }
}

/// The main pattern matcher
///
/// Quamina uses automaton-based matching for all supported operators (exact, prefix, suffix,
/// wildcard, numeric comparisons, CIDR, I-Regexp with lookarounds, etc.)
///
/// Quamina is Clone, allowing you to create snapshots for concurrent use:
/// ```
/// # use quamina::Quamina;
/// let mut q = Quamina::new();
/// q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
///
/// // Clone for use in another thread
/// let q_snapshot = q.clone();
/// ```
///
/// For shared concurrent access, wrap in Arc:
/// ```
/// # use quamina::Quamina;
/// use std::sync::Arc;
///
/// let q = Arc::new(Quamina::<String>::new());
/// let q_clone = Arc::clone(&q);
/// // Both can now be used for concurrent matching
/// ```
pub struct Quamina<X: Clone + Eq + Hash + Send + Sync = String> {
    /// Automaton-based matcher
    automaton: ThreadSafeCoreMatcher<X>,
    /// The live pattern definitions, and the source of truth for rebuilding
    /// and cloning. A delete drops its definitions from here, so an id in this
    /// map is never also in `deleted_patterns`.
    pattern_defs: FxHashMap<X, Vec<StoredPattern>>,
    /// Stamp given to the next stored pattern, recording add order
    next_add_position: usize,
    /// Ids whose definitions are gone but whose states the automaton still
    /// holds, so their matches have to be filtered out of results until a
    /// rebuild reclaims them. Adding an id again lifts the filter.
    deleted_patterns: FxHashSet<X>,
    /// Whether the automaton holds states for definitions that are no longer
    /// live, which makes it bigger than a fresh build from `pattern_defs`.
    automaton_is_stale: bool,
    /// Segments tree for fast field skipping during event parsing
    segments_tree: SegmentsTree,
    /// Custom flattener for non-JSON formats (if provided)
    custom_flattener: Option<Mutex<Box<dyn flattener::Flattener>>>,
    /// Envelope flattener for headers/CloudEvents formats (if provided)
    envelope_flattener: Option<Mutex<Box<dyn envelope::EnvelopeFlattener>>>,
    /// Statistics for auto-rebuild decisions
    pruner_stats: PrunerStats,
    /// Whether auto-rebuild is enabled (default: true)
    auto_rebuild_enabled: bool,
    /// Pattern complexity limits
    pattern_limits: PatternLimits,
}

impl<X: Clone + Eq + Hash + Send + Sync> Clone for Quamina<X> {
    fn clone(&self) -> Self {
        // Built from the live definitions alone, so the clone starts with
        // nothing left to reclaim and nothing to filter out.
        let (automaton, segments_tree) = self.build_from_live_patterns();

        // Copy custom flattener if present
        let custom_flattener = self.custom_flattener.as_ref().map(|f| {
            let flattener = f.lock();
            Mutex::new(flattener.copy())
        });
        let envelope_flattener = self.envelope_flattener.as_ref().map(|f| {
            let flattener = f.lock();
            Mutex::new(flattener.copy())
        });

        Self {
            automaton,
            pattern_defs: self.pattern_defs.clone(),
            next_add_position: self.next_add_position,
            deleted_patterns: FxHashSet::default(),
            automaton_is_stale: false,
            segments_tree,
            custom_flattener,
            envelope_flattener,
            pruner_stats: PrunerStats::new(),
            auto_rebuild_enabled: self.auto_rebuild_enabled,
            pattern_limits: self.pattern_limits.clone(),
        }
    }
}

impl<X: Clone + Eq + Hash + Send + Sync> Quamina<X> {
    /// Create a new Quamina instance with default pattern complexity limits.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// let mut q = Quamina::<String>::new();
    /// assert!(q.is_empty());
    /// ```
    #[must_use]
    pub fn new() -> Self {
        let limits = PatternLimits::default();
        Self {
            automaton: ThreadSafeCoreMatcher::with_limits(
                limits.arena_byte_budget,
                limits.max_states_per_pattern,
            ),
            pattern_defs: FxHashMap::default(),
            next_add_position: 0,
            deleted_patterns: FxHashSet::default(),
            automaton_is_stale: false,
            segments_tree: SegmentsTree::new(),
            custom_flattener: None,
            envelope_flattener: None,
            pruner_stats: PrunerStats::new(),
            auto_rebuild_enabled: true,
            pattern_limits: limits,
        }
    }

    /// Add a pattern with the given identifier.
    ///
    /// `pattern_json` is a JSON object whose values are arrays of match expressions;
    /// see the [README](https://github.com/baldawarishi/quamina-rs#patterns) for the full pattern syntax.
    ///
    /// # Example
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("alert", r#"{"severity": ["high", "critical"]}"#)?;
    /// assert!(q.matches_for_event(br#"{"severity":"high"}"#)?.contains(&"alert"));
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// Returns [`QuaminaError::InvalidJson`] when `pattern_json` is not
    /// syntactically valid JSON, [`QuaminaError::InvalidPattern`] when the JSON
    /// is well-formed but is not a pattern object (e.g. a leaf is not an array,
    /// or the pattern uses unsupported syntax), and
    /// [`QuaminaError::PatternTooComplex`] when the pattern would exceed the
    /// configured [`PatternLimits`] (depth, field count, state count) or the
    /// shared arena byte budget.
    pub fn add_pattern(&mut self, x: X, pattern_json: &str) -> Result<(), QuaminaError> {
        let fields = json::parse_pattern(pattern_json, &self.pattern_limits)?;

        // Route to automaton first — if this fails (e.g. budget exceeded),
        // we must NOT store the pattern in pattern_defs, segments_tree, etc.
        let pattern_fields: Vec<(String, Vec<Matcher>)> = fields.clone().into_iter().collect();
        if let Err(e) = self.automaton.add_pattern(x.clone(), &pattern_fields) {
            // The automaton merges fields one at a time and does not unwind the
            // ones it merged before the failure. Nothing here records the
            // pattern, so only a rebuild can reclaim those states.
            self.automaton_is_stale = true;
            return Err(e);
        }

        // Automaton accepted — now commit to bookkeeping state
        for field_path in fields.keys() {
            let segment_path = field_path.replace('.', "\n");
            self.segments_tree.add(&segment_path);
        }

        // If pattern was previously deleted, un-delete it
        self.deleted_patterns.remove(&x);

        // Store pattern definition for cloning/rebuild
        self.pattern_defs.entry(x).or_default().push(StoredPattern {
            added_at: self.next_add_position,
            fields,
        });
        self.next_add_position += 1;

        Ok(())
    }

    /// Find all patterns that match the given event.
    ///
    /// `event` must be valid UTF-8 JSON bytes (objects, not arrays or scalars).
    ///
    /// # Example
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("p1", r#"{"status": ["error"]}"#)?;
    /// q.add_pattern("p2", r#"{"level": [1, 2, 3]}"#)?;
    /// let hits = q.matches_for_event(br#"{"status":"error","level":2}"#)?;
    /// assert!(hits.contains(&"p1") && hits.contains(&"p2"));
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// Returns [`QuaminaError::InvalidJson`] when `event` is not a valid JSON
    /// object (encoding issues such as invalid UTF-8 inside strings surface as
    /// `InvalidJson` from the parser). If a custom
    /// [`Flattener`] was configured via the builder, this
    /// method propagates any [`QuaminaError`] its
    /// [`flatten`](flattener::Flattener::flatten) implementation returns.
    pub fn matches_for_event(&self, event: &[u8]) -> Result<Vec<X>, QuaminaError> {
        // Check if we have a custom flattener
        if let Some(ref custom_flattener_mutex) = self.custom_flattener {
            // Use custom flattener path
            return self.matches_for_event_custom_flattener(event, custom_flattener_mutex);
        }

        // Default path: use thread-local flattener + NFA buffers (no Mutex overhead)
        TL_FLATTENER.with(|flattener_cell| {
            TL_NFA_BUFS.with(|bufs_cell| {
                let mut flattener = flattener_cell.borrow_mut();
                let mut bufs = bufs_cell.borrow_mut();

                let streaming_fields = flattener.flatten(event, &self.segments_tree)?;

                // Sort by path for automaton matching
                streaming_fields.sort_unstable_by(|a, b| a.path.cmp(&b.path));

                let raw_matches = self
                    .automaton
                    .matches_for_fields_direct(streaming_fields, &mut bufs);

                Ok(self.filter_deleted_matches(raw_matches))
            })
        })
    }

    /// Match using a custom flattener (slower path with owned data)
    fn matches_for_event_custom_flattener(
        &self,
        event: &[u8],
        custom_flattener_mutex: &Mutex<Box<dyn flattener::Flattener>>,
    ) -> Result<Vec<X>, QuaminaError> {
        // Get owned fields from custom flattener (still needs Mutex — user-provided)
        let mut custom_flattener = custom_flattener_mutex.lock();
        let owned_fields = custom_flattener.flatten(event, &self.segments_tree)?;
        drop(custom_flattener); // Release lock early

        Ok(self.matches_for_owned_fields(owned_fields))
    }

    /// Find all patterns that match the given transport envelope (headers,
    /// and optionally a body decoded by the configured
    /// [`EnvelopeFlattener`](envelope::EnvelopeFlattener)).
    ///
    /// # Errors
    ///
    /// Returns [`QuaminaError::InvalidPattern`] if no envelope flattener was
    /// configured via [`QuaminaBuilder::with_envelope_flattener`].
    /// Otherwise propagates any [`QuaminaError`] the flattener's
    /// `flatten_envelope` implementation returns.
    pub fn matches_for_envelope(&self, envelope: &Envelope) -> Result<Vec<X>, QuaminaError> {
        let Some(ref envelope_flattener_mutex) = self.envelope_flattener else {
            return Err(QuaminaError::InvalidPattern(
                "no envelope flattener configured".to_owned(),
            ));
        };
        let mut envelope_flattener = envelope_flattener_mutex.lock();
        let owned_fields = envelope_flattener.flatten_envelope(envelope, &self.segments_tree)?;
        drop(envelope_flattener);

        Ok(self.matches_for_owned_fields(owned_fields))
    }

    /// Shared tail of the owned-field match paths: sort by path and run the
    /// automaton using thread-local NFA buffers.
    fn matches_for_owned_fields(&self, owned_fields: Vec<OwnedField>) -> Vec<X> {
        use std::sync::Arc;

        // Convert OwnedField to flatten_json::Field with owned data
        let mut streaming_fields: Vec<flatten_json::Field<'static>> = owned_fields
            .into_iter()
            .map(|f| flatten_json::Field {
                path: Arc::from(f.path.as_slice()),
                val: flatten_json::FieldValue::Owned(f.val),
                array_trail: f.array_trail.into(),
                is_number: f.is_number,
            })
            .collect();

        // Sort by path for automaton matching
        streaming_fields.sort_unstable_by(|a, b| a.path.cmp(&b.path));

        // Get matches from automaton using thread-local NFA buffers
        TL_NFA_BUFS.with(|bufs_cell| {
            let mut bufs = bufs_cell.borrow_mut();
            let raw_matches = self
                .automaton
                .matches_for_fields_direct(&streaming_fields, &mut bufs);
            self.filter_deleted_matches(raw_matches)
        })
    }

    /// Build a fresh automaton and segments tree from the live pattern
    /// definitions, replaying them in the order they were originally added.
    ///
    /// This repeats the work [`add_pattern`](Self::add_pattern) does, so the
    /// pair comes back as the surviving adds would have built it — including a
    /// tree that has forgotten the fields only deleted patterns mentioned.
    fn build_from_live_patterns(&self) -> (ThreadSafeCoreMatcher<X>, SegmentsTree) {
        let automaton = ThreadSafeCoreMatcher::with_limits(
            self.pattern_limits.arena_byte_budget,
            self.pattern_limits.max_states_per_pattern,
        );
        automaton.set_build_mode(self.automaton.build_mode());

        let mut live: Vec<(&X, &StoredPattern)> = self
            .pattern_defs
            .iter()
            .flat_map(|(id, patterns)| patterns.iter().map(move |stored| (id, stored)))
            .collect();
        live.sort_unstable_by_key(|(_, stored)| stored.added_at);

        let mut segments_tree = SegmentsTree::new();
        for (id, stored) in live {
            for field_path in stored.fields.keys() {
                segments_tree.add(&field_path.replace('.', "\n"));
            }
            let pattern_fields: Vec<(String, Vec<Matcher>)> = stored
                .fields
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            automaton
                .add_pattern(id.clone(), &pattern_fields)
                .expect("pre-validated pattern should not fail on rebuild");
        }

        (automaton, segments_tree)
    }

    /// Remove soft-deleted patterns from raw match results and update pruner stats.
    fn filter_deleted_matches(&self, raw_matches: Vec<X>) -> Vec<X> {
        if self.deleted_patterns.is_empty() {
            self.pruner_stats.add_emitted(raw_matches.len() as u64);
            raw_matches
        } else {
            let raw_count = raw_matches.len();
            let filtered: Vec<X> = raw_matches
                .into_iter()
                .filter(|x| !self.deleted_patterns.contains(x))
                .collect();
            let filtered_count = raw_count - filtered.len();
            self.pruner_stats.add_emitted(filtered.len() as u64);
            self.pruner_stats.add_filtered(filtered_count as u64);
            filtered
        }
    }

    /// Access the underlying automaton (for direct matching without Mutex).
    #[doc(hidden)]
    pub const fn automaton(&self) -> &ThreadSafeCoreMatcher<X> {
        &self.automaton
    }

    /// Access the segments tree (for direct flattening without Mutex).
    #[doc(hidden)]
    pub const fn segments_tree(&self) -> &SegmentsTree {
        &self.segments_tree
    }

    /// Flatten an event without matching (for benchmarking)
    #[doc(hidden)]
    pub fn flatten_only(&self, event: &[u8]) -> Result<usize, QuaminaError> {
        TL_FLATTENER.with(|flattener_cell| {
            let mut flattener = flattener_cell.borrow_mut();
            let fields = flattener.flatten(event, &self.segments_tree)?;
            Ok(fields.len())
        })
    }

    /// Mark all patterns with the given identifier as deleted.
    ///
    /// Deleted patterns are excluded from match results immediately, but
    /// their automaton memory is not reclaimed until [`rebuild()`](Self::rebuild).
    ///
    /// # Example
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("temp", r#"{"x": [1]}"#)?;
    /// q.delete_patterns(&"temp")?;
    /// assert!(q.matches_for_event(br#"{"x":1}"#)?.is_empty());
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// The current implementation always returns `Ok`. The fallible signature
    /// matches the upstream Quamina API and is reserved for future backends
    /// (such as a [`LivePatternsState`](https://github.com/timbray/quamina#dynamic-pattern-storage)
    /// store) that may need to surface I/O or storage errors.
    pub fn delete_patterns(&mut self, x: &X) -> Result<(), QuaminaError> {
        // Drop the definitions outright so nothing can replay them later, the
        // way Go's memState.Delete filters the id's entries out at delete time.
        if self.pattern_defs.remove(x).is_none() {
            return Ok(()); // Pattern doesn't exist or is already deleted
        }

        // The automaton keeps the states it built for those definitions until a
        // rebuild, so its matches for this id have to be suppressed meanwhile.
        self.deleted_patterns.insert(x.clone());
        self.automaton_is_stale = true;

        Ok(())
    }

    /// Checks whether any pattern matches the event.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// q.add_pattern("b", r#"{"x": [1]}"#)?;
    /// assert!(q.has_matches(br#"{"x":1}"#)?);
    /// assert!(!q.has_matches(br#"{"x":2}"#)?);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// Returns the same errors as [`matches_for_event`](Self::matches_for_event).
    pub fn has_matches(&self, event: &[u8]) -> Result<bool, QuaminaError> {
        // Use matches_for_event and check if non-empty
        // This could be optimized to return early, but for now this is simpler
        Ok(!self.matches_for_event(event)?.is_empty())
    }

    /// Counts how many unique pattern IDs match the event.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// q.add_pattern("b", r#"{"x": [1]}"#)?;
    /// assert_eq!(q.count_matches(br#"{"x":1}"#)?, 2);
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// # Errors
    ///
    /// Returns the same errors as [`matches_for_event`](Self::matches_for_event).
    pub fn count_matches(&self, event: &[u8]) -> Result<usize, QuaminaError> {
        Ok(self.matches_for_event(event)?.len())
    }

    /// Returns the number of unique pattern IDs stored
    pub fn pattern_count(&self) -> usize {
        self.pattern_defs.len()
    }

    /// Returns true if no patterns are stored
    pub fn is_empty(&self) -> bool {
        self.pattern_count() == 0
    }

    /// Get the pruner statistics
    pub const fn pruner_stats(&self) -> &PrunerStats {
        &self.pruner_stats
    }

    /// Get aggregate arena statistics across all frozen value matchers.
    ///
    /// Returns stats covering state counts, table sizes, epsilon transitions,
    /// closure sizes, and flattened buffer usage. Useful for diagnostics and
    /// verifying optimization effectiveness.
    pub fn arena_stats(&self) -> automaton::arena::Stats {
        self.automaton.arena_stats()
    }

    /// Reports resource-consumption data for the materialized matcher. The
    /// figures depend on the [`add_pattern`](Self::add_pattern) calls you made
    /// and on the current [`MatcherBuildMode`]:
    /// [`BuiltForSpeed`](MatcherBuildMode::BuiltForSpeed) reports the converted
    /// DFAs, which are usually larger than the NFAs
    /// [`BuiltForComfort`](MatcherBuildMode::BuiltForComfort) reports.
    ///
    /// A pattern too costly to convert up front gets a DFA cached as matching
    /// visits it instead, so under `BuiltForSpeed` these figures can also grow
    /// with the events you feed [`matches_for_event`](Self::matches_for_event).
    ///
    /// The most useful figure is [`MatcherStats::bytes`], an estimate of the
    /// memory consumed by the matcher's data structures. Its growth
    /// correlates well with the slowdown in `add_pattern` and
    /// [`matches_for_event`](Self::matches_for_event) performance when the
    /// patterns being added are of the `wildcard` or `regexp` flavors.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::<&str>::new();
    /// assert_eq!(q.matcher_stats().bytes, 0);
    /// q.add_pattern("p", r#"{"x": [{"wildcard": "*z"}]}"#)?;
    /// let stats = q.matcher_stats();
    /// assert!(stats.bytes > 0 && stats.states > 0);
    /// # Ok(())
    /// # }
    /// ```
    #[must_use]
    pub fn matcher_stats(&self) -> MatcherStats {
        self.automaton.matcher_stats()
    }

    /// Set the [`MatcherBuildMode`] used when freezing wildcard and regexp
    /// matchers; see [`MatcherBuildMode`] for the comfort/speed trade-off.
    ///
    /// The mode applies to the whole matcher, and the next match after this call
    /// re-freezes the automaton under it. Deleting patterns does not disturb the
    /// choice: [`rebuild`](Self::rebuild) and cloning replay under whichever mode
    /// is set. Go upstream instead pins each pattern's mode when it is added.
    ///
    /// ```
    /// # use quamina::{Quamina, MatcherBuildMode};
    /// let mut q = Quamina::<String>::new();
    /// assert_eq!(q.matcher_build_mode(), MatcherBuildMode::BuiltForComfort);
    /// q.set_matcher_build_mode(MatcherBuildMode::BuiltForSpeed);
    /// assert_eq!(q.matcher_build_mode(), MatcherBuildMode::BuiltForSpeed);
    /// ```
    pub fn set_matcher_build_mode(&mut self, mode: MatcherBuildMode) {
        self.automaton.set_build_mode(mode);
    }

    /// Return the current [`MatcherBuildMode`] (default
    /// [`BuiltForComfort`](MatcherBuildMode::BuiltForComfort)).
    #[must_use]
    pub fn matcher_build_mode(&self) -> MatcherBuildMode {
        self.automaton.build_mode()
    }

    /// Enable or disable auto-rebuild
    pub const fn set_auto_rebuild(&mut self, enabled: bool) {
        self.auto_rebuild_enabled = enabled;
    }

    /// Check if auto-rebuild is enabled
    pub const fn auto_rebuild_enabled(&self) -> bool {
        self.auto_rebuild_enabled
    }

    /// Rebuild the automaton from only live patterns, reclaiming memory from soft-deleted patterns.
    ///
    /// The live patterns are replayed in the order they were added, so the
    /// rebuilt matcher is the one those [`add_pattern`](Self::add_pattern)
    /// calls would have built on their own.
    ///
    /// Returns the number of soft-deleted ids dropped. A rebuild also reclaims
    /// the definitions behind a delete whose id was added again afterwards, but
    /// those are not counted, the id itself still being live.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// q.add_pattern("b", r#"{"x": [2]}"#)?;
    /// assert_eq!(q.pattern_count(), 2);
    ///
    /// q.delete_patterns(&"a")?;
    /// let purged = q.rebuild();
    /// assert_eq!(purged, 1);
    /// assert_eq!(q.pattern_count(), 1);
    /// # Ok(())
    /// # }
    /// ```
    pub fn rebuild(&mut self) -> usize {
        if !self.automaton_is_stale {
            return 0;
        }

        let (new_automaton, new_segments_tree) = self.build_from_live_patterns();

        let purged = self.deleted_patterns.len();
        self.deleted_patterns.clear();
        self.automaton_is_stale = false;
        self.pruner_stats.reset();

        self.automaton = new_automaton;
        self.segments_tree = new_segments_tree;

        purged
    }

    /// Check if rebuild is recommended based on pruner statistics.
    /// Returns true when filtered/emitted ratio exceeds 0.2 and at least 1 000
    /// total observations have been recorded.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// let q = Quamina::<&str>::new();
    /// // No activity yet — rebuild not recommended.
    /// assert!(!q.should_rebuild());
    /// ```
    pub fn should_rebuild(&self) -> bool {
        self.pruner_stats.should_rebuild()
    }

    /// Perform rebuild only when auto-rebuild is enabled and [`should_rebuild()`](Self::should_rebuild)
    /// returns true. Returns the number of patterns purged, or 0 if no rebuild occurred.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// q.delete_patterns(&"a")?;
    /// // Threshold not yet met, so no rebuild happens.
    /// assert_eq!(q.maybe_rebuild(), 0);
    /// # Ok(())
    /// # }
    /// ```
    pub fn maybe_rebuild(&mut self) -> usize {
        if self.auto_rebuild_enabled && self.pruner_stats.should_rebuild() {
            self.rebuild()
        } else {
            0
        }
    }

    /// Removes all patterns and resets the matcher to its initial state.
    ///
    /// ```
    /// # use quamina::Quamina;
    /// # fn main() -> Result<(), quamina::QuaminaError> {
    /// let mut q = Quamina::new();
    /// q.add_pattern("a", r#"{"x": [1]}"#)?;
    /// assert!(!q.is_empty());
    /// q.clear();
    /// assert!(q.is_empty());
    /// # Ok(())
    /// # }
    /// ```
    pub fn clear(&mut self) {
        let build_mode = self.automaton.build_mode();
        self.automaton = ThreadSafeCoreMatcher::with_limits(
            self.pattern_limits.arena_byte_budget,
            self.pattern_limits.max_states_per_pattern,
        );
        self.automaton.set_build_mode(build_mode);
        self.pattern_defs.clear();
        self.next_add_position = 0;
        self.deleted_patterns.clear();
        self.automaton_is_stale = false;
        self.segments_tree = SegmentsTree::new();
        self.pruner_stats.reset();
    }

    /// Returns a list of all active (non-deleted) pattern identifiers.
    ///
    /// This provides a way to inspect what patterns are currently registered
    /// with the Quamina instance.
    ///
    /// # Example
    /// ```
    /// # use quamina::Quamina;
    /// let mut q: Quamina<String> = Quamina::new();
    /// q.add_pattern("p1".into(), r#"{"status": ["active"]}"#).unwrap();
    /// q.add_pattern("p2".into(), r#"{"type": ["event"]}"#).unwrap();
    ///
    /// let ids = q.list_pattern_ids();
    /// assert_eq!(ids.len(), 2);
    /// ```
    pub fn list_pattern_ids(&self) -> Vec<&X> {
        self.pattern_defs.keys().collect()
    }

    /// Checks if a pattern with the given identifier exists (and hasn't been deleted).
    ///
    /// # Example
    /// ```
    /// # use quamina::Quamina;
    /// let mut q: Quamina<String> = Quamina::new();
    /// let p1: String = "p1".into();
    /// assert!(!q.contains_pattern(&p1));
    ///
    /// q.add_pattern(p1.clone(), r#"{"status": ["active"]}"#).unwrap();
    /// assert!(q.contains_pattern(&p1));
    /// ```
    pub fn contains_pattern(&self, id: &X) -> bool {
        self.pattern_defs.contains_key(id)
    }
}

impl<X: Clone + Eq + Hash + Send + Sync> Default for Quamina<X> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[macro_use]
mod test_helpers;
#[cfg(test)]
mod tests_core;
#[cfg(test)]
mod tests_operators;
#[cfg(test)]
mod tests_stress;
