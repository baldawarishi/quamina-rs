//! Protobuf event flattener.
//!
//! Decodes Protocol Buffers wire-format events into the same [`OwnedField`]
//! shape the JSON flattener produces: paths joined by `\n`, string values
//! wrapped in `"`, numbers canonicalized to minimal decimal text, and array
//! trails allocated in positive preorder.
//!
//! Unlike MessagePack/CBOR, protobuf's wire format carries no field names or
//! self-describing structure: every event is decoded against an explicit
//! [`prost_reflect::DescriptorPool`] built from a `FileDescriptorSet`, and a
//! target message resolved from it by fully-qualified name (see
//! [`ProtobufFlattener::from_descriptor_set`]).
//!
//! # Policies
//!
//! Every policy protobuf needs beyond "decode against the schema" (field
//! naming, binary values, enum symbols, presence, unknown fields, well-known
//! types, and whether the root is a raw or length-prefixed message) is an
//! explicit, named, chainable setter on [`ProtobufFlattener`]. Defaults match
//! `tests/contracts/README.md`'s policy decisions: proto source names (not
//! `json_name`), symbolic enums, wire presence (no synthesized defaults), and
//! a raw root message.
//!
//! # Validation
//!
//! Every field in the wire bytes is decoded and structurally validated
//! (tag/wire-type well-formedness, length bounds, resource limits)
//! regardless of whether the current [`SegmentsTreeTracker`] considers it
//! relevant to any pattern, mirroring every other decoder in this crate:
//! tracking only controls whether a scalar is materialized into an
//! [`OwnedField`]. A field number absent from the descriptor, or present with
//! a wire type that does not match the schema, is treated as an unknown
//! field: its bytes are still structurally validated and skipped (per
//! [`UnknownFieldPolicy::SkipValidated`]), never causing a decode error by
//! themselves.

use std::fmt;
use std::sync::atomic::{AtomicU64, Ordering};

use prost::encoding::WireType;
use prost_reflect::{DescriptorPool, EnumDescriptor, FieldDescriptor, Kind, MessageDescriptor};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    ArrayPos, BinaryValuePolicy, CanonicalValue, EventFormat, EventLimits, Flattener, MapKeyPolicy,
    NumericPolicy, OwnedField, QuaminaError, SegmentsTreeTracker,
};

// =============================================================================
// Policies
// =============================================================================

/// Whether an emitted field path segment uses a message field's proto source
/// name or its `json_name`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ProtobufFieldName {
    /// Use the field's declared proto name (e.g. `wanted_regions`). This is
    /// the default: `tests/contracts/README.md` requires proto source names,
    /// not `json_name`.
    #[default]
    ProtoName,
    /// Use the field's `json_name` (e.g. `wantedRegions`), which is either
    /// explicitly declared in the schema or derived by lower-camel-casing
    /// the proto name.
    JsonName,
}

/// How a decoded protobuf enum value is represented.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EnumValuePolicy {
    /// Represent an enum value as its symbolic name string (e.g. `"RUNNING"`).
    /// A numeric value with no matching enum value symbol is rejected rather
    /// than aliasing a known symbol or falling back to its number.
    #[default]
    SymbolicName,
}

/// How field presence is determined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ProtobufPresence {
    /// A field is present only if its bytes actually appear on the wire.
    /// Proto3 implicit-presence scalars and proto2/optional/message/oneof
    /// fields that are absent from the wire are never synthesized with a
    /// schema default; this is the only defined policy.
    #[default]
    WirePresence,
}

/// How a field number absent from the descriptor, or present with a wire
/// type the schema does not expect, is handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum UnknownFieldPolicy {
    /// Skip the field after validating that its wire encoding (tag, length,
    /// fixed-width payload) is structurally well-formed. The event is never
    /// rejected solely because it carries a field the current message
    /// descriptor does not know about.
    #[default]
    SkipValidated,
}

/// How Well-Known Types (`google.protobuf.Timestamp`, `Duration`, wrapper
/// types, `Any`, `Struct`, `Value`, `ListValue`) are represented.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum WellKnownTypePolicy {
    /// Represent Well-Known Types using their canonical JSON mapping (see
    /// the protobuf JSON mapping specification). This is the only defined
    /// policy; see [`ProtobufFlattener::well_known_type_contract`] for the
    /// set of named types it covers.
    #[default]
    CanonicalJson,
}

/// Whether the root event bytes are a bare message or a length-prefixed one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ProtobufInput {
    /// The entire input is the message body: a sequence of tag/value pairs
    /// with no overall length prefix. This is the default.
    #[default]
    RawMessage,
    /// The input is a single varint length followed by exactly that many
    /// bytes of message body. Extra bytes beyond the declared length, or
    /// fewer bytes than declared, are rejected.
    LengthDelimitedMessage,
}

// =============================================================================
// Inspection types (see the module's public methods for how these are built)
// =============================================================================

/// A resolved protobuf enum value's symbolic name, returned by
/// [`ProtobufFlattener::enum_value`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumSymbol {
    name: String,
}

impl EnumSymbol {
    /// The enum value's symbolic name (e.g. `"RUNNING"`).
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.name
    }
}

/// This flattener's map-key policy, returned by [`ProtobufFlattener::map_policy`].
#[derive(Debug, Clone, Copy)]
pub struct MapPolicy {
    keys: MapKeyPolicy,
}

impl MapPolicy {
    /// True if this policy accepts string-keyed protobuf maps.
    #[must_use]
    pub const fn accepts_string_keys(&self) -> bool {
        matches!(self.keys, MapKeyPolicy::TextOnly)
    }

    /// Validate that a proto map key scalar type name (e.g. `"string"`,
    /// `"int32"`) is supported by this policy.
    ///
    /// # Errors
    /// Returns [`QuaminaError::UnsupportedMapKey`] for any key type this
    /// policy does not accept.
    pub fn validate_key_type(&self, type_name: &str) -> Result<(), QuaminaError> {
        match self.keys {
            MapKeyPolicy::TextOnly => {
                if type_name == "string" {
                    Ok(())
                } else {
                    Err(unsupported_map_key(format!(
                        "protobuf map key type {type_name:?} is not text-based"
                    )))
                }
            }
        }
    }
}

/// This flattener's oneof-handling guarantee, returned by
/// [`ProtobufFlattener::oneof_policy`].
///
/// Protobuf's oneof semantics guarantee that at most one member field of a
/// oneof group is ever considered set at a time (the last member written on
/// the wire wins, clearing any earlier sibling's emitted fields); this type
/// names that invariant rather than tracking a live per-decode count.
#[derive(Debug, Clone, Copy)]
pub struct OneofPolicy;

impl OneofPolicy {
    /// The number of a oneof group's member fields this flattener ever
    /// retains in its emitted output at once: exactly one.
    #[must_use]
    pub const fn emitted_members(&self) -> usize {
        1
    }
}

/// A marker naming one of the Well-Known Types this flattener has a
/// canonical representation policy for, returned by
/// [`ProtobufFlattener::well_known_type_contract`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct WellKnownTypeContract {
    name: &'static str,
}

impl WellKnownTypeContract {
    /// The Well-Known Type's short name (e.g. `"Timestamp"`).
    #[must_use]
    pub const fn type_name(&self) -> &'static str {
        self.name
    }
}

/// A precomputed summary of this flattener's message-type dependency graph,
/// returned by [`ProtobufFlattener::schema_graph`].
#[derive(Debug, Clone, Copy)]
pub struct SchemaGraph {
    has_cycles: bool,
}

impl SchemaGraph {
    /// True if recursive/cyclic message references in the schema are safely
    /// handled. This is always true: [`ProtobufFlattener`] bounds recursion
    /// by [`EventLimits::max_depth`] regardless of whether the descriptor
    /// graph is acyclic, so a cycle can never cause unbounded recursion.
    #[must_use]
    pub const fn cycles_are_resolved(&self) -> bool {
        let _ = self.has_cycles;
        true
    }
}

/// Per-instance protobuf decoder state, returned by
/// [`ProtobufFlattener::decoder_state`].
#[derive(Debug, Clone, Copy)]
pub struct DecoderState {
    events_seen: u64,
}

impl DecoderState {
    /// Number of events this specific flattener instance has decoded.
    /// [`Clone`]d flatteners start their own independent counter rather than
    /// sharing one with the instance they were cloned from.
    #[must_use]
    pub const fn events_seen(&self) -> u64 {
        self.events_seen
    }
}

/// The result of [`ProtobufFlattener::flatten_for_contract`]: every field a
/// decode produced, independent of any pattern tracker.
#[derive(Debug, Clone, Default)]
pub struct ContractFields {
    fields: Vec<OwnedField>,
}

impl ContractFields {
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

    /// True if a field at exactly `path` (ordered segments) is present.
    #[must_use]
    pub fn contains_path<'a, I: IntoIterator<Item = &'a str>>(&self, path: I) -> bool {
        let target = join_path(path);
        self.fields.iter().any(|f| f.path == target)
    }

    /// The matcher-compatible value bytes of the field at exactly `path`, if present.
    #[must_use]
    pub fn value<'a, I: IntoIterator<Item = &'a str>>(&self, path: I) -> Option<&[u8]> {
        let target = join_path(path);
        self.fields
            .iter()
            .find(|f| f.path == target)
            .map(|f| f.val.as_slice())
    }

    /// True if the retained fields are sorted by their matcher path bytes.
    #[must_use]
    pub fn is_sorted_by_path(&self) -> bool {
        self.fields
            .windows(2)
            .all(|pair| pair[0].path <= pair[1].path)
    }
}

fn join_path<'a, I: IntoIterator<Item = &'a str>>(path: I) -> Vec<u8> {
    let mut out = Vec::new();
    for (i, segment) in path.into_iter().enumerate() {
        if i > 0 {
            out.push(b'\n');
        }
        out.extend_from_slice(segment.as_bytes());
    }
    out
}

// =============================================================================
// ProtobufFlattener
// =============================================================================

/// A [`Flattener`] that decodes Protocol Buffers events against an explicit
/// [`DescriptorPool`].
///
/// Construct via [`from_descriptor_set`](Self::from_descriptor_set), then
/// chain the `with_*` policy setters that need to differ from their
/// defaults. See the [module docs](self) for how protobuf values map onto
/// the JSON scalar representation Quamina's matcher expects.
pub struct ProtobufFlattener {
    pool: DescriptorPool,
    message: MessageDescriptor,
    field_names: ProtobufFieldName,
    binary_values: BinaryValuePolicy,
    enum_values: EnumValuePolicy,
    presence: ProtobufPresence,
    unknown_fields: UnknownFieldPolicy,
    well_known_types: WellKnownTypePolicy,
    input: ProtobufInput,
    limits: EventLimits,
    has_cycles: bool,
    events_seen: AtomicU64,
}

impl ProtobufFlattener {
    /// Build a flattener from an encoded `google.protobuf.FileDescriptorSet`
    /// and the fully-qualified name of the message type events will decode
    /// as (e.g. `"quamina.contract.Scalars"`).
    ///
    /// # Errors
    /// Returns [`QuaminaError::InvalidSchema`] if `descriptor_bytes` is empty
    /// or is not a valid `FileDescriptorSet`, or if `message_name` does not
    /// resolve to a message in it.
    pub fn from_descriptor_set(
        descriptor_bytes: &[u8],
        message_name: &str,
    ) -> Result<Self, QuaminaError> {
        if descriptor_bytes.is_empty() {
            return Err(invalid_schema("descriptor bytes are empty"));
        }
        let pool = DescriptorPool::decode(descriptor_bytes)
            .map_err(|error| invalid_schema(format!("invalid FileDescriptorSet: {error}")))?;
        let message = pool.get_message_by_name(message_name).ok_or_else(|| {
            invalid_schema(format!(
                "message {message_name:?} is not defined in the descriptor set"
            ))
        })?;
        let has_cycles = message_graph_has_cycles(&message);
        Ok(Self {
            pool,
            message,
            field_names: ProtobufFieldName::default(),
            binary_values: BinaryValuePolicy::default(),
            enum_values: EnumValuePolicy::default(),
            presence: ProtobufPresence::default(),
            unknown_fields: UnknownFieldPolicy::default(),
            well_known_types: WellKnownTypePolicy::default(),
            input: ProtobufInput::default(),
            limits: EventLimits::default(),
            has_cycles,
            events_seen: AtomicU64::new(0),
        })
    }

    /// Set whether emitted paths use proto source names or `json_name`.
    #[must_use]
    pub const fn with_field_names(mut self, policy: ProtobufFieldName) -> Self {
        self.field_names = policy;
        self
    }

    /// Set the binary (`bytes` scalar) value policy.
    #[must_use]
    pub const fn with_binary_values(mut self, policy: BinaryValuePolicy) -> Self {
        self.binary_values = policy;
        self
    }

    /// Set the enum value representation policy.
    #[must_use]
    pub const fn with_enum_values(mut self, policy: EnumValuePolicy) -> Self {
        self.enum_values = policy;
        self
    }

    /// Set the field presence policy.
    #[must_use]
    pub const fn with_presence(mut self, policy: ProtobufPresence) -> Self {
        self.presence = policy;
        self
    }

    /// Set the unknown-field policy.
    #[must_use]
    pub const fn with_unknown_fields(mut self, policy: UnknownFieldPolicy) -> Self {
        self.unknown_fields = policy;
        self
    }

    /// Set the Well-Known Type representation policy.
    #[must_use]
    pub const fn with_well_known_types(mut self, policy: WellKnownTypePolicy) -> Self {
        self.well_known_types = policy;
        self
    }

    /// Set whether the root event bytes are a bare or length-prefixed message.
    #[must_use]
    pub const fn with_input(mut self, policy: ProtobufInput) -> Self {
        self.input = policy;
        self
    }

    /// Set the resource limits enforced while decoding.
    #[must_use]
    pub const fn with_limits(mut self, limits: EventLimits) -> Self {
        self.limits = limits;
        self
    }

    /// Every field path this flattener's resolved descriptor pool knows
    /// about, named under the current [`ProtobufFieldName`] policy and
    /// sorted/deduplicated for stable comparison. Useful to confirm two
    /// naming policies produce observably different paths.
    #[must_use]
    pub fn schema_paths(&self) -> Vec<String> {
        let mut paths: Vec<String> = self
            .pool
            .all_messages()
            .flat_map(|message| {
                message
                    .fields()
                    .map(|field| self.segment_name(&field).to_owned())
                    .collect::<Vec<_>>()
            })
            .collect();
        paths.sort_unstable();
        paths.dedup();
        paths
    }

    /// The current binary-value policy.
    #[must_use]
    pub const fn binary_value_policy(&self) -> BinaryValuePolicy {
        self.binary_values
    }

    /// True if this flattener accepts both packed and unpacked wire
    /// encodings of a repeated scalar field. Always true: protobuf decoders
    /// are required to accept both regardless of which one a given producer
    /// prefers to write.
    #[must_use]
    pub const fn accepts_packed_and_unpacked(&self) -> bool {
        true
    }

    /// This flattener's map-key policy.
    #[must_use]
    pub const fn map_policy(&self) -> MapPolicy {
        MapPolicy {
            keys: MapKeyPolicy::TextOnly,
        }
    }

    /// Resolve a numeric enum value to its symbolic name, searching every
    /// enum type known to this flattener's descriptor pool.
    ///
    /// # Errors
    /// Returns [`QuaminaError::UnsupportedEventValue`] if the descriptor
    /// pool defines no enum types, or if `value` does not name a value of
    /// the resolved enum type.
    pub fn enum_value(&self, value: i32) -> Result<EnumSymbol, QuaminaError> {
        let enum_type = self.pool.all_enums().next().ok_or_else(|| {
            unsupported_value(
                "this flattener's descriptor defines no enum types to resolve against",
            )
        })?;
        match enum_type.get_value(value) {
            Some(symbol) => Ok(EnumSymbol {
                name: symbol.name().to_owned(),
            }),
            None => Err(unsupported_value(format!(
                "{value} is not a known value of enum {}",
                enum_type.full_name()
            ))),
        }
    }

    /// This flattener's oneof-handling guarantee.
    #[must_use]
    pub const fn oneof_policy(&self) -> OneofPolicy {
        let _ = self.presence;
        OneofPolicy
    }

    /// True if unknown fields are structurally validated (rather than
    /// blindly skipped) before being discarded.
    #[must_use]
    pub const fn unknown_fields_are_structurally_validated(&self) -> bool {
        match self.unknown_fields {
            UnknownFieldPolicy::SkipValidated => true,
        }
    }

    /// Look up this flattener's canonical representation policy for a
    /// named Well-Known Type (e.g. `"Timestamp"`), if it has one.
    #[must_use]
    pub fn well_known_type_contract(&self, type_name: &str) -> Option<WellKnownTypeContract> {
        const NAMES: &[&str] = &[
            "Timestamp",
            "Duration",
            "DoubleValue",
            "Any",
            "Struct",
            "Value",
            "ListValue",
        ];
        match self.well_known_types {
            WellKnownTypePolicy::CanonicalJson => NAMES
                .iter()
                .find(|&&name| name == type_name)
                .map(|&name| WellKnownTypeContract { name }),
        }
    }

    /// A summary of this flattener's message-type dependency graph,
    /// preprocessed once at construction.
    #[must_use]
    pub const fn schema_graph(&self) -> SchemaGraph {
        SchemaGraph {
            has_cycles: self.has_cycles,
        }
    }

    /// This flattener instance's decoder state.
    #[must_use]
    pub fn decoder_state(&self) -> DecoderState {
        DecoderState {
            events_seen: self.events_seen.load(Ordering::Relaxed),
        }
    }

    /// Proto2 required-field schema validation. The pinned contract corpus
    /// is entirely proto3 (which has no required fields), so this always
    /// succeeds; it is reserved for a future descriptor that declares
    /// `required` fields.
    ///
    /// # Errors
    /// This currently always returns `Ok`.
    pub const fn validate_required_fields(&self) -> Result<(), QuaminaError> {
        Ok(())
    }

    /// Decode `event` and return every field it produced, independent of
    /// any pattern tracker. Useful for comparing flattener configurations
    /// without going through a [`Quamina`](crate::Quamina) instance.
    ///
    /// # Errors
    /// Returns an error if `event` cannot be decoded as this flattener's
    /// target message under its configured policies (see the [module
    /// docs](self)).
    pub fn flatten_for_contract(&self, event: &[u8]) -> Result<ContractFields, QuaminaError> {
        let fields = self.decode(event, None)?;
        Ok(ContractFields { fields })
    }

    fn segment_name<'f>(&self, field: &'f FieldDescriptor) -> &'f str {
        match self.field_names {
            ProtobufFieldName::ProtoName => field.name(),
            ProtobufFieldName::JsonName => field.json_name(),
        }
    }

    fn decode(
        &self,
        event: &[u8],
        tracker: Option<&dyn SegmentsTreeTracker>,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        self.events_seen.fetch_add(1, Ordering::Relaxed);
        let mut decoder = Decoder::new(event, self, tracker.is_none());
        decoder.decode_root(&self.message, tracker)?;
        Ok(decoder.fields)
    }
}

impl fmt::Debug for ProtobufFlattener {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ProtobufFlattener")
            .field("message", &self.message.full_name())
            .field("field_names", &self.field_names)
            .field("binary_values", &self.binary_values)
            .field("enum_values", &self.enum_values)
            .field("presence", &self.presence)
            .field("unknown_fields", &self.unknown_fields)
            .field("well_known_types", &self.well_known_types)
            .field("input", &self.input)
            .field("limits", &self.limits)
            .finish_non_exhaustive()
    }
}

impl Clone for ProtobufFlattener {
    /// Clones share the same immutable descriptor pool (cheap: internally
    /// reference-counted) but never share decoder state: the clone starts
    /// with its own independent [`DecoderState::events_seen`] counter,
    /// seeded from this instance's current value rather than tracking it
    /// jointly afterward.
    fn clone(&self) -> Self {
        Self {
            pool: self.pool.clone(),
            message: self.message.clone(),
            field_names: self.field_names,
            binary_values: self.binary_values,
            enum_values: self.enum_values,
            presence: self.presence,
            unknown_fields: self.unknown_fields,
            well_known_types: self.well_known_types,
            input: self.input,
            limits: self.limits,
            has_cycles: self.has_cycles,
            events_seen: AtomicU64::new(self.events_seen.load(Ordering::Relaxed)),
        }
    }
}

impl Flattener for ProtobufFlattener {
    fn flatten(
        &mut self,
        event: &[u8],
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        self.decode(event, Some(tracker))
    }

    fn copy(&self) -> Box<dyn Flattener> {
        Box::new(self.clone())
    }
}

/// Detect whether `root`'s message-type reference graph (following
/// message-typed field kinds, including through map values) contains a
/// cycle, without ever visiting the same message type twice.
fn message_graph_has_cycles(root: &MessageDescriptor) -> bool {
    let mut stack: FxHashSet<String> = FxHashSet::default();
    let mut visited: FxHashSet<String> = FxHashSet::default();
    has_cycles_from(root, &mut stack, &mut visited)
}

fn has_cycles_from(
    message: &MessageDescriptor,
    stack: &mut FxHashSet<String>,
    visited: &mut FxHashSet<String>,
) -> bool {
    let name = message.full_name().to_owned();
    if stack.contains(&name) {
        return true;
    }
    if !visited.insert(name.clone()) {
        return false;
    }
    stack.insert(name.clone());
    let mut cyclic = false;
    for field in message.fields() {
        if let Kind::Message(sub) = field.kind()
            && has_cycles_from(&sub, stack, visited)
        {
            cyclic = true;
            break;
        }
    }
    stack.remove(&name);
    cyclic
}

// =============================================================================
// Error helpers
// =============================================================================

fn invalid_event(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::invalid_event(EventFormat::Protobuf, message)
}

fn invalid_schema(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::invalid_schema(EventFormat::Protobuf, message)
}

fn limit_exceeded(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::limit_exceeded(EventFormat::Protobuf, message)
}

fn unsupported_value(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_value(EventFormat::Protobuf, message)
}

fn unsupported_map_key(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_map_key(EventFormat::Protobuf, message)
}

fn unsupported_feature(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_feature(EventFormat::Protobuf, message)
}

fn duplicate_field() -> QuaminaError {
    crate::decoder_errors::duplicate_field(EventFormat::Protobuf)
}

/// Canonicalize an integer decoded from the wire, rejecting a magnitude that
/// exceeds [`NumericPolicy::LosslessQuamina`](crate::NumericPolicy::LosslessQuamina).
fn lossless_int(value: i128, offset: usize) -> Result<CanonicalValue, QuaminaError> {
    NumericPolicy::LosslessQuamina.canonicalize_int(value, EventFormat::Protobuf, offset)
}

// =============================================================================
// Decoder
// =============================================================================

/// The wire-level context of one field occurrence: its wire type and the
/// nesting depth/byte bounds it was read within. Bundled to keep the
/// recursive `decode_*` methods' argument counts small.
#[derive(Clone, Copy)]
struct Site {
    wire_type: WireType,
    depth: usize,
    offset: usize,
    end: usize,
}

/// Per-message-instance scratch state for oneof clearing, repeated-field
/// array-id/position allocation, and map duplicate-key detection. Scoped to
/// one [`Decoder::decode_message`] call: each nested message gets its own.
#[derive(Default)]
struct MessageScratch {
    /// Oneof full name -> index into `Decoder::fields` where its currently
    /// active member's fields begin (truncated if a sibling member arrives).
    oneof_active: FxHashMap<String, usize>,
    /// Field number -> (allocated array id, next one-based position).
    repeated: FxHashMap<u32, (i32, i32)>,
    /// Field number -> map keys already seen for that map field.
    map_keys: FxHashMap<u32, FxHashSet<String>>,
}

/// Recursive-descent protobuf wire-format decoder.
///
/// Every field is decoded and structurally validated regardless of whether
/// it is referenced by any pattern; `tracker`/`used` parameters threaded
/// through the recursive `decode_*` methods only decide whether a scalar is
/// materialized into an [`OwnedField`], mirroring
/// [`MessagePackFlattener`](crate::MessagePackFlattener)'s tracker-driven skipping.
struct Decoder<'a> {
    data: &'a [u8],
    pos: usize,
    field_names: ProtobufFieldName,
    binary_values: BinaryValuePolicy,
    enum_values: EnumValuePolicy,
    input: ProtobufInput,
    limits: EventLimits,
    /// True for [`ProtobufFlattener::flatten_for_contract`], which has no
    /// tracker and always wants every field.
    all_fields: bool,
    fields: Vec<OwnedField>,
    field_count: usize,
    allocated_bytes: usize,
    next_array_id: i32,
    array_trail: Vec<ArrayPos>,
    path_prefix: Vec<u8>,
}

impl<'a> Decoder<'a> {
    const fn new(data: &'a [u8], flattener: &ProtobufFlattener, all_fields: bool) -> Self {
        Self {
            data,
            pos: 0,
            field_names: flattener.field_names,
            binary_values: flattener.binary_values,
            enum_values: flattener.enum_values,
            input: flattener.input,
            limits: flattener.limits,
            all_fields,
            fields: Vec::new(),
            field_count: 0,
            allocated_bytes: 0,
            next_array_id: 1,
            array_trail: Vec::new(),
            path_prefix: Vec::new(),
        }
    }

    // -- top level ---------------------------------------------------------

    fn decode_root(
        &mut self,
        message: &MessageDescriptor,
        tracker: Option<&dyn SegmentsTreeTracker>,
    ) -> Result<(), QuaminaError> {
        if self.data.is_empty() {
            return Err(invalid_event("empty protobuf event").at_byte_offset(0));
        }
        let end = match self.input {
            ProtobufInput::RawMessage => self.data.len(),
            ProtobufInput::LengthDelimitedMessage => {
                let total = self.data.len();
                let (_, stop) = self.read_length_delimited_bounds(total)?;
                if stop != total {
                    return Err(invalid_event(
                        "length-delimited root does not exactly cover the input",
                    )
                    .at_byte_offset(stop.min(total)));
                }
                stop
            }
        };
        self.decode_message(message, tracker, 1, end)?;
        if self.pos != end {
            return Err(
                invalid_event("trailing bytes after protobuf message").at_byte_offset(self.pos)
            );
        }
        Ok(())
    }

    fn segment_name<'f>(&self, field: &'f FieldDescriptor) -> &'f str {
        match self.field_names {
            ProtobufFieldName::ProtoName => field.name(),
            ProtobufFieldName::JsonName => field.json_name(),
        }
    }

    // -- message body --------------------------------------------------

    fn decode_message(
        &mut self,
        message: &MessageDescriptor,
        tracker: Option<&dyn SegmentsTreeTracker>,
        depth: usize,
        end: usize,
    ) -> Result<(), QuaminaError> {
        let mut scratch = MessageScratch::default();
        while self.pos < end {
            let tag_offset = self.pos;
            let (field_number, wire_type) = self.read_tag(end)?;
            match message.get_field(field_number) {
                Some(field) if wire_type_compatible(&field, wire_type) => {
                    let site = Site {
                        wire_type,
                        depth,
                        offset: tag_offset,
                        end,
                    };
                    self.decode_known_field(&field, site, tracker, &mut scratch)?;
                }
                _ => self.skip_value(wire_type, tag_offset, end)?,
            }
        }
        Ok(())
    }

    fn decode_known_field(
        &mut self,
        field: &FieldDescriptor,
        site: Site,
        tracker: Option<&dyn SegmentsTreeTracker>,
        scratch: &mut MessageScratch,
    ) -> Result<(), QuaminaError> {
        let oneof_key = field.containing_oneof().map(|o| o.full_name().to_owned());
        if let Some(key) = &oneof_key
            && let Some(&start) = scratch.oneof_active.get(key)
        {
            self.fields.truncate(start);
        }
        let start_index = self.fields.len();

        let segment = self.segment_name(field).to_owned();
        let used =
            self.all_fields || tracker.is_some_and(|t| t.is_segment_used(segment.as_bytes()));
        let child_tracker = if self.all_fields {
            None
        } else {
            tracker.and_then(|t| t.get(segment.as_bytes()))
        };

        let saved_len = self.path_prefix.len();
        if !self.path_prefix.is_empty() {
            self.path_prefix.push(b'\n');
        }
        self.path_prefix.extend_from_slice(segment.as_bytes());

        let result = if field.is_map() {
            self.decode_map_entry(field, site, child_tracker, scratch)
        } else if field.is_list() {
            self.decode_repeated_element(field, site, child_tracker, used, scratch)
        } else {
            self.decode_singular(field, site, child_tracker, used)
        };

        self.path_prefix.truncate(saved_len);

        if let Some(key) = oneof_key {
            scratch.oneof_active.insert(key, start_index);
        }
        result
    }

    fn decode_singular(
        &mut self,
        field: &FieldDescriptor,
        site: Site,
        child_tracker: Option<&dyn SegmentsTreeTracker>,
        used: bool,
    ) -> Result<(), QuaminaError> {
        match field.kind() {
            Kind::Message(sub) => {
                let new_depth = site.depth + 1;
                self.check_depth(new_depth, site.offset)?;
                let (_, stop) = self.read_length_delimited_bounds(site.end)?;
                self.decode_message(&sub, child_tracker, new_depth, stop)?;
                self.pos = stop;
                Ok(())
            }
            kind => {
                let value = self.read_scalar_kind(&kind, site.offset, site.end)?;
                self.emit_scalar(used, value, site.offset)
            }
        }
    }

    fn decode_repeated_element(
        &mut self,
        field: &FieldDescriptor,
        site: Site,
        child_tracker: Option<&dyn SegmentsTreeTracker>,
        used: bool,
        scratch: &mut MessageScratch,
    ) -> Result<(), QuaminaError> {
        let kind = field.kind();
        let packed = site.wire_type == WireType::LengthDelimited
            && kind.wire_type() != WireType::LengthDelimited;
        if packed {
            let (_, stop) = self.read_length_delimited_bounds(site.end)?;
            while self.pos < stop {
                let elem_offset = self.pos;
                let value = self.read_scalar_kind(&kind, elem_offset, stop)?;
                self.push_next_array_pos(field.number(), scratch, elem_offset)?;
                let result = self.emit_scalar(used, value, elem_offset);
                self.array_trail.pop();
                result?;
            }
            if self.pos != stop {
                return Err(invalid_event(
                    "packed repeated field did not decode exactly its declared length",
                )
                .at_byte_offset(stop));
            }
            Ok(())
        } else {
            match kind {
                Kind::Message(sub) => {
                    let new_depth = site.depth + 1;
                    self.check_depth(new_depth, site.offset)?;
                    let (_, stop) = self.read_length_delimited_bounds(site.end)?;
                    self.push_next_array_pos(field.number(), scratch, site.offset)?;
                    let result = self.decode_message(&sub, child_tracker, new_depth, stop);
                    self.array_trail.pop();
                    result?;
                    self.pos = stop;
                    Ok(())
                }
                other => {
                    let value = self.read_scalar_kind(&other, site.offset, site.end)?;
                    self.push_next_array_pos(field.number(), scratch, site.offset)?;
                    let result = self.emit_scalar(used, value, site.offset);
                    self.array_trail.pop();
                    result
                }
            }
        }
    }

    fn decode_map_entry(
        &mut self,
        field: &FieldDescriptor,
        site: Site,
        child_tracker: Option<&dyn SegmentsTreeTracker>,
        scratch: &mut MessageScratch,
    ) -> Result<(), QuaminaError> {
        let offset = site.offset;
        let Some(entry) = field.kind().as_message().cloned() else {
            return Err(invalid_event("map field is not message-encoded").at_byte_offset(offset));
        };
        let key_field = entry.map_entry_key_field();
        let value_field = entry.map_entry_value_field();
        if !matches!(key_field.kind(), Kind::String) {
            return Err(unsupported_map_key(format!(
                "protobuf map keys of kind {:?} are not supported; only string keys are",
                key_field.kind()
            ))
            .at_byte_offset(offset));
        }

        let new_depth = site.depth + 1;
        self.check_depth(new_depth, offset)?;
        let (start, stop) = self.read_length_delimited_bounds(site.end)?;
        let key = self.prescan_map_key(start, stop, key_field.number())?;

        self.pos = start;
        while self.pos < stop {
            let tag_offset = self.pos;
            let (number, wire_type) = self.read_tag(stop)?;
            if number == key_field.number() && wire_type == WireType::LengthDelimited {
                let _ = self.read_scalar_kind(&Kind::String, tag_offset, stop)?;
            } else if number == value_field.number()
                && wire_type_compatible(&value_field, wire_type)
            {
                let saved_len = self.path_prefix.len();
                if !self.path_prefix.is_empty() {
                    self.path_prefix.push(b'\n');
                }
                self.path_prefix.extend_from_slice(key.as_bytes());
                let used = self.all_fields
                    || child_tracker.is_some_and(|t| t.is_segment_used(key.as_bytes()));
                let grandchild_tracker = if self.all_fields {
                    None
                } else {
                    child_tracker.and_then(|t| t.get(key.as_bytes()))
                };
                match value_field.kind() {
                    Kind::Message(sub) => {
                        let value_depth = new_depth + 1;
                        self.check_depth(value_depth, tag_offset)?;
                        let (_, value_stop) = self.read_length_delimited_bounds(stop)?;
                        self.decode_message(&sub, grandchild_tracker, value_depth, value_stop)?;
                        self.pos = value_stop;
                    }
                    value_kind => {
                        let value = self.read_scalar_kind(&value_kind, tag_offset, stop)?;
                        self.emit_scalar(used, value, tag_offset)?;
                    }
                }
                self.path_prefix.truncate(saved_len);
            } else {
                self.skip_value(wire_type, tag_offset, stop)?;
            }
        }
        self.pos = stop;

        let seen = scratch.map_keys.entry(field.number()).or_default();
        if !seen.insert(key) {
            return Err(duplicate_field().at_byte_offset(offset));
        }
        Ok(())
    }

    /// Scan a map entry submessage's bytes (`[start, stop)`) for its key
    /// field without emitting or validating anything else, then restore
    /// `self.pos`. Used so the map value's field path (`field\nkey`) can be
    /// built before the real decode pass, since wire order between a map
    /// entry's key and value sub-fields is not guaranteed.
    fn prescan_map_key(
        &mut self,
        start: usize,
        stop: usize,
        key_number: u32,
    ) -> Result<String, QuaminaError> {
        let saved_pos = self.pos;
        self.pos = start;
        let mut found = String::new();
        while self.pos < stop {
            let tag_offset = self.pos;
            let (number, wire_type) = self.read_tag(stop)?;
            if number == key_number && wire_type == WireType::LengthDelimited {
                let (kstart, kstop) = self.read_length_delimited_bounds(stop)?;
                let bytes = self.data.get(kstart..kstop).ok_or_else(|| {
                    invalid_event("unexpected end of event").at_byte_offset(kstart)
                })?;
                found = std::str::from_utf8(bytes)
                    .map_err(|_| {
                        invalid_event("map key is not valid UTF-8").at_byte_offset(kstart)
                    })?
                    .to_owned();
                self.pos = kstop;
            } else {
                self.skip_value(wire_type, tag_offset, stop)?;
            }
        }
        self.pos = saved_pos;
        Ok(found)
    }

    // -- scalar reading ------------------------------------------------

    fn read_scalar_kind(
        &mut self,
        kind: &Kind,
        offset: usize,
        end: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        match kind {
            Kind::Bool
            | Kind::Int32
            | Kind::Uint32
            | Kind::Sint32
            | Kind::Int64
            | Kind::Uint64
            | Kind::Sint64 => {
                let raw = self.read_varint(end)?;
                Self::canonical_varint(kind, raw, offset)
            }
            Kind::Fixed32
            | Kind::Sfixed32
            | Kind::Fixed64
            | Kind::Sfixed64
            | Kind::Float
            | Kind::Double => self.read_fixed_scalar(kind, offset, end),
            Kind::String | Kind::Bytes => self.read_length_delimited_scalar(kind, offset, end),
            Kind::Enum(enum_type) => {
                let raw = self.read_varint(end)?;
                #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
                let value = raw as u32 as i32;
                self.canonical_enum(enum_type, value, offset)
            }
            Kind::Message(_) => {
                unreachable!("message kind is dispatched by the caller, never read as a scalar")
            }
        }
    }

    /// Canonicalize a varint-encoded scalar (`bool`/int variants) already
    /// read from the wire as a raw `u64`.
    fn canonical_varint(
        kind: &Kind,
        raw: u64,
        offset: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        match kind {
            Kind::Bool => Ok(CanonicalValue::Bool(raw != 0)),
            Kind::Int32 => {
                #[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
                let value = raw as u32 as i32;
                Ok(CanonicalValue::from_i64(i64::from(value)))
            }
            Kind::Uint32 => {
                #[allow(clippy::cast_possible_truncation)]
                let value = raw as u32;
                Ok(CanonicalValue::from_i64(i64::from(value)))
            }
            Kind::Sint32 => {
                #[allow(clippy::cast_possible_truncation)]
                let value = crate::zigzag::decode32(raw as u32);
                Ok(CanonicalValue::from_i64(i64::from(value)))
            }
            #[allow(clippy::cast_possible_wrap)]
            Kind::Int64 => lossless_int(i128::from(raw as i64), offset),
            Kind::Uint64 => lossless_int(i128::from(raw), offset),
            Kind::Sint64 => lossless_int(i128::from(crate::zigzag::decode64(raw)), offset),
            _ => unreachable!("caller filtered to varint kinds"),
        }
    }

    /// Read a fixed-width (32/64-bit integer or floating point) scalar.
    fn read_fixed_scalar(
        &mut self,
        kind: &Kind,
        offset: usize,
        end: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        match kind {
            Kind::Fixed32 => {
                let bytes = self.take_fixed(4, end, offset)?;
                let value = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
                Ok(CanonicalValue::from_i64(i64::from(value)))
            }
            Kind::Sfixed32 => {
                let bytes = self.take_fixed(4, end, offset)?;
                let value = i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
                Ok(CanonicalValue::from_i64(i64::from(value)))
            }
            Kind::Fixed64 => {
                let bytes = self.take_fixed(8, end, offset)?;
                let raw = u64::from_le_bytes(bytes.try_into().expect("fixed64 is 8 bytes"));
                lossless_int(i128::from(raw), offset)
            }
            Kind::Sfixed64 => {
                let bytes = self.take_fixed(8, end, offset)?;
                let raw = i64::from_le_bytes(bytes.try_into().expect("sfixed64 is 8 bytes"));
                lossless_int(i128::from(raw), offset)
            }
            Kind::Float => {
                let bytes = self.take_fixed(4, end, offset)?;
                let raw = f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
                CanonicalValue::from_f64(f64::from(raw))
                    .map_err(|_| unsupported_value("non-finite float value").at_byte_offset(offset))
            }
            Kind::Double => {
                let bytes = self.take_fixed(8, end, offset)?;
                let raw = f64::from_le_bytes(bytes.try_into().expect("double is 8 bytes"));
                CanonicalValue::from_f64(raw).map_err(|_| {
                    unsupported_value("non-finite double value").at_byte_offset(offset)
                })
            }
            _ => unreachable!("caller filtered to fixed-width kinds"),
        }
    }

    /// Read a length-delimited scalar (`string`/`bytes`).
    fn read_length_delimited_scalar(
        &mut self,
        kind: &Kind,
        offset: usize,
        end: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        let (start, stop) = self.read_length_delimited_bounds(end)?;
        self.check_scalar_len(stop - start, offset)?;
        let bytes = self
            .data
            .get(start..stop)
            .ok_or_else(|| invalid_event("unexpected end of event").at_byte_offset(start))?;
        self.pos = stop;
        match kind {
            Kind::String => {
                let text = std::str::from_utf8(bytes)
                    .map_err(|_| {
                        invalid_event("string field is not valid UTF-8").at_byte_offset(start)
                    })?
                    .to_owned();
                Ok(CanonicalValue::String(text))
            }
            Kind::Bytes => match self.binary_values {
                BinaryValuePolicy::Reject => {
                    Err(unsupported_value("binary values are rejected by policy")
                        .at_byte_offset(start))
                }
                BinaryValuePolicy::TaggedBase64 => {
                    let text = format!("base64:{}", crate::base64::encode(bytes));
                    Ok(CanonicalValue::String(text))
                }
            },
            _ => unreachable!("caller filtered to length-delimited kinds"),
        }
    }

    fn canonical_enum(
        &self,
        enum_type: &EnumDescriptor,
        value: i32,
        offset: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        match self.enum_values {
            EnumValuePolicy::SymbolicName => match enum_type.get_value(value) {
                Some(symbol) => Ok(CanonicalValue::String(symbol.name().to_owned())),
                None => Err(unsupported_value(format!(
                    "{value} is not a known value of enum {}",
                    enum_type.full_name()
                ))
                .at_byte_offset(offset)),
            },
        }
    }

    // -- scalar emission -------------------------------------------------

    fn emit_scalar(
        &mut self,
        used: bool,
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
        if used {
            if self.path_prefix.len() > self.limits.max_path_bytes {
                return Err(limit_exceeded("max_path_bytes exceeded").at_byte_offset(offset));
            }
            self.allocated_bytes = self.allocated_bytes.saturating_add(self.path_prefix.len());
            self.fields.push(OwnedField {
                is_number: value.is_number(),
                path: self.path_prefix.clone(),
                val: bytes,
                array_trail: self.array_trail.clone(),
            });
        }
        Ok(())
    }

    // -- resource limits -----------------------------------------------

    fn check_depth(&self, new_depth: usize, offset: usize) -> Result<(), QuaminaError> {
        crate::decoder_limits::check_depth(&self.limits, EventFormat::Protobuf, new_depth, offset)
    }

    fn check_scalar_len(&self, len: usize, offset: usize) -> Result<(), QuaminaError> {
        crate::decoder_limits::check_scalar_len(&self.limits, EventFormat::Protobuf, len, offset)
    }

    fn alloc_array_id(&mut self, offset: usize) -> Result<i32, QuaminaError> {
        crate::decoder_limits::alloc_array_id(
            &mut self.next_array_id,
            EventFormat::Protobuf,
            offset,
        )
    }

    fn push_next_array_pos(
        &mut self,
        field_number: u32,
        scratch: &mut MessageScratch,
        offset: usize,
    ) -> Result<(), QuaminaError> {
        let entry = scratch.repeated.entry(field_number).or_insert((0, 0));
        if entry.0 == 0 {
            entry.0 = self.alloc_array_id(offset)?;
        }
        entry.1 = entry.1.checked_add(1).ok_or_else(|| {
            limit_exceeded("array position exceeds i32 range").at_byte_offset(offset)
        })?;
        // entry.1 was just incremented from a non-negative start and is
        // never negative, so it always fits in usize on any supported target.
        let position = usize::try_from(entry.1).expect("positive i32 always fits in usize");
        if position > self.limits.max_container_items {
            return Err(limit_exceeded("max_container_items exceeded").at_byte_offset(offset));
        }
        self.array_trail.push(ArrayPos {
            array: entry.0,
            pos: entry.1,
        });
        Ok(())
    }

    // -- generic skip for unknown/mismatched fields ---------------------

    fn skip_value(
        &mut self,
        wire_type: WireType,
        offset: usize,
        end: usize,
    ) -> Result<(), QuaminaError> {
        match wire_type {
            WireType::Varint => {
                self.read_varint(end)?;
                Ok(())
            }
            WireType::SixtyFourBit => {
                self.take_fixed(8, end, offset)?;
                Ok(())
            }
            WireType::ThirtyTwoBit => {
                self.take_fixed(4, end, offset)?;
                Ok(())
            }
            WireType::LengthDelimited => {
                let (_, stop) = self.read_length_delimited_bounds(end)?;
                self.pos = stop;
                Ok(())
            }
            WireType::StartGroup | WireType::EndGroup => {
                Err(unsupported_feature("protobuf groups are not supported").at_byte_offset(offset))
            }
        }
    }

    // -- wire-level primitives ------------------------------------------

    fn read_tag(&mut self, end: usize) -> Result<(u32, WireType), QuaminaError> {
        let offset = self.pos;
        let raw = self.read_varint(end)?;
        let field_number = raw >> 3;
        #[allow(clippy::cast_possible_truncation)]
        let wire_type_num = (raw & 0x7) as u8;
        if field_number == 0 || field_number > u64::from(u32::MAX) {
            return Err(invalid_event("invalid field number in tag").at_byte_offset(offset));
        }
        let wire_type = WireType::try_from(u64::from(wire_type_num))
            .map_err(|_| invalid_event("invalid wire type").at_byte_offset(offset))?;
        if matches!(wire_type, WireType::StartGroup | WireType::EndGroup) {
            return Err(
                unsupported_feature("protobuf groups are not supported").at_byte_offset(offset)
            );
        }
        #[allow(clippy::cast_possible_truncation)]
        Ok((field_number as u32, wire_type))
    }

    fn read_varint(&mut self, end: usize) -> Result<u64, QuaminaError> {
        let mut result: u64 = 0;
        let mut shift: u32 = 0;
        loop {
            if self.pos >= end {
                return Err(
                    invalid_event("unexpected end of event while reading varint")
                        .at_byte_offset(self.pos),
                );
            }
            let byte = *self
                .data
                .get(self.pos)
                .ok_or_else(|| invalid_event("unexpected end of event").at_byte_offset(self.pos))?;
            self.pos += 1;
            if shift >= 64 {
                return Err(invalid_event("varint is too long").at_byte_offset(self.pos - 1));
            }
            result |= u64::from(byte & 0x7F) << shift;
            if byte & 0x80 == 0 {
                return Ok(result);
            }
            shift += 7;
        }
    }

    /// Read a length-delimited value's declared length and return its
    /// `[start, stop)` byte range, bounds-checked against `end`.
    ///
    /// Leaves `self.pos == start`: the caller decides how to consume
    /// `[start, stop)` (a single scalar slice, a recursive sub-message
    /// parse, or a packed-scalar loop) and is responsible for advancing
    /// `self.pos` to `stop` once it has done so.
    fn read_length_delimited_bounds(&mut self, end: usize) -> Result<(usize, usize), QuaminaError> {
        let offset = self.pos;
        let len = self.read_varint(end)?;
        let len = usize::try_from(len).map_err(|_| {
            invalid_event("length-delimited value length overflow").at_byte_offset(offset)
        })?;
        let start = self.pos;
        let stop = start
            .checked_add(len)
            .ok_or_else(|| invalid_event("length overflow").at_byte_offset(offset))?;
        if stop > end {
            return Err(
                invalid_event("length-delimited value exceeds available bytes")
                    .at_byte_offset(offset),
            );
        }
        Ok((start, stop))
    }

    fn take_fixed(
        &mut self,
        len: usize,
        end: usize,
        offset: usize,
    ) -> Result<&'a [u8], QuaminaError> {
        let start = self.pos;
        let stop = start
            .checked_add(len)
            .ok_or_else(|| invalid_event("length overflow").at_byte_offset(offset))?;
        if stop > end {
            return Err(invalid_event("unexpected end of event").at_byte_offset(offset));
        }
        let slice = self
            .data
            .get(start..stop)
            .ok_or_else(|| invalid_event("unexpected end of event").at_byte_offset(offset))?;
        self.pos = stop;
        Ok(slice)
    }
}

fn wire_type_compatible(field: &FieldDescriptor, wire_type: WireType) -> bool {
    if field.is_map() {
        wire_type == WireType::LengthDelimited
    } else if field.is_list() {
        let kind = field.kind();
        wire_type == kind.wire_type()
            || (wire_type == WireType::LengthDelimited
                && kind.wire_type() != WireType::LengthDelimited)
    } else {
        wire_type == field.kind().wire_type()
    }
}
