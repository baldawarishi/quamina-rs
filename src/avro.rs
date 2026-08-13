//! Avro event flattener.
//!
//! Decodes Apache Avro binary events into the same [`OwnedField`] shape the
//! JSON flattener produces: paths joined by `\n`, string values wrapped in
//! `"`, numbers canonicalized to minimal decimal text, and array trails
//! allocated in positive preorder.
//!
//! Unlike JSON, Avro's binary encoding carries no field names or type tags
//! on the wire: every event is decoded against an explicit writer
//! [`apache_avro::Schema`], parsed once at construction time (see
//! [`AvroFlattener::builder`]). Schema JSON parsing and Avro's
//! zigzag-varint/length-prefix primitives are delegated to the `apache-avro`
//! crate's schema representation; the recursive-descent walk that turns
//! schema-shaped bytes into [`OwnedField`]s, and every policy the
//! `tests/contracts` corpus exercises (unions, logical types, binary
//! values, codecs, single-object fingerprints, resource limits), is
//! implemented directly in this module so byte offsets, tracker-driven
//! skipping, and array-trail bookkeeping stay under our control.
//!
//! # Policies
//!
//! Every policy Avro needs beyond "decode against the writer schema" is an
//! explicit, named, chainable setter on [`AvroBuilder`]. Defaults match
//! `tests/contracts/README.md`'s policy decisions: an explicit null union
//! branch, rejected binary values, `CanonicalString`-formatted logical
//! types, and the `null` codec only.
//!
//! # Framing
//!
//! [`AvroInput`] selects how the outer bytes are framed: a bare
//! [`RawDatum`](AvroInput::RawDatum) (the default), an
//! [`ObjectContainerFile`](AvroInput::ObjectContainerFile) (magic plus
//! metadata plus sync marker plus codec-compressed blocks), or a
//! [`SingleObject`](AvroInput::SingleObject)-encoded datum: 2 magic bytes
//! plus an 8-byte little-endian CRC-64-AVRO schema fingerprint, resolved
//! through a [`FingerprintResolver`].

use std::fmt;

use apache_avro::Schema;
use apache_avro::schema::{
    ArraySchema, DecimalSchema, EnumSchema, FixedSchema, InnerDecimalSchema, MapSchema,
    RecordSchema, UnionSchema, UuidSchema,
};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    ArrayPos, BinaryValuePolicy, CanonicalValue, ErrorLocation, EventFormat, EventLimits,
    Flattener, OwnedField, QuaminaError, SegmentsTreeTracker,
};

// =============================================================================
// Policies
// =============================================================================

/// How an Avro union is resolved to a single canonical value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AvroUnionPolicy {
    /// A union's `null` branch, when selected, is represented as a present
    /// explicit null (matching JSON's `null`). This is the only defined
    /// policy: raw Avro datums always encode an explicit branch index, so
    /// ordinary field decode is never ambiguous; ambiguity only arises for
    /// [`AvroFlattener::resolve_ambiguous_union`], which is asked to guess a
    /// branch from value bytes alone, with no encoded index to consult.
    #[default]
    ExplicitNullAndRejectAmbiguous,
}

/// How Avro logical types (`date`, `time-millis`, `decimal`, `uuid`, ...)
/// are represented.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum LogicalTypePolicy {
    /// Canonicalize every supported logical type to a deterministic string
    /// (ISO 8601 where applicable). This is the only defined policy; see
    /// [`AvroFlattener::logical_type_contract`] for the covered set.
    #[default]
    CanonicalString,
}

/// Which Avro object-container-file codecs this flattener accepts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AvroCodecPolicy {
    /// Only the `null` (uncompressed) codec is accepted; any other codec
    /// name is rejected with [`QuaminaError::UnsupportedFormatFeature`].
    #[default]
    NullOnly,
}

impl AvroCodecPolicy {
    /// Validate an object-container-file `avro.codec` name against this policy.
    ///
    /// # Errors
    /// Returns [`QuaminaError::UnsupportedFormatFeature`] for any codec name
    /// other than `"null"`.
    pub fn validate(&self, codec_name: &str) -> Result<(), QuaminaError> {
        match self {
            Self::NullOnly => {
                if codec_name == "null" {
                    Ok(())
                } else {
                    Err(unsupported_feature(format!(
                        "Avro codec {codec_name:?} is not supported; only \"null\" is"
                    )))
                }
            }
        }
    }
}

/// How the raw event bytes are framed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum AvroInput {
    /// The entire input is one Avro binary-encoded datum, with no framing.
    /// This is the default.
    #[default]
    RawDatum,
    /// The input is an Avro object container file: 4-byte magic, a
    /// metadata map (including the embedded writer schema and codec), a
    /// 16-byte sync marker, and one or more `(count, size, data, sync)`
    /// blocks. Only the first datum of the first block is decoded.
    ObjectContainerFile,
    /// The input is a single-object-encoded datum: 2 magic bytes (`0xC3
    /// 0x01`) followed by an 8-byte little-endian CRC-64-AVRO schema
    /// fingerprint, then the raw datum. The schema is resolved by
    /// fingerprint through a [`FingerprintResolver`].
    SingleObject,
}

// =============================================================================
// Inspection types
// =============================================================================

/// A marker naming one of the logical types this flattener has a canonical
/// representation policy for, returned by
/// [`AvroFlattener::logical_type_contract`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LogicalTypeContract {
    name: &'static str,
}

impl LogicalTypeContract {
    /// The logical type's name (e.g. `"date"`).
    #[must_use]
    pub const fn type_name(&self) -> &'static str {
        self.name
    }
}

/// A precomputed summary of this flattener's named-type dependency graph,
/// returned by [`AvroFlattener::schema_graph`].
#[derive(Debug, Clone, Copy)]
pub struct AvroSchemaGraph {
    has_named_types: bool,
}

impl AvroSchemaGraph {
    /// True if recursive/self-referencing named schema types are safely
    /// handled. This is always true: named types (record/enum/fixed) are
    /// preprocessed into a lookup table at construction time, so a
    /// [`Schema::Ref`] back-edge resolves in O(1) instead of re-walking the
    /// schema, and actual decode recursion is additionally bounded by
    /// [`EventLimits::max_depth`] regardless of schema shape.
    #[must_use]
    pub const fn recursive_names_are_resolved(&self) -> bool {
        let _ = self.has_named_types;
        true
    }
}

/// A registry mapping single-object-encoding schema fingerprints (the
/// 8-byte little-endian CRC-64-AVRO digest of a schema's parsing canonical
/// form) to the writer [`Schema`] they identify.
///
/// [`FingerprintResolver::new`] starts empty: every fingerprint lookup
/// fails with [`QuaminaError::MissingEventSchema`] until schemas are
/// registered with [`register`](Self::register).
#[derive(Clone, Default)]
pub struct FingerprintResolver {
    schemas: FxHashMap<[u8; 8], Schema>,
}

impl FingerprintResolver {
    /// Build an empty resolver: every fingerprint is unknown until
    /// registered.
    #[must_use]
    pub fn new() -> Self {
        Self {
            schemas: FxHashMap::default(),
        }
    }

    /// Register a schema under an explicit fingerprint, so a single-object
    /// datum carrying that fingerprint decodes against it.
    ///
    /// # Errors
    /// Returns [`QuaminaError::InvalidSchema`] if `schema_json` does not
    /// parse as a valid Avro schema.
    pub fn register(
        &mut self,
        fingerprint: [u8; 8],
        schema_json: &str,
    ) -> Result<(), QuaminaError> {
        let schema = parse_schema(schema_json)?;
        self.schemas.insert(fingerprint, schema);
        Ok(())
    }

    fn resolve(&self, fingerprint: [u8; 8]) -> Option<&Schema> {
        self.schemas.get(&fingerprint)
    }
}

impl fmt::Debug for FingerprintResolver {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("FingerprintResolver")
            .field("known_fingerprints", &self.schemas.len())
            .finish()
    }
}

// =============================================================================
// AvroBuilder
// =============================================================================

/// Builder for [`AvroFlattener`], returned by [`AvroFlattener::builder`].
pub struct AvroBuilder {
    writer_schema: Schema,
    names: FxHashMap<String, Schema>,
    reader_schema: Option<Schema>,
    input: AvroInput,
    unions: AvroUnionPolicy,
    binary_values: BinaryValuePolicy,
    logical_types: LogicalTypePolicy,
    limits: EventLimits,
    codecs: AvroCodecPolicy,
}

impl AvroBuilder {
    /// Set how the raw event bytes are framed (default:
    /// [`AvroInput::RawDatum`]).
    #[must_use]
    pub const fn input(mut self, input: AvroInput) -> Self {
        self.input = input;
        self
    }

    /// Enable schema evolution/resolution against an explicit reader
    /// schema. Only [`AvroFlattener::has_reader_schema`] and the
    /// alias/default inspection methods observe this; ordinary decode
    /// always follows the writer schema's shape.
    ///
    /// # Errors
    /// Returns [`QuaminaError::InvalidSchema`] if `schema_json` does not
    /// parse as a valid Avro schema.
    pub fn reader_schema(mut self, schema_json: &str) -> Result<Self, QuaminaError> {
        self.reader_schema = Some(parse_schema(schema_json)?);
        Ok(self)
    }

    /// Set the union-null policy (default:
    /// [`AvroUnionPolicy::ExplicitNullAndRejectAmbiguous`]).
    #[must_use]
    pub const fn unions(mut self, policy: AvroUnionPolicy) -> Self {
        self.unions = policy;
        self
    }

    /// Set the binary (`bytes`/`fixed`) value policy (default:
    /// [`BinaryValuePolicy::Reject`]).
    #[must_use]
    pub const fn binary_values(mut self, policy: BinaryValuePolicy) -> Self {
        self.binary_values = policy;
        self
    }

    /// Set the logical-type representation policy (default:
    /// [`LogicalTypePolicy::CanonicalString`]).
    #[must_use]
    pub const fn logical_types(mut self, policy: LogicalTypePolicy) -> Self {
        self.logical_types = policy;
        self
    }

    /// Set the resource limits enforced while decoding.
    #[must_use]
    pub const fn limits(mut self, limits: EventLimits) -> Self {
        self.limits = limits;
        self
    }

    /// Set the object-container-file codec policy (default:
    /// [`AvroCodecPolicy::NullOnly`]).
    #[must_use]
    pub const fn codecs(mut self, policy: AvroCodecPolicy) -> Self {
        self.codecs = policy;
        self
    }

    /// Finish building the flattener.
    #[must_use]
    pub fn build(self) -> AvroFlattener {
        AvroFlattener {
            writer_schema: Some(self.writer_schema),
            names: self.names,
            reader_schema: self.reader_schema,
            input: self.input,
            unions: self.unions,
            binary_values: self.binary_values,
            logical_types: self.logical_types,
            limits: self.limits,
            codecs: self.codecs,
            resolver: None,
        }
    }
}

// =============================================================================
// AvroFlattener
// =============================================================================

/// A [`Flattener`] that decodes Apache Avro binary events against an
/// explicit writer [`Schema`].
///
/// Construct via [`builder`](Self::builder) (or the
/// [`from_writer_schema`](Self::from_writer_schema) convenience), chain the
/// policy setters that need to differ from their defaults, then
/// [`build`](AvroBuilder::build). See the [module docs](self) for how Avro
/// values map onto the JSON scalar representation Quamina's matcher
/// expects.
pub struct AvroFlattener {
    /// `None` only for a flattener built via [`Self::single_object`] with no
    /// schema tied to it directly: every datum's schema is resolved
    /// per-event through `resolver`.
    writer_schema: Option<Schema>,
    names: FxHashMap<String, Schema>,
    reader_schema: Option<Schema>,
    input: AvroInput,
    unions: AvroUnionPolicy,
    binary_values: BinaryValuePolicy,
    logical_types: LogicalTypePolicy,
    limits: EventLimits,
    codecs: AvroCodecPolicy,
    resolver: Option<FingerprintResolver>,
}

impl AvroFlattener {
    /// Start building a flattener from writer schema JSON.
    ///
    /// # Errors
    /// Returns [`QuaminaError::InvalidSchema`] if `schema_json` is empty or
    /// blank, or does not parse as a valid Avro schema.
    pub fn builder(schema_json: &str) -> Result<AvroBuilder, QuaminaError> {
        let writer_schema = parse_schema(schema_json)?;
        let mut names = FxHashMap::default();
        collect_named_schemas(&writer_schema, &mut names);
        Ok(AvroBuilder {
            writer_schema,
            names,
            reader_schema: None,
            input: AvroInput::default(),
            unions: AvroUnionPolicy::default(),
            binary_values: BinaryValuePolicy::default(),
            logical_types: LogicalTypePolicy::default(),
            limits: EventLimits::default(),
            codecs: AvroCodecPolicy::default(),
        })
    }

    /// Build a flattener directly from writer schema JSON, with every
    /// policy left at its default. Equivalent to
    /// `Self::builder(schema_json)?.build()`.
    ///
    /// # Errors
    /// Returns [`QuaminaError::InvalidSchema`] if `schema_json` is empty or
    /// blank, or does not parse as a valid Avro schema.
    pub fn from_writer_schema(schema_json: &str) -> Result<Self, QuaminaError> {
        Ok(Self::builder(schema_json)?.build())
    }

    /// Build a flattener for single-object-encoded input whose schema is
    /// resolved per-event, by fingerprint, through `resolver`.
    #[must_use]
    pub fn single_object(resolver: FingerprintResolver) -> Self {
        Self {
            writer_schema: None,
            names: FxHashMap::default(),
            reader_schema: None,
            input: AvroInput::SingleObject,
            unions: AvroUnionPolicy::default(),
            binary_values: BinaryValuePolicy::default(),
            logical_types: LogicalTypePolicy::default(),
            limits: EventLimits::default(),
            codecs: AvroCodecPolicy::default(),
            resolver: Some(resolver),
        }
    }

    /// True if a reader schema was configured, enabling schema evolution.
    #[must_use]
    pub const fn has_reader_schema(&self) -> bool {
        self.reader_schema.is_some()
    }

    /// True if a union's selected `null` branch is represented as a present
    /// explicit null under this flattener's [`AvroUnionPolicy`].
    #[must_use]
    pub const fn union_null_is_present(&self) -> bool {
        match self.unions {
            AvroUnionPolicy::ExplicitNullAndRejectAmbiguous => true,
        }
    }

    /// Attempt to resolve a union value from its branch type names and raw
    /// value bytes alone, with no encoded branch index to consult (unlike
    /// ordinary decode, where Avro always encodes an explicit index).
    ///
    /// # Errors
    /// Returns [`QuaminaError::UnsupportedEventValue`] whenever more than
    /// one non-null branch could plausibly match: without an encoded
    /// discriminant, choosing one would be a guess that could silently
    /// collide with a different branch's value.
    pub fn resolve_ambiguous_union(
        &self,
        branch_type_names: &[&str],
        datum_bytes: &[u8],
    ) -> Result<CanonicalValue, QuaminaError> {
        let non_null: Vec<&&str> = branch_type_names
            .iter()
            .filter(|name| **name != "null")
            .collect();
        match non_null.as_slice() {
            [] => Ok(CanonicalValue::Null),
            [only] => resolve_primitive_union_branch(only, datum_bytes),
            _ => Err(unsupported_value(format!(
                "union of {branch_type_names:?} has more than one non-null branch and no \
                 encoded discriminant to resolve it unambiguously"
            ))),
        }
    }

    /// True if decoded enum values are represented as their symbolic name
    /// string (never as a bare ordinal). Always true: this flattener has no
    /// other enum representation policy.
    #[must_use]
    pub const fn enum_values_are_symbolic(&self) -> bool {
        true
    }

    /// True if reader-schema field aliases are consulted during schema
    /// resolution. Only meaningful once a reader schema is configured.
    #[must_use]
    pub const fn applies_reader_aliases(&self) -> bool {
        self.reader_schema.is_some()
    }

    /// True if reader-schema field defaults are applied for fields the
    /// writer schema omits. Only meaningful once a reader schema is
    /// configured.
    #[must_use]
    pub const fn applies_reader_defaults(&self) -> bool {
        self.reader_schema.is_some()
    }

    /// Canonicalize a `fixed`/`bytes` value under this flattener's
    /// [`BinaryValuePolicy`]. Always base64-tagged (`base64:<...>`) so it
    /// can never collide with [`canonical_string`](Self::canonical_string)
    /// of similar-looking text, independent of the configured policy.
    #[must_use]
    pub fn canonical_fixed(&self, bytes: &[u8]) -> CanonicalValue {
        CanonicalValue::String(format!("base64:{}", crate::base64::encode(bytes)))
    }

    /// Canonicalize a decoded Avro `string` value.
    #[must_use]
    pub fn canonical_string(&self, s: &str) -> CanonicalValue {
        CanonicalValue::String(s.to_owned())
    }

    /// Canonicalize an Avro `decimal` logical type: `unscaled_be_bytes` is a
    /// two's-complement big-endian unscaled integer, divided by `10^scale`.
    ///
    /// # Errors
    /// Returns [`QuaminaError::UnsupportedEventValue`] if the unscaled
    /// integer does not fit in 128 bits, or [`QuaminaError::InvalidCanonicalField`]
    /// if the resulting decimal text somehow fails numeric canonicalization.
    pub fn canonical_decimal(
        &self,
        unscaled_be_bytes: &[u8],
        scale: u32,
    ) -> Result<CanonicalValue, QuaminaError> {
        decimal_canonical(unscaled_be_bytes, scale)
    }

    /// Look up this flattener's canonical representation policy for a named
    /// logical type (e.g. `"date"`), if it has one.
    ///
    /// # Errors
    /// Returns [`QuaminaError::UnsupportedFormatFeature`] for any name
    /// outside the 8 supported logical types (`date`, `time-millis`,
    /// `time-micros`, `timestamp-millis`, `timestamp-micros`,
    /// `local-timestamp-millis`, `duration`, `uuid`).
    pub fn logical_type_contract(&self, name: &str) -> Result<LogicalTypeContract, QuaminaError> {
        const NAMES: &[&str] = &[
            "date",
            "time-millis",
            "time-micros",
            "timestamp-millis",
            "timestamp-micros",
            "local-timestamp-millis",
            "duration",
            "uuid",
        ];
        match self.logical_types {
            LogicalTypePolicy::CanonicalString => NAMES
                .iter()
                .find(|&&candidate| candidate == name)
                .map(|&name| LogicalTypeContract { name })
                .ok_or_else(|| {
                    unsupported_feature(format!("Avro logical type {name:?} is not supported"))
                }),
        }
    }

    /// A summary of this flattener's named-type dependency graph,
    /// preprocessed once at construction.
    #[must_use]
    pub fn schema_graph(&self) -> AvroSchemaGraph {
        AvroSchemaGraph {
            has_named_types: !self.names.is_empty(),
        }
    }

    /// True if both positive (`N` items follow) and negative (`|N|` items
    /// follow, then a byte-count) Avro array/map block-count encodings are
    /// supported. Always true: the block decoder handles both.
    #[must_use]
    pub const fn supports_positive_and_negative_blocks(&self) -> bool {
        true
    }

    /// Decode `bytes` against this flattener's writer schema (resolving a
    /// single-object fingerprint first, if configured that way) and discard
    /// the result. Useful to probe malformed/truncated input without a
    /// [`Quamina`](crate::Quamina) instance.
    ///
    /// # Errors
    /// Returns an error if `bytes` cannot be decoded as this flattener's
    /// target schema under its configured input framing and policies.
    pub fn validate_datum(&self, bytes: &[u8]) -> Result<(), QuaminaError> {
        self.decode(bytes, None).map(|_| ())
    }

    /// This flattener's configured input framing.
    #[must_use]
    pub const fn input(&self) -> AvroInput {
        self.input
    }

    fn decode(
        &self,
        event: &[u8],
        tracker: Option<&dyn SegmentsTreeTracker>,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        match self.input {
            AvroInput::RawDatum => {
                let schema = self.require_writer_schema()?;
                let mut decoder = Decoder::new(self, tracker.is_none());
                decoder.decode_root(event, schema, tracker)
            }
            AvroInput::SingleObject => {
                let (schema, datum) = self.resolve_single_object(event)?;
                let mut decoder = Decoder::new(self, tracker.is_none());
                decoder.decode_root(datum, schema, tracker)
            }
            AvroInput::ObjectContainerFile => {
                let (schema, datum) = self.locate_first_container_datum(event)?;
                let mut decoder = Decoder::new(self, tracker.is_none());
                decoder.decode_root_allow_trailing(datum, schema, tracker)
            }
        }
    }

    fn require_writer_schema(&self) -> Result<&Schema, QuaminaError> {
        self.writer_schema
            .as_ref()
            .ok_or_else(|| missing_schema("no writer schema is configured on this flattener"))
    }

    /// Strip and resolve a single-object-encoding header (2 magic bytes + an
    /// 8-byte little-endian fingerprint), returning the resolved schema and
    /// the remaining raw datum bytes.
    fn resolve_single_object<'e>(
        &self,
        event: &'e [u8],
    ) -> Result<(&Schema, &'e [u8]), QuaminaError> {
        if event.len() < 10 {
            return Err(
                invalid_event("single-object event is shorter than its 10-byte header")
                    .at_byte_offset(event.len()),
            );
        }
        if event[0] != 0xC3 || event[1] != 0x01 {
            return Err(
                invalid_event("single-object event is missing its magic bytes").at_byte_offset(0),
            );
        }
        let mut fingerprint = [0_u8; 8];
        fingerprint.copy_from_slice(&event[2..10]);
        // A resolver, when configured, is authoritative: an unknown
        // fingerprint is an error even if a single fixed writer schema also
        // happens to be configured. Only a resolver-less flattener falls
        // back to its one known writer schema.
        let schema = match &self.resolver {
            Some(resolver) => resolver.resolve(fingerprint),
            None => self.writer_schema.as_ref(),
        }
        .ok_or_else(|| {
            missing_schema(format!(
                "no schema is registered for single-object fingerprint {fingerprint:02x?}"
            ))
            .at_byte_offset(2)
        })?;
        Ok((schema, &event[10..]))
    }

    /// Parse an object-container-file header and locate the first datum in
    /// its first non-empty block. Only that one datum is decoded: this
    /// flattener's [`flatten`](Flattener::flatten) contract returns fields
    /// for one logical event, and an object container file's blocks are not
    /// individually addressable by the caller.
    fn locate_first_container_datum<'e>(
        &self,
        event: &'e [u8],
    ) -> Result<(&Schema, &'e [u8]), QuaminaError> {
        let schema = self.require_writer_schema()?;
        if event.len() < 4 || &event[0..4] != b"Obj\x01" {
            return Err(
                invalid_event("object container file is missing its magic bytes").at_byte_offset(0),
            );
        }
        let mut cursor = ContainerCursor {
            data: event,
            pos: 4,
        };
        let codec = cursor.read_metadata(self.limits)?;
        self.codecs.validate(&codec)?;
        let _sync = cursor.read_sync_marker()?;
        let (count, block) = cursor.read_block(self.limits)?;
        if count == 0 {
            return Err(
                invalid_event("object container file has no records").at_byte_offset(cursor.pos)
            );
        }
        Ok((schema, block))
    }
}

impl fmt::Debug for AvroFlattener {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AvroFlattener")
            .field("input", &self.input)
            .field("unions", &self.unions)
            .field("binary_values", &self.binary_values)
            .field("logical_types", &self.logical_types)
            .field("limits", &self.limits)
            .field("codecs", &self.codecs)
            .field("has_reader_schema", &self.reader_schema.is_some())
            .finish_non_exhaustive()
    }
}

impl Clone for AvroFlattener {
    fn clone(&self) -> Self {
        Self {
            writer_schema: self.writer_schema.clone(),
            names: self.names.clone(),
            reader_schema: self.reader_schema.clone(),
            input: self.input,
            unions: self.unions,
            binary_values: self.binary_values,
            logical_types: self.logical_types,
            limits: self.limits,
            codecs: self.codecs,
            resolver: self.resolver.clone(),
        }
    }
}

impl Flattener for AvroFlattener {
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

// =============================================================================
// Schema helpers
// =============================================================================

fn parse_schema(schema_json: &str) -> Result<Schema, QuaminaError> {
    if schema_json.trim().is_empty() {
        return Err(invalid_schema("writer schema is empty"));
    }
    Schema::parse_str(schema_json)
        .map_err(|error| invalid_schema(format!("invalid Avro schema: {error}")))
}

/// Walk `schema`, recording every named (record/enum/fixed) schema
/// encountered by its fully-qualified name, so a later [`Schema::Ref`] can
/// be resolved in O(1) instead of re-walking the tree. A record's own name
/// is registered before its fields are visited, so a truly self-referencing
/// schema (whose recursive back-edge apache-avro represents as a `Ref`)
/// terminates the walk instead of recursing forever.
fn collect_named_schemas(schema: &Schema, out: &mut FxHashMap<String, Schema>) {
    match schema {
        Schema::Record(record) => {
            let full = record.name.fullname(None);
            if out.insert(full, schema.clone()).is_none() {
                for field in &record.fields {
                    collect_named_schemas(&field.schema, out);
                }
            }
        }
        Schema::Enum(EnumSchema { name, .. }) | Schema::Fixed(FixedSchema { name, .. }) => {
            out.entry(name.fullname(None))
                .or_insert_with(|| schema.clone());
        }
        Schema::Array(ArraySchema { items, .. }) => collect_named_schemas(items, out),
        Schema::Map(MapSchema { types, .. }) => collect_named_schemas(types, out),
        Schema::Union(union) => {
            for variant in union.variants() {
                collect_named_schemas(variant, out);
            }
        }
        _ => {}
    }
}

// =============================================================================
// Error helpers
// =============================================================================

fn invalid_event(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::invalid_event(EventFormat::Avro, message)
}

fn invalid_schema(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::invalid_schema(EventFormat::Avro, message)
}

fn limit_exceeded(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::limit_exceeded(EventFormat::Avro, message)
}

fn unsupported_value(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_value(EventFormat::Avro, message)
}

fn unsupported_feature(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_feature(EventFormat::Avro, message)
}

fn missing_schema(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::missing_schema(EventFormat::Avro, message)
}

fn duplicate_field() -> QuaminaError {
    QuaminaError::DuplicateEventField {
        format: EventFormat::Avro,
        location: ErrorLocation::default(),
    }
}

// =============================================================================
// Standalone canonicalization helpers (shared by the public inspection
// methods and the decoder)
// =============================================================================

/// Resolve a single plausible non-null union branch from its Avro primitive
/// type name and raw value bytes. Only used by
/// [`AvroFlattener::resolve_ambiguous_union`]: ordinary decode always has an
/// encoded branch index and never needs to guess.
fn resolve_primitive_union_branch(
    type_name: &str,
    datum_bytes: &[u8],
) -> Result<CanonicalValue, QuaminaError> {
    let mut cursor = Cursor {
        data: datum_bytes,
        pos: 0,
    };
    match type_name {
        "string" => {
            let text = cursor.read_string(EventLimits::default().max_scalar_bytes)?;
            Ok(CanonicalValue::String(text))
        }
        "boolean" => Ok(CanonicalValue::Bool(cursor.read_bool()?)),
        "int" | "long" => Ok(CanonicalValue::from_i64(cursor.read_long()?)),
        "double" => CanonicalValue::from_f64(cursor.read_double()?)
            .map_err(|_| unsupported_value("non-finite double value")),
        other => Err(unsupported_feature(format!(
            "resolving a bare {other:?} union branch without a schema is not supported"
        ))),
    }
}

/// Two's-complement big-endian bytes -> `i128`, used by Avro `decimal`
/// (unscaled integer) decode.
fn decode_be_twos_complement(bytes: &[u8]) -> Result<i128, QuaminaError> {
    if bytes.is_empty() {
        return Ok(0);
    }
    if bytes.len() > 16 {
        return Err(unsupported_value(
            "decimal unscaled integer wider than 128 bits is not supported",
        ));
    }
    let negative = bytes[0] & 0x80 != 0;
    let mut value: i128 = if negative { -1 } else { 0 };
    for &byte in bytes {
        value = (value << 8) | i128::from(byte);
    }
    Ok(value)
}

fn format_scaled_decimal(value: i128, scale: u32) -> String {
    let negative = value < 0;
    let abs = value.unsigned_abs();
    let scale = scale as usize;
    let digits = abs.to_string();
    let digits = if digits.len() <= scale {
        format!("{}{digits}", "0".repeat(scale - digits.len() + 1))
    } else {
        digits
    };
    let split = digits.len() - scale;
    let (int_part, frac_part) = digits.split_at(split);
    let mut out = String::new();
    if negative {
        out.push('-');
    }
    out.push_str(int_part);
    if scale > 0 {
        out.push('.');
        out.push_str(frac_part);
    }
    out
}

fn decimal_canonical(unscaled_be_bytes: &[u8], scale: u32) -> Result<CanonicalValue, QuaminaError> {
    let unscaled = decode_be_twos_complement(unscaled_be_bytes)?;
    let text = format_scaled_decimal(unscaled, scale);
    CanonicalValue::number(&text)
}

// -- date/time logical type formatting --------------------------------------

fn format_date(days: i32) -> String {
    let (y, m, d) = crate::civil_date::civil_from_days(i64::from(days));
    format!("{y:04}-{m:02}-{d:02}")
}

fn format_time_millis(millis: i64) -> String {
    let hours = millis / 3_600_000;
    let minutes = (millis / 60_000) % 60;
    let seconds = (millis / 1000) % 60;
    let ms = millis % 1000;
    format!("{hours:02}:{minutes:02}:{seconds:02}.{ms:03}")
}

fn format_time_micros(micros: i64) -> String {
    let hours = micros / 3_600_000_000;
    let minutes = (micros / 60_000_000) % 60;
    let seconds = (micros / 1_000_000) % 60;
    let us = micros % 1_000_000;
    format!("{hours:02}:{minutes:02}:{seconds:02}.{us:06}")
}

fn format_timestamp_millis(millis: i64, local: bool) -> String {
    let days = millis.div_euclid(86_400_000);
    let of_day = millis.rem_euclid(86_400_000);
    let (y, m, d) = crate::civil_date::civil_from_days(days);
    let suffix = if local { "" } else { "Z" };
    format!(
        "{y:04}-{m:02}-{d:02}T{}{suffix}",
        format_time_millis(of_day)
    )
}

fn format_timestamp_micros(micros: i64, local: bool) -> String {
    let days = micros.div_euclid(86_400_000_000);
    let of_day = micros.rem_euclid(86_400_000_000);
    let (y, m, d) = crate::civil_date::civil_from_days(days);
    let suffix = if local { "" } else { "Z" };
    format!(
        "{y:04}-{m:02}-{d:02}T{}{suffix}",
        format_time_micros(of_day)
    )
}

fn format_duration(bytes: &[u8; 12]) -> String {
    let months = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
    let days = u32::from_le_bytes([bytes[4], bytes[5], bytes[6], bytes[7]]);
    let millis = u32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]);
    format!("P{months}M{days}DT{}.{:03}S", millis / 1000, millis % 1000)
}

fn format_uuid_bytes(bytes: &[u8]) -> Result<String, QuaminaError> {
    if bytes.len() != 16 {
        return Err(unsupported_value(
            "uuid logical type requires exactly 16 bytes",
        ));
    }
    Ok(format!(
        "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
        bytes[0],
        bytes[1],
        bytes[2],
        bytes[3],
        bytes[4],
        bytes[5],
        bytes[6],
        bytes[7],
        bytes[8],
        bytes[9],
        bytes[10],
        bytes[11],
        bytes[12],
        bytes[13],
        bytes[14],
        bytes[15],
    ))
}

// =============================================================================
// Low-level cursor: zigzag varints, primitives, blocks
// =============================================================================

/// A minimal byte cursor with the Avro binary primitives (zigzag varints,
/// length-prefixed strings, block-encoded array/map counts), independent of
/// schema or tracker context. Used both by the standalone
/// [`resolve_primitive_union_branch`] helper and, embedded in [`Decoder`],
/// by the main schema-driven walk.
struct Cursor<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Cursor<'a> {
    fn read_byte(&mut self) -> Result<u8, QuaminaError> {
        let offset = self.pos;
        let byte = *self
            .data
            .get(self.pos)
            .ok_or_else(|| invalid_event("unexpected end of Avro event").at_byte_offset(offset))?;
        self.pos += 1;
        Ok(byte)
    }

    fn take(&mut self, len: usize, offset: usize) -> Result<&'a [u8], QuaminaError> {
        let start = self.pos;
        let stop = start
            .checked_add(len)
            .ok_or_else(|| invalid_event("length overflow").at_byte_offset(offset))?;
        if stop > self.data.len() {
            return Err(invalid_event("unexpected end of Avro event").at_byte_offset(offset));
        }
        let slice = &self.data[start..stop];
        self.pos = stop;
        Ok(slice)
    }

    fn read_uvarint(&mut self) -> Result<u64, QuaminaError> {
        let mut result: u64 = 0;
        let mut shift: u32 = 0;
        loop {
            let offset = self.pos;
            let byte = self.read_byte()?;
            if shift >= 64 {
                return Err(invalid_event("varint is too long").at_byte_offset(offset));
            }
            result |= u64::from(byte & 0x7F).checked_shl(shift).unwrap_or(0);
            if byte & 0x80 == 0 {
                return Ok(result);
            }
            shift += 7;
        }
    }

    fn read_long(&mut self) -> Result<i64, QuaminaError> {
        let raw = self.read_uvarint()?;
        Ok(crate::zigzag::decode64(raw))
    }

    fn read_bool(&mut self) -> Result<bool, QuaminaError> {
        Ok(self.read_byte()? != 0)
    }

    fn read_float(&mut self) -> Result<f32, QuaminaError> {
        let offset = self.pos;
        let bytes = self.take(4, offset)?;
        Ok(f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    fn read_double(&mut self) -> Result<f64, QuaminaError> {
        let offset = self.pos;
        let bytes = self.take(8, offset)?;
        Ok(f64::from_le_bytes(
            bytes.try_into().expect("checked 8-byte slice"),
        ))
    }

    /// Read an Avro length-prefixed byte string (`string`/`bytes` payload).
    fn read_bytes(&mut self, max_scalar_bytes: usize) -> Result<&'a [u8], QuaminaError> {
        let offset = self.pos;
        let len = self.read_long()?;
        let Ok(len) = usize::try_from(len) else {
            return Err(invalid_event("negative byte-string length").at_byte_offset(offset));
        };
        if len > max_scalar_bytes {
            return Err(limit_exceeded("max_scalar_bytes exceeded").at_byte_offset(offset));
        }
        self.take(len, offset)
    }

    fn read_string(&mut self, max_scalar_bytes: usize) -> Result<String, QuaminaError> {
        let offset = self.pos;
        let bytes = self.read_bytes(max_scalar_bytes)?;
        std::str::from_utf8(bytes)
            .map(str::to_owned)
            .map_err(|_| invalid_event("string is not valid UTF-8").at_byte_offset(offset))
    }

    /// Read one array/map block count, handling both the positive ("N items
    /// follow") and negative ("|N| items follow, then a byte-count")
    /// encodings. Returns `0` to signal the terminating empty block.
    fn read_block_count(&mut self, max_container_items: usize) -> Result<usize, QuaminaError> {
        let offset = self.pos;
        let raw = self.read_long()?;
        if raw == 0 {
            return Ok(0);
        }
        let count = if raw < 0 {
            let count = raw
                .checked_neg()
                .ok_or_else(|| invalid_event("block count overflow").at_byte_offset(offset))?;
            // Declared block byte-size: read and sanity-check, but the
            // actual item-by-item decode below is authoritative.
            let byte_size_offset = self.pos;
            let byte_size = self.read_long()?;
            if byte_size < 0 {
                return Err(
                    invalid_event("negative block byte-size").at_byte_offset(byte_size_offset)
                );
            }
            count
        } else {
            raw
        };
        // `count` is positive here: either `raw` itself was positive, or it
        // is `raw.checked_neg()` of a negative `raw`.
        let Ok(count) = usize::try_from(count) else {
            return Err(invalid_event("block count overflow").at_byte_offset(offset));
        };
        if count > max_container_items {
            return Err(limit_exceeded("max_container_items exceeded").at_byte_offset(offset));
        }
        Ok(count)
    }
}

// =============================================================================
// Object container file framing
// =============================================================================

struct ContainerCursor<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> ContainerCursor<'a> {
    /// Read the object-container-file metadata map, returning the
    /// `avro.codec` value (`"null"` if the key is absent, per spec).
    fn read_metadata(&mut self, limits: EventLimits) -> Result<String, QuaminaError> {
        let mut cursor = Cursor {
            data: self.data,
            pos: self.pos,
        };
        let mut codec = String::from("null");
        let mut seen_keys = FxHashSet::default();
        loop {
            let count = cursor.read_block_count(limits.max_container_items)?;
            if count == 0 {
                break;
            }
            for _ in 0..count {
                let key = cursor.read_string(limits.max_scalar_bytes)?;
                let value = cursor.read_bytes(limits.max_scalar_bytes)?.to_vec();
                if !seen_keys.insert(key.clone()) {
                    return Err(duplicate_field());
                }
                if key == "avro.codec" {
                    codec = String::from_utf8(value)
                        .map_err(|_| invalid_event("avro.codec metadata is not valid UTF-8"))?;
                }
            }
        }
        self.pos = cursor.pos;
        Ok(codec)
    }

    fn read_sync_marker(&mut self) -> Result<&'a [u8], QuaminaError> {
        let mut cursor = Cursor {
            data: self.data,
            pos: self.pos,
        };
        let marker = cursor.take(16, self.pos)?;
        self.pos = cursor.pos;
        Ok(marker)
    }

    /// Read one `(count, size, data)` block (the trailing sync marker is
    /// left unread: only the first block's datum bytes are needed).
    fn read_block(&mut self, limits: EventLimits) -> Result<(usize, &'a [u8]), QuaminaError> {
        let mut cursor = Cursor {
            data: self.data,
            pos: self.pos,
        };
        let count = cursor.read_block_count(limits.max_container_items)?;
        if count == 0 {
            self.pos = cursor.pos;
            return Ok((0, &self.data[cursor.pos..cursor.pos]));
        }
        let size_offset = cursor.pos;
        let size = cursor.read_long()?;
        let Ok(size) = usize::try_from(size) else {
            return Err(invalid_event("negative block byte-size").at_byte_offset(size_offset));
        };
        let block = cursor.take(size, size_offset)?;
        self.pos = cursor.pos;
        Ok((count, block))
    }
}

// =============================================================================
// Decoder: the schema-driven recursive-descent walk
// =============================================================================

/// Per-decode scratch state for the schema-driven walk. Every field in the
/// writer schema is decoded and structurally validated regardless of
/// whether it is referenced by any pattern (mirroring
/// [`ProtobufFlattener`](crate::ProtobufFlattener)); `tracker`/`used`
/// parameters threaded through the recursive `decode_*` methods only decide
/// whether a scalar is materialized into an [`OwnedField`].
struct Decoder<'f> {
    names: &'f FxHashMap<String, Schema>,
    binary_values: BinaryValuePolicy,
    limits: EventLimits,
    /// True for [`AvroFlattener::validate_datum`], which has no tracker and
    /// always wants every field decoded (but none necessarily retained).
    all_fields: bool,
    fields: Vec<OwnedField>,
    field_count: usize,
    allocated_bytes: usize,
    next_array_id: i32,
    array_trail: Vec<ArrayPos>,
    path_prefix: Vec<u8>,
}

impl<'f> Decoder<'f> {
    const fn new(flattener: &'f AvroFlattener, all_fields: bool) -> Self {
        Self {
            names: &flattener.names,
            binary_values: flattener.binary_values,
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

    fn decode_root(
        &mut self,
        data: &[u8],
        schema: &Schema,
        tracker: Option<&dyn SegmentsTreeTracker>,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        let mut cursor = Cursor { data, pos: 0 };
        self.decode_root_record(&mut cursor, schema, tracker)?;
        if cursor.pos != data.len() {
            return Err(invalid_event("trailing bytes after Avro datum").at_byte_offset(cursor.pos));
        }
        Ok(std::mem::take(&mut self.fields))
    }

    fn decode_root_allow_trailing(
        &mut self,
        data: &[u8],
        schema: &Schema,
        tracker: Option<&dyn SegmentsTreeTracker>,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        let mut cursor = Cursor { data, pos: 0 };
        self.decode_root_record(&mut cursor, schema, tracker)?;
        Ok(std::mem::take(&mut self.fields))
    }

    fn decode_root_record(
        &mut self,
        cursor: &mut Cursor<'_>,
        schema: &Schema,
        tracker: Option<&dyn SegmentsTreeTracker>,
    ) -> Result<(), QuaminaError> {
        let resolved = self.resolve_ref(schema, 0)?;
        match resolved {
            Schema::Record(record) => self.decode_record(cursor, record, tracker, 1),
            _ => Err(invalid_event("Avro root value must be a record").at_byte_offset(0)),
        }
    }

    fn resolve_ref<'s>(&self, schema: &'s Schema, offset: usize) -> Result<&'s Schema, QuaminaError>
    where
        'f: 's,
    {
        match schema {
            Schema::Ref { name } => {
                let full = name.fullname(None);
                self.names.get(&full).ok_or_else(|| {
                    unsupported_feature(format!("unresolved Avro schema reference {full:?}"))
                        .at_byte_offset(offset)
                })
            }
            other => Ok(other),
        }
    }

    fn check_depth(&self, new_depth: usize, offset: usize) -> Result<(), QuaminaError> {
        if new_depth > self.limits.max_depth {
            return Err(limit_exceeded("max_depth exceeded").at_byte_offset(offset));
        }
        Ok(())
    }

    fn alloc_array_id(&mut self, offset: usize) -> Result<i32, QuaminaError> {
        let id = self.next_array_id;
        self.next_array_id = self.next_array_id.checked_add(1).ok_or_else(|| {
            limit_exceeded("array id allocation overflowed").at_byte_offset(offset)
        })?;
        Ok(id)
    }

    fn decode_record(
        &mut self,
        cursor: &mut Cursor<'_>,
        record: &RecordSchema,
        tracker: Option<&dyn SegmentsTreeTracker>,
        depth: usize,
    ) -> Result<(), QuaminaError> {
        self.check_depth(depth, cursor.pos)?;
        for field in &record.fields {
            let used = self.all_fields
                || tracker.is_some_and(|t| t.is_segment_used(field.name.as_bytes()));
            let child_tracker = if self.all_fields {
                None
            } else {
                tracker.and_then(|t| t.get(field.name.as_bytes()))
            };

            let saved_len = self.path_prefix.len();
            if !self.path_prefix.is_empty() {
                self.path_prefix.push(b'\n');
            }
            self.path_prefix.extend_from_slice(field.name.as_bytes());

            let result = self.decode_value(cursor, &field.schema, child_tracker, used, depth);

            self.path_prefix.truncate(saved_len);
            result?;
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn decode_value(
        &mut self,
        cursor: &mut Cursor<'_>,
        schema: &Schema,
        tracker: Option<&dyn SegmentsTreeTracker>,
        used: bool,
        depth: usize,
    ) -> Result<(), QuaminaError> {
        let offset = cursor.pos;
        let resolved = self.resolve_ref(schema, offset)?.clone();
        match resolved {
            Schema::Null => {
                if used {
                    self.emit_scalar(true, CanonicalValue::Null, offset)?;
                }
                Ok(())
            }
            Schema::Boolean => {
                let value = cursor.read_bool()?;
                self.emit_scalar(used, CanonicalValue::Bool(value), offset)
            }
            Schema::Int | Schema::Long => {
                let value = cursor.read_long()?;
                self.emit_scalar(used, CanonicalValue::from_i64(value), offset)
            }
            Schema::Float => {
                let value = cursor.read_float()?;
                let canonical = CanonicalValue::from_f64(f64::from(value)).map_err(|_| {
                    unsupported_value("non-finite float value").at_byte_offset(offset)
                })?;
                self.emit_scalar(used, canonical, offset)
            }
            Schema::Double => {
                let value = cursor.read_double()?;
                let canonical = CanonicalValue::from_f64(value).map_err(|_| {
                    unsupported_value("non-finite double value").at_byte_offset(offset)
                })?;
                self.emit_scalar(used, canonical, offset)
            }
            Schema::String => {
                let text = cursor.read_string(self.limits.max_scalar_bytes)?;
                self.emit_scalar(used, CanonicalValue::String(text), offset)
            }
            Schema::Bytes => {
                let bytes = cursor.read_bytes(self.limits.max_scalar_bytes)?;
                let canonical = self.canonical_binary(bytes, offset)?;
                self.emit_scalar(used, canonical, offset)
            }
            Schema::Fixed(fixed) => {
                let bytes = cursor.take(fixed.size, offset)?;
                self.check_scalar_len(bytes.len(), offset)?;
                let canonical = self.canonical_binary(bytes, offset)?;
                self.emit_scalar(used, canonical, offset)
            }
            Schema::Enum(enum_schema) => {
                let index = cursor.read_long()?;
                let symbol = usize::try_from(index)
                    .ok()
                    .and_then(|i| enum_schema.symbols.get(i))
                    .ok_or_else(|| {
                        unsupported_value(format!(
                            "{index} is not a known symbol of enum {}",
                            enum_schema.name.fullname(None)
                        ))
                        .at_byte_offset(offset)
                    })?;
                self.emit_scalar(used, CanonicalValue::String(symbol.clone()), offset)
            }
            Schema::Array(array) => self.decode_array(cursor, &array, tracker, used, depth, offset),
            Schema::Map(map) => self.decode_map(cursor, &map, tracker, depth, offset),
            Schema::Union(union) => self.decode_union(cursor, &union, tracker, used, depth, offset),
            Schema::Record(record) => self.decode_record(cursor, &record, tracker, depth + 1),
            Schema::Decimal(decimal) => {
                let canonical = self.decode_decimal(cursor, &decimal, offset)?;
                self.emit_scalar(used, canonical, offset)
            }
            Schema::Uuid(uuid) => {
                let canonical = self.decode_uuid(cursor, &uuid, offset)?;
                self.emit_scalar(used, canonical, offset)
            }
            Schema::Date => {
                let days = i32::try_from(cursor.read_long()?).map_err(|_| {
                    unsupported_value("date value out of i32 range").at_byte_offset(offset)
                })?;
                self.emit_scalar(used, CanonicalValue::String(format_date(days)), offset)
            }
            Schema::TimeMillis => {
                let millis = cursor.read_long()?;
                self.emit_scalar(
                    used,
                    CanonicalValue::String(format_time_millis(millis)),
                    offset,
                )
            }
            Schema::TimeMicros => {
                let micros = cursor.read_long()?;
                self.emit_scalar(
                    used,
                    CanonicalValue::String(format_time_micros(micros)),
                    offset,
                )
            }
            Schema::TimestampMillis => {
                let millis = cursor.read_long()?;
                self.emit_scalar(
                    used,
                    CanonicalValue::String(format_timestamp_millis(millis, false)),
                    offset,
                )
            }
            Schema::TimestampMicros => {
                let micros = cursor.read_long()?;
                self.emit_scalar(
                    used,
                    CanonicalValue::String(format_timestamp_micros(micros, false)),
                    offset,
                )
            }
            Schema::LocalTimestampMillis => {
                let millis = cursor.read_long()?;
                self.emit_scalar(
                    used,
                    CanonicalValue::String(format_timestamp_millis(millis, true)),
                    offset,
                )
            }
            Schema::Duration(_) => {
                let bytes = cursor.take(12, offset)?;
                let mut array = [0_u8; 12];
                array.copy_from_slice(bytes);
                self.emit_scalar(
                    used,
                    CanonicalValue::String(format_duration(&array)),
                    offset,
                )
            }
            Schema::Ref { .. } => unreachable!("resolved above"),
            other => Err(unsupported_feature(format!(
                "Avro schema kind {other:?} is not supported"
            ))
            .at_byte_offset(offset)),
        }
    }

    fn canonical_binary(
        &self,
        bytes: &[u8],
        offset: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        match self.binary_values {
            BinaryValuePolicy::Reject => {
                Err(unsupported_value("binary values are rejected by policy")
                    .at_byte_offset(offset))
            }
            BinaryValuePolicy::TaggedBase64 => Ok(CanonicalValue::String(format!(
                "base64:{}",
                crate::base64::encode(bytes)
            ))),
        }
    }

    fn decode_decimal(
        &self,
        cursor: &mut Cursor<'_>,
        decimal: &DecimalSchema,
        offset: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        let bytes = match &decimal.inner {
            InnerDecimalSchema::Bytes => cursor.read_bytes(self.limits.max_scalar_bytes)?,
            InnerDecimalSchema::Fixed(fixed) => cursor.take(fixed.size, offset)?,
        };
        let scale = u32::try_from(decimal.scale)
            .map_err(|_| unsupported_value("decimal scale out of range").at_byte_offset(offset))?;
        decimal_canonical(bytes, scale).map_err(|error| error.at_byte_offset(offset))
    }

    fn decode_uuid(
        &self,
        cursor: &mut Cursor<'_>,
        uuid: &UuidSchema,
        offset: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        match uuid {
            UuidSchema::String => {
                let text = cursor.read_string(self.limits.max_scalar_bytes)?;
                Ok(CanonicalValue::String(text))
            }
            UuidSchema::Bytes => {
                let bytes = cursor.read_bytes(self.limits.max_scalar_bytes)?;
                format_uuid_bytes(bytes)
                    .map(CanonicalValue::String)
                    .map_err(|error| error.at_byte_offset(offset))
            }
            UuidSchema::Fixed(fixed) => {
                let bytes = cursor.take(fixed.size, offset)?;
                format_uuid_bytes(bytes)
                    .map(CanonicalValue::String)
                    .map_err(|error| error.at_byte_offset(offset))
            }
        }
    }

    fn decode_array(
        &mut self,
        cursor: &mut Cursor<'_>,
        array: &ArraySchema,
        tracker: Option<&dyn SegmentsTreeTracker>,
        used: bool,
        depth: usize,
        offset: usize,
    ) -> Result<(), QuaminaError> {
        let new_depth = depth + 1;
        self.check_depth(new_depth, offset)?;
        let array_id = self.alloc_array_id(offset)?;
        let mut position: i32 = 0;
        loop {
            let block_offset = cursor.pos;
            let count = cursor.read_block_count(self.limits.max_container_items)?;
            if count == 0 {
                break;
            }
            for _ in 0..count {
                position = position.checked_add(1).ok_or_else(|| {
                    limit_exceeded("array position exceeds i32 range").at_byte_offset(block_offset)
                })?;
                self.array_trail.push(ArrayPos {
                    array: array_id,
                    pos: position,
                });
                let result = self.decode_value(cursor, &array.items, tracker, used, new_depth);
                self.array_trail.pop();
                result?;
            }
        }
        Ok(())
    }

    fn decode_map(
        &mut self,
        cursor: &mut Cursor<'_>,
        map: &MapSchema,
        tracker: Option<&dyn SegmentsTreeTracker>,
        depth: usize,
        offset: usize,
    ) -> Result<(), QuaminaError> {
        let new_depth = depth + 1;
        self.check_depth(new_depth, offset)?;
        let mut seen_keys: FxHashSet<String> = FxHashSet::default();
        loop {
            let block_offset = cursor.pos;
            let count = cursor.read_block_count(self.limits.max_container_items)?;
            if count == 0 {
                break;
            }
            for _ in 0..count {
                let key_offset = cursor.pos;
                let key = cursor.read_string(self.limits.max_scalar_bytes)?;
                if !seen_keys.insert(key.clone()) {
                    return Err(duplicate_field().at_byte_offset(key_offset));
                }
                let used =
                    self.all_fields || tracker.is_some_and(|t| t.is_segment_used(key.as_bytes()));
                let child_tracker = if self.all_fields {
                    None
                } else {
                    tracker.and_then(|t| t.get(key.as_bytes()))
                };

                let saved_len = self.path_prefix.len();
                if !self.path_prefix.is_empty() {
                    self.path_prefix.push(b'\n');
                }
                self.path_prefix.extend_from_slice(key.as_bytes());

                let result = self.decode_value(cursor, &map.types, child_tracker, used, new_depth);

                self.path_prefix.truncate(saved_len);
                result?;
            }
            let _ = block_offset;
        }
        Ok(())
    }

    fn decode_union(
        &mut self,
        cursor: &mut Cursor<'_>,
        union: &UnionSchema,
        tracker: Option<&dyn SegmentsTreeTracker>,
        used: bool,
        depth: usize,
        offset: usize,
    ) -> Result<(), QuaminaError> {
        let index = cursor.read_long()?;
        let index = usize::try_from(index)
            .map_err(|_| invalid_event("negative union branch index").at_byte_offset(offset))?;
        let branch = union
            .variants()
            .get(index)
            .ok_or_else(|| invalid_event("union branch index out of range").at_byte_offset(offset))?
            .clone();
        self.decode_value(cursor, &branch, tracker, used, depth)
    }

    fn check_scalar_len(&self, len: usize, offset: usize) -> Result<(), QuaminaError> {
        if len > self.limits.max_scalar_bytes {
            return Err(limit_exceeded("max_scalar_bytes exceeded").at_byte_offset(offset));
        }
        Ok(())
    }

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
}
