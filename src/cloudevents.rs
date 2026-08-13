//! Binary-mode CloudEvents envelope flattener.
//!
//! Decodes a binary-content-mode CloudEvents [`Envelope`] (HTTP `ce-*`
//! headers, or Kafka `ce_*` headers) into the same [`OwnedField`] shape the
//! JSON flattener produces. Context and extension attributes (`id`, `type`,
//! `source`, `specversion`, `datacontenttype`, and any `ce-<ext>`/`ce_<ext>`
//! extension) are emitted at the event root; a decoded payload body is
//! nested under `data`, dispatched through an explicit [`FlattenerRegistry`]
//! keyed by the `Content-Type` media type. This decoder never guesses a
//! payload's shape from its bytes: a media type with no registered
//! [`Flattener`] is handled entirely by [`UnknownMediaTypePolicy`].
//!
//! # Transport-neutral binding
//!
//! HTTP `ce-specversion`/`ce-id`/`ce-type`/`ce-source`/`ce-<ext>` and Kafka
//! `ce_specversion`/`ce_id`/`ce_type`/`ce_source`/`ce_<ext>` normalize to the
//! same attribute names and values; `Content-Type` (no transport-specific
//! prefix on either transport) maps to `datacontenttype`.
//!
//! # Validation
//!
//! Every header is decoded and validated regardless of whether the current
//! [`SegmentsTreeTracker`] considers it relevant to any pattern, mirroring
//! every other format flattener in this crate: tracking only controls
//! whether a decoded value is materialized into an [`OwnedField`], or
//! whether the payload registry is even consulted. See
//! `tests/contracts/README.md`'s "Policy decisions" section for the full
//! rationale.

use crate::{
    CanonicalValue, Envelope, EnvelopeFlattener, EventFormat, EventLimits, FieldPath,
    FieldSetBuilder, FieldSetOutput, Flattener, OwnedField, PatternFieldTracker, QuaminaError,
    SegmentsTreeTracker, Transport,
};
use rustc_hash::FxHashMap;

/// The four CloudEvents context attributes every event must carry.
const REQUIRED_ATTRIBUTES: [&str; 4] = ["specversion", "id", "source", "type"];

// =============================================================================
// Policies
// =============================================================================

/// Which CloudEvents `specversion` values are accepted.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CloudEventsVersionPolicy {
    /// Only `specversion: "1.0"` is accepted; any other value (e.g. the
    /// legacy `"0.3"`) is rejected as an unsupported format feature.
    #[default]
    V1Only,
}

/// How a `Content-Type` whose media type has no [`Flattener`] registered in
/// the [`FlattenerRegistry`] is handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum UnknownMediaTypePolicy {
    /// Reject the envelope: an unrecognized media type is an error.
    #[default]
    Error,
    /// Match on context/extension attributes only. No `data` fields are
    /// ever invented for a media type this decoder cannot decode, and no
    /// error is raised.
    MetadataOnly,
}

/// How an absent body, an explicit empty body, and a Kafka tombstone (a
/// record with a `null` value) are represented relative to one another.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EmptyDataPolicy {
    /// Keep an absent body, an explicit empty body, and a Kafka tombstone
    /// distinguishable: `data` is only ever decoded for a present body, and
    /// `datacontenttype` is only ever emitted when a `Content-Type` header
    /// was actually sent.
    #[default]
    DistinguishAbsentEmptyAndTombstone,
}

/// Which CloudEvents content modes this flattener accepts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum CloudEventModePolicy {
    /// Only binary content mode is accepted. A `Content-Type` of
    /// `application/cloudevents+json` (structured mode) or
    /// `application/cloudevents-batch+json` (batch mode) is rejected as an
    /// unsupported format feature; this binary-mode flattener has no way to
    /// decode either.
    #[default]
    BinaryOnly,
}

// =============================================================================
// FlattenerRegistry
// =============================================================================

/// Maps a `Content-Type` media type to the payload [`Flattener`] that
/// decodes bodies of that type.
///
/// Lookups are case-insensitive and ignore any `;`-separated parameters
/// (`charset`, `profile`, ...). [`BinaryCloudEventFlattener`] never sniffs a
/// body's bytes to guess its shape: a media type absent from the registry
/// is handled entirely by [`UnknownMediaTypePolicy`].
pub struct FlattenerRegistry {
    flatteners: FxHashMap<String, Box<dyn Flattener>>,
}

impl FlattenerRegistry {
    /// Create an empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self {
            flatteners: FxHashMap::default(),
        }
    }

    /// Register `flattener` as the decoder for bodies whose `Content-Type`
    /// media type (case-insensitive, parameters ignored) is `media_type`.
    /// Registering the same media type again replaces the previous entry.
    ///
    /// # Errors
    /// Returns [`QuaminaError::InvalidEnvelope`] if `media_type` is empty
    /// after trimming surrounding whitespace.
    pub fn register(
        &mut self,
        media_type: &str,
        flattener: Box<dyn Flattener>,
    ) -> Result<(), QuaminaError> {
        let key = media_type.trim().to_ascii_lowercase();
        if key.is_empty() {
            return Err(invalid_envelope(
                "datacontenttype",
                "registered media type must not be empty",
            ));
        }
        self.flatteners.insert(key, flattener);
        Ok(())
    }

    /// The media types with a registered flattener, one entry each.
    #[must_use]
    pub fn registered_media_types(&self) -> Vec<String> {
        self.flatteners.keys().cloned().collect()
    }

    fn get_mut(&mut self, media_type: &str) -> Option<&mut Box<dyn Flattener>> {
        self.flatteners.get_mut(media_type)
    }
}

impl Default for FlattenerRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for FlattenerRegistry {
    fn clone(&self) -> Self {
        Self {
            flatteners: self
                .flatteners
                .iter()
                .map(|(media_type, flattener)| (media_type.clone(), flattener.copy()))
                .collect(),
        }
    }
}

// =============================================================================
// Builder
// =============================================================================

/// Builder for [`BinaryCloudEventFlattener`].
///
/// Construct via [`BinaryCloudEventFlattener::builder`], chain the policy
/// setters that need to differ from their defaults, and finish with
/// [`build`](Self::build).
pub struct BinaryCloudEventFlattenerBuilder {
    registry: FlattenerRegistry,
    versions: CloudEventsVersionPolicy,
    unknown_media_types: UnknownMediaTypePolicy,
    empty_data: EmptyDataPolicy,
    modes: CloudEventModePolicy,
}

impl BinaryCloudEventFlattenerBuilder {
    /// Set the accepted `specversion` policy.
    #[must_use]
    pub const fn versions(mut self, policy: CloudEventsVersionPolicy) -> Self {
        self.versions = policy;
        self
    }

    /// Set the unknown-media-type policy.
    #[must_use]
    pub const fn unknown_media_types(mut self, policy: UnknownMediaTypePolicy) -> Self {
        self.unknown_media_types = policy;
        self
    }

    /// Set the absent/empty/tombstone data policy.
    #[must_use]
    pub const fn empty_data(mut self, policy: EmptyDataPolicy) -> Self {
        self.empty_data = policy;
        self
    }

    /// Set the accepted content-mode policy.
    #[must_use]
    pub const fn modes(mut self, policy: CloudEventModePolicy) -> Self {
        self.modes = policy;
        self
    }

    /// Finish building the flattener.
    #[must_use]
    pub fn build(self) -> BinaryCloudEventFlattener {
        BinaryCloudEventFlattener {
            registry: self.registry,
            metadata_only: false,
            versions: self.versions,
            unknown_media_types: self.unknown_media_types,
            empty_data: self.empty_data,
            modes: self.modes,
        }
    }
}

// =============================================================================
// BinaryCloudEventFlattener
// =============================================================================

/// An [`EnvelopeFlattener`] that decodes a binary-content-mode CloudEvents envelope.
///
/// Context/extension attributes land at the event root, and (unless
/// [`metadata_only`](Self::metadata_only)) a payload body is dispatched
/// through a [`FlattenerRegistry`] and nested under `data`.
///
/// Use [`new`](Self::new) for default policies, [`builder`](Self::builder)
/// to select non-default policies, or [`metadata_only`](Self::metadata_only)
/// for a flattener that never touches the payload registry at all. See the
/// [module docs](self) for how attributes map onto the JSON scalar
/// representation Quamina's matcher expects.
pub struct BinaryCloudEventFlattener {
    registry: FlattenerRegistry,
    metadata_only: bool,
    versions: CloudEventsVersionPolicy,
    unknown_media_types: UnknownMediaTypePolicy,
    empty_data: EmptyDataPolicy,
    modes: CloudEventModePolicy,
}

impl BinaryCloudEventFlattener {
    /// Create a flattener that only ever decodes context/extension
    /// attributes. The payload registry is never consulted and a body is
    /// never decoded, regardless of `Content-Type`.
    #[must_use]
    pub fn metadata_only() -> Self {
        Self {
            registry: FlattenerRegistry::new(),
            metadata_only: true,
            versions: CloudEventsVersionPolicy::default(),
            unknown_media_types: UnknownMediaTypePolicy::default(),
            empty_data: EmptyDataPolicy::default(),
            modes: CloudEventModePolicy::default(),
        }
    }

    /// Create a flattener that dispatches payload bodies through `registry`,
    /// with every other policy at its default.
    #[must_use]
    pub fn new(registry: FlattenerRegistry) -> Self {
        Self::builder(registry).build()
    }

    /// Start a [`BinaryCloudEventFlattenerBuilder`] that dispatches payload
    /// bodies through `registry`, to select non-default policies.
    #[must_use]
    pub const fn builder(registry: FlattenerRegistry) -> BinaryCloudEventFlattenerBuilder {
        BinaryCloudEventFlattenerBuilder {
            registry,
            versions: CloudEventsVersionPolicy::V1Only,
            unknown_media_types: UnknownMediaTypePolicy::Error,
            empty_data: EmptyDataPolicy::DistinguishAbsentEmptyAndTombstone,
            modes: CloudEventModePolicy::BinaryOnly,
        }
    }

    /// Flatten `envelope`'s context and extension attributes only, without
    /// going through a [`Quamina`](crate::Quamina) instance, a pattern
    /// tracker, or (regardless of configuration) the payload registry.
    /// Every attribute is decoded and emitted, useful for comparing two
    /// flattener configurations' output.
    ///
    /// # Errors
    /// Returns an error if the envelope's headers cannot be interpreted
    /// under this flattener's configured policies (see the [module
    /// docs](self)).
    pub fn flatten_metadata(&self, envelope: &Envelope) -> Result<FieldSetOutput, QuaminaError> {
        let attributes = self.compute_attributes(envelope)?;
        let paths: Vec<FieldPath> = attributes
            .fields
            .iter()
            .map(|(name, _)| FieldPath::from_segments([name.as_str()]))
            .collect();
        let tracker = PatternFieldTracker::from_paths(paths);
        let mut builder = FieldSetBuilder::new(&tracker, EventLimits::default());
        for (name, value) in attributes.fields {
            builder.emit([name.as_str()], value)?;
        }
        builder.finish()
    }

    /// Validate `envelope`'s headers and compute its CloudEvents attributes,
    /// consulting every configured policy. Never touches the payload
    /// registry or body.
    fn compute_attributes(&self, envelope: &Envelope) -> Result<Attributes, QuaminaError> {
        let prefix = match envelope.transport() {
            Transport::Http => "ce-",
            Transport::Kafka => "ce_",
        };

        let mut grouped: FxHashMap<String, Vec<String>> = FxHashMap::default();
        for (name, value) in envelope.headers().iter() {
            let lowered = normalize_attribute_name(name)?;
            let key = if lowered == "content-type" {
                "content-type".to_owned()
            } else if let Some(rest) = lowered.strip_prefix(prefix) {
                rest.to_owned()
            } else {
                continue;
            };
            let text = std::str::from_utf8(value)
                .map_err(|_| {
                    invalid_envelope("value", "CloudEvents attribute value is not valid UTF-8")
                })?
                .to_owned();
            grouped.entry(key).or_default().push(text);
        }

        let mut attributes: FxHashMap<String, String> = FxHashMap::default();
        for (key, values) in grouped {
            if values.len() > 1 {
                return Err(conflicting_headers(format!(
                    "attribute \"{key}\" was set by more than one differently-cased header"
                )));
            }
            attributes.insert(key, values.into_iter().next().unwrap_or_default());
        }

        // Content mode is checked before required-attribute presence: a
        // structured or batch envelope may carry no binary-mode `ce-*`
        // attributes at all, and should still fail with a mode error rather
        // than a misleading "missing attribute" one.
        if let Some(content_type) = attributes.get("content-type") {
            let media_type = media_type_of(content_type);
            if media_type == "application/cloudevents+json"
                || media_type == "application/cloudevents-batch+json"
            {
                match self.modes {
                    CloudEventModePolicy::BinaryOnly => {
                        return Err(unsupported_feature(format!(
                            "content mode for media type {media_type:?} is not supported by the binary-mode flattener"
                        )));
                    }
                }
            }
        }

        for name in REQUIRED_ATTRIBUTES {
            if !attributes.contains_key(name) {
                return Err(invalid_envelope(
                    name,
                    format!("required CloudEvents attribute \"{name}\" is missing"),
                ));
            }
        }

        let specversion = attributes
            .get("specversion")
            .map(String::as_str)
            .unwrap_or_default();
        match self.versions {
            CloudEventsVersionPolicy::V1Only => {
                if specversion != "1.0" {
                    return Err(unsupported_feature(format!(
                        "CloudEvents specversion {specversion:?} is not supported"
                    )));
                }
            }
        }

        // The only defined policy distinguishes an absent body, an empty
        // body, and a tombstone purely via `Content-Type`'s and the body's
        // own presence/absence, which is how attributes and payload
        // dispatch are already built; matching here keeps the policy
        // consulted rather than dead weight.
        match self.empty_data {
            EmptyDataPolicy::DistinguishAbsentEmptyAndTombstone => {}
        }

        let mut fields: Vec<(String, CanonicalValue)> = Vec::with_capacity(attributes.len());
        let mut media_type: Option<String> = None;
        for (key, raw) in &attributes {
            match key.as_str() {
                "content-type" => {
                    media_type = Some(media_type_of(raw));
                    fields.push((
                        "datacontenttype".to_owned(),
                        CanonicalValue::String(normalize_content_type(raw)),
                    ));
                }
                "specversion" | "id" | "source" | "type" => {
                    fields.push((key.clone(), CanonicalValue::String(raw.clone())));
                }
                extension => {
                    fields.push((extension.to_owned(), infer_canonical(raw)));
                }
            }
        }

        Ok(Attributes { fields, media_type })
    }

    /// Decode `envelope`'s body through the registered flattener for its
    /// `Content-Type` media type (if any pattern references anything under
    /// `data`), nesting every resulting field under `data`.
    fn dispatch_payload(
        &mut self,
        envelope: &Envelope,
        media_type: Option<&str>,
        tracker: &dyn SegmentsTreeTracker,
        fields: &mut Vec<OwnedField>,
    ) -> Result<(), QuaminaError> {
        if envelope.is_body_absent() || envelope.is_tombstone() {
            return Ok(());
        }
        let Some(data_tracker) = tracker.get(b"data") else {
            return Ok(());
        };
        let body = envelope.body().unwrap_or(&[]);
        let media_type = media_type.unwrap_or_default();
        let Some(flattener) = self.registry.get_mut(media_type) else {
            return match self.unknown_media_types {
                UnknownMediaTypePolicy::MetadataOnly => Ok(()),
                UnknownMediaTypePolicy::Error => Err(unsupported_value(format!(
                    "no payload flattener registered for media type {media_type:?}"
                ))),
            };
        };
        // Some decoders (JSON, MessagePack, CBOR) echo back a matched
        // leaf's *stored* path text as the field path they emit, rather
        // than building it themselves while walking the event. `data_tracker`
        // still carries the full pattern path ("data\n...") its leaves were
        // registered under, so it must be rebased to relative paths first —
        // otherwise those decoders would emit "data\n..." themselves, and
        // prepending "data\n" below would double it. Every decoder in this
        // crate treats a rebased tree as if it were the payload's own root,
        // so this is safe uniformly across the registry.
        let rebased = data_tracker
            .as_any()
            .downcast_ref::<crate::segments_tree::SegmentsTree>()
            .ok_or_else(|| {
                unsupported_feature(
                    "CloudEvents payload dispatch requires a SegmentsTree-based pattern tracker",
                )
            })?
            .rebased();
        let nested = flattener.flatten(body, &rebased)?;
        fields.reserve(nested.len());
        for field in nested {
            let mut path = Vec::with_capacity(field.path.len() + 5);
            path.extend_from_slice(b"data\n");
            path.extend_from_slice(&field.path);
            fields.push(OwnedField {
                path,
                val: field.val,
                array_trail: field.array_trail,
                is_number: field.is_number,
            });
        }
        Ok(())
    }
}

/// The event-root context/extension attributes decoded from an envelope,
/// plus (if a `Content-Type` header was present) the media type to look up
/// in the payload registry.
struct Attributes {
    fields: Vec<(String, CanonicalValue)>,
    media_type: Option<String>,
}

impl EnvelopeFlattener for BinaryCloudEventFlattener {
    fn flatten_envelope(
        &mut self,
        envelope: &Envelope,
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        let attributes = self.compute_attributes(envelope)?;

        let mut fields = Vec::new();
        for (name, value) in &attributes.fields {
            if tracker.is_segment_used(name.as_bytes()) {
                fields.push(OwnedField {
                    path: name.as_bytes().to_vec(),
                    val: value.matcher_bytes(),
                    array_trail: Vec::new(),
                    is_number: value.is_number(),
                });
            }
        }

        if !self.metadata_only {
            self.dispatch_payload(
                envelope,
                attributes.media_type.as_deref(),
                tracker,
                &mut fields,
            )?;
        }

        Ok(fields)
    }

    fn copy(&self) -> Box<dyn EnvelopeFlattener> {
        Box::new(Self {
            registry: self.registry.clone(),
            metadata_only: self.metadata_only,
            versions: self.versions,
            unknown_media_types: self.unknown_media_types,
            empty_data: self.empty_data,
            modes: self.modes,
        })
    }
}

// =============================================================================
// Error helpers
// =============================================================================

fn invalid_envelope(attribute: &'static str, message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::invalid_envelope(EventFormat::CloudEventsBinary, attribute, message)
}

fn conflicting_headers(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::conflicting_envelope_headers(EventFormat::CloudEventsBinary, message)
}

fn unsupported_feature(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_feature(EventFormat::CloudEventsBinary, message)
}

fn unsupported_value(message: impl Into<String>) -> QuaminaError {
    crate::decoder_errors::unsupported_value(EventFormat::CloudEventsBinary, message)
}

// =============================================================================
// Byte/string-level helpers
// =============================================================================

/// Lowercase a header name for attribute-key comparison, rejecting an
/// embedded newline (which would corrupt the single-segment matcher path)
/// or invalid UTF-8.
fn normalize_attribute_name(name: &[u8]) -> Result<String, QuaminaError> {
    if name.contains(&b'\n') {
        return Err(invalid_envelope(
            "name",
            "header name contains an embedded newline, which would corrupt the matcher path",
        ));
    }
    let text = std::str::from_utf8(name)
        .map_err(|_| invalid_envelope("name", "header name is not valid UTF-8"))?;
    Ok(text.to_ascii_lowercase())
}

/// The media type portion of a `Content-Type` value: everything before the
/// first `;`, trimmed and lowercased.
fn media_type_of(content_type: &str) -> String {
    content_type
        .split(';')
        .next()
        .unwrap_or_default()
        .trim()
        .to_ascii_lowercase()
}

/// Normalize a `Content-Type` value into `datacontenttype`: split on `;`,
/// trim each segment, strip a matching pair of surrounding double quotes
/// from any `key=value` segment's value, lowercase every segment (media
/// type, parameter keys, and parameter values alike), then rejoin with
/// `"; "`.
fn normalize_content_type(raw: &str) -> String {
    raw.split(';')
        .map(|segment| {
            let trimmed = segment.trim();
            let normalized = trimmed.split_once('=').map_or_else(
                || trimmed.to_owned(),
                |(key, value)| format!("{}={}", key.trim(), strip_matching_quotes(value.trim())),
            );
            normalized.to_ascii_lowercase()
        })
        .collect::<Vec<_>>()
        .join("; ")
}

/// Strip one matching pair of surrounding double quotes, if present.
fn strip_matching_quotes(value: &str) -> &str {
    if value.len() >= 2 && value.starts_with('"') && value.ends_with('"') {
        &value[1..value.len() - 1]
    } else {
        value
    }
}

/// Infer an extension attribute's canonical type from its transported
/// string value: `"true"`/`"false"` become a canonical boolean, a value
/// that parses as a finite decimal number becomes a canonical number,
/// anything else stays a string.
fn infer_canonical(raw: &str) -> CanonicalValue {
    match raw {
        "true" => CanonicalValue::Bool(true),
        "false" => CanonicalValue::Bool(false),
        _ => CanonicalValue::number(raw).unwrap_or_else(|_| CanonicalValue::String(raw.to_owned())),
    }
}
