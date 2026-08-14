//! `QuaminaError` construction helpers shared by every binary/envelope
//! decoder. Each decoder module still exposes its own zero-argument
//! wrappers (`invalid_event(msg)`, `limit_exceeded(msg)`, ...) bound to its
//! own [`EventFormat`], but delegates the actual variant construction here
//! instead of repeating the same match arm in every format module.
//!
//! Not every helper is used by every format (e.g. `missing_schema` is
//! Avro-only), and which format modules are compiled at all depends on
//! which `--features` are enabled, so an individual build can legitimately
//! use only a subset of these.
#![allow(dead_code)]

use crate::{ErrorLocation, EventFormat, QuaminaError};

/// Build a generic "malformed event bytes" error for `format`.
pub fn invalid_event(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::invalid_event(format).with_message(message)
}

/// Build an "invalid schema/descriptor" error for `format`.
pub fn invalid_schema(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::InvalidSchema {
        format,
        message: message.into(),
    }
}

/// Build a "resource limit exceeded" error for `format`.
pub fn limit_exceeded(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::EventLimitExceeded {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build an "unsupported event value" error for `format`.
pub fn unsupported_value(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::UnsupportedEventValue {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build an "unsupported map/object key" error for `format`.
pub fn unsupported_map_key(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::UnsupportedMapKey {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build an "unsupported format feature" error for `format`.
pub fn unsupported_feature(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::UnsupportedFormatFeature {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build a "schema needed to decode this event is missing" error for `format`.
pub fn missing_schema(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::MissingEventSchema {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build a "duplicate field" error for `format`.
pub fn duplicate_field(format: EventFormat) -> QuaminaError {
    QuaminaError::DuplicateEventField {
        format,
        location: ErrorLocation::default(),
    }
}

/// Build an "invalid transport envelope" error for `format`, naming the
/// offending `attribute`.
pub fn invalid_envelope(
    format: EventFormat,
    attribute: &'static str,
    message: impl Into<String>,
) -> QuaminaError {
    QuaminaError::InvalidEnvelope {
        format,
        location: ErrorLocation::default(),
        attribute,
        message: message.into(),
    }
}

/// Build a "conflicting transport envelope headers" error for `format`.
pub fn conflicting_envelope_headers(
    format: EventFormat,
    message: impl Into<String>,
) -> QuaminaError {
    QuaminaError::ConflictingEnvelopeHeaders {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build an "envelope path collision" error for `format`.
pub fn envelope_path_collision(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::EnvelopePathCollision {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build an "invalid field path" error for `format`.
pub fn invalid_event_path(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::InvalidEventPath {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build an "ambiguous field path" error for `format`: a raw path embedded
/// the segment separator instead of being constructed as distinct segments.
pub fn ambiguous_event_path(format: EventFormat) -> QuaminaError {
    QuaminaError::AmbiguousEventPath {
        format,
        location: ErrorLocation::default(),
    }
}

/// Build an "invalid canonical field" error for `format`: a raw scalar's
/// bytes did not match its declared numeric/string tag.
pub fn invalid_canonical_field(format: EventFormat, message: impl Into<String>) -> QuaminaError {
    QuaminaError::InvalidCanonicalField {
        format,
        location: ErrorLocation::default(),
        message: message.into(),
    }
}

/// Build a "conflicting array id" error for `format`: the same array id was
/// used for two structurally different arrays.
pub fn conflicting_array_id(format: EventFormat, id: i32) -> QuaminaError {
    QuaminaError::ConflictingArrayId {
        format,
        location: ErrorLocation::default(),
        id,
    }
}
