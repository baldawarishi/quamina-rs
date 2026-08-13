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
