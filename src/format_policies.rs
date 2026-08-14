//! Shared, format-neutral policy enums used by more than one binary
//! flattener (MessagePack, CBOR, Protobuf, Avro, ...).
//!
//! Each binary format can express values JSON has no equivalent for (raw
//! binary, non-text map keys, duplicate keys, non-object roots, numbers
//! outside JSON's safe range). Rather than let every decoder invent its own
//! name for the same choice, decoders that share a concept share its policy
//! type here. A format-specific concept (e.g. MessagePack's extension
//! types) stays defined in that format's own module.

use crate::canonical::CanonicalValue;
use crate::decoder_errors::unsupported_value;
use crate::{EventFormat, QuaminaError};

/// The largest integer magnitude that still round-trips exactly through an
/// `f64`, i.e. `2^53` — matching JSON's `Number.isSafeInteger` boundary.
const MAX_SAFE_INT: i128 = 9_007_199_254_740_992;

/// How a decoder canonicalizes integers and floats to Quamina's numeric
/// matcher representation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum NumericPolicy {
    /// Canonicalize to Quamina's lossless numeric form. Integers whose
    /// magnitude exceeds the range an `f64` can represent exactly, and
    /// non-finite floats, are rejected rather than silently truncated.
    #[default]
    LosslessQuamina,
}

impl NumericPolicy {
    /// Canonicalize a decoded integer per this policy.
    ///
    /// # Errors
    /// Returns `QuaminaError::UnsupportedEventValue` if `value`'s magnitude
    /// exceeds what this policy allows.
    pub fn canonicalize_int(
        self,
        value: i128,
        format: EventFormat,
        offset: usize,
    ) -> Result<CanonicalValue, QuaminaError> {
        match self {
            Self::LosslessQuamina => {
                if !(-MAX_SAFE_INT..=MAX_SAFE_INT).contains(&value) {
                    return Err(unsupported_value(
                        format,
                        "integer exceeds lossless numeric range",
                    )
                    .at_byte_offset(offset));
                }
                // Range-checked above: value fits in i64.
                #[allow(clippy::cast_possible_truncation)]
                Ok(CanonicalValue::from_i64(value as i64))
            }
        }
    }
}

/// How a decoder handles raw binary values (MessagePack `bin`, CBOR byte
/// strings, ...) that have no JSON equivalent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BinaryValuePolicy {
    /// Reject any event containing a binary value. This is the default: a
    /// binary value has no JSON equivalent, so silently coercing it risks
    /// colliding with a string of the same content.
    #[default]
    Reject,
    /// Represent binary values as the string `base64:<standard base64>`,
    /// e.g. bytes `[0x00, 0xff]` become `"base64:AP8="`. The `base64:`
    /// prefix keeps this collision-free with ordinary string values.
    TaggedBase64,
}

/// How a decoder validates map/object keys.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MapKeyPolicy {
    /// Map keys must be text; any other key type is rejected.
    #[default]
    TextOnly,
}

/// How a decoder handles a map/object that repeats the same key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DuplicateKeyPolicy {
    /// Reject the event outright. Quamina never silently collapses
    /// duplicate keys with a first-write or last-write rule.
    #[default]
    Reject,
}

/// What root-level values a decoder accepts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum RootValuePolicy {
    /// The root value must be a map/object. A root scalar or array is
    /// rejected, matching the JSON flattener's object-only root.
    #[default]
    MapOnly,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn canonicalize_int_accepts_zero_and_typical_values() {
        let policy = NumericPolicy::LosslessQuamina;
        assert_eq!(
            policy
                .canonicalize_int(0, EventFormat::Custom("test"), 0)
                .unwrap(),
            CanonicalValue::from_i64(0)
        );
        assert_eq!(
            policy
                .canonicalize_int(-42, EventFormat::Custom("test"), 0)
                .unwrap(),
            CanonicalValue::from_i64(-42)
        );
    }

    #[test]
    fn canonicalize_int_accepts_exactly_the_safe_integer_boundary() {
        let policy = NumericPolicy::LosslessQuamina;
        assert!(
            policy
                .canonicalize_int(MAX_SAFE_INT, EventFormat::Custom("test"), 0)
                .is_ok()
        );
        assert!(
            policy
                .canonicalize_int(-MAX_SAFE_INT, EventFormat::Custom("test"), 0)
                .is_ok()
        );
    }

    #[test]
    fn canonicalize_int_rejects_one_past_the_safe_integer_boundary() {
        let policy = NumericPolicy::LosslessQuamina;
        let over = policy.canonicalize_int(MAX_SAFE_INT + 1, EventFormat::Custom("test"), 7);
        assert!(matches!(
            over,
            Err(QuaminaError::UnsupportedEventValue { .. })
        ));
        let under = policy.canonicalize_int(-MAX_SAFE_INT - 1, EventFormat::Custom("test"), 7);
        assert!(matches!(
            under,
            Err(QuaminaError::UnsupportedEventValue { .. })
        ));
    }

    #[test]
    fn canonicalize_int_error_carries_the_given_format_and_offset() {
        let err = NumericPolicy::LosslessQuamina
            .canonicalize_int(MAX_SAFE_INT + 1, EventFormat::Cbor, 13)
            .unwrap_err();
        match err {
            QuaminaError::UnsupportedEventValue {
                format, location, ..
            } => {
                assert_eq!(format, EventFormat::Cbor);
                assert_eq!(location.byte_offset(), Some(13));
            }
            other => panic!("expected UnsupportedEventValue, got {other:?}"),
        }
    }
}
