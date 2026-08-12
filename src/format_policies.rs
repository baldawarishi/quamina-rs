//! Shared, format-neutral policy enums used by more than one binary
//! flattener (MessagePack, CBOR, Protobuf, Avro, ...).
//!
//! Each binary format can express values JSON has no equivalent for (raw
//! binary, non-text map keys, duplicate keys, non-object roots, numbers
//! outside JSON's safe range). Rather than let every decoder invent its own
//! name for the same choice, decoders that share a concept share its policy
//! type here. A format-specific concept (e.g. MessagePack's extension
//! types) stays defined in that format's own module.

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
