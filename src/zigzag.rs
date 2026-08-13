//! Zigzag varint decoding, shared by Protobuf (`sint32`/`sint64`) and Avro
//! (every variable-length integer on the wire uses zigzag encoding).

/// Decode a zigzag-encoded 32-bit integer.
#[cfg_attr(
    not(feature = "protobuf"),
    allow(dead_code, reason = "protobuf-only; avro has no 32-bit zigzag use")
)]
pub const fn decode32(n: u32) -> i32 {
    (n >> 1).cast_signed() ^ -(n & 1).cast_signed()
}

/// Decode a zigzag-encoded 64-bit integer.
pub const fn decode64(n: u64) -> i64 {
    (n >> 1).cast_signed() ^ -(n & 1).cast_signed()
}
