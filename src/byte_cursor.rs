//! Shared byte-level read primitives for schema-less binary decoders (CBOR,
//! MessagePack) that walk a `&[u8]` one marker at a time.
//!
//! Each decoder still owns its `data`/`pos` fields directly (a cursor type
//! of its own would just relocate them, not remove the duplication) and
//! exposes its own zero-argument-beyond-`self` wrappers bound to its own
//! [`EventFormat`], but delegates the actual bounds-checked reads here.

use crate::decoder_errors::invalid_event;
use crate::{EventFormat, QuaminaError};

/// The byte at `data[pos]`, without advancing.
///
/// # Errors
/// Returns `QuaminaError::MalformedEvent` if `pos` is past the end of `data`.
pub fn peek_u8(data: &[u8], pos: usize, format: EventFormat) -> Result<u8, QuaminaError> {
    data.get(pos)
        .copied()
        .ok_or_else(|| invalid_event(format, "unexpected end of event").at_byte_offset(pos))
}

/// The byte at `*pos`, advancing `*pos` by one.
///
/// # Errors
/// Returns `QuaminaError::MalformedEvent` if `*pos` is past the end of `data`.
pub fn take_u8(data: &[u8], pos: &mut usize, format: EventFormat) -> Result<u8, QuaminaError> {
    let byte = peek_u8(data, *pos, format)?;
    *pos += 1;
    Ok(byte)
}

/// `len` bytes starting at `*pos`, advancing `*pos` past them.
///
/// # Errors
/// Returns `QuaminaError::MalformedEvent` if `*pos + len` overflows or runs
/// past the end of `data`.
pub fn take_bytes<'a>(
    data: &'a [u8],
    pos: &mut usize,
    len: usize,
    format: EventFormat,
) -> Result<&'a [u8], QuaminaError> {
    let start = *pos;
    let end = start
        .checked_add(len)
        .ok_or_else(|| invalid_event(format, "length overflow").at_byte_offset(start))?;
    let slice = data
        .get(start..end)
        .ok_or_else(|| invalid_event(format, "unexpected end of event").at_byte_offset(start))?;
    *pos = end;
    Ok(slice)
}

/// A big-endian `u16` starting at `*pos`, advancing `*pos` past it.
///
/// # Errors
/// Returns `QuaminaError::MalformedEvent` if fewer than 2 bytes remain.
pub fn take_u16(data: &[u8], pos: &mut usize, format: EventFormat) -> Result<u16, QuaminaError> {
    let b = take_bytes(data, pos, 2, format)?;
    Ok(u16::from_be_bytes([b[0], b[1]]))
}

/// A big-endian `u32` starting at `*pos`, advancing `*pos` past it.
///
/// # Errors
/// Returns `QuaminaError::MalformedEvent` if fewer than 4 bytes remain.
pub fn take_u32(data: &[u8], pos: &mut usize, format: EventFormat) -> Result<u32, QuaminaError> {
    let b = take_bytes(data, pos, 4, format)?;
    Ok(u32::from_be_bytes([b[0], b[1], b[2], b[3]]))
}

/// A big-endian `u64` starting at `*pos`, advancing `*pos` past it.
///
/// # Errors
/// Returns `QuaminaError::MalformedEvent` if fewer than 8 bytes remain.
pub fn take_u64(data: &[u8], pos: &mut usize, format: EventFormat) -> Result<u64, QuaminaError> {
    let b = take_bytes(data, pos, 8, format)?;
    Ok(u64::from_be_bytes([
        b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7],
    ]))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn peek_and_take_u8_agree_without_advancing_on_peek() {
        let data = [0x11, 0x22];
        let mut pos = 0;
        assert_eq!(peek_u8(&data, pos, EventFormat::Cbor).unwrap(), 0x11);
        assert_eq!(pos, 0);
        assert_eq!(take_u8(&data, &mut pos, EventFormat::Cbor).unwrap(), 0x11);
        assert_eq!(pos, 1);
        assert_eq!(take_u8(&data, &mut pos, EventFormat::Cbor).unwrap(), 0x22);
        assert_eq!(pos, 2);
        assert!(take_u8(&data, &mut pos, EventFormat::Cbor).is_err());
    }

    #[test]
    fn take_bytes_advances_past_the_slice_and_rejects_overrun() {
        let data = [1, 2, 3, 4];
        let mut pos = 1;
        assert_eq!(
            take_bytes(&data, &mut pos, 2, EventFormat::Cbor).unwrap(),
            &[2, 3]
        );
        assert_eq!(pos, 3);
        assert!(take_bytes(&data, &mut pos, 5, EventFormat::Cbor).is_err());
        // A rejected read must not silently succeed by clamping to what's left.
        assert_eq!(pos, 3);
    }

    #[test]
    fn take_bytes_rejects_length_overflow_without_panicking() {
        let data = [1, 2, 3];
        let mut pos = 1;
        assert!(take_bytes(&data, &mut pos, usize::MAX, EventFormat::Cbor).is_err());
    }

    #[test]
    fn take_u16_u32_u64_decode_big_endian() {
        let data = [
            0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03,
        ];
        let mut pos = 0;
        assert_eq!(take_u16(&data, &mut pos, EventFormat::Cbor).unwrap(), 1);
        assert_eq!(take_u32(&data, &mut pos, EventFormat::Cbor).unwrap(), 2);
        assert_eq!(take_u64(&data, &mut pos, EventFormat::Cbor).unwrap(), 3);
        assert_eq!(pos, data.len());
    }
}
