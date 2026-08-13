//! `EventLimits` bounds-checking helpers shared by every binary decoder's
//! recursive-descent walk (depth, scalar length, container item count, and
//! array-id allocation). Parameterized by [`EventFormat`] and delegating
//! error construction to [`crate::decoder_errors`], the same pattern used
//! throughout this crate's format modules.

use crate::{EventFormat, EventLimits, QuaminaError};

/// Check a candidate nesting depth against [`EventLimits::max_depth`].
pub fn check_depth(
    limits: &EventLimits,
    format: EventFormat,
    new_depth: usize,
    offset: usize,
) -> Result<(), QuaminaError> {
    if new_depth > limits.max_depth {
        return Err(
            crate::decoder_errors::limit_exceeded(format, "max_depth exceeded")
                .at_byte_offset(offset),
        );
    }
    Ok(())
}

/// Check a scalar's byte length against [`EventLimits::max_scalar_bytes`].
pub fn check_scalar_len(
    limits: &EventLimits,
    format: EventFormat,
    len: usize,
    offset: usize,
) -> Result<(), QuaminaError> {
    if len > limits.max_scalar_bytes {
        return Err(
            crate::decoder_errors::limit_exceeded(format, "max_scalar_bytes exceeded")
                .at_byte_offset(offset),
        );
    }
    Ok(())
}

/// Check a container's declared item count against
/// [`EventLimits::max_container_items`].
#[cfg_attr(
    not(any(feature = "messagepack", feature = "cbor")),
    allow(
        dead_code,
        reason = "MessagePack/CBOR track container items this way today"
    )
)]
pub fn check_container_len(
    limits: &EventLimits,
    format: EventFormat,
    len: usize,
    offset: usize,
) -> Result<(), QuaminaError> {
    if len > limits.max_container_items {
        return Err(
            crate::decoder_errors::limit_exceeded(format, "max_container_items exceeded")
                .at_byte_offset(offset),
        );
    }
    Ok(())
}

/// Allocate the next array id from `next_id`, rejecting overflow.
pub fn alloc_array_id(
    next_id: &mut i32,
    format: EventFormat,
    offset: usize,
) -> Result<i32, QuaminaError> {
    let id = *next_id;
    *next_id = next_id.checked_add(1).ok_or_else(|| {
        crate::decoder_errors::limit_exceeded(format, "array id allocation overflowed")
            .at_byte_offset(offset)
    })?;
    Ok(id)
}
