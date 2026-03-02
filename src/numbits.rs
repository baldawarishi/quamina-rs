//! IEEE 754 float64 to ordered bytes conversion for automaton numeric matching.
//!
//! This module provides a representation of float64 numbers that preserves ordering
//! when compared as byte sequences. This allows numeric comparisons to be performed
//! using the same automaton-based approach used for string matching.
//!
//! The implementation is based on Arne Hormann's "numbits" construct, which was
//! also used in the disk format of DB2.
//!
//! # How it works
//!
//! IEEE 754 float64 representation: (sign | exponent | mantissa)
//! - 1 bit sign
//! - 11 bits exponent
//! - 52 bits mantissa
//!
//! The problem: IEEE 754 doesn't preserve numeric ordering when compared as unsigned integers.
//! Negative numbers have the sign bit set, making them appear "larger" than positive numbers.
//!
//! The solution: Transform the bit representation:
//! - For positive numbers (sign bit 0): XOR with sign bit (1 << 63)
//! - For negative numbers (sign bit 1): Negate (XOR with !0)
//!
//! This transformation ensures that when comparing the resulting integers:
//! - Negative numbers compare less than positive numbers
//! - Numbers within the same sign compare in the correct order

/// Maximum bytes needed for base-128 encoding of a 64-bit value.
pub(crate) const MAX_BYTES_IN_ENCODING: usize = 10;

/// Stack-allocated Q-number representation for zero-allocation numeric matching.
///
/// This type holds a Q-number in a fixed 10-byte stack buffer (the maximum size
/// for base-128 encoded 64-bit values), avoiding heap allocation during the
/// matching hot path.
///
/// Created via [`q_num_stack`]. Use [`as_slice`](Self::as_slice) to get the
/// actual bytes for FA traversal.
#[derive(Clone, Copy, Debug)]
pub struct QNumberStack {
    bytes: [u8; MAX_BYTES_IN_ENCODING],
    len: u8,
}

impl QNumberStack {
    /// Returns the Q-number bytes as a slice.
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        &self.bytes[..self.len as usize]
    }

    /// Returns the length of the Q-number.
    #[inline]
    pub fn len(&self) -> usize {
        self.len as usize
    }

    /// Returns true if the Q-number is empty (should never happen for valid floats).
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
}

/// Convert numbits to a stack-allocated Q-number.
pub(crate) fn to_q_number_stack(nb: u64) -> QNumberStack {
    let mut nb = nb;
    let mut bytes = [0u8; MAX_BYTES_IN_ENCODING];

    // Count trailing zero septets (same algorithm as to_q_number)
    let mut trailing_zeroes = 0usize;
    let mut index = MAX_BYTES_IN_ENCODING - 1;

    loop {
        if nb & 0x7f != 0 {
            break;
        }
        trailing_zeroes += 1;
        nb >>= 7;
        if index == 0 {
            break;
        }
        index -= 1;
    }

    let len = MAX_BYTES_IN_ENCODING - trailing_zeroes;

    // Fill bytes from right to left
    for i in (0..len).rev() {
        bytes[i] = (nb & 0x7f) as u8;
        nb >>= 7;
    }

    QNumberStack {
        bytes,
        len: len as u8,
    }
}

/// Convert a float64 to a stack-allocated Q-number (zero heap allocation).
///
/// Use this for the **matching hot path** where the Q-number is temporary and
/// doesn't need to be stored. The stack allocation avoids heap overhead.
///
/// For pattern building where you need to store the bytes, use [`q_num_from_f64`]
/// which returns a `Vec<u8>`.
///
/// Both functions produce identical byte sequences.
pub fn q_num_stack(f: f64) -> QNumberStack {
    to_q_number_stack(numbits_from_f64(f))
}

/// Convert a float64 to its numbits representation.
///
/// The resulting u64 can be compared directly to preserve numeric ordering:
/// if a < b as floats, then numbits_from_f64(a) < numbits_from_f64(b) as u64.
///
/// Note: This implementation ignores NaN, -0, and infinities because JSON
/// rules and Quamina's parsers prevent those values from occurring.
pub(crate) fn numbits_from_f64(f: f64) -> u64 {
    let u = f.to_bits();
    // Transform without branching:
    // If high bit is 0, xor with sign bit (1 << 63), else negate (xor with !0).
    // Using a sign extending right shift was proposed by Raph Levien in
    // https://mastodon.online/@raph/113071041069390831
    let mask = ((u as i64 >> 63) as u64) | (1 << 63);
    u ^ mask
}

/// Convert numbits to a minimal variable-width encoding that preserves ordering.
///
/// Storing 8 bytes of data in base-128 would in principle require 10 bytes,
/// but since the encoding is big-endian, trailing zeroes don't count, so
/// the encoding can be as short as one byte.
///
/// Idea and some code by Axel Wagner.
pub(crate) fn to_q_number(nb: u64) -> Vec<u8> {
    let mut nb = nb;

    // Iterate through the numbits 7 bits at a time, right to left,
    // first bypassing bits that generate trailing zeroes in the encoded form.
    // Note that index could go to 0 if the numbits value was 0,
    // but that value represents NaN and can't appear in JSON.
    let mut trailing_zeroes = 0usize;
    let mut index = MAX_BYTES_IN_ENCODING - 1;

    loop {
        if nb & 0x7f != 0 {
            break;
        }
        trailing_zeroes += 1;
        nb >>= 7;
        if index == 0 {
            break;
        }
        index -= 1;
    }

    // Now fill in the byte encoding for the digits up to the last non-zero
    let len = MAX_BYTES_IN_ENCODING - trailing_zeroes;
    let mut result = vec![0u8; len];

    for i in (0..len).rev() {
        result[i] = (nb & 0x7f) as u8;
        nb >>= 7;
    }

    result
}

/// Convert a float64 to its Q-number representation (heap-allocated).
///
/// Use this for **pattern building** where the Q-number bytes need to be stored
/// or passed to FA construction functions.
///
/// For the matching hot path where the Q-number is temporary, use [`q_num_stack`]
/// which avoids heap allocation.
///
/// Both functions produce identical byte sequences.
pub fn q_num_from_f64(f: f64) -> Vec<u8> {
    to_q_number(numbits_from_f64(f))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Parse a byte string as a float64 and convert to Q-number.
    /// Returns None if the string cannot be parsed as a float.
    fn q_num_from_bytes(bytes: &[u8]) -> Option<Vec<u8>> {
        let s = std::str::from_utf8(bytes).ok()?;
        let f: f64 = s.parse().ok()?;
        Some(q_num_from_f64(f))
    }

    /// Format a Q-number for debugging.
    fn q_num_to_string(q: &[u8]) -> String {
        q.iter()
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<_>>()
            .join("-")
    }

    #[test]
    #[allow(clippy::inconsistent_digit_grouping)]
    fn test_wildly_varying_numbers_are_comparable() {
        let data: Vec<f64> = vec![
            -5_000_000_000.0,
            -4_999_999_999.99999,
            -4_999_999_999.99998,
            -4_999_999_999.99997,
            -999999999.99999,
            -999999999.99,
            -10000.0,
            -122.413496,
            -0.000002,
            0.0,
            0.000001,
            3.8,
            3.9,
            11.0,
            12.0,
            122.415028,
            2.5e4,
            999999999.999998,
            999999999.999999,
            4_999_999_999.99997,
            4_999_999_999.99998,
            4_999_999_999.99999,
            5_000_000_000.0,
        ];

        for i in 1..data.len() {
            let s0 = q_num_from_f64(data[i - 1]);
            let s1 = q_num_from_f64(data[i]);
            assert!(
                s0 < s1,
                "Ordering failed at index {}: {} ({:?}) should be < {} ({:?})",
                i,
                data[i - 1],
                q_num_to_string(&s0),
                data[i],
                q_num_to_string(&s1)
            );
        }
    }

    #[test]
    fn test_float_variants() {
        // Different representations of the same value should produce the same Q-number
        let floats: Vec<f64> = vec![350.0, 350.0, 350.0000000000, 3.5e2];
        let q_nums: Vec<Vec<u8>> = floats.iter().map(|&f| q_num_from_f64(f)).collect();

        for i in 1..q_nums.len() {
            assert_eq!(
                q_nums[i],
                q_nums[i - 1],
                "Q-numbers differ for {} vs {}",
                floats[i - 1],
                floats[i]
            );
        }
    }

    #[test]
    fn test_byte_variants() {
        // Different string representations of the same value should produce the same Q-number
        let strings: Vec<&str> = vec!["350", "350.0", "350.0000", "3.5e2"];
        let q_nums: Vec<Vec<u8>> = strings
            .iter()
            .map(|s| q_num_from_bytes(s.as_bytes()).unwrap())
            .collect();

        for i in 1..q_nums.len() {
            assert_eq!(
                q_nums[i],
                q_nums[i - 1],
                "Q-numbers differ for '{}' vs '{}'",
                strings[i - 1],
                strings[i]
            );
        }
    }

    /// Shared helper: generate `count` random floats via LCG, sort them, and verify
    /// that Q-number ordering is preserved.
    fn verify_ordering_random(count: usize) {
        use std::cmp::Ordering;

        let mut floats: Vec<f64> = Vec::new();
        let mut rng_state = 12345u64;

        for _ in 0..count {
            rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1);
            let random_u64 = rng_state;
            let f = ((random_u64 as f64) / (u64::MAX as f64)) * 2_000_000_000.0 - 1_000_000_000.0;
            floats.push(f);
        }

        floats.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));

        let q_nums: Vec<Vec<u8>> = floats.iter().map(|&f| q_num_from_f64(f)).collect();

        for i in 1..q_nums.len() {
            assert!(
                q_nums[i - 1] <= q_nums[i],
                "Q-number ordering failed at index {}: {:?} > {:?} (floats: {} > {})",
                i,
                q_nums[i - 1],
                q_nums[i],
                floats[i - 1],
                floats[i]
            );
        }
    }

    // MIRI SKIP RATIONALE: 10,000 random floats with sort + Q-number conversion is slow
    // under Miri (~90s). Coverage: test_ordering_random_miri_friendly uses 50 floats.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_ordering_random() {
        verify_ordering_random(10_000);
    }

    /// Miri-friendly version — 50 random floats instead of 10,000.
    #[test]
    fn test_ordering_random_miri_friendly() {
        verify_ordering_random(50);
    }

    #[test]
    fn test_bad_numbers() {
        let bads = vec!["xy", "- 53", "124x", "1.5ee7"];
        for bad in bads {
            assert!(
                q_num_from_bytes(bad.as_bytes()).is_none(),
                "Should reject: {}",
                bad
            );
        }
    }

    #[test]
    fn test_q_number_length() {
        // Test that Q-numbers are variable length (strip trailing zeros)
        let q0 = q_num_from_f64(0.0);
        let q_large = q_num_from_f64(1e15);

        // Both should be <= MAX_BYTES_IN_ENCODING
        assert!(q0.len() <= MAX_BYTES_IN_ENCODING);
        assert!(q_large.len() <= MAX_BYTES_IN_ENCODING);

        // Different values may have different lengths
        // (specific lengths depend on the encoding)
    }

    #[test]
    fn test_numbits_ordering_property() {
        // Verify that numbits preserves ordering at the u64 level
        let pairs: Vec<(f64, f64)> = vec![
            (-100.0, -50.0),
            (-50.0, 0.0),
            (0.0, 50.0),
            (50.0, 100.0),
            (-1e10, 1e10),
            (0.0001, 0.0002),
        ];

        for (a, b) in pairs {
            let na = numbits_from_f64(a);
            let nb = numbits_from_f64(b);
            assert!(
                na < nb,
                "numbits ordering failed: {} ({}) should be < {} ({})",
                a,
                na,
                b,
                nb
            );
        }
    }

    #[test]
    fn test_zero_handling() {
        // Test that 0.0 and -0.0 both work (though JSON typically only has 0)
        let q_zero = q_num_from_f64(0.0);
        let q_neg_zero = q_num_from_f64(-0.0);

        // Both should produce valid Q-numbers
        assert!(!q_zero.is_empty());
        assert!(!q_neg_zero.is_empty());
    }

    // =========================================================================
    // Tests for Q-number variant equivalence
    // =========================================================================

    #[test]
    fn test_q_number_variants_equivalence() {
        // Vec and Stack implementations must produce identical byte sequences
        let test_values: Vec<f64> = vec![
            0.0,
            1.0,
            -1.0,
            42.0,
            -42.0,
            999.0,
            1000.0,
            123456.0,
            0.000001,
            -0.000001,
            std::f64::consts::PI,
            -std::f64::consts::PI,
            1e10,
            -1e10,
            1e-10,
            -1e-10,
            f64::MIN_POSITIVE,
            f64::MAX,
            f64::MIN,
        ];

        for &val in &test_values {
            let vec_result = q_num_from_f64(val);
            let stack_result = q_num_stack(val);

            assert_eq!(
                vec_result.as_slice(),
                stack_result.as_slice(),
                "Stack variant differs from Vec for value {}",
                val
            );
        }
    }

    #[test]
    fn test_q_number_variants_random() {
        // Test with random values to ensure equivalence
        let mut rng_state = 54321u64;

        for _ in 0..100 {
            rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1);
            let f = f64::from_bits(rng_state & 0x7FEFFFFFFFFFFFFF); // Avoid NaN/Inf

            // Skip if not finite
            if !f.is_finite() {
                continue;
            }

            let vec_result = q_num_from_f64(f);
            let stack_result = q_num_stack(f);

            assert_eq!(
                vec_result.as_slice(),
                stack_result.as_slice(),
                "Stack variant differs from Vec for value {}",
                f
            );
        }
    }

    #[test]
    fn test_q_number_stack_length() {
        // Test that QNumberStack correctly reports length
        let q0 = q_num_stack(0.0);
        let q_large = q_num_stack(1e15);

        assert!(!q0.is_empty());
        assert!(q0.len() <= MAX_BYTES_IN_ENCODING);
        assert!(q_large.len() <= MAX_BYTES_IN_ENCODING);
        assert!(!q0.is_empty());
    }

    #[test]
    fn test_q_number_stack_len_matches_slice() {
        let values = [
            0.0,
            1.0,
            -1.0,
            42.0,
            1e15,
            -1e-10,
            f64::MAX,
            f64::MIN_POSITIVE,
        ];
        for &val in &values {
            let q = q_num_stack(val);
            assert_eq!(
                q.len(),
                q.as_slice().len(),
                "len() disagrees with as_slice().len() for {}",
                val
            );
        }
    }

    #[test]
    fn test_q_number_stack_is_empty_synthetic() {
        // A zero-length QNumberStack should report is_empty() == true.
        // This can't happen via q_num_stack (all valid floats produce non-empty),
        // but verifies is_empty correctness at the boundary.
        let empty = QNumberStack {
            bytes: [0; MAX_BYTES_IN_ENCODING],
            len: 0,
        };
        assert!(empty.is_empty());
        assert_eq!(empty.len(), 0);
    }

    #[test]
    fn test_encoding_with_zero_numbits() {
        // nb=0 corresponds to NaN (can't appear in JSON), but exercising
        // the encoding boundary verifies the trailing-zero-skip loop guard.
        let vec_result = to_q_number(0);
        assert!(vec_result.is_empty());

        let stack_result = to_q_number_stack(0);
        assert!(stack_result.is_empty());
        assert_eq!(stack_result.len(), 0);
    }
}
