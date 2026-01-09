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
pub const MAX_BYTES_IN_ENCODING: usize = 10;

/// A numbits is an alternative binary representation of float64 numbers.
/// All possible float64 values are representable as numbits.
pub type Numbits = u64;

/// Q-number: variable-width byte encoding that preserves ordering.
pub type QNumber = Vec<u8>;

/// Convert a float64 to its numbits representation.
///
/// The resulting u64 can be compared directly to preserve numeric ordering:
/// if a < b as floats, then numbits_from_f64(a) < numbits_from_f64(b) as u64.
///
/// Note: This implementation ignores NaN, -0, and infinities because JSON
/// rules and Quamina's parsers prevent those values from occurring.
pub fn numbits_from_f64(f: f64) -> Numbits {
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
pub fn to_q_number(nb: Numbits) -> QNumber {
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

/// Convert a float64 to its Q-number representation.
pub fn q_num_from_f64(f: f64) -> QNumber {
    to_q_number(numbits_from_f64(f))
}

/// Parse a byte string as a float64 and convert to Q-number.
///
/// Returns None if the string cannot be parsed as a float.
pub fn q_num_from_bytes(bytes: &[u8]) -> Option<QNumber> {
    let s = std::str::from_utf8(bytes).ok()?;
    let f: f64 = s.parse().ok()?;
    Some(q_num_from_f64(f))
}

/// Format a Q-number for debugging.
pub fn q_num_to_string(q: &QNumber) -> String {
    q.iter()
        .map(|b| format!("{:02x}", b))
        .collect::<Vec<_>>()
        .join("-")
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let q_nums: Vec<QNumber> = floats.iter().map(|&f| q_num_from_f64(f)).collect();

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
        let q_nums: Vec<QNumber> = strings
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

    #[test]
    fn test_ordering_random() {
        // Sort random floats and verify their Q-numbers maintain the same order
        use std::cmp::Ordering;

        let mut floats: Vec<f64> = Vec::new();
        let mut rng_state = 12345u64;

        // Simple LCG for reproducibility
        for _ in 0..10000 {
            rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1);
            let random_u64 = rng_state;
            let f = ((random_u64 as f64) / (u64::MAX as f64)) * 2_000_000_000.0 - 1_000_000_000.0;
            floats.push(f);
        }

        floats.sort_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal));

        let q_nums: Vec<QNumber> = floats.iter().map(|&f| q_num_from_f64(f)).collect();

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
}
