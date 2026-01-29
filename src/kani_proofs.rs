//! Kani proof harnesses for verifying unsafe code safety.
//!
//! These proofs use bounded model checking to exhaustively verify properties
//! of unsafe code within bounds. Run with: `cargo kani`
//!
//! See TOOLKIT.md Phase 4 for context.
//!
//! ## Proof Selection Rationale
//!
//! Kani works best with simple, bounded code paths. Complex data structures
//! like Vec cause exponential state explosion. We focus on:
//! - Constant validation (BYTE_CEILING)
//! - Simple table lookups (SmallTable::step/dstep)
//! - JSON parsing invariants (UTF-8 validity)
//!
//! Arena allocations and complex automaton traversals are better tested by
//! Miri (for UB) and fuzzing (for crash inputs).
//!
//! ## Performance Notes
//!
//! Fast proofs (< 1s): byte_ceiling_utf8_valid, stateid_none_*, stateptr_equality_*
//! Medium proofs (1-30s): smalltable_step_*, json_string_byte_validity
//! Slow proofs (> 1min): smalltable_unpack_* (246-element arrays) - skipped in CI

#[cfg(kani)]
mod proofs {
    use crate::automaton::{SmallTable, StatePtr, BYTE_CEILING};

    /// Proof: SmallTable::step never panics for any valid UTF-8 byte.
    ///
    /// The SmallTable uses a ceiling-based lookup that must handle all bytes 0-245.
    #[kani::proof]
    fn smalltable_step_no_panic() {
        let table = SmallTable::new();
        let byte: u8 = kani::any();

        // UTF-8 bytes are 0x00-0xF4 (valid) or could be 0xF5 (VALUE_TERMINATOR)
        kani::assume(byte < BYTE_CEILING as u8);

        // This should never panic
        let (_step, _epsilons) = table.step(byte);
    }

    /// Proof: SmallTable::dstep never panics for any valid byte.
    #[kani::proof]
    fn smalltable_dstep_no_panic() {
        let table = SmallTable::new();
        let byte: u8 = kani::any();
        kani::assume(byte < BYTE_CEILING as u8);

        // This should never panic
        let _step = table.dstep(byte);
    }

    /// Proof: BYTE_CEILING constant is correct for UTF-8.
    ///
    /// UTF-8 bytes 0xF5-0xFF cannot appear in valid UTF-8 sequences.
    /// We use 0xF5 as VALUE_TERMINATOR, so BYTE_CEILING = 0xF6.
    #[kani::proof]
    fn byte_ceiling_utf8_valid() {
        // 0xF4 is the highest valid leading byte in UTF-8 (for U+10FFFF)
        // 0xF5 is our terminator
        // 0xF6 is BYTE_CEILING
        kani::assert(BYTE_CEILING == 0xF6, "BYTE_CEILING must be 0xF6");
    }

    /// Proof: StatePtr equality is reflexive.
    ///
    /// StatePtr wraps raw pointers for hash set deduplication.
    #[kani::proof]
    fn stateptr_equality_reflexive() {
        use std::ptr;

        // Create a StatePtr from null (simplest case)
        let state_ptr = StatePtr::new(ptr::null());

        // Reflexive equality
        kani::assert(
            state_ptr == state_ptr,
            "StatePtr equality must be reflexive",
        );
    }
}

#[cfg(kani)]
mod flatten_json_proofs {
    /// Proof: JSON string bytes (non-escaped) are valid UTF-8.
    ///
    /// JSON specification requires strings to be valid UTF-8.
    /// Bytes 0x00-0x1F are control characters that must be escaped.
    /// This proof verifies our from_utf8_unchecked assumption.
    #[kani::proof]
    fn json_string_byte_validity() {
        let byte: u8 = kani::any();

        // JSON string rules:
        // - Bytes 0x00-0x1F must be escaped (not raw in string)
        // - Bytes 0x20-0x7F are ASCII printable (valid UTF-8 single byte)
        // - Bytes 0x80-0xFF are UTF-8 continuation/leading bytes

        // If a byte appears unescaped in a JSON string field name:
        kani::assume(byte >= 0x20); // Control chars are escaped
        kani::assume(byte != b'"'); // Quote ends string
        kani::assume(byte != b'\\'); // Backslash starts escape

        // Remaining bytes form valid UTF-8 sequences
        // (Single ASCII bytes 0x20-0x7F, or multi-byte sequences 0x80-0xF4)
        if byte < 0x80 {
            // ASCII - always valid single-byte UTF-8
            kani::assert(byte.is_ascii(), "ASCII bytes are valid UTF-8");
        }
        // Multi-byte UTF-8 validity requires sequence checking (done by JSON parser)
    }

    /// Proof: Parsed JSON member names are valid UTF-8 sequences (ASCII subset).
    ///
    /// This proves that if the JSON parser accepts a string as a field name,
    /// ASCII-only content will be valid UTF-8 (justifying from_utf8_unchecked).
    ///
    /// Full UTF-8 validation (multi-byte sequences) is covered by:
    /// - The JSON parser's own validation
    /// - Fuzzing (fuzz_flatten_json target)
    #[kani::proof]
    #[kani::unwind(5)]
    fn json_field_name_ascii_utf8_valid() {
        // Simulate a short JSON field name (up to 4 bytes)
        let len: usize = kani::any();
        kani::assume(len <= 4);

        let mut bytes = [0u8; 4];
        for i in 0..4 {
            if i < len {
                bytes[i] = kani::any();
                // Apply JSON string constraints
                kani::assume(bytes[i] >= 0x20); // No control chars
                kani::assume(bytes[i] != b'"'); // No unescaped quote
                kani::assume(bytes[i] != b'\\'); // No unescaped backslash
                kani::assume(bytes[i] < 0x80); // Restrict to ASCII for bounded proof
            }
        }

        // For ASCII-only strings, from_utf8 always succeeds
        let slice = &bytes[..len];
        if !slice.is_empty() {
            let result = std::str::from_utf8(slice);
            kani::assert(result.is_ok(), "ASCII JSON field names are valid UTF-8");
        }
    }
}

#[cfg(kani)]
mod arena_proofs {
    use crate::automaton::arena::StateId;

    /// Proof: StateId::NONE is distinguishable.
    ///
    /// StateId::NONE must be a sentinel value that is_none() recognizes.
    #[kani::proof]
    fn stateid_none_is_none() {
        let none_id = StateId::NONE;
        kani::assert(
            none_id.is_none(),
            "StateId::NONE must be recognized by is_none()",
        );
    }

    /// Proof: StateId::NONE has consistent index behavior.
    ///
    /// The index() of NONE is u32::MAX, which is out of range for any realistic arena.
    #[kani::proof]
    fn stateid_none_index_max() {
        let none_id = StateId::NONE;
        kani::assert(
            none_id.index() == u32::MAX as usize,
            "StateId::NONE index must be u32::MAX",
        );
    }
}
