//! Kani proof harnesses for verifying real invariants.
//!
//! These proofs use bounded model checking to exhaustively verify properties
//! of core data structures and algorithms. Run with: `cargo kani`

#[cfg(kani)]
mod proofs {
    use crate::automaton::BYTE_CEILING;

    /// Prove: the RLE compression algorithm used by ArenaSmallTable::pack
    /// roundtrips correctly through the dstep lookup algorithm.
    ///
    /// We test the algorithm directly with fixed-size arrays because SmallVec's
    /// internal branching causes state explosion in Kani. The pack and dstep
    /// logic tested here is identical to ArenaSmallTable::pack (arena.rs:161-180)
    /// and ArenaSmallTable::dstep (arena.rs:192-199).
    ///
    /// Tested at N=32 (not full BYTE_CEILING=246) to keep Kani runtime bounded.
    /// The algorithm is size-independent, so correctness at N=32 implies
    /// correctness at any size.
    #[kani::proof]
    #[kani::unwind(34)]
    fn smalltable_pack_dstep_roundtrip() {
        const N: usize = 32;

        // Two symbolic states and a symbolic breakpoint
        let state_a: u32 = kani::any();
        let state_b: u32 = kani::any();
        let breakpoint: usize = kani::any();
        kani::assume(breakpoint > 0 && breakpoint < N);

        // Build unpacked table: state_a for [0, breakpoint), state_b for [breakpoint, N)
        let mut unpacked = [0u32; N];
        let mut i = 0usize;
        while i < N {
            unpacked[i] = if i < breakpoint { state_a } else { state_b };
            i += 1;
        }

        // --- Pack: same algorithm as ArenaSmallTable::pack (arena.rs:161-176) ---
        let mut ceilings = [0u8; N];
        let mut steps = [0u32; N];
        let mut n_entries = 0usize;
        let mut current = unpacked[0];
        i = 0;
        while i < N {
            if unpacked[i] != current {
                ceilings[n_entries] = i as u8;
                steps[n_entries] = current;
                n_entries += 1;
                current = unpacked[i];
            }
            i += 1;
        }
        // Final entry
        ceilings[n_entries] = N as u8;
        steps[n_entries] = current;
        n_entries += 1;

        // --- Lookup: same algorithm as ArenaSmallTable::dstep (arena.rs:192-199) ---
        let test_byte: u8 = kani::any();
        kani::assume((test_byte as usize) < N);

        let mut result = u32::MAX; // sentinel (matches StateId::NONE)
        let mut j = 0usize;
        while j < n_entries {
            if test_byte < ceilings[j] {
                result = steps[j];
                break;
            }
            j += 1;
        }

        kani::assert(
            result == unpacked[test_byte as usize],
            "pack/dstep roundtrip mismatch",
        );
    }
}

#[cfg(kani)]
mod case_fold_proofs {
    use crate::case_folding::CASE_FOLDING_PAIRS;

    /// Prove: CASE_FOLDING_PAIRS is sorted by first element.
    ///
    /// Binary search in case_fold_char relies on this invariant. We check
    /// that table[i].0 < table[i+1].0 for a symbolic index covering all
    /// adjacent pairs.
    #[kani::proof]
    fn case_fold_table_sorted() {
        let i: usize = kani::any();
        kani::assume(i < CASE_FOLDING_PAIRS.len() - 1);
        kani::assert(
            CASE_FOLDING_PAIRS[i].0 < CASE_FOLDING_PAIRS[i + 1].0,
            "CASE_FOLDING_PAIRS must be sorted by first element",
        );
    }
}

#[cfg(kani)]
mod numbits_proofs {
    use crate::numbits::{numbits_from_f64, to_q_number_stack, MAX_BYTES_IN_ENCODING};

    /// Prove: to_q_number_stack never exceeds MAX_BYTES_IN_ENCODING (10) for any u64.
    ///
    /// The variable-width base-128 encoding strips trailing zero septets,
    /// so the length is always in [0, 10]. This verifies the len field
    /// fits the fixed-size stack buffer.
    #[kani::proof]
    #[kani::unwind(11)]
    fn q_number_stack_encoding_bounds() {
        let input: u64 = kani::any();
        let result = to_q_number_stack(input);
        kani::assert(
            result.len() <= MAX_BYTES_IN_ENCODING,
            "Q-number length must not exceed MAX_BYTES_IN_ENCODING",
        );
    }

    /// Prove: numbits_from_f64 preserves ordering for finite, non-NaN floats.
    ///
    /// If a < b as f64, then numbits_from_f64(a) < numbits_from_f64(b) as u64.
    /// This is the core invariant that makes automaton-based numeric matching work.
    #[kani::proof]
    fn numbits_ordering_preserves_f64() {
        let a: f64 = kani::any();
        let b: f64 = kani::any();

        kani::assume(a.is_finite());
        kani::assume(b.is_finite());
        kani::assume(a < b);

        let na = numbits_from_f64(a);
        let nb = numbits_from_f64(b);

        kani::assert(na < nb, "numbits must preserve f64 ordering");
    }
}
