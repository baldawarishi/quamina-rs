//! Kani proof harnesses for verifying real invariants.
//!
//! These proofs use bounded model checking to exhaustively verify properties
//! of core data structures and algorithms. Run with: `cargo kani`
//!
//! ArenaSmallTable pack/dstep roundtrip is verified in
//! src/automaton/arena.rs::kani_arena_proofs::smalltable_pack_dstep_roundtrip
//! (bounded to 3-region tables to keep the proof tractable).

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
    use crate::numbits::{MAX_BYTES_IN_ENCODING, numbits_from_f64, to_q_number_stack};

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
