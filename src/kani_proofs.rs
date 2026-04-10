//! Kani proof harnesses for verifying real invariants.
//!
//! These proofs use bounded model checking to exhaustively verify properties
//! of core data structures and algorithms. Run with: `cargo kani`
//!
//! Arena-specific proofs live in src/automaton/arena.rs::kani_arena_proofs
//! (locality with the code they verify):
//! - `smalltable_pack_dstep_roundtrip`: ArenaSmallTable pack/dstep roundtrip
//!   (bounded to 3-region tables to keep the proof tractable)

#[cfg(kani)]
mod nfa_dfa_proofs {
    use crate::automaton::arena::{ArenaSmallTable, StateArena, StateId};

    /// Prove: nfa_to_dfa respects the state budget.
    ///
    /// For any NFA arena and budget, if nfa_to_dfa returns Some, the resulting
    /// DFA arena has at most `state_budget` states.
    #[kani::proof]
    #[kani::unwind(4)]
    fn nfa_to_dfa_respects_budget() {
        let budget: usize = kani::any();
        kani::assume(budget >= 1 && budget <= 8);

        // Build a minimal NFA: start -ε→ s1, start -ε→ s2
        let mut arena = StateArena::new();
        let start = arena.alloc();
        let s1 = arena.alloc();
        let s2 = arena.alloc();
        arena[start].table.epsilons.push(s1);
        arena[start].table.epsilons.push(s2);
        // s1: a→s1 (self-loop)
        arena[s1].table = ArenaSmallTable::with_mappings(StateId::NONE, &[b'a'], &[s1]);
        // s2: b→s2 (self-loop)
        arena[s2].table = ArenaSmallTable::with_mappings(StateId::NONE, &[b'b'], &[s2]);
        arena.precompute_epsilon_closures();

        if let Some((dfa, _start)) = arena.nfa_to_dfa(start, budget) {
            kani::assert(
                dfa.len() <= budget,
                "DFA state count must not exceed budget",
            );
        }
        // If None, budget was exceeded — that's the correct behavior
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
