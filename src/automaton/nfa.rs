//! NFA/DFA traversal functions.
//!
//! This module contains functions for traversing the finite automaton:
//! - `traverse_dfa`: Deterministic traversal for simple patterns
//! - `traverse_nfa`: Non-deterministic traversal for complex patterns (wildcards, etc.)

use std::sync::Arc;

use super::small_table::{
    FaState, FieldMatcher, NfaBuffers, SmallTable, StatePtr, VALUE_TERMINATOR,
};

/// Traverse a DFA (deterministic finite automaton) on a value.
///
/// Appends the field matchers to the provided transitions Vec.
/// Like Go, accepts a Vec to append to for reduced allocations.
#[inline]
pub fn traverse_dfa(table: &SmallTable, val: &[u8], transitions: &mut Vec<Arc<FieldMatcher>>) {
    let mut current_table = table;

    for i in 0..=val.len() {
        let byte = if i < val.len() {
            val[i]
        } else {
            VALUE_TERMINATOR
        };

        match current_table.dstep(byte) {
            Some(next) => {
                transitions.extend(next.field_transitions.iter().cloned());
                current_table = &next.table;
            }
            None => break,
        }
    }
}

/// Traverse an NFA (nondeterministic finite automaton) on a value.
///
/// This handles epsilon transitions, multiple active states, and spinout (wildcard) states.
/// Appends transitions to bufs.transitions (assumes it's already cleared by caller).
///
/// # Epsilon Closure Optimization Notes
///
/// The epsilon closure computation uses FxHashSet for O(1) membership testing. This was changed
/// from O(n) Vec scanning (using `.any()`) to fix O(n²) worst-case behavior on patterns with
/// many epsilon transitions (regexp alternations, bounded quantifiers, lookarounds).
///
/// **Tradeoff**: This adds ~6% overhead on patterns with very small epsilon closures (1-2 states),
/// such as simple shellstyle patterns like `"A*"`. The HashSet's fixed overhead (~15-25 cycles
/// for hash + lookup) exceeds the cost of scanning 1-2 pointers (~3-6 cycles). However, for
/// patterns with larger epsilon closures (n > 4-5), the O(1) lookup is significantly faster.
///
/// **Potential optimization - hybrid approach**: A future optimization could use Vec scanning
/// for small closures and HashSet for larger ones:
/// ```ignore
/// let contained = if bufs.epsilon_closure.len() < 4 {
///     bufs.epsilon_closure.iter().any(|s| Arc::ptr_eq(s, eps))
/// } else {
///     !bufs.epsilon_seen.insert(StatePtr::new(Arc::as_ptr(eps)))
/// };
/// ```
/// This would recover the ~6% shellstyle regression while keeping O(1) behavior for complex
/// patterns. The threshold of 4 is approximate - the crossover point where HashSet becomes
/// faster depends on CPU cache behavior. This was deemed not worth the added code complexity
/// since shellstyle remains 1.6x faster than Go even with the overhead.
///
/// **Cleaner architectural fix**: Shellstyle patterns (e.g., `"A*"`, `"*foo*"`) create spinout
/// states with minimal epsilon transitions - they're essentially DFA-compatible. A cleaner fix
/// would be to detect shellstyle-only ValueMatchers at pattern-add time and route them through
/// `traverse_dfa` instead of `traverse_nfa`. This would require:
/// 1. A flag on ValueMatcher indicating DFA-compatibility (no epsilon transitions needed)
/// 2. Logic in the matching path to choose DFA vs NFA traversal
/// 3. Ensuring merged patterns (shellstyle + regexp on same field) fall back to NFA
///
/// This architectural change would eliminate NFA overhead entirely for pure shellstyle patterns,
/// but requires changes to pattern building and the ValueMatcher API. See Go quamina's approach
/// to spinout states in nfa.go for related design discussion.
#[inline]
pub fn traverse_nfa(table: &SmallTable, val: &[u8], bufs: &mut NfaBuffers) {
    // Clear state buffers but NOT transitions (caller manages that)
    bufs.current_states.clear();
    bufs.next_states.clear();
    bufs.seen_transitions.clear();

    // Start with initial state
    let initial = Arc::new(FaState::with_table(table.clone()));
    bufs.current_states.push(initial);

    for i in 0..=val.len() {
        if bufs.current_states.is_empty() {
            break;
        }

        let byte = if i < val.len() {
            val[i]
        } else {
            VALUE_TERMINATOR
        };

        // Process each current state - avoid clone by iterating with index
        let num_current = bufs.current_states.len();
        for state_idx in 0..num_current {
            let state = bufs.current_states[state_idx].clone();

            // Compute epsilon closure inline using buffers
            bufs.epsilon_closure.clear();
            bufs.epsilon_stack.clear();
            bufs.epsilon_seen.clear();
            bufs.epsilon_seen.insert(StatePtr::new(Arc::as_ptr(&state)));
            bufs.epsilon_closure.push(state.clone());
            bufs.epsilon_stack.push(state);

            while let Some(current) = bufs.epsilon_stack.pop() {
                for eps in &current.table.epsilons {
                    // O(1) membership check instead of O(n)
                    if bufs.epsilon_seen.insert(StatePtr::new(Arc::as_ptr(eps))) {
                        bufs.epsilon_closure.push(eps.clone());
                        bufs.epsilon_stack.push(eps.clone());
                    }
                }
            }

            // Process each state in the epsilon closure
            for ec_state in &bufs.epsilon_closure {
                // Collect field transitions (deduplicated using sorted vec)
                for ft in &ec_state.field_transitions {
                    let ptr = Arc::as_ptr(ft) as usize;
                    // Binary search for insertion point
                    match bufs.seen_transitions.binary_search(&ptr) {
                        Ok(_) => {} // Already seen
                        Err(pos) => {
                            bufs.seen_transitions.insert(pos, ptr);
                            bufs.transitions.push(ft.clone());
                        }
                    }
                }

                // Check if this is a spinout state (wildcard)
                if ec_state.table.spinout.is_some() && byte != VALUE_TERMINATOR {
                    // Spinout: on any non-terminator byte, stay in spinout
                    // Reuse the existing Arc instead of allocating a new one
                    bufs.next_states.push(ec_state.clone());
                }

                // Take step on current byte
                let (next, _epsilons) = ec_state.table.step(byte);
                if let Some(next) = next {
                    bufs.next_states.push(next.clone());
                }
            }
        }

        // Swap buffers
        std::mem::swap(&mut bufs.current_states, &mut bufs.next_states);
        bufs.next_states.clear();
    }

    // Check final states for matches
    let final_states = std::mem::take(&mut bufs.current_states);
    for state in &final_states {
        bufs.epsilon_closure.clear();
        bufs.epsilon_stack.clear();
        bufs.epsilon_seen.clear();
        bufs.epsilon_seen.insert(StatePtr::new(Arc::as_ptr(state)));
        bufs.epsilon_closure.push(state.clone());
        bufs.epsilon_stack.push(state.clone());

        while let Some(current) = bufs.epsilon_stack.pop() {
            for eps in &current.table.epsilons {
                // O(1) membership check instead of O(n)
                if bufs.epsilon_seen.insert(StatePtr::new(Arc::as_ptr(eps))) {
                    bufs.epsilon_closure.push(eps.clone());
                    bufs.epsilon_stack.push(eps.clone());
                }
            }
        }

        for ec_state in &bufs.epsilon_closure {
            for ft in &ec_state.field_transitions {
                let ptr = Arc::as_ptr(ft) as usize;
                match bufs.seen_transitions.binary_search(&ptr) {
                    Ok(_) => {}
                    Err(pos) => {
                        bufs.seen_transitions.insert(pos, ptr);
                        bufs.transitions.push(ft.clone());
                    }
                }
            }
        }
    }
    bufs.current_states = final_states;
}
