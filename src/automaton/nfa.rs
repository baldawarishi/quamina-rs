//! NFA/DFA traversal functions.
//!
//! This module contains functions for traversing the finite automaton:
//! - `traverse_dfa`: Deterministic traversal for simple patterns
//! - `traverse_nfa`: Non-deterministic traversal for complex patterns (wildcards, etc.)

use std::collections::HashSet;
use std::sync::Arc;

use super::small_table::{FaState, FieldMatcher, NfaBuffers, SmallTable, VALUE_TERMINATOR};

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
#[inline]
pub fn traverse_nfa(table: &SmallTable, val: &[u8], bufs: &mut NfaBuffers) {
    // Clear state buffers but NOT transitions (caller manages that)
    bufs.current_states.clear();
    bufs.next_states.clear();

    // Start with initial state
    let initial = Arc::new(FaState::with_table(table.clone()));
    bufs.current_states.push(initial);

    let mut seen_transitions: HashSet<*const FieldMatcher> = HashSet::new();

    for i in 0..=val.len() {
        if bufs.current_states.is_empty() {
            break;
        }

        let byte = if i < val.len() {
            val[i]
        } else {
            VALUE_TERMINATOR
        };

        for state in bufs.current_states.clone() {
            // Get epsilon closure
            let closure = get_epsilon_closure(&state);

            for ec_state in &closure {
                // Collect field transitions (deduplicated)
                for ft in &ec_state.field_transitions {
                    let ptr = Arc::as_ptr(ft);
                    if seen_transitions.insert(ptr) {
                        bufs.transitions.push(ft.clone());
                    }
                }

                // Check if this is a spinout state (wildcard)
                if ec_state.table.spinout.is_some() && byte != VALUE_TERMINATOR {
                    // Spinout: on any non-terminator byte, stay in spinout
                    // This creates a new state with the same table
                    let spinout_state = Arc::new(FaState::with_table(ec_state.table.clone()));
                    bufs.next_states.push(spinout_state);
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
    for state in &bufs.current_states {
        let closure = get_epsilon_closure(state);
        for ec_state in closure {
            for ft in &ec_state.field_transitions {
                let ptr = Arc::as_ptr(ft);
                if seen_transitions.insert(ptr) {
                    bufs.transitions.push(ft.clone());
                }
            }
        }
    }
}

/// Compute the epsilon closure of a state.
///
/// The epsilon closure is the set of states reachable via epsilon transitions.
fn get_epsilon_closure(state: &Arc<FaState>) -> Vec<Arc<FaState>> {
    let mut closure = vec![state.clone()];
    let mut stack = vec![state.clone()];

    while let Some(current) = stack.pop() {
        for eps in &current.table.epsilons {
            if !closure.iter().any(|s| Arc::ptr_eq(s, eps)) {
                closure.push(eps.clone());
                stack.push(eps.clone());
            }
        }
    }

    closure
}
