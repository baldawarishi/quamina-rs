//! NFA/DFA traversal functions.
//!
//! This module contains functions for traversing the finite automaton:
//! - `traverse_dfa`: Deterministic traversal for simple patterns
//! - `traverse_nfa`: Non-deterministic traversal for complex patterns (wildcards, etc.)

use std::sync::Arc;

use super::small_table::{FaState, FieldMatcher, NfaBuffers, SmallTable, StatePtr, VALUE_TERMINATOR};

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
