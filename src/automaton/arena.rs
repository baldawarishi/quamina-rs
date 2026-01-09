//! Arena-based state allocation for cyclic NFA structures.
//!
//! This module provides an index-based state arena that allows true cyclic
//! references in NFA structures. This is necessary for efficient implementation
//! of `*` and `+` regexp quantifiers.
//!
//! The key insight: `StateId` is just an index (u32), so multiple states can
//! reference each other cyclically without ownership issues.
//!
//! ## Example: Implementing [a-z]* with cycles
//!
//! With Arc (current approach - 100 chained states):
//! ```text
//! S₁ → S₂ → S₃ → ... → S₁₀₀ → exit
//! ```
//!
//! With arena (2 states with true cycle):
//! ```text
//! loop_state ←→ loopback → exit
//!      ↓              ↓
//!   [a-z]          epsilon
//! ```

use std::sync::Arc;

use super::small_table::{FieldMatcher, BYTE_CEILING};

/// A state identifier - just an index into the arena.
///
/// This can be freely copied and allows cyclic references.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StateId(u32);

impl StateId {
    /// Special sentinel value for "no state" / null reference.
    pub const NONE: StateId = StateId(u32::MAX);

    #[inline]
    pub fn is_none(self) -> bool {
        self.0 == u32::MAX
    }

    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

/// A state in the arena-based finite automaton.
#[derive(Clone, Default)]
pub struct ArenaFaState {
    /// The transition table for this state
    pub table: ArenaSmallTable,
    /// Field matchers to transition to when this state is reached at end of value
    pub field_transitions: Vec<Arc<FieldMatcher>>,
}

impl std::fmt::Debug for ArenaFaState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ArenaFaState")
            .field("table", &self.table)
            .field("field_transitions_count", &self.field_transitions.len())
            .finish()
    }
}

impl ArenaFaState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_table(table: ArenaSmallTable) -> Self {
        Self {
            table,
            field_transitions: Vec::new(),
        }
    }
}

/// A compact lookup table using arena-based state references.
#[derive(Clone, Debug)]
pub struct ArenaSmallTable {
    /// Upper bounds (exclusive) for each byte range
    pub ceilings: Vec<u8>,
    /// State IDs to transition to for each range (StateId::NONE = no transition)
    pub steps: Vec<StateId>,
    /// Epsilon transitions (taken regardless of input byte)
    pub epsilons: Vec<StateId>,
    /// Special state for handling wildcard patterns
    pub spinout: StateId,
}

impl Default for ArenaSmallTable {
    fn default() -> Self {
        Self::new()
    }
}

impl ArenaSmallTable {
    /// Create a new empty table.
    pub fn new() -> Self {
        Self {
            ceilings: vec![BYTE_CEILING as u8],
            steps: vec![StateId::NONE],
            epsilons: Vec::new(),
            spinout: StateId::NONE,
        }
    }

    /// Create a table with specific byte mappings.
    pub fn with_mappings(default: StateId, bytes: &[u8], targets: &[StateId]) -> Self {
        let mut unpacked = [StateId::NONE; BYTE_CEILING];

        // Set default for all
        if !default.is_none() {
            for slot in unpacked.iter_mut() {
                *slot = default;
            }
        }

        // Set specific mappings
        for (b, t) in bytes.iter().zip(targets.iter()) {
            unpacked[*b as usize] = *t;
        }

        let mut table = Self::new();
        table.pack(&unpacked);
        table
    }

    /// Pack an unpacked table (256 entries) into the compressed format.
    pub fn pack(&mut self, unpacked: &[StateId; BYTE_CEILING]) {
        self.ceilings.clear();
        self.steps.clear();

        let mut current = unpacked[0];
        for (i, &state_id) in unpacked.iter().enumerate() {
            if state_id != current {
                self.ceilings.push(i as u8);
                self.steps.push(current);
                current = state_id;
            }
        }

        // Final entry
        self.ceilings.push(BYTE_CEILING as u8);
        self.steps.push(current);
    }

    /// Get the state for a given byte (deterministic step).
    #[inline]
    pub fn dstep(&self, byte: u8) -> StateId {
        for (i, &ceiling) in self.ceilings.iter().enumerate() {
            if byte < ceiling {
                return self.steps[i];
            }
        }
        StateId::NONE
    }

    /// Get the state and epsilons for a given byte.
    #[inline]
    pub fn step(&self, byte: u8) -> (StateId, &[StateId]) {
        (self.dstep(byte), &self.epsilons)
    }
}

/// Arena for allocating NFA states.
///
/// States are allocated contiguously and referenced by `StateId`.
/// The arena owns all state memory and frees it when dropped.
#[derive(Clone, Default)]
pub struct StateArena {
    states: Vec<ArenaFaState>,
}

impl std::fmt::Debug for StateArena {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StateArena")
            .field("states_count", &self.states.len())
            .finish()
    }
}

impl StateArena {
    pub fn new() -> Self {
        Self { states: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            states: Vec::with_capacity(capacity),
        }
    }

    /// Allocate a new default state, returning its ID.
    pub fn alloc(&mut self) -> StateId {
        let id = StateId(self.states.len() as u32);
        self.states.push(ArenaFaState::default());
        id
    }

    /// Allocate a new state with the given table, returning its ID.
    pub fn alloc_with_table(&mut self, table: ArenaSmallTable) -> StateId {
        let id = StateId(self.states.len() as u32);
        self.states.push(ArenaFaState::with_table(table));
        id
    }

    /// Get a reference to a state by ID.
    #[inline]
    pub fn get(&self, id: StateId) -> Option<&ArenaFaState> {
        if id.is_none() {
            None
        } else {
            self.states.get(id.index())
        }
    }

    /// Get a mutable reference to a state by ID.
    #[inline]
    pub fn get_mut(&mut self, id: StateId) -> Option<&mut ArenaFaState> {
        if id.is_none() {
            None
        } else {
            self.states.get_mut(id.index())
        }
    }

    /// Number of states in the arena.
    pub fn len(&self) -> usize {
        self.states.len()
    }

    /// Check if arena is empty.
    pub fn is_empty(&self) -> bool {
        self.states.is_empty()
    }
}

impl std::ops::Index<StateId> for StateArena {
    type Output = ArenaFaState;

    #[inline]
    fn index(&self, id: StateId) -> &Self::Output {
        &self.states[id.index()]
    }
}

impl std::ops::IndexMut<StateId> for StateArena {
    #[inline]
    fn index_mut(&mut self, id: StateId) -> &mut Self::Output {
        &mut self.states[id.index()]
    }
}

/// Buffers for arena NFA traversal (avoid allocation during matching).
#[derive(Default)]
pub struct ArenaNfaBuffers {
    /// Current active states
    pub current_states: Vec<StateId>,
    /// Next states after transition
    pub next_states: Vec<StateId>,
    /// Accumulated field matcher transitions
    pub transitions: Vec<Arc<FieldMatcher>>,
    /// Seen state IDs (for epsilon closure deduplication)
    seen_states: Vec<bool>,
    /// Closure buffer
    closure_stack: Vec<StateId>,
    closure_result: Vec<StateId>,
}

impl ArenaNfaBuffers {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(state_capacity: usize) -> Self {
        Self {
            current_states: Vec::with_capacity(16),
            next_states: Vec::with_capacity(16),
            transitions: Vec::new(),
            seen_states: vec![false; state_capacity],
            closure_stack: Vec::with_capacity(16),
            closure_result: Vec::with_capacity(16),
        }
    }

    pub fn clear(&mut self) {
        self.current_states.clear();
        self.next_states.clear();
        self.transitions.clear();
        // Note: seen_states is reset during epsilon closure
    }

    /// Ensure seen_states buffer is large enough for the arena.
    fn ensure_seen_capacity(&mut self, arena_size: usize) {
        if self.seen_states.len() < arena_size {
            self.seen_states.resize(arena_size, false);
        }
    }
}

/// Value terminator (same as in small_table)
pub const ARENA_VALUE_TERMINATOR: u8 = 0xF5;

/// Traverse an arena-based NFA on a value.
///
/// This is the arena equivalent of `traverse_nfa` but uses index-based
/// state references, allowing true cyclic structures.
#[inline]
pub fn traverse_arena_nfa(
    arena: &StateArena,
    start: StateId,
    val: &[u8],
    bufs: &mut ArenaNfaBuffers,
) {
    bufs.clear();
    bufs.ensure_seen_capacity(arena.len());

    if start.is_none() {
        return;
    }

    bufs.current_states.push(start);

    // Track seen field matchers by pointer
    let mut seen_transitions: std::collections::HashSet<*const FieldMatcher> =
        std::collections::HashSet::new();

    for i in 0..=val.len() {
        if bufs.current_states.is_empty() {
            break;
        }

        let byte = if i < val.len() {
            val[i]
        } else {
            ARENA_VALUE_TERMINATOR
        };

        for &state_id in bufs.current_states.clone().iter() {
            // Get epsilon closure
            let closure = get_arena_epsilon_closure(arena, state_id, bufs);

            for &ec_state_id in &closure {
                let ec_state = &arena[ec_state_id];

                // Collect field transitions (deduplicated)
                for ft in &ec_state.field_transitions {
                    let ptr = Arc::as_ptr(ft);
                    if seen_transitions.insert(ptr) {
                        bufs.transitions.push(ft.clone());
                    }
                }

                // Check spinout (wildcard)
                if !ec_state.table.spinout.is_none() && byte != ARENA_VALUE_TERMINATOR {
                    // For spinout, stay in same state
                    bufs.next_states.push(ec_state_id);
                }

                // Take step on current byte
                let next = ec_state.table.dstep(byte);
                if !next.is_none() {
                    bufs.next_states.push(next);
                }
            }
        }

        // Swap buffers
        std::mem::swap(&mut bufs.current_states, &mut bufs.next_states);
        bufs.next_states.clear();
    }

    // Check final states for matches
    for &state_id in bufs.current_states.clone().iter() {
        let closure = get_arena_epsilon_closure(arena, state_id, bufs);
        for &ec_state_id in &closure {
            let ec_state = &arena[ec_state_id];
            for ft in &ec_state.field_transitions {
                let ptr = Arc::as_ptr(ft);
                if seen_transitions.insert(ptr) {
                    bufs.transitions.push(ft.clone());
                }
            }
        }
    }
}

/// Compute the epsilon closure of a state in the arena.
fn get_arena_epsilon_closure(
    arena: &StateArena,
    start: StateId,
    bufs: &mut ArenaNfaBuffers,
) -> Vec<StateId> {
    // Reset seen markers for states we'll visit
    bufs.closure_result.clear();
    bufs.closure_stack.clear();

    bufs.closure_result.push(start);
    bufs.closure_stack.push(start);

    // Mark start as seen
    if !start.is_none() && start.index() < bufs.seen_states.len() {
        bufs.seen_states[start.index()] = true;
    }

    while let Some(current_id) = bufs.closure_stack.pop() {
        if current_id.is_none() {
            continue;
        }

        let state = &arena[current_id];
        for &eps_id in &state.table.epsilons {
            if eps_id.is_none() {
                continue;
            }

            let idx = eps_id.index();
            if idx < bufs.seen_states.len() && !bufs.seen_states[idx] {
                bufs.seen_states[idx] = true;
                bufs.closure_result.push(eps_id);
                bufs.closure_stack.push(eps_id);
            }
        }
    }

    // Clear seen markers for next use
    for &id in &bufs.closure_result {
        if !id.is_none() && id.index() < bufs.seen_states.len() {
            bufs.seen_states[id.index()] = false;
        }
    }

    bufs.closure_result.clone()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_id_none() {
        assert!(StateId::NONE.is_none());
        assert!(!StateId(0).is_none());
        assert!(!StateId(100).is_none());
    }

    #[test]
    fn test_arena_alloc() {
        let mut arena = StateArena::new();
        let id1 = arena.alloc();
        let id2 = arena.alloc();

        assert_eq!(id1.index(), 0);
        assert_eq!(id2.index(), 1);
        assert_eq!(arena.len(), 2);
    }

    #[test]
    fn test_arena_cyclic_reference() {
        let mut arena = StateArena::new();

        // Create two states that reference each other (cycle!)
        let state_a = arena.alloc();
        let state_b = arena.alloc();

        // A has epsilon to B
        arena[state_a].table.epsilons.push(state_b);
        // B has epsilon to A (cycle!)
        arena[state_b].table.epsilons.push(state_a);

        // Verify the cycle exists
        assert_eq!(arena[state_a].table.epsilons[0], state_b);
        assert_eq!(arena[state_b].table.epsilons[0], state_a);
    }

    #[test]
    fn test_arena_small_table_pack() {
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];

        // Set 'a' (97) to state 0
        unpacked[b'a' as usize] = StateId(0);
        // Set 'b' (98) to state 1
        unpacked[b'b' as usize] = StateId(1);

        table.pack(&unpacked);

        assert_eq!(table.dstep(b'a'), StateId(0));
        assert_eq!(table.dstep(b'b'), StateId(1));
        assert!(table.dstep(b'c').is_none());
    }

    #[test]
    fn test_traverse_arena_nfa_simple() {
        // Test a simple NFA that matches "a" followed by VALUE_TERMINATOR
        let mut arena = StateArena::new();
        let field_matcher = Arc::new(FieldMatcher::new());

        // Create states:
        // start --(a)--> match_state --(VT)--> final
        let final_state = arena.alloc();
        arena[final_state]
            .field_transitions
            .push(field_matcher.clone());

        let match_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            b"a",
            &[match_state],
        ));

        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

        // Should match "a"
        let value = b"a";
        traverse_arena_nfa(&arena, start, value, &mut bufs);
        assert_eq!(bufs.transitions.len(), 1);
        assert!(Arc::ptr_eq(&bufs.transitions[0], &field_matcher));

        // Should NOT match "b"
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"b", &mut bufs);
        assert!(bufs.transitions.is_empty());
    }

    #[test]
    fn test_traverse_arena_nfa_star_cyclic() {
        // Test [ab]* - matches zero or more 'a' or 'b' characters
        // This uses a TRUE CYCLIC structure (unlike the chain-based Arc approach)
        //
        // Structure:
        //   start --epsilon--> exit (for zero matches)
        //   start --(a/b)--> loopback --epsilon--> start (cycle!)
        //                            --epsilon--> exit
        let mut arena = StateArena::new();
        let field_matcher = Arc::new(FieldMatcher::new());

        // exit state (has VALUE_TERMINATOR transition to final)
        let final_state = arena.alloc();
        arena[final_state]
            .field_transitions
            .push(field_matcher.clone());

        let exit_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        // loopback state (placeholder, we'll fill in epsilons after allocating start)
        let loopback = arena.alloc();

        // start state - matches 'a' or 'b' -> loopback
        let start = arena.alloc_with_table({
            let mut table =
                ArenaSmallTable::with_mappings(StateId::NONE, b"ab", &[loopback, loopback]);
            // For *, add epsilon to exit (can match zero times)
            table.epsilons.push(exit_state);
            table
        });

        // Now set up loopback's epsilons: to exit AND back to start (CYCLE!)
        arena[loopback].table.epsilons = vec![exit_state, start];

        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

        // Should match empty string (zero times)
        traverse_arena_nfa(&arena, start, b"", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]* should match empty string");

        // Should match "a"
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"a", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]* should match 'a'");

        // Should match "ab"
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"ab", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]* should match 'ab'");

        // Should match "aaa"
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"aaa", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]* should match 'aaa'");

        // Should match "abba"
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"abba", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]* should match 'abba'");

        // Should match very long string (tests that cycles work efficiently)
        bufs.clear();
        let long_value = "ab".repeat(100);
        traverse_arena_nfa(&arena, start, long_value.as_bytes(), &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]* should match long string");

        // Should NOT match "c" (not in [ab])
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"c", &mut bufs);
        assert!(bufs.transitions.is_empty(), "[ab]* should NOT match 'c'");
    }

    #[test]
    fn test_traverse_arena_nfa_plus_cyclic() {
        // Test [ab]+ - matches one or more 'a' or 'b' characters
        // Unlike *, this requires at least one match
        //
        // Structure:
        //   start --(a/b)--> loopback --epsilon--> start (cycle!)
        //                            --epsilon--> exit
        let mut arena = StateArena::new();
        let field_matcher = Arc::new(FieldMatcher::new());

        // exit state
        let final_state = arena.alloc();
        arena[final_state]
            .field_transitions
            .push(field_matcher.clone());

        let exit_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        // loopback state (placeholder)
        let loopback = arena.alloc();

        // start state - matches 'a' or 'b' -> loopback
        // NO epsilon to exit (must match at least once for +)
        let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            b"ab",
            &[loopback, loopback],
        ));

        // Set up loopback's epsilons: to exit AND back to start (CYCLE!)
        arena[loopback].table.epsilons = vec![exit_state, start];

        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

        // Should NOT match empty string (+ requires at least one)
        traverse_arena_nfa(&arena, start, b"", &mut bufs);
        assert!(
            bufs.transitions.is_empty(),
            "[ab]+ should NOT match empty string"
        );

        // Should match "a"
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"a", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]+ should match 'a'");

        // Should match "ab"
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"ab", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]+ should match 'ab'");

        // Should match very long string
        bufs.clear();
        let long_value = "ab".repeat(100);
        traverse_arena_nfa(&arena, start, long_value.as_bytes(), &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "[ab]+ should match long string");
    }

    #[test]
    fn test_arena_state_count_vs_chain() {
        // Demonstrate that arena approach uses O(1) states for [a]* vs O(depth) for chain
        let mut arena = StateArena::new();
        let field_matcher = Arc::new(FieldMatcher::new());

        // Build [a]* with true cycle - only needs ~4 states
        let final_state = arena.alloc();
        arena[final_state].field_transitions.push(field_matcher);

        let exit_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        let loopback = arena.alloc();

        let start = arena.alloc_with_table({
            let mut table = ArenaSmallTable::with_mappings(StateId::NONE, b"a", &[loopback]);
            table.epsilons.push(exit_state);
            table
        });

        arena[loopback].table.epsilons = vec![exit_state, start];

        // Only 4 states needed for [a]* with arena!
        // The chain-based approach needs 100+ states for the same pattern.
        assert_eq!(arena.len(), 4, "Arena [a]* should only need 4 states");

        // Verify it works
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(&arena, start, b"aaaaaaaaaa", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1);
    }
}
