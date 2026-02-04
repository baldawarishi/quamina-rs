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

use super::small_table::{AccelInfo, FieldMatcher, BYTE_CEILING};

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
    /// Acceleration info for self-loop states (exit bytes for memchr skip)
    pub accel: Option<AccelInfo>,
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
            accel: None,
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

/// Try to accelerate through a self-loop state using memchr.
///
/// When a state has acceleration info (1-3 exit bytes), we can use
/// SIMD-optimized memchr to skip directly to the next exit byte instead of
/// processing byte-by-byte.
///
/// Returns Some(skip) if acceleration found an exit byte at position `skip`,
/// or None if acceleration is not applicable.
///
/// Used for ASCII-only negated patterns like `[^x]+` where the exit bytes
/// are just the negated ASCII characters (not all invalid UTF-8 bytes).
#[inline]
fn try_accelerate_arena(table: &ArenaSmallTable, remaining: &[u8]) -> Option<usize> {
    let accel = table.accel.as_ref()?;

    match accel.len {
        1 => memchr::memchr(accel.exit_bytes[0], remaining),
        2 => memchr::memchr2(accel.exit_bytes[0], accel.exit_bytes[1], remaining),
        3 => memchr::memchr3(
            accel.exit_bytes[0],
            accel.exit_bytes[1],
            accel.exit_bytes[2],
            remaining,
        ),
        _ => None,
    }
}

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

    let len = val.len();
    let mut i = 0;

    while i <= len {
        if bufs.current_states.is_empty() {
            break;
        }

        // State acceleration: For ASCII-only negated patterns like [^x]+, use memchr
        // to skip directly to exit bytes. This is enabled when patterns have 1-3 exit bytes.
        //
        // Note: Generic Unicode patterns still have too many exit bytes due to UTF-8 validation,
        // but ASCII-only negated patterns (detected at parse time) work well because JSON input
        // is valid UTF-8 and doesn't need re-validation during matching.
        if i < len && bufs.current_states.len() == 1 {
            let state_id = bufs.current_states[0];
            let state = &arena[state_id];
            if let Some(skip) = try_accelerate_arena(&state.table, &val[i..]) {
                if skip > 0 {
                    i += skip;
                    continue;
                }
            }
        }

        let byte = if i < len {
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
        i += 1;
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

/// Merge two arena-based DFAs into one that matches either pattern.
///
/// This is the arena equivalent of `merge_fas` for chain-based FAs.
/// For DFA-only patterns (no epsilons/spinouts), this is a simplified merge
/// that recursively merges overlapping byte transitions.
///
/// # Arguments
/// * `arena1` - First arena
/// * `start1` - Start state in first arena (StateId::NONE if empty)
/// * `arena2` - Second arena
/// * `start2` - Start state in second arena (StateId::NONE if empty)
///
/// # Returns
/// A new arena containing the merged DFA and its start state
pub fn merge_arena_dfas(
    arena1: &StateArena,
    start1: StateId,
    arena2: &StateArena,
    start2: StateId,
) -> (StateArena, StateId) {
    use std::collections::HashMap;

    // Handle empty cases
    if start1.is_none() && start2.is_none() {
        return (StateArena::new(), StateId::NONE);
    }

    if start1.is_none() {
        // Clone arena2
        return clone_arena_subset(arena2, start2);
    }

    if start2.is_none() {
        // Clone arena1
        return clone_arena_subset(arena1, start1);
    }

    // Memoization: (state1_id, state2_id) -> merged_state_id in new arena
    // Use i32 to handle StateId::NONE as -1
    type MemoKey = (i32, i32);
    let mut memo: HashMap<MemoKey, StateId> = HashMap::new();
    let mut new_arena = StateArena::new();

    let start =
        merge_arena_states_recursive(arena1, start1, arena2, start2, &mut new_arena, &mut memo);

    (new_arena, start)
}

/// Clone a subset of an arena starting from a given state.
fn clone_arena_subset(arena: &StateArena, start: StateId) -> (StateArena, StateId) {
    use std::collections::HashMap;

    if start.is_none() {
        return (StateArena::new(), StateId::NONE);
    }

    let mut new_arena = StateArena::new();
    let mut id_map: HashMap<u32, StateId> = HashMap::new();

    clone_state_recursive(arena, start, &mut new_arena, &mut id_map);

    let new_start = id_map.get(&start.0).copied().unwrap_or(StateId::NONE);
    (new_arena, new_start)
}

/// Recursively clone a state and its descendants.
fn clone_state_recursive(
    arena: &StateArena,
    state_id: StateId,
    new_arena: &mut StateArena,
    id_map: &mut std::collections::HashMap<u32, StateId>,
) -> StateId {
    if state_id.is_none() {
        return StateId::NONE;
    }

    // Check if already cloned
    if let Some(&new_id) = id_map.get(&state_id.0) {
        return new_id;
    }

    // Allocate new state first (to handle cycles)
    let new_id = new_arena.alloc();
    id_map.insert(state_id.0, new_id);

    let old_state = &arena[state_id];

    // Clone field transitions (Arc clone is cheap)
    new_arena[new_id].field_transitions = old_state.field_transitions.clone();

    // Clone table with remapped state IDs
    let old_table = &old_state.table;
    let mut new_table = ArenaSmallTable {
        ceilings: old_table.ceilings.clone(),
        steps: Vec::with_capacity(old_table.steps.len()),
        epsilons: Vec::with_capacity(old_table.epsilons.len()),
        spinout: StateId::NONE,
        accel: old_table.accel.clone(),
    };

    // Remap steps
    for &step_id in &old_table.steps {
        let new_step = clone_state_recursive(arena, step_id, new_arena, id_map);
        new_table.steps.push(new_step);
    }

    // Remap epsilons
    for &eps_id in &old_table.epsilons {
        let new_eps = clone_state_recursive(arena, eps_id, new_arena, id_map);
        new_table.epsilons.push(new_eps);
    }

    // Remap spinout
    if !old_table.spinout.is_none() {
        new_table.spinout = clone_state_recursive(arena, old_table.spinout, new_arena, id_map);
    }

    new_arena[new_id].table = new_table;

    new_id
}

/// Recursively merge two states from different arenas.
fn merge_arena_states_recursive(
    arena1: &StateArena,
    state1: StateId,
    arena2: &StateArena,
    state2: StateId,
    new_arena: &mut StateArena,
    memo: &mut std::collections::HashMap<(i32, i32), StateId>,
) -> StateId {
    // Convert to memo key (using -1 for NONE)
    let key1 = if state1.is_none() {
        -1
    } else {
        state1.0 as i32
    };
    let key2 = if state2.is_none() {
        -1
    } else {
        state2.0 as i32
    };
    let key = (key1, key2);

    // Check memo
    if let Some(&cached) = memo.get(&key) {
        return cached;
    }

    // Handle one-sided cases
    if state1.is_none() && state2.is_none() {
        return StateId::NONE;
    }

    // Allocate new state first (before recursion, to handle cycles)
    let new_id = new_arena.alloc();
    memo.insert(key, new_id);

    if state1.is_none() {
        // Copy from arena2
        let s2 = &arena2[state2];
        new_arena[new_id].field_transitions = s2.field_transitions.clone();
        new_arena[new_id].table =
            remap_table_recursive(arena2, &s2.table, arena1, new_arena, memo, false);
        return new_id;
    }

    if state2.is_none() {
        // Copy from arena1
        let s1 = &arena1[state1];
        new_arena[new_id].field_transitions = s1.field_transitions.clone();
        new_arena[new_id].table =
            remap_table_recursive(arena1, &s1.table, arena2, new_arena, memo, true);
        return new_id;
    }

    // Both states exist - merge them
    let s1 = &arena1[state1];
    let s2 = &arena2[state2];

    // Combine field transitions
    let mut field_transitions = s1.field_transitions.clone();
    field_transitions.extend(s2.field_transitions.iter().cloned());
    new_arena[new_id].field_transitions = field_transitions;

    // Merge tables byte-by-byte
    new_arena[new_id].table =
        merge_arena_tables(arena1, &s1.table, arena2, &s2.table, new_arena, memo);

    new_id
}

/// Remap a table from one arena to the merged arena.
fn remap_table_recursive(
    source_arena: &StateArena,
    table: &ArenaSmallTable,
    _other_arena: &StateArena,
    new_arena: &mut StateArena,
    memo: &mut std::collections::HashMap<(i32, i32), StateId>,
    is_arena1: bool,
) -> ArenaSmallTable {
    let mut new_table = ArenaSmallTable {
        ceilings: table.ceilings.clone(),
        steps: Vec::with_capacity(table.steps.len()),
        epsilons: Vec::with_capacity(table.epsilons.len()),
        spinout: StateId::NONE,
        accel: table.accel.clone(),
    };

    for &step_id in &table.steps {
        if step_id.is_none() {
            new_table.steps.push(StateId::NONE);
        } else {
            let merged = if is_arena1 {
                merge_arena_states_recursive(
                    source_arena,
                    step_id,
                    _other_arena,
                    StateId::NONE,
                    new_arena,
                    memo,
                )
            } else {
                merge_arena_states_recursive(
                    _other_arena,
                    StateId::NONE,
                    source_arena,
                    step_id,
                    new_arena,
                    memo,
                )
            };
            new_table.steps.push(merged);
        }
    }

    for &eps_id in &table.epsilons {
        if eps_id.is_none() {
            new_table.epsilons.push(StateId::NONE);
        } else {
            let merged = if is_arena1 {
                merge_arena_states_recursive(
                    source_arena,
                    eps_id,
                    _other_arena,
                    StateId::NONE,
                    new_arena,
                    memo,
                )
            } else {
                merge_arena_states_recursive(
                    _other_arena,
                    StateId::NONE,
                    source_arena,
                    eps_id,
                    new_arena,
                    memo,
                )
            };
            new_table.epsilons.push(merged);
        }
    }

    if !table.spinout.is_none() {
        let merged = if is_arena1 {
            merge_arena_states_recursive(
                source_arena,
                table.spinout,
                _other_arena,
                StateId::NONE,
                new_arena,
                memo,
            )
        } else {
            merge_arena_states_recursive(
                _other_arena,
                StateId::NONE,
                source_arena,
                table.spinout,
                new_arena,
                memo,
            )
        };
        new_table.spinout = merged;
    }

    new_table
}

/// Merge two arena tables byte-by-byte.
fn merge_arena_tables(
    arena1: &StateArena,
    table1: &ArenaSmallTable,
    arena2: &StateArena,
    table2: &ArenaSmallTable,
    new_arena: &mut StateArena,
    memo: &mut std::collections::HashMap<(i32, i32), StateId>,
) -> ArenaSmallTable {
    // Unpack both tables to 256-element arrays for simplicity
    let mut unpacked1 = [StateId::NONE; BYTE_CEILING];
    let mut unpacked2 = [StateId::NONE; BYTE_CEILING];

    unpack_arena_table(table1, &mut unpacked1);
    unpack_arena_table(table2, &mut unpacked2);

    // Merge each byte
    let mut merged_unpacked = [StateId::NONE; BYTE_CEILING];
    for i in 0..BYTE_CEILING {
        let s1 = unpacked1[i];
        let s2 = unpacked2[i];

        merged_unpacked[i] = merge_arena_states_recursive(arena1, s1, arena2, s2, new_arena, memo);
    }

    // Pack result
    let mut result = ArenaSmallTable::new();
    result.pack(&merged_unpacked);

    // Merge epsilons (for DFA, these should be empty, but handle them anyway)
    for &eps1 in &table1.epsilons {
        let merged =
            merge_arena_states_recursive(arena1, eps1, arena2, StateId::NONE, new_arena, memo);
        if !merged.is_none() {
            result.epsilons.push(merged);
        }
    }
    for &eps2 in &table2.epsilons {
        let merged =
            merge_arena_states_recursive(arena1, StateId::NONE, arena2, eps2, new_arena, memo);
        if !merged.is_none() {
            result.epsilons.push(merged);
        }
    }

    // Merge spinouts (for DFA, these should be NONE)
    if !table1.spinout.is_none() || !table2.spinout.is_none() {
        result.spinout = merge_arena_states_recursive(
            arena1,
            table1.spinout,
            arena2,
            table2.spinout,
            new_arena,
            memo,
        );
    }

    result
}

/// Unpack an ArenaSmallTable into a 256-element array.
fn unpack_arena_table(table: &ArenaSmallTable, unpacked: &mut [StateId; BYTE_CEILING]) {
    let mut byte_idx = 0usize;
    for (i, &ceiling) in table.ceilings.iter().enumerate() {
        let ceiling = ceiling as usize;
        while byte_idx < ceiling && byte_idx < BYTE_CEILING {
            unpacked[byte_idx] = table.steps[i];
            byte_idx += 1;
        }
    }
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

    #[test]
    fn test_ascii_fast_path_acceleration() {
        // Test that ASCII-only negated patterns can be accelerated.
        // This builds a [^x]+ style loop with explicit AccelInfo.
        let mut arena = StateArena::new();
        let field_matcher = Arc::new(FieldMatcher::new());

        // Create exit/final states
        let final_state = arena.alloc();
        arena[final_state]
            .field_transitions
            .push(field_matcher.clone());

        let exit_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        // Create loopback state
        let loopback = arena.alloc();

        // Create start state that matches everything except 'x' (like [^x]+)
        // Build transition table that accepts all ASCII except 'x'
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        for byte in 0..BYTE_CEILING {
            if byte != b'x' as usize {
                unpacked[byte] = loopback;
            }
        }

        let mut start_table = ArenaSmallTable::new();
        start_table.pack(&unpacked);
        // Add acceleration info - exit byte is just 'x'
        start_table.accel = Some(super::super::AccelInfo {
            exit_bytes: [b'x', 0, 0],
            len: 1,
        });

        let start = arena.alloc_with_table(start_table);

        // Set up loopback
        arena[loopback].table.epsilons = vec![exit_state, start];
        arena[loopback].table.accel = Some(super::super::AccelInfo {
            exit_bytes: [b'x', 0, 0],
            len: 1,
        });

        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

        // Test with a long string where 'x' is at the end
        // The acceleration should skip directly to 'x'
        let test_value = b"aaaaaaaaaaaaaaaaaaaaaaaaax";
        traverse_arena_nfa(&arena, start, test_value, &mut bufs);
        // Should NOT match because 'x' is at the end and breaks the pattern
        assert!(
            bufs.transitions.is_empty(),
            "[^x]+ should NOT match string ending with 'x'"
        );

        // Test with string without 'x' - should match
        bufs.clear();
        let test_value2 = b"aaaaaaaaaaaaaaaaaaaaaaaa";
        traverse_arena_nfa(&arena, start, test_value2, &mut bufs);
        assert_eq!(
            bufs.transitions.len(),
            1,
            "[^x]+ should match string without 'x'"
        );

        // Test with 'x' in the middle - should NOT match
        bufs.clear();
        let test_value3 = b"aaaaaaxaaaaaa";
        traverse_arena_nfa(&arena, start, test_value3, &mut bufs);
        assert!(
            bufs.transitions.is_empty(),
            "[^x]+ should NOT match string with 'x' in middle"
        );
    }

    #[test]
    fn test_try_accelerate_arena() {
        // Test the try_accelerate_arena function directly
        let mut table = ArenaSmallTable::new();

        // No accel info - should return None
        assert!(try_accelerate_arena(&table, b"hello").is_none());

        // With 1 exit byte
        table.accel = Some(super::super::AccelInfo {
            exit_bytes: [b'x', 0, 0],
            len: 1,
        });
        assert_eq!(try_accelerate_arena(&table, b"helloxworld"), Some(5)); // finds 'x' at position 5
        assert!(try_accelerate_arena(&table, b"hello").is_none()); // no 'x'

        // With 2 exit bytes
        table.accel = Some(super::super::AccelInfo {
            exit_bytes: [b'x', b'y', 0],
            len: 2,
        });
        assert!(try_accelerate_arena(&table, b"helloworld").is_none()); // neither 'x' nor 'y'
        assert_eq!(try_accelerate_arena(&table, b"hellxyworld"), Some(4)); // finds 'x' at position 4
        assert_eq!(try_accelerate_arena(&table, b"hellyxworld"), Some(4)); // finds 'y' at position 4
        assert_eq!(try_accelerate_arena(&table, b"helloxyw"), Some(5)); // finds 'x' at position 5

        // With 3 exit bytes
        table.accel = Some(super::super::AccelInfo {
            exit_bytes: [b'x', b'y', b'z'],
            len: 3,
        });
        assert_eq!(try_accelerate_arena(&table, b"abcdefghijz"), Some(10)); // finds 'z' at position 10
        assert_eq!(try_accelerate_arena(&table, b"abcxyz"), Some(3)); // finds 'x' at position 3
        assert!(try_accelerate_arena(&table, b"abcdefghij").is_none()); // none of x, y, z
    }
}

#[cfg(test)]
mod merge_tests {
    use super::*;

    /// Helper to create a simple arena with a single transition: start --(byte)--> end
    /// The end state has a field transition to the given FieldMatcher.
    fn make_single_byte_arena(byte: u8, fm: Arc<FieldMatcher>) -> (StateArena, StateId) {
        let mut arena = StateArena::new();

        // End state with field transition
        let end = arena.alloc();
        arena[end].field_transitions.push(fm);

        // Terminator state (required for VALUE_TERMINATOR handling)
        let term = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[end],
        ));

        // Start state transitions on byte to terminator
        let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[byte],
            &[term],
        ));

        (arena, start)
    }

    #[test]
    fn test_merge_empty_arenas() {
        let arena1 = StateArena::new();
        let arena2 = StateArena::new();

        let (merged, start) = merge_arena_dfas(&arena1, StateId::NONE, &arena2, StateId::NONE);

        assert!(
            start.is_none(),
            "Merging empty arenas should return NONE start"
        );
        assert!(merged.is_empty(), "Merged arena should be empty");
    }

    #[test]
    fn test_merge_one_empty_arena() {
        let fm = Arc::new(FieldMatcher::new());
        let (arena1, start1) = make_single_byte_arena(b'a', fm.clone());

        // Merge with empty arena
        let (merged, start) = merge_arena_dfas(&arena1, start1, &StateArena::new(), StateId::NONE);

        // Should work like arena1
        let mut bufs = ArenaNfaBuffers::with_capacity(merged.len());
        traverse_arena_nfa(&merged, start, b"a", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Should match 'a'");
    }

    #[test]
    fn test_merge_single_transition() {
        let fm1 = Arc::new(FieldMatcher::new());
        let fm2 = Arc::new(FieldMatcher::new());

        // Arena1 matches 'a', Arena2 matches 'b'
        let (arena1, start1) = make_single_byte_arena(b'a', fm1.clone());
        let (arena2, start2) = make_single_byte_arena(b'b', fm2.clone());

        let (merged, start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let mut bufs = ArenaNfaBuffers::with_capacity(merged.len());

        // Should match 'a'
        traverse_arena_nfa(&merged, start, b"a", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Merged should match 'a'");
        assert!(Arc::ptr_eq(&bufs.transitions[0], &fm1));

        // Should match 'b'
        bufs.clear();
        traverse_arena_nfa(&merged, start, b"b", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Merged should match 'b'");
        assert!(Arc::ptr_eq(&bufs.transitions[0], &fm2));

        // Should NOT match 'c'
        bufs.clear();
        traverse_arena_nfa(&merged, start, b"c", &mut bufs);
        assert!(bufs.transitions.is_empty(), "Merged should NOT match 'c'");
    }

    #[test]
    fn test_merge_overlapping_transitions() {
        let fm1 = Arc::new(FieldMatcher::new());
        let fm2 = Arc::new(FieldMatcher::new());

        // Both arenas match 'a' but with different field matchers
        let (arena1, start1) = make_single_byte_arena(b'a', fm1.clone());
        let (arena2, start2) = make_single_byte_arena(b'a', fm2.clone());

        let (merged, start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let mut bufs = ArenaNfaBuffers::with_capacity(merged.len());
        traverse_arena_nfa(&merged, start, b"a", &mut bufs);

        // Should have both field matchers
        assert_eq!(
            bufs.transitions.len(),
            2,
            "Overlapping merge should have 2 transitions"
        );
    }

    #[test]
    fn test_merge_preserves_field_transitions() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(100));
        let fm2 = Arc::new(FieldMatcher::with_match_id(200));

        let (arena1, start1) = make_single_byte_arena(b'x', fm1.clone());
        let (arena2, start2) = make_single_byte_arena(b'y', fm2.clone());

        let (merged, start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let mut bufs = ArenaNfaBuffers::with_capacity(merged.len());

        // Check 'x' has fm1
        traverse_arena_nfa(&merged, start, b"x", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1);
        assert_eq!(bufs.transitions[0].match_id, Some(100));

        // Check 'y' has fm2
        bufs.clear();
        traverse_arena_nfa(&merged, start, b"y", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1);
        assert_eq!(bufs.transitions[0].match_id, Some(200));
    }

    #[test]
    fn test_merge_multiple_arenas_associative() {
        // (A merge B) merge C should equal A merge (B merge C)
        let fm_a = Arc::new(FieldMatcher::with_match_id(1));
        let fm_b = Arc::new(FieldMatcher::with_match_id(2));
        let fm_c = Arc::new(FieldMatcher::with_match_id(3));

        let (arena_a, start_a) = make_single_byte_arena(b'a', fm_a.clone());
        let (arena_b, start_b) = make_single_byte_arena(b'b', fm_b.clone());
        let (arena_c, start_c) = make_single_byte_arena(b'c', fm_c.clone());

        // (A merge B) merge C
        let (ab, ab_start) = merge_arena_dfas(&arena_a, start_a, &arena_b, start_b);
        let (abc_left, abc_left_start) = merge_arena_dfas(&ab, ab_start, &arena_c, start_c);

        // A merge (B merge C)
        let (bc, bc_start) = merge_arena_dfas(&arena_b, start_b, &arena_c, start_c);
        let (abc_right, abc_right_start) = merge_arena_dfas(&arena_a, start_a, &bc, bc_start);

        // Both should match 'a', 'b', 'c'
        let mut bufs1 = ArenaNfaBuffers::with_capacity(abc_left.len());
        let mut bufs2 = ArenaNfaBuffers::with_capacity(abc_right.len());

        for byte in [b'a', b'b', b'c'] {
            bufs1.clear();
            bufs2.clear();
            traverse_arena_nfa(&abc_left, abc_left_start, &[byte], &mut bufs1);
            traverse_arena_nfa(&abc_right, abc_right_start, &[byte], &mut bufs2);

            assert_eq!(
                bufs1.transitions.len(),
                bufs2.transitions.len(),
                "Associativity: both should have same number of transitions for '{}'",
                byte as char
            );
            assert_eq!(
                bufs1.transitions.len(),
                1,
                "Should match '{}'",
                byte as char
            );
        }
    }

    #[test]
    fn test_merge_multi_byte_sequences() {
        // Test merging patterns like "ab" and "ac"
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        // Build "ab" arena
        let (arena1, start1) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm1.clone());

            let term = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));

            let state_b = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[b'b'],
                &[term],
            ));

            let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[b'a'],
                &[state_b],
            ));

            (arena, start)
        };

        // Build "ac" arena
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2.clone());

            let term = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));

            let state_c = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[b'c'],
                &[term],
            ));

            let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[b'a'],
                &[state_c],
            ));

            (arena, start)
        };

        let (merged, start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let mut bufs = ArenaNfaBuffers::with_capacity(merged.len());

        // Should match "ab"
        traverse_arena_nfa(&merged, start, b"ab", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Should match 'ab'");
        assert_eq!(bufs.transitions[0].match_id, Some(1));

        // Should match "ac"
        bufs.clear();
        traverse_arena_nfa(&merged, start, b"ac", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Should match 'ac'");
        assert_eq!(bufs.transitions[0].match_id, Some(2));

        // Should NOT match "a", "ad", "bc"
        for val in [b"a".as_slice(), b"ad", b"bc"] {
            bufs.clear();
            traverse_arena_nfa(&merged, start, val, &mut bufs);
            assert!(
                bufs.transitions.is_empty(),
                "Should NOT match {:?}",
                std::str::from_utf8(val)
            );
        }
    }
}
