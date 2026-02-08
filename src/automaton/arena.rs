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

use rustc_hash::FxHashSet;
use smallvec::{smallvec, SmallVec};

use super::small_table::{AccelInfo, FieldMatcher, BYTE_CEILING};
use super::sparse_set::SparseSet;

/// A state identifier - just an index into the arena.
///
/// This can be freely copied and allows cyclic references.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StateId(u32);

impl StateId {
    /// Special sentinel value for "no state" / null reference.
    pub const NONE: StateId = StateId(u32::MAX);

    /// Create a `StateId` from an index.
    #[inline]
    pub fn from_index(index: usize) -> Self {
        StateId(index as u32)
    }

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
    /// Field matchers to transition to when this state is reached at end of value.
    /// SmallVec<[_; 1]> avoids heap allocation for the common case (0 or 1 transitions).
    pub field_transitions: SmallVec<[Arc<FieldMatcher>; 1]>,
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
            field_transitions: SmallVec::new(),
        }
    }
}

/// A compact lookup table using arena-based state references.
///
/// Uses SmallVec to keep small tables (the common case) inline on the stack,
/// avoiding heap allocation for most states.
#[derive(Clone, Debug)]
pub struct ArenaSmallTable {
    /// Upper bounds (exclusive) for each byte range.
    /// SmallVec<[_; 8]> covers most tables inline (typically 1-8 ranges).
    pub ceilings: SmallVec<[u8; 8]>,
    /// State IDs to transition to for each range (StateId::NONE = no transition)
    pub steps: SmallVec<[StateId; 8]>,
    /// Epsilon transitions (taken regardless of input byte).
    /// SmallVec<[_; 2]> covers the common case of 0-2 epsilon transitions.
    pub epsilons: SmallVec<[StateId; 2]>,
    /// Special state for handling wildcard patterns
    pub spinout: StateId,
    /// Acceleration info for self-loop states (exit bytes for memchr skip)
    pub accel: Option<AccelInfo>,
    /// Default transition (computed after pack)
    pub default: StateId,
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
            ceilings: smallvec![BYTE_CEILING as u8],
            steps: smallvec![StateId::NONE],
            epsilons: SmallVec::new(),
            spinout: StateId::NONE,
            accel: None,
            default: StateId::NONE,
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
        table.default = default;
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

        // Compute default as the most common transition
        // For anything-but, we explicitly set it in with_mappings
    }

    /// Set a single byte transition, unpacking and repacking the table.
    pub fn set_transition(&mut self, byte: u8, target: StateId) {
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpack_arena_table(self, &mut unpacked);
        unpacked[byte as usize] = target;
        self.pack(&unpacked);
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

    /// Iterate over sparse transitions (byte, state) pairs that differ from the default.
    pub fn sparse_transitions(&self) -> impl Iterator<Item = (u8, StateId)> + '_ {
        let mut result = Vec::new();
        let mut prev_ceiling: u8 = 0;

        for (i, &ceiling) in self.ceilings.iter().enumerate() {
            let state = self.steps[i];
            if state != self.default {
                // This range has a non-default transition
                for byte in prev_ceiling..ceiling {
                    result.push((byte, state));
                }
            }
            prev_ceiling = ceiling;
        }

        result.into_iter()
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
    /// Seen state IDs (for epsilon closure deduplication) - O(1) clear
    seen_states: SparseSet,
    /// Closure buffer
    closure_stack: Vec<StateId>,
    closure_result: Vec<StateId>,
    /// Seen field matcher transitions (for deduplication, stored as pointer addresses).
    seen_transitions: FxHashSet<usize>,
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
            seen_states: SparseSet::new(state_capacity),
            closure_stack: Vec::with_capacity(16),
            closure_result: Vec::with_capacity(16),
            seen_transitions: FxHashSet::default(),
        }
    }

    pub fn clear(&mut self) {
        self.current_states.clear();
        self.next_states.clear();
        self.transitions.clear();
        self.seen_transitions.clear();
        // Note: seen_states is reset during epsilon closure
    }

    /// Ensure seen_states buffer is large enough for the arena.
    fn ensure_seen_capacity(&mut self, arena_size: usize) {
        if self.seen_states.capacity() < arena_size {
            self.seen_states.resize(arena_size);
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

        // Take ownership of current_states to avoid clone
        let states_to_process = std::mem::take(&mut bufs.current_states);

        for state_id in states_to_process {
            // Get epsilon closure into bufs.closure_result
            fill_epsilon_closure(arena, state_id, bufs);

            // Iterate by index to avoid borrow conflicts
            for ec_idx in 0..bufs.closure_result.len() {
                let ec_state_id = bufs.closure_result[ec_idx];
                let ec_state = &arena[ec_state_id];

                // Collect field transitions from cold storage (deduplicated)
                for ft in &arena[ec_state_id].field_transitions {
                    let ptr = Arc::as_ptr(ft) as usize;
                    if bufs.seen_transitions.insert(ptr) {
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
    let final_states = std::mem::take(&mut bufs.current_states);
    for state_id in final_states {
        // Get epsilon closure into bufs.closure_result
        fill_epsilon_closure(arena, state_id, bufs);

        // Iterate by index to avoid borrow conflicts
        for ec_idx in 0..bufs.closure_result.len() {
            let ec_state_id = bufs.closure_result[ec_idx];
            // Collect field transitions from cold storage (deduplicated)
            for ft in &arena[ec_state_id].field_transitions {
                let ptr = Arc::as_ptr(ft) as usize;
                if bufs.seen_transitions.insert(ptr) {
                    bufs.transitions.push(ft.clone());
                }
            }
        }
    }
}

/// Fast DFA traversal for arena-based automata.
///
/// This is the arena equivalent of the old chain-based `traverse_dfa`.
/// For pure DFA patterns (no epsilon transitions, no spinout states), this is
/// significantly faster than `traverse_arena_nfa` because it follows a single
/// state pointer per byte with no buffer management overhead.
///
/// The caller must ensure the arena is a pure DFA (no epsilon transitions or
/// spinout states). For NFA patterns, use `traverse_arena_nfa`.
#[inline]
pub fn traverse_arena_dfa(
    arena: &StateArena,
    start: StateId,
    val: &[u8],
    transitions: &mut Vec<Arc<FieldMatcher>>,
) {
    if start.is_none() {
        return;
    }

    let mut current = start;

    for i in 0..=val.len() {
        let state = &arena[current];

        // Collect any field transitions at this state (cold data)
        transitions.extend(arena[current].field_transitions.iter().cloned());

        let byte = if i < val.len() {
            val[i]
        } else {
            ARENA_VALUE_TERMINATOR
        };

        let next = state.table.dstep(byte);
        if next.is_none() {
            return;
        }
        current = next;
    }

    // Check final state (cold data)
    transitions.extend(arena[current].field_transitions.iter().cloned());
}

/// Compute the epsilon closure of a state in the arena.
///
/// For DFA-only patterns (no epsilon transitions), this is a fast O(1) operation.
/// For NFA patterns, this computes the full epsilon closure.
///
/// Results are written to `bufs.closure_result`. Callers should iterate
/// `bufs.closure_result` directly after calling this function.
fn fill_epsilon_closure(arena: &StateArena, start: StateId, bufs: &mut ArenaNfaBuffers) {
    bufs.closure_result.clear();

    if start.is_none() {
        return;
    }

    // Fast path: DFA state with no epsilon transitions (common case for numeric patterns)
    let start_state = &arena[start];
    if start_state.table.epsilons.is_empty() {
        bufs.closure_result.push(start);
        return;
    }

    // Slow path: compute full epsilon closure
    bufs.closure_stack.clear();
    bufs.seen_states.clear();

    bufs.closure_result.push(start);
    bufs.closure_stack.push(start);

    // Mark start as seen
    if start.index() < bufs.seen_states.capacity() {
        bufs.seen_states.insert(start.index());
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
            if idx < bufs.seen_states.capacity() && bufs.seen_states.insert(idx) {
                bufs.closure_result.push(eps_id);
                bufs.closure_stack.push(eps_id);
            }
        }
    }
    // SparseSet: no manual cleanup needed - O(1) clear on next call
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

    // Clone field transitions (Arc clone is cheap) - cold data
    new_arena[new_id].field_transitions = arena[state_id].field_transitions.clone();

    let old_state = &arena[state_id];

    // Clone table with remapped state IDs
    let old_table = &old_state.table;
    let mut new_table = ArenaSmallTable {
        ceilings: old_table.ceilings.clone(),
        steps: SmallVec::with_capacity(old_table.steps.len()),
        epsilons: SmallVec::with_capacity(old_table.epsilons.len()),
        spinout: StateId::NONE,
        accel: old_table.accel.clone(),
        default: StateId::NONE,
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

    // Remap default
    if !old_table.default.is_none() {
        new_table.default = clone_state_recursive(arena, old_table.default, new_arena, id_map);
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
        steps: SmallVec::with_capacity(table.steps.len()),
        epsilons: SmallVec::with_capacity(table.epsilons.len()),
        spinout: StateId::NONE,
        accel: table.accel.clone(),
        default: StateId::NONE,
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

    // Remap default
    if !table.default.is_none() {
        let merged = if is_arena1 {
            merge_arena_states_recursive(
                source_arena,
                table.default,
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
                table.default,
                new_arena,
                memo,
            )
        };
        new_table.default = merged;
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

// =============================================================================
// Arena NFA Merge (with epsilon/spinout support)
// =============================================================================

/// Merge two arena-based NFAs into one that matches either pattern.
///
/// This is the full NFA merge that handles:
/// - Epsilon transitions (for alternation patterns)
/// - Spinout states (for wildcard patterns like `*`)
/// - Cycles (for `+` quantifiers)
///
/// The merge strategy follows Go quamina's approach:
/// - If both states have spinouts, merge them recursively
/// - If either has epsilons (but not both spinouts), create a splice state
///   that branches to try both patterns independently
/// - If neither has epsilons, do byte-wise merge
///
/// # Arguments
/// * `arena1` - First arena
/// * `start1` - Start state in first arena (StateId::NONE if empty)
/// * `arena2` - Second arena
/// * `start2` - Start state in second arena (StateId::NONE if empty)
///
/// # Returns
/// A new arena containing the merged NFA and its start state
pub fn merge_arena_nfas(
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
        return clone_arena_subset(arena2, start2);
    }

    if start2.is_none() {
        return clone_arena_subset(arena1, start1);
    }

    // Memoization: (state1_id, state2_id) -> merged_state_id in new arena
    type MemoKey = (i32, i32);
    let mut memo: HashMap<MemoKey, StateId> = HashMap::new();
    let mut new_arena = StateArena::new();

    let start =
        merge_arena_nfa_states_recursive(arena1, start1, arena2, start2, &mut new_arena, &mut memo);

    (new_arena, start)
}

/// Check if a state is an "epsilon-only" splice state created during merges.
///
/// These synthetic states only serve to branch into multiple epsilon targets,
/// with no byte transitions, spinout behavior, or field transitions.
/// Mirrors Go's `smallTable.isEpsilonOnly()`, with additional guards for
/// Rust's spinout and field_transition fields.
fn is_epsilon_only_state(arena: &StateArena, state_id: StateId) -> bool {
    if state_id.is_none() {
        return false;
    }
    let state = &arena[state_id];
    !state.table.epsilons.is_empty()
        && state.table.ceilings.len() == 1
        && state.table.spinout.is_none()
        && state.field_transitions.is_empty()
}

/// Flatten immediate epsilon-only splice states one level deep.
///
/// When merging creates splice states, repeated merges can nest them:
///   splice2 -> [splice1 -> [A, B], C]  (depth 2)
/// This function inlines one level of splice targets:
///   splice2 -> [A, B, C]  (depth reduced by 1)
///
/// Only flattens one level to avoid creating huge epsilon lists for
/// high-pattern-count scenarios (10k+ patterns). Mirrors the intent of
/// Go's `flattenEpsilonTargets()` from PR #486, adapted for Rust's
/// arena architecture where large inline lists hurt cache performance.
fn flatten_epsilon_targets(arena: &StateArena, states: &[StateId]) -> SmallVec<[StateId; 2]> {
    let mut targets = SmallVec::new();

    for &state_id in states {
        if !state_id.is_none() && is_epsilon_only_state(arena, state_id) {
            // Splice state - inline its direct epsilon targets (one level)
            for &eps_id in &arena[state_id].table.epsilons {
                targets.push(eps_id);
            }
        } else {
            targets.push(state_id);
        }
    }
    targets
}

/// Check if a state is a "spinout state" (has spinout marker and exactly 1 epsilon).
///
/// Spinout states are used for wildcard patterns. The convention is:
/// - State has a non-NONE spinout field (marks it as a spinout)
/// - State has exactly 1 epsilon (the continuation after the wildcard)
fn is_spinout_state(arena: &StateArena, state_id: StateId) -> bool {
    if state_id.is_none() {
        return false;
    }
    let state = &arena[state_id];
    !state.table.spinout.is_none() && state.table.epsilons.len() == 1
}

/// Recursively merge two NFA states from different arenas.
///
/// This handles the full NFA merge including epsilons and spinouts.
fn merge_arena_nfa_states_recursive(
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

    // Handle case where one state is NONE
    if state1.is_none() {
        // Copy from arena2
        let s2 = &arena2[state2];
        new_arena[new_id].field_transitions = s2.field_transitions.clone();
        new_arena[new_id].table =
            remap_nfa_table_recursive(arena2, &s2.table, arena1, new_arena, memo, false);
        return new_id;
    }

    if state2.is_none() {
        // Copy from arena1
        let s1 = &arena1[state1];
        new_arena[new_id].field_transitions = s1.field_transitions.clone();
        new_arena[new_id].table =
            remap_nfa_table_recursive(arena1, &s1.table, arena2, new_arena, memo, true);
        return new_id;
    }

    // Both states exist - check for spinout and epsilon cases
    let s1 = &arena1[state1];
    let s2 = &arena2[state2];

    let s1_has_spinout = is_spinout_state(arena1, state1);
    let s2_has_spinout = is_spinout_state(arena2, state2);
    let s1_has_epsilons = !s1.table.epsilons.is_empty();
    let s2_has_epsilons = !s2.table.epsilons.is_empty();

    // Case 1: Both have spinouts - merge them recursively
    if s1_has_spinout && s2_has_spinout {
        let spinout1_eps = s1.table.epsilons[0];
        let spinout2_eps = s2.table.epsilons[0];
        let merged_eps = merge_arena_nfa_states_recursive(
            arena1,
            spinout1_eps,
            arena2,
            spinout2_eps,
            new_arena,
            memo,
        );

        let mut combined_table =
            merge_nfa_tables_bytewise(arena1, &s1.table, arena2, &s2.table, new_arena, memo);

        combined_table.spinout = new_id;
        combined_table.epsilons = smallvec![merged_eps];

        let mut field_transitions = s1.field_transitions.clone();
        field_transitions.extend(s2.field_transitions.iter().cloned());

        new_arena[new_id].table = combined_table;
        new_arena[new_id].field_transitions = field_transitions;
        return new_id;
    }

    // Case 2: Either has epsilons (but not both spinouts) - create splice
    // Flatten epsilon targets to prevent deep nesting from repeated merges.
    // (Mirrors Go PR #486: flattenEpsilonTargets)
    if s1_has_epsilons || s2_has_epsilons {
        let mut clone_map1: std::collections::HashMap<u32, StateId> =
            std::collections::HashMap::new();
        let mut clone_map2: std::collections::HashMap<u32, StateId> =
            std::collections::HashMap::new();
        let cloned1 = clone_state_into_arena(arena1, state1, new_arena, &mut clone_map1);
        let cloned2 = clone_state_into_arena(arena2, state2, new_arena, &mut clone_map2);

        // Flatten: if cloned states are themselves epsilon-only splices,
        // collect their real targets directly instead of nesting splices.
        let epsilons = flatten_epsilon_targets(new_arena, &[cloned1, cloned2]);

        new_arena[new_id].table = ArenaSmallTable {
            ceilings: smallvec![BYTE_CEILING as u8],
            steps: smallvec![StateId::NONE],
            epsilons,
            spinout: StateId::NONE,
            accel: None,
            default: StateId::NONE,
        };
        return new_id;
    }

    // Case 3: Neither has epsilons - do byte-wise merge (DFA case)
    let combined_table =
        merge_nfa_tables_bytewise(arena1, &s1.table, arena2, &s2.table, new_arena, memo);

    let mut field_transitions = s1.field_transitions.clone();
    field_transitions.extend(s2.field_transitions.iter().cloned());

    new_arena[new_id].table = combined_table;
    new_arena[new_id].field_transitions = field_transitions;

    new_id
}

/// Clone a state and all its reachable states from source arena into target arena.
///
/// Uses a separate id_map to track old->new state mappings, allowing multiple
/// independent clones from different arenas without memo key conflicts.
fn clone_state_into_arena(
    source_arena: &StateArena,
    state_id: StateId,
    target_arena: &mut StateArena,
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
    let new_id = target_arena.alloc();
    id_map.insert(state_id.0, new_id);

    // Clone field transitions (Arc clone is cheap) - cold data
    target_arena[new_id].field_transitions = source_arena[state_id].field_transitions.clone();

    let old_state = &source_arena[state_id];

    // Clone table with remapped state IDs
    let old_table = &old_state.table;
    let mut new_table = ArenaSmallTable {
        ceilings: old_table.ceilings.clone(),
        steps: SmallVec::with_capacity(old_table.steps.len()),
        epsilons: SmallVec::with_capacity(old_table.epsilons.len()),
        spinout: StateId::NONE,
        accel: old_table.accel.clone(),
        default: StateId::NONE,
    };

    // Remap steps
    for &step_id in &old_table.steps {
        let new_step = clone_state_into_arena(source_arena, step_id, target_arena, id_map);
        new_table.steps.push(new_step);
    }

    // Remap epsilons
    for &eps_id in &old_table.epsilons {
        let new_eps = clone_state_into_arena(source_arena, eps_id, target_arena, id_map);
        new_table.epsilons.push(new_eps);
    }

    // Remap spinout
    if !old_table.spinout.is_none() {
        new_table.spinout =
            clone_state_into_arena(source_arena, old_table.spinout, target_arena, id_map);
    }

    // Remap default
    if !old_table.default.is_none() {
        new_table.default =
            clone_state_into_arena(source_arena, old_table.default, target_arena, id_map);
    }

    target_arena[new_id].table = new_table;

    new_id
}

/// Remap a table from source arena to the merged arena (NFA version).
fn remap_nfa_table_recursive(
    source_arena: &StateArena,
    table: &ArenaSmallTable,
    _other_arena: &StateArena,
    new_arena: &mut StateArena,
    memo: &mut std::collections::HashMap<(i32, i32), StateId>,
    is_arena1: bool,
) -> ArenaSmallTable {
    let mut new_table = ArenaSmallTable {
        ceilings: table.ceilings.clone(),
        steps: SmallVec::with_capacity(table.steps.len()),
        epsilons: SmallVec::with_capacity(table.epsilons.len()),
        spinout: StateId::NONE,
        accel: table.accel.clone(),
        default: StateId::NONE,
    };

    for &step_id in &table.steps {
        if step_id.is_none() {
            new_table.steps.push(StateId::NONE);
        } else {
            let merged = if is_arena1 {
                merge_arena_nfa_states_recursive(
                    source_arena,
                    step_id,
                    _other_arena,
                    StateId::NONE,
                    new_arena,
                    memo,
                )
            } else {
                merge_arena_nfa_states_recursive(
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

    // Remap default
    if !table.default.is_none() {
        let merged = if is_arena1 {
            merge_arena_nfa_states_recursive(
                source_arena,
                table.default,
                _other_arena,
                StateId::NONE,
                new_arena,
                memo,
            )
        } else {
            merge_arena_nfa_states_recursive(
                _other_arena,
                StateId::NONE,
                source_arena,
                table.default,
                new_arena,
                memo,
            )
        };
        new_table.default = merged;
    }

    for &eps_id in &table.epsilons {
        if eps_id.is_none() {
            new_table.epsilons.push(StateId::NONE);
        } else {
            let merged = if is_arena1 {
                merge_arena_nfa_states_recursive(
                    source_arena,
                    eps_id,
                    _other_arena,
                    StateId::NONE,
                    new_arena,
                    memo,
                )
            } else {
                merge_arena_nfa_states_recursive(
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
            merge_arena_nfa_states_recursive(
                source_arena,
                table.spinout,
                _other_arena,
                StateId::NONE,
                new_arena,
                memo,
            )
        } else {
            merge_arena_nfa_states_recursive(
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

/// Merge two NFA tables byte-by-byte.
fn merge_nfa_tables_bytewise(
    arena1: &StateArena,
    table1: &ArenaSmallTable,
    arena2: &StateArena,
    table2: &ArenaSmallTable,
    new_arena: &mut StateArena,
    memo: &mut std::collections::HashMap<(i32, i32), StateId>,
) -> ArenaSmallTable {
    // Unpack both tables to 256-element arrays
    let mut unpacked1 = [StateId::NONE; BYTE_CEILING];
    let mut unpacked2 = [StateId::NONE; BYTE_CEILING];

    unpack_arena_table(table1, &mut unpacked1);
    unpack_arena_table(table2, &mut unpacked2);

    // Merge each byte
    let mut merged_unpacked = [StateId::NONE; BYTE_CEILING];
    for i in 0..BYTE_CEILING {
        let s1 = unpacked1[i];
        let s2 = unpacked2[i];
        merged_unpacked[i] =
            merge_arena_nfa_states_recursive(arena1, s1, arena2, s2, new_arena, memo);
    }

    // Pack result
    let mut result = ArenaSmallTable::new();
    result.pack(&merged_unpacked);

    // Merge epsilons - collect all unique epsilons
    for &eps1 in &table1.epsilons {
        let merged =
            merge_arena_nfa_states_recursive(arena1, eps1, arena2, StateId::NONE, new_arena, memo);
        if !merged.is_none() {
            result.epsilons.push(merged);
        }
    }
    for &eps2 in &table2.epsilons {
        let merged =
            merge_arena_nfa_states_recursive(arena1, StateId::NONE, arena2, eps2, new_arena, memo);
        if !merged.is_none() {
            result.epsilons.push(merged);
        }
    }

    // Merge spinouts
    if !table1.spinout.is_none() || !table2.spinout.is_none() {
        result.spinout = merge_arena_nfa_states_recursive(
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

// =============================================================================
// Numeric Range Arena FA Builders
// =============================================================================

/// Build an arena-based FA that matches Q-numbers less than a bound.
///
/// This is the arena equivalent of `make_numeric_less_fa` for chain-based FAs.
/// Q-numbers preserve ordering, so we can compare bytes lexicographically.
///
/// # Arguments
/// * `bound` - The numeric bound as f64
/// * `inclusive` - If true, matches <= bound; if false, matches < bound
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_numeric_less_arena_fa(
    bound: f64,
    inclusive: bool,
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let bound_q = crate::numbits::q_num_from_f64(bound);
    let mut arena = StateArena::new();

    // Create the "match" state - has field_transitions to mark the match
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Build the FA recursively
    let start = make_less_arena_fa_step(&bound_q, 0, inclusive, match_state, &mut arena);

    (arena, start)
}

/// Build an arena-based FA that matches Q-numbers greater than a bound.
///
/// # Arguments
/// * `bound` - The numeric bound as f64
/// * `inclusive` - If true, matches >= bound; if false, matches > bound
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_numeric_greater_arena_fa(
    bound: f64,
    inclusive: bool,
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let bound_q = crate::numbits::q_num_from_f64(bound);
    let mut arena = StateArena::new();

    // Create the "match" state
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Build the FA recursively
    let start = make_greater_arena_fa_step(&bound_q, 0, inclusive, match_state, &mut arena);

    (arena, start)
}

/// Build an arena-based FA that matches Q-numbers within a two-sided range.
///
/// This is used for numeric range patterns like `{"numeric": [">=", 0, "<=", 100]}`.
///
/// # Arguments
/// * `lower` - Lower bound value
/// * `lower_incl` - If true, lower bound is inclusive (>=)
/// * `upper` - Upper bound value
/// * `upper_incl` - If true, upper bound is inclusive (<=)
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_numeric_range_arena_fa(
    lower: f64,
    lower_incl: bool,
    upper: f64,
    upper_incl: bool,
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let lower_q = crate::numbits::q_num_from_f64(lower);
    let upper_q = crate::numbits::q_num_from_f64(upper);
    let mut arena = StateArena::new();

    // Create the "match" state
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Build the FA recursively
    let start = make_range_arena_fa_step(
        &lower_q,
        &upper_q,
        0,
        lower_incl,
        upper_incl,
        match_state,
        &mut arena,
    );

    (arena, start)
}

/// Recursive helper for building less-than FA.
fn make_less_arena_fa_step(
    bound_q: &[u8],
    index: usize,
    inclusive: bool,
    match_state: StateId,
    arena: &mut StateArena,
) -> StateId {
    if index >= bound_q.len() {
        // All bound bytes consumed
        // VALUE_TERMINATOR: input == bound (if inclusive, match; else no match)
        // Any other byte: input > bound (no match)
        if inclusive {
            // On VALUE_TERMINATOR: match (equal case)
            let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));
            return start;
        } else {
            // No match for equal case - return a state with no transitions
            return arena.alloc();
        }
    }

    let bound_byte = bound_q[index];

    // Continuation for when input byte == bound_byte
    let continuation = make_less_arena_fa_step(bound_q, index + 1, inclusive, match_state, arena);

    // Build transition table
    let mut unpacked = [StateId::NONE; BYTE_CEILING];

    // VALUE_TERMINATOR: input shorter than bound = input < bound, MATCH
    unpacked[ARENA_VALUE_TERMINATOR as usize] = match_state;

    // Bytes 0..(bound_byte-1): input < bound, MATCH
    for b in 0..bound_byte {
        if b != ARENA_VALUE_TERMINATOR {
            unpacked[b as usize] = match_state;
        }
    }

    // Byte == bound_byte: check rest
    unpacked[bound_byte as usize] = continuation;

    // Bytes > bound_byte: no transition (implicit fail)

    let mut table = ArenaSmallTable::new();
    table.pack(&unpacked);
    arena.alloc_with_table(table)
}

/// Recursive helper for building greater-than FA.
fn make_greater_arena_fa_step(
    bound_q: &[u8],
    index: usize,
    inclusive: bool,
    match_state: StateId,
    arena: &mut StateArena,
) -> StateId {
    if index >= bound_q.len() {
        // All bound bytes consumed
        // VALUE_TERMINATOR: input == bound
        // Any other byte: input has more bytes, so input > bound, MATCH
        let mut unpacked = [match_state; BYTE_CEILING];

        if !inclusive {
            // Strictly greater - VALUE_TERMINATOR = equal, don't match
            unpacked[ARENA_VALUE_TERMINATOR as usize] = StateId::NONE;
        }
        // If inclusive, VALUE_TERMINATOR also matches (equal case)

        let mut table = ArenaSmallTable::new();
        table.pack(&unpacked);
        return arena.alloc_with_table(table);
    }

    let bound_byte = bound_q[index];

    // Continuation for when input byte == bound_byte
    let continuation =
        make_greater_arena_fa_step(bound_q, index + 1, inclusive, match_state, arena);

    // Build table:
    // - VALUE_TERMINATOR: input shorter than bound = input < bound, NO MATCH
    // - byte < bound_byte: input < bound, NO MATCH
    // - byte == bound_byte: check rest
    // - byte > bound_byte (but not VALUE_TERMINATOR): input > bound, MATCH

    let mut unpacked = [StateId::NONE; BYTE_CEILING];

    // Byte == bound_byte: check rest
    unpacked[bound_byte as usize] = continuation;

    // Bytes > bound_byte: input > bound, MATCH
    // But exclude VALUE_TERMINATOR - it means input is shorter, so input < bound
    for b in (bound_byte + 1)..(BYTE_CEILING as u8) {
        if b != ARENA_VALUE_TERMINATOR {
            unpacked[b as usize] = match_state;
        }
    }

    // VALUE_TERMINATOR and bytes < bound_byte: no transition (implicit fail)

    let mut table = ArenaSmallTable::new();
    table.pack(&unpacked);
    arena.alloc_with_table(table)
}

/// Recursive helper for building two-sided range FA.
fn make_range_arena_fa_step(
    lower_q: &[u8],
    upper_q: &[u8],
    index: usize,
    lower_incl: bool,
    upper_incl: bool,
    match_state: StateId,
    arena: &mut StateArena,
) -> StateId {
    let lower_done = index >= lower_q.len();
    let upper_done = index >= upper_q.len();

    // Both bounds exhausted - check terminators
    if lower_done && upper_done {
        // Input has same length as both bounds
        // VALUE_TERMINATOR means we've matched both bounds exactly
        if lower_incl && upper_incl {
            // Both inclusive - accept equal
            return arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));
        } else {
            // At least one exclusive - reject equal
            return arena.alloc();
        }
    }

    // Only lower done - we've established input >= lower, now just check upper
    if lower_done {
        // Delegate to upper-only check (less-than)
        return make_less_arena_fa_step(upper_q, index, upper_incl, match_state, arena);
    }

    // Only upper done - we've established input <= upper, now just check lower
    if upper_done {
        // Input has more bytes than upper bound, so input > upper
        // This means input is out of range (> upper bound)
        return arena.alloc();
    }

    // Both bounds have bytes at this position
    let lower_byte = lower_q[index];
    let upper_byte = upper_q[index];

    if lower_byte == upper_byte {
        // Same byte in both bounds - only that byte continues, others fail
        let continuation = make_range_arena_fa_step(
            lower_q,
            upper_q,
            index + 1,
            lower_incl,
            upper_incl,
            match_state,
            arena,
        );

        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[lower_byte as usize] = continuation;

        let mut table = ArenaSmallTable::new();
        table.pack(&unpacked);
        return arena.alloc_with_table(table);
    }

    // Different bytes in bounds - we have a range of valid first bytes
    // lower_byte < upper_byte (since lower < upper)
    let mut unpacked = [StateId::NONE; BYTE_CEILING];

    // VALUE_TERMINATOR: input shorter than both bounds, input < lower, fail

    // Bytes < lower_byte: fail (< lower)

    // Byte == lower_byte: need to check rest >= lower[index+1:]
    let lower_continuation =
        make_greater_arena_fa_step(lower_q, index + 1, lower_incl, match_state, arena);
    unpacked[lower_byte as usize] = lower_continuation;

    // Bytes in (lower_byte, upper_byte): accept (> lower and < upper)
    for b in (lower_byte + 1)..upper_byte {
        unpacked[b as usize] = match_state;
    }

    // Byte == upper_byte: need to check rest <= upper[index+1:]
    let upper_continuation =
        make_less_arena_fa_step(upper_q, index + 1, upper_incl, match_state, arena);
    unpacked[upper_byte as usize] = upper_continuation;

    // Bytes > upper_byte: fail (> upper)

    let mut table = ArenaSmallTable::new();
    table.pack(&unpacked);
    arena.alloc_with_table(table)
}

// =============================================================================
// String Arena FA Builders
// =============================================================================

/// Build an arena-based FA that matches an exact string.
///
/// This is the arena equivalent of `make_string_fa` for chain-based FAs.
/// Creates a chain of states where each byte transitions to the next,
/// with a final transition on VALUE_TERMINATOR to a match state.
///
/// # Arguments
/// * `val` - The string bytes to match
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_string_arena_fa(val: &[u8], next_field: Arc<FieldMatcher>) -> (StateArena, StateId) {
    let mut arena = StateArena::new();

    // Create the "match" state - has field_transitions to mark the match
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Build the FA chain from end to start
    let start = make_string_arena_fa_step(val, 0, match_state, &mut arena);

    (arena, start)
}

/// Recursive helper for building string-matching FA.
fn make_string_arena_fa_step(
    val: &[u8],
    index: usize,
    match_state: StateId,
    arena: &mut StateArena,
) -> StateId {
    if index >= val.len() {
        // Final step: transition on VALUE_TERMINATOR to match state
        return arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));
    }

    // Recursive step: build rest of chain first, then prepend current byte
    let continuation = make_string_arena_fa_step(val, index + 1, match_state, arena);

    arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &[val[index]],
        &[continuation],
    ))
}

/// Insert a string into an existing arena in-place, sharing prefix structure.
///
/// This is O(L) per string where L is the string length, avoiding the O(n²) cost
/// of repeated `merge_arena_nfas` calls. It walks the existing trie, following
/// existing transitions where they match and creating new states where they diverge.
pub fn insert_string_into_arena(
    arena: &mut StateArena,
    start: StateId,
    val: &[u8],
    field_matcher: Arc<FieldMatcher>,
) {
    let mut current = start;

    for i in 0..=val.len() {
        let byte = if i < val.len() {
            val[i]
        } else {
            ARENA_VALUE_TERMINATOR
        };

        let next = arena[current].table.dstep(byte);
        if !next.is_none() {
            // Transition exists, follow it
            current = next;
        } else {
            // No transition for this byte — create the remaining chain
            let match_state = arena.alloc();
            arena[match_state].field_transitions.push(field_matcher);

            // Build chain backwards for any remaining bytes after this one
            let mut target = match_state;
            for j in (i + 1..=val.len()).rev() {
                let b = if j < val.len() {
                    val[j]
                } else {
                    ARENA_VALUE_TERMINATOR
                };
                target = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[b],
                    &[target],
                ));
            }

            // Add transition from current state to the new chain
            arena[current].table.set_transition(byte, target);
            return;
        }
    }

    // Full path already exists — add field transition to the terminal state
    arena[current].field_transitions.push(field_matcher);
}

/// Build an arena-based FA that matches strings with a given prefix.
///
/// This is the arena equivalent of `make_prefix_fa` for chain-based FAs.
/// After matching all prefix bytes, accepts any remaining bytes (default
/// transition to match state).
///
/// # Arguments
/// * `prefix` - The prefix bytes to match
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_prefix_arena_fa(prefix: &[u8], next_field: Arc<FieldMatcher>) -> (StateArena, StateId) {
    let mut arena = StateArena::new();

    // Create the "match" state - has field_transitions to mark the match
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Build the FA chain from end to start
    let start = make_prefix_arena_fa_step(prefix, 0, match_state, &mut arena);

    (arena, start)
}

/// Recursive helper for building prefix-matching FA.
fn make_prefix_arena_fa_step(
    prefix: &[u8],
    index: usize,
    match_state: StateId,
    arena: &mut StateArena,
) -> StateId {
    if index >= prefix.len() {
        // End of prefix: all bytes should transition to match state (default)
        // Use match_state as default for all byte values
        return arena.alloc_with_table(ArenaSmallTable::with_mappings(
            match_state, // Default transition for all bytes
            &[],
            &[],
        ));
    }

    // Recursive step: build rest of chain first, then prepend current byte
    let continuation = make_prefix_arena_fa_step(prefix, index + 1, match_state, arena);

    arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &[prefix[index]],
        &[continuation],
    ))
}

/// Build an arena-based FA that matches shellstyle wildcard patterns.
///
/// This is the arena equivalent of `make_shellstyle_fa` for chain-based FAs.
/// Shellstyle patterns use `*` as a wildcard that matches zero or more characters.
///
/// # Arguments
/// * `pattern` - The pattern bytes (with `*` as wildcard)
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_shellstyle_arena_fa(
    pattern: &[u8],
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let mut arena = StateArena::new();

    // Create the "match" state - has field_transitions to mark the match
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Parse the pattern into segments
    let segments = parse_shellstyle_segments(pattern);

    // Build the FA from segments
    let start = build_shellstyle_arena_segments(&segments, 0, match_state, &mut arena);

    (arena, start)
}

/// Segment types for shellstyle patterns
#[derive(Debug)]
enum ShellstyleSegment {
    Literal(Vec<u8>),
    Wildcard,
}

/// Parse a shellstyle pattern into segments
fn parse_shellstyle_segments(pattern: &[u8]) -> Vec<ShellstyleSegment> {
    let mut segments = Vec::new();
    let mut i = 0;

    while i < pattern.len() {
        if pattern[i] == b'*' {
            segments.push(ShellstyleSegment::Wildcard);
            i += 1;
        } else {
            // Collect consecutive literal bytes
            let start = i;
            while i < pattern.len() && pattern[i] != b'*' {
                i += 1;
            }
            segments.push(ShellstyleSegment::Literal(pattern[start..i].to_vec()));
        }
    }

    segments
}

/// Build the FA from shellstyle segments
fn build_shellstyle_arena_segments(
    segments: &[ShellstyleSegment],
    index: usize,
    match_state: StateId,
    arena: &mut StateArena,
) -> StateId {
    if index >= segments.len() {
        // End of pattern - transition on VALUE_TERMINATOR to match
        return arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));
    }

    match &segments[index] {
        ShellstyleSegment::Literal(bytes) => {
            // Build continuation first
            let continuation =
                build_shellstyle_arena_segments(segments, index + 1, match_state, arena);

            // Build literal chain
            build_literal_arena_chain(bytes, continuation, arena)
        }
        ShellstyleSegment::Wildcard => {
            // Build continuation first
            let continuation =
                build_shellstyle_arena_segments(segments, index + 1, match_state, arena);

            // Build wildcard (spinout) structure
            build_wildcard_arena_spinout(continuation, arena)
        }
    }
}

/// Build a chain of states for a literal byte sequence
fn build_literal_arena_chain(
    bytes: &[u8],
    continuation: StateId,
    arena: &mut StateArena,
) -> StateId {
    if bytes.is_empty() {
        return continuation;
    }

    // Build from end to start
    let mut current = continuation;
    for &byte in bytes.iter().rev() {
        current = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[byte],
            &[current],
        ));
    }
    current
}

/// Build a wildcard spinout structure
///
/// The spinout state structure:
/// - Has spinout marker (self-reference for wildcard looping)
/// - Has epsilon to continuation (to try matching after any consumed bytes)
/// - On any non-terminator byte, stays in spinout state
fn build_wildcard_arena_spinout(continuation: StateId, arena: &mut StateArena) -> StateId {
    // Create spinout state
    let spinout = arena.alloc();

    // Set up spinout behavior:
    // - spinout marker points to self (enables wildcard "consume any" behavior)
    // - epsilon to continuation (try matching pattern after wildcard)
    arena[spinout].table.spinout = spinout;
    arena[spinout].table.epsilons.push(continuation);

    // Create start state that has epsilon to spinout
    // This allows zero-length wildcard matches
    let start = arena.alloc();
    arena[start].table.epsilons.push(spinout);

    start
}

/// Build an arena-based FA that matches wildcard patterns with escape sequences.
///
/// This is the arena equivalent of `make_wildcard_fa` for chain-based FAs.
/// Similar to shellstyle patterns but with escape sequence support:
/// - `*` matches zero or more characters
/// - `\*` matches literal asterisk
/// - `\\` matches literal backslash
///
/// # Arguments
/// * `pattern` - The pattern bytes (with `*` as wildcard, `\` for escape)
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_wildcard_arena_fa(
    pattern: &[u8],
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let mut arena = StateArena::new();

    // Create the "match" state - has field_transitions to mark the match
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Parse the pattern into segments (handles escape sequences)
    let segments = parse_wildcard_segments(pattern);

    // Build the FA from segments (reuse shellstyle segment builder)
    let start = build_shellstyle_arena_segments(&segments, 0, match_state, &mut arena);

    (arena, start)
}

/// Parse a wildcard pattern into segments, handling escape sequences.
///
/// Differences from shellstyle:
/// - `\*` becomes literal `*`
/// - `\\` becomes literal `\`
/// - Other `\x` sequences pass through as literal (both chars)
fn parse_wildcard_segments(pattern: &[u8]) -> Vec<ShellstyleSegment> {
    let mut segments = Vec::new();
    let mut i = 0;

    while i < pattern.len() {
        if pattern[i] == b'*' {
            segments.push(ShellstyleSegment::Wildcard);
            i += 1;
        } else if pattern[i] == b'\\' && i + 1 < pattern.len() {
            // Escape sequence - consume the escaped character
            let escaped = pattern[i + 1];
            // Start or extend literal segment with escaped character
            if let Some(ShellstyleSegment::Literal(ref mut bytes)) = segments.last_mut() {
                bytes.push(escaped);
            } else {
                segments.push(ShellstyleSegment::Literal(vec![escaped]));
            }
            i += 2;
        } else {
            // Regular character - add to literal segment
            if let Some(ShellstyleSegment::Literal(ref mut bytes)) = segments.last_mut() {
                bytes.push(pattern[i]);
            } else {
                segments.push(ShellstyleSegment::Literal(vec![pattern[i]]));
            }
            i += 1;
        }
    }

    segments
}

/// Build an arena-based FA that matches anything NOT in the excluded list.
///
/// This is the arena equivalent of `make_anything_but_fa` for chain-based FAs.
/// Uses a trie-like structure where:
/// - Default transition goes to success state
/// - Bytes that are prefixes of excluded values recurse
/// - VALUE_TERMINATOR for excluded values goes to failure (no field transitions)
///
/// # Arguments
/// * `excluded` - The list of excluded values (byte sequences)
/// * `next_field` - The field matcher to transition to on success
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_anything_but_arena_fa(
    excluded: &[Vec<u8>],
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let mut arena = StateArena::new();

    // Success state - we match if we get here
    let success = arena.alloc();
    arena[success].field_transitions.push(next_field);

    // Build the trie-like structure
    let start = build_anything_but_step(excluded, 0, success, &mut arena);

    (arena, start)
}

/// Build one step of the anything-but arena automaton.
fn build_anything_but_step(
    vals: &[Vec<u8>],
    index: usize,
    success: StateId,
    arena: &mut StateArena,
) -> StateId {
    use std::collections::{HashMap, HashSet};

    // Group values by the byte at current index
    let mut vals_with_bytes_remaining: HashMap<u8, Vec<&Vec<u8>>> = HashMap::new();
    let mut vals_ending_here: HashSet<u8> = HashSet::new();

    for val in vals {
        let last_index = val.len().saturating_sub(1);
        if index <= last_index && !val.is_empty() {
            let utf8_byte = val[index];
            if index < last_index {
                vals_with_bytes_remaining
                    .entry(utf8_byte)
                    .or_default()
                    .push(val);
            }
            if index == last_index {
                vals_ending_here.insert(utf8_byte);
            }
        }
    }

    // Collect all bytes that need special handling
    let all_bytes: HashSet<u8> = vals_with_bytes_remaining
        .keys()
        .chain(vals_ending_here.iter())
        .copied()
        .collect();

    // Build state for this step
    let mut special_mappings: Vec<(u8, StateId)> = Vec::new();

    for utf8_byte in all_bytes {
        let has_continuation = vals_with_bytes_remaining.contains_key(&utf8_byte);
        let ends_here = vals_ending_here.contains(&utf8_byte);

        if has_continuation && ends_here {
            // Both continues and ends - need combined state
            let continuing_vals = vals_with_bytes_remaining.get(&utf8_byte).unwrap();
            let owned_vals: Vec<Vec<u8>> = continuing_vals.iter().cloned().cloned().collect();

            // Recurse for continuation
            let continuation = build_anything_but_step(&owned_vals, index + 1, success, arena);

            // Build combined state: fail on VALUE_TERMINATOR, inherit continuation for others
            // We need to create a new state that merges the continuation but overrides VALUE_TERMINATOR
            let fail_state = arena.alloc(); // Empty state = fail

            // Build full unpacked table: start with success as default
            let mut combined_unpacked: [StateId; BYTE_CEILING] = [success; BYTE_CEILING];

            // Copy sparse transitions from continuation
            for (byte, next) in arena[continuation].table.sparse_transitions() {
                combined_unpacked[byte as usize] = next;
            }

            // Also copy default if continuation has one
            if !arena[continuation].table.default.is_none() {
                // Fill non-sparse positions with continuation's default
                for slot in combined_unpacked.iter_mut() {
                    if *slot == success {
                        *slot = arena[continuation].table.default;
                    }
                }
            }

            // Override VALUE_TERMINATOR to fail
            combined_unpacked[ARENA_VALUE_TERMINATOR as usize] = fail_state;

            let combined_state = arena.alloc();
            arena[combined_state].table.pack(&combined_unpacked);
            special_mappings.push((utf8_byte, combined_state));
        } else if has_continuation {
            // Only continues
            let continuing_vals = vals_with_bytes_remaining.get(&utf8_byte).unwrap();
            let owned_vals: Vec<Vec<u8>> = continuing_vals.iter().cloned().cloned().collect();
            let next_state = build_anything_but_step(&owned_vals, index + 1, success, arena);
            special_mappings.push((utf8_byte, next_state));
        } else if ends_here {
            // Only ends here - fail on VALUE_TERMINATOR, success on other bytes
            let fail_state = arena.alloc(); // Empty state = fail
            let last_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                success, // Default: success on any other byte
                &[ARENA_VALUE_TERMINATOR],
                &[fail_state], // Fail on terminator
            ));
            special_mappings.push((utf8_byte, last_state));
        }
    }

    // Build the start state with default to success
    if special_mappings.is_empty() {
        // No excluded values to track - just default to success
        return arena.alloc_with_table(ArenaSmallTable::with_mappings(success, &[], &[]));
    }

    // Build state with default success and special transitions
    let bytes: Vec<u8> = special_mappings.iter().map(|(b, _)| *b).collect();
    let states: Vec<StateId> = special_mappings.iter().map(|(_, s)| *s).collect();

    arena.alloc_with_table(ArenaSmallTable::with_mappings(success, &bytes, &states))
}

/// Build an arena-based FA that matches strings case-insensitively.
///
/// This is the arena equivalent of `make_monocase_fa` for chain-based FAs.
/// For each character with a case-folding alternate, creates two paths that
/// converge to the same next state.
///
/// # Arguments
/// * `val` - The pattern value to match case-insensitively (UTF-8 bytes)
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_monocase_arena_fa(val: &[u8], next_field: Arc<FieldMatcher>) -> (StateArena, StateId) {
    use crate::case_folding::case_fold_char;

    let mut arena = StateArena::new();

    // Create the "match" state - has field_transitions to mark the match
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Empty string case
    if val.is_empty() {
        let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));
        return (arena, start);
    }

    // Try to convert to UTF-8 for proper Unicode case folding
    let s = match std::str::from_utf8(val) {
        Ok(s) => s,
        Err(_) => {
            // Invalid UTF-8 - fall back to ASCII-only case folding
            let start = build_monocase_ascii_chain(val, match_state, &mut arena);
            return (arena, start);
        }
    };

    // Collect character info: (original bytes, alternate bytes if any)
    let chars: Vec<(Vec<u8>, Option<Vec<u8>>)> = s
        .char_indices()
        .map(|(offset, ch)| {
            let next_offset = s[offset..]
                .chars()
                .next()
                .map(|c| offset + c.len_utf8())
                .unwrap_or(val.len());
            let orig = val[offset..next_offset].to_vec();

            let alt = case_fold_char(ch).map(|alt_char| {
                let mut buf = [0u8; 4];
                alt_char.encode_utf8(&mut buf);
                buf[..alt_char.len_utf8()].to_vec()
            });

            (orig, alt)
        })
        .collect();

    // Build the FA recursively
    let start = build_monocase_arena_recursive(&chars, 0, match_state, &mut arena);

    (arena, start)
}

/// Build ASCII-only monocase chain (fallback for invalid UTF-8)
fn build_monocase_ascii_chain(val: &[u8], match_state: StateId, arena: &mut StateArena) -> StateId {
    // First create the terminator state
    let term_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &[ARENA_VALUE_TERMINATOR],
        &[match_state],
    ));

    // Build from end to start
    let mut current_next = term_state;

    for i in (0..val.len()).rev() {
        let byte = val[i];
        let alt_byte = if byte.is_ascii_lowercase() {
            Some(byte.to_ascii_uppercase())
        } else if byte.is_ascii_uppercase() {
            Some(byte.to_ascii_lowercase())
        } else {
            None
        };

        let state = if let Some(alt) = alt_byte {
            // Two paths to next state
            if byte < alt {
                arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[byte, alt],
                    &[current_next, current_next],
                ))
            } else {
                arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[alt, byte],
                    &[current_next, current_next],
                ))
            }
        } else {
            // Single path
            arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[byte],
                &[current_next],
            ))
        };

        current_next = state;
    }

    current_next
}

/// Recursively build monocase arena FA
fn build_monocase_arena_recursive(
    chars: &[(Vec<u8>, Option<Vec<u8>>)],
    idx: usize,
    match_state: StateId,
    arena: &mut StateArena,
) -> StateId {
    if idx >= chars.len() {
        // End of string - create state that matches on VALUE_TERMINATOR
        return arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));
    }

    let (orig, alt) = &chars[idx];

    // First, build the state for after this character
    let next_state = build_monocase_arena_recursive(chars, idx + 1, match_state, arena);

    // Now build the transition(s) for this character
    if let Some(alt_bytes) = alt {
        // Two paths to next state - handle common prefix
        let common_prefix = orig
            .iter()
            .zip(alt_bytes.iter())
            .take_while(|(a, b)| a == b)
            .count();

        if common_prefix == 0 {
            // No common prefix - both paths start with different bytes
            let orig_state = build_arena_fragment(orig, next_state, arena);
            let alt_state = build_arena_fragment(alt_bytes, next_state, arena);

            if orig[0] < alt_bytes[0] {
                arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[orig[0], alt_bytes[0]],
                    &[orig_state, alt_state],
                ))
            } else {
                arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[alt_bytes[0], orig[0]],
                    &[alt_state, orig_state],
                ))
            }
        } else {
            // Common prefix - share states for common bytes, then branch
            let orig_suffix = &orig[common_prefix..];
            let alt_suffix = &alt_bytes[common_prefix..];

            // Build the divergent part
            let diverge_state = if orig_suffix.is_empty() && alt_suffix.is_empty() {
                // Identical after common prefix
                next_state
            } else if orig_suffix.is_empty() {
                // Original is done, alternate has more bytes
                let alt_state = build_arena_fragment(alt_suffix, next_state, arena);
                arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[alt_suffix[0]],
                    &[alt_state],
                ))
            } else if alt_suffix.is_empty() {
                // Alternate is done, original has more bytes
                let orig_state = build_arena_fragment(orig_suffix, next_state, arena);
                arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[orig_suffix[0]],
                    &[orig_state],
                ))
            } else {
                // Both have remaining bytes
                let orig_state = build_arena_fragment(orig_suffix, next_state, arena);
                let alt_state = build_arena_fragment(alt_suffix, next_state, arena);

                if orig_suffix[0] < alt_suffix[0] {
                    arena.alloc_with_table(ArenaSmallTable::with_mappings(
                        StateId::NONE,
                        &[orig_suffix[0], alt_suffix[0]],
                        &[orig_state, alt_state],
                    ))
                } else {
                    arena.alloc_with_table(ArenaSmallTable::with_mappings(
                        StateId::NONE,
                        &[alt_suffix[0], orig_suffix[0]],
                        &[alt_state, orig_state],
                    ))
                }
            };

            // Now build the common prefix chain leading to diverge_state
            // Must build a proper chain for all prefix bytes, including single-byte prefixes
            let prefix = &orig[..common_prefix];
            if prefix.is_empty() {
                diverge_state
            } else {
                let mut current = diverge_state;
                for &byte in prefix.iter().rev() {
                    current = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                        StateId::NONE,
                        &[byte],
                        &[current],
                    ));
                }
                current
            }
        }
    } else {
        // No case alternate - single path
        // Build chain from all bytes (including first) to next_state
        let mut current = next_state;
        for &byte in orig.iter().rev() {
            current = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[byte],
                &[current],
            ));
        }
        current
    }
}

/// Build an FA fragment for a byte sequence, returning the state to use as target.
///
/// For sequences of length <= 1, returns end_at since the caller will
/// create the transition on the first byte. For longer sequences, builds
/// a chain from the second byte to end_at.
fn build_arena_fragment(val: &[u8], end_at: StateId, arena: &mut StateArena) -> StateId {
    if val.is_empty() || val.len() == 1 {
        // Caller handles the first (or only) byte
        return end_at;
    }

    // Build chain from last byte back to second byte (skip first byte)
    let mut current = end_at;
    for &byte in val[1..].iter().rev() {
        current = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[byte],
            &[current],
        ));
    }

    current
}

/// Build an arena-based FA that matches CIDR patterns (IPv4/IPv6).
///
/// This is the arena equivalent of `make_cidr_fa` for chain-based FAs.
/// For IPv4, matches IP address strings within the specified CIDR range.
/// For IPv6, matches expanded form hex addresses.
///
/// # Arguments
/// * `cidr` - The CIDR pattern to match
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the FA and its start state
pub fn make_cidr_arena_fa(
    cidr: &crate::json::CidrPattern,
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    use crate::json::CidrPattern;

    match cidr {
        CidrPattern::V4 {
            network,
            prefix_len,
        } => make_ipv4_cidr_arena_fa(network, *prefix_len, next_field),
        CidrPattern::V6 {
            network,
            prefix_len,
        } => make_ipv6_cidr_arena_fa(network, *prefix_len, next_field),
    }
}

/// Build arena FA for IPv4 CIDR matching.
fn make_ipv4_cidr_arena_fa(
    network: &[u8; 4],
    prefix_len: u8,
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let mut arena = StateArena::new();

    // Create match state
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Create terminator state: " → VT → match
    // (closing quote before value terminator, since string values retain quotes)
    let term_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &[ARENA_VALUE_TERMINATOR],
        &[match_state],
    ));
    let close_quote_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        b"\"",
        &[term_state],
    ));

    // Build from right to left (last octet first)
    let mut current_state = close_quote_state;

    for octet_idx in (0..4).rev() {
        // Calculate bit constraints for this octet
        let octet_start_bit = octet_idx * 8;
        let octet_end_bit = octet_start_bit + 8;

        let (min_val, max_val) = if prefix_len as usize >= octet_end_bit {
            // All 8 bits constrained - exact match
            (network[octet_idx], network[octet_idx])
        } else if (prefix_len as usize) <= octet_start_bit {
            // No bits constrained - any value 0-255
            (0u8, 255u8)
        } else {
            // Partial constraint
            let constrained_bits = prefix_len as usize - octet_start_bit;
            let mask = !0u8 << (8 - constrained_bits);
            let base = network[octet_idx] & mask;
            let range_size = 1u16 << (8 - constrained_bits);
            (base, (base as u16 + range_size - 1).min(255) as u8)
        };

        // Build FA for this octet range
        let octet_start = build_octet_range_arena_fa(min_val, max_val, current_state, &mut arena);

        // If not first octet, prepend dot
        if octet_idx > 0 {
            current_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b".",
                &[octet_start],
            ));
        } else {
            current_state = octet_start;
        }
    }

    // Prepend opening quote: " → first_octet
    let open_quote_start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        b"\"",
        &[current_state],
    ));

    (arena, open_quote_start)
}

/// Build arena FA for IPv6 CIDR matching.
fn make_ipv6_cidr_arena_fa(
    network: &[u8; 16],
    prefix_len: u8,
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let mut arena = StateArena::new();

    // Create match state
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Create terminator state: " → VT → match
    let term_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &[ARENA_VALUE_TERMINATOR],
        &[match_state],
    ));
    let close_quote_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        b"\"",
        &[term_state],
    ));

    // Build from right to left
    let mut current_state = close_quote_state;

    for group_idx in (0..8).rev() {
        let byte_idx = group_idx * 2;
        let group_start_bit = group_idx * 16;
        let group_end_bit = group_start_bit + 16;

        let group_value = ((network[byte_idx] as u16) << 8) | (network[byte_idx + 1] as u16);

        let (min_val, max_val) = if prefix_len as usize >= group_end_bit {
            (group_value, group_value)
        } else if (prefix_len as usize) <= group_start_bit {
            (0u16, 0xffffu16)
        } else {
            let constrained_bits = prefix_len as usize - group_start_bit;
            let mask = !0u16 << (16 - constrained_bits);
            let base = group_value & mask;
            let range_size = 1u32 << (16 - constrained_bits);
            (base, (base as u32 + range_size - 1).min(0xffff) as u16)
        };

        // Build FA for this hex group
        let group_start =
            build_ipv6_group_range_arena_fa(min_val, max_val, current_state, &mut arena);

        // If not first group, prepend colon
        if group_idx > 0 {
            current_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b":",
                &[group_start],
            ));
        } else {
            current_state = group_start;
        }
    }

    // Prepend opening quote: " → first_group
    let open_quote_start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        b"\"",
        &[current_state],
    ));

    (arena, open_quote_start)
}

/// Build arena FA for matching an IPv4 octet in range [min_val, max_val].
fn build_octet_range_arena_fa(
    min_val: u8,
    max_val: u8,
    continuation: StateId,
    arena: &mut StateArena,
) -> StateId {
    // For each value, build the string representation and create an NFA
    // Then merge all NFAs together using epsilon transitions

    if min_val == max_val {
        // Single value - just build the literal chain
        let val_str = min_val.to_string();
        return build_literal_chain_arena(val_str.as_bytes(), continuation, arena);
    }

    // Create a start state with epsilon transitions to each value's FA
    let start = arena.alloc();
    let mut value_starts = Vec::new();

    for val in min_val..=max_val {
        let val_str = val.to_string();
        let val_start = build_literal_chain_arena(val_str.as_bytes(), continuation, arena);
        value_starts.push(val_start);
    }

    // Add epsilon transitions to all value FAs
    arena[start].table.epsilons = SmallVec::from_vec(value_starts);

    start
}

/// Build arena FA for matching an IPv6 group in range [min_val, max_val].
fn build_ipv6_group_range_arena_fa(
    min_val: u16,
    max_val: u16,
    continuation: StateId,
    arena: &mut StateArena,
) -> StateId {
    // For efficiency, special case full range (any hex value)
    if min_val == 0 && max_val == 0xffff {
        return build_any_hex_group_arena(continuation, arena);
    }

    if min_val == max_val {
        // Single value
        let val_str = format!("{:x}", min_val);
        return build_literal_chain_arena(val_str.as_bytes(), continuation, arena);
    }

    // Create a start state with epsilon transitions to each value's FA
    let start = arena.alloc();
    let mut value_starts = Vec::new();

    for val in min_val..=max_val {
        let val_str = format!("{:x}", val);
        let val_start = build_literal_chain_arena(val_str.as_bytes(), continuation, arena);
        value_starts.push(val_start);
    }

    arena[start].table.epsilons = SmallVec::from_vec(value_starts);

    start
}

/// Build a literal chain in the arena.
fn build_literal_chain_arena(val: &[u8], continuation: StateId, arena: &mut StateArena) -> StateId {
    let mut current = continuation;
    for &byte in val.iter().rev() {
        current = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[byte],
            &[current],
        ));
    }
    current
}

/// Build FA matching any 1-4 hex digit group.
fn build_any_hex_group_arena(continuation: StateId, arena: &mut StateArena) -> StateId {
    // Match 1-4 hex digits: [0-9a-fA-F]{1,4}
    let hex_chars: Vec<u8> = (b'0'..=b'9')
        .chain(b'a'..=b'f')
        .chain(b'A'..=b'F')
        .collect();

    // Build states for 1, 2, 3, 4 digits
    // Each state should:
    // 1. Accept hex chars to continue matching more digits
    // 2. Have epsilon transition to continuation (allowing match to end here)

    // Start with continuation (state after 4th digit)
    let mut current = continuation;

    // Build from digit 4 back to digit 1
    // After digit 4: must transition to continuation (no more digits allowed)
    // After digit 3, 2, 1: can either continue or epsilon to continuation

    for digit_pos in (0..4).rev() {
        // Create state that accepts any hex char
        let next_state = arena.alloc();

        // Build table with all hex transitions to current
        let mut bytes = Vec::new();
        let mut targets = Vec::new();
        for &b in &hex_chars {
            bytes.push(b);
            targets.push(current);
        }

        arena[next_state].table = ArenaSmallTable::with_mappings(StateId::NONE, &bytes, &targets);

        // For positions 1, 2, 3 (not 0), add epsilon transition to allow match to end
        // After matching 1-3 digits, we can optionally match more or transition out
        if digit_pos > 0 {
            arena[next_state].table.epsilons.push(continuation);
        }

        current = next_state;
    }

    current
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
        arena[loopback].table.epsilons = smallvec![exit_state, start];

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
        arena[loopback].table.epsilons = smallvec![exit_state, start];

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

        arena[loopback].table.epsilons = smallvec![exit_state, start];

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
        for (byte, slot) in unpacked.iter_mut().enumerate() {
            if byte != b'x' as usize {
                *slot = loopback;
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
        arena[loopback].table.epsilons = smallvec![exit_state, start];
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
                b"b",
                &[term],
            ));

            let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"a",
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
                b"c",
                &[term],
            ));

            let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"a",
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

#[cfg(test)]
mod numeric_arena_tests {
    use super::*;
    use crate::numbits::q_num_from_f64;

    /// Helper to test if a Q-number matches against an arena FA
    fn matches_arena(arena: &StateArena, start: StateId, q_num: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, q_num, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_numeric_less_arena_fa_basic() {
        let next_field = Arc::new(FieldMatcher::new());
        let (arena, start) = make_numeric_less_arena_fa(100.0, true, next_field.clone());

        // Q-numbers for testing
        let q50 = q_num_from_f64(50.0);
        let q100 = q_num_from_f64(100.0);
        let q150 = q_num_from_f64(150.0);
        let q0 = q_num_from_f64(0.0);
        let q_neg = q_num_from_f64(-50.0);

        // Should match: 50 < 100
        assert!(matches_arena(&arena, start, &q50), "50 should match <= 100");

        // Should match: 100 <= 100 (inclusive)
        assert!(
            matches_arena(&arena, start, &q100),
            "100 should match <= 100 (inclusive)"
        );

        // Should NOT match: 150 > 100
        assert!(
            !matches_arena(&arena, start, &q150),
            "150 should NOT match <= 100"
        );

        // Should match: 0 < 100
        assert!(matches_arena(&arena, start, &q0), "0 should match <= 100");

        // Should match: -50 < 100
        assert!(
            matches_arena(&arena, start, &q_neg),
            "-50 should match <= 100"
        );
    }

    #[test]
    fn test_numeric_less_arena_fa_exclusive() {
        let next_field = Arc::new(FieldMatcher::new());
        let (arena, start) = make_numeric_less_arena_fa(100.0, false, next_field.clone());

        let q99 = q_num_from_f64(99.0);
        let q100 = q_num_from_f64(100.0);

        // Should match: 99 < 100
        assert!(matches_arena(&arena, start, &q99), "99 should match < 100");

        // Should NOT match: 100 is not < 100 (exclusive)
        assert!(
            !matches_arena(&arena, start, &q100),
            "100 should NOT match < 100 (exclusive)"
        );
    }

    #[test]
    fn test_numeric_greater_arena_fa_basic() {
        let next_field = Arc::new(FieldMatcher::new());
        let (arena, start) = make_numeric_greater_arena_fa(100.0, true, next_field.clone());

        let q50 = q_num_from_f64(50.0);
        let q100 = q_num_from_f64(100.0);
        let q150 = q_num_from_f64(150.0);

        // Should NOT match: 50 < 100
        assert!(
            !matches_arena(&arena, start, &q50),
            "50 should NOT match >= 100"
        );

        // Should match: 100 >= 100 (inclusive)
        assert!(
            matches_arena(&arena, start, &q100),
            "100 should match >= 100 (inclusive)"
        );

        // Should match: 150 > 100
        assert!(
            matches_arena(&arena, start, &q150),
            "150 should match >= 100"
        );
    }

    #[test]
    fn test_numeric_greater_arena_fa_exclusive() {
        let next_field = Arc::new(FieldMatcher::new());
        let (arena, start) = make_numeric_greater_arena_fa(100.0, false, next_field.clone());

        let q100 = q_num_from_f64(100.0);
        let q101 = q_num_from_f64(101.0);

        // Should NOT match: 100 is not > 100 (exclusive)
        assert!(
            !matches_arena(&arena, start, &q100),
            "100 should NOT match > 100 (exclusive)"
        );

        // Should match: 101 > 100
        assert!(
            matches_arena(&arena, start, &q101),
            "101 should match > 100"
        );
    }

    #[test]
    fn test_numeric_range_arena_fa_two_sided() {
        let next_field = Arc::new(FieldMatcher::new());
        // Range: 50 <= x <= 150
        let (arena, start) =
            make_numeric_range_arena_fa(50.0, true, 150.0, true, next_field.clone());

        let q25 = q_num_from_f64(25.0);
        let q50 = q_num_from_f64(50.0);
        let q100 = q_num_from_f64(100.0);
        let q150 = q_num_from_f64(150.0);
        let q200 = q_num_from_f64(200.0);

        // Should NOT match: 25 < 50
        assert!(
            !matches_arena(&arena, start, &q25),
            "25 should NOT match [50, 150]"
        );

        // Should match: 50 is lower bound (inclusive)
        assert!(
            matches_arena(&arena, start, &q50),
            "50 should match [50, 150]"
        );

        // Should match: 100 is in range
        assert!(
            matches_arena(&arena, start, &q100),
            "100 should match [50, 150]"
        );

        // Should match: 150 is upper bound (inclusive)
        assert!(
            matches_arena(&arena, start, &q150),
            "150 should match [50, 150]"
        );

        // Should NOT match: 200 > 150
        assert!(
            !matches_arena(&arena, start, &q200),
            "200 should NOT match [50, 150]"
        );
    }

    #[test]
    fn test_numeric_range_arena_fa_exclusive_bounds() {
        let next_field = Arc::new(FieldMatcher::new());
        // Range: 50 < x < 150 (exclusive both sides)
        let (arena, start) =
            make_numeric_range_arena_fa(50.0, false, 150.0, false, next_field.clone());

        let q50 = q_num_from_f64(50.0);
        let q51 = q_num_from_f64(51.0);
        let q149 = q_num_from_f64(149.0);
        let q150 = q_num_from_f64(150.0);

        // Should NOT match: 50 is lower bound (exclusive)
        assert!(
            !matches_arena(&arena, start, &q50),
            "50 should NOT match (50, 150)"
        );

        // Should match: 51 > 50
        assert!(
            matches_arena(&arena, start, &q51),
            "51 should match (50, 150)"
        );

        // Should match: 149 < 150
        assert!(
            matches_arena(&arena, start, &q149),
            "149 should match (50, 150)"
        );

        // Should NOT match: 150 is upper bound (exclusive)
        assert!(
            !matches_arena(&arena, start, &q150),
            "150 should NOT match (50, 150)"
        );
    }

    #[test]
    fn test_numeric_arena_fa_edge_cases() {
        let next_field = Arc::new(FieldMatcher::new());

        // Test with zero
        let (arena, start) = make_numeric_less_arena_fa(0.0, true, next_field.clone());
        let q_neg = q_num_from_f64(-1.0);
        let q0 = q_num_from_f64(0.0);
        let q1 = q_num_from_f64(1.0);

        assert!(matches_arena(&arena, start, &q_neg), "-1 should match <= 0");
        assert!(matches_arena(&arena, start, &q0), "0 should match <= 0");
        assert!(
            !matches_arena(&arena, start, &q1),
            "1 should NOT match <= 0"
        );

        // Test with negative bound
        let (arena2, start2) = make_numeric_greater_arena_fa(-100.0, true, next_field.clone());
        let q_neg50 = q_num_from_f64(-50.0);
        let q_neg100 = q_num_from_f64(-100.0);
        let q_neg150 = q_num_from_f64(-150.0);

        assert!(
            matches_arena(&arena2, start2, &q_neg50),
            "-50 should match >= -100"
        );
        assert!(
            matches_arena(&arena2, start2, &q_neg100),
            "-100 should match >= -100"
        );
        assert!(
            !matches_arena(&arena2, start2, &q_neg150),
            "-150 should NOT match >= -100"
        );
    }

    #[test]
    fn test_numeric_arena_fa_float_values() {
        let next_field = Arc::new(FieldMatcher::new());

        // Range: 1.5 <= x <= 2.5
        let (arena, start) = make_numeric_range_arena_fa(1.5, true, 2.5, true, next_field.clone());

        let q1 = q_num_from_f64(1.0);
        let q1_5 = q_num_from_f64(1.5);
        let q2 = q_num_from_f64(2.0);
        let q2_5 = q_num_from_f64(2.5);
        let q3 = q_num_from_f64(3.0);

        assert!(
            !matches_arena(&arena, start, &q1),
            "1.0 should NOT match [1.5, 2.5]"
        );
        assert!(
            matches_arena(&arena, start, &q1_5),
            "1.5 should match [1.5, 2.5]"
        );
        assert!(
            matches_arena(&arena, start, &q2),
            "2.0 should match [1.5, 2.5]"
        );
        assert!(
            matches_arena(&arena, start, &q2_5),
            "2.5 should match [1.5, 2.5]"
        );
        assert!(
            !matches_arena(&arena, start, &q3),
            "3.0 should NOT match [1.5, 2.5]"
        );
    }

    #[test]
    fn test_numeric_arena_fa_merge() {
        // Test that numeric arena FAs can be merged
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        // FA1: x < 50
        let (arena1, start1) = make_numeric_less_arena_fa(50.0, false, fm1.clone());

        // FA2: x > 100
        let (arena2, start2) = make_numeric_greater_arena_fa(100.0, false, fm2.clone());

        // Merge: should match x < 50 OR x > 100
        let (merged, merged_start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let q25 = q_num_from_f64(25.0);
        let q75 = q_num_from_f64(75.0);
        let q150 = q_num_from_f64(150.0);

        let mut bufs = ArenaNfaBuffers::with_capacity(merged.len());

        // 25 should match (< 50)
        traverse_arena_nfa(&merged, merged_start, &q25, &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "25 should match merged FA");
        assert_eq!(bufs.transitions[0].match_id, Some(1));

        // 75 should NOT match (50 <= 75 <= 100)
        bufs.clear();
        traverse_arena_nfa(&merged, merged_start, &q75, &mut bufs);
        assert!(bufs.transitions.is_empty(), "75 should NOT match merged FA");

        // 150 should match (> 100)
        bufs.clear();
        traverse_arena_nfa(&merged, merged_start, &q150, &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "150 should match merged FA");
        assert_eq!(bufs.transitions[0].match_id, Some(2));
    }

    #[test]
    fn test_numeric_arena_fa_ordering_preserved() {
        // Property test: Q-number ordering should match float ordering
        let next_field = Arc::new(FieldMatcher::new());

        // Test a series of values
        let test_values = vec![
            -1000.0, -100.0, -10.0, -1.0, -0.5, 0.0, 0.5, 1.0, 10.0, 100.0, 1000.0,
        ];

        for &bound in &test_values {
            let (arena_less, start_less) =
                make_numeric_less_arena_fa(bound, false, next_field.clone());
            let (arena_greater, start_greater) =
                make_numeric_greater_arena_fa(bound, false, next_field.clone());

            for &val in &test_values {
                let q_val = q_num_from_f64(val);

                let matches_less = matches_arena(&arena_less, start_less, &q_val);
                let matches_greater = matches_arena(&arena_greater, start_greater, &q_val);

                if val < bound {
                    assert!(
                        matches_less,
                        "{} should match < {} (Q-number ordering)",
                        val, bound
                    );
                    assert!(
                        !matches_greater,
                        "{} should NOT match > {} (Q-number ordering)",
                        val, bound
                    );
                } else if val > bound {
                    assert!(
                        !matches_less,
                        "{} should NOT match < {} (Q-number ordering)",
                        val, bound
                    );
                    assert!(
                        matches_greater,
                        "{} should match > {} (Q-number ordering)",
                        val, bound
                    );
                } else {
                    // val == bound, exclusive should not match
                    assert!(
                        !matches_less,
                        "{} should NOT match < {} (exclusive)",
                        val, bound
                    );
                    assert!(
                        !matches_greater,
                        "{} should NOT match > {} (exclusive)",
                        val, bound
                    );
                }
            }
        }
    }
}

#[cfg(test)]
mod nfa_merge_tests {
    use super::*;

    /// Helper to check if a value matches against an arena FA
    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    /// Helper to get field matcher match IDs from traversal
    fn get_match_ids(arena: &StateArena, start: StateId, value: &[u8]) -> Vec<u64> {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        bufs.transitions
            .iter()
            .filter_map(|fm| fm.match_id)
            .collect()
    }

    /// Build an arena FA with epsilon transitions (for alternation patterns).
    ///
    /// This creates an FA that matches either "a" or "b" via epsilon branching:
    ///   start --eps--> [matches 'a'] --> match
    ///         --eps--> [matches 'b'] --> match
    fn make_epsilon_alternation_arena(
        patterns: &[&[u8]],
        fm: Arc<FieldMatcher>,
    ) -> (StateArena, StateId) {
        let mut arena = StateArena::new();

        // Create the match state (has field transition)
        let match_state = arena.alloc();
        arena[match_state].field_transitions.push(fm);

        // Create terminator state
        let term_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));

        // Create branch states for each pattern
        let mut branches = Vec::new();
        for pattern in patterns {
            if pattern.is_empty() {
                // Empty pattern - directly transition to term_state
                branches.push(term_state);
            } else {
                // Build chain for pattern bytes
                let mut current = term_state;
                for &byte in pattern.iter().rev() {
                    let state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                        StateId::NONE,
                        &[byte],
                        &[current],
                    ));
                    current = state;
                }
                branches.push(current);
            }
        }

        // Create start state with epsilon transitions to all branches
        let start = arena.alloc();
        arena[start].table.epsilons = SmallVec::from_vec(branches);

        (arena, start)
    }

    /// Build a spinout (wildcard) arena FA.
    ///
    /// Creates an FA that matches `prefix*suffix` pattern:
    ///   - Matches prefix literally
    ///   - Spinout consumes any characters
    ///   - Then matches suffix literally
    fn make_spinout_arena(
        prefix: &[u8],
        suffix: &[u8],
        fm: Arc<FieldMatcher>,
    ) -> (StateArena, StateId) {
        let mut arena = StateArena::new();

        // Match state
        let match_state = arena.alloc();
        arena[match_state].field_transitions.push(fm);

        // Terminal state (matches VALUE_TERMINATOR)
        let term_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));

        // Build suffix chain (backwards)
        let mut after_spinout = term_state;
        for &byte in suffix.iter().rev() {
            let state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[byte],
                &[after_spinout],
            ));
            after_spinout = state;
        }

        // Build spinout state
        // Spinout structure:
        //   spinout_state --any byte--> spinout_state (via spinout marker)
        //                --eps--> after_spinout (to try matching suffix)
        let spinout_state = arena.alloc();
        arena[spinout_state].table.spinout = spinout_state; // Self-loop on any byte
        arena[spinout_state].table.epsilons.push(after_spinout);

        // If suffix starts with a specific byte, also add direct transition
        if !suffix.is_empty() {
            let mut unpacked = [StateId::NONE; BYTE_CEILING];
            unpacked[suffix[0] as usize] = after_spinout;
            arena[spinout_state].table.pack(&unpacked);
        }

        // Build prefix chain
        let mut current = spinout_state;
        // Add epsilon from start of spinout to after_spinout for zero-width wildcard
        if prefix.is_empty() {
            // No prefix - start is the spinout with epsilon to continuation
            let start = arena.alloc();
            arena[start].table.epsilons.push(spinout_state);
            return (arena, start);
        }

        for &byte in prefix.iter().rev() {
            let state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[byte],
                &[current],
            ));
            current = state;
        }

        (arena, current)
    }

    #[test]
    fn test_merge_arena_with_epsilons() {
        // Arena 1: matches "a" OR "b" via epsilon branching
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = make_epsilon_alternation_arena(&[b"a", b"b"], fm1.clone());

        // Arena 2: matches "c" (simple, no epsilons)
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2.clone());
            let term = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"c",
                &[term],
            ));
            (arena, start)
        };

        // Merge: should match "a", "b", or "c"
        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // Test "a" matches from arena1
        let ids_a = get_match_ids(&merged, merged_start, b"a");
        assert!(ids_a.contains(&1), "Merged should match 'a' (id=1)");

        // Test "b" matches from arena1
        let ids_b = get_match_ids(&merged, merged_start, b"b");
        assert!(ids_b.contains(&1), "Merged should match 'b' (id=1)");

        // Test "c" matches from arena2
        let ids_c = get_match_ids(&merged, merged_start, b"c");
        assert!(ids_c.contains(&2), "Merged should match 'c' (id=2)");

        // Test "d" should not match
        assert!(
            !matches_value(&merged, merged_start, b"d"),
            "Merged should NOT match 'd'"
        );
    }

    #[test]
    fn test_merge_arena_with_spinout() {
        // Arena 1: matches "a*b" (wildcard pattern)
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = make_spinout_arena(b"a", b"b", fm1.clone());

        // Arena 2: matches "x*y" (another wildcard pattern)
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = make_spinout_arena(b"x", b"y", fm2.clone());

        // Merge
        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // Test "ab" matches (a + empty wildcard + b)
        let ids_ab = get_match_ids(&merged, merged_start, b"ab");
        assert!(ids_ab.contains(&1), "Merged should match 'ab'");

        // Test "aXXXb" matches (a + wildcard + b)
        let ids_axxxb = get_match_ids(&merged, merged_start, b"aXXXb");
        assert!(ids_axxxb.contains(&1), "Merged should match 'aXXXb'");

        // Test "xy" matches
        let ids_xy = get_match_ids(&merged, merged_start, b"xy");
        assert!(ids_xy.contains(&2), "Merged should match 'xy'");

        // Test "xZZZy" matches
        let ids_xzzzy = get_match_ids(&merged, merged_start, b"xZZZy");
        assert!(ids_xzzzy.contains(&2), "Merged should match 'xZZZy'");

        // Test "abc" should not match (doesn't end with 'b' after 'a')
        // Actually "abc" has 'a' then 'bc', where 'c' breaks the 'b' requirement
        // But "a*b" means "a" followed by anything followed by "b"
        // "abc" = a + bc -> at 'c', we need 'b' but got 'c', no match
        assert!(
            !matches_value(&merged, merged_start, b"ac"),
            "Merged should NOT match 'ac'"
        );
    }

    #[test]
    fn test_merge_arena_shellstyle_patterns() {
        // Test merging shellstyle patterns like "foo*" and "*bar"
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        // Arena 1: matches "foo*" (prefix with wildcard)
        let (arena1, start1) = make_spinout_arena(b"foo", b"", fm1.clone());

        // Arena 2: matches "*bar" (wildcard with suffix)
        let (arena2, start2) = make_spinout_arena(b"", b"bar", fm2.clone());

        // Merge
        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // Test "foo" matches (foo + empty wildcard)
        let ids_foo = get_match_ids(&merged, merged_start, b"foo");
        assert!(ids_foo.contains(&1), "Merged should match 'foo'");

        // Test "fooXYZ" matches (foo + wildcard)
        let ids_fooxyz = get_match_ids(&merged, merged_start, b"fooXYZ");
        assert!(ids_fooxyz.contains(&1), "Merged should match 'fooXYZ'");

        // Test "bar" matches (empty wildcard + bar)
        let ids_bar = get_match_ids(&merged, merged_start, b"bar");
        assert!(ids_bar.contains(&2), "Merged should match 'bar'");

        // Test "XYZbar" matches (wildcard + bar)
        let ids_xyzbar = get_match_ids(&merged, merged_start, b"XYZbar");
        assert!(ids_xyzbar.contains(&2), "Merged should match 'XYZbar'");

        // Test "foobar" matches BOTH patterns!
        let ids_foobar = get_match_ids(&merged, merged_start, b"foobar");
        assert!(
            ids_foobar.contains(&1) && ids_foobar.contains(&2),
            "Merged should match 'foobar' with both patterns"
        );
    }

    #[test]
    fn test_merge_arena_preserves_cycles() {
        // Create cyclic NFA for [ab]+ pattern (one or more 'a' or 'b')
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = {
            let mut arena = StateArena::new();

            // Match state
            let match_state = arena.alloc();
            arena[match_state].field_transitions.push(fm1.clone());

            // Terminal state
            let term_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));

            // Loopback state (placeholder, epsilons set after start is created)
            let loopback = arena.alloc();

            // Start state: matches 'a' or 'b' -> loopback
            let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"ab",
                &[loopback, loopback],
            ));

            // Set up cycle: loopback -> term_state (for exit) and -> start (for loop)
            arena[loopback].table.epsilons = smallvec![term_state, start];

            (arena, start)
        };

        // Arena 2: matches "c" (simple)
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2.clone());
            let term = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"c",
                &[term],
            ));
            (arena, start)
        };

        // Merge
        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // Test cyclic pattern still works: "a", "b", "ab", "aba", "abba", etc.
        assert!(
            matches_value(&merged, merged_start, b"a"),
            "Merged cycle should match 'a'"
        );
        assert!(
            matches_value(&merged, merged_start, b"b"),
            "Merged cycle should match 'b'"
        );
        assert!(
            matches_value(&merged, merged_start, b"ab"),
            "Merged cycle should match 'ab'"
        );
        assert!(
            matches_value(&merged, merged_start, b"aba"),
            "Merged cycle should match 'aba'"
        );
        assert!(
            matches_value(&merged, merged_start, b"abba"),
            "Merged cycle should match 'abba'"
        );
        assert!(
            matches_value(&merged, merged_start, b"aaabbb"),
            "Merged cycle should match 'aaabbb'"
        );

        // Test long cyclic pattern (tests that cycles work efficiently after merge)
        let long_ab = "ab".repeat(50);
        assert!(
            matches_value(&merged, merged_start, long_ab.as_bytes()),
            "Merged cycle should match long 'abab...' pattern"
        );

        // Test "c" from arena2 still works
        assert!(
            matches_value(&merged, merged_start, b"c"),
            "Merged should match 'c'"
        );

        // Test non-matching patterns
        assert!(
            !matches_value(&merged, merged_start, b"d"),
            "Merged should NOT match 'd'"
        );
        assert!(
            !matches_value(&merged, merged_start, b""),
            "[ab]+ should NOT match empty string"
        );
    }

    #[test]
    fn test_merge_arena_both_have_spinouts() {
        // Both arenas have spinout patterns - test that they merge correctly
        // Arena 1: "*X*" (X anywhere)
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = {
            let mut arena = StateArena::new();

            // Match state
            let match_state = arena.alloc();
            arena[match_state].field_transitions.push(fm1.clone());

            // Terminal state
            let term_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));

            // Second spinout (after X)
            let spinout2 = arena.alloc();
            arena[spinout2].table.spinout = spinout2;
            arena[spinout2].table.epsilons.push(term_state);

            // State that matches 'X' -> spinout2
            let x_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"X",
                &[spinout2],
            ));

            // First spinout (before X)
            let spinout1 = arena.alloc();
            arena[spinout1].table.spinout = spinout1;
            arena[spinout1].table.epsilons.push(x_state);
            // Also add direct transition on 'X'
            let mut unpacked = [StateId::NONE; BYTE_CEILING];
            unpacked[b'X' as usize] = spinout2;
            arena[spinout1].table.pack(&unpacked);

            // Start with epsilon to spinout1
            let start = arena.alloc();
            arena[start].table.epsilons.push(spinout1);

            (arena, start)
        };

        // Arena 2: "*Y*" (Y anywhere)
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = {
            let mut arena = StateArena::new();

            let match_state = arena.alloc();
            arena[match_state].field_transitions.push(fm2.clone());

            let term_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));

            let spinout2 = arena.alloc();
            arena[spinout2].table.spinout = spinout2;
            arena[spinout2].table.epsilons.push(term_state);

            let y_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"Y",
                &[spinout2],
            ));

            let spinout1 = arena.alloc();
            arena[spinout1].table.spinout = spinout1;
            arena[spinout1].table.epsilons.push(y_state);
            let mut unpacked = [StateId::NONE; BYTE_CEILING];
            unpacked[b'Y' as usize] = spinout2;
            arena[spinout1].table.pack(&unpacked);

            let start = arena.alloc();
            arena[start].table.epsilons.push(spinout1);

            (arena, start)
        };

        // Merge
        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // Test patterns containing 'X' match id=1
        assert!(
            get_match_ids(&merged, merged_start, b"X").contains(&1),
            "Should match 'X'"
        );
        assert!(
            get_match_ids(&merged, merged_start, b"aXb").contains(&1),
            "Should match 'aXb'"
        );
        assert!(
            get_match_ids(&merged, merged_start, b"aaaXbbb").contains(&1),
            "Should match 'aaaXbbb'"
        );

        // Test patterns containing 'Y' match id=2
        assert!(
            get_match_ids(&merged, merged_start, b"Y").contains(&2),
            "Should match 'Y'"
        );
        assert!(
            get_match_ids(&merged, merged_start, b"aYb").contains(&2),
            "Should match 'aYb'"
        );

        // Test patterns containing both 'X' and 'Y' match both
        let ids_xy = get_match_ids(&merged, merged_start, b"XY");
        assert!(
            ids_xy.contains(&1) && ids_xy.contains(&2),
            "Should match 'XY' with both patterns"
        );

        let ids_yax = get_match_ids(&merged, merged_start, b"YaX");
        assert!(
            ids_yax.contains(&1) && ids_yax.contains(&2),
            "Should match 'YaX' with both patterns"
        );

        // Test pattern with neither 'X' nor 'Y' - should not match
        assert!(
            !matches_value(&merged, merged_start, b"abc"),
            "Should NOT match 'abc'"
        );
    }

    #[test]
    fn test_merge_arena_nfas_empty_cases() {
        let fm = Arc::new(FieldMatcher::new());
        let (arena1, start1) = make_epsilon_alternation_arena(&[b"a"], fm.clone());

        // Merge with empty arena
        let (merged, merged_start) =
            merge_arena_nfas(&arena1, start1, &StateArena::new(), StateId::NONE);
        assert!(
            matches_value(&merged, merged_start, b"a"),
            "Merging with empty should preserve original"
        );

        // Merge empty with non-empty
        let (merged2, merged_start2) =
            merge_arena_nfas(&StateArena::new(), StateId::NONE, &arena1, start1);
        assert!(
            matches_value(&merged2, merged_start2, b"a"),
            "Merging empty with non-empty should preserve original"
        );

        // Merge two empty arenas
        let (_merged3, merged_start3) = merge_arena_nfas(
            &StateArena::new(),
            StateId::NONE,
            &StateArena::new(),
            StateId::NONE,
        );
        assert!(
            merged_start3.is_none(),
            "Merging two empty arenas should return NONE"
        );
    }

    /// Verify that repeated merges flatten splice states instead of nesting them.
    ///
    /// Without flattening, merging A+B+C+D creates:
    ///   splice3 -> [splice2 -> [splice1 -> [A, B], C], D]  (depth 3)
    /// With flattening:
    ///   splice3 -> [A, B, C, D]  (depth 1)
    #[test]
    fn test_flatten_epsilon_targets_on_repeated_merge() {
        let fm = Arc::new(FieldMatcher::new());

        // Build 4 separate single-value arenas
        let (a1, s1) = make_epsilon_alternation_arena(&[b"a"], fm.clone());
        let (a2, s2) = make_epsilon_alternation_arena(&[b"b"], fm.clone());
        let (a3, s3) = make_epsilon_alternation_arena(&[b"c"], fm.clone());
        let (a4, s4) = make_epsilon_alternation_arena(&[b"d"], fm.clone());

        // Merge them one by one (simulates adding patterns sequentially)
        let (m12, s12) = merge_arena_nfas(&a1, s1, &a2, s2);
        let (m123, s123) = merge_arena_nfas(&m12, s12, &a3, s3);
        let (m1234, s1234) = merge_arena_nfas(&m123, s123, &a4, s4);

        // All 4 values should still match
        assert!(matches_value(&m1234, s1234, b"a"), "should match 'a'");
        assert!(matches_value(&m1234, s1234, b"b"), "should match 'b'");
        assert!(matches_value(&m1234, s1234, b"c"), "should match 'c'");
        assert!(matches_value(&m1234, s1234, b"d"), "should match 'd'");
        assert!(!matches_value(&m1234, s1234, b"e"), "should not match 'e'");

        // Verify flattening: the start state's epsilons should point to
        // real states (not nested splice states). With flattening, the
        // start state should have more direct epsilon targets than 2.
        let start_state = &m1234[s1234];
        let eps_count = start_state.table.epsilons.len();
        // Without flattening: always 2 epsilons (splice -> [prev_merge, new])
        // With flattening: should be > 2 (all real states flattened)
        assert!(
            eps_count > 2,
            "Flattening should produce > 2 direct epsilon targets, got {eps_count}"
        );
    }
}

// =============================================================================
// String/Prefix/Shellstyle Arena FA Builder Tests
// =============================================================================

#[cfg(test)]
mod string_arena_tests {
    use super::*;

    /// Helper to check if a value matches against an arena FA
    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_string_arena_fa_basic() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_string_arena_fa(b"hello", fm.clone());

        // Should match exact string
        assert!(
            matches_value(&arena, start, b"hello"),
            "Should match 'hello'"
        );

        // Should NOT match prefix
        assert!(
            !matches_value(&arena, start, b"hell"),
            "Should NOT match 'hell' (prefix)"
        );

        // Should NOT match longer string
        assert!(
            !matches_value(&arena, start, b"helloworld"),
            "Should NOT match 'helloworld' (longer)"
        );

        // Should NOT match different string
        assert!(
            !matches_value(&arena, start, b"world"),
            "Should NOT match 'world'"
        );

        // Should NOT match empty string
        assert!(
            !matches_value(&arena, start, b""),
            "Should NOT match empty string"
        );
    }

    #[test]
    fn test_string_arena_fa_empty_string() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_string_arena_fa(b"", fm.clone());

        // Should match empty string
        assert!(
            matches_value(&arena, start, b""),
            "Should match empty string"
        );

        // Should NOT match non-empty string
        assert!(!matches_value(&arena, start, b"a"), "Should NOT match 'a'");
    }

    #[test]
    fn test_string_arena_fa_single_char() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_string_arena_fa(b"x", fm.clone());

        // Should match single character
        assert!(matches_value(&arena, start, b"x"), "Should match 'x'");

        // Should NOT match different single char
        assert!(!matches_value(&arena, start, b"y"), "Should NOT match 'y'");

        // Should NOT match longer string
        assert!(
            !matches_value(&arena, start, b"xy"),
            "Should NOT match 'xy'"
        );
    }

    #[test]
    fn test_string_arena_fa_utf8() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_string_arena_fa("café".as_bytes(), fm.clone());

        // Should match UTF-8 string
        assert!(
            matches_value(&arena, start, "café".as_bytes()),
            "Should match 'café'"
        );

        // Should NOT match ASCII-only
        assert!(
            !matches_value(&arena, start, b"cafe"),
            "Should NOT match 'cafe'"
        );
    }

    #[test]
    fn test_string_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_string_arena_fa(b"foo", fm1.clone());
        let (arena2, start2) = make_string_arena_fa(b"bar", fm2.clone());

        let (merged, merged_start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        // Should match both patterns
        assert!(
            matches_value(&merged, merged_start, b"foo"),
            "Merged should match 'foo'"
        );
        assert!(
            matches_value(&merged, merged_start, b"bar"),
            "Merged should match 'bar'"
        );

        // Should NOT match other strings
        assert!(
            !matches_value(&merged, merged_start, b"baz"),
            "Merged should NOT match 'baz'"
        );
    }

    #[test]
    fn test_string_arena_fa_merge_common_prefix() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_string_arena_fa(b"prefix_one", fm1.clone());
        let (arena2, start2) = make_string_arena_fa(b"prefix_two", fm2.clone());

        let (merged, merged_start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        // Should match both patterns with common prefix
        assert!(
            matches_value(&merged, merged_start, b"prefix_one"),
            "Merged should match 'prefix_one'"
        );
        assert!(
            matches_value(&merged, merged_start, b"prefix_two"),
            "Merged should match 'prefix_two'"
        );

        // Should NOT match prefix alone
        assert!(
            !matches_value(&merged, merged_start, b"prefix"),
            "Merged should NOT match 'prefix'"
        );
        assert!(
            !matches_value(&merged, merged_start, b"prefix_"),
            "Merged should NOT match 'prefix_'"
        );
    }
}

#[cfg(test)]
mod prefix_arena_tests {
    use super::*;

    /// Helper to check if a value matches against an arena FA
    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_prefix_arena_fa_basic() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_prefix_arena_fa(b"hello", fm.clone());

        // Should match exact prefix
        assert!(
            matches_value(&arena, start, b"hello"),
            "Should match 'hello'"
        );

        // Should match longer strings with prefix
        assert!(
            matches_value(&arena, start, b"helloworld"),
            "Should match 'helloworld'"
        );
        assert!(
            matches_value(&arena, start, b"hello_test"),
            "Should match 'hello_test'"
        );
        assert!(
            matches_value(&arena, start, b"hello123"),
            "Should match 'hello123'"
        );

        // Should NOT match prefix substring
        assert!(
            !matches_value(&arena, start, b"hell"),
            "Should NOT match 'hell' (shorter than prefix)"
        );

        // Should NOT match non-prefix
        assert!(
            !matches_value(&arena, start, b"world"),
            "Should NOT match 'world'"
        );
        assert!(
            !matches_value(&arena, start, b""),
            "Should NOT match empty string"
        );
    }

    #[test]
    fn test_prefix_arena_fa_empty_prefix() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_prefix_arena_fa(b"", fm.clone());

        // Empty prefix should match everything
        assert!(
            matches_value(&arena, start, b""),
            "Should match empty string"
        );
        assert!(
            matches_value(&arena, start, b"anything"),
            "Should match 'anything'"
        );
        assert!(
            matches_value(&arena, start, b"hello world"),
            "Should match 'hello world'"
        );
    }

    #[test]
    fn test_prefix_arena_fa_single_char() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_prefix_arena_fa(b"a", fm.clone());

        // Should match strings starting with 'a'
        assert!(matches_value(&arena, start, b"a"), "Should match 'a'");
        assert!(matches_value(&arena, start, b"abc"), "Should match 'abc'");

        // Should NOT match strings not starting with 'a'
        assert!(!matches_value(&arena, start, b"b"), "Should NOT match 'b'");
        assert!(
            !matches_value(&arena, start, b""),
            "Should NOT match empty string"
        );
    }

    #[test]
    fn test_prefix_arena_fa_utf8() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_prefix_arena_fa("caf".as_bytes(), fm.clone());

        // Should match strings with UTF-8 prefix
        assert!(
            matches_value(&arena, start, "café".as_bytes()),
            "Should match 'café'"
        );
        assert!(
            matches_value(&arena, start, "cafeteria".as_bytes()),
            "Should match 'cafeteria'"
        );

        // Should NOT match non-prefix
        assert!(
            !matches_value(&arena, start, "ca".as_bytes()),
            "Should NOT match 'ca'"
        );
    }

    #[test]
    fn test_prefix_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_prefix_arena_fa(b"foo", fm1.clone());
        let (arena2, start2) = make_prefix_arena_fa(b"bar", fm2.clone());

        let (merged, merged_start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        // Should match both prefixes
        assert!(
            matches_value(&merged, merged_start, b"foo"),
            "Merged should match 'foo'"
        );
        assert!(
            matches_value(&merged, merged_start, b"foobar"),
            "Merged should match 'foobar'"
        );
        assert!(
            matches_value(&merged, merged_start, b"bar"),
            "Merged should match 'bar'"
        );
        assert!(
            matches_value(&merged, merged_start, b"barfoo"),
            "Merged should match 'barfoo'"
        );

        // Should NOT match non-prefixed
        assert!(
            !matches_value(&merged, merged_start, b"baz"),
            "Merged should NOT match 'baz'"
        );
    }
}

#[cfg(test)]
mod shellstyle_arena_tests {
    use super::*;

    /// Helper to check if a value matches against an arena FA
    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_shellstyle_arena_fa_prefix_wildcard() {
        // Pattern: "foo*" - matches "foo" followed by anything
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"foo*", fm.clone());

        assert!(matches_value(&arena, start, b"foo"), "Should match 'foo'");
        assert!(
            matches_value(&arena, start, b"foobar"),
            "Should match 'foobar'"
        );
        assert!(
            matches_value(&arena, start, b"foo123"),
            "Should match 'foo123'"
        );
        assert!(
            !matches_value(&arena, start, b"fo"),
            "Should NOT match 'fo'"
        );
        assert!(
            !matches_value(&arena, start, b"bar"),
            "Should NOT match 'bar'"
        );
    }

    #[test]
    fn test_shellstyle_arena_fa_suffix_wildcard() {
        // Pattern: "*bar" - matches anything followed by "bar"
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"*bar", fm.clone());

        assert!(matches_value(&arena, start, b"bar"), "Should match 'bar'");
        assert!(
            matches_value(&arena, start, b"foobar"),
            "Should match 'foobar'"
        );
        assert!(
            matches_value(&arena, start, b"123bar"),
            "Should match '123bar'"
        );
        assert!(
            !matches_value(&arena, start, b"ba"),
            "Should NOT match 'ba'"
        );
        assert!(
            !matches_value(&arena, start, b"baz"),
            "Should NOT match 'baz'"
        );
    }

    #[test]
    fn test_shellstyle_arena_fa_infix_wildcard() {
        // Pattern: "foo*bar" - matches "foo" then anything then "bar"
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"foo*bar", fm.clone());

        assert!(
            matches_value(&arena, start, b"foobar"),
            "Should match 'foobar'"
        );
        assert!(
            matches_value(&arena, start, b"foo_bar"),
            "Should match 'foo_bar'"
        );
        assert!(
            matches_value(&arena, start, b"foo123bar"),
            "Should match 'foo123bar'"
        );
        assert!(
            !matches_value(&arena, start, b"foo"),
            "Should NOT match 'foo'"
        );
        assert!(
            !matches_value(&arena, start, b"bar"),
            "Should NOT match 'bar'"
        );
        assert!(
            !matches_value(&arena, start, b"foobaz"),
            "Should NOT match 'foobaz'"
        );
    }

    #[test]
    fn test_shellstyle_arena_fa_no_wildcard() {
        // Pattern without wildcard should match exactly
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"hello", fm.clone());

        assert!(
            matches_value(&arena, start, b"hello"),
            "Should match 'hello'"
        );
        assert!(
            !matches_value(&arena, start, b"helloworld"),
            "Should NOT match 'helloworld'"
        );
        assert!(
            !matches_value(&arena, start, b"hell"),
            "Should NOT match 'hell'"
        );
    }

    #[test]
    fn test_shellstyle_arena_fa_only_wildcard() {
        // Pattern: "*" - matches anything
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"*", fm.clone());

        assert!(matches_value(&arena, start, b""), "Should match empty");
        assert!(
            matches_value(&arena, start, b"anything"),
            "Should match 'anything'"
        );
        assert!(
            matches_value(&arena, start, b"foo bar baz"),
            "Should match 'foo bar baz'"
        );
    }

    #[test]
    fn test_shellstyle_arena_fa_double_wildcard() {
        // Pattern: "*foo*" - matches anything containing "foo"
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"*foo*", fm.clone());

        assert!(matches_value(&arena, start, b"foo"), "Should match 'foo'");
        assert!(
            matches_value(&arena, start, b"foobar"),
            "Should match 'foobar'"
        );
        assert!(
            matches_value(&arena, start, b"barfoo"),
            "Should match 'barfoo'"
        );
        assert!(
            matches_value(&arena, start, b"barfoobaz"),
            "Should match 'barfoobaz'"
        );
        assert!(
            !matches_value(&arena, start, b"bar"),
            "Should NOT match 'bar'"
        );
    }

    #[test]
    fn test_shellstyle_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_shellstyle_arena_fa(b"foo*", fm1.clone());
        let (arena2, start2) = make_shellstyle_arena_fa(b"*bar", fm2.clone());

        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // "foo" matches foo*
        assert!(
            matches_value(&merged, merged_start, b"foo"),
            "Merged should match 'foo'"
        );
        // "bar" matches *bar
        assert!(
            matches_value(&merged, merged_start, b"bar"),
            "Merged should match 'bar'"
        );
        // "foobar" matches both
        assert!(
            matches_value(&merged, merged_start, b"foobar"),
            "Merged should match 'foobar'"
        );
        // "baz" matches neither
        assert!(
            !matches_value(&merged, merged_start, b"baz"),
            "Merged should NOT match 'baz'"
        );
    }
}

#[cfg(test)]
mod wildcard_arena_tests {
    use super::*;

    /// Helper to check if a value matches against an arena FA
    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_wildcard_arena_fa_basic() {
        // Same as shellstyle for basic patterns
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_wildcard_arena_fa(b"foo*bar", fm.clone());

        assert!(
            matches_value(&arena, start, b"foobar"),
            "Should match 'foobar'"
        );
        assert!(
            matches_value(&arena, start, b"foo123bar"),
            "Should match 'foo123bar'"
        );
        assert!(
            !matches_value(&arena, start, b"foo"),
            "Should NOT match 'foo'"
        );
    }

    #[test]
    fn test_wildcard_arena_fa_escape_star() {
        // Pattern: "foo\*bar" - matches literal "foo*bar"
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_wildcard_arena_fa(b"foo\\*bar", fm.clone());

        assert!(
            matches_value(&arena, start, b"foo*bar"),
            "Should match 'foo*bar'"
        );
        assert!(
            !matches_value(&arena, start, b"foobar"),
            "Should NOT match 'foobar'"
        );
        assert!(
            !matches_value(&arena, start, b"foo123bar"),
            "Should NOT match 'foo123bar'"
        );
    }

    #[test]
    fn test_wildcard_arena_fa_escape_backslash() {
        // Pattern: "foo\\bar" - matches "foo\bar"
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_wildcard_arena_fa(b"foo\\\\bar", fm.clone());

        assert!(
            matches_value(&arena, start, b"foo\\bar"),
            "Should match 'foo\\bar'"
        );
        assert!(
            !matches_value(&arena, start, b"foobar"),
            "Should NOT match 'foobar'"
        );
    }

    #[test]
    fn test_wildcard_arena_fa_escape_with_wildcard() {
        // Pattern: "foo\\*bar" - matches "foo\" followed by anything then "bar"
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_wildcard_arena_fa(b"foo\\\\*bar", fm.clone());

        assert!(
            matches_value(&arena, start, b"foo\\bar"),
            "Should match 'foo\\bar'"
        );
        assert!(
            matches_value(&arena, start, b"foo\\123bar"),
            "Should match 'foo\\123bar'"
        );
        assert!(
            !matches_value(&arena, start, b"foobar"),
            "Should NOT match 'foobar'"
        );
    }

    #[test]
    fn test_wildcard_arena_fa_star_at_end_with_escape() {
        // Pattern: "foo\**" - matches "foo*" followed by anything
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_wildcard_arena_fa(b"foo\\**", fm.clone());

        assert!(matches_value(&arena, start, b"foo*"), "Should match 'foo*'");
        assert!(
            matches_value(&arena, start, b"foo*bar"),
            "Should match 'foo*bar'"
        );
        assert!(
            !matches_value(&arena, start, b"foo"),
            "Should NOT match 'foo'"
        );
        assert!(
            !matches_value(&arena, start, b"foobar"),
            "Should NOT match 'foobar'"
        );
    }

    #[test]
    fn test_wildcard_arena_fa_no_escape() {
        // Pattern without escape - same as shellstyle
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_wildcard_arena_fa(b"hello", fm.clone());

        assert!(
            matches_value(&arena, start, b"hello"),
            "Should match 'hello'"
        );
        assert!(
            !matches_value(&arena, start, b"helloworld"),
            "Should NOT match 'helloworld'"
        );
    }

    #[test]
    fn test_wildcard_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_wildcard_arena_fa(b"foo\\*", fm1.clone());
        let (arena2, start2) = make_wildcard_arena_fa(b"bar*", fm2.clone());

        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // "foo*" matches exactly
        assert!(
            matches_value(&merged, merged_start, b"foo*"),
            "Merged should match 'foo*'"
        );
        // "bar" matches bar*
        assert!(
            matches_value(&merged, merged_start, b"bar"),
            "Merged should match 'bar'"
        );
        // "baz" matches neither
        assert!(
            !matches_value(&merged, merged_start, b"baz"),
            "Merged should NOT match 'baz'"
        );
    }
}

#[cfg(test)]
mod anything_but_arena_tests {
    use super::*;

    /// Helper to check if a value matches against an arena FA
    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_anything_but_arena_fa_single_value() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let excluded = vec![b"foo".to_vec()];
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm.clone());

        // Should NOT match excluded value
        assert!(
            !matches_value(&arena, start, b"foo"),
            "Should NOT match excluded 'foo'"
        );

        // Should match other values
        assert!(matches_value(&arena, start, b"bar"), "Should match 'bar'");
        assert!(
            matches_value(&arena, start, b"foobar"),
            "Should match 'foobar' (longer than excluded)"
        );
        assert!(matches_value(&arena, start, b"fo"), "Should match 'fo'");
        assert!(matches_value(&arena, start, b""), "Should match empty");
    }

    #[test]
    fn test_anything_but_arena_fa_multiple_values() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let excluded = vec![b"foo".to_vec(), b"bar".to_vec()];
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm.clone());

        // Should NOT match excluded values
        assert!(
            !matches_value(&arena, start, b"foo"),
            "Should NOT match excluded 'foo'"
        );
        assert!(
            !matches_value(&arena, start, b"bar"),
            "Should NOT match excluded 'bar'"
        );

        // Should match other values
        assert!(matches_value(&arena, start, b"baz"), "Should match 'baz'");
        assert!(
            matches_value(&arena, start, b"foobar"),
            "Should match 'foobar'"
        );
    }

    #[test]
    fn test_anything_but_arena_fa_common_prefix() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let excluded = vec![b"foo".to_vec(), b"foobar".to_vec()];
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm.clone());

        // Should NOT match excluded values
        assert!(
            !matches_value(&arena, start, b"foo"),
            "Should NOT match excluded 'foo'"
        );
        assert!(
            !matches_value(&arena, start, b"foobar"),
            "Should NOT match excluded 'foobar'"
        );

        // Should match prefixes and other values
        assert!(matches_value(&arena, start, b"fo"), "Should match 'fo'");
        assert!(matches_value(&arena, start, b"foob"), "Should match 'foob'");
        assert!(
            matches_value(&arena, start, b"foobarbaz"),
            "Should match 'foobarbaz'"
        );
    }

    #[test]
    fn test_anything_but_arena_fa_empty_excluded() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let excluded: Vec<Vec<u8>> = vec![];
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm.clone());

        // Should match everything
        assert!(
            matches_value(&arena, start, b"anything"),
            "Should match 'anything'"
        );
        assert!(matches_value(&arena, start, b""), "Should match empty");
    }

    #[test]
    fn test_anything_but_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_anything_but_arena_fa(&[b"foo".to_vec()], fm1.clone());
        let (arena2, start2) = make_string_arena_fa(b"bar", fm2.clone());

        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // "foo" should NOT match anything-but (id=1) but also not string (id=2)
        assert!(
            !matches_value(&merged, merged_start, b"foo"),
            "Merged should NOT match 'foo'"
        );

        // "bar" should match string (id=2)
        assert!(
            matches_value(&merged, merged_start, b"bar"),
            "Merged should match 'bar'"
        );

        // "baz" should match anything-but (id=1)
        assert!(
            matches_value(&merged, merged_start, b"baz"),
            "Merged should match 'baz'"
        );
    }
}

#[cfg(test)]
mod monocase_arena_tests {
    use super::*;

    /// Helper to check if a value matches against an arena FA
    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_monocase_arena_fa_single_char() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"A", fm.clone());

        // Should match 'A'
        assert!(matches_value(&arena, start, b"A"), "Should match 'A'");
        // Should match 'a'
        assert!(matches_value(&arena, start, b"a"), "Should match 'a'");
        // Should NOT match other
        assert!(!matches_value(&arena, start, b"B"), "Should NOT match 'B'");
    }

    #[test]
    fn test_monocase_arena_fa_two_chars() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"Ab", fm.clone());

        // Should match all case variants
        assert!(matches_value(&arena, start, b"Ab"), "Should match 'Ab'");
        assert!(matches_value(&arena, start, b"ab"), "Should match 'ab'");
        assert!(matches_value(&arena, start, b"AB"), "Should match 'AB'");
        assert!(matches_value(&arena, start, b"aB"), "Should match 'aB'");
        // Should NOT match other
        assert!(
            !matches_value(&arena, start, b"Ac"),
            "Should NOT match 'Ac'"
        );
    }

    #[test]
    fn test_monocase_arena_fa_three_chars() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"cat", fm.clone());

        // Should match all case variants
        assert!(matches_value(&arena, start, b"cat"), "Should match 'cat'");
        assert!(matches_value(&arena, start, b"CAT"), "Should match 'CAT'");
        assert!(matches_value(&arena, start, b"Cat"), "Should match 'Cat'");
    }

    #[test]
    fn test_monocase_arena_fa_basic_ascii() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"Hello", fm.clone());

        // Should match original case
        assert!(
            matches_value(&arena, start, b"Hello"),
            "Should match 'Hello'"
        );

        // Should match different cases
        assert!(
            matches_value(&arena, start, b"hello"),
            "Should match 'hello'"
        );
        assert!(
            matches_value(&arena, start, b"HELLO"),
            "Should match 'HELLO'"
        );
        assert!(
            matches_value(&arena, start, b"hElLo"),
            "Should match 'hElLo'"
        );

        // Should NOT match different strings
        assert!(
            !matches_value(&arena, start, b"world"),
            "Should NOT match 'world'"
        );
        assert!(
            !matches_value(&arena, start, b"hell"),
            "Should NOT match 'hell'"
        );
    }

    #[test]
    fn test_monocase_arena_fa_empty() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"", fm.clone());

        // Empty should match empty
        assert!(matches_value(&arena, start, b""), "Should match empty");

        // Should NOT match non-empty
        assert!(!matches_value(&arena, start, b"a"), "Should NOT match 'a'");
    }

    #[test]
    fn test_monocase_arena_fa_no_case_chars() {
        // Pattern with no case-sensitive chars
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"123", fm.clone());

        // Should match exactly
        assert!(matches_value(&arena, start, b"123"), "Should match '123'");

        // Should NOT match other
        assert!(
            !matches_value(&arena, start, b"456"),
            "Should NOT match '456'"
        );
    }

    #[test]
    fn test_monocase_arena_fa_mixed_ascii() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"Abc123", fm.clone());

        // Should match any case combination
        assert!(
            matches_value(&arena, start, b"Abc123"),
            "Should match 'Abc123'"
        );
        assert!(
            matches_value(&arena, start, b"abc123"),
            "Should match 'abc123'"
        );
        assert!(
            matches_value(&arena, start, b"ABC123"),
            "Should match 'ABC123'"
        );

        // Should NOT match different string
        assert!(
            !matches_value(&arena, start, b"Abc124"),
            "Should NOT match 'Abc124'"
        );
    }

    #[test]
    fn test_monocase_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_monocase_arena_fa(b"Foo", fm1.clone());
        let (arena2, start2) = make_monocase_arena_fa(b"Bar", fm2.clone());

        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // Should match both patterns case-insensitively
        assert!(
            matches_value(&merged, merged_start, b"foo"),
            "Merged should match 'foo'"
        );
        assert!(
            matches_value(&merged, merged_start, b"FOO"),
            "Merged should match 'FOO'"
        );
        assert!(
            matches_value(&merged, merged_start, b"bar"),
            "Merged should match 'bar'"
        );
        assert!(
            matches_value(&merged, merged_start, b"BAR"),
            "Merged should match 'BAR'"
        );

        // Should NOT match other
        assert!(
            !matches_value(&merged, merged_start, b"baz"),
            "Merged should NOT match 'baz'"
        );
    }

    #[test]
    fn test_monocase_arena_fa_greek_sigma() {
        // Test with simple Greek word without accents to test sigma case folding
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        // Pattern: "Σοφα" (Sopha in Greek - without accent, for simpler testing)
        let pattern = "Σοφα".as_bytes();
        let (arena, start) = make_monocase_arena_fa(pattern, fm.clone());

        // Original pattern should match
        assert!(
            matches_value(&arena, start, "Σοφα".as_bytes()),
            "Original Greek should match"
        );

        // Lowercase sigma should match
        assert!(
            matches_value(&arena, start, "σοφα".as_bytes()),
            "Lowercase sigma at start should match"
        );

        // All uppercase should match
        assert!(
            matches_value(&arena, start, "ΣΟΦΑ".as_bytes()),
            "All uppercase should match"
        );

        // Mixed case should match
        assert!(
            matches_value(&arena, start, "σΟΦΑ".as_bytes()),
            "Mixed case should match"
        );
    }
}

#[cfg(test)]
mod cidr_arena_tests {
    use super::*;
    use crate::json::CidrPattern;

    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_cidr_arena_fa_ipv4_exact() {
        // /32 means exact match
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let cidr = CidrPattern::V4 {
            network: [192, 168, 1, 1],
            prefix_len: 32,
        };
        let (arena, start) = make_cidr_arena_fa(&cidr, fm.clone());

        // IP addresses are JSON strings, so they include surrounding quotes
        assert!(
            matches_value(&arena, start, b"\"192.168.1.1\""),
            "Should match exact IP"
        );
        assert!(
            !matches_value(&arena, start, b"\"192.168.1.2\""),
            "Should NOT match different IP"
        );
    }

    // MIRI SKIP RATIONALE: /24 CIDR creates an automaton matching 256 IPs; traversal with
    // 5 test IPs takes ~44s under Miri. Coverage: test_cidr_arena_fa_ipv4_exact (/32, single
    // IP) and test_cidr_arena_fa_ipv4_range (/30, 4 IPs) exercise the same CIDR arena logic.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_cidr_arena_fa_ipv4_24() {
        // /24 means first 3 octets exact, last octet 0-255
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let cidr = CidrPattern::V4 {
            network: [10, 0, 0, 0],
            prefix_len: 24,
        };
        let (arena, start) = make_cidr_arena_fa(&cidr, fm.clone());

        // Should match any IP in 10.0.0.0/24 (quoted, as JSON strings)
        assert!(
            matches_value(&arena, start, b"\"10.0.0.0\""),
            "Should match 10.0.0.0"
        );
        assert!(
            matches_value(&arena, start, b"\"10.0.0.1\""),
            "Should match 10.0.0.1"
        );
        assert!(
            matches_value(&arena, start, b"\"10.0.0.255\""),
            "Should match 10.0.0.255"
        );

        // Should NOT match IPs outside the range
        assert!(
            !matches_value(&arena, start, b"\"10.0.1.0\""),
            "Should NOT match 10.0.1.0"
        );
        assert!(
            !matches_value(&arena, start, b"\"192.168.1.1\""),
            "Should NOT match 192.168.1.1"
        );
    }

    #[test]
    fn test_cidr_arena_fa_ipv4_range() {
        // /30 means 4 addresses: x.x.x.0, x.x.x.1, x.x.x.2, x.x.x.3
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let cidr = CidrPattern::V4 {
            network: [172, 16, 0, 0],
            prefix_len: 30,
        };
        let (arena, start) = make_cidr_arena_fa(&cidr, fm.clone());

        // Should match all 4 addresses (quoted, as JSON strings)
        assert!(
            matches_value(&arena, start, b"\"172.16.0.0\""),
            "Should match 172.16.0.0"
        );
        assert!(
            matches_value(&arena, start, b"\"172.16.0.1\""),
            "Should match 172.16.0.1"
        );
        assert!(
            matches_value(&arena, start, b"\"172.16.0.2\""),
            "Should match 172.16.0.2"
        );
        assert!(
            matches_value(&arena, start, b"\"172.16.0.3\""),
            "Should match 172.16.0.3"
        );

        // Should NOT match outside range
        assert!(
            !matches_value(&arena, start, b"\"172.16.0.4\""),
            "Should NOT match 172.16.0.4"
        );
    }

    #[test]
    fn test_cidr_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let cidr1 = CidrPattern::V4 {
            network: [10, 0, 0, 0],
            prefix_len: 32,
        };
        let cidr2 = CidrPattern::V4 {
            network: [192, 168, 0, 0],
            prefix_len: 32,
        };

        let (arena1, start1) = make_cidr_arena_fa(&cidr1, fm1.clone());
        let (arena2, start2) = make_cidr_arena_fa(&cidr2, fm2.clone());

        let (merged, merged_start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // Should match both (quoted, as JSON strings)
        assert!(
            matches_value(&merged, merged_start, b"\"10.0.0.0\""),
            "Merged should match 10.0.0.0"
        );
        assert!(
            matches_value(&merged, merged_start, b"\"192.168.0.0\""),
            "Merged should match 192.168.0.0"
        );

        // Should NOT match others
        assert!(
            !matches_value(&merged, merged_start, b"\"172.16.0.0\""),
            "Merged should NOT match 172.16.0.0"
        );
    }

    #[test]
    fn test_cidr_arena_fa_ipv6_basic() {
        // IPv6 CIDR pattern 2001:db8::/32
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let cidr = CidrPattern::V6 {
            network: [0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            prefix_len: 32,
        };
        let (arena, start) = make_cidr_arena_fa(&cidr, fm.clone());

        // Should match IPs in range (full form, quoted as JSON strings)
        assert!(
            matches_value(&arena, start, b"\"2001:db8:0:0:0:0:0:1\""),
            "Should match 2001:db8:0:0:0:0:0:1"
        );
        assert!(
            matches_value(&arena, start, b"\"2001:db8:ffff:ffff:ffff:ffff:ffff:ffff\""),
            "Should match end of range"
        );

        // Should NOT match IPs outside range
        assert!(
            !matches_value(&arena, start, b"\"2001:db9:0:0:0:0:0:1\""),
            "Should NOT match 2001:db9:..."
        );
    }
}
