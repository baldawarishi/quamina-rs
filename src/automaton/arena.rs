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

use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::{SmallVec, smallvec};

use super::small_table::{AccelInfo, BYTE_CEILING, FieldMatcher};
use super::sparse_set::SparseSet;

/// A state identifier - just an index into the arena.
///
/// This can be freely copied and allows cyclic references.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StateId(u32);

impl StateId {
    /// Special sentinel value for "no state" / null reference.
    pub const NONE: Self = Self(u32::MAX);

    /// Create a `StateId` from an index.
    #[inline]
    pub fn from_index(index: usize) -> Self {
        Self(index as u32)
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
    /// Start index into `StateArena::closure_data` for this state's epsilon closure.
    pub closure_start: u32,
    /// Number of states in this state's epsilon closure (max 65535).
    pub closure_len: u16,
    /// Start index into `StateArena::ft_ptrs` for this state's field transition pointers.
    /// Populated by `flatten_tables()`.
    pub ft_start: u32,
    /// Number of field transition pointers for this state.
    pub ft_len: u8,
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
            closure_start: 0,
            closure_len: 0,
            ft_start: 0,
            ft_len: 0,
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

        // Note: `default` is not recomputed here. Callers that need it
        // (e.g. `with_mappings`, `make_byte_dot_table`) set it explicitly.
    }

    /// Set a single byte transition, unpacking and repacking the table.
    pub fn set_transition(&mut self, byte: u8, target: StateId) {
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpack_arena_table(self, &mut unpacked);
        unpacked[byte as usize] = target;
        self.pack(&unpacked);
    }

    /// Get the state for a given byte (deterministic step).
    #[inline(always)]
    #[allow(unsafe_code)]
    pub fn dstep(&self, byte: u8) -> StateId {
        let ceilings = self.ceilings.as_slice();
        for (i, &ceiling) in ceilings.iter().enumerate() {
            if byte < ceiling {
                // SAFETY: ceilings and steps always have the same length (enforced by
                // pack/with_mappings). Since i < ceilings.len(), i < steps.len().
                return unsafe { *self.steps.as_slice().get_unchecked(i) };
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

/// Statistics about a `StateArena`'s structure.
#[derive(Clone, Debug, Default)]
pub struct ArenaStats {
    /// Total states in the arena.
    pub state_count: u32,
    /// States with non-trivial transition tables (more than the default catch-all).
    pub tables_with_transitions: u32,
    /// Total ceiling entries across all transition tables.
    pub total_ceiling_entries: u32,
    /// Max ceiling entries in any single table.
    pub max_ceilings: u16,
    /// Total epsilon transitions across all states.
    pub total_epsilons: u32,
    /// Max epsilon transitions on any single state.
    pub max_epsilons: u16,
    /// States that have field transitions (match endpoints).
    pub states_with_field_transitions: u32,
    /// Total entries in the flattened closure_data buffer.
    pub closure_data_len: u32,
    /// States that have precomputed closures.
    pub states_with_closures: u32,
    /// Sum of all closure_len values.
    pub total_closure_entries: u32,
    /// Max closure_len of any single state.
    pub max_closure_len: u16,
    /// Total entries in the flattened ft_ptrs buffer.
    pub ft_ptrs_len: u32,
    /// Number of states with 256-entry DFA lookup tables (0 if not frozen).
    pub dfa_lookup_states: u32,
    /// Estimated total byte size of the arena.
    pub estimated_bytes: usize,
}

impl std::fmt::Display for ArenaStats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "states={}, tables={} (avg_ceil={:.1}, max_ceil={}), \
             epsilons={} (max={}), field_trans={}, \
             closures={}/{} (avg={:.1}, max={}), \
             ft_ptrs={}, dfa_lookup={}, ~{}KB",
            self.state_count,
            self.tables_with_transitions,
            if self.tables_with_transitions > 0 {
                self.total_ceiling_entries as f64 / self.tables_with_transitions as f64
            } else {
                0.0
            },
            self.max_ceilings,
            self.total_epsilons,
            self.max_epsilons,
            self.states_with_field_transitions,
            self.states_with_closures,
            self.state_count,
            if self.states_with_closures > 0 {
                self.total_closure_entries as f64 / self.states_with_closures as f64
            } else {
                0.0
            },
            self.max_closure_len,
            self.ft_ptrs_len,
            self.dfa_lookup_states,
            self.estimated_bytes / 1024,
        )
    }
}

impl ArenaStats {
    /// Accumulate another arena's stats into this aggregate.
    pub fn add(&mut self, other: &Self) {
        self.state_count += other.state_count;
        self.tables_with_transitions += other.tables_with_transitions;
        self.total_ceiling_entries += other.total_ceiling_entries;
        if other.max_ceilings > self.max_ceilings {
            self.max_ceilings = other.max_ceilings;
        }
        self.total_epsilons += other.total_epsilons;
        if other.max_epsilons > self.max_epsilons {
            self.max_epsilons = other.max_epsilons;
        }
        self.states_with_field_transitions += other.states_with_field_transitions;
        self.closure_data_len += other.closure_data_len;
        self.states_with_closures += other.states_with_closures;
        self.total_closure_entries += other.total_closure_entries;
        if other.max_closure_len > self.max_closure_len {
            self.max_closure_len = other.max_closure_len;
        }
        self.ft_ptrs_len += other.ft_ptrs_len;
        self.dfa_lookup_states += other.dfa_lookup_states;
        self.estimated_bytes += other.estimated_bytes;
    }
}

/// Arena for allocating NFA states.
///
/// States are allocated contiguously and referenced by `StateId`.
/// The arena owns all state memory and frees it when dropped.
#[derive(Clone, Default)]
pub struct StateArena {
    states: Vec<ArenaFaState>,
    /// All epsilon closures concatenated. Each state indexes into this via
    /// `closure_start`/`closure_len`. Populated by `precompute_epsilon_closures()`.
    closure_data: Vec<StateId>,
    /// All field transition raw pointers (as `usize`) concatenated. Each state
    /// indexes into this via `ft_start`/`ft_len`. Populated by `flatten_tables()`.
    ft_ptrs: Vec<usize>,
    /// 256-entry lookup table per state for O(1) byte transitions.
    /// Layout: `dfa_lookup[state_index * 256 + byte] = next_state_id`.
    /// Populated by `flatten_tables()`. Empty if not yet frozen.
    dfa_lookup: Vec<StateId>,
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
        Self {
            states: Vec::new(),
            closure_data: Vec::new(),
            ft_ptrs: Vec::new(),
            dfa_lookup: Vec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            states: Vec::with_capacity(capacity),
            closure_data: Vec::with_capacity(capacity),
            ft_ptrs: Vec::new(),
            dfa_lookup: Vec::new(),
        }
    }

    /// Estimate the byte size of this arena (state vector capacity * per-state size).
    pub fn estimated_byte_size(&self) -> usize {
        self.states.capacity() * std::mem::size_of::<ArenaFaState>()
            + self.closure_data.capacity() * std::mem::size_of::<StateId>()
            + self.ft_ptrs.capacity() * std::mem::size_of::<usize>()
            + self.dfa_lookup.capacity() * std::mem::size_of::<StateId>()
    }

    /// Get a state reference without bounds checking.
    ///
    /// # Safety
    /// `id` must be a valid state ID returned by `alloc()` on this arena.
    #[inline(always)]
    #[allow(unsafe_code)]
    unsafe fn state_unchecked(&self, id: StateId) -> &ArenaFaState {
        // SAFETY: caller guarantees `id` is a valid index from `alloc()` on this arena
        unsafe { self.states.get_unchecked(id.index()) }
    }

    /// Get the epsilon closure for a state as a slice.
    #[inline(always)]
    #[allow(unsafe_code)]
    pub fn closure_of(&self, id: StateId) -> &[StateId] {
        // SAFETY: `id` was returned by `alloc()` on this arena, so `state_unchecked` is valid.
        // `closure_start` and `closure_len` are set by `precompute_epsilon_closures()` to
        // valid indices within `closure_data`.
        unsafe {
            let state = self.state_unchecked(id);
            let start = state.closure_start as usize;
            let len = state.closure_len as usize;
            self.closure_data.get_unchecked(start..start + len)
        }
    }

    /// Get field transition pointers for a state as a slice.
    ///
    /// Returns raw pointer values (`Arc::as_ptr` cast to `usize`) for dedup.
    /// Only valid after `flatten_tables()` has been called.
    #[inline(always)]
    #[allow(unsafe_code)]
    pub fn ft_ptrs_of(&self, id: StateId) -> &[usize] {
        // SAFETY: `id` was returned by `alloc()` on this arena, so `state_unchecked` is valid.
        // `ft_start` and `ft_len` are set by `flatten_tables()` to valid indices within `ft_ptrs`.
        unsafe {
            let state = self.state_unchecked(id);
            let len = state.ft_len as usize;
            if len == 0 {
                return &[];
            }
            let start = state.ft_start as usize;
            self.ft_ptrs.get_unchecked(start..start + len)
        }
    }

    /// Fast deterministic step using 256-entry lookup table.
    ///
    /// When `dfa_lookup` is populated (after `flatten_tables()`), this is a single
    /// array lookup: O(1). Otherwise falls back to SmallVec linear scan.
    #[inline(always)]
    #[allow(unsafe_code)]
    pub fn dstep(&self, id: StateId, byte: u8) -> StateId {
        if !self.dfa_lookup.is_empty() {
            // SAFETY: id.index() < states.len(), byte is 0..255,
            // so id.index() * 256 + byte < states.len() * 256 = dfa_lookup.len()
            unsafe {
                *self
                    .dfa_lookup
                    .get_unchecked(id.index() * 256 + byte as usize)
            }
        } else {
            // Fallback: flat buffers not populated (mutable path)
            self.states[id.index()].table.dstep(byte)
        }
    }

    /// Allocate a new default state, returning its ID.
    pub fn alloc(&mut self) -> StateId {
        let id = StateId(self.states.len() as u32);
        // Set trivial epsilon closure so states added after
        // precompute_epsilon_closures() are visible during NFA traversal.
        let state = ArenaFaState {
            closure_start: self.closure_data.len() as u32,
            closure_len: 1,
            ..Default::default()
        };
        self.closure_data.push(id);
        self.states.push(state);
        id
    }

    /// Allocate a new state with the given table, returning its ID.
    pub fn alloc_with_table(&mut self, table: ArenaSmallTable) -> StateId {
        let id = StateId(self.states.len() as u32);
        let mut state = ArenaFaState::with_table(table);
        state.closure_start = self.closure_data.len() as u32;
        state.closure_len = 1;
        self.closure_data.push(id);
        self.states.push(state);
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

    /// Compute statistics about this arena's structure.
    pub fn stats(&self) -> ArenaStats {
        let state_count = self.states.len();
        if state_count == 0 {
            return ArenaStats::default();
        }

        let mut tables_with_transitions = 0u32;
        let mut total_ceiling_entries = 0u32;
        let mut max_ceilings = 0u16;
        let mut total_epsilons = 0u32;
        let mut max_epsilons = 0u16;
        let mut states_with_field_transitions = 0u32;
        let mut total_closure_entries = 0u32;
        let mut max_closure_len = 0u16;
        let mut states_with_closures = 0u32;

        for state in &self.states {
            // Table stats: count states that have non-trivial transitions
            // (more than just the default catch-all entry)
            let nc = state.table.ceilings.len();
            if nc > 1 {
                tables_with_transitions += 1;
                total_ceiling_entries += nc as u32;
                if nc > max_ceilings as usize {
                    max_ceilings = nc as u16;
                }
            }

            let ne = state.table.epsilons.len();
            if ne > 0 {
                total_epsilons += ne as u32;
                if ne > max_epsilons as usize {
                    max_epsilons = ne as u16;
                }
            }

            if !state.field_transitions.is_empty() {
                states_with_field_transitions += 1;
            }

            if state.closure_len > 0 {
                states_with_closures += 1;
                total_closure_entries += state.closure_len as u32;
                if state.closure_len > max_closure_len {
                    max_closure_len = state.closure_len;
                }
            }
        }

        let dfa_lookup_states = if self.dfa_lookup.is_empty() {
            0
        } else {
            self.dfa_lookup.len() / 256
        };

        ArenaStats {
            state_count: state_count as u32,
            tables_with_transitions,
            total_ceiling_entries,
            max_ceilings,
            total_epsilons,
            max_epsilons,
            states_with_field_transitions,
            closure_data_len: self.closure_data.len() as u32,
            states_with_closures,
            total_closure_entries,
            max_closure_len,
            ft_ptrs_len: self.ft_ptrs.len() as u32,
            dfa_lookup_states: dfa_lookup_states as u32,
            estimated_bytes: self.estimated_byte_size(),
        }
    }

    /// Check if any state in the arena has epsilon transitions.
    ///
    /// Not all regexp FAs are nondeterministic. This can be detected after
    /// building the FA to allow deterministic regexps to use the faster DFA
    /// traversal path.
    pub fn is_nondeterministic(&self) -> bool {
        self.states
            .iter()
            .any(|state| !state.table.epsilons.is_empty())
    }

    /// Precompute epsilon closures for all states in the arena.
    ///
    /// For each state, computes the set of all states reachable via epsilon
    /// transitions (including the state itself) and stores it on the state.
    /// This eliminates per-byte DFS computation during NFA traversal.
    ///
    /// Must be called after the arena structure is finalized (e.g., after merging).
    pub fn precompute_epsilon_closures(&mut self) {
        let arena_len = self.states.len();
        if arena_len == 0 {
            return;
        }

        let mut seen = SparseSet::new(arena_len);
        let mut stack: Vec<StateId> = Vec::new();

        // Build all closures into a single flat buffer
        let mut closure_data: Vec<StateId> = Vec::with_capacity(arena_len);
        // Temporary per-state closure for NFA states
        let mut closure_buf: Vec<StateId> = Vec::new();

        for state_idx in 0..arena_len {
            let state_id = StateId::from_index(state_idx);
            let start = closure_data.len() as u32;

            if self.states[state_idx].table.epsilons.is_empty() {
                // DFA state: closure is just [self]
                closure_data.push(state_id);
                self.states[state_idx].closure_start = start;
                self.states[state_idx].closure_len = 1;
            } else {
                // NFA state: compute full epsilon closure via DFS
                seen.clear();
                stack.clear();
                closure_buf.clear();

                closure_buf.push(state_id);
                stack.push(state_id);
                seen.insert(state_idx);

                while let Some(current_id) = stack.pop() {
                    if current_id.is_none() {
                        continue;
                    }
                    for &eps_id in &self.states[current_id.index()].table.epsilons {
                        if !eps_id.is_none() {
                            let idx = eps_id.index();
                            if idx < seen.capacity() && seen.insert(idx) {
                                closure_buf.push(eps_id);
                                stack.push(eps_id);
                            }
                        }
                    }
                }

                debug_assert!(
                    closure_buf.len() <= u16::MAX as usize,
                    "epsilon closure exceeds u16::MAX states"
                );
                let len = closure_buf.len() as u16;
                closure_data.extend_from_slice(&closure_buf);
                self.states[state_idx].closure_start = start;
                self.states[state_idx].closure_len = len;
            }
        }

        self.closure_data = closure_data;
    }

    /// Build frozen lookup structures for fast traversal.
    ///
    /// Populates:
    /// - `ft_ptrs`: contiguous buffer of field-transition raw pointers
    /// - `dfa_lookup`: 256-entry-per-state table for O(1) byte transitions (skipped under Miri)
    ///
    /// Must be called after all table modifications are complete (i.e., at freeze time).
    pub fn flatten_tables(&mut self) {
        self.flatten_ft_ptrs();
        self.build_dfa_lookup();
    }

    /// Flatten field-transition pointers into a contiguous buffer.
    ///
    /// Each state's `ft_start`/`ft_len` index into `self.ft_ptrs`, enabling
    /// `ft_ptrs_of()` to return a slice without touching per-state `SmallVec`s.
    fn flatten_ft_ptrs(&mut self) {
        let arena_len = self.states.len();
        if arena_len == 0 {
            return;
        }
        let mut ft_ptrs = Vec::new();

        for state_idx in 0..arena_len {
            let state = &self.states[state_idx];
            let ft_start = ft_ptrs.len();
            let ft_len = state.field_transitions.len();
            for ft in &state.field_transitions {
                ft_ptrs.push(Arc::as_ptr(ft) as usize);
            }
            self.states[state_idx].ft_start = ft_start as u32;
            self.states[state_idx].ft_len = ft_len as u8;
        }
        self.ft_ptrs = ft_ptrs;
    }

    /// Build a 256-entry-per-state lookup table for O(1) byte transitions.
    ///
    /// Skipped under Miri: the large array (states × 256) is expensive to
    /// interpret, and `dstep()` falls back to `ArenaSmallTable::dstep()` when
    /// `dfa_lookup` is empty. The fallback exercises the same transitions
    /// through the same unsafe `get_unchecked` pattern.
    #[cfg(not(miri))]
    fn build_dfa_lookup(&mut self) {
        let arena_len = self.states.len();
        if arena_len == 0 {
            return;
        }
        let mut dfa_lookup = vec![StateId::NONE; arena_len * 256];

        for state_idx in 0..arena_len {
            let state = &self.states[state_idx];
            let ceilings = state.table.ceilings.as_slice();
            let steps = state.table.steps.as_slice();

            let base = state_idx * 256;
            let mut prev_ceiling: u8 = 0;
            for (ci, &ceiling) in ceilings.iter().enumerate() {
                let step = steps[ci];
                for byte in prev_ceiling..ceiling {
                    dfa_lookup[base + byte as usize] = step;
                }
                prev_ceiling = ceiling;
            }
        }
        self.dfa_lookup = dfa_lookup;
    }

    /// No-op under Miri — `dstep()` falls back to `ArenaSmallTable::dstep()`.
    /// Correctness of the lookup table is verified by
    /// `tests::test_dfa_lookup_matches_smalltable_dstep` in non-Miri builds.
    #[cfg(miri)]
    fn build_dfa_lookup(&mut self) {}

    /// Convert an NFA arena to a DFA arena using the subset construction algorithm.
    ///
    /// Each DFA state corresponds to a set of NFA states (after epsilon closure).
    /// The algorithm explores all reachable byte transitions from each DFA state-set,
    /// computing the epsilon closure of the resulting NFA states to form the next
    /// DFA state.
    ///
    /// # Arguments
    /// * `start` - The NFA start state
    /// * `state_budget` - Maximum number of DFA states before aborting (prevents
    ///   exponential blowup). If exceeded, returns `None`.
    ///
    /// # Returns
    /// `Some((dfa_arena, dfa_start))` if conversion succeeded within budget,
    /// `None` if the budget was exceeded (caller should keep using NFA).
    ///
    /// Inspired by Go quamina's `nfa2Dfa` (PR #76) and sayrer's three-tier
    /// strategy from issue #481: eager DFA with low budget, then NFA fallback.
    pub fn nfa_to_dfa(&self, start: StateId, state_budget: usize) -> Option<(Self, StateId)> {
        if start.is_none() || self.states.is_empty() {
            return Some((Self::new(), StateId::NONE));
        }

        debug_assert!(
            !self.closure_data.is_empty(),
            "epsilon closures must be precomputed before nfa_to_dfa"
        );

        let mut dfa_arena = Self::with_capacity(self.states.len());

        // Map from sorted NFA state-set → DFA state ID.
        // Key: sorted Vec<StateId> representing the epsilon closure of a set of NFA states.
        let mut state_map: FxHashMap<Vec<u32>, StateId> = FxHashMap::default();

        // Work queue of DFA states to process.
        let mut worklist: Vec<StateId> = Vec::new();

        // Scratch buffers (reused across iterations to avoid allocation)
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        let mut dfa_unpacked = [StateId::NONE; BYTE_CEILING];
        let mut closure_set: Vec<StateId> = Vec::new();
        let mut seen: FxHashSet<StateId> = FxHashSet::default();
        // Per-byte next-state collector, hoisted out of the loop to reuse capacity.
        // Each entry is a Vec of NFA states reachable via that byte value.
        let mut byte_to_next: Vec<Vec<StateId>> = (0..BYTE_CEILING).map(|_| Vec::new()).collect();

        // Step 1: Compute the start DFA state = epsilon closure of NFA start
        let start_closure = self.closure_of(start);
        let start_key = Self::make_state_set_key(start_closure);
        let dfa_start = dfa_arena.alloc();
        state_map.insert(start_key, dfa_start);
        worklist.push(dfa_start);

        // Store the NFA state-set for each DFA state (indexed by DFA state index)
        let mut dfa_nfa_sets: Vec<Vec<StateId>> = Vec::new();
        dfa_nfa_sets.push(start_closure.to_vec());

        // Copy field transitions from the start closure
        Self::collect_field_transitions(self, start_closure, &mut dfa_arena[dfa_start]);

        // Step 2: Process worklist
        while let Some(dfa_state) = worklist.pop() {
            let nfa_states = dfa_nfa_sets[dfa_state.index()].clone();

            // Compute the combined transition table: for each byte, collect all
            // reachable NFA states (union of transitions from all states in the set)
            // then compute epsilon closure of the result.
            for v in &mut byte_to_next {
                v.clear();
            }

            for &nfa_state in &nfa_states {
                if nfa_state.is_none() {
                    continue;
                }
                // Unpack this NFA state's transition table
                unpack_arena_table(&self[nfa_state].table, &mut unpacked);

                for byte in 0..BYTE_CEILING {
                    let target = unpacked[byte];
                    if !target.is_none() {
                        // Expand target through epsilon closure
                        let target_closure = self.closure_of(target);
                        byte_to_next[byte].extend_from_slice(target_closure);
                    }
                }
            }

            // Build the DFA transition table: deduplicate and sort each byte's
            // NFA state-set, then intern it as a DFA state.
            dfa_unpacked.fill(StateId::NONE);

            for byte in 0..BYTE_CEILING {
                if byte_to_next[byte].is_empty() {
                    continue;
                }

                // Deduplicate and sort
                seen.clear();
                closure_set.clear();
                for &s in &byte_to_next[byte] {
                    if seen.insert(s) {
                        closure_set.push(s);
                    }
                }
                closure_set.sort_unstable_by_key(|s| s.0);

                let key = Self::make_state_set_key(&closure_set);

                let dfa_next = if let Some(&existing) = state_map.get(&key) {
                    existing
                } else {
                    // Budget check
                    if dfa_arena.len() >= state_budget {
                        return None; // Budget exceeded, abort
                    }

                    let new_dfa = dfa_arena.alloc();
                    state_map.insert(key, new_dfa);
                    dfa_nfa_sets.push(closure_set.clone());

                    // Collect field transitions for this new DFA state
                    Self::collect_field_transitions(self, &closure_set, &mut dfa_arena[new_dfa]);

                    worklist.push(new_dfa);
                    new_dfa
                };

                dfa_unpacked[byte] = dfa_next;
            }

            // Pack the transition table
            dfa_arena[dfa_state].table.pack(&dfa_unpacked);
        }

        // Precompute epsilon closures (should be trivial - all DFA states have no epsilons)
        dfa_arena.precompute_epsilon_closures();

        Some((dfa_arena, dfa_start))
    }

    /// Create a canonical key from a sorted set of NFA state IDs.
    fn make_state_set_key(states: &[StateId]) -> Vec<u32> {
        states.iter().map(|s| s.0).collect()
    }

    /// Collect field transitions from a set of NFA states onto a DFA state.
    fn collect_field_transitions(
        nfa_arena: &Self,
        nfa_states: &[StateId],
        dfa_state: &mut ArenaFaState,
    ) {
        let mut seen_ptrs: FxHashSet<usize> = FxHashSet::default();
        for &nfa_state in nfa_states {
            if nfa_state.is_none() {
                continue;
            }
            for ft in &nfa_arena[nfa_state].field_transitions {
                let ptr = Arc::as_ptr(ft) as usize;
                if seen_ptrs.insert(ptr) {
                    dfa_state.field_transitions.push(ft.clone());
                }
            }
        }
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

// =============================================================================
// Lazy DFA — on-demand DFA construction during matching
// =============================================================================

/// A cached lazy DFA state — built on-demand during matching.
///
/// Each lazy DFA state corresponds to a set of NFA states (after epsilon closure).
/// Transitions are computed lazily: the first time a byte is encountered, the
/// next NFA state-set is computed and cached.
#[derive(Clone)]
struct LazyDfaState {
    /// Transition table: byte → lazy DFA state index (u32::MAX = not yet computed).
    /// Allocated as a full 256-entry table for O(1) lookup.
    transitions: Vec<u32>,
    /// Field transition pointers collected from the NFA state-set.
    field_transition_ptrs: Vec<usize>,
    /// Whether this state is cached (persisted in the cache) or temporary.
    cached: bool,
}

/// Sentinel for "transition not yet computed".
const LAZY_DFA_UNKNOWN: u32 = u32::MAX;
/// Sentinel for "no valid transition" (dead state).
const LAZY_DFA_DEAD: u32 = u32::MAX - 1;

// Compile-time check: sentinels must be distinct so cached dead transitions
// are not re-computed on every byte.
const _: () = assert!(
    LAZY_DFA_UNKNOWN != LAZY_DFA_DEAD,
    "LAZY_DFA_UNKNOWN and LAZY_DFA_DEAD must be distinct sentinels"
);

/// A lazy DFA cache that builds DFA states on-demand during matching.
///
/// Implements tier 2 of the three-tier strategy from Go quamina issue #481:
/// eager DFA (tier 1) → **lazy DFA** (tier 2) → NFA fallback (tier 3).
///
/// The cache has a state budget. When full, new states are created as temporary
/// (uncached) and discarded after the current traversal, but the traversal can
/// still "snap back" to a cached state on the next byte transition.
pub struct LazyDfa {
    /// The underlying NFA arena (shared, not mutated).
    nfa_arena: StateArena,
    /// Cached DFA states.
    states: Vec<LazyDfaState>,
    /// Map from NFA state-set key → DFA state index.
    state_map: FxHashMap<Vec<u32>, u32>,
    /// NFA state-sets for each DFA state (indexed by DFA state index).
    nfa_sets: Vec<Vec<StateId>>,
    /// Maximum number of cached DFA states.
    state_budget: usize,
    /// Number of currently cached states.
    cached_count: usize,
}

impl LazyDfa {
    /// Create a new lazy DFA wrapping the given NFA arena.
    ///
    /// # Arguments
    /// * `nfa_arena` - The NFA arena (must have precomputed epsilon closures)
    /// * `nfa_start` - The NFA start state
    /// * `state_budget` - Maximum number of cached DFA states
    pub fn new(nfa_arena: StateArena, nfa_start: StateId, state_budget: usize) -> Self {
        let mut lazy = Self {
            nfa_arena,
            states: Vec::new(),
            state_map: FxHashMap::default(),
            nfa_sets: Vec::new(),
            state_budget,
            cached_count: 0,
        };

        // Create the start state from the NFA start's epsilon closure
        if !nfa_start.is_none() {
            let closure = lazy.nfa_arena.closure_of(nfa_start).to_vec();
            lazy.intern_state(&closure, true);
        }

        lazy
    }

    /// Intern an NFA state-set into the lazy DFA, returning its index.
    ///
    /// If the set is already cached, returns the existing index.
    /// If `allow_cache` is true and budget permits, the state is cached;
    /// otherwise it is created as a temporary (uncached) state.
    fn intern_state(&mut self, nfa_states: &[StateId], allow_cache: bool) -> u32 {
        let key = StateArena::make_state_set_key(nfa_states);

        if let Some(&idx) = self.state_map.get(&key) {
            return idx;
        }

        let can_cache = allow_cache && self.cached_count < self.state_budget;
        let idx = self.states.len() as u32;

        // Collect field transitions from the NFA state-set
        let mut field_transition_ptrs = Vec::new();
        let mut seen_ptrs: FxHashSet<usize> = FxHashSet::default();
        for &nfa_state in nfa_states {
            if nfa_state.is_none() {
                continue;
            }
            for ft in &self.nfa_arena[nfa_state].field_transitions {
                let ptr = Arc::as_ptr(ft) as usize;
                if seen_ptrs.insert(ptr) {
                    field_transition_ptrs.push(ptr);
                }
            }
        }

        let state = LazyDfaState {
            transitions: vec![LAZY_DFA_UNKNOWN; BYTE_CEILING],
            field_transition_ptrs,
            cached: can_cache,
        };

        self.states.push(state);
        self.nfa_sets.push(nfa_states.to_vec());

        if can_cache {
            self.state_map.insert(key, idx);
            self.cached_count += 1;
        }

        idx
    }

    /// Compute the transition for a given DFA state and byte value.
    ///
    /// If the transition is not yet computed, performs the NFA state-set
    /// computation (union of byte transitions + epsilon closure) and interns
    /// the resulting state.
    fn step(&mut self, state_idx: u32, byte: u8, scratch: &mut LazyDfaScratch) -> u32 {
        let cached = self.states[state_idx as usize].transitions[byte as usize];
        if cached != LAZY_DFA_UNKNOWN {
            return cached;
        }

        // Compute the next NFA state-set for this byte
        let nfa_states = &self.nfa_sets[state_idx as usize];

        scratch.next_nfa_states.clear();
        scratch.seen.clear();

        for &nfa_state in nfa_states {
            if nfa_state.is_none() {
                continue;
            }
            let next = self.nfa_arena.dstep(nfa_state, byte);
            if !next.is_none() {
                // Expand through epsilon closure
                let closure = self.nfa_arena.closure_of(next);
                for &cs in closure {
                    if scratch.seen.insert(cs) {
                        scratch.next_nfa_states.push(cs);
                    }
                }
            }
        }

        if scratch.next_nfa_states.is_empty() {
            self.states[state_idx as usize].transitions[byte as usize] = LAZY_DFA_DEAD;
            return LAZY_DFA_DEAD;
        }

        // Sort for canonical key
        scratch.next_nfa_states.sort_unstable_by_key(|s| s.0);

        let next_idx = self.intern_state(&scratch.next_nfa_states, true);

        // Cache the transition on the current state (only if it's a cached state)
        if self.states[state_idx as usize].cached {
            self.states[state_idx as usize].transitions[byte as usize] = next_idx;
        }

        next_idx
    }
}

/// Scratch buffers for lazy DFA traversal (avoids per-step allocation).
#[derive(Default)]
struct LazyDfaScratch {
    next_nfa_states: Vec<StateId>,
    seen: FxHashSet<StateId>,
}

/// Traverse a value through a lazy DFA, collecting field transitions.
///
/// This is the tier-2 matching path: faster than NFA traversal (single state
/// tracked, cached transitions), but uses more memory for the cache.
///
/// # Arguments
/// * `lazy_dfa` - The lazy DFA cache (mutated to cache new states)
/// * `val` - The value bytes to match against
/// * `transitions` - Output: collected field matcher pointer addresses
pub fn traverse_lazy_dfa(lazy_dfa: &mut LazyDfa, val: &[u8], transitions: &mut Vec<usize>) {
    if lazy_dfa.states.is_empty() {
        return;
    }

    let mut scratch = LazyDfaScratch::default();
    let mut current: u32 = 0; // Start state is always index 0

    // Collect field transitions from start state
    transitions.extend_from_slice(&lazy_dfa.states[0].field_transition_ptrs);

    for i in 0..=val.len() {
        let byte = if i < val.len() {
            val[i]
        } else {
            ARENA_VALUE_TERMINATOR
        };

        let next = lazy_dfa.step(current, byte, &mut scratch);
        if next == LAZY_DFA_DEAD {
            return;
        }
        current = next;

        // Collect field transitions
        transitions.extend_from_slice(&lazy_dfa.states[current as usize].field_transition_ptrs);
    }
}

/// Buffers for arena NFA traversal (avoid allocation during matching).
#[derive(Default)]
pub struct ArenaNfaBuffers {
    /// Current active states
    pub current_states: Vec<StateId>,
    /// Next states after transition
    pub next_states: Vec<StateId>,
    /// Accumulated field matcher transitions (stored as pointer addresses to avoid Arc::clone).
    pub transitions: Vec<usize>,
    /// Seen field matcher transitions (for deduplication, stored as pointer addresses).
    seen_transitions: FxHashSet<usize>,
    /// Generation counter for O(n) state dedup during NFA traversal.
    step_gen: u64,
    /// Map from StateId index to last-seen generation, for dedup.
    seen_states: FxHashMap<StateId, u64>,
}

impl ArenaNfaBuffers {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity() -> Self {
        Self {
            current_states: Vec::with_capacity(16),
            next_states: Vec::with_capacity(16),
            transitions: Vec::new(),
            seen_transitions: FxHashSet::default(),
            step_gen: 0,
            seen_states: FxHashMap::default(),
        }
    }

    pub fn clear(&mut self) {
        self.current_states.clear();
        self.next_states.clear();
        self.transitions.clear();
        self.seen_transitions.clear();
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
        if i < len && bufs.current_states.len() == 1 {
            let state_id = bufs.current_states[0];
            let state = &arena[state_id];
            if let Some(skip) = try_accelerate_arena(&state.table, &val[i..])
                && skip > 0
            {
                i += skip;
                continue;
            }
        }

        let byte = if i < len {
            val[i]
        } else {
            ARENA_VALUE_TERMINATOR
        };

        // Destructure bufs for split borrows: iterate current_states immutably
        // while pushing to next_states mutably.
        let ArenaNfaBuffers {
            ref mut current_states,
            ref mut next_states,
            ref mut transitions,
            ref mut seen_transitions,
            ref mut step_gen,
            ref mut seen_states,
        } = *bufs;

        if !arena.ft_ptrs.is_empty() {
            // Frozen path: use precomputed flat buffers
            for &state_id in current_states.iter() {
                let closure = arena.closure_of(state_id);

                if closure.len() == 1 {
                    for &ptr in arena.ft_ptrs_of(state_id) {
                        if seen_transitions.insert(ptr) {
                            transitions.push(ptr);
                        }
                    }
                    let next = arena.dstep(state_id, byte);
                    if !next.is_none() {
                        next_states.push(next);
                    }
                } else {
                    for &ec_state_id in closure {
                        for &ptr in arena.ft_ptrs_of(ec_state_id) {
                            if seen_transitions.insert(ptr) {
                                transitions.push(ptr);
                            }
                        }
                        let next = arena.dstep(ec_state_id, byte);
                        if !next.is_none() {
                            next_states.push(next);
                        }
                    }
                }
            }
        } else {
            // Mutable/test path: read field_transitions directly
            for &state_id in current_states.iter() {
                let closure = arena.closure_of(state_id);

                if closure.len() == 1 {
                    for ft in &arena[state_id].field_transitions {
                        let ptr = Arc::as_ptr(ft) as usize;
                        if seen_transitions.insert(ptr) {
                            transitions.push(ptr);
                        }
                    }
                    let next = arena.dstep(state_id, byte);
                    if !next.is_none() {
                        next_states.push(next);
                    }
                } else {
                    for &ec_state_id in closure {
                        for ft in &arena[ec_state_id].field_transitions {
                            let ptr = Arc::as_ptr(ft) as usize;
                            if seen_transitions.insert(ptr) {
                                transitions.push(ptr);
                            }
                        }
                        let next = arena.dstep(ec_state_id, byte);
                        if !next.is_none() {
                            next_states.push(next);
                        }
                    }
                }
            }
        }

        // Nested quantifiers like (([abc]?)*)+ create epsilon loops that
        // cause duplicate states to compound exponentially across steps.
        // Dedup in-place using a generation counter when growth is detected.
        if next_states.len() > 64 {
            *step_gen += 1;
            let generation = *step_gen;
            let mut j = 0;
            for i_ns in 0..next_states.len() {
                let state = next_states[i_ns];
                if seen_states.get(&state).copied() != Some(generation) {
                    seen_states.insert(state, generation);
                    next_states[j] = state;
                    j += 1;
                }
            }
            next_states.truncate(j);
        }

        // Swap buffers — clear+swap preserves capacity on both Vecs
        current_states.clear();
        std::mem::swap(current_states, next_states);
        i += 1;
    }

    // Check final states for matches (split borrows to avoid take)
    let ArenaNfaBuffers {
        ref current_states,
        ref mut transitions,
        ref mut seen_transitions,
        ..
    } = *bufs;
    if !arena.ft_ptrs.is_empty() {
        for &state_id in current_states.iter() {
            let closure = arena.closure_of(state_id);
            if closure.len() == 1 {
                for &ptr in arena.ft_ptrs_of(state_id) {
                    if seen_transitions.insert(ptr) {
                        transitions.push(ptr);
                    }
                }
            } else {
                for &ec_state_id in closure {
                    for &ptr in arena.ft_ptrs_of(ec_state_id) {
                        if seen_transitions.insert(ptr) {
                            transitions.push(ptr);
                        }
                    }
                }
            }
        }
    } else {
        for &state_id in current_states.iter() {
            let closure = arena.closure_of(state_id);
            if closure.len() == 1 {
                for ft in &arena[state_id].field_transitions {
                    let ptr = Arc::as_ptr(ft) as usize;
                    if seen_transitions.insert(ptr) {
                        transitions.push(ptr);
                    }
                }
            } else {
                for &ec_state_id in closure {
                    for ft in &arena[ec_state_id].field_transitions {
                        let ptr = Arc::as_ptr(ft) as usize;
                        if seen_transitions.insert(ptr) {
                            transitions.push(ptr);
                        }
                    }
                }
            }
        }
    }
}

/// Fast DFA traversal for arena-based automata.
///
/// This is the arena equivalent of the old chain-based `traverse_dfa`.
/// For pure DFA patterns (no epsilon transitions), this is significantly faster
/// than `traverse_arena_nfa` because it follows a single state pointer per byte
/// with no buffer management overhead.
///
/// The caller must ensure the arena is a pure DFA (no epsilon transitions).
/// For NFA patterns, use `traverse_arena_nfa`.
#[inline]
pub fn traverse_arena_dfa(
    arena: &StateArena,
    start: StateId,
    val: &[u8],
    transitions: &mut Vec<usize>,
) {
    if start.is_none() {
        return;
    }

    let has_flat = !arena.ft_ptrs.is_empty();
    let mut current = start;

    for i in 0..=val.len() {
        if has_flat {
            for &ptr in arena.ft_ptrs_of(current) {
                transitions.push(ptr);
            }
        } else {
            for ft in &arena[current].field_transitions {
                transitions.push(Arc::as_ptr(ft) as usize);
            }
        }

        let byte = if i < val.len() {
            val[i]
        } else {
            ARENA_VALUE_TERMINATOR
        };

        let next = arena.dstep(current, byte);
        if next.is_none() {
            return;
        }
        current = next;
    }

    if has_flat {
        for &ptr in arena.ft_ptrs_of(current) {
            transitions.push(ptr);
        }
    } else {
        for ft in &arena[current].field_transitions {
            transitions.push(Arc::as_ptr(ft) as usize);
        }
    }
}

/// Fast backward DFA traversal for suffix matching.
///
/// Walks value bytes right-to-left through a DFA trie built from reversed suffix
/// patterns. This is O(max_suffix_len) — it only touches the last few bytes of the
/// value, exiting as soon as the trie has no transition.
///
/// The trie is built without the ARENA_VALUE_TERMINATOR convention. Field transitions
/// on intermediate/leaf states mark suffix matches of various lengths.
#[inline]
pub fn traverse_arena_dfa_backward(
    arena: &StateArena,
    start: StateId,
    val: &[u8],
    transitions: &mut Vec<usize>,
) {
    if start.is_none() || val.is_empty() {
        return;
    }

    let mut current = start;

    // Walk backward through value bytes (right to left)
    for i in (0..val.len()).rev() {
        let next = arena.dstep(current, val[i]);
        if next.is_none() {
            return;
        }
        current = next;

        // Collect field_transitions (suffix match found at this depth)
        if !arena.ft_ptrs.is_empty() {
            for &ptr in arena.ft_ptrs_of(current) {
                transitions.push(ptr);
            }
        } else {
            for ft in &arena[current].field_transitions {
                transitions.push(Arc::as_ptr(ft) as usize);
            }
        }
    }
}

/// Merge two arena-based DFAs into one that matches either pattern.
///
/// This is the arena equivalent of `merge_fas` for chain-based FAs.
/// For DFA-only patterns (no epsilons), this is a simplified merge
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
    let mut memo: FxHashMap<MemoKey, StateId> = FxHashMap::default();
    let mut new_arena = StateArena::new();

    let start =
        merge_arena_states_recursive(arena1, start1, arena2, start2, &mut new_arena, &mut memo);

    new_arena.precompute_epsilon_closures();
    (new_arena, start)
}

/// Clone a subset of an arena starting from a given state.
fn clone_arena_subset(arena: &StateArena, start: StateId) -> (StateArena, StateId) {
    if start.is_none() {
        return (StateArena::new(), StateId::NONE);
    }

    let mut new_arena = StateArena::new();
    let mut id_map: FxHashMap<u32, StateId> = FxHashMap::default();

    clone_state_recursive(arena, start, &mut new_arena, &mut id_map);

    let new_start = id_map.get(&start.0).copied().unwrap_or(StateId::NONE);
    new_arena.precompute_epsilon_closures();
    (new_arena, new_start)
}

/// Recursively clone a state and its descendants.
fn clone_state_recursive(
    arena: &StateArena,
    state_id: StateId,
    new_arena: &mut StateArena,
    id_map: &mut FxHashMap<u32, StateId>,
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
    memo: &mut FxHashMap<(i32, i32), StateId>,
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
    memo: &mut FxHashMap<(i32, i32), StateId>,
    is_arena1: bool,
) -> ArenaSmallTable {
    let mut new_table = ArenaSmallTable {
        ceilings: table.ceilings.clone(),
        steps: SmallVec::with_capacity(table.steps.len()),
        epsilons: SmallVec::with_capacity(table.epsilons.len()),
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

    new_table
}

/// Merge two arena tables byte-by-byte.
fn merge_arena_tables(
    arena1: &StateArena,
    table1: &ArenaSmallTable,
    arena2: &StateArena,
    table2: &ArenaSmallTable,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(i32, i32), StateId>,
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
/// - Spinout states (for wildcard patterns like `*`, self-loop encoded in table)
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
    let mut memo: FxHashMap<MemoKey, StateId> = FxHashMap::default();
    let mut new_arena = StateArena::new();

    let start =
        merge_arena_nfa_states_recursive(arena1, start1, arena2, start2, &mut new_arena, &mut memo);

    // Precompute epsilon closures for all states in the merged arena.
    // This eliminates per-byte DFS computation during NFA traversal.
    new_arena.precompute_epsilon_closures();

    (new_arena, start)
}

/// Check if a state is an "epsilon-only" splice state created during merges.
///
/// These synthetic states only serve to branch into multiple epsilon targets,
/// with no byte transitions or field transitions.
/// Mirrors Go's `smallTable.isEpsilonOnly()`, with additional guard for
/// Rust's field_transition field.
fn is_epsilon_only_state(arena: &StateArena, state_id: StateId) -> bool {
    if state_id.is_none() {
        return false;
    }
    let state = &arena[state_id];
    !state.table.epsilons.is_empty()
        && state.table.ceilings.len() == 1
        && state.table.default.is_none()
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

/// Check if a state is a spinner/spinout state (self-loop in transition table).
///
/// Spinner states are used for wildcard patterns. The convention is:
/// - State's table default points to itself (self-loop via `make_byte_dot_table`)
/// - Old construction: 1 epsilon (to continuation after the wildcard)
/// - New construction: 0 epsilons (escape states have epsilon back to spinner)
fn is_spinout_state(arena: &StateArena, state_id: StateId) -> bool {
    if state_id.is_none() {
        return false;
    }
    let state = &arena[state_id];
    state.table.default == state_id && state.table.epsilons.len() <= 1
}

/// Recursively merge two NFA states from different arenas.
///
/// This handles the full NFA merge including epsilons and spinout states.
fn merge_arena_nfa_states_recursive(
    arena1: &StateArena,
    state1: StateId,
    arena2: &StateArena,
    state2: StateId,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(i32, i32), StateId>,
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
    // The byte-dot self-loop entries are merged by merge_nfa_tables_bytewise
    // (they recurse back to this merge, hit the memo, and return new_id).
    // We set default = new_id to mark the merged state as a spinout.
    if s1_has_spinout && s2_has_spinout {
        let mut combined_table =
            merge_nfa_tables_bytewise(arena1, &s1.table, arena2, &s2.table, new_arena, memo);

        combined_table.default = new_id;

        // Merge epsilons from both spinners (0 or 1 each)
        let mut merged_epsilons: SmallVec<[StateId; 2]> = SmallVec::new();
        for &eps1 in &s1.table.epsilons {
            let merged = merge_arena_nfa_states_recursive(
                arena1,
                eps1,
                arena2,
                StateId::NONE,
                new_arena,
                memo,
            );
            if !merged.is_none() {
                merged_epsilons.push(merged);
            }
        }
        for &eps2 in &s2.table.epsilons {
            let merged = merge_arena_nfa_states_recursive(
                arena1,
                StateId::NONE,
                arena2,
                eps2,
                new_arena,
                memo,
            );
            if !merged.is_none() {
                merged_epsilons.push(merged);
            }
        }
        combined_table.epsilons = merged_epsilons;

        let mut field_transitions = s1.field_transitions.clone();
        field_transitions.extend(s2.field_transitions.iter().cloned());

        new_arena[new_id].table = combined_table;
        new_arena[new_id].field_transitions = field_transitions;
        return new_id;
    }

    // Case 2: Asymmetric spinner merge - one spinout, other has no epsilons
    // Mirrors Go's asymmetricSpinnerMerge: when a spinout is merged with a
    // non-epsilon state, we can avoid creating splice states by inlining the
    // epsilon-to-spinner relationship into the merged table.
    if (s1_has_spinout && !s2_has_epsilons) || (s2_has_spinout && !s1_has_epsilons) {
        let (spinner_arena, spinner_id, spinner_table, other_arena, _other_id, other_table) =
            if s1_has_spinout {
                (arena1, state1, &s1.table, arena2, state2, &s2.table)
            } else {
                (arena2, state2, &s2.table, arena1, state1, &s1.table)
            };

        // Unpack both tables to 256-element arrays
        let mut spinner_unpacked = [StateId::NONE; BYTE_CEILING];
        let mut other_unpacked = [StateId::NONE; BYTE_CEILING];
        unpack_arena_table(spinner_table, &mut spinner_unpacked);
        unpack_arena_table(other_table, &mut other_unpacked);

        // For each byte, decide how to merge
        let mut merged_unpacked = [StateId::NONE; BYTE_CEILING];
        for i in 0..BYTE_CEILING {
            let spinner_next = spinner_unpacked[i];
            let other_next = other_unpacked[i];

            if spinner_next.is_none() {
                // Illegal UTF-8 byte
                merged_unpacked[i] = StateId::NONE;
            } else if other_next.is_none() {
                // Only spinner has a transition - remap it
                if spinner_next == spinner_id {
                    merged_unpacked[i] = new_id; // self-loop maps to combined
                } else {
                    // Spinner has a real branch (not self-loop)
                    merged_unpacked[i] = if s1_has_spinout {
                        merge_arena_nfa_states_recursive(
                            spinner_arena,
                            spinner_next,
                            other_arena,
                            StateId::NONE,
                            new_arena,
                            memo,
                        )
                    } else {
                        merge_arena_nfa_states_recursive(
                            other_arena,
                            StateId::NONE,
                            spinner_arena,
                            spinner_next,
                            new_arena,
                            memo,
                        )
                    };
                }
            } else if spinner_next == spinner_id {
                // Spinner self-loops here AND other has a branch.
                // Create a state with other's transitions + epsilon back to combined.
                // This is the key optimization: avoid full merge, just add epsilon.
                let remapped_other = if s1_has_spinout {
                    merge_arena_nfa_states_recursive(
                        spinner_arena,
                        StateId::NONE,
                        other_arena,
                        other_next,
                        new_arena,
                        memo,
                    )
                } else {
                    merge_arena_nfa_states_recursive(
                        other_arena,
                        other_next,
                        spinner_arena,
                        StateId::NONE,
                        new_arena,
                        memo,
                    )
                };
                // Add epsilon from the remapped other state back to the combined spinner
                if !remapped_other.is_none() {
                    new_arena[remapped_other].table.epsilons.push(new_id);
                    // Also copy spinner's field transitions to the escape state
                    let spinner_fts = if s1_has_spinout {
                        &arena1[state1].field_transitions
                    } else {
                        &arena2[state2].field_transitions
                    };
                    for ft in spinner_fts {
                        new_arena[remapped_other].field_transitions.push(ft.clone());
                    }
                }
                merged_unpacked[i] = remapped_other;
            } else {
                // Spinner has a real branch (not self-loop) AND other has a branch.
                // Merge them, then add epsilon back to combined spinner.
                let merged_branch = if s1_has_spinout {
                    merge_arena_nfa_states_recursive(
                        spinner_arena,
                        spinner_next,
                        other_arena,
                        other_next,
                        new_arena,
                        memo,
                    )
                } else {
                    merge_arena_nfa_states_recursive(
                        other_arena,
                        other_next,
                        spinner_arena,
                        spinner_next,
                        new_arena,
                        memo,
                    )
                };
                if !merged_branch.is_none() {
                    new_arena[merged_branch].table.epsilons.push(new_id);
                }
                merged_unpacked[i] = merged_branch;
            }
        }

        // Pack the merged table
        let mut combined_table = ArenaSmallTable::new();
        combined_table.pack(&merged_unpacked);
        combined_table.default = new_id; // self-loop for the combined spinner

        // Remap spinner's epsilons (0 or 1)
        let mut merged_epsilons: SmallVec<[StateId; 2]> = SmallVec::new();
        for &spinner_eps in &spinner_table.epsilons {
            let merged = if s1_has_spinout {
                merge_arena_nfa_states_recursive(
                    spinner_arena,
                    spinner_eps,
                    other_arena,
                    StateId::NONE,
                    new_arena,
                    memo,
                )
            } else {
                merge_arena_nfa_states_recursive(
                    other_arena,
                    StateId::NONE,
                    spinner_arena,
                    spinner_eps,
                    new_arena,
                    memo,
                )
            };
            if !merged.is_none() {
                merged_epsilons.push(merged);
            }
        }
        combined_table.epsilons = merged_epsilons;

        // Combine field transitions
        let mut field_transitions = s1.field_transitions.clone();
        field_transitions.extend(s2.field_transitions.iter().cloned());

        new_arena[new_id].table = combined_table;
        new_arena[new_id].field_transitions = field_transitions;
        return new_id;
    }

    // Case 3: Either has epsilons (but not both spinouts) - create splice
    // Flatten epsilon targets to prevent deep nesting from repeated merges.
    // (Mirrors Go PR #486: flattenEpsilonTargets)
    if s1_has_epsilons || s2_has_epsilons {
        let mut clone_map1: FxHashMap<u32, StateId> = FxHashMap::default();
        let mut clone_map2: FxHashMap<u32, StateId> = FxHashMap::default();
        let cloned1 = clone_state_into_arena(arena1, state1, new_arena, &mut clone_map1);
        let cloned2 = clone_state_into_arena(arena2, state2, new_arena, &mut clone_map2);

        // Flatten: if cloned states are themselves epsilon-only splices,
        // collect their real targets directly instead of nesting splices.
        let epsilons = flatten_epsilon_targets(new_arena, &[cloned1, cloned2]);

        new_arena[new_id].table = ArenaSmallTable {
            ceilings: smallvec![BYTE_CEILING as u8],
            steps: smallvec![StateId::NONE],
            epsilons,
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
    id_map: &mut FxHashMap<u32, StateId>,
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
    memo: &mut FxHashMap<(i32, i32), StateId>,
    is_arena1: bool,
) -> ArenaSmallTable {
    let mut new_table = ArenaSmallTable {
        ceilings: table.ceilings.clone(),
        steps: SmallVec::with_capacity(table.steps.len()),
        epsilons: SmallVec::with_capacity(table.epsilons.len()),
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

    new_table
}

/// Merge two NFA tables byte-by-byte.
fn merge_nfa_tables_bytewise(
    arena1: &StateArena,
    table1: &ArenaSmallTable,
    arena2: &StateArena,
    table2: &ArenaSmallTable,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(i32, i32), StateId>,
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

    arena.precompute_epsilon_closures();
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

    arena.precompute_epsilon_closures();
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

    arena.precompute_epsilon_closures();
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

    arena.precompute_epsilon_closures();
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

/// Build a DFA trie for a single reversed suffix pattern.
///
/// Unlike `make_string_arena_fa`, this does NOT append ARENA_VALUE_TERMINATOR.
/// The reversed bytes are inserted as-is, with field_transitions on the final state.
///
/// # Arguments
/// * `reversed_bytes` - The reversed suffix bytes (e.g., `['"', '0', '5', 't', 'x', 'e', '.']`)
/// * `next_field` - The field matcher to transition to on match
///
/// # Returns
/// A new arena containing the suffix DFA and its start state
pub fn make_suffix_dfa(
    reversed_bytes: &[u8],
    next_field: Arc<FieldMatcher>,
) -> (StateArena, StateId) {
    let mut arena = StateArena::new();

    // Create the match state with field_transitions
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Build chain backwards: last byte → ... → first byte → start
    let mut target = match_state;
    for &byte in reversed_bytes.iter().rev() {
        target = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[byte],
            &[target],
        ));
    }

    arena.precompute_epsilon_closures();
    (arena, target) // target is the start state
}

/// Insert a reversed suffix pattern into an existing suffix DFA trie.
///
/// Like `insert_string_into_arena` but without the ARENA_VALUE_TERMINATOR.
/// Shares prefix structure with existing patterns in the trie.
pub fn insert_suffix_into_arena(
    arena: &mut StateArena,
    start: StateId,
    reversed_bytes: &[u8],
    field_matcher: Arc<FieldMatcher>,
) {
    let mut current = start;

    for (i, &byte) in reversed_bytes.iter().enumerate() {
        let next = arena[current].table.dstep(byte);
        if !next.is_none() {
            // Transition exists, follow it
            current = next;
        } else {
            // No transition — create the remaining chain
            let match_state = arena.alloc();
            arena[match_state].field_transitions.push(field_matcher);

            // Build chain backwards for remaining bytes after this one
            let mut target = match_state;
            for &b in reversed_bytes[i + 1..].iter().rev() {
                target = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[b],
                    &[target],
                ));
            }

            // Connect current state to the new chain
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

    arena.precompute_epsilon_closures();
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

    // Build the FA byte-by-byte, mirroring Go's makeShellStyleFA.
    //
    // Go's approach processes one byte at a time. When encountering '*':
    //   1. Current state becomes an epsilon-only junction (branch point)
    //   2. Create a spinner with self-loop on all bytes
    //   3. Create an escape state with epsilon back to the spinner
    //   4. Override the spinner's transition for the NEXT byte to go to escape
    //   5. Advance past the next byte (it's consumed as the escape trigger)
    //
    // The junction gives insert_string_into_arena a clean branch point (dstep
    // returns NONE on the junction for any byte), while spinner states have
    // closure size 1 during self-loop, reducing dstep calls by ~2x.
    let start = arena.alloc();
    let mut state = start;
    let mut i = 0;

    while i < pattern.len() {
        let ch = pattern[i];
        if ch == b'*' {
            // Current state becomes an epsilon-only junction before the spinner.
            // This gives insert_string_into_arena a clean branch point: dstep on
            // the junction returns NONE for any byte, so exact string paths get
            // their own separate states instead of following the spinner's self-loop.
            let spinner = arena.alloc();
            arena[spinner].table = make_byte_dot_table(spinner);
            arena[state].table.epsilons.push(spinner);

            i += 1;
            if i < pattern.len() {
                // Create escape state with epsilon back to spinner
                let spin_escape = arena.alloc();
                arena[spin_escape].table.epsilons.push(spinner);
                // Override spinner's transition for the next byte to go to escape
                arena[spinner].table.set_transition(pattern[i], spin_escape);
                state = spin_escape;
            } else {
                // '*' is the last byte: state becomes the spinner so the
                // VT transition added after the loop goes on the spinner.
                state = spinner;
            }
        } else {
            // Literal byte: transition to a new state
            let next_step = arena.alloc();
            arena[state].table.set_transition(ch, next_step);
            state = next_step;
        }
        i += 1;
    }

    // Add VALUE_TERMINATOR → last_step on the final state.
    // This works for all cases:
    //   - Literal ending: state is a fresh alloc, set_transition adds VT
    //   - Wildcard ending: state is a spinner, set_transition overrides VT in dot table
    //   - Escape ending: state has epsilons, set_transition preserves them
    let last_step = arena.alloc();
    arena[last_step].field_transitions = arena[match_state].field_transitions.clone();
    arena[state]
        .table
        .set_transition(ARENA_VALUE_TERMINATOR, last_step);

    arena.precompute_epsilon_closures();
    (arena, start)
}

/// Segment types for shellstyle patterns
#[derive(Debug)]
enum ShellstyleSegment {
    Literal(Vec<u8>),
    Wildcard,
}

/// Build an FA from parsed segments using left-to-right construction.
///
/// This mirrors Go's construction approach where spinner (spinout) states have
/// direct byte exits for the next literal's first byte, rather than using epsilon
/// transitions. This eliminates unnecessary epsilon closures and reduces the
/// number of dstep calls during NFA traversal.
///
/// Used by `make_wildcard_arena_fa` (which needs segment parsing for escape sequences).
fn build_fa_from_segments(
    segments: &[ShellstyleSegment],
    match_state: StateId,
    arena: &mut StateArena,
) -> StateId {
    let start = arena.alloc();
    let mut state = start;
    let mut skip_first_literal_byte = false;

    for (seg_idx, seg) in segments.iter().enumerate() {
        match seg {
            ShellstyleSegment::Literal(bytes) => {
                // When preceded by a wildcard, the first byte was already consumed
                // as the spinner's escape trigger — skip it here.
                let byte_start = if skip_first_literal_byte { 1 } else { 0 };
                skip_first_literal_byte = false;

                for &ch in &bytes[byte_start..] {
                    let next_step = arena.alloc();
                    // set_transition preserves existing epsilons on the state
                    arena[state].table.set_transition(ch, next_step);
                    state = next_step;
                }
            }
            ShellstyleSegment::Wildcard => {
                // Current state becomes an epsilon-only junction before the spinner.
                // This gives insert_string_into_arena a clean branch point.
                let spinner = arena.alloc();
                arena[spinner].table = make_byte_dot_table(spinner);
                arena[state].table.epsilons.push(spinner);

                // Look ahead: if next segment is a literal, create escape with
                // direct byte exit on the literal's first byte
                if let Some(ShellstyleSegment::Literal(next_bytes)) = segments.get(seg_idx + 1)
                    && !next_bytes.is_empty()
                {
                    let spin_escape = arena.alloc();
                    arena[spin_escape].table.epsilons.push(spinner);
                    arena[spinner]
                        .table
                        .set_transition(next_bytes[0], spin_escape);
                    state = spin_escape;
                    skip_first_literal_byte = true;
                }

                // If wildcard is last or followed by another wildcard,
                // state becomes the spinner for VT transition at the end.
                if !skip_first_literal_byte {
                    state = spinner;
                }
            }
        }
    }

    // Unconditionally add VALUE_TERMINATOR → last_step on the final state.
    // This works correctly for all endings:
    //   - Literal ending: state is a fresh alloc, set_transition adds VT
    //   - Wildcard ending: state is a spinner, set_transition overrides VT in dot table
    //   - Escape ending: state has epsilons, set_transition preserves them
    let last_step = arena.alloc();
    arena[last_step].field_transitions = arena[match_state].field_transitions.clone();
    arena[state]
        .table
        .set_transition(ARENA_VALUE_TERMINATOR, last_step);

    start
}

/// Create a spinout loopback table that maps most valid UTF-8 bytes to `dest`.
///
/// This matches Go's `makeByteDotFA(dest)`. The table maps:
/// - `[0x00, 0xC0)` → dest (valid single-byte UTF-8 range)
/// - `[0xC0, 0xC2)` → NONE (illegal UTF-8 lead bytes)
/// - `[0xC2, 0xF5)` → dest (valid multi-byte UTF-8 lead bytes)
/// - `[0xF5, 0xF6)` → NONE (value terminator 0xF5, excluded from BYTE_CEILING)
///
/// This encodes the wildcard self-loop directly in the transition table,
/// eliminating the need for a separate spinout check in the traversal loop.
fn make_byte_dot_table(dest: StateId) -> ArenaSmallTable {
    let mut table = ArenaSmallTable::new();
    table.ceilings = smallvec![0xC0, 0xC2, ARENA_VALUE_TERMINATOR, BYTE_CEILING as u8];
    table.steps = smallvec![dest, StateId::NONE, dest, StateId::NONE];
    table.default = dest;
    table
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

    // Build the FA from segments
    let start = build_fa_from_segments(&segments, match_state, &mut arena);

    arena.precompute_epsilon_closures();
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
            if let Some(ShellstyleSegment::Literal(bytes)) = segments.last_mut() {
                bytes.push(escaped);
            } else {
                segments.push(ShellstyleSegment::Literal(vec![escaped]));
            }
            i += 2;
        } else {
            // Regular character - add to literal segment
            if let Some(ShellstyleSegment::Literal(bytes)) = segments.last_mut() {
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

    arena.precompute_epsilon_closures();
    (arena, start)
}

/// Build one step of the anything-but arena automaton.
fn build_anything_but_step(
    vals: &[Vec<u8>],
    index: usize,
    success: StateId,
    arena: &mut StateArena,
) -> StateId {
    // Group values by the byte at current index
    let mut vals_with_bytes_remaining: FxHashMap<u8, Vec<&Vec<u8>>> = FxHashMap::default();
    let mut vals_ending_here: FxHashSet<u8> = FxHashSet::default();

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
    let all_bytes: FxHashSet<u8> = vals_with_bytes_remaining
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
    let start = if val.is_empty() {
        arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ))
    } else if let Ok(s) = std::str::from_utf8(val) {
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
        build_monocase_arena_recursive(&chars, 0, match_state, &mut arena)
    } else {
        // Invalid UTF-8 - fall back to ASCII-only case folding
        build_monocase_ascii_chain(val, match_state, &mut arena)
    };

    arena.precompute_epsilon_closures();
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

    let (mut arena, start) = match cidr {
        CidrPattern::V4 {
            network,
            prefix_len,
        } => make_ipv4_cidr_arena_fa(network, *prefix_len, next_field),
        CidrPattern::V6 {
            network,
            prefix_len,
        } => make_ipv6_cidr_arena_fa(network, *prefix_len, next_field),
    };
    arena.precompute_epsilon_closures();
    (arena, start)
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

        arena.precompute_epsilon_closures();
        let mut bufs = ArenaNfaBuffers::with_capacity();

        // Should match "a"
        let value = b"a";
        traverse_arena_nfa(&arena, start, value, &mut bufs);
        assert_eq!(bufs.transitions.len(), 1);
        assert_eq!(bufs.transitions[0], Arc::as_ptr(&field_matcher) as usize);

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

        arena.precompute_epsilon_closures();
        let mut bufs = ArenaNfaBuffers::with_capacity();

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

        arena.precompute_epsilon_closures();
        let mut bufs = ArenaNfaBuffers::with_capacity();

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
        arena.precompute_epsilon_closures();
        let mut bufs = ArenaNfaBuffers::with_capacity();
        traverse_arena_nfa(&arena, start, b"aaaaaaaaaa", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1);
    }

    #[test]
    fn test_nested_quantifier_dedup() {
        // Nested quantifiers like (([abc]?)*)+ create epsilon loops that cause
        // duplicate states to compound exponentially. Verify the generation-counter
        // dedup keeps next_states bounded.
        //
        // Build: [abc]? loop — each of a, b, c transitions to a loopback that
        // has epsilon back to start + epsilon to exit. The '?' is implicit via
        // the epsilon from start to exit.
        let mut arena = StateArena::new();
        let field_matcher = Arc::new(FieldMatcher::new());

        // Final state (has field_transitions to signal a match)
        let final_state = arena.alloc();
        arena[final_state]
            .field_transitions
            .push(field_matcher.clone());

        // Exit state: matches VALUE_TERMINATOR → final
        let exit_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        // Loopback state (epsilon to exit + start — creates the cycle)
        let loopback = arena.alloc();

        // Start state: transitions on a/b/c → loopback, epsilon to exit (for ?)
        let start = arena.alloc_with_table({
            let mut table = ArenaSmallTable::with_mappings(StateId::NONE, b"abc", &[loopback; 3]);
            table.epsilons.push(exit_state);
            table
        });

        // loopback → epsilon to both exit and start (the * / + cycle)
        arena[loopback].table.epsilons = smallvec![exit_state, start];

        arena.precompute_epsilon_closures();
        let mut bufs = ArenaNfaBuffers::with_capacity();

        // A long input of 'a's — without dedup this would explode exponentially
        let long_input: Vec<u8> = std::iter::repeat(b'a').take(200).collect();
        traverse_arena_nfa(&arena, start, &long_input, &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Should match the long input");

        // Verify current_states stayed bounded (should be <= arena size, not exponential)
        // The arena has 4 states, so current_states should never exceed ~4
        // (after dedup). We can't check mid-traversal, but the fact that it
        // completed without hanging proves the dedup worked.

        // Also verify correctness on short inputs
        bufs.clear();
        traverse_arena_nfa(&arena, start, b"abc", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Should match 'abc'");

        bufs.clear();
        traverse_arena_nfa(&arena, start, b"", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Should match empty (via ?)");

        bufs.clear();
        traverse_arena_nfa(&arena, start, b"d", &mut bufs);
        assert!(
            bufs.transitions.is_empty(),
            "Should not match 'd' (only a/b/c)"
        );
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

        arena.precompute_epsilon_closures();
        let mut bufs = ArenaNfaBuffers::with_capacity();

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

    /// Verify that `StateArena::dstep` via `dfa_lookup` returns the same result
    /// as `ArenaSmallTable::dstep` for every byte value.
    ///
    /// This test is critical because `build_dfa_lookup` is skipped under Miri
    /// (`#[cfg(miri)]` no-op) to avoid a ~165s slowdown from interpreting the
    /// large 256-entry-per-state array on every byte transition. Under Miri,
    /// `StateArena::dstep` falls back to `ArenaSmallTable::dstep`. This test
    /// ensures the two paths are equivalent in non-Miri builds.
    #[test]
    fn test_dfa_lookup_matches_smalltable_dstep() {
        let mut arena = StateArena::new();

        // State 0: two transitions  b'a'..b'c' -> state1, b'm'..b'z' -> state2
        let s0 = arena.alloc();
        let s1 = arena.alloc();
        let s2 = arena.alloc();

        arena[s0].table.ceilings = smallvec![b'c', b'z'];
        arena[s0].table.steps = smallvec![s1, s2];

        // State 1: single transition  b'\x00'..b'\x80' -> state0
        arena[s1].table.ceilings = smallvec![0x80];
        arena[s1].table.steps = smallvec![s0];

        // State 2: no transitions (empty table)

        // Record expected results from ArenaSmallTable::dstep before flattening
        let mut expected: Vec<Vec<StateId>> = Vec::new();
        for sid in [s0, s1, s2] {
            let mut row = Vec::new();
            for byte in 0..=255u8 {
                row.push(arena[sid].table.dstep(byte));
            }
            expected.push(row);
        }

        // Flatten — builds dfa_lookup (in non-Miri builds)
        arena.flatten_tables();

        // Verify StateArena::dstep matches for every state × byte
        for (i, sid) in [s0, s1, s2].iter().enumerate() {
            for byte in 0..=255u8 {
                assert_eq!(
                    arena.dstep(*sid, byte),
                    expected[i][byte as usize],
                    "mismatch at state {i}, byte {byte:#04x}"
                );
            }
        }
    }

    #[test]
    fn test_arena_stats_empty() {
        let arena = StateArena::new();
        let stats = arena.stats();
        assert_eq!(stats.state_count, 0);
        assert_eq!(stats.tables_with_transitions, 0);
        assert_eq!(stats.total_epsilons, 0);
        // Display should not panic
        let _s = format!("{stats}");
    }

    #[test]
    fn test_arena_stats_basic() {
        let mut arena = StateArena::new();
        let s0 = arena.alloc();
        let s1 = arena.alloc();
        let s2 = arena.alloc();

        // s0 --'a'--> s1 (via transition table)
        arena[s0].table.set_transition(b'a', s1);
        // s1 has an epsilon to s2
        arena[s1].table.epsilons.push(s2);
        // s2 is a match state
        let fm = Arc::new(FieldMatcher::new());
        arena[s2].field_transitions.push(fm);

        let stats = arena.stats();
        assert_eq!(stats.state_count, 3);
        assert!(stats.tables_with_transitions >= 1); // s0 has 'a' transition
        assert_eq!(stats.total_epsilons, 1);
        assert_eq!(stats.max_epsilons, 1);
        assert_eq!(stats.states_with_field_transitions, 1);
        // alloc() gives each state a trivial self-closure (len=1)
        assert_eq!(stats.states_with_closures, 3);
        assert_eq!(stats.max_closure_len, 1);
        assert_eq!(stats.dfa_lookup_states, 0);

        // After precomputing closures — s1's closure should include s2 (via epsilon)
        arena.precompute_epsilon_closures();
        let stats = arena.stats();
        assert_eq!(stats.states_with_closures, 3);
        assert!(stats.max_closure_len >= 2); // s1 closure includes s1 + s2
        assert!(stats.closure_data_len > 0);

        // After flattening
        arena.flatten_tables();
        let stats = arena.stats();
        // dfa_lookup is skipped under Miri (cfg(miri) no-op in build_dfa_lookup)
        #[cfg(not(miri))]
        assert_eq!(stats.dfa_lookup_states, 3);
        assert!(stats.ft_ptrs_len > 0);

        // Display should produce readable output
        let display = format!("{stats}");
        assert!(display.contains("states=3"));
    }
}

#[cfg(test)]
mod arena_stats_utility_tests {
    use super::*;

    #[test]
    fn test_arena_stats_add_sums_and_maxes() {
        let mut a = ArenaStats {
            state_count: 10,
            tables_with_transitions: 5,
            total_ceiling_entries: 20,
            max_ceilings: 4,
            total_epsilons: 3,
            max_epsilons: 2,
            states_with_field_transitions: 2,
            closure_data_len: 15,
            states_with_closures: 8,
            total_closure_entries: 12,
            max_closure_len: 3,
            ft_ptrs_len: 6,
            dfa_lookup_states: 10,
            estimated_bytes: 1000,
        };
        let b = ArenaStats {
            state_count: 7,
            tables_with_transitions: 3,
            total_ceiling_entries: 10,
            max_ceilings: 6,
            total_epsilons: 5,
            max_epsilons: 1,
            states_with_field_transitions: 1,
            closure_data_len: 9,
            states_with_closures: 4,
            total_closure_entries: 7,
            max_closure_len: 5,
            ft_ptrs_len: 2,
            dfa_lookup_states: 7,
            estimated_bytes: 500,
        };
        a.add(&b);

        // Additive fields
        assert_eq!(a.state_count, 17);
        assert_eq!(a.tables_with_transitions, 8);
        assert_eq!(a.total_ceiling_entries, 30);
        assert_eq!(a.total_epsilons, 8);
        assert_eq!(a.states_with_field_transitions, 3);
        assert_eq!(a.closure_data_len, 24);
        assert_eq!(a.states_with_closures, 12);
        assert_eq!(a.total_closure_entries, 19);
        assert_eq!(a.ft_ptrs_len, 8);
        assert_eq!(a.dfa_lookup_states, 17);
        assert_eq!(a.estimated_bytes, 1500);

        // Max fields — should take the larger value
        assert_eq!(a.max_ceilings, 6); // b had larger
        assert_eq!(a.max_epsilons, 2); // a had larger
        assert_eq!(a.max_closure_len, 5); // b had larger
    }

    #[test]
    fn test_arena_stats_add_equal_max_values() {
        // When both sides have equal max, result should still be that value
        let mut a = ArenaStats {
            max_ceilings: 4,
            max_epsilons: 3,
            max_closure_len: 5,
            ..Default::default()
        };
        let b = ArenaStats {
            max_ceilings: 4,
            max_epsilons: 3,
            max_closure_len: 5,
            ..Default::default()
        };
        a.add(&b);
        assert_eq!(a.max_ceilings, 4);
        assert_eq!(a.max_epsilons, 3);
        assert_eq!(a.max_closure_len, 5);
    }

    #[test]
    fn test_estimated_byte_size() {
        let mut arena = StateArena::new();
        let empty_size = arena.estimated_byte_size();

        arena.alloc();
        arena.alloc();
        arena.alloc();

        let size_with_states = arena.estimated_byte_size();
        let expected = arena.states.capacity() * std::mem::size_of::<ArenaFaState>()
            + arena.closure_data.capacity() * std::mem::size_of::<StateId>()
            + arena.ft_ptrs.capacity() * std::mem::size_of::<usize>()
            + arena.dfa_lookup.capacity() * std::mem::size_of::<StateId>();
        assert_eq!(size_with_states, expected);
        assert!(size_with_states >= empty_size);
    }

    #[test]
    fn test_debug_fmt_arena() {
        let mut arena = StateArena::new();
        arena.alloc();
        arena.alloc();
        let dbg = format!("{:?}", arena);
        assert!(dbg.contains("states_count"));
        assert!(dbg.contains("2")); // 2 states
    }

    #[test]
    fn test_debug_fmt_state() {
        let state = ArenaFaState::new();
        let dbg = format!("{:?}", state);
        assert!(dbg.contains("ArenaFaState"));
        assert!(dbg.contains("field_transitions_count"));
    }

    #[test]
    fn test_with_capacity() {
        let mut arena = StateArena::with_capacity(10);
        assert!(arena.is_empty());
        assert_eq!(arena.len(), 0);
        assert!(arena.states.capacity() >= 10);
        assert!(arena.closure_data.capacity() >= 10);
        let id = arena.alloc();
        assert_eq!(id.index(), 0);
        assert!(!arena.is_empty());
        assert_eq!(arena.len(), 1);
    }

    #[test]
    fn test_is_empty_transitions() {
        let mut arena = StateArena::new();
        assert!(arena.is_empty());
        let id = arena.alloc();
        assert!(!arena.is_empty());
        // Verify the state is accessible
        assert!(arena.get(id).is_some());
    }

    #[test]
    fn test_get_mut_valid_and_invalid() {
        let mut arena = StateArena::new();
        let id = arena.alloc();

        // Valid ID should return Some
        assert!(arena.get_mut(id).is_some());
        // NONE should return None
        assert!(arena.get_mut(StateId::NONE).is_none());
        // Out-of-range should return None
        assert!(arena.get_mut(StateId::from_index(999)).is_none());
    }

    #[test]
    fn test_stats_max_tracking_across_states() {
        let mut arena = StateArena::new();
        let s0 = arena.alloc();
        let s1 = arena.alloc();
        let s2 = arena.alloc();
        let s3 = arena.alloc();

        // s0: 2 ceilings (non-trivial), 0 epsilons
        arena[s0].table.ceilings = smallvec![b'a', BYTE_CEILING as u8];
        arena[s0].table.steps = smallvec![s1, StateId::NONE];

        // s1: 3 ceilings (non-trivial), 1 epsilon
        arena[s1].table.ceilings = smallvec![b'a', b'b', BYTE_CEILING as u8];
        arena[s1].table.steps = smallvec![s0, s2, StateId::NONE];
        arena[s1].table.epsilons.push(s3);

        // s2: 4 ceilings (non-trivial), 2 epsilons
        arena[s2].table.ceilings = smallvec![b'a', b'b', b'c', BYTE_CEILING as u8];
        arena[s2].table.steps = smallvec![s0, s1, s3, StateId::NONE];
        arena[s2].table.epsilons.push(s0);
        arena[s2].table.epsilons.push(s1);

        // s3: default table (1 ceiling = trivial), 0 epsilons

        let stats = arena.stats();
        assert_eq!(stats.state_count, 4);
        assert_eq!(stats.tables_with_transitions, 3); // s0, s1, s2
        assert_eq!(stats.max_ceilings, 4); // s2 has 4
        assert_eq!(stats.total_epsilons, 3); // 0 + 1 + 2 + 0
        assert_eq!(stats.max_epsilons, 2); // s2 has 2
    }

    #[test]
    fn test_is_nondeterministic() {
        let mut arena = StateArena::new();
        let s0 = arena.alloc();
        let s1 = arena.alloc();

        // No epsilons = deterministic
        assert!(!arena.is_nondeterministic());

        // Add epsilon = nondeterministic
        arena[s0].table.epsilons.push(s1);
        assert!(arena.is_nondeterministic());
    }
}

#[cfg(test)]
#[allow(unsafe_code)]
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
        let mut bufs = ArenaNfaBuffers::with_capacity();
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

        let mut bufs = ArenaNfaBuffers::with_capacity();

        // Should match 'a'
        traverse_arena_nfa(&merged, start, b"a", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Merged should match 'a'");
        assert_eq!(bufs.transitions[0], Arc::as_ptr(&fm1) as usize);

        // Should match 'b'
        bufs.clear();
        traverse_arena_nfa(&merged, start, b"b", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Merged should match 'b'");
        assert_eq!(bufs.transitions[0], Arc::as_ptr(&fm2) as usize);

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

        let mut bufs = ArenaNfaBuffers::with_capacity();
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

        let mut bufs = ArenaNfaBuffers::with_capacity();

        // Check 'x' has fm1
        traverse_arena_nfa(&merged, start, b"x", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1);
        assert_eq!(
            unsafe { &*(bufs.transitions[0] as *const FieldMatcher) }.match_id,
            Some(100)
        );

        // Check 'y' has fm2
        bufs.clear();
        traverse_arena_nfa(&merged, start, b"y", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1);
        assert_eq!(
            unsafe { &*(bufs.transitions[0] as *const FieldMatcher) }.match_id,
            Some(200)
        );
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
        let mut bufs1 = ArenaNfaBuffers::with_capacity();
        let mut bufs2 = ArenaNfaBuffers::with_capacity();

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

        let mut bufs = ArenaNfaBuffers::with_capacity();

        // Should match "ab"
        traverse_arena_nfa(&merged, start, b"ab", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Should match 'ab'");
        assert_eq!(
            unsafe { &*(bufs.transitions[0] as *const FieldMatcher) }.match_id,
            Some(1)
        );

        // Should match "ac"
        bufs.clear();
        traverse_arena_nfa(&merged, start, b"ac", &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "Should match 'ac'");
        assert_eq!(
            unsafe { &*(bufs.transitions[0] as *const FieldMatcher) }.match_id,
            Some(2)
        );

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
#[allow(unsafe_code)]
mod numeric_arena_tests {
    use super::*;
    use crate::numbits::q_num_from_f64;

    /// Helper to test if a Q-number matches against an arena FA
    fn matches_arena(arena: &StateArena, start: StateId, q_num: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity();
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

        let mut bufs = ArenaNfaBuffers::with_capacity();

        // 25 should match (< 50)
        traverse_arena_nfa(&merged, merged_start, &q25, &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "25 should match merged FA");
        assert_eq!(
            unsafe { &*(bufs.transitions[0] as *const FieldMatcher) }.match_id,
            Some(1)
        );

        // 75 should NOT match (50 <= 75 <= 100)
        bufs.clear();
        traverse_arena_nfa(&merged, merged_start, &q75, &mut bufs);
        assert!(bufs.transitions.is_empty(), "75 should NOT match merged FA");

        // 150 should match (> 100)
        bufs.clear();
        traverse_arena_nfa(&merged, merged_start, &q150, &mut bufs);
        assert_eq!(bufs.transitions.len(), 1, "150 should match merged FA");
        assert_eq!(
            unsafe { &*(bufs.transitions[0] as *const FieldMatcher) }.match_id,
            Some(2)
        );
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
#[allow(unsafe_code)]
mod nfa_merge_tests {
    use super::*;

    /// Helper to check if a value matches against an arena FA
    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    /// Helper to get field matcher match IDs from traversal
    fn get_match_ids(arena: &StateArena, start: StateId, value: &[u8]) -> Vec<u64> {
        let mut bufs = ArenaNfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        bufs.transitions
            .iter()
            .filter_map(|&ptr| unsafe { &*(ptr as *const FieldMatcher) }.match_id)
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

        arena.precompute_epsilon_closures();
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

        // Build spinout state with self-loop encoded in transition table
        let spinout_state = arena.alloc();
        arena[spinout_state].table = make_byte_dot_table(spinout_state);
        arena[spinout_state].table.epsilons.push(after_spinout);

        // If suffix starts with a specific byte, also add direct transition
        // by unpacking the byte-dot table, overriding the suffix byte, and repacking
        if !suffix.is_empty() {
            let mut unpacked = [StateId::NONE; BYTE_CEILING];
            unpack_arena_table(&arena[spinout_state].table, &mut unpacked);
            unpacked[suffix[0] as usize] = after_spinout;
            arena[spinout_state].table.pack(&unpacked);
        }

        // Build prefix chain
        let mut current = spinout_state;
        // Add epsilon from start of spinout to after_spinout for zero-width wildcard
        let start = if prefix.is_empty() {
            // No prefix - start is the spinout with epsilon to continuation
            let start = arena.alloc();
            arena[start].table.epsilons.push(spinout_state);
            start
        } else {
            for &byte in prefix.iter().rev() {
                let state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                    StateId::NONE,
                    &[byte],
                    &[current],
                ));
                current = state;
            }
            current
        };

        arena.precompute_epsilon_closures();
        (arena, start)
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
            arena[spinout2].table = make_byte_dot_table(spinout2);
            arena[spinout2].table.epsilons.push(term_state);

            // State that matches 'X' -> spinout2
            let x_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"X",
                &[spinout2],
            ));

            // First spinout (before X)
            let spinout1 = arena.alloc();
            arena[spinout1].table = make_byte_dot_table(spinout1);
            arena[spinout1].table.epsilons.push(x_state);
            // Override 'X' byte to go directly to spinout2
            let mut unpacked = [StateId::NONE; BYTE_CEILING];
            unpack_arena_table(&arena[spinout1].table, &mut unpacked);
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
            arena[spinout2].table = make_byte_dot_table(spinout2);
            arena[spinout2].table.epsilons.push(term_state);

            let y_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
                StateId::NONE,
                b"Y",
                &[spinout2],
            ));

            let spinout1 = arena.alloc();
            arena[spinout1].table = make_byte_dot_table(spinout1);
            arena[spinout1].table.epsilons.push(y_state);
            let mut unpacked = [StateId::NONE; BYTE_CEILING];
            unpack_arena_table(&arena[spinout1].table, &mut unpacked);
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
        let mut bufs = ArenaNfaBuffers::with_capacity();
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
        let mut bufs = ArenaNfaBuffers::with_capacity();
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
        let mut bufs = ArenaNfaBuffers::with_capacity();
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
            matches_value(&arena, start, b"foofoofoo"),
            "Should match 'foofoofoo'"
        );
        assert!(
            !matches_value(&arena, start, b"bar"),
            "Should NOT match 'bar'"
        );
        assert!(
            !matches_value(&arena, start, b"fo"),
            "Should NOT match 'fo'"
        );
        assert!(
            !matches_value(&arena, start, b"ffo"),
            "Should NOT match 'ffo'"
        );
    }

    #[test]
    fn test_shellstyle_arena_fa_foo_bar_multi_star() {
        // Pattern: "*foo*bar*" — from Go PR #500 commit 137fe99
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"*foo*bar*", fm.clone());

        assert!(matches_value(&arena, start, b"foobar"));
        assert!(matches_value(&arena, start, b"xfooybar"));
        assert!(matches_value(&arena, start, b"foobarbaz"));
        assert!(matches_value(&arena, start, b"xxfooxxbarxx"));
        assert!(!matches_value(&arena, start, b"barfoo"));
        assert!(!matches_value(&arena, start, b"foo"));
        assert!(!matches_value(&arena, start, b"bar"));
        assert!(!matches_value(&arena, start, b"fobar"));
    }

    #[test]
    fn test_shellstyle_arena_fa_five_star() {
        // Pattern: "*a*b*c*d*e*" — from Go PR #500 commit 137fe99
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"*a*b*c*d*e*", fm.clone());

        assert!(matches_value(&arena, start, b"abcde"));
        assert!(matches_value(&arena, start, b"xaxbxcxdxex"));
        assert!(matches_value(&arena, start, b"aabbccddee"));
        assert!(!matches_value(&arena, start, b"abcd"));
        assert!(!matches_value(&arena, start, b"edcba"));
        assert!(!matches_value(&arena, start, b"abce"));
    }

    #[test]
    fn test_shellstyle_arena_fa_eight_star() {
        // Pattern: "*a*b*c*d*e*f*g*h*" — from Go PR #500 commit 137fe99
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"*a*b*c*d*e*f*g*h*", fm.clone());

        assert!(matches_value(&arena, start, b"abcdefgh"));
        assert!(matches_value(&arena, start, b"xaxbxcxdxexfxgxhx"));
        assert!(!matches_value(&arena, start, b"abcdefg"));
        assert!(!matches_value(&arena, start, b"hgfedcba"));
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
        let mut bufs = ArenaNfaBuffers::with_capacity();
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
        let mut bufs = ArenaNfaBuffers::with_capacity();
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
        let mut bufs = ArenaNfaBuffers::with_capacity();
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
        let mut bufs = ArenaNfaBuffers::with_capacity();
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

#[cfg(kani)]
mod kani_arena_proofs {
    use super::*;

    /// Prove: dstep returns the correct state for any byte on a real
    /// ArenaSmallTable with symbolic ceilings, steps, and lookup byte.
    ///
    /// Constructs a 3-entry packed table directly (bypassing pack() which
    /// triggers SmallVec state explosion at 246 iterations). This verifies
    /// the actual dstep() code path with fully symbolic inputs.
    #[kani::proof]
    #[kani::unwind(4)]
    fn arena_dstep_symbolic_lookup() {
        let c0: u8 = kani::any();
        let c1: u8 = kani::any();
        let c2: u8 = kani::any();
        kani::assume(c0 > 0);
        kani::assume(c1 > c0);
        kani::assume(c2 > c1);
        kani::assume((c2 as usize) <= BYTE_CEILING);

        let s0 = StateId::from_index(kani::any::<u8>() as usize);
        let s1 = StateId::from_index(kani::any::<u8>() as usize);
        let s2 = StateId::from_index(kani::any::<u8>() as usize);

        let mut table = ArenaSmallTable::new();
        table.ceilings = smallvec![c0, c1, c2];
        table.steps = smallvec![s0, s1, s2];

        let byte: u8 = kani::any();
        kani::assume((byte as usize) < BYTE_CEILING);

        let result = table.dstep(byte);

        if byte < c0 {
            kani::assert(result == s0, "byte in first range must return s0");
        } else if byte < c1 {
            kani::assert(result == s1, "byte in second range must return s1");
        } else if byte < c2 {
            kani::assert(result == s2, "byte in third range must return s2");
        } else {
            kani::assert(
                result == StateId::NONE,
                "byte past last ceiling must return NONE",
            );
        }
    }

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

#[cfg(test)]
mod nfa_to_dfa_tests {
    use super::*;
    use crate::regexp::{make_regexp_nfa_arena, parse_regexp};

    /// Helper: build a regexp NFA arena from a pattern string.
    fn build_regexp_nfa(pattern: &str) -> (StateArena, StateId) {
        let root = parse_regexp(pattern).expect("valid regexp");
        let (mut arena, start, _fm) = make_regexp_nfa_arena(root);
        arena.precompute_epsilon_closures();
        (arena, start)
    }

    /// Helper: check if a value matches in an arena (NFA path).
    fn nfa_matches(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    /// Helper: check if a value matches in an arena (DFA path).
    fn dfa_matches(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut transitions = Vec::new();
        traverse_arena_dfa(arena, start, value, &mut transitions);
        !transitions.is_empty()
    }

    /// Assert that NFA and DFA produce identical match results for all test values.
    ///
    /// Verifies both the NFA and the converted DFA agree with the expected result,
    /// ensuring the subset construction preserves matching semantics.
    fn assert_nfa_dfa_equivalence(
        nfa: &StateArena,
        nfa_start: StateId,
        dfa: &StateArena,
        dfa_start: StateId,
        test_values: &[(&[u8], bool)],
    ) {
        for &(val, expected) in test_values {
            let label = String::from_utf8_lossy(val);
            assert_eq!(
                nfa_matches(nfa, nfa_start, val),
                expected,
                "NFA mismatch on {label:?}",
            );
            assert_eq!(
                dfa_matches(dfa, dfa_start, val),
                expected,
                "DFA mismatch on {label:?}",
            );
        }
    }

    #[test]
    fn test_nfa_to_dfa_simple_plus() {
        let (nfa, nfa_start) = build_regexp_nfa("[abc]+");
        assert!(nfa.is_nondeterministic(), "should be NFA");

        let (dfa, dfa_start) = nfa.nfa_to_dfa(nfa_start, 1000).expect("should convert");
        assert!(!dfa.is_nondeterministic(), "should be DFA");

        assert_nfa_dfa_equivalence(
            &nfa,
            nfa_start,
            &dfa,
            dfa_start,
            &[
                (b"\"a\"", true),
                (b"\"abc\"", true),
                (b"\"aaa\"", true),
                (b"\"d\"", false),
                (b"\"\"", false),
            ],
        );
    }

    #[test]
    fn test_nfa_to_dfa_star() {
        let (nfa, nfa_start) = build_regexp_nfa("[xyz]*end");
        assert!(nfa.is_nondeterministic());

        let (dfa, dfa_start) = nfa.nfa_to_dfa(nfa_start, 1000).expect("should convert");
        assert!(!dfa.is_nondeterministic());

        assert_nfa_dfa_equivalence(
            &nfa,
            nfa_start,
            &dfa,
            dfa_start,
            &[
                (b"\"end\"", true),
                (b"\"xend\"", true),
                (b"\"xyzend\"", true),
                (b"\"xyxyend\"", true),
                (b"\"en\"", false),
                (b"\"aend\"", false),
            ],
        );
    }

    #[test]
    fn test_nfa_to_dfa_nested_quantifiers() {
        let (nfa, nfa_start) = build_regexp_nfa("(([abc]?)*)+");
        assert!(nfa.is_nondeterministic());

        let (dfa, dfa_start) = nfa.nfa_to_dfa(nfa_start, 1000).expect("should convert");
        assert!(!dfa.is_nondeterministic());

        assert_nfa_dfa_equivalence(
            &nfa,
            nfa_start,
            &dfa,
            dfa_start,
            &[
                (b"\"\"", true),
                (b"\"a\"", true),
                (b"\"abc\"", true),
                (b"\"aabbcc\"", true),
                (b"\"d\"", false),
            ],
        );
    }

    #[test]
    fn test_nfa_to_dfa_budget_exceeded() {
        let (nfa, nfa_start) = build_regexp_nfa("(([abc]?)*)+");
        assert!(nfa.is_nondeterministic());

        // Budget of 2 is too small for any real conversion
        let result = nfa.nfa_to_dfa(nfa_start, 2);
        assert!(result.is_none(), "should exceed budget");
    }

    #[test]
    fn test_nfa_to_dfa_empty_arena() {
        let arena = StateArena::new();
        let result = arena.nfa_to_dfa(StateId::NONE, 1000);
        assert!(result.is_some());
        let (dfa, start) = result.unwrap();
        assert!(start.is_none());
        assert!(dfa.is_empty());
    }

    #[test]
    fn test_nfa_to_dfa_none_start_nonempty_arena() {
        // Covers the `start.is_none()` branch when arena is non-empty.
        // Catches mutant: `start.is_none() || arena.is_empty()` → `&&`.
        let (nfa, _nfa_start) = build_regexp_nfa("[abc]+");
        assert!(!nfa.is_empty());
        let result = nfa.nfa_to_dfa(StateId::NONE, 1000);
        assert!(result.is_some());
        let (dfa, start) = result.unwrap();
        assert!(start.is_none());
        assert!(dfa.is_empty());
    }

    #[test]
    fn test_nfa_to_dfa_alternation() {
        let (nfa, nfa_start) = build_regexp_nfa("[a]+d|[b]+d");
        if !nfa.is_nondeterministic() {
            return; // Skip if deterministic (no conversion needed)
        }

        let (dfa, dfa_start) = nfa.nfa_to_dfa(nfa_start, 1000).expect("should convert");
        assert!(!dfa.is_nondeterministic());

        assert_nfa_dfa_equivalence(
            &nfa,
            nfa_start,
            &dfa,
            dfa_start,
            &[
                (b"\"ad\"", true),
                (b"\"aad\"", true),
                (b"\"bd\"", true),
                (b"\"bbd\"", true),
                (b"\"cd\"", false),
                (b"\"ab\"", false),
            ],
        );
    }

    #[test]
    fn test_nfa_to_dfa_preserves_field_transitions() {
        let (nfa, nfa_start) = build_regexp_nfa("[abc]+");
        let (dfa, dfa_start) = nfa.nfa_to_dfa(nfa_start, 1000).expect("should convert");

        // DFA should have at least one state with field transitions
        let has_ft =
            (0..dfa.len()).any(|i| !dfa[StateId::from_index(i)].field_transitions.is_empty());
        assert!(has_ft, "DFA should have field transitions");

        // Matching should find transitions
        let mut transitions = Vec::new();
        traverse_arena_dfa(&dfa, dfa_start, b"\"a\"", &mut transitions);
        assert!(!transitions.is_empty(), "should find field transitions");
    }
}

#[cfg(test)]
mod lazy_dfa_tests {
    use super::*;
    use crate::regexp::{make_regexp_nfa_arena, parse_regexp};

    /// Helper: build a regexp NFA arena from a pattern string.
    fn build_regexp_nfa(pattern: &str) -> (StateArena, StateId) {
        let root = parse_regexp(pattern).expect("valid regexp");
        let (mut arena, start, _fm) = make_regexp_nfa_arena(root);
        arena.precompute_epsilon_closures();
        (arena, start)
    }

    /// Helper: check if a value matches via lazy DFA.
    fn lazy_dfa_matches(lazy_dfa: &mut LazyDfa, value: &[u8]) -> bool {
        let mut transitions = Vec::new();
        traverse_lazy_dfa(lazy_dfa, value, &mut transitions);
        !transitions.is_empty()
    }

    /// Helper: check if a value matches via NFA.
    fn nfa_matches(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = ArenaNfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_lazy_dfa_basic() {
        let (nfa, nfa_start) = build_regexp_nfa("[abc]+");
        let mut lazy = LazyDfa::new(nfa, nfa_start, 100);

        assert!(lazy_dfa_matches(&mut lazy, b"\"a\""));
        assert!(lazy_dfa_matches(&mut lazy, b"\"abc\""));
        assert!(!lazy_dfa_matches(&mut lazy, b"\"d\""));
        assert!(!lazy_dfa_matches(&mut lazy, b"\"\""));
    }

    #[test]
    fn test_lazy_dfa_matches_nfa() {
        let (nfa, nfa_start) = build_regexp_nfa("[xyz]*end");

        let test_values: &[&[u8]] = &[
            b"\"end\"",
            b"\"xend\"",
            b"\"xyzend\"",
            b"\"xyxyend\"",
            b"\"en\"",
            b"\"aend\"",
            b"\"\"",
        ];

        let mut lazy = LazyDfa::new(nfa.clone(), nfa_start, 100);

        for &val in test_values {
            assert_eq!(
                nfa_matches(&nfa, nfa_start, val),
                lazy_dfa_matches(&mut lazy, val),
                "NFA/lazy-DFA disagree on {:?}",
                String::from_utf8_lossy(val),
            );
        }
    }

    #[test]
    fn test_lazy_dfa_cache_reuse() {
        let (nfa, nfa_start) = build_regexp_nfa("[abc]+");
        let mut lazy = LazyDfa::new(nfa, nfa_start, 100);

        // First traversal builds states
        assert!(lazy_dfa_matches(&mut lazy, b"\"abc\""));
        let states_after_first = lazy.states.len();

        // Second traversal reuses cached states
        assert!(lazy_dfa_matches(&mut lazy, b"\"abc\""));
        assert_eq!(
            lazy.states.len(),
            states_after_first,
            "should reuse cached states"
        );
    }

    #[test]
    fn test_lazy_dfa_budget_limits_cached_states() {
        let (nfa, nfa_start) = build_regexp_nfa("[abc]+");
        let mut lazy = LazyDfa::new(nfa, nfa_start, 3);

        // Should still work but limit cached states
        assert!(lazy_dfa_matches(&mut lazy, b"\"abc\""));
        assert!(lazy.cached_count <= 3, "should respect budget");
        // Cached count must be positive (catches += → *= mutation on cached_count)
        assert!(
            lazy.cached_count > 0,
            "should have cached at least one state"
        );
    }

    #[test]
    fn test_lazy_dfa_nested_quantifiers() {
        let (nfa, nfa_start) = build_regexp_nfa("(([abc]?)*)+");

        let test_values: &[&[u8]] = &[b"\"\"", b"\"a\"", b"\"abc\"", b"\"aabbcc\"", b"\"d\""];

        let mut lazy = LazyDfa::new(nfa.clone(), nfa_start, 1000);

        for &val in test_values {
            assert_eq!(
                nfa_matches(&nfa, nfa_start, val),
                lazy_dfa_matches(&mut lazy, val),
                "NFA/lazy-DFA disagree on {:?}",
                String::from_utf8_lossy(val),
            );
        }
    }
}
