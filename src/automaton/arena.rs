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

use std::num::NonZero;
use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};
use smallvec::{SmallVec, smallvec};

use super::small_table::{AccelInfo, BYTE_CEILING, BYTE_CEILING_U8, FieldMatcher};
use super::sparse_set::SparseSet;

/// A state identifier - just an index into the arena.
///
/// `StateId` is a u32 because we want it cheap to copy, hash, and store in
/// the transition tables. That gives us a ceiling of just under 2^32 states
/// per arena, which is fine: an arena that big would have run out of memory
/// long before. The conversions between `usize` and `StateId` rely on that
/// fact instead of checking it on every alloc.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct StateId(u32);

impl StateId {
    /// Special sentinel value for "no state" / null reference.
    pub const NONE: Self = Self(u32::MAX);

    /// Sentinel for "transition computed and known dead" in the lazy DFA
    /// table. Distinct from NONE ("not yet computed") so a known-dead
    /// transition is O(1) instead of being re-derived on every call.
    pub const DEAD: Self = Self(u32::MAX - 1);

    /// Build a `StateId` from a `usize` index. `const` so we can construct
    /// `StateId` arrays and table sentinels at compile time; the truncating
    /// cast can't overflow in practice (see the type doc).
    #[inline]
    #[must_use]
    #[allow(
        clippy::cast_possible_truncation,
        reason = "StateId encodes a u32-bounded index; arenas can't reach 2^32 states"
    )]
    pub const fn from_index(index: usize) -> Self {
        Self(index as u32)
    }

    #[inline]
    #[must_use]
    pub const fn is_none(self) -> bool {
        self.0 == u32::MAX
    }

    #[inline]
    #[must_use]
    pub const fn is_dead(self) -> bool {
        self.0 == u32::MAX - 1
    }

    #[inline]
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

/// Narrow a state-count-style `usize` into a `u32`. State counts live
/// under the same u32 ceiling that `StateId` enforces (see the type doc
/// on `StateId`); an arena big enough to violate it would have run out
/// of memory long before.
///
/// # Panics
/// Panics if `count` exceeds `u32::MAX`. In practice unreachable.
#[inline]
fn state_count_u32(count: usize, what: &'static str) -> u32 {
    u32::try_from(count)
        .unwrap_or_else(|_| panic!("arena {what} exceeds u32::MAX (arena overflow)"))
}

/// Narrow a flattened-buffer offset (`closure_data`, `ft_ptrs`, …) into a
/// `u32`. These buffers grow as the *sum* of per-state contents, so they
/// aren't strictly bounded by `state_count`. In practice each per-state
/// contribution is small (closures are typically 1-8 entries, field
/// transitions 0-1) so the offsets fit comfortably; we centralise the
/// narrowing here so a single point catches it if that assumption ever
/// breaks.
///
/// # Panics
/// Panics if `offset` exceeds `u32::MAX`. In practice unreachable.
#[inline]
fn buffer_offset_u32(offset: usize, what: &'static str) -> u32 {
    u32::try_from(offset)
        .unwrap_or_else(|_| panic!("arena {what} exceeds u32::MAX (buffer overflow)"))
}

/// A state in the arena-based finite automaton.
#[derive(Clone, Default)]
pub struct FaState {
    /// The transition table for this state
    pub table: SmallTable,
    /// Field matchers to transition to when this state is reached at end of value.
    /// SmallVec<[_; 1]> avoids heap allocation for the common case (0 or 1 transitions).
    pub field_transitions: SmallVec<[Arc<FieldMatcher>; 1]>,
    /// Offset into `StateArena::closure_data` where this state's closure begins.
    pub closure_start: u32,
    /// Number of states in this state's stored epsilon closure. Zero means the
    /// closure is just this state — or, until `StateArena::closures_computed`
    /// reaches it, that the closure has not been computed yet.
    pub closure_len: u16,
    /// Start index into `StateArena::ft_ptrs` for this state's field transition pointers.
    /// Populated by `flatten_tables()`.
    pub ft_start: u32,
    /// Number of field transition pointers for this state.
    pub ft_len: u8,
}

impl std::fmt::Debug for FaState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FaState")
            .field("table", &self.table)
            .field("field_transitions_count", &self.field_transitions.len())
            .finish()
    }
}

impl FaState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_table(table: SmallTable) -> Self {
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
pub struct SmallTable {
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
}

impl Default for SmallTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SmallTable {
    /// Create a new empty table.
    #[must_use]
    pub fn new() -> Self {
        Self {
            ceilings: smallvec![BYTE_CEILING_U8],
            steps: smallvec![StateId::NONE],
            epsilons: SmallVec::new(),
            accel: None,
        }
    }

    /// Create a table with specific byte mappings.
    ///
    /// `default` is the destination for every byte not listed in `bytes`; it is
    /// baked into the packed `steps` array.
    #[must_use]
    pub fn with_mappings(default: StateId, bytes: &[u8], targets: &[StateId]) -> Self {
        let mut unpacked = [StateId::NONE; BYTE_CEILING];

        if !default.is_none() {
            unpacked.fill(default);
        }

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
                // `unpacked` has at most BYTE_CEILING (0xF6) entries, so
                // `i` always fits in u8; `try_from` just keeps it explicit.
                self.ceilings
                    .push(u8::try_from(i).expect("unpacked index bounded by BYTE_CEILING"));
                self.steps.push(current);
                current = state_id;
            }
        }

        // Final entry
        self.ceilings.push(BYTE_CEILING_U8);
        self.steps.push(current);
    }

    /// Set a single byte transition, unpacking and repacking the table.
    pub fn set_transition(&mut self, byte: u8, target: StateId) {
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpack_arena_table(self, &mut unpacked);
        unpacked[byte as usize] = target;
        self.pack(&unpacked);
    }

    /// Returns the destination state for `byte`, or `StateId::NONE` if no
    /// transition applies.
    ///
    /// This is the white-hot center of NFA stepping: a linear scan over a
    /// SmallVec that is almost always 1–8 entries long. Epsilon transitions
    /// are *not* consulted here — they're precomputed once into
    /// `StateArena::closure_data` and iterated separately by the traversal
    /// loop. Returning a `StateId` (a `u32` newtype) keeps the call
    /// allocation-free; upstream Go originally threaded a `*stepOut` struct
    /// through this path and discovered the struct was escaping to the heap
    /// once per match (see upstream `e33139f`). Our value-typed return has
    /// no equivalent: the result lives in a register.
    ///
    /// Forbidden UTF-8 lead bytes (`0xC0`, `0xC1`, `0xF5..=0xFF`) are
    /// encoded directly into the table via `make_byte_dot_table` and the
    /// `BYTE_CEILING` packing in `pack`, so this method just falls off the
    /// end of `ceilings` and returns `StateId::NONE` for them — no separate
    /// forbidden-byte map lookup.
    #[inline(always)]
    #[allow(unsafe_code)]
    #[must_use]
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
}

/// Statistics about a `StateArena`'s structure.
#[derive(Clone, Debug, Default)]
pub struct Stats {
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
    /// States with a stored closure; None for state that is its own closure stores.
    pub states_with_closures: u32,
    /// Sum of all stored `closure_len` values if `states_with_closures` is set.
    pub total_closure_entries: u32,
    /// Max stored `closure_len` of any single state.
    pub max_closure_len: u16,
    /// Total entries in the flattened ft_ptrs buffer.
    pub ft_ptrs_len: u32,
    /// Number of states with 256-entry DFA lookup tables (0 if not frozen).
    pub dfa_lookup_states: u32,
    /// Estimated total byte size of the arena.
    pub estimated_bytes: usize,
}

impl std::fmt::Display for Stats {
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
                f64::from(self.total_ceiling_entries) / f64::from(self.tables_with_transitions)
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
                f64::from(self.total_closure_entries) / f64::from(self.states_with_closures)
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

impl Stats {
    /// Accumulate another arena's stats into this aggregate.
    pub fn add(&mut self, other: &Self) {
        self.state_count += other.state_count;
        self.tables_with_transitions += other.tables_with_transitions;
        self.total_ceiling_entries += other.total_ceiling_entries;
        self.max_ceilings = self.max_ceilings.max(other.max_ceilings);
        self.total_epsilons += other.total_epsilons;
        self.max_epsilons = self.max_epsilons.max(other.max_epsilons);
        self.states_with_field_transitions += other.states_with_field_transitions;
        self.closure_data_len += other.closure_data_len;
        self.states_with_closures += other.states_with_closures;
        self.total_closure_entries += other.total_closure_entries;
        self.max_closure_len = self.max_closure_len.max(other.max_closure_len);
        self.ft_ptrs_len += other.ft_ptrs_len;
        self.dfa_lookup_states += other.dfa_lookup_states;
        self.estimated_bytes += other.estimated_bytes;
    }
}

/// Arena for allocating NFA states.
///
/// States are allocated contiguously and referenced by `StateId`.
/// The arena owns all state memory and frees it when dropped.
// `Arena` suffix is descriptive (this is the state container itself), not a
// redundant repetition of the module name.
#[allow(clippy::module_name_repetitions)]
#[derive(Clone, Default)]
pub struct StateArena {
    states: Vec<FaState>,
    /// Shared buffer for every stored epsilon closure; states index into it via
    /// `closure_start`/`closure_len`.
    closure_data: Vec<StateId>,
    /// Number of states (from index 0) whose closures are already computed, so a
    /// rebuild can skip them and `closure_of` can tell a real self-closure from
    /// an uncomputed one.
    closures_computed: usize,
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

/// Reusable scratch for the epsilon-closure pass: a visited set sized to the
/// arena, a DFS stack, and a per-closure accumulator.
///
/// The build-time matcher owns one and threads it through every pattern add so
/// the buffers grow once and are reused, instead of reallocating on each add.
/// Cold callers use the one-shot [`StateArena::precompute_epsilon_closures`]
/// wrapper.
pub(crate) struct ClosureScratch {
    /// Deduplicates states as a closure is collected; must be able to index any
    /// state in the arena, so it is grown to the arena length on demand.
    seen: SparseSet,
    /// DFS work list for the epsilon walk.
    stack: Vec<StateId>,
    /// Accumulates one state's closure before it is copied into `closure_data`.
    closure_buf: Vec<StateId>,
}

impl ClosureScratch {
    pub(crate) fn new() -> Self {
        Self {
            seen: SparseSet::new(0),
            stack: Vec::new(),
            closure_buf: Vec::new(),
        }
    }

    /// Ensure `seen` can index every state in an arena of `arena_len` states,
    /// growing in power-of-two steps so reallocations across a build stay
    /// amortized cheap.
    fn ensure_seen_capacity(&mut self, arena_len: usize) {
        if self.seen.capacity() < arena_len {
            self.seen = SparseSet::new(arena_len.next_power_of_two());
        }
    }
}

impl Default for ClosureScratch {
    fn default() -> Self {
        Self::new()
    }
}

impl StateArena {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            states: Vec::new(),
            closure_data: Vec::new(),
            closures_computed: 0,
            ft_ptrs: Vec::new(),
            dfa_lookup: Vec::new(),
        }
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            states: Vec::with_capacity(capacity),
            // Grown on demand by precompute; most states store no closure.
            closure_data: Vec::new(),
            closures_computed: 0,
            ft_ptrs: Vec::new(),
            dfa_lookup: Vec::new(),
        }
    }

    /// Estimate the byte size of this arena (state vector capacity * per-state size).
    #[must_use]
    pub const fn estimated_byte_size(&self) -> usize {
        self.states.capacity() * std::mem::size_of::<FaState>()
            + self.closure_data.capacity() * std::mem::size_of::<StateId>()
            + self.ft_ptrs.capacity() * std::mem::size_of::<usize>()
            + self.dfa_lookup.capacity() * std::mem::size_of::<StateId>()
    }

    /// Accumulate this arena's contribution to matcher-level resource statistics.
    ///
    /// Bytes are capacity-based: the arena's flat buffers (via
    /// [`estimated_byte_size`](Self::estimated_byte_size)) plus the heap
    /// capacity of any per-state SmallVec that has spilled past its inline slots.
    /// Inline slots are not counted here — `size_of::<FaState>()` already covers
    /// them, so adding them again would double-count. `fanouts`/`max_fanout`
    /// come from each state's stored epsilon closure; a state that is its own
    /// closure contributes zero.
    pub fn accumulate_matcher_stats(&self, stats: &mut crate::MatcherStats) {
        stats.states += self.states.len();
        stats.bytes += self.byte_size();
        for state in &self.states {
            let fanout = state.closure_len as usize;
            stats.fanouts += fanout;
            stats.max_fanout = stats.max_fanout.max(fanout);
        }
    }

    /// Bytes this arena holds, flat buffers plus every per-state buffer that
    /// spilled past its inline slots.
    ///
    /// [`estimated_byte_size`](Self::estimated_byte_size) covers only the flat
    /// buffers and is O(1); this walks the states, so it is the figure to use
    /// where an arena's real footprint matters more than the cost of measuring
    /// it. Transition tables can dwarf the flat buffers, several times over for
    /// a DFA, so the two are far apart for anything but a trivial automaton.
    #[must_use]
    pub fn byte_size(&self) -> usize {
        let mut bytes = self.estimated_byte_size();
        for state in &self.states {
            let table = &state.table;
            if table.ceilings.spilled() {
                // ceilings holds u8, so its capacity is already a byte count.
                bytes += table.ceilings.capacity();
            }
            if table.steps.spilled() {
                bytes += table.steps.capacity() * std::mem::size_of::<StateId>();
            }
            if table.epsilons.spilled() {
                bytes += table.epsilons.capacity() * std::mem::size_of::<StateId>();
            }
            if state.field_transitions.spilled() {
                bytes +=
                    state.field_transitions.capacity() * std::mem::size_of::<Arc<FieldMatcher>>();
            }
        }
        bytes
    }

    /// Get a state reference without bounds checking.
    ///
    /// # Safety
    /// `id` must be a valid state ID returned by `alloc()` on this arena.
    #[inline(always)]
    #[allow(unsafe_code)]
    unsafe fn state_unchecked(&self, id: StateId) -> &FaState {
        // SAFETY: caller guarantees `id` is a valid index from `alloc()` on this arena
        unsafe { self.states.get_unchecked(id.index()) }
    }

    /// The states in `id`'s epsilon closure. An empty slice means the closure is
    /// just `id` itself. Only meaningful once `id` has been closed by
    /// [`Self::precompute_epsilon_closures`]; reading it before then is a bug.
    #[inline(always)]
    #[allow(unsafe_code)]
    #[must_use]
    pub fn closure_of(&self, id: StateId) -> &[StateId] {
        // SAFETY: `id` came from `alloc()` on this arena, so `state_unchecked` is
        // valid. A nonzero `closure_len` is a valid range within `closure_data`;
        // len==0 returns early and never indexes it.
        unsafe {
            let state = self.state_unchecked(id);
            let len = state.closure_len as usize;
            if len == 0 {
                return &[];
            }
            let start = state.closure_start as usize;
            self.closure_data.get_unchecked(start..start + len)
        }
    }

    /// Append a state's closure to `out`; an empty closure means just that state.
    #[inline(always)]
    fn extend_closure_or_self(&self, id: StateId, out: &mut Vec<StateId>) {
        let closure = self.closure_of(id);
        if closure.is_empty() {
            out.push(id);
        } else {
            out.extend_from_slice(closure);
        }
    }

    /// Get field transition pointers for a state as a slice.
    ///
    /// Returns raw pointer values (`Arc::as_ptr` cast to `usize`) for dedup.
    /// Only valid after `flatten_tables()` has been called.
    #[inline(always)]
    #[allow(unsafe_code)]
    #[must_use]
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

    /// Returns the destination state reached from `id` on `byte`, using the
    /// flattened 256-entry-per-state lookup table when available.
    ///
    /// After `flatten_tables()` has run, this is a single indexed load —
    /// `dfa_lookup[id * 256 + byte]` — which beats the SmallTable linear
    /// scan by ~21% on `pathological_epsilon`. Before flattening (the
    /// mutable build path and tests) it falls back to `SmallTable::dstep`,
    /// which produces the same result but pays the linear-scan cost.
    ///
    /// Like `SmallTable::dstep`, this is allocation-free by construction:
    /// the caller's `next_states` buffer is the only growing collection in
    /// the traversal loop, and the closure walk reads from
    /// `closure_data`, not from a per-step temporary. Upstream Go's
    /// `e33139f` removed a `*stepOut` heap escape from the analogous path;
    /// our `StateId` return type means there's no struct to escape.
    #[inline(always)]
    #[allow(unsafe_code)]
    #[must_use]
    pub fn dstep(&self, id: StateId, byte: u8) -> StateId {
        if self.dfa_lookup.is_empty() {
            // Fallback: flat buffers not populated (mutable path)
            self.states[id.index()].table.dstep(byte)
        } else {
            // SAFETY: id.index() < states.len(), byte is 0..255,
            // so id.index() * 256 + byte < states.len() * 256 = dfa_lookup.len()
            unsafe {
                *self
                    .dfa_lookup
                    .get_unchecked(id.index() * 256 + byte as usize)
            }
        }
    }

    /// Allocate a new default state, returning its ID.
    ///
    /// # Panics
    /// Panics if the arena's state count or `closure_data` length would
    /// exceed `u32::MAX`. In practice unreachable: the machine would run
    /// out of memory long before either counter saturates.
    pub fn alloc(&mut self) -> StateId {
        let id = StateId::from_index(self.states.len());
        // Closure computed later by precompute; a fresh state stores none.
        let state = FaState {
            closure_start: buffer_offset_u32(self.closure_data.len(), "closure_data length"),
            ..Default::default()
        };
        self.states.push(state);
        id
    }

    /// Allocate a new state with the given table, returning its ID.
    ///
    /// # Panics
    /// Panics if the arena's state count or `closure_data` length would
    /// exceed `u32::MAX`. In practice unreachable; see [`Self::alloc`].
    pub fn alloc_with_table(&mut self, table: SmallTable) -> StateId {
        let id = StateId::from_index(self.states.len());
        let mut state = FaState::with_table(table);
        state.closure_start = buffer_offset_u32(self.closure_data.len(), "closure_data length");
        self.states.push(state);
        id
    }

    /// Get a reference to a state by ID.
    #[inline]
    #[must_use]
    pub fn get(&self, id: StateId) -> Option<&FaState> {
        if id.is_none() {
            None
        } else {
            self.states.get(id.index())
        }
    }

    /// Get a mutable reference to a state by ID.
    #[inline]
    pub fn get_mut(&mut self, id: StateId) -> Option<&mut FaState> {
        if id.is_none() {
            None
        } else {
            self.states.get_mut(id.index())
        }
    }

    /// Number of states in the arena.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.states.len()
    }

    /// Check if arena is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.states.is_empty()
    }

    /// Drop every state with index `>= n`, restoring the arena to an earlier
    /// length.
    ///
    /// `append_merge_arena_nfas` only ever appends states and never edits the
    /// ones already present, so truncating back to a length captured before the
    /// merge exactly undoes it. The surviving states keep their tables and
    /// epsilons untouched. Epsilon closures are laid out in state order, so the
    /// survivors own a prefix of `closure_data`; the tail is trimmed to match.
    ///
    /// This is a build-time operation: the frozen lookup buffers are only
    /// populated at freeze, so they are empty here and need no adjustment.
    pub fn truncate_states(&mut self, n: usize) {
        if n >= self.states.len() {
            return;
        }
        let closure_end = if n == 0 {
            0
        } else {
            let last = &self.states[n - 1];
            last.closure_start as usize + last.closure_len as usize
        };
        self.states.truncate(n);
        self.closure_data.truncate(closure_end);
        // Some of the dropped states may have been closed, so lower the
        // computed count to cover only the states that remain.
        self.closures_computed = self.closures_computed.min(n);
    }

    /// Compute statistics about this arena's structure.
    #[must_use]
    pub fn stats(&self) -> Stats {
        let state_count = self.states.len();
        if state_count == 0 {
            return Stats::default();
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
            // We only count states with non-trivial transitions (i.e.
            // more than the default catch-all entry). Per-state table
            // sizes are bounded by `BYTE_CEILING` (246), so the narrowing
            // to u16 and u32 here can't actually fail at runtime; we route
            // through `try_from` only to keep the casts explicit.
            let nc = state.table.ceilings.len();
            if nc > 1 {
                tables_with_transitions += 1;
                total_ceiling_entries +=
                    u32::try_from(nc).expect("ceiling count bounded by BYTE_CEILING");
                let nc_u16 = u16::try_from(nc).expect("ceiling count bounded by BYTE_CEILING");
                max_ceilings = max_ceilings.max(nc_u16);
            }

            let ne = state.table.epsilons.len();
            if ne > 0 {
                total_epsilons += u32::try_from(ne).expect("epsilon count bounded by BYTE_CEILING");
                let ne_u16 = u16::try_from(ne).expect("epsilon count bounded by BYTE_CEILING");
                max_epsilons = max_epsilons.max(ne_u16);
            }

            if !state.field_transitions.is_empty() {
                states_with_field_transitions += 1;
            }

            if state.closure_len > 0 {
                states_with_closures += 1;
                total_closure_entries += u32::from(state.closure_len);
                max_closure_len = max_closure_len.max(state.closure_len);
            }
        }

        let dfa_lookup_states = if self.dfa_lookup.is_empty() {
            0
        } else {
            self.dfa_lookup.len() / 256
        };

        Stats {
            state_count: state_count_u32(state_count, "state count"),
            tables_with_transitions,
            total_ceiling_entries,
            max_ceilings,
            total_epsilons,
            max_epsilons,
            states_with_field_transitions,
            closure_data_len: buffer_offset_u32(self.closure_data.len(), "closure_data length"),
            states_with_closures,
            total_closure_entries,
            max_closure_len,
            ft_ptrs_len: buffer_offset_u32(self.ft_ptrs.len(), "ft_ptrs length"),
            dfa_lookup_states: state_count_u32(dfa_lookup_states, "dfa_lookup state count"),
            estimated_bytes: self.estimated_byte_size(),
        }
    }

    /// Check if any state in the arena has epsilon transitions.
    ///
    /// Not all regexp FAs are nondeterministic. This can be detected after
    /// building the FA to allow deterministic regexps to use the faster DFA
    /// traversal path.
    #[must_use]
    pub fn is_nondeterministic(&self) -> bool {
        self.states
            .iter()
            .any(|state| !state.table.epsilons.is_empty())
    }

    /// Compute and store the epsilon closure of every state not yet closed.
    ///
    /// A state's epsilon closure is the set of states reachable from it through
    /// epsilon (zero-input) transitions, including the state itself. NFA
    /// traversal needs a state's closure on every step, so instead of chasing
    /// epsilon edges live at match time we compute each closure once here and
    /// store it flat in `closure_data`, ready for [`Self::closure_of`] to hand
    /// back as a slice.
    ///
    /// Each not-yet-closed state gets a DFS over its epsilon edges; the reachable
    /// set is deduplicated through the `seen` [`SparseSet`] and recorded as a
    /// `(closure_start, closure_len)` window into `closure_data`.
    ///
    /// Two properties are worth knowing before changing this:
    ///
    /// 1. **It runs incrementally.** `closures_computed` marks how far the arena
    ///    has already been closed, and only states past that mark are walked.
    ///    This is safe because a merge only appends states and never adds an
    ///    epsilon edge to an existing one, so a closure that is already computed
    ///    can never grow. Re-closing the whole arena on every pattern add would
    ///    be O(N²) over N adds.
    ///
    /// 2. **Self-only closures are implicit.** Most states close to just
    ///    `{self}` — they have no epsilons, or only ones that loop back. These
    ///    store `closure_len == 0` and take no room in `closure_data`;
    ///    [`Self::closure_of`] returns an empty slice, which callers read as
    ///    "self only" (see [`Self::extend_closure_or_self`]). So a stored closure
    ///    is never a one-element `{self}`: it is either empty or two-plus states.
    ///
    /// Call once the arena is final (after any merges) and before matching.
    /// Returns how many states were closed.
    pub fn precompute_epsilon_closures(&mut self) -> usize {
        self.precompute_epsilon_closures_into(&mut ClosureScratch::new())
    }

    /// [`Self::precompute_epsilon_closures`] with caller-supplied scratch.
    ///
    /// The build path passes the value matcher's long-lived [`ClosureScratch`] so
    /// its buffers are reused across every pattern add rather than reallocated per
    /// call; see [`ClosureScratch`].
    pub(crate) fn precompute_epsilon_closures_into(
        &mut self,
        scratch: &mut ClosureScratch,
    ) -> usize {
        let arena_len = self.states.len();
        let start_idx = self.closures_computed;
        if start_idx >= arena_len {
            return 0;
        }

        // Reuse the settled prefix and re-close only the new tail; a
        // from-scratch pass starts fresh.
        let mut closure_data = if start_idx == 0 {
            Vec::new()
        } else {
            let tail_start = self.states[start_idx].closure_start as usize;
            let mut existing = std::mem::take(&mut self.closure_data);
            existing.truncate(tail_start);
            existing
        };

        // Grow the visited set to span the arena; it and the stack/accumulator
        // are reused across states (and, via `scratch`, across adds), cleared
        // per state below.
        scratch.ensure_seen_capacity(arena_len);
        let seen = &mut scratch.seen;
        let stack = &mut scratch.stack;
        let closure_buf = &mut scratch.closure_buf;

        for state_idx in start_idx..arena_len {
            let state_id = StateId::from_index(state_idx);
            let start = buffer_offset_u32(closure_data.len(), "closure_data length");

            if self.states[state_idx].table.epsilons.is_empty() {
                // No epsilons, so the closure is just this state; store nothing.
                self.states[state_idx].closure_start = start;
                self.states[state_idx].closure_len = 0;
            } else {
                // Walk the epsilon graph to collect the full closure.
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
                            // `seen` may be larger than the arena (reused and
                            // grown in power-of-two steps), so gate stray targets
                            // on arena_len, not seen.capacity().
                            if idx < arena_len && seen.insert(idx) {
                                closure_buf.push(eps_id);
                                stack.push(eps_id);
                                // A broken dedup would grow closure_buf past the
                                // arena size instead of terminating.
                                debug_assert!(
                                    closure_buf.len() <= arena_len,
                                    "epsilon closure exceeded arena size; dedup invariant broken"
                                );
                            }
                        }
                    }
                }

                self.states[state_idx].closure_start = start;
                if closure_buf.len() == 1 {
                    // The epsilons only cycled back here, so the closure is again
                    // just this state; store nothing.
                    self.states[state_idx].closure_len = 0;
                } else {
                    let len = u16::try_from(closure_buf.len())
                        .expect("epsilon closure exceeds u16::MAX states");
                    closure_data.extend_from_slice(closure_buf.as_slice());
                    self.states[state_idx].closure_len = len;
                }
            }
        }

        self.closure_data = closure_data;
        self.closures_computed = arena_len;
        arena_len - start_idx
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
            self.states[state_idx].ft_start = buffer_offset_u32(ft_start, "ft_start offset");
            // `ft_len` is u8 because we expect 0 or 1 field transitions per
            // state in practice (the SmallVec is sized for that). Anything
            // approaching 256 would mean something else has already gone
            // very wrong.
            self.states[state_idx].ft_len =
                u8::try_from(ft_len).expect("per-state field_transitions.len() fits in u8");
        }
        self.ft_ptrs = ft_ptrs;
    }

    /// Build a 256-entry-per-state lookup table for O(1) byte transitions.
    ///
    /// Skipped under Miri: the large array (states × 256) is expensive to
    /// interpret, and `dstep()` falls back to `SmallTable::dstep()` when
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

    /// No-op under Miri — `dstep()` falls back to `SmallTable::dstep()`.
    /// Correctness of the lookup table is verified by
    /// `tests::test_dfa_lookup_matches_smalltable_dstep` in non-Miri builds.
    #[cfg(miri)]
    fn build_dfa_lookup(&mut self) {}

    /// Convert an NFA arena to a DFA via subset construction.
    ///
    /// Returns `None` if `state_budget` is exceeded (caller should fall back to NFA).
    /// Inspired by Go quamina's `nfa2Dfa` and the two-tier strategy.
    /// [`Self::precompute_epsilon_closures`] must have been called after the
    /// arena's last structural change.
    #[must_use]
    pub fn nfa_to_dfa(&self, start: StateId, state_budget: usize) -> Option<(Self, StateId)> {
        if start.is_none() || self.states.is_empty() {
            return Some((Self::new(), StateId::NONE));
        }

        debug_assert_eq!(
            self.closures_computed,
            self.states.len(),
            "epsilon closures must be precomputed before nfa_to_dfa"
        );
        debug_assert!(
            self.states.iter().all(|state| {
                state.closure_len == 0
                    || state.closure_start as usize + state.closure_len as usize
                        <= self.closure_data.len()
            }),
            "epsilon closure metadata must be valid before nfa_to_dfa"
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
        // Per-byte next-state collector, hoisted out of the loop to reuse capacity.
        // Each entry is a Vec of NFA states reachable via that byte value.
        let mut byte_to_next: Vec<Vec<StateId>> = (0..BYTE_CEILING).map(|_| Vec::new()).collect();

        // Step 1: Compute the start DFA state = epsilon closure of NFA start
        let mut start_set = Vec::new();
        self.extend_closure_or_self(start, &mut start_set);
        let start_key = Self::make_state_set_key(&start_set);
        let dfa_start = dfa_arena.alloc();
        state_map.insert(start_key, dfa_start);
        worklist.push(dfa_start);

        // Store the NFA state-set for each DFA state (indexed by DFA state index)
        let mut dfa_nfa_sets: Vec<Vec<StateId>> = Vec::new();
        dfa_nfa_sets.push(start_set);

        // Copy field transitions from the start closure
        Self::collect_field_transitions(self, &dfa_nfa_sets[0], &mut dfa_arena[dfa_start]);

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
                        self.extend_closure_or_self(target, &mut byte_to_next[byte]);
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

                // Deduplicate by sorting then dropping adjacent equal states.
                // The sort also produces the canonical set key built below, so
                // no separate dedup set is needed.
                closure_set.clear();
                closure_set.extend_from_slice(&byte_to_next[byte]);
                closure_set.sort_unstable_by_key(|s| s.0);
                closure_set.dedup();

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

        // Reconstruct acceleration info for self-loop DFA states.
        // NFA AccelInfo is lost during subset construction; this detects the same
        // spinout pattern on DFA states (self-loop on most bytes, 1-3 exit bytes).
        dfa_arena.compute_dfa_accel();

        Some((dfa_arena, dfa_start))
    }

    /// Detect self-loop DFA states and populate their `AccelInfo`.
    ///
    /// After NFA→DFA conversion, the original NFA acceleration info is lost.
    /// This scans all states and detects "spinout" patterns: states that
    /// self-loop on most ASCII bytes, with 1-3 distinct exit bytes (including
    /// rejection bytes that lead to `StateId::NONE`). These states can use
    /// memchr SIMD to skip ahead during traversal.
    ///
    /// `StateId::NONE` targets count as exit bytes: memchr skips to them, then
    /// `dstep` returns NONE and `traverse_arena_dfa` exits early — correct behavior.
    ///
    /// Only ASCII bytes (0x00-0x7F) are checked. Non-ASCII bytes (multi-byte
    /// UTF-8 lead/continuation bytes) are ignored because:
    /// 1. Exit bytes for acceleratable patterns are always ASCII
    /// 2. memchr for ASCII bytes never matches inside multi-byte UTF-8 sequences
    /// 3. Complete multi-byte sequences always return to the self-loop state
    fn compute_dfa_accel(&mut self) {
        let mut unpacked = [StateId::NONE; BYTE_CEILING];

        for state_idx in 0..self.states.len() {
            let state_id = StateId::from_index(state_idx);
            unpack_arena_table(&self.states[state_idx].table, &mut unpacked);

            let mut has_self_loop = false;
            let mut exit_count = 0u8;
            let mut exit_bytes = [0u8; 3];

            // We only walk the ASCII slice (0x00..0x80), so `byte` always
            // fits in u8 — `try_from` here is just paranoia.
            for (byte, &target) in unpacked[..0x80].iter().enumerate() {
                if target == state_id {
                    has_self_loop = true;
                } else {
                    // An exit byte is either a real transition or a
                    // rejection (NONE). Either way, memchr can skip
                    // straight to it: if it's NONE, traversal exits
                    // early, which is the correct rejection behaviour.
                    if exit_count < 3 {
                        exit_bytes[exit_count as usize] =
                            u8::try_from(byte).expect("ASCII range bounded above by 0x80");
                    }
                    exit_count += 1;
                }
            }

            if has_self_loop && (1..=3).contains(&exit_count) {
                self.states[state_idx].table.accel = Some(AccelInfo {
                    exit_bytes,
                    len: exit_count,
                });
            }
        }
    }

    /// Create a canonical key from a sorted set of NFA state IDs.
    fn make_state_set_key(states: &[StateId]) -> Vec<u32> {
        states.iter().map(|s| s.0).collect()
    }

    /// Collect field transitions from a set of NFA states onto a DFA state.
    fn collect_field_transitions(
        nfa_arena: &Self,
        nfa_states: &[StateId],
        dfa_state: &mut FaState,
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
    type Output = FaState;

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
struct LazyDfaState {
    /// Full 256-entry transition table for O(1) lookup.
    /// `NONE` = not yet computed, `DEAD` = known dead transition.
    transitions: Vec<StateId>,
    /// Raw `Arc::as_ptr` values for field matchers reachable from this state's NFA set.
    field_transition_ptrs: Vec<usize>,
    /// Whether this state's transition table is memoized for reuse across traversals.
    /// Uncached states (budget exceeded) recompute transitions on every visit.
    cached: bool,
    /// SIMD skip info if this state self-loops on most bytes with ≤ 3 exit bytes.
    accel: Option<AccelInfo>,
}

/// A lazy DFA cache that builds DFA states on-demand during matching.
///
/// Implements tier 2 of the three-tier strategy:
/// eager DFA (tier 1) → **lazy DFA** (tier 2) → NFA fallback (tier 3).
///
/// The cache has a state budget. When full, new states are created as temporary
/// (uncached) and discarded after the current traversal, but the traversal can
/// still "snap back" to a cached state on the next byte transition.
pub(crate) struct LazyDfa {
    nfa_arena: StateArena,
    states: Vec<LazyDfaState>,
    state_map: FxHashMap<Vec<u32>, StateId>,
    nfa_sets: Vec<Vec<StateId>>,
    state_budget: usize,
    cached_count: usize,
}

impl LazyDfa {
    /// Create a lazy DFA over a fully closed NFA arena.
    ///
    /// `nfa_arena.precompute_epsilon_closures()` must have been called after
    /// the arena's last structural change.
    pub fn new(nfa_arena: StateArena, nfa_start: StateId, state_budget: usize) -> Self {
        debug_assert_eq!(
            nfa_arena.closures_computed,
            nfa_arena.states.len(),
            "epsilon closures must be precomputed before LazyDfa::new"
        );
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
            let mut closure = Vec::new();
            lazy.nfa_arena
                .extend_closure_or_self(nfa_start, &mut closure);
            lazy.intern_state(&closure, true);
        }

        lazy
    }

    fn intern_state(&mut self, nfa_states: &[StateId], allow_cache: bool) -> StateId {
        let key = StateArena::make_state_set_key(nfa_states);

        if let Some(&idx) = self.state_map.get(&key) {
            return idx;
        }

        let can_cache = allow_cache && self.cached_count < self.state_budget;
        let idx = StateId::from_index(self.states.len());

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
            // Only a cached state ever has its table written, so a state past
            // the budget goes without one.
            transitions: if can_cache {
                vec![StateId::NONE; BYTE_CEILING]
            } else {
                Vec::new()
            },
            field_transition_ptrs,
            cached: can_cache,
            accel: None,
        };

        self.states.push(state);
        self.nfa_sets.push(nfa_states.to_vec());

        // Always insert into state_map — even uncached states — so that the same
        // NFA state-set is never allocated twice across traversals. Without this,
        // a budget-exhausted lazy DFA accumulates duplicate states on every call:
        // state_map misses the first allocation and creates a fresh one on the next.
        // The `cached` flag still controls whether this state's transition table
        // is filled in for reuse; state_map membership only prevents duplication.
        self.state_map.insert(key, idx);

        if can_cache {
            self.cached_count += 1;
        }

        idx
    }

    fn step(&mut self, state_idx: StateId, byte: u8, scratch: &mut LazyDfaScratch) -> StateId {
        let state = &self.states[state_idx.index()];
        if state.cached {
            let cached = state.transitions[byte as usize];
            if !cached.is_none() {
                return cached;
            }
        }

        // Compute the next NFA state-set for this byte
        let nfa_states = &self.nfa_sets[state_idx.index()];

        scratch.next_nfa_states.clear();

        for &nfa_state in nfa_states {
            if nfa_state.is_none() {
                continue;
            }
            let next = self.nfa_arena.dstep(nfa_state, byte);
            if !next.is_none() {
                self.nfa_arena
                    .extend_closure_or_self(next, &mut scratch.next_nfa_states);
            }
        }

        if scratch.next_nfa_states.is_empty() {
            if self.states[state_idx.index()].cached {
                self.states[state_idx.index()].transitions[byte as usize] = StateId::DEAD;
            }
            return StateId::DEAD;
        }

        // Sort for the canonical key, which also makes duplicates adjacent so
        // dedup drops them without a separate set.
        scratch.next_nfa_states.sort_unstable_by_key(|s| s.0);
        scratch.next_nfa_states.dedup();

        let next_idx = self.intern_state(&scratch.next_nfa_states, true);

        // Cache the transition on the current state (only if it's a cached state)
        if self.states[state_idx.index()].cached {
            self.states[state_idx.index()].transitions[byte as usize] = next_idx;
        }

        next_idx
    }

    fn try_compute_accel(&mut self, state_idx: StateId, scratch: &mut LazyDfaScratch) {
        let nfa_states = &self.nfa_sets[state_idx.index()];
        let mut common_accel: Option<AccelInfo> = None;

        for &nfa_state in nfa_states {
            if nfa_state.is_none() {
                continue;
            }
            if let Some(ref accel) = self.nfa_arena[nfa_state].table.accel {
                match &common_accel {
                    None => common_accel = Some(accel.clone()),
                    Some(existing)
                        if existing.len == accel.len
                            && existing.exit_bytes[..existing.len as usize]
                                == accel.exit_bytes[..accel.len as usize] => {}
                    Some(_) => return,
                }
            }
        }

        let accel = match common_accel {
            Some(a) => a,
            None => return,
        };

        // Verify: each exit byte must NOT self-loop in the lazy DFA
        for i in 0..accel.len as usize {
            let next = self.step(state_idx, accel.exit_bytes[i], scratch);
            if next == state_idx {
                return;
            }
        }

        self.states[state_idx.index()].accel = Some(accel);
    }

    /// Accumulate the cache's contribution to matcher-level resource statistics.
    ///
    /// The cache holds its own copy of the NFA and interns DFA states as values
    /// are matched, so its footprint grows with use and counts on top of the
    /// arena the copy came from.
    pub(crate) fn accumulate_matcher_stats(&self, stats: &mut crate::MatcherStats) {
        self.nfa_arena.accumulate_matcher_stats(stats);
        stats.states += self.states.len();
        stats.bytes += self.cached_state_byte_size();
    }

    /// Arena statistics for the copy of the NFA the cache holds.
    pub(crate) fn nfa_arena_stats(&self) -> Stats {
        self.nfa_arena.stats()
    }

    /// How many states the cache may hold within `bytes`, for sizing its state
    /// budget against a byte cap.
    pub(crate) const fn states_within_bytes(bytes: usize) -> usize {
        bytes / (size_of::<LazyDfaState>() + BYTE_CEILING * size_of::<StateId>())
    }

    /// Estimated bytes held by the interned DFA states, excluding the NFA copy.
    ///
    /// Every cached state carries a full 256-entry transition table, so this
    /// outgrows the NFA the cache was built from as matching fills it in.
    fn cached_state_byte_size(&self) -> usize {
        let mut bytes = self.states.capacity() * size_of::<LazyDfaState>()
            + self.nfa_sets.capacity() * size_of::<Vec<StateId>>()
            + self.state_map.capacity() * (size_of::<Vec<u32>>() + size_of::<StateId>());

        for state in &self.states {
            bytes += state.transitions.capacity() * size_of::<StateId>();
            bytes += state.field_transition_ptrs.capacity() * size_of::<usize>();
        }
        for nfa_set in &self.nfa_sets {
            bytes += nfa_set.capacity() * size_of::<StateId>();
        }
        for key in self.state_map.keys() {
            bytes += key.capacity() * size_of::<u32>();
        }

        bytes
    }
}

#[derive(Default)]
struct LazyDfaScratch {
    next_nfa_states: Vec<StateId>,
}

/// Traverse a value through a lazy DFA, collecting field transitions.
///
/// This is the tier-2 matching path: faster than NFA traversal (single state
/// tracked, cached transitions), but uses more memory for the cache.
#[inline]
pub(crate) fn traverse_lazy_dfa(lazy_dfa: &mut LazyDfa, val: &[u8], transitions: &mut Vec<usize>) {
    if lazy_dfa.states.is_empty() {
        return;
    }

    let mut scratch = LazyDfaScratch::default();
    let mut current = StateId::from_index(0); // Start state is always index 0
    let mut remaining = val;

    // Collect field transitions from start state
    transitions.extend_from_slice(&lazy_dfa.states[0].field_transition_ptrs);

    loop {
        // Acceleration: when the current state self-loops with 1-3 memchr exit
        // bytes, jump the cursor straight to the next exit byte. On the
        // terminator step `remaining` is empty, which memchr reports as "no
        // hit", so we fall through to the normal step below.
        if let Some(ref accel) = lazy_dfa.states[current.index()].accel
            && let Some(skip) = accel.try_accelerate(remaining)
        {
            remaining = &remaining[skip.get()..];
            continue;
        }

        let (byte, at_end) = match remaining.split_first() {
            Some((&b, rest)) => {
                remaining = rest;
                (b, false)
            }
            None => (ARENA_VALUE_TERMINATOR, true),
        };

        let next = lazy_dfa.step(current, byte, &mut scratch);
        if next.is_dead() {
            return;
        }

        // On a self-loop, try to compute acceleration for future bytes. This is
        // a pure speed-up: `try_compute_accel` only installs an accelerator it
        // has re-checked, so attempting it more or less often never changes a
        // match.
        if next == current && lazy_dfa.states[current.index()].accel.is_none() {
            lazy_dfa.try_compute_accel(current, &mut scratch);
        }

        current = next;

        // Collect field transitions
        transitions.extend_from_slice(&lazy_dfa.states[current.index()].field_transition_ptrs);

        if at_end {
            break;
        }
    }
}

/// Buffers for arena NFA traversal (avoid allocation during matching).
#[derive(Default)]
pub struct NfaBuffers {
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

impl NfaBuffers {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
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
fn try_accelerate_arena(table: &SmallTable, remaining: &[u8]) -> Option<NonZero<usize>> {
    table.accel.as_ref()?.try_accelerate(remaining)
}

/// Traverse an arena-based NFA on a value.
///
/// This is the arena equivalent of `traverse_nfa` but uses index-based
/// state references, allowing true cyclic structures.
/// [`StateArena::precompute_epsilon_closures`] must have been called after the
/// arena's last structural change.
#[inline]
#[allow(clippy::too_many_lines)] // hot loop kept monolithic so the inliner sees the whole stepping path
pub fn traverse_arena_nfa(arena: &StateArena, start: StateId, val: &[u8], bufs: &mut NfaBuffers) {
    bufs.clear();
    if start.is_none() {
        return;
    }
    debug_assert_eq!(
        arena.closures_computed,
        arena.states.len(),
        "epsilon closures must be precomputed before NFA traversal"
    );

    // Seed with the start state directly rather than its closure: the entry
    // state only ever has byte transitions. Epsilon edges (spinner loopbacks,
    // splices, regexp branching) first appear in states reached after at least
    // one input byte, so the start state's closure is always just itself.
    bufs.current_states.push(start);

    let mut remaining = val;

    loop {
        if bufs.current_states.is_empty() {
            break;
        }

        // State acceleration: ASCII-only negated patterns like [^x]+ park the NFA
        // in a single self-looping state. When 1-3 exit bytes are known we use
        // memchr to jump the cursor to the next exit byte. On the terminator
        // step `remaining` is empty and we fall through to the normal step.
        if bufs.current_states.len() == 1 {
            let state_id = bufs.current_states[0];
            let state = &arena[state_id];
            if let Some(skip) = try_accelerate_arena(&state.table, remaining) {
                remaining = &remaining[skip.get()..];
                continue;
            }
        }

        // Advance the cursor before the step block below, so only `at_end`
        // crosses its borrows. Carrying the slice tail across the block instead
        // regresses the hot epsilon path.
        let (byte, at_end) = match remaining.split_first() {
            Some((&b, rest)) => {
                remaining = rest;
                (b, false)
            }
            None => (ARENA_VALUE_TERMINATOR, true),
        };

        // Destructure bufs for split borrows: iterate current_states immutably
        // while pushing to next_states mutably.
        let NfaBuffers {
            ref mut current_states,
            ref mut next_states,
            ref mut transitions,
            ref mut seen_transitions,
            ref mut step_gen,
            ref mut seen_states,
        } = *bufs;

        if arena.ft_ptrs.is_empty() {
            // Mutable/test path: read field_transitions directly
            for &state_id in current_states.iter() {
                let closure = arena.closure_of(state_id);

                if closure.is_empty() {
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
        } else {
            // Frozen path: use precomputed flat buffers
            for &state_id in current_states.iter() {
                let closure = arena.closure_of(state_id);

                if closure.is_empty() {
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
        }

        // Nested quantifiers like (([abc]?)*)+ create epsilon loops that
        // compound duplicate states across steps. Compact in place when growth
        // is detected; this only drops exact duplicates, so the threshold is a
        // speed knob, not a correctness one.
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

        if at_end {
            break;
        }
    }

    // Check final states for matches (split borrows to avoid take)
    let NfaBuffers {
        ref current_states,
        ref mut transitions,
        ref mut seen_transitions,
        ..
    } = *bufs;
    if arena.ft_ptrs.is_empty() {
        for &state_id in current_states {
            let closure = arena.closure_of(state_id);
            if closure.is_empty() {
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
    } else {
        for &state_id in current_states {
            let closure = arena.closure_of(state_id);
            if closure.is_empty() {
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
    let mut remaining = val;

    loop {
        if has_flat {
            for &ptr in arena.ft_ptrs_of(current) {
                transitions.push(ptr);
            }
        } else {
            for ft in &arena[current].field_transitions {
                transitions.push(Arc::as_ptr(ft) as usize);
            }
        }

        // Acceleration: for self-loop states with 1-3 exit bytes, use memchr to
        // jump the cursor directly to the next interesting byte. On the
        // terminator step `remaining` is empty and we fall through to the
        // normal step.
        if let Some(skip) = try_accelerate_arena(&arena[current].table, remaining) {
            // A self-loop (accelerated) state must not be accepting. If it were,
            // the FT collection at the top of this loop would fire again on the
            // next iteration, emitting duplicate match results.
            // `debug_assert!` fires only in debug/test builds; zero cost in release.
            debug_assert!(
                if has_flat {
                    arena.ft_ptrs_of(current).is_empty()
                } else {
                    arena[current].field_transitions.is_empty()
                },
                "accelerated state {current:?} has field transitions; they would be collected redundantly on each skip"
            );
            remaining = &remaining[skip.get()..];
            continue;
        }

        let (byte, at_end) = match remaining.split_first() {
            Some((&b, rest)) => {
                remaining = rest;
                (b, false)
            }
            None => (ARENA_VALUE_TERMINATOR, true),
        };

        let next = arena.dstep(current, byte);
        if next.is_none() {
            return;
        }
        current = next;

        if at_end {
            break;
        }
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
        if arena.ft_ptrs.is_empty() {
            for ft in &arena[current].field_transitions {
                transitions.push(Arc::as_ptr(ft) as usize);
            }
        } else {
            for &ptr in arena.ft_ptrs_of(current) {
                transitions.push(ptr);
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
#[must_use]
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

    // Memoization: (state1_id, state2_id) -> merged_state_id in new arena.
    // i32 lets us encode StateId::NONE as -1.
    let mut memo: FxHashMap<(StateId, StateId), StateId> = FxHashMap::default();
    let mut new_arena = StateArena::new();

    let start =
        merge_arena_states_recursive(arena1, start1, arena2, start2, &mut new_arena, &mut memo);

    new_arena.precompute_epsilon_closures();
    (new_arena, start)
}

/// Clone a subset of an arena starting from a given state.
pub(crate) fn clone_arena_subset(arena: &StateArena, start: StateId) -> (StateArena, StateId) {
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
    let mut new_table = SmallTable {
        ceilings: old_table.ceilings.clone(),
        steps: SmallVec::with_capacity(old_table.steps.len()),
        epsilons: SmallVec::with_capacity(old_table.epsilons.len()),
        accel: old_table.accel.clone(),
    };

    for &step_id in &old_table.steps {
        let new_step = clone_state_recursive(arena, step_id, new_arena, id_map);
        new_table.steps.push(new_step);
    }

    for &eps_id in &old_table.epsilons {
        let new_eps = clone_state_recursive(arena, eps_id, new_arena, id_map);
        new_table.epsilons.push(new_eps);
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
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
) -> StateId {
    // Memo by `(StateId, StateId)`. `StateId::NONE` already encodes the
    // "no state on this side" key without needing a separate sentinel.
    let key = (state1, state2);

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
    table: &SmallTable,
    _other_arena: &StateArena,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    is_arena1: bool,
) -> SmallTable {
    let mut new_table = SmallTable {
        ceilings: table.ceilings.clone(),
        steps: SmallVec::with_capacity(table.steps.len()),
        epsilons: SmallVec::with_capacity(table.epsilons.len()),
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

    new_table
}

/// Merge two arena tables byte-by-byte.
fn merge_arena_tables(
    arena1: &StateArena,
    table1: &SmallTable,
    arena2: &StateArena,
    table2: &SmallTable,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
) -> SmallTable {
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
    let mut result = SmallTable::new();
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

/// Unpack an SmallTable into a 256-element array.
fn unpack_arena_table(table: &SmallTable, unpacked: &mut [StateId; BYTE_CEILING]) {
    let mut byte_idx = 0usize;
    for (i, &ceiling) in table.ceilings.iter().enumerate() {
        let end = ceiling as usize;
        unpacked[byte_idx..end].fill(table.steps[i]);
        byte_idx = end;
    }
}

// =============================================================================
// Alternation branch determinization
// =============================================================================

/// Fold alternation branch entry states into one deterministic entry, merging
/// their byte transitions and re-converging wherever two branches reach the
/// same state.
///
/// All branches share the continuation `next_step`, so a state both branches
/// reach is returned directly — the shared tail stays a single subgraph. Two
/// kinds of state must be spliced behind an epsilon rather than byte-merged:
/// an entry that begins with an epsilon, whose `+`/`*` loop must remain NFA
/// structure; and `next_step` itself, which is opaque here because it may be a
/// `+`/`*` loopback whose edges are wired up only after this returns — merging
/// or collapsing it would drop those edges.
///
/// New states are appended to `arena`; the original branch entries are left
/// unreachable and dropped by the later reachability compaction
/// ([`clone_arena_subset`]).
pub(crate) fn determinize_branch_starts(
    arena: &mut StateArena,
    starts: &[StateId],
    next_step: StateId,
) -> StateId {
    debug_assert!(
        !starts.is_empty(),
        "alternation must have at least one branch"
    );
    let mut acc = starts[0];
    // The pair memo is local to each fold step: branch entries are built from
    // disjoint state sets, so a pair from one step never recurs in the next.
    let mut memo: FxHashMap<(StateId, StateId), StateId> = FxHashMap::default();
    for &next in &starts[1..] {
        memo.clear();
        acc = merge_branch_pair(arena, acc, next, next_step, &mut memo);
    }
    acc
}

/// Merge two alternation branch states within a single arena. `boundary` is the
/// shared continuation, treated as an opaque leaf (never baked or collapsed).
fn merge_branch_pair(
    arena: &mut StateArena,
    a: StateId,
    b: StateId,
    boundary: StateId,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
) -> StateId {
    // Converging branches share their state directly; no new node is allocated.
    if a == b {
        return a;
    }
    if a.is_none() {
        return b;
    }
    if b.is_none() {
        return a;
    }
    if let Some(&cached) = memo.get(&(a, b)) {
        return cached;
    }

    // Epsilon-led entries (a quantifier loop) and the boundary cannot be
    // byte-merged; they are joined behind an epsilon splice instead.
    let needs_splice = !arena[a].table.epsilons.is_empty()
        || !arena[b].table.epsilons.is_empty()
        || a == boundary
        || b == boundary;

    // Insert the memo entry before recursing so byte cycles terminate.
    let new_id = arena.alloc();
    memo.insert((a, b), new_id);

    if needs_splice {
        // The new state's epsilons are the two entries' real targets, so both
        // remain reachable as alternatives without flattening their loops.
        let mut targets: SmallVec<[StateId; 2]> = SmallVec::new();
        let mut visited: FxHashSet<u32> = FxHashSet::default();
        collect_branch_epsilon_targets(arena, a, boundary, &mut visited, &mut targets);
        collect_branch_epsilon_targets(arena, b, boundary, &mut visited, &mut targets);
        arena[new_id].table = SmallTable {
            ceilings: smallvec![BYTE_CEILING_U8],
            steps: smallvec![StateId::NONE],
            epsilons: targets,
            accel: None,
        };
        return new_id;
    }

    // Byte-wise merge: for each byte value, merge the pair of target states.
    let mut unpacked_a = [StateId::NONE; BYTE_CEILING];
    let mut unpacked_b = [StateId::NONE; BYTE_CEILING];
    unpack_arena_table(&arena[a].table, &mut unpacked_a);
    unpack_arena_table(&arena[b].table, &mut unpacked_b);

    let mut field_transitions = arena[a].field_transitions.clone();
    field_transitions.extend(arena[b].field_transitions.iter().cloned());

    let mut merged = [StateId::NONE; BYTE_CEILING];
    for i in 0..BYTE_CEILING {
        merged[i] = merge_branch_pair(arena, unpacked_a[i], unpacked_b[i], boundary, memo);
    }

    arena[new_id].field_transitions = field_transitions;
    arena[new_id].table.pack(&merged);
    new_id
}

/// Collect the real (non-epsilon-only) targets reachable from `state` through
/// epsilon-only splice states, so the splice points at real states instead of
/// chaining through other splices.
///
/// `boundary` is kept as a leaf, never collapsed: its epsilons may not be wired
/// yet — a `+`/`*` loopback is empty here until its loop/exit edges are added
/// later — so descending into it could capture an incomplete target set.
/// `visited` only avoids re-walking an already-flattened splice.
fn collect_branch_epsilon_targets(
    arena: &StateArena,
    state: StateId,
    boundary: StateId,
    visited: &mut FxHashSet<u32>,
    out: &mut SmallVec<[StateId; 2]>,
) {
    if state.is_none() || !visited.insert(state.0) {
        return;
    }
    if state != boundary && is_epsilon_only_state(arena, state) {
        for &eps in &arena[state].table.epsilons {
            collect_branch_epsilon_targets(arena, eps, boundary, visited, out);
        }
    } else {
        out.push(state);
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
#[must_use]
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

    // Memoization: (state1_id, state2_id) -> merged_state_id in new arena.
    let mut memo: FxHashMap<(StateId, StateId), StateId> = FxHashMap::default();
    let mut new_arena = StateArena::new();

    let start =
        merge_arena_nfa_states_recursive(arena1, start1, arena2, start2, &mut new_arena, &mut memo);

    // Precompute epsilon closures for all states in the merged arena.
    // This eliminates per-byte DFS computation during NFA traversal.
    new_arena.precompute_epsilon_closures();

    (new_arena, start)
}

/// Merge two FA start states into a combined start state.
///
/// The named entry point for the value-matcher start merges, mirroring Go's
/// `mergeStartStates`. An arena start is already a state (`arena` + `StateId`),
/// so this delegates to [`merge_arena_nfas`], which stays for callers composing
/// arenas directly (regexp NFA construction, tests).
#[must_use]
pub fn merge_start_states(
    arena1: &StateArena,
    start1: StateId,
    arena2: &StateArena,
    start2: StateId,
) -> (StateArena, StateId) {
    merge_arena_nfas(arena1, start1, arena2, start2)
}

/// Append the union of `target_start` and `source_start` into `target`.
///
/// Existing target states are treated as immutable. Target-only branches reuse
/// their existing `StateId`s, source-only branches are copied into `target`,
/// and overlapping target/source pairs allocate newly appended states. The
/// returned state is the new live start; callers should install it only after
/// budget checks have passed.
pub fn append_merge_arena_nfas(
    target: &mut StateArena,
    target_start: StateId,
    source: &StateArena,
    source_start: StateId,
) -> StateId {
    if target_start.is_none() {
        let mut memo = FxHashMap::default();
        return append_merge_nfa_states_recursive(
            target,
            StateId::NONE,
            source,
            source_start,
            &mut memo,
        );
    }
    if source_start.is_none() {
        return target_start;
    }

    let mut memo = FxHashMap::default();
    append_merge_nfa_states_recursive(target, target_start, source, source_start, &mut memo)
}

/// Check if a state is an "epsilon-only" splice state created during merges.
///
/// These synthetic states branch into multiple epsilon targets with no byte
/// transitions and no field transitions: the table holds a single ceiling
/// whose step is `NONE`, so every byte falls through and the only outgoing
/// edge is via epsilons.
fn is_epsilon_only_state(arena: &StateArena, state_id: StateId) -> bool {
    if state_id.is_none() {
        return false;
    }
    let state = &arena[state_id];
    !state.table.epsilons.is_empty()
        && state.table.ceilings.len() == 1
        && state.table.steps[0].is_none()
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

/// Check if a state is a spinner/spinout state — a wildcard self-loop where
/// every non-forbidden byte transitions back to the state itself, encoded
/// as at least one entry in `steps` equal to the state's own id.
fn is_spinout_state(arena: &StateArena, state_id: StateId) -> bool {
    if state_id.is_none() {
        return false;
    }
    let state = &arena[state_id];
    state.table.steps.contains(&state_id) && state.table.epsilons.len() <= 1
}

fn append_merge_nfa_states_recursive(
    target: &mut StateArena,
    target_state: StateId,
    source: &StateArena,
    source_state: StateId,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
) -> StateId {
    let key = (target_state, source_state);
    if let Some(&cached) = memo.get(&key) {
        return cached;
    }

    if target_state.is_none() && source_state.is_none() {
        return StateId::NONE;
    }

    if source_state.is_none() {
        memo.insert(key, target_state);
        return target_state;
    }

    if target_state.is_none() {
        let new_id = target.alloc();
        memo.insert(key, new_id);

        let source_state_ref = source[source_state].clone();
        target[new_id].field_transitions = source_state_ref.field_transitions.clone();
        target[new_id].table =
            append_remap_source_nfa_table(target, source, &source_state_ref.table, memo);
        return new_id;
    }

    let new_id = target.alloc();
    memo.insert(key, new_id);

    let target_state_ref = target[target_state].clone();
    let source_state_ref = source[source_state].clone();

    let target_has_spinout = is_spinout_state(target, target_state);
    let source_has_spinout = is_spinout_state(source, source_state);
    let target_has_epsilons = !target_state_ref.table.epsilons.is_empty();
    let source_has_epsilons = !source_state_ref.table.epsilons.is_empty();

    if target_has_spinout && source_has_spinout {
        append_merge_dual_spinout_states(
            target,
            &target_state_ref,
            source,
            &source_state_ref,
            memo,
            new_id,
        );
        return new_id;
    }

    if (target_has_spinout && !source_has_epsilons) || (source_has_spinout && !target_has_epsilons)
    {
        return append_asymmetric_spinner_merge(
            target,
            target_state,
            &target_state_ref,
            source,
            source_state,
            &source_state_ref,
            target_has_spinout,
            memo,
            new_id,
        );
    }

    if target_has_epsilons || source_has_epsilons {
        let cloned_source =
            append_merge_nfa_states_recursive(target, StateId::NONE, source, source_state, memo);
        let epsilons = flatten_epsilon_targets(target, &[target_state, cloned_source]);
        target[new_id].table = SmallTable {
            ceilings: smallvec![BYTE_CEILING_U8],
            steps: smallvec![StateId::NONE],
            epsilons,
            accel: None,
        };
        return new_id;
    }

    let combined_table = append_merge_nfa_tables_bytewise(
        target,
        &target_state_ref.table,
        source,
        &source_state_ref.table,
        memo,
    );

    let mut field_transitions = target_state_ref.field_transitions;
    field_transitions.extend(source_state_ref.field_transitions);

    target[new_id].table = combined_table;
    target[new_id].field_transitions = field_transitions;

    new_id
}

fn append_merge_dual_spinout_states(
    target: &mut StateArena,
    target_state_ref: &FaState,
    source: &StateArena,
    source_state_ref: &FaState,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    new_id: StateId,
) {
    let mut combined_table = append_merge_nfa_tables_bytewise(
        target,
        &target_state_ref.table,
        source,
        &source_state_ref.table,
        memo,
    );

    let mut merged_epsilons: SmallVec<[StateId; 2]> = SmallVec::new();
    for &eps in &target_state_ref.table.epsilons {
        let merged = append_merge_nfa_states_recursive(target, eps, source, StateId::NONE, memo);
        if !merged.is_none() {
            merged_epsilons.push(merged);
        }
    }
    for &eps in &source_state_ref.table.epsilons {
        let merged = append_merge_nfa_states_recursive(target, StateId::NONE, source, eps, memo);
        if !merged.is_none() {
            merged_epsilons.push(merged);
        }
    }
    combined_table.epsilons = merged_epsilons;

    let mut field_transitions = target_state_ref.field_transitions.clone();
    field_transitions.extend(source_state_ref.field_transitions.iter().cloned());

    target[new_id].table = combined_table;
    target[new_id].field_transitions = field_transitions;
}

#[allow(clippy::too_many_arguments)]
fn append_asymmetric_spinner_merge(
    target: &mut StateArena,
    target_state: StateId,
    target_state_ref: &FaState,
    source: &StateArena,
    source_state: StateId,
    source_state_ref: &FaState,
    target_has_spinout: bool,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    new_id: StateId,
) -> StateId {
    let mut target_unpacked = [StateId::NONE; BYTE_CEILING];
    let mut source_unpacked = [StateId::NONE; BYTE_CEILING];
    unpack_arena_table(&target_state_ref.table, &mut target_unpacked);
    unpack_arena_table(&source_state_ref.table, &mut source_unpacked);

    let mut target_clone_map = FxHashMap::default();
    let mut merged_unpacked = [StateId::NONE; BYTE_CEILING];
    for i in 0..BYTE_CEILING {
        merged_unpacked[i] = if target_has_spinout {
            append_asymmetric_spinner_byte_target_spinout(
                target,
                target_state,
                target_unpacked[i],
                target_state_ref,
                source,
                source_unpacked[i],
                memo,
                new_id,
            )
        } else {
            append_asymmetric_spinner_byte_source_spinout(
                target,
                target_unpacked[i],
                source,
                source_state,
                source_unpacked[i],
                source_state_ref,
                memo,
                &mut target_clone_map,
                new_id,
            )
        };
    }

    let mut combined_table = SmallTable::new();
    combined_table.pack(&merged_unpacked);

    let spinner_epsilons = if target_has_spinout {
        &target_state_ref.table.epsilons
    } else {
        &source_state_ref.table.epsilons
    };
    for &spinner_eps in spinner_epsilons {
        let merged = if target_has_spinout {
            append_merge_nfa_states_recursive(target, spinner_eps, source, StateId::NONE, memo)
        } else {
            append_merge_nfa_states_recursive(target, StateId::NONE, source, spinner_eps, memo)
        };
        if !merged.is_none() {
            combined_table.epsilons.push(merged);
        }
    }

    let mut field_transitions = target_state_ref.field_transitions.clone();
    field_transitions.extend(source_state_ref.field_transitions.iter().cloned());

    target[new_id].table = combined_table;
    target[new_id].field_transitions = field_transitions;
    new_id
}

#[allow(clippy::too_many_arguments)]
fn append_asymmetric_spinner_byte_target_spinout(
    target: &mut StateArena,
    spinner_id: StateId,
    spinner_next: StateId,
    spinner_state: &FaState,
    source: &StateArena,
    other_next: StateId,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    new_id: StateId,
) -> StateId {
    if spinner_next.is_none() {
        return StateId::NONE;
    }

    if other_next.is_none() {
        if spinner_next == spinner_id {
            return new_id;
        }
        return append_merge_nfa_states_recursive(
            target,
            spinner_next,
            source,
            StateId::NONE,
            memo,
        );
    }

    if spinner_next == spinner_id {
        let remapped_other =
            append_merge_nfa_states_recursive(target, StateId::NONE, source, other_next, memo);
        append_add_spinner_backedge(
            target,
            remapped_other,
            new_id,
            &spinner_state.field_transitions,
        );
        return remapped_other;
    }

    let merged_branch =
        append_merge_nfa_states_recursive(target, spinner_next, source, other_next, memo);
    if !merged_branch.is_none() {
        target[merged_branch].table.epsilons.push(new_id);
    }
    merged_branch
}

#[allow(clippy::too_many_arguments)]
fn append_asymmetric_spinner_byte_source_spinout(
    target: &mut StateArena,
    other_next: StateId,
    source: &StateArena,
    spinner_id: StateId,
    spinner_next: StateId,
    spinner_state: &FaState,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    target_clone_map: &mut FxHashMap<StateId, StateId>,
    new_id: StateId,
) -> StateId {
    if spinner_next.is_none() {
        return StateId::NONE;
    }

    if other_next.is_none() {
        if spinner_next == spinner_id {
            return new_id;
        }
        return append_merge_nfa_states_recursive(
            target,
            StateId::NONE,
            source,
            spinner_next,
            memo,
        );
    }

    if spinner_next == spinner_id {
        let remapped_other = append_clone_target_state(target, other_next, target_clone_map);
        append_add_spinner_backedge(
            target,
            remapped_other,
            new_id,
            &spinner_state.field_transitions,
        );
        return remapped_other;
    }

    let merged_branch =
        append_merge_nfa_states_recursive(target, other_next, source, spinner_next, memo);
    if !merged_branch.is_none() {
        target[merged_branch].table.epsilons.push(new_id);
    }
    merged_branch
}

fn append_add_spinner_backedge(
    target: &mut StateArena,
    state_id: StateId,
    backedge: StateId,
    spinner_field_transitions: &SmallVec<[Arc<FieldMatcher>; 1]>,
) {
    if state_id.is_none() {
        return;
    }
    target[state_id].table.epsilons.push(backedge);
    target[state_id]
        .field_transitions
        .extend(spinner_field_transitions.iter().cloned());
}

fn append_clone_target_state(
    target: &mut StateArena,
    state_id: StateId,
    id_map: &mut FxHashMap<StateId, StateId>,
) -> StateId {
    if state_id.is_none() {
        return StateId::NONE;
    }
    if let Some(&new_id) = id_map.get(&state_id) {
        return new_id;
    }

    let new_id = target.alloc();
    id_map.insert(state_id, new_id);

    let old_state = target[state_id].clone();
    target[new_id].field_transitions = old_state.field_transitions;
    target[new_id].table = append_clone_target_table(target, &old_state.table, id_map);
    new_id
}

fn append_clone_target_table(
    target: &mut StateArena,
    table: &SmallTable,
    id_map: &mut FxHashMap<StateId, StateId>,
) -> SmallTable {
    let mut new_table = SmallTable {
        ceilings: table.ceilings.clone(),
        steps: SmallVec::with_capacity(table.steps.len()),
        epsilons: SmallVec::with_capacity(table.epsilons.len()),
        accel: table.accel.clone(),
    };

    for &step_id in &table.steps {
        new_table
            .steps
            .push(append_clone_target_state(target, step_id, id_map));
    }
    for &eps_id in &table.epsilons {
        new_table
            .epsilons
            .push(append_clone_target_state(target, eps_id, id_map));
    }

    new_table
}

fn append_remap_source_nfa_table(
    target: &mut StateArena,
    source: &StateArena,
    table: &SmallTable,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
) -> SmallTable {
    let mut new_table = SmallTable {
        ceilings: table.ceilings.clone(),
        steps: SmallVec::with_capacity(table.steps.len()),
        epsilons: SmallVec::with_capacity(table.epsilons.len()),
        accel: table.accel.clone(),
    };

    for &step_id in &table.steps {
        let merged =
            append_merge_nfa_states_recursive(target, StateId::NONE, source, step_id, memo);
        new_table.steps.push(merged);
    }

    for &eps_id in &table.epsilons {
        let merged = append_merge_nfa_states_recursive(target, StateId::NONE, source, eps_id, memo);
        new_table.epsilons.push(merged);
    }

    new_table
}

fn append_merge_nfa_tables_bytewise(
    target: &mut StateArena,
    target_table: &SmallTable,
    source: &StateArena,
    source_table: &SmallTable,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
) -> SmallTable {
    let mut unpacked_target = [StateId::NONE; BYTE_CEILING];
    let mut unpacked_source = [StateId::NONE; BYTE_CEILING];

    unpack_arena_table(target_table, &mut unpacked_target);
    unpack_arena_table(source_table, &mut unpacked_source);

    let mut merged_unpacked = [StateId::NONE; BYTE_CEILING];
    for i in 0..BYTE_CEILING {
        merged_unpacked[i] = append_merge_nfa_states_recursive(
            target,
            unpacked_target[i],
            source,
            unpacked_source[i],
            memo,
        );
    }

    let mut result = SmallTable::new();
    result.pack(&merged_unpacked);

    for &eps in &target_table.epsilons {
        let merged = append_merge_nfa_states_recursive(target, eps, source, StateId::NONE, memo);
        if !merged.is_none() {
            result.epsilons.push(merged);
        }
    }
    for &eps in &source_table.epsilons {
        let merged = append_merge_nfa_states_recursive(target, StateId::NONE, source, eps, memo);
        if !merged.is_none() {
            result.epsilons.push(merged);
        }
    }

    result
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
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
) -> StateId {
    // Memo by `(StateId, StateId)`. `StateId::NONE` already encodes the
    // "no state on this side" key without needing a separate sentinel.
    let key = (state1, state2);

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
        merge_dual_spinout_states(arena1, s1, arena2, s2, new_arena, memo, new_id);
        return new_id;
    }

    // Case 2: Asymmetric spinner merge - one spinout, other has no epsilons.
    // Mirrors Go's asymmetricSpinnerMerge: when a spinout is merged with a
    // non-epsilon state, we can avoid creating splice states by inlining the
    // epsilon-to-spinner relationship into the merged table.
    if (s1_has_spinout && !s2_has_epsilons) || (s2_has_spinout && !s1_has_epsilons) {
        return asymmetric_spinner_merge(
            arena1,
            state1,
            arena2,
            state2,
            s1_has_spinout,
            new_arena,
            memo,
            new_id,
        );
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

        new_arena[new_id].table = SmallTable {
            ceilings: smallvec![BYTE_CEILING_U8],
            steps: smallvec![StateId::NONE],
            epsilons,
            accel: None,
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

/// Merge two spinout states (Case 1 of `merge_arena_nfa_states_recursive`).
///
/// Bytewise-merges both tables; the byte-dot self-loops recurse back through
/// the memo and resolve to `new_id`, marking the merged state as a spinout.
/// Epsilons (0 or 1 per side) are remapped via one-sided merges.
fn merge_dual_spinout_states(
    arena1: &StateArena,
    s1: &FaState,
    arena2: &StateArena,
    s2: &FaState,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    new_id: StateId,
) {
    let mut combined_table =
        merge_nfa_tables_bytewise(arena1, &s1.table, arena2, &s2.table, new_arena, memo);

    let mut merged_epsilons: SmallVec<[StateId; 2]> = SmallVec::new();
    for &eps1 in &s1.table.epsilons {
        let merged =
            merge_arena_nfa_states_recursive(arena1, eps1, arena2, StateId::NONE, new_arena, memo);
        if !merged.is_none() {
            merged_epsilons.push(merged);
        }
    }
    for &eps2 in &s2.table.epsilons {
        let merged =
            merge_arena_nfa_states_recursive(arena1, StateId::NONE, arena2, eps2, new_arena, memo);
        if !merged.is_none() {
            merged_epsilons.push(merged);
        }
    }
    combined_table.epsilons = merged_epsilons;

    let mut field_transitions = s1.field_transitions.clone();
    field_transitions.extend(s2.field_transitions.iter().cloned());

    new_arena[new_id].table = combined_table;
    new_arena[new_id].field_transitions = field_transitions;
}

/// Merge a spinout state in one arena with a non-epsilon state in the other, mirroring
/// Go's `asymmetricSpinnerMerge`. Inlining the spinner's self-loop into the merged byte
/// table avoids creating splice states for the cross-arena epsilon back-edge.
// Two arenas + two state ids + flags + outputs naturally hit 8 params; bundling
// would only add indirection for a single-use helper.
#[allow(clippy::too_many_arguments)]
fn asymmetric_spinner_merge(
    arena1: &StateArena,
    state1: StateId,
    arena2: &StateArena,
    state2: StateId,
    s1_has_spinout: bool,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    new_id: StateId,
) -> StateId {
    let s1 = &arena1[state1];
    let s2 = &arena2[state2];
    let (spinner_arena, spinner_id, spinner_table, other_arena, other_table) = if s1_has_spinout {
        (arena1, state1, &s1.table, arena2, &s2.table)
    } else {
        (arena2, state2, &s2.table, arena1, &s1.table)
    };

    let mut spinner_unpacked = [StateId::NONE; BYTE_CEILING];
    let mut other_unpacked = [StateId::NONE; BYTE_CEILING];
    unpack_arena_table(spinner_table, &mut spinner_unpacked);
    unpack_arena_table(other_table, &mut other_unpacked);

    let mut merged_unpacked = [StateId::NONE; BYTE_CEILING];
    for i in 0..BYTE_CEILING {
        merged_unpacked[i] = merge_asymmetric_spinner_byte(
            spinner_arena,
            spinner_id,
            other_arena,
            spinner_unpacked[i],
            other_unpacked[i],
            s1_has_spinout,
            new_arena,
            memo,
            new_id,
            arena1,
            state1,
            arena2,
            state2,
        );
    }

    let mut combined_table = SmallTable::new();
    combined_table.pack(&merged_unpacked);

    // Remap spinner's epsilons (0 or 1).
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

    let mut field_transitions = arena1[state1].field_transitions.clone();
    field_transitions.extend(arena2[state2].field_transitions.iter().cloned());

    new_arena[new_id].table = combined_table;
    new_arena[new_id].field_transitions = field_transitions;
    new_id
}

/// Resolve one byte slot of the asymmetric spinner merge. Returns the `StateId` to
/// place in `merged_unpacked[i]`.
#[allow(clippy::too_many_arguments)]
fn merge_asymmetric_spinner_byte(
    spinner_arena: &StateArena,
    spinner_id: StateId,
    other_arena: &StateArena,
    spinner_next: StateId,
    other_next: StateId,
    s1_has_spinout: bool,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    new_id: StateId,
    arena1: &StateArena,
    state1: StateId,
    arena2: &StateArena,
    state2: StateId,
) -> StateId {
    if spinner_next.is_none() {
        // Illegal UTF-8 byte.
        return StateId::NONE;
    }

    if other_next.is_none() {
        // Only the spinner has a transition - remap it.
        if spinner_next == spinner_id {
            return new_id; // self-loop maps to combined
        }
        return if s1_has_spinout {
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

    if spinner_next == spinner_id {
        // Spinner self-loops here AND other has a branch. Create a state with the
        // other's transitions plus an epsilon back to the combined spinner. This is
        // the key optimization: avoid a full merge, just add an epsilon.
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
        if !remapped_other.is_none() {
            new_arena[remapped_other].table.epsilons.push(new_id);
            // Copy the spinner's field transitions onto the escape state.
            let spinner_fts = if s1_has_spinout {
                &arena1[state1].field_transitions
            } else {
                &arena2[state2].field_transitions
            };
            for ft in spinner_fts {
                new_arena[remapped_other].field_transitions.push(ft.clone());
            }
        }
        return remapped_other;
    }

    // Spinner has a real branch (not a self-loop) AND other has a branch. Merge
    // them, then add an epsilon back to the combined spinner.
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
    merged_branch
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
    let mut new_table = SmallTable {
        ceilings: old_table.ceilings.clone(),
        steps: SmallVec::with_capacity(old_table.steps.len()),
        epsilons: SmallVec::with_capacity(old_table.epsilons.len()),
        accel: old_table.accel.clone(),
    };

    for &step_id in &old_table.steps {
        let new_step = clone_state_into_arena(source_arena, step_id, target_arena, id_map);
        new_table.steps.push(new_step);
    }

    for &eps_id in &old_table.epsilons {
        let new_eps = clone_state_into_arena(source_arena, eps_id, target_arena, id_map);
        new_table.epsilons.push(new_eps);
    }

    target_arena[new_id].table = new_table;

    new_id
}

/// Remap a table from source arena to the merged arena (NFA version).
fn remap_nfa_table_recursive(
    source_arena: &StateArena,
    table: &SmallTable,
    _other_arena: &StateArena,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
    is_arena1: bool,
) -> SmallTable {
    let mut new_table = SmallTable {
        ceilings: table.ceilings.clone(),
        steps: SmallVec::with_capacity(table.steps.len()),
        epsilons: SmallVec::with_capacity(table.epsilons.len()),
        accel: table.accel.clone(),
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
    table1: &SmallTable,
    arena2: &StateArena,
    table2: &SmallTable,
    new_arena: &mut StateArena,
    memo: &mut FxHashMap<(StateId, StateId), StateId>,
) -> SmallTable {
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
    let mut result = SmallTable::new();
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
#[must_use]
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
#[must_use]
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
#[must_use]
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
            let start = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));
            return start;
        }
        // No match for equal case - return a state with no transitions
        return arena.alloc();
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

    let mut table = SmallTable::new();
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

        let mut table = SmallTable::new();
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
    for b in (bound_byte + 1)..(BYTE_CEILING_U8) {
        if b != ARENA_VALUE_TERMINATOR {
            unpacked[b as usize] = match_state;
        }
    }

    // VALUE_TERMINATOR and bytes < bound_byte: no transition (implicit fail)

    let mut table = SmallTable::new();
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
            return arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));
        }
        // At least one exclusive - reject equal
        return arena.alloc();
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

        let mut table = SmallTable::new();
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

    let mut table = SmallTable::new();
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
#[must_use]
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
        return arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));
    }

    // Recursive step: build rest of chain first, then prepend current byte
    let continuation = make_string_arena_fa_step(val, index + 1, match_state, arena);

    arena.alloc_with_table(SmallTable::with_mappings(
        StateId::NONE,
        &[val[index]],
        &[continuation],
    ))
}

/// Rollback data for an in-place trie insertion.
///
/// `insert_string` and `insert_suffix` append states and edit at most one
/// existing state. This records just enough old state to undo that mutation if
/// the caller rejects the insertion after a budget check.
#[doc(hidden)]
pub struct ArenaInsertRollback {
    old_len: usize,
    touched_state: StateId,
    old_table: SmallTable,
    old_field_transitions: SmallVec<[Arc<FieldMatcher>; 1]>,
}

impl ArenaInsertRollback {
    /// Undo the insertion this token was returned for.
    pub fn rollback(self, arena: &mut StateArena) {
        let touched = &mut arena[self.touched_state];
        touched.table = self.old_table;
        touched.field_transitions = self.old_field_transitions;
        arena.truncate_states(self.old_len);
    }
}

fn arena_insert_rollback(
    arena: &StateArena,
    old_len: usize,
    touched_state: StateId,
) -> ArenaInsertRollback {
    ArenaInsertRollback {
        old_len,
        touched_state,
        old_table: arena[touched_state].table.clone(),
        old_field_transitions: arena[touched_state].field_transitions.clone(),
    }
}

/// Insert a string into an existing arena in-place, sharing prefix structure.
///
/// This is O(L) per string where L is the string length, avoiding the O(n²) cost
/// of repeated `merge_arena_nfas` calls. It walks the existing trie, following
/// existing transitions where they match and creating new states where they diverge.
pub fn insert_string(
    arena: &mut StateArena,
    start: StateId,
    val: &[u8],
    field_matcher: Arc<FieldMatcher>,
) -> ArenaInsertRollback {
    let old_len = arena.len();
    let mut current = start;

    for i in 0..=val.len() {
        let byte = if i < val.len() {
            val[i]
        } else {
            ARENA_VALUE_TERMINATOR
        };

        let next = arena[current].table.dstep(byte);
        if next.is_none() {
            let rollback = arena_insert_rollback(arena, old_len, current);

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
                target = arena.alloc_with_table(SmallTable::with_mappings(
                    StateId::NONE,
                    &[b],
                    &[target],
                ));
            }

            // Add transition from current state to the new chain
            arena[current].table.set_transition(byte, target);
            return rollback;
        }
        // Transition exists, follow it
        current = next;
    }

    // Full path already exists — add field transition to the terminal state
    let rollback = arena_insert_rollback(arena, old_len, current);
    arena[current].field_transitions.push(field_matcher);
    rollback
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
#[must_use]
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
        target =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, &[byte], &[target]));
    }

    arena.precompute_epsilon_closures();
    (arena, target) // target is the start state
}

/// Insert a reversed suffix pattern into an existing suffix DFA trie.
///
/// Like `insert_string` but without the ARENA_VALUE_TERMINATOR.
/// Shares prefix structure with existing patterns in the trie.
pub fn insert_suffix(
    arena: &mut StateArena,
    start: StateId,
    reversed_bytes: &[u8],
    field_matcher: Arc<FieldMatcher>,
) -> ArenaInsertRollback {
    let old_len = arena.len();
    let mut current = start;

    for (i, &byte) in reversed_bytes.iter().enumerate() {
        let next = arena[current].table.dstep(byte);
        if next.is_none() {
            let rollback = arena_insert_rollback(arena, old_len, current);

            // No transition — create the remaining chain
            let match_state = arena.alloc();
            arena[match_state].field_transitions.push(field_matcher);

            // Build chain backwards for remaining bytes after this one
            let mut target = match_state;
            for &b in reversed_bytes[i + 1..].iter().rev() {
                target = arena.alloc_with_table(SmallTable::with_mappings(
                    StateId::NONE,
                    &[b],
                    &[target],
                ));
            }

            // Connect current state to the new chain
            arena[current].table.set_transition(byte, target);
            return rollback;
        }
        // Transition exists, follow it
        current = next;
    }

    // Full path already exists — add field transition to the terminal state
    let rollback = arena_insert_rollback(arena, old_len, current);
    arena[current].field_transitions.push(field_matcher);
    rollback
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
#[must_use]
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
        return arena.alloc_with_table(SmallTable::with_mappings(
            match_state, // Default transition for all bytes
            &[],
            &[],
        ));
    }

    // Recursive step: build rest of chain first, then prepend current byte
    let continuation = make_prefix_arena_fa_step(prefix, index + 1, match_state, arena);

    arena.alloc_with_table(SmallTable::with_mappings(
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
#[must_use]
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
    // The junction gives insert_string a clean branch point (dstep
    // returns NONE on the junction for any byte), while spinner states have
    // closure size 1 during self-loop, reducing dstep calls by ~2x.
    let start = arena.alloc();
    let mut state = start;
    let mut iter = pattern.iter();

    while let Some(&ch) = iter.next() {
        if ch == b'*' {
            // Current state becomes an epsilon-only junction before the spinner.
            // This gives insert_string a clean branch point: dstep on
            // the junction returns NONE for any byte, so exact string paths get
            // their own separate states instead of following the spinner's self-loop.
            let spinner = arena.alloc();
            arena[spinner].table = make_byte_dot_table(spinner);
            arena[state].table.epsilons.push(spinner);

            if let Some(&next_byte) = iter.next() {
                // Override spinner's transition for the next byte to an escape
                // state with an epsilon back to the spinner. The escape trigger
                // byte is consumed here.
                let spin_escape = arena.alloc();
                arena[spin_escape].table.epsilons.push(spinner);
                arena[spinner].table.set_transition(next_byte, spin_escape);
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
                let byte_start = usize::from(skip_first_literal_byte);
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
                // This gives insert_string a clean branch point.
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
fn make_byte_dot_table(dest: StateId) -> SmallTable {
    let mut table = SmallTable::new();
    table.ceilings = smallvec![0xC0, 0xC2, ARENA_VALUE_TERMINATOR, BYTE_CEILING_U8];
    table.steps = smallvec![dest, StateId::NONE, dest, StateId::NONE];
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
#[must_use]
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
    let mut iter = pattern.iter();

    while let Some(&ch) = iter.next() {
        if ch == b'*' {
            segments.push(ShellstyleSegment::Wildcard);
            continue;
        }

        // For a non-trailing backslash, consume the next byte and append IT as
        // the literal. A trailing backslash has no next byte and falls through
        // to be appended as a literal `\` itself.
        let literal = if ch == b'\\'
            && let Some(&escaped) = iter.next()
        {
            escaped
        } else {
            ch
        };

        if let Some(ShellstyleSegment::Literal(bytes)) = segments.last_mut() {
            bytes.push(literal);
        } else {
            segments.push(ShellstyleSegment::Literal(vec![literal]));
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
#[must_use]
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
        // `get` rejects empty values and indices past the end in one check,
        // so the matched byte is always a valid position in a non-empty value.
        let Some(&utf8_byte) = val.get(index) else {
            continue;
        };
        if index == val.len() - 1 {
            // The value terminates on this byte.
            vals_ending_here.insert(utf8_byte);
        } else {
            // More bytes follow; defer to a continuation keyed on this byte.
            vals_with_bytes_remaining
                .entry(utf8_byte)
                .or_default()
                .push(val);
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
            let owned_vals: Vec<Vec<u8>> = continuing_vals.iter().copied().cloned().collect();

            // Recurse for continuation
            let continuation = build_anything_but_step(&owned_vals, index + 1, success, arena);

            // Build combined state: fail on VALUE_TERMINATOR, inherit continuation for others
            // We need to create a new state that merges the continuation but overrides VALUE_TERMINATOR
            let fail_state = arena.alloc(); // Empty state = fail

            // Inherit the continuation's byte map, then steer the terminator
            // to fail so any value ending on this byte rejects.
            let mut combined_unpacked: [StateId; BYTE_CEILING] = [success; BYTE_CEILING];
            unpack_arena_table(&arena[continuation].table, &mut combined_unpacked);
            combined_unpacked[ARENA_VALUE_TERMINATOR as usize] = fail_state;

            let combined_state = arena.alloc();
            arena[combined_state].table.pack(&combined_unpacked);
            special_mappings.push((utf8_byte, combined_state));
        } else if has_continuation {
            // Only continues
            let continuing_vals = vals_with_bytes_remaining.get(&utf8_byte).unwrap();
            let owned_vals: Vec<Vec<u8>> = continuing_vals.iter().copied().cloned().collect();
            let next_state = build_anything_but_step(&owned_vals, index + 1, success, arena);
            special_mappings.push((utf8_byte, next_state));
        } else if ends_here {
            // Only ends here - fail on VALUE_TERMINATOR, success on other bytes
            let fail_state = arena.alloc(); // Empty state = fail
            let last_state = arena.alloc_with_table(SmallTable::with_mappings(
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
        return arena.alloc_with_table(SmallTable::with_mappings(success, &[], &[]));
    }

    // Build state with default success and special transitions
    let bytes: Vec<u8> = special_mappings.iter().map(|(b, _)| *b).collect();
    let states: Vec<StateId> = special_mappings.iter().map(|(_, s)| *s).collect();

    arena.alloc_with_table(SmallTable::with_mappings(success, &bytes, &states))
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
#[must_use]
pub fn make_monocase_arena_fa(val: &[u8], next_field: Arc<FieldMatcher>) -> (StateArena, StateId) {
    use crate::case_folding::case_fold_char;

    let mut arena = StateArena::new();

    // Create the "match" state - has field_transitions to mark the match
    let match_state = arena.alloc();
    arena[match_state].field_transitions.push(next_field);

    // Empty string case
    let start = if val.is_empty() {
        arena.alloc_with_table(SmallTable::with_mappings(
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
                    .map_or(val.len(), |c| offset + c.len_utf8());
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
    let term_state = arena.alloc_with_table(SmallTable::with_mappings(
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
            arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[byte, alt],
                &[current_next, current_next],
            ))
        } else {
            // Single path
            arena.alloc_with_table(SmallTable::with_mappings(
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
        return arena.alloc_with_table(SmallTable::with_mappings(
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

            arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[orig[0], alt_bytes[0]],
                &[orig_state, alt_state],
            ))
        } else {
            // Shared leading bytes, then a divergence on the first byte that
            // differs. Two complete UTF-8 encodings of distinct scalars are
            // prefix-free, so once the shared prefix is consumed each side
            // still carries at least one distinguishing byte — neither suffix
            // can be empty.
            let orig_suffix = &orig[common_prefix..];
            let alt_suffix = &alt_bytes[common_prefix..];
            debug_assert!(
                !orig_suffix.is_empty() && !alt_suffix.is_empty(),
                "monocase suffixes are prefix-free after the shared prefix"
            );

            let orig_state = build_arena_fragment(orig_suffix, next_state, arena);
            let alt_state = build_arena_fragment(alt_suffix, next_state, arena);
            let mut current = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[orig_suffix[0], alt_suffix[0]],
                &[orig_state, alt_state],
            ));

            // Prepend the shared prefix as a single-byte chain (common_prefix
            // is non-zero in this branch, so this always emits at least one
            // state).
            for &byte in orig[..common_prefix].iter().rev() {
                current = arena.alloc_with_table(SmallTable::with_mappings(
                    StateId::NONE,
                    &[byte],
                    &[current],
                ));
            }
            current
        }
    } else {
        // No case alternate - single path
        // Build chain from all bytes (including first) to next_state
        let mut current = next_state;
        for &byte in orig.iter().rev() {
            current = arena.alloc_with_table(SmallTable::with_mappings(
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
/// The caller emits the transition on the first byte, so this chains only the
/// remaining bytes back to `end_at`. Callers pass whole UTF-8 char encodings or
/// their suffixes, which are never empty; a single-byte input therefore returns
/// `end_at` unchanged.
fn build_arena_fragment(val: &[u8], end_at: StateId, arena: &mut StateArena) -> StateId {
    let Some((_first, rest)) = val.split_first() else {
        unreachable!("monocase fragment is a non-empty char encoding")
    };

    // Build chain from last byte back to second byte (the first is the caller's).
    let mut current = end_at;
    for &byte in rest.iter().rev() {
        current = arena.alloc_with_table(SmallTable::with_mappings(
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
#[must_use]
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
    let term_state = arena.alloc_with_table(SmallTable::with_mappings(
        StateId::NONE,
        &[ARENA_VALUE_TERMINATOR],
        &[match_state],
    ));
    let close_quote_state = arena.alloc_with_table(SmallTable::with_mappings(
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
            (base, (u16::from(base) + range_size - 1).min(255) as u8)
        };

        // Build FA for this octet range
        let octet_start = build_octet_range_arena_fa(min_val, max_val, current_state, &mut arena);

        // If not first octet, prepend dot
        if octet_idx > 0 {
            current_state = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                b".",
                &[octet_start],
            ));
        } else {
            current_state = octet_start;
        }
    }

    // Prepend opening quote: " → first_octet
    let open_quote_start = arena.alloc_with_table(SmallTable::with_mappings(
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
    let term_state = arena.alloc_with_table(SmallTable::with_mappings(
        StateId::NONE,
        &[ARENA_VALUE_TERMINATOR],
        &[match_state],
    ));
    let close_quote_state = arena.alloc_with_table(SmallTable::with_mappings(
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

        let group_value = u16::from_be_bytes([network[byte_idx], network[byte_idx + 1]]);

        let (min_val, max_val) = if prefix_len as usize >= group_end_bit {
            (group_value, group_value)
        } else if (prefix_len as usize) <= group_start_bit {
            (0u16, 0xffffu16)
        } else {
            let constrained_bits = prefix_len as usize - group_start_bit;
            let mask = !0u16 << (16 - constrained_bits);
            let base = group_value & mask;
            let range_size = 1u32 << (16 - constrained_bits);
            (base, (u32::from(base) + range_size - 1).min(0xffff) as u16)
        };

        // Build FA for this hex group
        let group_start =
            build_ipv6_group_range_arena_fa(min_val, max_val, current_state, &mut arena);

        // If not first group, prepend colon
        if group_idx > 0 {
            current_state = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                b":",
                &[group_start],
            ));
        } else {
            current_state = group_start;
        }
    }

    // Prepend opening quote: " → first_group
    let open_quote_start = arena.alloc_with_table(SmallTable::with_mappings(
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
        let val_str = format!("{min_val:x}");
        return build_literal_chain_arena(val_str.as_bytes(), continuation, arena);
    }

    // Create a start state with epsilon transitions to each value's FA
    let start = arena.alloc();
    let mut value_starts = Vec::new();

    for val in min_val..=max_val {
        let val_str = format!("{val:x}");
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
        current = arena.alloc_with_table(SmallTable::with_mappings(
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

        arena[next_state].table = SmallTable::with_mappings(StateId::NONE, &bytes, &targets);

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

    /// `accumulate_matcher_stats` adds the heap capacity of any per-state
    /// SmallVec that has spilled past its inline slots — transition tables
    /// (`ceilings`/`steps`/`epsilons`) and `field_transitions`. Real test
    /// matchers stay inline, so this drives the spill branches on a hand-built
    /// arena and pins the exact byte arithmetic against an independent estimate.
    #[test]
    fn test_accumulate_matcher_stats_counts_spilled_capacity() {
        let mut arena = StateArena::new();
        let id = arena.alloc();

        // Inline capacities: ceilings/steps = 8, epsilons = 2,
        // field_transitions = 1. Fill each past its inline slots so it spills.
        let fa = &mut arena[id];
        fa.table.ceilings = (0..=15u8).collect();
        fa.table.steps = (0..16u32).map(StateId).collect();
        fa.table.epsilons = (0..4u32).map(StateId).collect();
        fa.field_transitions.push(Arc::new(FieldMatcher::new()));
        fa.field_transitions.push(Arc::new(FieldMatcher::new()));

        let fa = &arena[id];
        assert!(fa.table.ceilings.spilled());
        assert!(fa.table.steps.spilled());
        assert!(fa.table.epsilons.spilled());
        assert!(fa.field_transitions.spilled());

        // Re-derive the total independently: the flat-buffer estimate plus each
        // spilled buffer's heap capacity in bytes. Reading capacity() here
        // (rather than predicting SmallVec growth) keeps the assertion exact.
        let expected = arena.estimated_byte_size()
            + fa.table.ceilings.capacity()
            + fa.table.steps.capacity() * std::mem::size_of::<StateId>()
            + fa.table.epsilons.capacity() * std::mem::size_of::<StateId>()
            + fa.field_transitions.capacity() * std::mem::size_of::<Arc<FieldMatcher>>();

        let mut stats = crate::MatcherStats::default();
        arena.accumulate_matcher_stats(&mut stats);

        assert_eq!(
            stats.bytes, expected,
            "spilled SmallVec capacity must add byte-for-byte to the total"
        );
        assert_eq!(stats.states, 1);
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
        let mut table = SmallTable::new();
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

    /// Forbidden UTF-8 lead bytes — `0xC0`, `0xC1`, and `0xF5..=0xFF` — must
    /// never produce a transition. Upstream Go covers this with
    /// `TestDodgeBadUTF8` (a single byte on a one-mapping table) and
    /// `TestMakeByteDotFA` (the byte-dot wildcard table). We do both at
    /// once: build a small table with a literal mapping for `'a'`, then
    /// confirm `dstep` returns `NONE` for every forbidden byte.
    #[test]
    fn test_dstep_rejects_forbidden_utf8() {
        let s = StateId(0);
        let table = SmallTable::with_mappings(StateId::NONE, b"a", &[s]);

        assert_eq!(table.dstep(b'a'), s, "valid byte should still transition");
        for byte in [0xC0_u8, 0xC1] {
            assert!(
                table.dstep(byte).is_none(),
                "forbidden byte {byte:#04x} must not transition"
            );
        }
        for byte in 0xF5_u8..=0xFF {
            assert!(
                table.dstep(byte).is_none(),
                "forbidden byte {byte:#04x} must not transition"
            );
        }
    }

    /// Exhaustive sweep of `make_byte_dot_table`: every forbidden byte must
    /// return `NONE`, every other byte must self-loop to `dest`. Mirrors
    /// upstream's `TestMakeByteDotFA` but covers the full byte range so a
    /// regression in the ceiling layout shows up immediately.
    #[test]
    fn test_make_byte_dot_table_forbidden_bytes() {
        let dest = StateId(7);
        let table = make_byte_dot_table(dest);

        for byte in 0..=255_u32 {
            // BYTE_CEILING_U8 is 0xF6; 0xF5 is the value terminator, also forbidden.
            #[allow(clippy::cast_possible_truncation)]
            let b = byte as u8;
            let forbidden = matches!(b, 0xC0 | 0xC1) || b >= 0xF5;
            let got = table.dstep(b);
            if forbidden {
                assert!(
                    got.is_none(),
                    "byte {b:#04x} is forbidden but transitioned to {got:?}"
                );
            } else {
                assert_eq!(got, dest, "byte {b:#04x} should self-loop to dest");
            }
        }
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

        let match_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        let start = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            b"a",
            &[match_state],
        ));

        arena.precompute_epsilon_closures();
        let mut bufs = NfaBuffers::with_capacity();

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
        arena[final_state].field_transitions.push(field_matcher);

        let exit_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        // loopback state (placeholder, we'll fill in epsilons after allocating start)
        let loopback = arena.alloc();

        // start state - matches 'a' or 'b' -> loopback
        let start = arena.alloc_with_table({
            let mut table = SmallTable::with_mappings(StateId::NONE, b"ab", &[loopback, loopback]);
            // For *, add epsilon to exit (can match zero times)
            table.epsilons.push(exit_state);
            table
        });

        // Now set up loopback's epsilons: to exit AND back to start (CYCLE!)
        arena[loopback].table.epsilons = smallvec![exit_state, start];

        arena.precompute_epsilon_closures();
        let mut bufs = NfaBuffers::with_capacity();

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
        arena[final_state].field_transitions.push(field_matcher);

        let exit_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        // loopback state (placeholder)
        let loopback = arena.alloc();

        // start state - matches 'a' or 'b' -> loopback
        // NO epsilon to exit (must match at least once for +)
        let start = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            b"ab",
            &[loopback, loopback],
        ));

        // Set up loopback's epsilons: to exit AND back to start (CYCLE!)
        arena[loopback].table.epsilons = smallvec![exit_state, start];

        arena.precompute_epsilon_closures();
        let mut bufs = NfaBuffers::with_capacity();

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

        let exit_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        let loopback = arena.alloc();

        let start = arena.alloc_with_table({
            let mut table = SmallTable::with_mappings(StateId::NONE, b"a", &[loopback]);
            table.epsilons.push(exit_state);
            table
        });

        arena[loopback].table.epsilons = smallvec![exit_state, start];

        // Only 4 states needed for [a]* with arena!
        // The chain-based approach needs 100+ states for the same pattern.
        assert_eq!(arena.len(), 4, "Arena [a]* should only need 4 states");

        // Verify it works
        arena.precompute_epsilon_closures();
        let mut bufs = NfaBuffers::with_capacity();
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
        arena[final_state].field_transitions.push(field_matcher);

        // Exit state: matches VALUE_TERMINATOR → final
        let exit_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));

        // Loopback state (epsilon to exit + start — creates the cycle)
        let loopback = arena.alloc();

        // Start state: transitions on a/b/c → loopback, epsilon to exit (for ?)
        let start = arena.alloc_with_table({
            let mut table = SmallTable::with_mappings(StateId::NONE, b"abc", &[loopback; 3]);
            table.epsilons.push(exit_state);
            table
        });

        // loopback → epsilon to both exit and start (the * / + cycle)
        arena[loopback].table.epsilons = smallvec![exit_state, start];

        arena.precompute_epsilon_closures();
        let mut bufs = NfaBuffers::with_capacity();

        // A long input of 'a's — without dedup this would explode exponentially
        let long_input: Vec<u8> = std::iter::repeat_n(b'a', 200).collect();
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
        arena[final_state].field_transitions.push(field_matcher);

        let exit_state = arena.alloc_with_table(SmallTable::with_mappings(
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

        let mut start_table = SmallTable::new();
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
        let mut bufs = NfaBuffers::with_capacity();

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

    // Acceleration skips input off a single state's exit set, so it is sound only
    // while exactly one state is active; with more, it would advance past bytes the
    // other states still need. The accel fast path must stay gated on a single
    // active state.
    #[test]
    fn test_arena_nfa_skips_acceleration_with_multiple_active_states() {
        let mut arena = StateArena::new();
        let field_matcher = Arc::new(FieldMatcher::new());

        // Literal path "abz": ...->lit_mid -b-> lit_ready -z-> lit_final -TERM-> match.
        let match_state = arena.alloc();
        arena[match_state].field_transitions.push(field_matcher);
        let lit_final = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));
        let lit_ready =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"z", &[lit_final]));
        let lit_mid =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"b", &[lit_ready]));

        // Accelerating self-loop on every byte except its exit byte 'z'.
        let accel_state = arena.alloc();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        for (byte, slot) in unpacked.iter_mut().enumerate() {
            if byte != b'z' as usize && byte != ARENA_VALUE_TERMINATOR as usize {
                *slot = accel_state;
            }
        }
        arena[accel_state].table.pack(&unpacked);
        arena[accel_state].table.accel = Some(AccelInfo {
            exit_bytes: [b'z', 0, 0],
            len: 1,
        });

        // Start steps 'a' into the accel state directly and, via an epsilon
        // sibling, into the literal path — so both are active from byte 1, with
        // the accel state first in the active set.
        let lit_entry =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[lit_mid]));
        let start = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            b"a",
            &[accel_state],
        ));
        arena[start].table.epsilons = smallvec![lit_entry];

        arena.precompute_epsilon_closures();

        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(&arena, start, b"abz", &mut bufs);
        assert_eq!(
            bufs.transitions.len(),
            1,
            "literal 'abz' must match: acceleration must not fire while a second state is active"
        );
    }

    // The dedup pass marks states with a per-step generation so a logical clear is
    // just a counter bump. The generation must advance each step, or states marked
    // in one step read as duplicates in the next and get dropped.
    #[test]
    fn test_arena_nfa_dedup_generation_advances_between_steps() {
        // More hubs than the dedup threshold so each 'a' step triggers a dedup.
        const HUBS: usize = 70;

        let mut arena = StateArena::new();
        let field_matcher = Arc::new(FieldMatcher::new());

        let match_state = arena.alloc();
        arena[match_state].field_transitions.push(field_matcher);

        let hubs: Vec<StateId> = (0..HUBS).map(|_| arena.alloc()).collect();
        for (k, &hub) in hubs.iter().enumerate() {
            if k == 0 {
                // Hub 0 also exits to the match on the value terminator.
                arena[hub].table = SmallTable::with_mappings(
                    StateId::NONE,
                    &[b'a', ARENA_VALUE_TERMINATOR],
                    &[hub, match_state],
                );
            } else {
                arena[hub].table = SmallTable::with_mappings(StateId::NONE, b"a", &[hub]);
            }
        }

        // Start fans out to every hub via epsilon; none of its own bytes step.
        let start = arena.alloc();
        arena[start].table.epsilons = SmallVec::from_vec(hubs);

        arena.precompute_epsilon_closures();

        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(&arena, start, b"aa", &mut bufs);
        assert_eq!(
            bufs.transitions.len(),
            1,
            "match must survive two consecutive dedup steps over a repeating active set"
        );
    }

    #[test]
    fn test_try_accelerate_arena() {
        // Test the try_accelerate_arena function directly
        let mut table = SmallTable::new();
        let nz = |n: usize| NonZero::new(n).unwrap();

        // No accel info - should return None
        assert!(try_accelerate_arena(&table, b"hello").is_none());

        // With 1 exit byte
        table.accel = Some(super::super::AccelInfo {
            exit_bytes: [b'x', 0, 0],
            len: 1,
        });
        assert_eq!(try_accelerate_arena(&table, b"helloxworld"), Some(nz(5))); // finds 'x' at position 5
        assert!(try_accelerate_arena(&table, b"hello").is_none()); // no 'x'
        // An exit byte at offset 0 collapses to None so the caller steps normally
        // instead of advancing by zero and spinning forever.
        assert!(try_accelerate_arena(&table, b"xhello").is_none());
        assert!(try_accelerate_arena(&table, b"").is_none()); // empty slice: no hit

        // With 2 exit bytes
        table.accel = Some(super::super::AccelInfo {
            exit_bytes: [b'x', b'y', 0],
            len: 2,
        });
        assert!(try_accelerate_arena(&table, b"helloworld").is_none()); // neither 'x' nor 'y'
        assert_eq!(try_accelerate_arena(&table, b"hellxyworld"), Some(nz(4))); // finds 'x' at position 4
        assert_eq!(try_accelerate_arena(&table, b"hellyxworld"), Some(nz(4))); // finds 'y' at position 4
        assert_eq!(try_accelerate_arena(&table, b"helloxyw"), Some(nz(5))); // finds 'x' at position 5

        // With 3 exit bytes
        table.accel = Some(super::super::AccelInfo {
            exit_bytes: [b'x', b'y', b'z'],
            len: 3,
        });
        assert_eq!(try_accelerate_arena(&table, b"abcdefghijz"), Some(nz(10))); // finds 'z' at position 10
        assert_eq!(try_accelerate_arena(&table, b"abcxyz"), Some(nz(3))); // finds 'x' at position 3
        assert!(try_accelerate_arena(&table, b"abcdefghij").is_none()); // none of x, y, z
    }

    #[test]
    fn test_traverse_dfa_backward_none_start_collects_nothing() {
        // A NONE start short-circuits even for a non-empty value, rather than
        // walking the value and indexing a state that does not exist.
        let arena = StateArena::new();
        let mut transitions = Vec::new();
        traverse_arena_dfa_backward(&arena, StateId::NONE, b"suffix", &mut transitions);
        assert!(transitions.is_empty());
    }

    /// Verify that `StateArena::dstep` via `dfa_lookup` returns the same result
    /// as `SmallTable::dstep` for every byte value.
    ///
    /// This test is critical because `build_dfa_lookup` is skipped under Miri
    /// (`#[cfg(miri)]` no-op) to avoid a ~165s slowdown from interpreting the
    /// large 256-entry-per-state array on every byte transition. Under Miri,
    /// `StateArena::dstep` falls back to `SmallTable::dstep`. This test
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

        // Record expected results from SmallTable::dstep before flattening
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
        // A state that is its own closure stores nothing, so none count here.
        assert_eq!(stats.states_with_closures, 0);
        assert_eq!(stats.max_closure_len, 0);
        assert_eq!(stats.closure_data_len, 0);
        assert_eq!(stats.dfa_lookup_states, 0);

        // After precomputing closures — s1's closure should include s2 (via epsilon)
        arena.precompute_epsilon_closures();
        let stats = arena.stats();
        assert_eq!(stats.states_with_closures, 1);
        assert_eq!(stats.max_closure_len, 2); // s1 closure includes s1 + s2
        assert_eq!(stats.closure_data_len, 2);

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
        let mut a = Stats {
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
        let b = Stats {
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
        let mut a = Stats {
            max_ceilings: 4,
            max_epsilons: 3,
            max_closure_len: 5,
            ..Default::default()
        };
        let b = Stats {
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
        let expected = arena.states.capacity() * std::mem::size_of::<FaState>()
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
        let dbg = format!("{arena:?}");
        assert!(dbg.contains("states_count"));
        assert!(dbg.contains('2')); // 2 states
    }

    #[test]
    fn test_debug_fmt_state() {
        let state = FaState::new();
        let dbg = format!("{state:?}");
        assert!(dbg.contains("FaState"));
        assert!(dbg.contains("field_transitions_count"));
    }

    #[test]
    fn test_with_capacity() {
        let mut arena = StateArena::with_capacity(10);
        assert!(arena.is_empty());
        assert_eq!(arena.len(), 0);
        assert!(arena.states.capacity() >= 10);
        assert_eq!(arena.closure_data.capacity(), 0);
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
        arena[s0].table.ceilings = smallvec![b'a', BYTE_CEILING_U8];
        arena[s0].table.steps = smallvec![s1, StateId::NONE];

        // s1: 3 ceilings (non-trivial), 1 epsilon
        arena[s1].table.ceilings = smallvec![b'a', b'b', BYTE_CEILING_U8];
        arena[s1].table.steps = smallvec![s0, s2, StateId::NONE];
        arena[s1].table.epsilons.push(s3);

        // s2: 4 ceilings (non-trivial), 2 epsilons
        arena[s2].table.ceilings = smallvec![b'a', b'b', b'c', BYTE_CEILING_U8];
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

    #[test]
    fn test_stats_add_max_takes_larger_both_directions() {
        // Each max_* field keeps the larger of the two sides. Exercise both
        // directions per field: self larger, then other larger.
        let mut self_larger = Stats {
            max_ceilings: 9,
            max_epsilons: 8,
            max_closure_len: 7,
            ..Default::default()
        };
        let smaller = Stats {
            max_ceilings: 2,
            max_epsilons: 1,
            max_closure_len: 3,
            ..Default::default()
        };
        self_larger.add(&smaller);
        assert_eq!(self_larger.max_ceilings, 9);
        assert_eq!(self_larger.max_epsilons, 8);
        assert_eq!(self_larger.max_closure_len, 7);

        let mut self_smaller = Stats {
            max_ceilings: 2,
            max_epsilons: 1,
            max_closure_len: 3,
            ..Default::default()
        };
        let larger = Stats {
            max_ceilings: 9,
            max_epsilons: 8,
            max_closure_len: 7,
            ..Default::default()
        };
        self_smaller.add(&larger);
        assert_eq!(self_smaller.max_ceilings, 9);
        assert_eq!(self_smaller.max_epsilons, 8);
        assert_eq!(self_smaller.max_closure_len, 7);
    }

    #[test]
    fn test_stats_total_ceiling_entries_accumulates() {
        // total_ceiling_entries sums ceiling counts across non-trivial tables;
        // trivial single-ceiling tables are not counted.
        let mut arena = StateArena::new();
        let s0 = arena.alloc();
        let s1 = arena.alloc();
        let s2 = arena.alloc();
        // s0: 2 ceilings, s1: 3 ceilings, s2: trivial (1 ceiling, uncounted)
        arena[s0].table.ceilings = smallvec![b'a', BYTE_CEILING_U8];
        arena[s0].table.steps = smallvec![s1, StateId::NONE];
        arena[s1].table.ceilings = smallvec![b'a', b'b', BYTE_CEILING_U8];
        arena[s1].table.steps = smallvec![s0, s2, StateId::NONE];
        let stats = arena.stats();
        assert_eq!(stats.tables_with_transitions, 2);
        assert_eq!(stats.total_ceiling_entries, 5); // 2 + 3
    }

    #[test]
    fn test_estimated_byte_size_with_flattened_buffers() {
        // estimated_byte_size includes the ft_ptrs and dfa_lookup buffer
        // terms. Populate both (non-empty) so their contribution is actually
        // exercised rather than collapsing to zero.
        let mut arena = StateArena::new();
        arena.alloc();
        arena.ft_ptrs = vec![1usize, 2, 3, 4, 5, 6, 7];
        arena.dfa_lookup = vec![StateId::NONE; 11];
        assert!(arena.ft_ptrs.capacity() > 0);
        assert!(arena.dfa_lookup.capacity() > 0);
        let expected = arena.states.capacity() * std::mem::size_of::<FaState>()
            + arena.closure_data.capacity() * std::mem::size_of::<StateId>()
            + arena.ft_ptrs.capacity() * std::mem::size_of::<usize>()
            + arena.dfa_lookup.capacity() * std::mem::size_of::<StateId>();
        assert_eq!(arena.estimated_byte_size(), expected);
    }

    #[test]
    fn test_nfa_buffers_with_capacity_preallocates() {
        // with_capacity() pre-allocates the hot traversal vectors; new() does not.
        let bufs = NfaBuffers::with_capacity();
        assert!(
            bufs.current_states.capacity() >= 16,
            "current_states must be pre-allocated, got cap={}",
            bufs.current_states.capacity()
        );
        assert!(
            bufs.next_states.capacity() >= 16,
            "next_states must be pre-allocated, got cap={}",
            bufs.next_states.capacity()
        );

        let empty = NfaBuffers::new();
        assert_eq!(empty.current_states.capacity(), 0);
        assert_eq!(empty.next_states.capacity(), 0);
    }

    #[test]
    fn test_alloc_sets_self_only_closure_sentinel() {
        // A freshly alloc'd state reads as the empty self-only closure both
        // before and after the closure pass, including one whose only epsilon
        // loops back to itself.
        let mut arena = StateArena::new();
        let s0 = arena.alloc();
        let s1 = arena.alloc();
        let s2 = arena.alloc();
        let looped = arena.alloc();
        arena[looped].table.epsilons.push(looped);
        assert!(arena.closure_of(s0).is_empty());
        assert!(arena.closure_of(s1).is_empty());
        assert!(arena.closure_of(s2).is_empty());
        assert!(arena.closure_of(looped).is_empty());
        assert!(arena.closure_data.is_empty());
        assert_eq!(arena.closure_data.capacity(), 0);

        arena.precompute_epsilon_closures();
        assert_eq!(arena.closures_computed, arena.len());
        assert!(arena.closure_of(s0).is_empty());
        assert!(arena.closure_of(s1).is_empty());
        assert!(arena.closure_of(s2).is_empty());
        assert!(arena.closure_of(looped).is_empty());
        assert!(arena.closure_data.is_empty());
        assert_eq!(arena.closure_data.capacity(), 0);
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "epsilon closures must be precomputed before NFA traversal")]
    fn test_nfa_traversal_rejects_uncomputed_self_only_closure() {
        // Matching a state whose closure was never computed is a bug; the
        // traversal guard should catch it even for a state with no epsilons.
        let mut arena = StateArena::new();
        let start = arena.alloc();
        let mut bufs = NfaBuffers::new();
        traverse_arena_nfa(&arena, start, b"", &mut bufs);
    }

    #[test]
    fn test_empty_closure_is_self_only_for_consumers() {
        // A state with no epsilons stores an empty closure. Check that every
        // matching path reads that as "just this state" rather than "no states".
        let mut arena = StateArena::new();
        let fm = Arc::new(FieldMatcher::with_match_id(29));
        let accept = arena.alloc();
        arena[accept].field_transitions.push(fm);
        let vt_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[accept],
        ));
        let start =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[vt_state]));
        arena.precompute_epsilon_closures();
        assert!(arena.states.iter().all(|state| state.closure_len == 0));
        assert!(arena.closure_data.is_empty());
        assert_eq!(arena.closure_data.capacity(), 0);

        let mut bufs = NfaBuffers::new();
        traverse_arena_nfa(&arena, start, b"a", &mut bufs);
        assert_eq!(
            bufs.transitions.len(),
            1,
            "unfrozen NFA traversal must process empty closures as self-only"
        );

        let (dfa, dfa_start) = arena
            .nfa_to_dfa(start, 16)
            .expect("empty-closure NFA should convert to a DFA within budget");
        let mut dfa_transitions = Vec::new();
        traverse_arena_dfa(&dfa, dfa_start, b"a", &mut dfa_transitions);
        assert_eq!(
            dfa_transitions.len(),
            1,
            "NFA-to-DFA must expand empty closures as self-only"
        );

        let mut lazy = LazyDfa::new(arena.clone(), start, 16);
        let mut lazy_transitions = Vec::new();
        traverse_lazy_dfa(&mut lazy, b"a", &mut lazy_transitions);
        assert_eq!(
            lazy_transitions.len(),
            1,
            "lazy DFA must expand empty closures as self-only"
        );

        arena.flatten_tables();
        let mut bufs = NfaBuffers::new();
        traverse_arena_nfa(&arena, start, b"a", &mut bufs);
        assert_eq!(
            bufs.transitions.len(),
            1,
            "frozen NFA traversal must process empty closures as self-only"
        );
    }

    #[test]
    fn test_precompute_epsilon_closures_skips_out_of_range_target() {
        // An epsilon target one past the last valid state must be skipped rather
        // than indexed: closure computation neither panics nor includes it.
        let mut arena = StateArena::new();
        let s0 = arena.alloc();
        let s1 = arena.alloc();
        let dangling = StateId::from_index(arena.len());
        arena[s0].table.epsilons.push(s1);
        arena[s0].table.epsilons.push(dangling);
        arena.precompute_epsilon_closures();
        let c = arena.closure_of(s0);
        assert!(c.contains(&s0));
        assert!(c.contains(&s1));
        assert_eq!(c.len(), 2);
    }

    #[test]
    fn test_precompute_epsilon_closures_into_reuses_scratch() {
        // The build reuses one ClosureScratch across adds on ever-larger arenas.
        // Growing the visited set between calls must not corrupt closures, and
        // stray epsilon targets must be gated on the arena length, not on the
        // reused (and possibly oversized) visited set's capacity.
        let mut scratch = ClosureScratch::new();

        // Small arena first: a two-state closure.
        let mut small = StateArena::new();
        let a0 = small.alloc();
        let a1 = small.alloc();
        small[a0].table.epsilons.push(a1);
        small.precompute_epsilon_closures_into(&mut scratch);
        let c = small.closure_of(a0);
        assert!(c.contains(&a0) && c.contains(&a1) && c.len() == 2);

        // Reuse the scratch on a bigger arena, forcing the visited set to grow.
        // Chain each state's epsilon to the next so state 0 closes over all 40,
        // and add a dangling target one past the last state: with the old
        // capacity-based guard the now-oversized set would wrongly admit it.
        let mut big = StateArena::new();
        let states: Vec<StateId> = (0..40).map(|_| big.alloc()).collect();
        for w in states.windows(2) {
            big[w[0]].table.epsilons.push(w[1]);
        }
        let dangling = StateId::from_index(big.len());
        big[states[0]].table.epsilons.push(dangling);
        big.precompute_epsilon_closures_into(&mut scratch);
        let head = big.closure_of(states[0]);
        assert_eq!(head.len(), 40, "state 0 closes over the whole chain");
        assert!(
            !head.contains(&dangling),
            "out-of-range target must be skipped"
        );

        // A third, smaller arena reuses the now-large scratch without growing it
        // and must not inherit stale visited state from the bigger arena.
        let mut third = StateArena::new();
        let t0 = third.alloc();
        let t1 = third.alloc();
        third[t0].table.epsilons.push(t1);
        third.precompute_epsilon_closures_into(&mut scratch);
        let tc = third.closure_of(t0);
        assert!(tc.contains(&t0) && tc.contains(&t1) && tc.len() == 2);
    }

    #[test]
    fn test_unpack_arena_table_writes_full_byte_ceiling_range() {
        // A table whose only mapping is its default unpacks to that default
        // across the entire byte range, including the final index.
        let mut arena = StateArena::new();
        let target = arena.alloc();
        let table = SmallTable::with_mappings(target, &[], &[]);
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpack_arena_table(&table, &mut unpacked);
        assert_eq!(unpacked[0], target);
        assert_eq!(unpacked[BYTE_CEILING - 1], target);
    }

    #[test]
    fn test_traverse_arena_nfa_multi_state_closure_final() {
        // The post-loop final-state check must walk a multi-state closure to
        // collect field transitions reachable only through closure mates, not
        // just the closure head's own (here empty) transitions. Cover both the
        // unfrozen (field_transitions) and frozen (ft_ptrs) branches.
        let mut arena = StateArena::new();
        let fm_a = Arc::new(FieldMatcher::with_match_id(11));
        let fm_b = Arc::new(FieldMatcher::with_match_id(13));
        let match_state_a = arena.alloc();
        arena[match_state_a].field_transitions.push(fm_a);
        let match_state_b = arena.alloc();
        arena[match_state_b].field_transitions.push(fm_b);
        // final_state carries no transitions of its own — only epsilon links.
        let final_state = arena.alloc();
        arena[final_state].table.epsilons.push(match_state_a);
        arena[final_state].table.epsilons.push(match_state_b);
        let vt_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[final_state],
        ));
        let start =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[vt_state]));
        arena.precompute_epsilon_closures();
        assert!(
            arena.closure_of(final_state).len() >= 2,
            "test setup expects final_state to have a multi-state closure"
        );

        // Unfrozen path: ft_ptrs not yet populated, so field_transitions are read.
        assert!(arena.ft_ptrs.is_empty());
        let mut bufs = NfaBuffers::new();
        traverse_arena_nfa(&arena, start, b"a", &mut bufs);
        assert!(
            !bufs.transitions.is_empty(),
            "unfrozen path must collect transitions from final_state's closure"
        );

        // Frozen path: flatten_tables populates ft_ptrs.
        arena.flatten_tables();
        assert!(!arena.ft_ptrs.is_empty());
        let mut bufs = NfaBuffers::new();
        traverse_arena_nfa(&arena, start, b"a", &mut bufs);
        assert!(
            !bufs.transitions.is_empty(),
            "frozen path must collect transitions from final_state's closure"
        );
    }

    #[test]
    fn test_traverse_arena_nfa_multi_state_closure_mid_value() {
        // A multi-state closure encountered mid-value must step every closure
        // mate, not just the closure head, so an onward transition reachable
        // only through a mate is not lost. Cover both step branches.
        let mut arena = StateArena::new();
        let fm = Arc::new(FieldMatcher::with_match_id(7));
        let accept = arena.alloc();
        arena[accept].field_transitions.push(fm);
        // m1 steps on 'b' to accept; m2 is a dead closure mate.
        let m1 = arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"b", &[accept]));
        let m2 = arena.alloc();
        // fork epsilon-forks to m1 and m2, giving fork a multi-state closure.
        let fork = arena.alloc();
        arena[fork].table.epsilons.push(m1);
        arena[fork].table.epsilons.push(m2);
        let start = arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[fork]));
        arena.precompute_epsilon_closures();
        assert!(arena.closure_of(fork).len() >= 2);

        // Unfrozen path.
        assert!(arena.ft_ptrs.is_empty());
        let mut bufs = NfaBuffers::new();
        traverse_arena_nfa(&arena, start, b"ab", &mut bufs);
        assert!(
            !bufs.transitions.is_empty(),
            "unfrozen path must follow the onward transition through a closure mate"
        );

        // Frozen path.
        arena.flatten_tables();
        let mut bufs = NfaBuffers::new();
        traverse_arena_nfa(&arena, start, b"ab", &mut bufs);
        assert!(
            !bufs.transitions.is_empty(),
            "frozen path must follow the onward transition through a closure mate"
        );
    }

    #[test]
    fn test_lazy_dfa_conflicting_nfa_accel_not_accelerated() {
        // A lazy-DFA state that aggregates NFA states with conflicting exit
        // bytes must not be accelerated: try_compute_accel bails when the
        // member AccelInfos disagree.
        let mut arena = StateArena::new();
        let s0 = arena.alloc();
        let s1 = arena.alloc();
        let s2 = arena.alloc();
        // s0 epsilon-forks to s1 and s2 so the start DFA state spans both.
        arena[s0].table.epsilons.push(s1);
        arena[s0].table.epsilons.push(s2);
        // s1 and s2 self-loop on 'a' but advertise different exit bytes.
        arena[s1].table.set_transition(b'a', s1);
        arena[s1].table.accel = Some(AccelInfo {
            exit_bytes: [b'x', 0, 0],
            len: 1,
        });
        arena[s2].table.set_transition(b'a', s2);
        arena[s2].table.accel = Some(AccelInfo {
            exit_bytes: [b'y', 0, 0],
            len: 1,
        });
        arena.precompute_epsilon_closures();

        let mut lazy = LazyDfa::new(arena, s0, 100);
        let mut transitions = Vec::new();
        traverse_lazy_dfa(&mut lazy, b"aaaa", &mut transitions);

        let accel_count = lazy.states.iter().filter(|s| s.accel.is_some()).count();
        assert_eq!(
            accel_count, 0,
            "state with conflicting NFA accel must not be accelerated"
        );
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
        let term = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[end],
        ));

        // Start state transitions on byte to terminator
        let start =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, &[byte], &[term]));

        (arena, start)
    }

    #[test]
    fn test_merge_dual_spinout_states_filters_none_epsilons() {
        // A NONE epsilon on either spinout side must not survive into the merged table.
        let mut arena1 = StateArena::new();
        let mut arena2 = StateArena::new();
        let s1_id = arena1.alloc();
        let s2_id = arena2.alloc();
        arena1[s1_id].table.epsilons.push(StateId::NONE);
        arena2[s2_id].table.epsilons.push(StateId::NONE);

        let s1 = arena1[s1_id].clone();
        let s2 = arena2[s2_id].clone();

        let mut new_arena = StateArena::new();
        let new_id = new_arena.alloc();
        let mut memo: FxHashMap<(StateId, StateId), StateId> = FxHashMap::default();
        merge_dual_spinout_states(
            &arena1,
            &s1,
            &arena2,
            &s2,
            &mut new_arena,
            &mut memo,
            new_id,
        );

        assert!(
            new_arena[new_id]
                .table
                .epsilons
                .iter()
                .all(|e| !e.is_none()),
            "merged spinout table retained a NONE epsilon"
        );
    }

    #[test]
    fn test_asymmetric_spinner_merge_filters_none_epsilons() {
        // A NONE epsilon on the spinner side must not survive into the merged table.
        let mut arena1 = StateArena::new();
        let mut arena2 = StateArena::new();
        let s1_id = arena1.alloc();
        let s2_id = arena2.alloc();
        arena1[s1_id].table.epsilons.push(StateId::NONE);

        let mut new_arena = StateArena::new();
        let new_id = new_arena.alloc();
        let mut memo: FxHashMap<(StateId, StateId), StateId> = FxHashMap::default();
        asymmetric_spinner_merge(
            &arena1,
            s1_id,
            &arena2,
            s2_id,
            true,
            &mut new_arena,
            &mut memo,
            new_id,
        );

        assert!(
            new_arena[new_id]
                .table
                .epsilons
                .iter()
                .all(|e| !e.is_none()),
            "merged spinner table retained a NONE epsilon"
        );
    }

    #[test]
    fn test_merge_nfa_tables_bytewise_filters_none_epsilons() {
        // NONE epsilons on either input table must not survive the NFA bytewise merge.
        let arena1 = StateArena::new();
        let arena2 = StateArena::new();
        let mut table1 = SmallTable::new();
        let mut table2 = SmallTable::new();
        table1.epsilons.push(StateId::NONE);
        table2.epsilons.push(StateId::NONE);
        let mut new_arena = StateArena::new();
        let mut memo: FxHashMap<(StateId, StateId), StateId> = FxHashMap::default();
        let result = merge_nfa_tables_bytewise(
            &arena1,
            &table1,
            &arena2,
            &table2,
            &mut new_arena,
            &mut memo,
        );
        assert!(
            result.epsilons.iter().all(|e| !e.is_none()),
            "merged NFA table retained a NONE epsilon"
        );
    }

    #[test]
    fn test_merge_arena_tables_filters_none_epsilons() {
        // NONE epsilons on either input table must not survive the DFA table merge.
        let arena1 = StateArena::new();
        let arena2 = StateArena::new();
        let mut table1 = SmallTable::new();
        let mut table2 = SmallTable::new();
        table1.epsilons.push(StateId::NONE);
        table2.epsilons.push(StateId::NONE);
        let mut new_arena = StateArena::new();
        let mut memo: FxHashMap<(StateId, StateId), StateId> = FxHashMap::default();
        let result = merge_arena_tables(
            &arena1,
            &table1,
            &arena2,
            &table2,
            &mut new_arena,
            &mut memo,
        );
        assert!(
            result.epsilons.iter().all(|e| !e.is_none()),
            "merged DFA table retained a NONE epsilon"
        );
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
        let (arena1, start1) = make_single_byte_arena(b'a', fm);

        // Merge with empty arena
        let (merged, start) = merge_arena_dfas(&arena1, start1, &StateArena::new(), StateId::NONE);

        // Should work like arena1
        let mut bufs = NfaBuffers::with_capacity();
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

        let mut bufs = NfaBuffers::with_capacity();

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
        let (arena1, start1) = make_single_byte_arena(b'a', fm1);
        let (arena2, start2) = make_single_byte_arena(b'a', fm2);

        let (merged, start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let mut bufs = NfaBuffers::with_capacity();
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

        let (arena1, start1) = make_single_byte_arena(b'x', fm1);
        let (arena2, start2) = make_single_byte_arena(b'y', fm2);

        let (merged, start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let mut bufs = NfaBuffers::with_capacity();

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

        let (arena_a, start_a) = make_single_byte_arena(b'a', fm_a);
        let (arena_b, start_b) = make_single_byte_arena(b'b', fm_b);
        let (arena_c, start_c) = make_single_byte_arena(b'c', fm_c);

        // (A merge B) merge C
        let (ab, ab_start) = merge_arena_dfas(&arena_a, start_a, &arena_b, start_b);
        let (abc_left, abc_left_start) = merge_arena_dfas(&ab, ab_start, &arena_c, start_c);

        // A merge (B merge C)
        let (bc, bc_start) = merge_arena_dfas(&arena_b, start_b, &arena_c, start_c);
        let (abc_right, abc_right_start) = merge_arena_dfas(&arena_a, start_a, &bc, bc_start);

        // Both should match 'a', 'b', 'c'
        let mut bufs1 = NfaBuffers::with_capacity();
        let mut bufs2 = NfaBuffers::with_capacity();

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
            arena[end].field_transitions.push(fm1);

            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));

            let state_b =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"b", &[term]));

            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[state_b]));

            (arena, start)
        };

        // Build "ac" arena
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2);

            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));

            let state_c =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"c", &[term]));

            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[state_c]));

            (arena, start)
        };

        let (merged, start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let mut bufs = NfaBuffers::with_capacity();

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
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, q_num, &mut bufs);
        !bufs.transitions.is_empty()
    }

    // q50/q100/q150-style locals name the literal Q-number value under test.
    #[allow(clippy::similar_names)]
    #[test]
    fn test_numeric_less_arena_fa_basic() {
        let next_field = Arc::new(FieldMatcher::new());
        let (arena, start) = make_numeric_less_arena_fa(100.0, true, next_field);

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
        let (arena, start) = make_numeric_less_arena_fa(100.0, false, next_field);

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

    #[allow(clippy::similar_names)]
    #[test]
    fn test_numeric_greater_arena_fa_basic() {
        let next_field = Arc::new(FieldMatcher::new());
        let (arena, start) = make_numeric_greater_arena_fa(100.0, true, next_field);

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
        let (arena, start) = make_numeric_greater_arena_fa(100.0, false, next_field);

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

    #[allow(clippy::similar_names)]
    #[test]
    fn test_numeric_range_arena_fa_two_sided() {
        let next_field = Arc::new(FieldMatcher::new());
        // Range: 50 <= x <= 150
        let (arena, start) = make_numeric_range_arena_fa(50.0, true, 150.0, true, next_field);

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

    #[allow(clippy::similar_names)]
    #[test]
    fn test_numeric_range_arena_fa_exclusive_bounds() {
        let next_field = Arc::new(FieldMatcher::new());
        // Range: 50 < x < 150 (exclusive both sides)
        let (arena, start) = make_numeric_range_arena_fa(50.0, false, 150.0, false, next_field);

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
    fn test_numeric_range_arena_fa_single_point() {
        let next_field = Arc::new(FieldMatcher::new());
        let q5 = q_num_from_f64(5.0);

        // [5, 5] accepts exactly the shared bound.
        let (incl, incl_start) =
            make_numeric_range_arena_fa(5.0, true, 5.0, true, next_field.clone());
        assert!(
            matches_arena(&incl, incl_start, &q5),
            "5 should match [5, 5]"
        );

        // [5, 5) is empty: an exclusive bound rejects the equal value even when
        // both bounds coincide.
        let (half_open, half_open_start) =
            make_numeric_range_arena_fa(5.0, true, 5.0, false, next_field);
        assert!(
            !matches_arena(&half_open, half_open_start, &q5),
            "5 should NOT match [5, 5)"
        );
    }

    #[allow(clippy::similar_names)]
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
        let (arena2, start2) = make_numeric_greater_arena_fa(-100.0, true, next_field);
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
        let (arena, start) = make_numeric_range_arena_fa(1.5, true, 2.5, true, next_field);

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
        let (arena1, start1) = make_numeric_less_arena_fa(50.0, false, fm1);

        // FA2: x > 100
        let (arena2, start2) = make_numeric_greater_arena_fa(100.0, false, fm2);

        // Merge: should match x < 50 OR x > 100
        let (merged, merged_start) = merge_arena_dfas(&arena1, start1, &arena2, start2);

        let q25 = q_num_from_f64(25.0);
        let q75 = q_num_from_f64(75.0);
        let q150 = q_num_from_f64(150.0);

        let mut bufs = NfaBuffers::with_capacity();

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
                        "{val} should match < {bound} (Q-number ordering)"
                    );
                    assert!(
                        !matches_greater,
                        "{val} should NOT match > {bound} (Q-number ordering)"
                    );
                } else if val > bound {
                    assert!(
                        !matches_less,
                        "{val} should NOT match < {bound} (Q-number ordering)"
                    );
                    assert!(
                        matches_greater,
                        "{val} should match > {bound} (Q-number ordering)"
                    );
                } else {
                    // val == bound, exclusive should not match
                    assert!(
                        !matches_less,
                        "{val} should NOT match < {bound} (exclusive)"
                    );
                    assert!(
                        !matches_greater,
                        "{val} should NOT match > {bound} (exclusive)"
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
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    /// Helper to get field matcher match IDs from traversal
    fn get_match_ids(arena: &StateArena, start: StateId, value: &[u8]) -> Vec<u64> {
        let mut bufs = NfaBuffers::with_capacity();
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
        let term_state = arena.alloc_with_table(SmallTable::with_mappings(
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
                    let state = arena.alloc_with_table(SmallTable::with_mappings(
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
        let term_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));

        // Build suffix chain (backwards)
        let mut after_spinout = term_state;
        for &byte in suffix.iter().rev() {
            let state = arena.alloc_with_table(SmallTable::with_mappings(
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
                let state = arena.alloc_with_table(SmallTable::with_mappings(
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

    /// Read the byte transition `state` takes on `byte` in `arena`.
    fn step_byte(arena: &StateArena, state: StateId, byte: u8) -> StateId {
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpack_arena_table(&arena[state].table, &mut unpacked);
        unpacked[byte as usize]
    }

    fn sorted_match_ids(arena: &StateArena, start: StateId, value: &[u8]) -> Vec<u64> {
        let mut ids = get_match_ids(arena, start, value);
        ids.sort_unstable();
        ids
    }

    fn assert_append_matches_rebuild(
        target: &StateArena,
        target_start: StateId,
        source: &StateArena,
        source_start: StateId,
        values: &[&[u8]],
    ) {
        let (rebuilt, rebuilt_start) = merge_arena_nfas(target, target_start, source, source_start);
        let mut appended = target.clone();
        let appended_start =
            append_merge_arena_nfas(&mut appended, target_start, source, source_start);
        appended.precompute_epsilon_closures();

        for value in values {
            assert_eq!(
                sorted_match_ids(&appended, appended_start, value),
                sorted_match_ids(&rebuilt, rebuilt_start, value),
                "append merge must match rebuild merge for {:?}",
                String::from_utf8_lossy(value)
            );
        }
    }

    fn assert_no_none_epsilons(arena: &StateArena, state: StateId) {
        assert!(
            arena[state].table.epsilons.iter().all(|eps| !eps.is_none()),
            "state {state:?} must not retain NONE epsilon targets"
        );
    }

    #[test]
    fn test_append_merge_arena_nfas_matches_rebuild_for_literals() {
        let (arena1, start1) =
            make_string_arena_fa(b"ab", Arc::new(FieldMatcher::with_match_id(1)));
        let (arena2, start2) =
            make_string_arena_fa(b"ac", Arc::new(FieldMatcher::with_match_id(2)));

        assert_append_matches_rebuild(
            &arena1,
            start1,
            &arena2,
            start2,
            &[b"ab", b"ac", b"a", b"ad"],
        );
    }

    #[test]
    fn test_append_merge_arena_nfas_matches_rebuild_for_empty_cases() {
        let empty = StateArena::new();
        let (arena, start) =
            make_string_arena_fa(b"solo", Arc::new(FieldMatcher::with_match_id(1)));

        assert_append_matches_rebuild(
            &empty,
            StateId::NONE,
            &empty,
            StateId::NONE,
            &[b"", b"solo"],
        );
        assert_append_matches_rebuild(
            &empty,
            StateId::NONE,
            &arena,
            start,
            &[b"", b"solo", b"other"],
        );
        assert_append_matches_rebuild(
            &arena,
            start,
            &empty,
            StateId::NONE,
            &[b"", b"solo", b"other"],
        );
    }

    #[test]
    fn test_append_merge_arena_nfas_matches_rebuild_for_epsilons_and_spinouts() {
        let (alternation, alternation_start) = make_epsilon_alternation_arena(
            &[b"ab", b"ac"],
            Arc::new(FieldMatcher::with_match_id(1)),
        );
        let (literal, literal_start) =
            make_string_arena_fa(b"ad", Arc::new(FieldMatcher::with_match_id(2)));
        assert_append_matches_rebuild(
            &alternation,
            alternation_start,
            &literal,
            literal_start,
            &[b"ab", b"ac", b"ad", b"ae", b"b"],
        );

        let (spinout, spinout_start) =
            make_spinout_arena(b"a", b"c", Arc::new(FieldMatcher::with_match_id(3)));
        let (overlap, overlap_start) =
            make_string_arena_fa(b"ac", Arc::new(FieldMatcher::with_match_id(4)));
        assert_append_matches_rebuild(
            &spinout,
            spinout_start,
            &overlap,
            overlap_start,
            &[b"ac", b"abc", b"aXYZc", b"c", b"ad"],
        );
    }

    #[test]
    fn test_append_merge_arena_nfas_matches_rebuild_for_cycles() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (cycle_arena, cycle_start) = {
            let mut arena = StateArena::new();
            let match_state = arena.alloc();
            arena[match_state].field_transitions.push(fm1);
            let term_state = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));
            let loopback = arena.alloc();
            let start = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                b"ab",
                &[loopback, loopback],
            ));
            arena[loopback].table.epsilons = smallvec![term_state, start];
            arena.precompute_epsilon_closures();
            (arena, start)
        };
        let (literal, literal_start) =
            make_string_arena_fa(b"c", Arc::new(FieldMatcher::with_match_id(2)));

        assert_append_matches_rebuild(
            &cycle_arena,
            cycle_start,
            &literal,
            literal_start,
            &[b"a", b"b", b"ab", b"abba", b"c", b"d", b""],
        );
    }

    #[test]
    fn test_append_merge_epsilon_operand_uses_splice() {
        let (alternation, alternation_start) = {
            let mut arena = StateArena::new();
            let m = arena.alloc();
            arena[m]
                .field_transitions
                .push(Arc::new(FieldMatcher::with_match_id(1)));
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[m],
            ));
            let b_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"b", &[term]));
            let c_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"c", &[term]));
            let alt = arena.alloc();
            arena[alt].table.epsilons = smallvec![b_state, c_state];
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[alt]));
            arena.precompute_epsilon_closures();
            (arena, start)
        };

        let (literal, literal_start) =
            make_string_arena_fa(b"ad", Arc::new(FieldMatcher::with_match_id(2)));
        let mut appended = alternation.clone();
        let appended_start =
            append_merge_arena_nfas(&mut appended, alternation_start, &literal, literal_start);
        let after_a = step_byte(&appended, appended_start, b'a');
        assert!(
            is_epsilon_only_state(&appended, after_a),
            "epsilon/plain merge must append a splice state"
        );

        let (spinout, spinout_start) =
            make_spinout_arena(b"a", b"d", Arc::new(FieldMatcher::with_match_id(3)));
        let mut appended = alternation;
        let appended_start =
            append_merge_arena_nfas(&mut appended, alternation_start, &spinout, spinout_start);
        let after_a = step_byte(&appended, appended_start, b'a');
        assert!(
            is_epsilon_only_state(&appended, after_a),
            "epsilon/spinout merge must splice instead of using asymmetric spinout merge"
        );
    }

    #[test]
    fn test_append_merge_dual_spinout_filters_none_epsilons() {
        fn spinout_with_none_epsilon() -> (StateArena, StateId) {
            let mut arena = StateArena::new();
            let start = arena.alloc();
            arena[start].table = make_byte_dot_table(start);
            arena[start].table.epsilons.push(StateId::NONE);
            arena.precompute_epsilon_closures();
            (arena, start)
        }

        let (mut target, target_start) = spinout_with_none_epsilon();
        let (source, source_start) = spinout_with_none_epsilon();
        let appended_start =
            append_merge_arena_nfas(&mut target, target_start, &source, source_start);

        assert_no_none_epsilons(&target, appended_start);
    }

    #[test]
    fn test_append_merge_nfa_tables_bytewise_filters_none_epsilons() {
        let mut target = StateArena::new();
        let source = StateArena::new();
        let target_table = SmallTable {
            ceilings: smallvec![BYTE_CEILING_U8],
            steps: smallvec![StateId::NONE],
            epsilons: smallvec![StateId::NONE],
            accel: None,
        };
        let source_table = target_table.clone();
        let mut memo = FxHashMap::default();

        let merged = append_merge_nfa_tables_bytewise(
            &mut target,
            &target_table,
            &source,
            &source_table,
            &mut memo,
        );

        assert!(
            merged.epsilons.iter().all(|eps| !eps.is_none()),
            "bytewise append merge must filter NONE epsilon targets"
        );
    }

    #[test]
    fn test_append_merge_source_spinout_keeps_self_loop_and_backedges() {
        let (target, target_start) =
            make_string_arena_fa(b"ad", Arc::new(FieldMatcher::with_match_id(1)));
        let (source, source_start) =
            make_spinout_arena(b"a", b"c", Arc::new(FieldMatcher::with_match_id(2)));
        let mut appended = target;
        let appended_start =
            append_merge_arena_nfas(&mut appended, target_start, &source, source_start);
        appended.precompute_epsilon_closures();

        let spinner = step_byte(&appended, appended_start, b'a');
        assert_eq!(
            step_byte(&appended, spinner, b'z'),
            spinner,
            "source spinout self-loop must map to the combined spinner"
        );
        let d_branch = step_byte(&appended, spinner, b'd');
        assert!(
            !is_spinout_state(&appended, d_branch),
            "target branch reached through a source spinout self-loop should stay a plain branch"
        );
        assert!(
            appended[d_branch].table.epsilons.contains(&spinner),
            "target branch entered from a source spinout self-loop needs a backedge"
        );
        assert!(
            step_byte(&appended, d_branch, b'z').is_none(),
            "target branch should use the spinner backedge instead of becoming a direct wildcard branch"
        );
        assert!(get_match_ids(&appended, appended_start, b"adXc").contains(&2));

        let (target, target_start) =
            make_string_arena_fa(b"ac", Arc::new(FieldMatcher::with_match_id(1)));
        let mut appended = target;
        let appended_start =
            append_merge_arena_nfas(&mut appended, target_start, &source, source_start);
        appended.precompute_epsilon_closures();
        let spinner = step_byte(&appended, appended_start, b'a');
        let c_branch = step_byte(&appended, spinner, b'c');
        assert!(
            appended[c_branch].table.epsilons.contains(&spinner),
            "merged source spinout suffix branch needs a backedge"
        );
        assert!(get_match_ids(&appended, appended_start, b"acXc").contains(&2));
    }

    #[test]
    fn test_append_merge_target_spinout_keeps_branch_backedges() {
        let (target, target_start) =
            make_spinout_arena(b"a", b"c", Arc::new(FieldMatcher::with_match_id(1)));
        let (source, source_start) =
            make_string_arena_fa(b"ad", Arc::new(FieldMatcher::with_match_id(2)));
        let mut appended = target.clone();
        let appended_start =
            append_merge_arena_nfas(&mut appended, target_start, &source, source_start);
        appended.precompute_epsilon_closures();

        let spinner = step_byte(&appended, appended_start, b'a');
        let d_branch = step_byte(&appended, spinner, b'd');
        assert!(
            !is_spinout_state(&appended, d_branch),
            "source branch reached through a target spinout self-loop should stay a plain branch"
        );
        assert!(
            appended[d_branch].table.epsilons.contains(&spinner),
            "source branch entered from a target spinout self-loop needs a backedge"
        );
        assert!(
            step_byte(&appended, d_branch, b'z').is_none(),
            "source branch should use the spinner backedge instead of becoming a direct wildcard branch"
        );
        assert!(get_match_ids(&appended, appended_start, b"adXc").contains(&1));

        let (source, source_start) =
            make_string_arena_fa(b"ac", Arc::new(FieldMatcher::with_match_id(2)));
        let mut appended = target;
        let appended_start =
            append_merge_arena_nfas(&mut appended, target_start, &source, source_start);
        appended.precompute_epsilon_closures();
        let spinner = step_byte(&appended, appended_start, b'a');
        let c_branch = step_byte(&appended, spinner, b'c');
        assert!(
            appended[c_branch].table.epsilons.contains(&spinner),
            "merged target spinout suffix branch needs a backedge"
        );
        assert!(get_match_ids(&appended, appended_start, b"acXc").contains(&1));
    }

    #[test]
    fn test_append_merge_arena_nfas_leaves_existing_target_states_unchanged() {
        let (mut target, target_start) =
            make_string_arena_fa(b"left", Arc::new(FieldMatcher::with_match_id(1)));
        let (source, source_start) =
            make_string_arena_fa(b"right", Arc::new(FieldMatcher::with_match_id(2)));
        let old_len = target.len();
        let before = target.clone();

        let appended_start =
            append_merge_arena_nfas(&mut target, target_start, &source, source_start);

        assert_ne!(
            appended_start, target_start,
            "overlapping non-empty append should return a new live start"
        );
        assert!(
            target.len() > old_len,
            "append merge should add states instead of rebuilding the target"
        );
        for i in 0..old_len {
            let state = &target[StateId::from_index(i)];
            let before_state = &before[StateId::from_index(i)];
            assert_eq!(state.table.ceilings, before_state.table.ceilings);
            assert_eq!(state.table.steps, before_state.table.steps);
            assert_eq!(state.table.epsilons, before_state.table.epsilons);
            assert!(
                state
                    .field_transitions
                    .iter()
                    .map(Arc::as_ptr)
                    .eq(before_state.field_transitions.iter().map(Arc::as_ptr))
            );
        }
    }

    #[test]
    fn test_truncate_states_drops_tail_and_trims_closures() {
        // State 0 has a stored closure followed by state 1's sentinel, while the
        // last two states have stored closures in the tail. Truncating after the
        // sentinel must use its closure_start + 0: preserve state 0's stored
        // prefix while dropping the tail.
        let mut arena = StateArena::new();
        let ids: Vec<_> = (0..5).map(|_| arena.alloc()).collect();
        arena[ids[0]].table.epsilons.push(ids[1]);
        arena[ids[3]].table.epsilons.push(ids[4]);
        arena[ids[4]].table.epsilons.push(ids[3]);
        arena.precompute_epsilon_closures();
        assert_eq!(arena.len(), 5);
        assert_eq!(arena.closure_data.len(), 6);
        assert_eq!(arena.closure_of(ids[0]), &[ids[0], ids[1]]);
        assert!(arena.closure_of(ids[1]).is_empty());

        // Truncating below the current length drops the tail states and trims
        // their closure slots without discarding the stored surviving closure.
        arena.truncate_states(2);
        assert_eq!(arena.len(), 2);
        assert_eq!(arena.closure_data.len(), 2);
        assert_eq!(arena.closure_of(ids[0]), &[ids[0], ids[1]]);
        assert!(arena.closure_of(ids[1]).is_empty());
        assert_eq!(arena[ids[1]].closure_start, 2);

        // Truncating to zero clears the arena and its closure buffer entirely.
        arena.truncate_states(0);
        assert!(arena.is_empty());
        assert!(arena.closure_data.is_empty());
    }

    #[test]
    fn test_precompute_epsilon_closures_is_incremental() {
        // Mirrors Go's TestClosureWalkPrunesClosedSubtree: a builder fully
        // closes every state, so re-running the closure pass over an unchanged
        // arena must recompute nothing.
        let (mut arena, arena_start) =
            make_shellstyle_arena_fa(br#""*foo*bar*""#, Arc::new(FieldMatcher::with_match_id(1)));
        assert!(
            arena.is_nondeterministic(),
            "shellstyle FA should carry epsilons to exercise the closure walk"
        );
        assert_eq!(
            arena.precompute_epsilon_closures(),
            0,
            "re-closing a fully-closed arena must recompute no states"
        );

        // Appending a pattern only adds new states; the closure pass then walks
        // exactly those and leaves the already-closed prefix untouched.
        let closed = arena.len();
        let (source, source_start) =
            make_shellstyle_arena_fa(br#""*baz*""#, Arc::new(FieldMatcher::with_match_id(2)));
        let _ = append_merge_arena_nfas(&mut arena, arena_start, &source, source_start);
        let added = arena.len() - closed;
        assert!(added > 0, "append should introduce new states");
        assert_eq!(
            arena.precompute_epsilon_closures(),
            added,
            "the closure pass should walk only the appended states"
        );
        assert_eq!(
            arena.precompute_epsilon_closures(),
            0,
            "a second pass with no new states is a no-op"
        );
    }

    #[test]
    fn test_merge_alternation_with_literal_builds_splice_not_expansion() {
        // A merge that pairs an epsilon-bearing operand with a plain literal on a
        // shared byte keeps the operand behind one epsilon-only splice state
        // instead of inlining its branches into the combined byte table. That
        // bounds merge growth: inlining per byte is what makes high-pattern-count
        // merges blow up. Here "a(b|c)" (alternation) meets "ad" (literal).
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = {
            let mut arena = StateArena::new();
            let m = arena.alloc();
            arena[m].field_transitions.push(fm1);
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[m],
            ));
            let b_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"b", &[term]));
            let c_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"c", &[term]));
            let alt = arena.alloc();
            arena[alt].table.epsilons = smallvec![b_state, c_state];
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[alt]));
            arena.precompute_epsilon_closures();
            (arena, start)
        };

        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2);
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            let d_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"d", &[term]));
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[d_state]));
            (arena, start)
        };

        let (merged, start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // The post-'a' merge state is the splice: every byte step is NONE, all
        // outgoing edges are epsilons.
        let after_a = step_byte(&merged, start, b'a');
        assert!(!after_a.is_none(), "'a' must reach a merged state");
        assert!(
            is_epsilon_only_state(&merged, after_a),
            "expected an epsilon-only splice, got a state that carries its own \
             byte transitions"
        );

        // The splice still routes both patterns.
        assert_eq!(get_match_ids(&merged, start, b"ab"), vec![1]);
        assert_eq!(get_match_ids(&merged, start, b"ac"), vec![1]);
        assert_eq!(get_match_ids(&merged, start, b"ad"), vec![2]);
    }

    #[test]
    fn test_merge_spinout_with_literal_stays_a_spinner() {
        // A merge that pairs a spinout with a plain literal on a shared byte
        // keeps the combined state one self-looping spinner: the literal's lone
        // branch folds in via an epsilon back-edge rather than cloning the
        // wildcard per byte. Cloning would mint a fresh state for every byte the
        // literal omits, ballooning state count for wildcard-heavy pattern sets.
        // Here "a*" (spinout) meets "ad" (literal).
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = make_spinout_arena(b"a", b"", fm1); // "a*"

        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2);
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            let d_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"d", &[term]));
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[d_state]));
            (arena, start)
        };

        let (merged, start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        // The post-'a' state is the spinner, and a byte the literal omits ('z')
        // loops back to it rather than landing in a clone.
        let spinner = step_byte(&merged, start, b'a');
        assert!(!spinner.is_none(), "'a' must reach the combined spinner");
        assert!(
            is_spinout_state(&merged, spinner),
            "the combined post-'a' state must remain a self-looping spinner"
        );
        assert_eq!(
            step_byte(&merged, spinner, b'z'),
            spinner,
            "a byte the literal does not cover must loop back to the same spinner"
        );

        // The spinner still routes both patterns.
        assert!(
            get_match_ids(&merged, start, b"a").contains(&1),
            "'a' is a*"
        );
        assert!(
            get_match_ids(&merged, start, b"azz").contains(&1),
            "'azz' is a*"
        );
        let ids_ad = get_match_ids(&merged, start, b"ad");
        assert!(
            ids_ad.contains(&1) && ids_ad.contains(&2),
            "'ad' matches both a* and the literal ad, got {ids_ad:?}"
        );
    }

    #[test]
    fn test_merge_arena_with_epsilons() {
        // Arena 1: matches "a" OR "b" via epsilon branching
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = make_epsilon_alternation_arena(&[b"a", b"b"], fm1);

        // Arena 2: matches "c" (simple, no epsilons)
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2);
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"c", &[term]));
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
    fn test_merge_shared_prefix_spinout_and_literal() {
        // Both arenas step on 'a', so the merge pairs a spinout state (arena1's
        // wildcard) with a plain literal state (arena2's 'b') on a shared byte —
        // the asymmetric-spinner path. The literal branch must survive: "ab"
        // matches both the wildcard (id 1) and the literal (id 2).
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = make_spinout_arena(b"a", b"", fm1); // "a*"

        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2);
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            let b_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"b", &[term]));
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[b_state]));
            (arena, start)
        };

        let (merged, start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        assert_eq!(
            get_match_ids(&merged, start, b"a"),
            vec![1],
            "'a' is a* only"
        );
        let ids_ab = get_match_ids(&merged, start, b"ab");
        assert!(
            ids_ab.contains(&1) && ids_ab.contains(&2),
            "'ab' matches both a* and the literal ab, got {ids_ab:?}"
        );
        assert_eq!(
            get_match_ids(&merged, start, b"aXYZ"),
            vec![1],
            "'aXYZ' is a* only"
        );
        assert!(
            !matches_value(&merged, start, b"b"),
            "'b' needs a leading 'a'"
        );
    }

    #[test]
    fn test_merge_shared_prefix_epsilon_alternation_and_literal() {
        // Both arenas step on 'a', so the merge pairs an epsilon-only alternation
        // state (arena1's "a(b|c)") with a plain literal state (arena2's 'd') on a
        // shared byte — the epsilon-splice path. The alternation must survive:
        // "ab" and "ac" still match id 1 alongside the literal "ad" (id 2).
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = {
            let mut arena = StateArena::new();
            let m = arena.alloc();
            arena[m].field_transitions.push(fm1);
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[m],
            ));
            let b_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"b", &[term]));
            let c_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"c", &[term]));
            let alt = arena.alloc();
            arena[alt].table.epsilons = smallvec![b_state, c_state];
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[alt]));
            arena.precompute_epsilon_closures();
            (arena, start)
        };

        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm2);
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            let d_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"d", &[term]));
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"a", &[d_state]));
            (arena, start)
        };

        let (merged, start) = merge_arena_nfas(&arena1, start1, &arena2, start2);

        assert_eq!(
            get_match_ids(&merged, start, b"ab"),
            vec![1],
            "'ab' is alternation only"
        );
        assert_eq!(
            get_match_ids(&merged, start, b"ac"),
            vec![1],
            "'ac' is alternation only"
        );
        assert_eq!(
            get_match_ids(&merged, start, b"ad"),
            vec![2],
            "'ad' is literal only"
        );
        assert!(
            !matches_value(&merged, start, b"ae"),
            "'ae' matches neither"
        );
        // Without the shared leading 'a' nothing is reachable: the alternation
        // branches ('b'/'c'), the literal byte ('d'), and an unrelated byte must
        // all miss, confirming the merge keeps both patterns anchored on 'a'.
        assert!(
            !matches_value(&merged, start, b"b"),
            "'b' has no leading 'a' — alternation must not fire"
        );
        assert!(
            !matches_value(&merged, start, b"d"),
            "'d' has no leading 'a' — literal must not fire"
        );
        assert!(
            !matches_value(&merged, start, b"f"),
            "'f' matches neither pattern"
        );
    }

    #[test]
    fn test_merge_suffixed_spinout_asymmetric_branches() {
        // "a*c" has a spinout whose suffix byte 'c' is a real (non-self) exit. Two
        // shared-prefix merges drive the asymmetric per-byte resolver:
        //   * against "ad" the spinner's 'c' exit lands where the literal has no
        //     transition, so the wildcard's own 'c' exit must be preserved;
        //   * against "ac" the spinner's 'c' exit coincides with the literal, so
        //     the merged branch must keep an epsilon back to the spinner to allow
        //     further looping ("acYc").
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let make_chain = |bytes: &[u8], fm: Arc<FieldMatcher>| {
            let mut arena = StateArena::new();
            let end = arena.alloc();
            arena[end].field_transitions.push(fm);
            let mut next = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            for &b in bytes.iter().rev() {
                next =
                    arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, &[b], &[next]));
            }
            (arena, next)
        };

        let (wild_arena, wild_start) = make_spinout_arena(b"a", b"c", fm1); // "a*c"

        // "a*c" against literal "ad": the spinner's 'c' exit lands where the
        // literal has no transition, so the wildcard's own 'c' exit must survive.
        // Run both merge orders to drive each asymmetric-dispatch clause.
        let (disjoint_arena, disjoint_start) =
            make_chain(b"ad", Arc::new(FieldMatcher::with_match_id(2)));
        for (a1, s1, a2, s2) in [
            (&wild_arena, wild_start, &disjoint_arena, disjoint_start),
            (&disjoint_arena, disjoint_start, &wild_arena, wild_start),
        ] {
            let (m, st) = merge_arena_nfas(a1, s1, a2, s2);
            assert_eq!(
                get_match_ids(&m, st, b"aXc"),
                vec![1],
                "a*c keeps its 'c' exit"
            );
            assert_eq!(
                get_match_ids(&m, st, b"ad"),
                vec![2],
                "literal 'ad' still matches"
            );
            // Both patterns are anchored on a leading 'a': a value ending in 'c'
            // but lacking the prefix must not reach the spinner, and 'd' alone
            // must not reach the literal.
            assert!(
                !matches_value(&m, st, b"Xc"),
                "a*c needs a leading 'a' — 'Xc' must not match"
            );
            assert!(
                !matches_value(&m, st, b"d"),
                "literal 'ad' needs a leading 'a' — 'd' must not match"
            );
        }

        // "a*c" against literal "ac": the spinner's 'c' exit coincides with the
        // literal, so the merged branch must keep an epsilon back to the spinner
        // to allow further looping ("acYc"). Both merge orders.
        let (shared_arena, shared_start) =
            make_chain(b"ac", Arc::new(FieldMatcher::with_match_id(2)));
        for (a1, s1, a2, s2) in [
            (&wild_arena, wild_start, &shared_arena, shared_start),
            (&shared_arena, shared_start, &wild_arena, wild_start),
        ] {
            let (m, st) = merge_arena_nfas(a1, s1, a2, s2);
            let ids = get_match_ids(&m, st, b"ac");
            assert!(
                ids.contains(&1) && ids.contains(&2),
                "'ac' matches a*c and literal ac, got {ids:?}"
            );
            assert!(
                get_match_ids(&m, st, b"acYc").contains(&1),
                "a*c still loops after its 'c' branch: 'acYc' matches"
            );
            // Neither "a*c" nor literal "ac" is reachable without the leading 'a'.
            assert!(
                !matches_value(&m, st, b"Xc"),
                "a*c needs a leading 'a' — 'Xc' must not match"
            );
            assert!(
                !matches_value(&m, st, b"c"),
                "literal 'ac' needs a leading 'a' — 'c' must not match"
            );
        }
    }

    #[test]
    fn test_merge_arena_with_spinout() {
        // Arena 1: matches "a*b" (wildcard pattern)
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let (arena1, start1) = make_spinout_arena(b"a", b"b", fm1);

        // Arena 2: matches "x*y" (another wildcard pattern)
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));
        let (arena2, start2) = make_spinout_arena(b"x", b"y", fm2);

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
        let (arena1, start1) = make_spinout_arena(b"foo", b"", fm1);

        // Arena 2: matches "*bar" (wildcard with suffix)
        let (arena2, start2) = make_spinout_arena(b"", b"bar", fm2);

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
            arena[match_state].field_transitions.push(fm1);

            // Terminal state
            let term_state = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));

            // Loopback state (placeholder, epsilons set after start is created)
            let loopback = arena.alloc();

            // Start state: matches 'a' or 'b' -> loopback
            let start = arena.alloc_with_table(SmallTable::with_mappings(
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
            arena[end].field_transitions.push(fm2);
            let term = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[end],
            ));
            let start =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"c", &[term]));
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
            arena[match_state].field_transitions.push(fm1);

            // Terminal state
            let term_state = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));

            // Second spinout (after X)
            let spinout2 = arena.alloc();
            arena[spinout2].table = make_byte_dot_table(spinout2);
            arena[spinout2].table.epsilons.push(term_state);

            // State that matches 'X' -> spinout2
            let x_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"X", &[spinout2]));

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
            arena[match_state].field_transitions.push(fm2);

            let term_state = arena.alloc_with_table(SmallTable::with_mappings(
                StateId::NONE,
                &[ARENA_VALUE_TERMINATOR],
                &[match_state],
            ));

            let spinout2 = arena.alloc();
            arena[spinout2].table = make_byte_dot_table(spinout2);
            arena[spinout2].table.epsilons.push(term_state);

            let y_state =
                arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"Y", &[spinout2]));

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
        let (arena1, start1) = make_epsilon_alternation_arena(&[b"a"], fm);

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
        let (a4, s4) = make_epsilon_alternation_arena(&[b"d"], fm);

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
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_string_arena_fa_basic() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_string_arena_fa(b"hello", fm);

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
        let (arena, start) = make_string_arena_fa(b"", fm);

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
        let (arena, start) = make_string_arena_fa(b"x", fm);

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
        let (arena, start) = make_string_arena_fa("café".as_bytes(), fm);

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

        let (arena1, start1) = make_string_arena_fa(b"foo", fm1);
        let (arena2, start2) = make_string_arena_fa(b"bar", fm2);

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

        let (arena1, start1) = make_string_arena_fa(b"prefix_one", fm1);
        let (arena2, start2) = make_string_arena_fa(b"prefix_two", fm2);

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
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_prefix_arena_fa_basic() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_prefix_arena_fa(b"hello", fm);

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
        let (arena, start) = make_prefix_arena_fa(b"", fm);

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
        let (arena, start) = make_prefix_arena_fa(b"a", fm);

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
        let (arena, start) = make_prefix_arena_fa(b"caf", fm);

        // Should match strings with UTF-8 prefix
        assert!(
            matches_value(&arena, start, "café".as_bytes()),
            "Should match 'café'"
        );
        assert!(
            matches_value(&arena, start, b"cafeteria"),
            "Should match 'cafeteria'"
        );

        // Should NOT match non-prefix
        assert!(
            !matches_value(&arena, start, b"ca"),
            "Should NOT match 'ca'"
        );
    }

    #[test]
    fn test_prefix_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_prefix_arena_fa(b"foo", fm1);
        let (arena2, start2) = make_prefix_arena_fa(b"bar", fm2);

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
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_shellstyle_arena_fa_prefix_wildcard() {
        // Pattern: "foo*" - matches "foo" followed by anything
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_shellstyle_arena_fa(b"foo*", fm);

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
        let (arena, start) = make_shellstyle_arena_fa(b"*bar", fm);

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
        let (arena, start) = make_shellstyle_arena_fa(b"foo*bar", fm);

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
        let (arena, start) = make_shellstyle_arena_fa(b"hello", fm);

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
        let (arena, start) = make_shellstyle_arena_fa(b"*", fm);

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
        let (arena, start) = make_shellstyle_arena_fa(b"*foo*", fm);

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
        let (arena, start) = make_shellstyle_arena_fa(b"*foo*bar*", fm);

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
        let (arena, start) = make_shellstyle_arena_fa(b"*a*b*c*d*e*", fm);

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
        let (arena, start) = make_shellstyle_arena_fa(b"*a*b*c*d*e*f*g*h*", fm);

        assert!(matches_value(&arena, start, b"abcdefgh"));
        assert!(matches_value(&arena, start, b"xaxbxcxdxexfxgxhx"));
        assert!(!matches_value(&arena, start, b"abcdefg"));
        assert!(!matches_value(&arena, start, b"hgfedcba"));
    }

    #[test]
    fn test_shellstyle_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_shellstyle_arena_fa(b"foo*", fm1);
        let (arena2, start2) = make_shellstyle_arena_fa(b"*bar", fm2);

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
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_wildcard_arena_fa_basic() {
        // Same as shellstyle for basic patterns
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_wildcard_arena_fa(b"foo*bar", fm);

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
        let (arena, start) = make_wildcard_arena_fa(b"foo\\*bar", fm);

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
        let (arena, start) = make_wildcard_arena_fa(b"foo\\\\bar", fm);

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
        let (arena, start) = make_wildcard_arena_fa(b"foo\\\\*bar", fm);

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
        let (arena, start) = make_wildcard_arena_fa(b"foo\\**", fm);

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
        let (arena, start) = make_wildcard_arena_fa(b"hello", fm);

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
    fn test_wildcard_arena_fa_trailing_backslash() {
        // A trailing backslash with nothing to escape is matched as a literal backslash.
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_wildcard_arena_fa(b"foo\\", fm);

        assert!(
            matches_value(&arena, start, b"foo\\"),
            "Should match 'foo\\'"
        );
        assert!(
            !matches_value(&arena, start, b"foo"),
            "Should NOT match 'foo'"
        );
    }

    #[test]
    fn test_wildcard_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_wildcard_arena_fa(b"foo\\*", fm1);
        let (arena2, start2) = make_wildcard_arena_fa(b"bar*", fm2);

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
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_anything_but_arena_fa_single_value() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let excluded = vec![b"foo".to_vec()];
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm);

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
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm);

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
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm);

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
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm);

        // Should match everything
        assert!(
            matches_value(&arena, start, b"anything"),
            "Should match 'anything'"
        );
        assert!(matches_value(&arena, start, b""), "Should match empty");
    }

    #[test]
    fn test_anything_but_arena_fa_empty_value() {
        // An empty excluded value is ignored, so every value still matches.
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let excluded = vec![b"".to_vec()];
        let (arena, start) = make_anything_but_arena_fa(&excluded, fm);

        assert!(matches_value(&arena, start, b"foo"), "Should match 'foo'");
        assert!(matches_value(&arena, start, b""), "Should match empty");
    }

    #[test]
    fn test_anything_but_arena_fa_merge() {
        let fm1 = Arc::new(FieldMatcher::with_match_id(1));
        let fm2 = Arc::new(FieldMatcher::with_match_id(2));

        let (arena1, start1) = make_anything_but_arena_fa(&[b"foo".to_vec()], fm1);
        let (arena2, start2) = make_string_arena_fa(b"bar", fm2);

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
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    #[test]
    fn test_monocase_arena_fa_single_char() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"A", fm);

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
        let (arena, start) = make_monocase_arena_fa(b"Ab", fm);

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
        let (arena, start) = make_monocase_arena_fa(b"cat", fm);

        // Should match all case variants
        assert!(matches_value(&arena, start, b"cat"), "Should match 'cat'");
        assert!(matches_value(&arena, start, b"CAT"), "Should match 'CAT'");
        assert!(matches_value(&arena, start, b"Cat"), "Should match 'Cat'");
    }

    #[test]
    fn test_monocase_arena_fa_basic_ascii() {
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"Hello", fm);

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
        let (arena, start) = make_monocase_arena_fa(b"", fm);

        // Empty should match empty
        assert!(matches_value(&arena, start, b""), "Should match empty");

        // Should NOT match non-empty
        assert!(!matches_value(&arena, start, b"a"), "Should NOT match 'a'");
    }

    #[test]
    fn test_monocase_arena_fa_no_case_chars() {
        // Pattern with no case-sensitive chars
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa(b"123", fm);

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
        let (arena, start) = make_monocase_arena_fa(b"Abc123", fm);

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

        let (arena1, start1) = make_monocase_arena_fa(b"Foo", fm1);
        let (arena2, start2) = make_monocase_arena_fa(b"Bar", fm2);

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
        let (arena, start) = make_monocase_arena_fa(pattern, fm);

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

    #[test]
    fn test_monocase_arena_fa_shared_lead_byte() {
        // 'À' (U+00C0, C3 80) folds to 'à' (U+00E0, C3 A0): the two encodings
        // share their leading byte, so the builder emits a shared-prefix chain
        // then diverges. A trailing ASCII letter forces another case-folded
        // transition after the shared prefix to keep the path going.
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let (arena, start) = make_monocase_arena_fa("Àb".as_bytes(), fm);

        assert!(
            matches_value(&arena, start, "Àb".as_bytes()),
            "Should match original 'Àb'"
        );
        assert!(
            matches_value(&arena, start, "àB".as_bytes()),
            "Should match folded 'àB'"
        );
        assert!(
            matches_value(&arena, start, "àb".as_bytes()),
            "Should match folded 'àb'"
        );

        // Sharing the leading byte must not collapse the divergence: a value
        // that keeps the shared prefix but takes neither real branch must fail.
        assert!(
            !matches_value(&arena, start, "Áb".as_bytes()),
            "Should NOT match 'Áb' (U+00C1 shares the C3 lead byte only)"
        );
        assert!(
            !matches_value(&arena, start, "Àc".as_bytes()),
            "Should NOT match 'Àc'"
        );
    }
}

#[cfg(test)]
mod cidr_arena_tests {
    use super::*;
    use crate::json::CidrPattern;

    fn matches_value(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = NfaBuffers::with_capacity();
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
        let (arena, start) = make_cidr_arena_fa(&cidr, fm);

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
        let (arena, start) = make_cidr_arena_fa(&cidr, fm);

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
        let (arena, start) = make_cidr_arena_fa(&cidr, fm);

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

        let (arena1, start1) = make_cidr_arena_fa(&cidr1, fm1);
        let (arena2, start2) = make_cidr_arena_fa(&cidr2, fm2);

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
        let (arena, start) = make_cidr_arena_fa(&cidr, fm);

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

    #[test]
    fn test_cidr_arena_fa_ipv6_partial_group_masks_host_bits() {
        // /60 constrains group 3 to its top 12 bits, masking off the low
        // nibble of the network value: 0xef08 collapses to the [0xef00, 0xef0f]
        // block. A host inside the block matches; the first host above it does
        // not.
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let cidr = CidrPattern::V6 {
            network: [
                0x20, 0x01, 0x0d, 0xb8, 0xab, 0xcd, 0xef, 0x08, 0, 0, 0, 0, 0, 0, 0, 0,
            ],
            prefix_len: 60,
        };
        let (arena, start) = make_cidr_arena_fa(&cidr, fm);

        assert!(
            matches_value(&arena, start, b"\"2001:db8:abcd:ef00:0:0:0:0\""),
            "0xef00 is the masked network base and must match"
        );
        assert!(
            !matches_value(&arena, start, b"\"2001:db8:abcd:ef10:0:0:0:0\""),
            "0xef10 is the next /60 block and must not match"
        );
    }

    #[test]
    fn test_build_any_hex_group_requires_at_least_one_digit() {
        // The start position consumes a hex digit before reaching the
        // continuation; only the inner positions hold an epsilon shortcut to
        // it, so a zero-length group cannot match.
        let mut arena = StateArena::new();
        let fm = Arc::new(FieldMatcher::with_match_id(1));
        let match_state = arena.alloc();
        arena[match_state].field_transitions.push(fm);
        let start = build_any_hex_group_arena(match_state, &mut arena);
        arena.precompute_epsilon_closures();

        assert!(
            matches_value(&arena, start, b"1"),
            "one hex digit must match"
        );
        assert!(
            !matches_value(&arena, start, b""),
            "empty group must not match"
        );
    }
}

#[cfg(kani)]
mod kani_arena_proofs {
    use super::*;

    /// Prove: dstep returns the correct state for any byte on a real
    /// SmallTable with symbolic ceilings, steps, and lookup byte.
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

        let mut table = SmallTable::new();
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
}

/// Shared test helpers for NFA/DFA conversion tests.
#[cfg(test)]
mod dfa_test_helpers {
    use super::*;
    use crate::regexp::{make_regexp_nfa_arena, parse_regexp};

    pub(super) fn build_regexp_nfa(pattern: &str) -> (StateArena, StateId) {
        let root = parse_regexp(pattern).expect("valid regexp");
        let (mut arena, start, _fm) = make_regexp_nfa_arena(root);
        arena.precompute_epsilon_closures();
        (arena, start)
    }

    pub(super) fn nfa_matches(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut bufs = NfaBuffers::with_capacity();
        traverse_arena_nfa(arena, start, value, &mut bufs);
        !bufs.transitions.is_empty()
    }

    pub(super) fn dfa_matches(arena: &StateArena, start: StateId, value: &[u8]) -> bool {
        let mut transitions = Vec::new();
        traverse_arena_dfa(arena, start, value, &mut transitions);
        !transitions.is_empty()
    }

    pub(super) fn lazy_dfa_matches(lazy_dfa: &mut LazyDfa, value: &[u8]) -> bool {
        let mut transitions = Vec::new();
        traverse_lazy_dfa(lazy_dfa, value, &mut transitions);
        !transitions.is_empty()
    }
}

#[cfg(test)]
mod nfa_to_dfa_tests {
    use super::dfa_test_helpers::*;
    use super::*;

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

/// Miri-only tests for traverse_arena_dfa with flattened tables.
///
/// freeze_value_matcher skips nfa_to_dfa under miri (FxHashMap/Mutex are
/// ~200× slower under interpretation), so end-to-end tests won't exercise
/// traverse_arena_dfa via the real pipeline. These tests reproduce the exact
/// code path: convert NFA → DFA, call flatten_tables(), then traverse.
#[cfg(all(test, miri))]
mod miri_dfa_traversal_tests {
    use super::dfa_test_helpers::*;
    use super::*;

    /// traverse_arena_dfa with flat tables: match and non-match.
    #[test]
    fn test_traverse_dfa_flat_match_and_no_match() {
        let (nfa, nfa_start) = build_regexp_nfa("[ab]+");
        let (mut dfa, dfa_start) = nfa.nfa_to_dfa(nfa_start, 1000).expect("should convert");
        dfa.flatten_tables(); // mirrors freeze_value_matcher

        assert!(dfa_matches(&dfa, dfa_start, b"\"a\""));
        assert!(dfa_matches(&dfa, dfa_start, b"\"ab\""));
        assert!(!dfa_matches(&dfa, dfa_start, b"\"c\""));
        assert!(!dfa_matches(&dfa, dfa_start, b"\"\""));
    }

    /// traverse_arena_dfa with flat tables: alternation pattern.
    #[test]
    fn test_traverse_dfa_flat_alternation() {
        let (nfa, nfa_start) = build_regexp_nfa("cat|dog");
        let (mut dfa, dfa_start) = nfa.nfa_to_dfa(nfa_start, 1000).expect("should convert");
        dfa.flatten_tables();

        assert!(dfa_matches(&dfa, dfa_start, b"\"cat\""));
        assert!(dfa_matches(&dfa, dfa_start, b"\"dog\""));
        assert!(!dfa_matches(&dfa, dfa_start, b"\"car\""));
    }
}

#[cfg(test)]
mod lazy_dfa_tests {
    use super::dfa_test_helpers::*;
    use super::*;

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

    /// The estimate has to cover every buffer the cache owns, at the capacity
    /// each currently holds, both fresh and once matching has grown it.
    #[test]
    fn test_lazy_dfa_byte_size_covers_every_owned_buffer() {
        let owned_bytes = |lazy: &LazyDfa| {
            let mut bytes = lazy.states.capacity() * size_of::<LazyDfaState>()
                + lazy.nfa_sets.capacity() * size_of::<Vec<StateId>>()
                + lazy.state_map.capacity() * (size_of::<Vec<u32>>() + size_of::<StateId>());
            for state in &lazy.states {
                bytes += state.transitions.capacity() * size_of::<StateId>();
                bytes += state.field_transition_ptrs.capacity() * size_of::<usize>();
            }
            for nfa_set in &lazy.nfa_sets {
                bytes += nfa_set.capacity() * size_of::<StateId>();
            }
            for key in lazy.state_map.keys() {
                bytes += key.capacity() * size_of::<u32>();
            }
            bytes
        };

        let (nfa, nfa_start) = build_regexp_nfa("[abc]+");
        let mut lazy = LazyDfa::new(nfa, nfa_start, 100);
        let fresh = lazy.cached_state_byte_size();
        assert_eq!(fresh, owned_bytes(&lazy));
        assert!(
            fresh >= lazy.states.len() * BYTE_CEILING * size_of::<StateId>(),
            "a transition table per state is the bulk of it: {fresh}",
        );

        assert!(lazy_dfa_matches(&mut lazy, b"\"abcabc\""));
        let grown = lazy.cached_state_byte_size();
        assert_eq!(grown, owned_bytes(&lazy));
        assert!(
            grown > fresh,
            "states interned while matching must add to the estimate: {grown} vs {fresh}",
        );
    }

    /// A state costs its record plus the full transition table that is the
    /// point of caching it, so anything short of that price buys none.
    #[test]
    fn test_lazy_dfa_states_within_bytes_prices_a_state_at_its_table() {
        let one_state = size_of::<LazyDfaState>() + BYTE_CEILING * size_of::<StateId>();

        assert_eq!(LazyDfa::states_within_bytes(one_state), 1);
        assert_eq!(LazyDfa::states_within_bytes(one_state - 1), 0);
        assert_eq!(LazyDfa::states_within_bytes(one_state * 9), 9);
    }

    /// The cache adds to the running tally rather than replacing it: its NFA
    /// copy counts as an arena, and the interned states count beyond that.
    #[test]
    fn test_lazy_dfa_matcher_stats_add_to_the_running_tally() {
        let (nfa, nfa_start) = build_regexp_nfa("[abc]+");
        let mut lazy = LazyDfa::new(nfa, nfa_start, 100);
        assert!(lazy_dfa_matches(&mut lazy, b"\"abcabc\""));

        // A non-zero starting tally stands in for the arenas walked earlier.
        let running = crate::MatcherStats {
            states: 7,
            bytes: 11,
            ..crate::MatcherStats::default()
        };
        let mut with_cache = running;
        lazy.accumulate_matcher_stats(&mut with_cache);

        let mut nfa_only = running;
        lazy.nfa_arena.accumulate_matcher_stats(&mut nfa_only);

        assert_eq!(with_cache.states, nfa_only.states + lazy.states.len());
        assert_eq!(
            with_cache.bytes,
            nfa_only.bytes + lazy.cached_state_byte_size()
        );
    }

    #[test]
    #[cfg_attr(miri, ignore)]
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

#[cfg(test)]
mod dfa_accel_tests {
    use super::dfa_test_helpers::build_regexp_nfa;
    use super::*;

    // MIRI SKIP RATIONALE: a[^x]+x builds a large Unicode NFA; traversal with a 1000-byte
    // value takes too long under interpretation. Coverage: test_accel_traversal_miri_friendly
    // exercises the same acceleration code path with a hand-crafted 2-state DFA.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_lazy_dfa_accel_negated_char_class() {
        let (nfa, nfa_start) = build_regexp_nfa("a[^x]+x");

        // NFA→DFA exceeds budget for this Unicode-heavy NFA
        assert!(
            nfa.nfa_to_dfa(nfa_start, 1000).is_none(),
            "expected budget exceeded for [^x]+ Unicode NFA"
        );

        // Lazy DFA should still work and acquire AccelInfo during traversal
        let mut lazy = LazyDfa::new(nfa, nfa_start, 10_000);

        // Build a long value: "aaa...ax" (1000 'a's + 'x')
        let long_val: Vec<u8> = std::iter::once(b'"')
            .chain(std::iter::once(b'a'))
            .chain(std::iter::repeat_n(b'a', 1000))
            .chain(std::iter::once(b'x'))
            .chain(std::iter::once(b'"'))
            .collect();

        let mut transitions = Vec::new();
        traverse_lazy_dfa(&mut lazy, &long_val, &mut transitions);
        assert!(
            !transitions.is_empty(),
            "should match: a followed by 1000 a's then x"
        );

        // After traversal, at least one state should have AccelInfo
        let accel_count = lazy.states.iter().filter(|s| s.accel.is_some()).count();
        assert!(
            accel_count > 0,
            "lazy DFA should detect AccelInfo on self-loop state"
        );
    }

    // MIRI SKIP RATIONALE: same as test_lazy_dfa_accel_negated_char_class — Unicode NFA.
    // Coverage: test_accel_traversal_miri_friendly covers the skip==0 fallback path.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_lazy_dfa_accel_skip_zero() {
        let (nfa, nfa_start) = build_regexp_nfa("a[^x]+x");

        let mut lazy = LazyDfa::new(nfa, nfa_start, 10_000);

        // "aax" — after literal 'a', loop sees 'a' (self-loop, triggers accel),
        // then 'x' at skip==0 → falls through to normal processing
        let mut transitions = Vec::new();
        traverse_lazy_dfa(&mut lazy, b"\"aax\"", &mut transitions);
        assert!(!transitions.is_empty(), "should match 'aax'");

        // "ax" — [^x]+ requires at least one non-x char, so this does NOT match
        transitions.clear();
        traverse_lazy_dfa(&mut lazy, b"\"ax\"", &mut transitions);
        assert!(
            transitions.is_empty(),
            "'ax' should NOT match a[^x]+x — no non-x chars between a and x"
        );
    }

    // MIRI SKIP RATIONALE: same as test_lazy_dfa_accel_negated_char_class — Unicode NFA.
    // Coverage: test_accel_traversal_miri_friendly covers the multi-exit-byte accel path.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_lazy_dfa_accel_multi_exit_bytes() {
        let (nfa, nfa_start) = build_regexp_nfa("a[^xy]+y");

        let mut lazy = LazyDfa::new(nfa, nfa_start, 10_000);

        let long_val: Vec<u8> = std::iter::once(b'"')
            .chain(std::iter::once(b'a'))
            .chain(std::iter::repeat_n(b'a', 500))
            .chain(std::iter::once(b'y'))
            .chain(std::iter::once(b'"'))
            .collect();

        let mut transitions = Vec::new();
        traverse_lazy_dfa(&mut lazy, &long_val, &mut transitions);
        assert!(!transitions.is_empty(), "should match 500 a's then y");

        let accel_states: Vec<_> = lazy
            .states
            .iter()
            .filter_map(|s| s.accel.as_ref())
            .collect();
        assert!(!accel_states.is_empty(), "should have accel on self-loop");
        for accel in &accel_states {
            assert_eq!(accel.len, 2, "expected 2 exit bytes for [^xy]+");
        }
    }

    // Verifies that `compute_dfa_accel` populates `AccelInfo` on the spinout state
    // produced by `[^x]+`. The loop state self-loops on every ASCII byte except b'x'
    // (→ NONE). NONE counts as a valid exit byte, so the state is acceleratable.
    // MIRI SKIP RATIONALE: `[^x]+` produces a 17K-state Unicode NFA; subset construction
    // under interpretation would exceed the 30-minute CI budget. Coverage:
    // test_accel_traversal_miri_friendly verifies accel detection on a hand-crafted DFA.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_eager_dfa_accel_simple_pattern() {
        let (nfa, nfa_start) = build_regexp_nfa("[^x]+");

        let (dfa, _dfa_start) = nfa.nfa_to_dfa(nfa_start, 100_000).expect("should convert");
        let accel_count = dfa
            .states
            .iter()
            .filter(|s| s.table.accel.is_some())
            .count();
        assert!(
            accel_count > 0,
            "loop state of [^x]+ DFA should be accelerated"
        );
        for state in &dfa.states {
            if let Some(ref accel) = state.table.accel {
                // The loop state has exactly two exit bytes:
                // - b'"' (closing quote → accept state)
                // - b'x' (rejection → NONE)
                assert_eq!(accel.len, 2, "expected exactly 2 exit bytes");
                let mut actual = [accel.exit_bytes[0], accel.exit_bytes[1]];
                actual.sort_unstable();
                assert_eq!(actual, [b'"', b'x']);
            }
        }
    }

    // Verifies that `traverse_arena_dfa` correctly matches a long value through
    // a large DFA converted from `[^x]+`.
    // MIRI SKIP RATIONALE: same as test_eager_dfa_accel_simple_pattern — 17K-state Unicode NFA.
    // Coverage: test_accel_traversal_miri_friendly exercises traverse_arena_dfa with accel.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_eager_dfa_traversal_with_accel() {
        let (nfa, nfa_start) = build_regexp_nfa("[^x]+");

        if let Some((mut dfa, dfa_start)) = nfa.nfa_to_dfa(nfa_start, 100_000) {
            dfa.flatten_tables();

            let long_val: Vec<u8> = std::iter::once(b'"')
                .chain(std::iter::repeat_n(b'a', 1000))
                .chain(std::iter::once(b'"'))
                .collect();

            let mut transitions = Vec::new();
            traverse_arena_dfa(&dfa, dfa_start, &long_val, &mut transitions);
            assert!(
                !transitions.is_empty(),
                "DFA should match 1000 a's (no x = full match)"
            );
        }
    }

    /// Miri-friendly accel coverage: builds a hand-crafted 2-state spinout DFA,
    /// sets `AccelInfo` directly, and verifies that `traverse_arena_dfa` uses it
    /// to skip to the exit byte without visiting every intermediate byte.
    ///
    /// The DFA accepts `[^z]+`: loop state self-loops on all ASCII bytes except
    /// `b'z'` (exit) and `VALUE_TERMINATOR` (implicit stop). A field matcher is
    /// attached to the exit state so a successful traversal produces a transition.
    #[test]
    fn test_accel_traversal_miri_friendly() {
        use super::super::small_table::{AccelInfo, FieldMatcher, VALUE_TERMINATOR};
        use std::sync::Arc;

        let mut arena = StateArena::new();
        let loop_state = arena.alloc();
        let exit_state = arena.alloc();

        // loop_state: b'z' → exit_state, VALUE_TERMINATOR → NONE, everything else → loop_state
        let mut unpacked = [loop_state; BYTE_CEILING];
        unpacked[b'z' as usize] = exit_state;
        unpacked[VALUE_TERMINATOR as usize] = StateId::NONE;
        arena[loop_state].table.pack(&unpacked);

        // exit_state: VALUE_TERMINATOR → NONE (accept via field transition)
        let fm = Arc::new(FieldMatcher::with_match_id(42));
        arena[exit_state].field_transitions.push(fm);

        // Attach AccelInfo directly — exit byte is b'z'
        arena[loop_state].table.accel = Some(AccelInfo {
            exit_bytes: [b'z', 0, 0],
            len: 1,
        });

        arena.flatten_tables();

        // "aaaz" — loop skips 'a','a','a', exits on 'z', then field transition fires
        let val = b"\"aaaz\"";
        let mut transitions = Vec::new();
        traverse_arena_dfa(&arena, loop_state, val, &mut transitions);
        assert!(
            !transitions.is_empty(),
            "accel traversal should find field transition on exit byte"
        );

        // "aaaa" — no exit byte, loop never exits, no field transition
        let val_no_exit = b"\"aaaa\"";
        transitions.clear();
        traverse_arena_dfa(&arena, loop_state, val_no_exit, &mut transitions);
        assert!(
            transitions.is_empty(),
            "no exit byte means no field transition"
        );
    }

    // Documents how `try_compute_accel` acquires AccelInfo from NFA spinout states.
    // The NFA builder sets AccelInfo on spinout states of ASCII-only negated patterns
    // (e.g. `[^x]+`). `try_compute_accel` propagates it to the lazy DFA state.
    // Non-negated patterns (`[abc]+`) and Unicode negated patterns produce no NFA
    // AccelInfo (too many exit bytes), so the lazy DFA traverses byte-by-byte.
    // MIRI SKIP RATIONALE: a[^x]+x builds a large Unicode NFA; the 100-byte traversal
    // is too slow under interpretation. Coverage: test_accel_traversal_miri_friendly
    // exercises the accel propagation path with a hand-crafted DFA.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_lazy_dfa_accel_from_nfa_spinout_states() {
        // a[^x]+x: NFA builder sets AccelInfo on the [^x]+ spinout states
        // (ASCII-only negated class, 1 exit byte: b'x')
        let (nfa, nfa_start) = build_regexp_nfa("a[^x]+x");

        let nfa_accel_count = nfa
            .states
            .iter()
            .filter(|s| s.table.accel.is_some())
            .count();
        assert!(
            nfa_accel_count > 0,
            "NFA builder should set AccelInfo on [^x]+ spinout states; got 0"
        );

        let mut lazy = LazyDfa::new(nfa, nfa_start, 10_000);
        let long_val: Vec<u8> = std::iter::once(b'"')
            .chain(std::iter::once(b'a'))
            .chain(std::iter::repeat_n(b'a', 100))
            .chain(std::iter::once(b'x'))
            .chain(std::iter::once(b'"'))
            .collect();

        let mut transitions = Vec::new();
        traverse_lazy_dfa(&mut lazy, &long_val, &mut transitions);
        assert!(!transitions.is_empty(), "should match correctly");

        // try_compute_accel finds NFA AccelInfo and propagates it → lazy DFA state accelerated
        let accel_count = lazy.states.iter().filter(|s| s.accel.is_some()).count();
        assert!(
            accel_count > 0,
            "lazy DFA should acquire AccelInfo from NFA spinout states"
        );

        // Non-negated [abc]+: NFA builder does NOT set AccelInfo
        // → try_compute_accel finds nothing → no lazy DFA acceleration
        let (nfa2, nfa_start2) = build_regexp_nfa("[abc]+");
        let nfa2_accel = nfa2
            .states
            .iter()
            .filter(|s| s.table.accel.is_some())
            .count();
        assert_eq!(
            nfa2_accel, 0,
            "non-negated pattern NFA states should have no AccelInfo"
        );

        let mut lazy2 = LazyDfa::new(nfa2, nfa_start2, 100);
        let mut t2 = Vec::new();
        traverse_lazy_dfa(&mut lazy2, b"\"abc\"", &mut t2);
        assert!(!t2.is_empty(), "should match");

        let accel_count2 = lazy2.states.iter().filter(|s| s.accel.is_some()).count();
        assert_eq!(
            accel_count2, 0,
            "non-negated [abc]+: no NFA AccelInfo → no lazy DFA accel"
        );
    }
}
