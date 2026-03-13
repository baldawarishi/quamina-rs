//! Thread-safe pattern matching components.
//!
//! This module contains thread-safe (Send + Sync) matchers for concurrent use:
//! - `FrozenFieldMatcher`: Immutable field matcher for concurrent reads
//! - `FrozenValueMatcher`: Immutable value matcher for concurrent reads
//! - `ThreadSafeCoreMatcher`: Thread-safe wrapper with lock-free matching
//! - `AutomatonValueMatcher`: A working value matcher that uses automata
//!
//! # Safety
//! This module contains unsafe Send/Sync implementations for frozen matchers and BuildState.
//! These are verified by Miri threading tests in CI.
#![allow(unsafe_code)]

use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use arc_swap::ArcSwap;
use parking_lot::Mutex;

use super::arena::{
    make_prefix_arena_fa, make_shellstyle_arena_fa, make_string_arena_fa, merge_arena_nfas,
    traverse_arena_dfa, traverse_arena_dfa_backward, traverse_arena_nfa, ArenaNfaBuffers,
    ArenaStats, StateArena, StateId,
};
use super::mutable_matcher::{
    EventField, EventFieldRef, MultiConditionNfa, MutableFieldMatcher, MutableValueMatcher,
};
use super::small_table::{FieldMatcher, NfaBuffers};

/// Compact collection for `transition_on` results, optimized for the common cases.
///
/// Most calls return 0 or 1 elements. This enum avoids both heap allocation (unlike Vec)
/// and the SmallVec discriminant/copy overhead on the empty path. Size: 16 bytes
/// (same as `Option<Arc<T>>` thanks to niche optimization on the `One` variant).
pub(crate) enum Transitions<T> {
    Empty,
    One(T),
    Many(Vec<T>),
}

impl<T> Transitions<T> {
    #[inline(always)]
    fn iter(&self) -> TransitionsIter<'_, T> {
        match self {
            Self::Empty => TransitionsIter::Empty,
            Self::One(val) => TransitionsIter::One(std::iter::once(val)),
            Self::Many(vec) => TransitionsIter::Many(vec.iter()),
        }
    }

    #[inline(always)]
    fn push(&mut self, val: T) {
        *self = match std::mem::replace(self, Self::Empty) {
            Self::Empty => Self::One(val),
            Self::One(first) => Self::Many(vec![first, val]),
            Self::Many(mut vec) => {
                vec.push(val);
                Self::Many(vec)
            }
        };
    }
}

enum TransitionsIter<'a, T> {
    Empty,
    One(std::iter::Once<&'a T>),
    Many(std::slice::Iter<'a, T>),
}

impl<'a, T> Iterator for TransitionsIter<'a, T> {
    type Item = &'a T;

    #[inline(always)]
    fn next(&mut self) -> Option<&'a T> {
        match self {
            TransitionsIter::Empty => None,
            TransitionsIter::One(iter) => iter.next(),
            TransitionsIter::Many(iter) => iter.next(),
        }
    }
}

/// Check if two array trails have no conflicts (using flatten_json::ArrayPos)
fn no_array_trail_conflict_ref(
    from: &[crate::flatten_json::ArrayPos],
    to: &[crate::flatten_json::ArrayPos],
) -> bool {
    for from_pos in from {
        for to_pos in to {
            if from_pos.array == to_pos.array && from_pos.pos != to_pos.pos {
                return false;
            }
        }
    }
    true
}

/// Check if two array trails have no conflicts
fn no_array_trail_conflict(from: &[crate::json::ArrayPos], to: &[crate::json::ArrayPos]) -> bool {
    for from_pos in from {
        for to_pos in to {
            if from_pos.array == to_pos.array && from_pos.pos != to_pos.pos {
                return false;
            }
        }
    }
    true
}

/// Frozen (immutable) field matcher - Send + Sync
///
/// This is the immutable counterpart to MutableFieldMatcher.
/// Once created, it cannot be modified, making it safe for concurrent access.
#[derive(Clone, Default)]
pub struct FrozenFieldMatcher<X: Clone + Eq + Hash> {
    /// Map from field paths to value matchers
    pub transitions: FxHashMap<String, Arc<FrozenValueMatcher<X>>>,
    /// Pattern identifiers that match when arriving at this state
    pub matches: Vec<X>,
    /// exists:true patterns - map from field path to next field matcher
    pub exists_true: FxHashMap<String, Arc<Self>>,
    /// exists:false patterns - map from field path to next field matcher
    pub exists_false: FxHashMap<String, Arc<Self>>,
}

// SAFETY: FrozenFieldMatcher only contains Arc, FxHashMap, and Vec - all Send+Sync when X is.
unsafe impl<X: Clone + Eq + Hash + Send + Sync> Send for FrozenFieldMatcher<X> {}
// SAFETY: Same as Send - all fields (Arc, FxHashMap, Vec) are Send+Sync when X is.
unsafe impl<X: Clone + Eq + Hash + Send + Sync> Sync for FrozenFieldMatcher<X> {}

impl<X: Clone + Eq + Hash> FrozenFieldMatcher<X> {
    pub fn new() -> Self {
        Self {
            transitions: FxHashMap::default(),
            matches: Vec::new(),
            exists_true: FxHashMap::default(),
            exists_false: FxHashMap::default(),
        }
    }

    /// Transition on a field value during matching
    pub(crate) fn transition_on(
        &self,
        path: &str,
        value: &[u8],
        is_number: bool,
        bufs: &mut NfaBuffers,
    ) -> Transitions<Arc<Self>> {
        if let Some(vm) = self.transitions.get(path) {
            vm.transition_on(value, is_number, bufs)
        } else {
            Transitions::Empty
        }
    }
}

/// Frozen (immutable) value matcher - Send + Sync
///
/// This is the immutable counterpart to MutableValueMatcher.
#[derive(Clone, Default)]
pub struct FrozenValueMatcher<X: Clone + Eq + Hash> {
    /// Optimization: for single exact match, store it directly
    singleton_match: Option<Vec<u8>>,
    /// Transition for singleton match
    singleton_transition: Option<Arc<FrozenFieldMatcher<X>>>,
    /// Whether this matcher has numeric patterns (for Q-number conversion)
    has_numbers: bool,
    /// Mapping from FieldMatcher pointer (as usize) to FrozenFieldMatcher
    /// Uses FxHashMap for fast integer key lookup
    transition_map: FxHashMap<usize, Arc<FrozenFieldMatcher<X>>>,
    /// Multi-condition NFAs for lookaround patterns with condition verification
    multi_condition_nfas: Vec<MultiConditionNfa>,
    /// Unified arena-based FA for all pattern types
    main_arena: Option<(StateArena, StateId)>,
    /// Whether main_arena contains NFA states (epsilon transitions or spinout states).
    /// When false, the fast traverse_arena_dfa path is used.
    main_arena_is_nfa: bool,
    /// Separate DFA trie for suffix patterns, traversed backward (right-to-left).
    suffix_arena: Option<(StateArena, StateId)>,
}

// SAFETY: FrozenValueMatcher only contains Arc, FxHashMap, Option, and primitives - all Send+Sync.
unsafe impl<X: Clone + Eq + Hash + Send + Sync> Send for FrozenValueMatcher<X> {}
// SAFETY: Same as Send - all fields (Arc, FxHashMap, Option, primitives) are Send+Sync when X is.
unsafe impl<X: Clone + Eq + Hash + Send + Sync> Sync for FrozenValueMatcher<X> {}

impl<X: Clone + Eq + Hash> FrozenValueMatcher<X> {
    pub fn new() -> Self {
        Self {
            singleton_match: None,
            singleton_transition: None,
            has_numbers: false,
            transition_map: FxHashMap::default(),
            multi_condition_nfas: Vec::new(),
            main_arena: None,
            main_arena_is_nfa: false,
            suffix_arena: None,
        }
    }

    /// Transition on a value during matching
    #[inline]
    pub(crate) fn transition_on(
        &self,
        value: &[u8],
        is_number: bool,
        bufs: &mut NfaBuffers,
    ) -> Transitions<Arc<FrozenFieldMatcher<X>>> {
        // Singleton fast path: when no multi-condition NFAs coexist with singleton,
        // we can short-circuit without touching transition_map.
        if self.multi_condition_nfas.is_empty() {
            if let Some(ref singleton_val) = self.singleton_match {
                if singleton_val == value {
                    if let Some(ref trans) = self.singleton_transition {
                        return Transitions::One(trans.clone());
                    }
                }
                return Transitions::Empty;
            }
        }

        let mut result = Transitions::Empty;

        // Check singleton match (when multi-condition NFAs coexist with singleton,
        // we couldn't use the fast path above)
        let has_singleton = if let Some(ref singleton_val) = self.singleton_match {
            if singleton_val == value {
                if let Some(ref trans) = self.singleton_transition {
                    result.push(trans.clone());
                }
            }
            true
        } else {
            false
        };

        // Try with Q-number conversion if this matcher has numbers and value is numeric
        // Use stack-allocated QNumberStack to avoid heap allocation
        let q_num_storage: Option<crate::numbits::QNumberStack> = if self.has_numbers && is_number {
            // Try to parse as f64 using fast-float (faster than std parse)
            fast_float2::parse(value)
                .ok()
                .map(crate::numbits::q_num_stack)
        } else {
            None
        };
        let value_to_match: &[u8] = match &q_num_storage {
            Some(q) => q.as_slice(),
            None => value,
        };

        // When singleton is active, main_arena and suffix_arena are empty — skip them.
        if !has_singleton {
            // Traverse main_arena (unified arena for all pattern types)
            if let Some((ref arena, start)) = self.main_arena {
                if self.main_arena_is_nfa {
                    // NFA path: handles epsilon transitions and spinout states
                    bufs.arena_bufs.clear();
                    traverse_arena_nfa(arena, start, value_to_match, &mut bufs.arena_bufs);
                } else {
                    // DFA fast path: tight loop, no buffer management overhead
                    bufs.arena_bufs.transitions.clear();
                    traverse_arena_dfa(
                        arena,
                        start,
                        value_to_match,
                        &mut bufs.arena_bufs.transitions,
                    );
                }

                for &ptr in &bufs.arena_bufs.transitions {
                    if let Some(frozen_fm) = self.transition_map.get(&ptr) {
                        result.push(frozen_fm.clone());
                    }
                }
            }

            // Traverse suffix_arena backward (right-to-left DFA for suffix patterns)
            if let Some((ref arena, start)) = self.suffix_arena {
                bufs.arena_bufs.transitions.clear();
                traverse_arena_dfa_backward(
                    arena,
                    start,
                    value_to_match,
                    &mut bufs.arena_bufs.transitions,
                );

                for &ptr in &bufs.arena_bufs.transitions {
                    if let Some(frozen_fm) = self.transition_map.get(&ptr) {
                        result.push(frozen_fm.clone());
                    }
                }
            }
        }

        // Traverse multi-condition NFAs (for lookaround patterns)
        // For lookaround patterns, we check conditions directly on the full value.
        // The conditions contain the combined patterns that capture full matching semantics:
        // - PositiveLookahead("foobar"): "foobar" must match full value
        // - NegativeLookahead("foobar"): "foobar" must NOT match full value
        // - Lookbehind conditions are pre-combined with primary during build
        if !self.multi_condition_nfas.is_empty() {
            for mc_nfa in self.multi_condition_nfas.iter() {
                // Verify all conditions against the full value
                let mut all_conditions_pass = true;

                for condition in &mc_nfa.conditions {
                    // Traverse the condition automaton on the full value
                    bufs.arena_bufs.clear();
                    traverse_arena_nfa(
                        &condition.arena,
                        condition.start,
                        value_to_match,
                        &mut bufs.arena_bufs,
                    );

                    let condition_matched = !bufs.arena_bufs.transitions.is_empty();

                    // Check if condition passes:
                    // - Positive condition: must match
                    // - Negative condition: must NOT match
                    let condition_passes = if condition.is_negative {
                        !condition_matched
                    } else {
                        condition_matched
                    };

                    if !condition_passes {
                        all_conditions_pass = false;
                        break; // Fast-fail: one condition failed, no need to check others
                    }
                }

                // Only add transitions if all conditions pass
                if all_conditions_pass {
                    let ptr = mc_nfa.field_matcher_ptr as usize;
                    if let Some(frozen_fm) = self.transition_map.get(&ptr) {
                        // Avoid duplicates
                        if !result.iter().any(|r| Arc::ptr_eq(r, frozen_fm)) {
                            result.push(frozen_fm.clone());
                        }
                    }
                }
            }
        }

        result
    }
}

/// Internal state for building patterns (protected by mutex)
struct BuildState<X: Clone + Eq + Hash> {
    /// The mutable root for building
    root: Rc<MutableFieldMatcher<X>>,
}

// SAFETY: BuildState is only ever accessed through a Mutex lock in ThreadSafeCoreMatcher.
// The Rc<MutableFieldMatcher> is never shared between threads - it's always accessed
// while holding the mutex lock. This makes it safe to implement Send.
unsafe impl<X: Clone + Eq + Hash + Send> Send for BuildState<X> {}

impl<X: Clone + Eq + Hash> BuildState<X> {
    fn new() -> Self {
        Self {
            root: Rc::new(MutableFieldMatcher::new()),
        }
    }
}

/// Thread-safe core matcher using automaton-based matching.
///
/// This matcher is `Send + Sync`, allowing concurrent access from multiple threads.
/// Pattern addition is serialized via a mutex. Matching is lock-free in steady state
/// but lazily freezes the mutable structures on the first match after pattern additions,
/// amortizing the freeze cost to avoid O(n²) overhead from per-add freezing.
///
/// See `tests::test_thread_safe_core_matcher_basic` for usage example.
pub struct ThreadSafeCoreMatcher<X: Clone + Eq + Hash + Send + Sync> {
    /// The frozen root - atomically swappable, lock-free reads
    root: ArcSwap<FrozenFieldMatcher<X>>,
    /// Mutex protecting pattern building
    build_lock: Mutex<BuildState<X>>,
    /// Flag indicating that patterns were added since the last freeze.
    /// When true, the next match operation will freeze before reading.
    /// This avoids O(n²) cost from freezing after every add_pattern.
    needs_freeze: AtomicBool,
    /// Arena byte budget for pattern complexity limiting
    arena_byte_budget: usize,
    /// Maximum field-matcher states during add_pattern (prevents 2^N blowup)
    max_states_per_pattern: usize,
}

// ThreadSafeCoreMatcher is Send + Sync because:
// - ArcSwap<T> is Send + Sync when T is Send + Sync
// - Mutex<T> is Send + Sync when T is Send
// - FrozenFieldMatcher is Send + Sync when X is
// - BuildState contains Rc which is !Send, but it's behind a Mutex which makes
//   the ThreadSafeCoreMatcher itself Send + Sync

impl<X: Clone + Eq + Hash + Send + Sync> ThreadSafeCoreMatcher<X> {
    /// Create a new ThreadSafeCoreMatcher with default limits.
    pub fn new() -> Self {
        let defaults = crate::PatternLimits::default();
        Self::with_limits(defaults.arena_byte_budget, defaults.max_states_per_pattern)
    }

    /// Create a new ThreadSafeCoreMatcher with custom limits.
    pub fn with_limits(arena_byte_budget: usize, max_states_per_pattern: usize) -> Self {
        Self {
            root: ArcSwap::from_pointee(FrozenFieldMatcher::new()),
            build_lock: Mutex::new(BuildState::new()),
            needs_freeze: AtomicBool::new(false),
            arena_byte_budget,
            max_states_per_pattern,
        }
    }

    /// Ensure the frozen snapshot is up-to-date.
    ///
    /// If patterns have been added since the last freeze, acquires the build lock
    /// and freezes the mutable structures into a new frozen snapshot. This defers
    /// the O(n*L) freeze cost to the first match after a batch of adds, avoiding
    /// O(n²*L) total cost from freezing after every individual add_pattern.
    fn ensure_frozen(&self) {
        if self.needs_freeze.load(Ordering::Acquire) {
            let build_state = self.build_lock.lock();
            // Double-check under lock: another thread may have frozen already
            if self.needs_freeze.load(Ordering::Relaxed) {
                let frozen = self.freeze_field_matcher(&build_state.root);
                self.root.store(Arc::new(frozen));
                self.needs_freeze.store(false, Ordering::Release);
            }
        }
    }

    /// Add a pattern with the given identifier.
    ///
    /// This method is thread-safe but serialized - only one pattern can be added at a time.
    /// The pattern is not frozen immediately; the frozen snapshot is updated lazily on
    /// the next match operation, amortizing the freeze cost across batches of adds.
    /// The pattern_fields should be a list of (path, matchers) tuples.
    pub fn add_pattern(
        &self,
        x: X,
        pattern_fields: &[(String, Vec<crate::json::Matcher>)],
    ) -> Result<(), crate::QuaminaError> {
        // Acquire build lock
        let build_state = self.build_lock.lock();

        // Sort fields lexically by path (like Go)
        let mut sorted_fields: Vec<_> = pattern_fields.to_vec();
        sorted_fields.sort_by(|a, b| a.0.cmp(&b.0));

        // Build using mutable structures
        let mut states: Vec<Rc<MutableFieldMatcher<X>>> = vec![build_state.root.clone()];

        for (path, matchers) in &sorted_fields {
            if matchers.is_empty() {
                continue;
            }

            let mut next_states = Vec::new();

            for state in &states {
                let first_matcher = &matchers[0];
                match first_matcher {
                    crate::json::Matcher::Exists(true) => {
                        let next = state.add_exists(true, path);
                        next_states.push(next);
                    }
                    crate::json::Matcher::Exists(false) => {
                        let next = state.add_exists(false, path);
                        next_states.push(next);
                    }
                    _ => {
                        let nexts = state.add_transition(path, matchers, self.arena_byte_budget)?;
                        next_states.extend(nexts);
                    }
                }
            }

            if next_states.len() > self.max_states_per_pattern {
                return Err(crate::QuaminaError::PatternTooComplex(format!(
                    "field-matcher state count {} exceeds maximum of {} \
                     (pattern has too many mixed-type matchers across fields)",
                    next_states.len(),
                    self.max_states_per_pattern
                )));
            }
            states = next_states;
        }

        // Mark terminal states with the pattern identifier
        for state in states {
            state.add_match(x.clone());
        }

        // Mark that frozen snapshot needs updating.
        // Freeze is deferred to the next match operation (ensure_frozen),
        // avoiding O(n²) cost from cloning the growing arena after every add.
        self.needs_freeze.store(true, Ordering::Release);
        Ok(())
    }

    /// Collect aggregate arena statistics from all frozen value matchers.
    ///
    /// Ensures the frozen snapshot is up-to-date, then walks the frozen tree
    /// summing stats from every arena (main + suffix + multi-condition).
    pub fn arena_stats(&self) -> ArenaStats {
        self.ensure_frozen();
        let root = self.root.load();
        let mut stats = ArenaStats::default();
        let mut visited_fm: FxHashSet<usize> = FxHashSet::default();
        let mut visited_vm: FxHashSet<usize> = FxHashSet::default();
        Self::collect_fm_stats(&root, &mut stats, &mut visited_fm, &mut visited_vm);
        stats
    }

    fn collect_fm_stats(
        fm: &FrozenFieldMatcher<X>,
        stats: &mut ArenaStats,
        visited_fm: &mut FxHashSet<usize>,
        visited_vm: &mut FxHashSet<usize>,
    ) {
        let ptr = fm as *const _ as usize;
        if !visited_fm.insert(ptr) {
            return;
        }
        for vm in fm.transitions.values() {
            Self::collect_vm_stats(vm, stats, visited_fm, visited_vm);
        }
        for fm_next in fm.exists_true.values() {
            Self::collect_fm_stats(fm_next, stats, visited_fm, visited_vm);
        }
        for fm_next in fm.exists_false.values() {
            Self::collect_fm_stats(fm_next, stats, visited_fm, visited_vm);
        }
    }

    fn collect_vm_stats(
        vm: &FrozenValueMatcher<X>,
        stats: &mut ArenaStats,
        visited_fm: &mut FxHashSet<usize>,
        visited_vm: &mut FxHashSet<usize>,
    ) {
        let ptr = vm as *const _ as usize;
        if !visited_vm.insert(ptr) {
            return;
        }
        if let Some((ref arena, _)) = vm.main_arena {
            stats.add(&arena.stats());
        }
        if let Some((ref arena, _)) = vm.suffix_arena {
            stats.add(&arena.stats());
        }
        for mc in &vm.multi_condition_nfas {
            stats.add(&mc.primary_arena.stats());
            for cond in &mc.conditions {
                stats.add(&cond.arena.stats());
            }
        }
        // Walk transition_map to reach nested FrozenFieldMatchers
        for fm_next in vm.transition_map.values() {
            Self::collect_fm_stats(fm_next, stats, visited_fm, visited_vm);
        }
        if let Some(ref fm_next) = vm.singleton_transition {
            Self::collect_fm_stats(fm_next, stats, visited_fm, visited_vm);
        }
    }

    /// Freeze a MutableFieldMatcher into a FrozenFieldMatcher
    fn freeze_field_matcher(&self, mutable: &Rc<MutableFieldMatcher<X>>) -> FrozenFieldMatcher<X> {
        // Use a cache to handle cycles and sharing
        let mut cache: HashMap<*const MutableFieldMatcher<X>, Arc<FrozenFieldMatcher<X>>> =
            HashMap::new();
        self.freeze_field_matcher_impl(mutable, &mut cache)
    }

    fn freeze_field_matcher_impl(
        &self,
        mutable: &Rc<MutableFieldMatcher<X>>,
        cache: &mut HashMap<*const MutableFieldMatcher<X>, Arc<FrozenFieldMatcher<X>>>,
    ) -> FrozenFieldMatcher<X> {
        let ptr = Rc::as_ptr(mutable);

        // Check cache first
        if let Some(cached) = cache.get(&ptr) {
            // Return a clone of the cached frozen matcher's contents
            return (*cached.as_ref()).clone();
        }

        // Create a placeholder to handle cycles
        let placeholder = Arc::new(FrozenFieldMatcher::new());
        cache.insert(ptr, placeholder);

        // Freeze transitions
        let mut frozen_transitions = FxHashMap::default();
        for (path, vm) in mutable.transitions.borrow().iter() {
            let frozen_vm = self.freeze_value_matcher(vm, cache);
            frozen_transitions.insert(path.clone(), Arc::new(frozen_vm));
        }

        // Freeze exists_true
        let mut frozen_exists_true = FxHashMap::default();
        for (path, fm) in mutable.exists_true.borrow().iter() {
            let frozen_fm = self.freeze_field_matcher_impl(fm, cache);
            frozen_exists_true.insert(path.clone(), Arc::new(frozen_fm));
        }

        // Freeze exists_false
        let mut frozen_exists_false = FxHashMap::default();
        for (path, fm) in mutable.exists_false.borrow().iter() {
            let frozen_fm = self.freeze_field_matcher_impl(fm, cache);
            frozen_exists_false.insert(path.clone(), Arc::new(frozen_fm));
        }

        FrozenFieldMatcher {
            transitions: frozen_transitions,
            matches: mutable.matches.borrow().clone(),
            exists_true: frozen_exists_true,
            exists_false: frozen_exists_false,
        }
    }

    fn freeze_value_matcher(
        &self,
        mutable: &Rc<MutableValueMatcher<X>>,
        cache: &mut HashMap<*const MutableFieldMatcher<X>, Arc<FrozenFieldMatcher<X>>>,
    ) -> FrozenValueMatcher<X> {
        // Handle singleton optimization
        let singleton_match = mutable.singleton_match.borrow().clone();
        let singleton_transition = mutable.singleton_transition.borrow().as_ref().map(|fm| {
            let frozen = self.freeze_field_matcher_impl(fm, cache);
            Arc::new(frozen)
        });

        // Build transition map for automaton-based matching
        // Use the pointer address as the key - matches what arena traversal returns
        let mut transition_map = FxHashMap::default();
        for (ptr, mutable_fm) in mutable.transition_map.borrow().iter() {
            let frozen_fm = self.freeze_field_matcher_impl(mutable_fm, cache);
            // Use the raw pointer value as the key (cast to usize for hash stability)
            transition_map.insert(*ptr as usize, Arc::new(frozen_fm));
        }

        // Copy the multi-condition NFAs (for lookaround patterns)
        let multi_condition_nfas = mutable.multi_condition_nfas.borrow().clone();

        // Copy the main_arena (unified arena for all pattern types).
        // Re-freeze table buffers to pick up any in-place modifications
        // (e.g. insert_string_into_arena) since the last precompute.
        let main_arena = mutable
            .main_arena
            .borrow()
            .clone()
            .map(|(mut arena, start)| {
                arena.precompute_epsilon_closures();
                arena.flatten_tables();
                (arena, start)
            });

        // Copy the suffix_arena (reversed DFA trie for suffix patterns).
        // Freeze tables for the suffix arena too (it skips precompute during build).
        let suffix_arena = mutable
            .suffix_arena
            .borrow()
            .clone()
            .map(|(mut arena, start)| {
                arena.precompute_epsilon_closures();
                arena.flatten_tables();
                (arena, start)
            });

        FrozenValueMatcher {
            singleton_match,
            singleton_transition,
            has_numbers: mutable.has_numbers.get(),
            transition_map,
            multi_condition_nfas,
            main_arena,
            main_arena_is_nfa: *mutable.main_arena_is_nfa.borrow(),
            suffix_arena,
        }
    }

    /// Match fields against patterns and return matching pattern identifiers.
    ///
    /// Can be called concurrently from multiple threads. Lock-free in steady state;
    /// briefly acquires the build lock on the first call after pattern additions
    /// to freeze the mutable structures into an immutable snapshot.
    pub fn matches_for_fields(&self, fields: &[EventField]) -> Vec<X> {
        self.ensure_frozen();
        let root = self.root.load();

        if fields.is_empty() {
            return self.collect_exists_false_matches(&root);
        }

        let mut matches = FrozenMatchSet::new();
        let mut bufs = NfaBuffers::new();

        // For each field, try to match from the start state
        for i in 0..fields.len() {
            self.try_to_match(fields, i, &root, &mut matches, &mut bufs);
        }

        matches.into_vec()
    }

    fn try_to_match(
        &self,
        fields: &[EventField],
        index: usize,
        state: &Arc<FrozenFieldMatcher<X>>,
        matches: &mut FrozenMatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        let field = &fields[index];

        // Check exists:true transition
        if let Some(exists_trans) = state.exists_true.get(&field.path) {
            for m in &exists_trans.matches {
                matches.add(m.clone());
            }
            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict(&field.array_trail, &fields[next_idx].array_trail) {
                    self.try_to_match(fields, next_idx, exists_trans, matches, bufs);
                }
            }
            self.check_exists_false(state, fields, index, matches, bufs);
        }

        // Check exists:false
        self.check_exists_false(state, fields, index, matches, bufs);

        // Try value transitions
        let next_states =
            state.transition_on(&field.path, field.value.as_bytes(), field.is_number, bufs);

        for next_state in next_states.iter() {
            for m in &next_state.matches {
                matches.add(m.clone());
            }

            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict(&field.array_trail, &fields[next_idx].array_trail) {
                    self.try_to_match(fields, next_idx, next_state, matches, bufs);
                }
            }

            self.check_exists_false(next_state, fields, index, matches, bufs);
        }
    }

    fn check_exists_false(
        &self,
        state: &Arc<FrozenFieldMatcher<X>>,
        fields: &[EventField],
        index: usize,
        matches: &mut FrozenMatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        for (path, exists_trans) in &state.exists_false {
            let field_exists = fields
                .binary_search_by(|f| f.path.as_str().cmp(path.as_str()))
                .is_ok();

            if !field_exists {
                for m in &exists_trans.matches {
                    matches.add(m.clone());
                }
                self.try_to_match(fields, index, exists_trans, matches, bufs);
            }
        }
    }

    fn collect_exists_false_matches(&self, state: &Arc<FrozenFieldMatcher<X>>) -> Vec<X> {
        let mut result = Vec::new();
        for exists_trans in state.exists_false.values() {
            result.extend(exists_trans.matches.iter().cloned());
        }
        result
    }

    /// Match fields using zero-copy field references.
    ///
    /// Can be called concurrently from multiple threads. Lock-free in steady state;
    /// briefly acquires the build lock on the first call after pattern additions.
    /// The `bufs` parameter should be a reusable NfaBuffers instance for reduced allocations.
    pub fn matches_for_fields_ref(
        &self,
        fields: &[EventFieldRef<'_>],
        bufs: &mut NfaBuffers,
    ) -> Vec<X> {
        self.ensure_frozen();
        let root = self.root.load();

        if fields.is_empty() {
            return self.collect_exists_false_matches(&root);
        }

        let mut matches = FrozenMatchSet::new();
        bufs.clear(); // Reset buffers for reuse

        for i in 0..fields.len() {
            self.try_to_match_ref(fields, i, &root, &mut matches, bufs);
        }

        matches.into_vec()
    }

    fn try_to_match_ref(
        &self,
        fields: &[EventFieldRef<'_>],
        index: usize,
        state: &Arc<FrozenFieldMatcher<X>>,
        matches: &mut FrozenMatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        let field = &fields[index];

        // Check exists:true transition
        if let Some(exists_trans) = state.exists_true.get(field.path) {
            for m in &exists_trans.matches {
                matches.add(m.clone());
            }
            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict_ref(field.array_trail, fields[next_idx].array_trail) {
                    self.try_to_match_ref(fields, next_idx, exists_trans, matches, bufs);
                }
            }
            self.check_exists_false_ref(state, fields, index, matches, bufs);
        }

        // Check exists:false
        self.check_exists_false_ref(state, fields, index, matches, bufs);

        // Try value transitions
        let next_states = state.transition_on(field.path, field.value, field.is_number, bufs);

        for next_state in next_states.iter() {
            for m in &next_state.matches {
                matches.add(m.clone());
            }

            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict_ref(field.array_trail, fields[next_idx].array_trail) {
                    self.try_to_match_ref(fields, next_idx, next_state, matches, bufs);
                }
            }

            self.check_exists_false_ref(next_state, fields, index, matches, bufs);
        }
    }

    fn check_exists_false_ref(
        &self,
        state: &Arc<FrozenFieldMatcher<X>>,
        fields: &[EventFieldRef<'_>],
        index: usize,
        matches: &mut FrozenMatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        for (path, exists_trans) in &state.exists_false {
            let field_exists = fields
                .binary_search_by(|f| f.path.cmp(path.as_str()))
                .is_ok();

            if !field_exists {
                for m in &exists_trans.matches {
                    matches.add(m.clone());
                }
                self.try_to_match_ref(fields, index, exists_trans, matches, bufs);
            }
        }
    }

    /// Match fields using flattened fields directly.
    ///
    /// This avoids the intermediate EventFieldRef allocation by working
    /// directly with flatten_json::Field. Fields should already be sorted by path.
    /// Can be called concurrently from multiple threads. Lock-free in steady state;
    /// briefly acquires the build lock on the first call after pattern additions.
    pub fn matches_for_fields_direct(
        &self,
        fields: &[crate::flatten_json::Field<'_>],
        bufs: &mut NfaBuffers,
    ) -> Vec<X> {
        self.ensure_frozen();
        let root = self.root.load();

        if fields.is_empty() {
            return self.collect_exists_false_matches(&root);
        }

        let mut matches = FrozenMatchSet::new();
        bufs.clear();

        for i in 0..fields.len() {
            self.try_to_match_direct(fields, i, &root, &mut matches, bufs);
        }

        matches.into_vec()
    }

    fn try_to_match_direct(
        &self,
        fields: &[crate::flatten_json::Field<'_>],
        index: usize,
        state: &Arc<FrozenFieldMatcher<X>>,
        matches: &mut FrozenMatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        let field = &fields[index];
        let path = field.path_str();
        let value = field.value_bytes();
        let array_trail = field.array_trail_slice();

        // Check exists:true transition
        if let Some(exists_trans) = state.exists_true.get(path) {
            for m in &exists_trans.matches {
                matches.add(m.clone());
            }
            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict_ref(array_trail, fields[next_idx].array_trail_slice()) {
                    self.try_to_match_direct(fields, next_idx, exists_trans, matches, bufs);
                }
            }
            self.check_exists_false_direct(state, fields, index, matches, bufs);
        }

        // Check exists:false
        self.check_exists_false_direct(state, fields, index, matches, bufs);

        // Try value transitions
        let next_states = state.transition_on(path, value, field.is_number, bufs);

        for next_state in next_states.iter() {
            for m in &next_state.matches {
                matches.add(m.clone());
            }

            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict_ref(array_trail, fields[next_idx].array_trail_slice()) {
                    self.try_to_match_direct(fields, next_idx, next_state, matches, bufs);
                }
            }

            self.check_exists_false_direct(next_state, fields, index, matches, bufs);
        }
    }

    fn check_exists_false_direct(
        &self,
        state: &Arc<FrozenFieldMatcher<X>>,
        fields: &[crate::flatten_json::Field<'_>],
        index: usize,
        matches: &mut FrozenMatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        for (path, exists_trans) in &state.exists_false {
            let field_exists = fields
                .binary_search_by(|f| f.path.as_ref().cmp(path.as_bytes()))
                .is_ok();

            if !field_exists {
                for m in &exists_trans.matches {
                    matches.add(m.clone());
                }
                self.try_to_match_direct(fields, index, exists_trans, matches, bufs);
            }
        }
    }
}

impl<X: Clone + Eq + Hash + Send + Sync> Default for ThreadSafeCoreMatcher<X> {
    fn default() -> Self {
        Self::new()
    }
}

/// A set of matches (deduplicated) for frozen matcher.
///
/// Uses linear dedup on a Vec instead of a HashSet. This is faster for the
/// common case of few matches (0-10) because it avoids hash table allocation
/// and hashing overhead. Pattern matching typically produces few results.
struct FrozenMatchSet<X: Clone + Eq> {
    matches: Vec<X>,
}

impl<X: Clone + Eq> FrozenMatchSet<X> {
    fn new() -> Self {
        Self {
            matches: Vec::new(),
        }
    }

    fn add(&mut self, x: X) {
        if !self.matches.contains(&x) {
            self.matches.push(x);
        }
    }

    fn into_vec(self) -> Vec<X> {
        self.matches
    }
}

/// A working value matcher that uses automata for matching
///
/// This demonstrates how automaton-based value matching works.
/// Values are matched by traversing the automaton on the value bytes.
#[derive(Clone, Default)]
pub struct AutomatonValueMatcher<X: Clone + Eq + std::hash::Hash> {
    /// The arena-based automaton (arena, start_state)
    arena: Option<(StateArena, StateId)>,
    /// Map from match ID to pattern identifiers
    /// Uses match_id stored in FieldMatcher, which survives FA merging
    pattern_map: FxHashMap<u64, X>,
    /// Counter for generating unique match IDs
    next_match_id: u64,
}

impl<X: Clone + Eq + std::hash::Hash> AutomatonValueMatcher<X> {
    /// Create a new empty value matcher
    pub fn new() -> Self {
        Self {
            arena: None,
            pattern_map: FxHashMap::default(),
            next_match_id: 1,
        }
    }

    /// Add a string match pattern
    pub fn add_string_match(&mut self, value: &[u8], x: X) {
        let match_id = self.next_match_id;
        self.next_match_id += 1;
        self.pattern_map.insert(match_id, x);

        let next_field = Arc::new(FieldMatcher::with_match_id(match_id));
        let (new_arena, new_start) = make_string_arena_fa(value, next_field);

        self.merge_arena(new_arena, new_start);
    }

    /// Add a prefix match pattern
    pub fn add_prefix_match(&mut self, prefix: &[u8], x: X) {
        let match_id = self.next_match_id;
        self.next_match_id += 1;
        self.pattern_map.insert(match_id, x);

        let next_field = Arc::new(FieldMatcher::with_match_id(match_id));
        let (new_arena, new_start) = make_prefix_arena_fa(prefix, next_field);

        self.merge_arena(new_arena, new_start);
    }

    /// Add a shellstyle pattern
    pub fn add_shellstyle_match(&mut self, pattern: &[u8], x: X) {
        let match_id = self.next_match_id;
        self.next_match_id += 1;
        self.pattern_map.insert(match_id, x);

        let next_field = Arc::new(FieldMatcher::with_match_id(match_id));
        let (new_arena, new_start) = make_shellstyle_arena_fa(pattern, next_field);

        self.merge_arena(new_arena, new_start);
    }

    /// Helper to merge a new arena into the existing one
    fn merge_arena(&mut self, new_arena: StateArena, new_start: StateId) {
        match self.arena.take() {
            Some((existing_arena, existing_start)) => {
                let (merged_arena, merged_start) =
                    merge_arena_nfas(&existing_arena, existing_start, &new_arena, new_start);
                self.arena = Some((merged_arena, merged_start));
            }
            None => {
                self.arena = Some((new_arena, new_start));
            }
        }
    }

    /// Match a value against all patterns
    pub fn match_value(&self, value: &[u8]) -> Vec<X> {
        let (arena, start) = match &self.arena {
            Some((a, s)) => (a, *s),
            None => return vec![],
        };

        let mut bufs = ArenaNfaBuffers::new();
        traverse_arena_nfa(arena, start, value, &mut bufs);

        // Map transitions back to pattern identifiers using match_id.
        let mut matches = Vec::new();
        let mut seen_ids = FxHashSet::default();
        for &ptr in &bufs.transitions {
            // SAFETY: Each pointer in bufs.transitions was obtained from Arc::as_ptr()
            // on an Arc<FieldMatcher> stored in the arena's field_transitions. The arena
            // is borrowed immutably for this entire scope (via &self.arena), so the
            // pointed-to FieldMatcher is guaranteed to be alive and immutable.
            let fm = unsafe { &*(ptr as *const FieldMatcher) };
            if let Some(match_id) = fm.match_id {
                if seen_ids.insert(match_id) {
                    if let Some(x) = self.pattern_map.get(&match_id) {
                        matches.push(x.clone());
                    }
                }
            }
        }

        matches
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::json::Matcher;

    /// Guard against replacing Transitions with a larger type (e.g. SmallVec<[...; 4]> = 48 bytes).
    /// A larger return type from transition_on causes a measurable no-match regression because
    /// try_to_match is recursive and the return value is constructed on every call, even when empty.
    /// See docs/plans/investigate-no-match-regression.md for the full analysis.
    #[test]
    fn transitions_size_must_stay_compact() {
        let size = std::mem::size_of::<Transitions<Arc<FrozenFieldMatcher<String>>>>();
        assert!(
            size <= 24,
            "Transitions<Arc<...>> is {size} bytes, must be <= 24 (Vec size). \
             Larger types cause no-match benchmark regressions in recursive try_to_match."
        );
    }

    #[test]
    fn test_thread_safe_core_matcher_basic() {
        let matcher: ThreadSafeCoreMatcher<String> = ThreadSafeCoreMatcher::new();

        // Add patterns (thread-safe, serialized)
        matcher
            .add_pattern(
                "p1".to_string(),
                &[(
                    "status".to_string(),
                    vec![Matcher::Exact("active".to_string())],
                )],
            )
            .unwrap();

        // Match events (thread-safe, concurrent)
        let fields = vec![EventField {
            path: "status".to_string(),
            value: "active".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        let matches = matcher.matches_for_fields(&fields);

        assert_eq!(matches, vec!["p1".to_string()]);
    }
}
