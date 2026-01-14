//! Thread-safe pattern matching components.
//!
//! This module contains thread-safe (Send + Sync) matchers for concurrent use:
//! - `FrozenFieldMatcher`: Immutable field matcher for concurrent reads
//! - `FrozenValueMatcher`: Immutable value matcher for concurrent reads
//! - `ThreadSafeCoreMatcher`: Thread-safe wrapper with lock-free matching
//! - `AutomatonValueMatcher`: A working value matcher that uses automata

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;
use std::sync::Arc;

use rustc_hash::FxHashMap;

use arc_swap::ArcSwap;
use parking_lot::Mutex;

use super::arena::{traverse_arena_nfa, ArenaNfaBuffers, StateArena, StateId};
use super::fa_builders::{make_prefix_fa, make_shellstyle_fa, make_string_fa, merge_fas};
use super::mutable_matcher::{EventField, EventFieldRef, MutableFieldMatcher, MutableValueMatcher};
use super::nfa::{traverse_dfa, traverse_nfa};
use super::small_table::{FieldMatcher, NfaBuffers, SmallTable};

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
    pub transitions: HashMap<String, Arc<FrozenValueMatcher<X>>>,
    /// Pattern identifiers that match when arriving at this state
    pub matches: Vec<X>,
    /// exists:true patterns - map from field path to next field matcher
    pub exists_true: HashMap<String, Arc<FrozenFieldMatcher<X>>>,
    /// exists:false patterns - map from field path to next field matcher
    pub exists_false: HashMap<String, Arc<FrozenFieldMatcher<X>>>,
}

// Safety: FrozenFieldMatcher only contains Arc, HashMap, and Vec - all Send+Sync when X is
unsafe impl<X: Clone + Eq + Hash + Send + Sync> Send for FrozenFieldMatcher<X> {}
unsafe impl<X: Clone + Eq + Hash + Send + Sync> Sync for FrozenFieldMatcher<X> {}

impl<X: Clone + Eq + Hash> FrozenFieldMatcher<X> {
    pub fn new() -> Self {
        Self {
            transitions: HashMap::new(),
            matches: Vec::new(),
            exists_true: HashMap::new(),
            exists_false: HashMap::new(),
        }
    }

    /// Transition on a field value during matching
    pub fn transition_on(
        &self,
        path: &str,
        value: &[u8],
        is_number: bool,
        bufs: &mut NfaBuffers,
    ) -> Vec<Arc<FrozenFieldMatcher<X>>> {
        if let Some(vm) = self.transitions.get(path) {
            vm.transition_on(value, is_number, bufs)
        } else {
            vec![]
        }
    }
}

/// Frozen (immutable) value matcher - Send + Sync
///
/// This is the immutable counterpart to MutableValueMatcher.
#[derive(Clone, Default)]
pub struct FrozenValueMatcher<X: Clone + Eq + Hash> {
    /// The automaton start table
    start_table: Option<SmallTable>,
    /// Optimization: for single exact match, store it directly
    singleton_match: Option<Vec<u8>>,
    /// Transition for singleton match
    singleton_transition: Option<Arc<FrozenFieldMatcher<X>>>,
    /// Whether this matcher requires NFA traversal
    is_nondeterministic: bool,
    /// Whether this matcher has numeric patterns (for Q-number conversion)
    has_numbers: bool,
    /// Mapping from FieldMatcher pointer (as usize) to FrozenFieldMatcher
    /// Uses FxHashMap for fast integer key lookup
    transition_map: FxHashMap<usize, Arc<FrozenFieldMatcher<X>>>,
    /// Arena-based NFAs for regexp patterns (2.5x faster than chain-based)
    arena_nfas: Vec<(StateArena, StateId)>,
}

// Safety: FrozenValueMatcher only contains Arc, FxHashMap, Option, and primitives
unsafe impl<X: Clone + Eq + Hash + Send + Sync> Send for FrozenValueMatcher<X> {}
unsafe impl<X: Clone + Eq + Hash + Send + Sync> Sync for FrozenValueMatcher<X> {}

impl<X: Clone + Eq + Hash> FrozenValueMatcher<X> {
    pub fn new() -> Self {
        Self {
            start_table: None,
            singleton_match: None,
            singleton_transition: None,
            is_nondeterministic: false,
            has_numbers: false,
            transition_map: FxHashMap::default(),
            arena_nfas: Vec::new(),
        }
    }

    /// Transition on a value during matching
    #[inline]
    pub fn transition_on(
        &self,
        value: &[u8],
        is_number: bool,
        bufs: &mut NfaBuffers,
    ) -> Vec<Arc<FrozenFieldMatcher<X>>> {
        // Check singleton first
        if let Some(ref singleton_val) = self.singleton_match {
            if singleton_val == value {
                if let Some(ref trans) = self.singleton_transition {
                    return vec![trans.clone()];
                }
            }
            return vec![];
        }

        let mut result = Vec::new();

        // Try with Q-number conversion if this matcher has numbers and value is numeric
        // Use Cow to avoid allocation when not converting to Q-number
        let value_to_match: Cow<'_, [u8]> = if self.has_numbers && is_number {
            // Try to parse as f64 and convert to Q-number
            if let Ok(s) = std::str::from_utf8(value) {
                if let Ok(n) = s.parse::<f64>() {
                    Cow::Owned(crate::numbits::q_num_from_f64(n))
                } else {
                    Cow::Borrowed(value)
                }
            } else {
                Cow::Borrowed(value)
            }
        } else {
            Cow::Borrowed(value)
        };

        // Use chain-based automaton if present
        if let Some(ref table) = self.start_table {
            // Clear and reuse the transitions buffer
            bufs.transitions.clear();

            if self.is_nondeterministic {
                traverse_nfa(table, &value_to_match, bufs);
            } else {
                traverse_dfa(table, &value_to_match, &mut bufs.transitions);
            }

            // Map FieldMatcher transitions to FrozenFieldMatcher using pointer address
            for arc_fm in &bufs.transitions {
                let ptr = Arc::as_ptr(arc_fm) as usize;
                if let Some(frozen_fm) = self.transition_map.get(&ptr) {
                    result.push(frozen_fm.clone());
                }
            }
        }

        // Traverse all arena NFAs (for regexp patterns)
        if !self.arena_nfas.is_empty() {
            let mut arena_bufs = ArenaNfaBuffers::new();
            for (arena, start) in self.arena_nfas.iter() {
                traverse_arena_nfa(arena, *start, &value_to_match, &mut arena_bufs);

                // Map Arc<FieldMatcher> transitions to FrozenFieldMatcher using pointer address
                for arc_fm in &arena_bufs.transitions {
                    let ptr = Arc::as_ptr(arc_fm) as usize;
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

// Safety: BuildState is only ever accessed through a Mutex lock in ThreadSafeCoreMatcher.
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
/// Pattern addition is serialized via a mutex, while matching is lock-free.
///
/// See `tests::test_thread_safe_core_matcher_basic` for usage example.
pub struct ThreadSafeCoreMatcher<X: Clone + Eq + Hash + Send + Sync> {
    /// The frozen root - atomically swappable, lock-free reads
    root: ArcSwap<FrozenFieldMatcher<X>>,
    /// Mutex protecting pattern building
    build_lock: Mutex<BuildState<X>>,
}

// ThreadSafeCoreMatcher is Send + Sync because:
// - ArcSwap<T> is Send + Sync when T is Send + Sync
// - Mutex<T> is Send + Sync when T is Send
// - FrozenFieldMatcher is Send + Sync when X is
// - BuildState contains Rc which is !Send, but it's behind a Mutex which makes
//   the ThreadSafeCoreMatcher itself Send + Sync

impl<X: Clone + Eq + Hash + Send + Sync> ThreadSafeCoreMatcher<X> {
    /// Create a new ThreadSafeCoreMatcher
    pub fn new() -> Self {
        Self {
            root: ArcSwap::from_pointee(FrozenFieldMatcher::new()),
            build_lock: Mutex::new(BuildState::new()),
        }
    }

    /// Add a pattern with the given identifier.
    ///
    /// This method is thread-safe but serialized - only one pattern can be added at a time.
    /// The pattern_fields should be a list of (path, matchers) tuples.
    pub fn add_pattern(&self, x: X, pattern_fields: &[(String, Vec<crate::json::Matcher>)]) {
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
                        let nexts = state.add_transition(path, matchers);
                        next_states.extend(nexts);
                    }
                }
            }

            states = next_states;
        }

        // Mark terminal states with the pattern identifier
        for state in states {
            state.add_match(x.clone());
        }

        // Freeze the mutable structures and store atomically
        let frozen = self.freeze_field_matcher(&build_state.root);
        self.root.store(Arc::new(frozen));
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
        cache.insert(ptr, placeholder.clone());

        // Freeze transitions
        let mut frozen_transitions = HashMap::new();
        for (path, vm) in mutable.transitions.borrow().iter() {
            let frozen_vm = self.freeze_value_matcher(vm, cache);
            frozen_transitions.insert(path.clone(), Arc::new(frozen_vm));
        }

        // Freeze exists_true
        let mut frozen_exists_true = HashMap::new();
        for (path, fm) in mutable.exists_true.borrow().iter() {
            let frozen_fm = self.freeze_field_matcher_impl(fm, cache);
            frozen_exists_true.insert(path.clone(), Arc::new(frozen_fm));
        }

        // Freeze exists_false
        let mut frozen_exists_false = HashMap::new();
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
        // Use the pointer address as the key - this matches what traverse_dfa/nfa returns
        let mut transition_map = FxHashMap::default();
        for (ptr, mutable_fm) in mutable.transition_map.borrow().iter() {
            let frozen_fm = self.freeze_field_matcher_impl(mutable_fm, cache);
            // Use the raw pointer value as the key (cast to usize for hash stability)
            transition_map.insert(*ptr as usize, Arc::new(frozen_fm));
        }

        // Copy the arena NFAs
        let arena_nfas = mutable.arena_nfas.borrow().clone();

        FrozenValueMatcher {
            start_table: mutable.start_table.borrow().clone(),
            singleton_match,
            singleton_transition,
            is_nondeterministic: *mutable.is_nondeterministic.borrow(),
            has_numbers: *mutable.has_numbers.borrow(),
            transition_map,
            arena_nfas,
        }
    }

    /// Match fields against patterns and return matching pattern identifiers.
    ///
    /// This method is lock-free and can be called concurrently from multiple threads.
    pub fn matches_for_fields(&self, fields: &[EventField]) -> Vec<X> {
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

        for next_state in next_states {
            for m in &next_state.matches {
                matches.add(m.clone());
            }

            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict(&field.array_trail, &fields[next_idx].array_trail) {
                    self.try_to_match(fields, next_idx, &next_state, matches, bufs);
                }
            }

            self.check_exists_false(&next_state, fields, index, matches, bufs);
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
            let field_exists = fields.iter().any(|f| &f.path == path);

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
    /// This method is lock-free and can be called concurrently from multiple threads.
    /// The `bufs` parameter should be a reusable NfaBuffers instance for reduced allocations.
    pub fn matches_for_fields_ref(
        &self,
        fields: &[EventFieldRef<'_>],
        bufs: &mut NfaBuffers,
    ) -> Vec<X> {
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

        for next_state in next_states {
            for m in &next_state.matches {
                matches.add(m.clone());
            }

            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict_ref(field.array_trail, fields[next_idx].array_trail) {
                    self.try_to_match_ref(fields, next_idx, &next_state, matches, bufs);
                }
            }

            self.check_exists_false_ref(&next_state, fields, index, matches, bufs);
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
            let field_exists = fields.iter().any(|f| f.path == path);

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
    /// This method is lock-free and can be called concurrently from multiple threads.
    pub fn matches_for_fields_direct(
        &self,
        fields: &[crate::flatten_json::Field<'_>],
        bufs: &mut NfaBuffers,
    ) -> Vec<X> {
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

        for next_state in next_states {
            for m in &next_state.matches {
                matches.add(m.clone());
            }

            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict_ref(array_trail, fields[next_idx].array_trail_slice()) {
                    self.try_to_match_direct(fields, next_idx, &next_state, matches, bufs);
                }
            }

            self.check_exists_false_direct(&next_state, fields, index, matches, bufs);
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
            let field_exists = fields.iter().any(|f| f.path_str() == path);

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

/// A set of matches (deduplicated) for frozen matcher
struct FrozenMatchSet<X: Clone + Eq + Hash> {
    seen: HashSet<X>,
    matches: Vec<X>,
}

impl<X: Clone + Eq + Hash> FrozenMatchSet<X> {
    fn new() -> Self {
        Self {
            seen: HashSet::new(),
            matches: Vec::new(),
        }
    }

    fn add(&mut self, x: X) {
        if self.seen.insert(x.clone()) {
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
    /// The start table of the automaton
    start_table: Option<SmallTable>,
    /// Whether this matcher requires NFA traversal (has wildcards)
    is_nondeterministic: bool,
    /// Map from match ID to pattern identifiers
    /// Uses match_id stored in FieldMatcher, which survives FA merging
    pattern_map: HashMap<u64, X>,
    /// Counter for generating unique match IDs
    next_match_id: u64,
}

impl<X: Clone + Eq + std::hash::Hash> AutomatonValueMatcher<X> {
    /// Create a new empty value matcher
    pub fn new() -> Self {
        Self {
            start_table: None,
            is_nondeterministic: false,
            pattern_map: HashMap::new(),
            next_match_id: 1,
        }
    }

    /// Add a string match pattern
    pub fn add_string_match(&mut self, value: &[u8], x: X) {
        let match_id = self.next_match_id;
        self.next_match_id += 1;
        self.pattern_map.insert(match_id, x);

        let next_field = Arc::new(FieldMatcher::with_match_id(match_id));
        let new_fa = make_string_fa(value, next_field);

        match &self.start_table {
            Some(existing) => {
                self.start_table = Some(merge_fas(existing, &new_fa));
            }
            None => {
                self.start_table = Some(new_fa);
            }
        }
    }

    /// Add a prefix match pattern
    pub fn add_prefix_match(&mut self, prefix: &[u8], x: X) {
        let match_id = self.next_match_id;
        self.next_match_id += 1;
        self.pattern_map.insert(match_id, x);

        let next_field = Arc::new(FieldMatcher::with_match_id(match_id));
        let new_fa = make_prefix_fa(prefix, next_field);

        match &self.start_table {
            Some(existing) => {
                self.start_table = Some(merge_fas(existing, &new_fa));
            }
            None => {
                self.start_table = Some(new_fa);
            }
        }
    }

    /// Add a shellstyle pattern
    pub fn add_shellstyle_match(&mut self, pattern: &[u8], x: X) {
        let match_id = self.next_match_id;
        self.next_match_id += 1;
        self.pattern_map.insert(match_id, x);

        let next_field = Arc::new(FieldMatcher::with_match_id(match_id));
        let new_fa = make_shellstyle_fa(pattern, next_field);
        self.is_nondeterministic = true;

        match &self.start_table {
            Some(existing) => {
                self.start_table = Some(merge_fas(existing, &new_fa));
            }
            None => {
                self.start_table = Some(new_fa);
            }
        }
    }

    /// Match a value against all patterns
    pub fn match_value(&self, value: &[u8]) -> Vec<X> {
        let table = match &self.start_table {
            Some(t) => t,
            None => return vec![],
        };

        let mut bufs = NfaBuffers::new();
        if self.is_nondeterministic {
            traverse_nfa(table, value, &mut bufs);
        } else {
            traverse_dfa(table, value, &mut bufs.transitions);
        }

        // Map transitions back to pattern identifiers using match_id
        let mut matches = Vec::new();
        let mut seen_ids = HashSet::new();
        for fm in &bufs.transitions {
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

    #[test]
    fn test_thread_safe_core_matcher_basic() {
        let matcher: ThreadSafeCoreMatcher<String> = ThreadSafeCoreMatcher::new();

        // Add patterns (thread-safe, serialized)
        matcher.add_pattern(
            "p1".to_string(),
            &[("status".to_string(), vec![Matcher::Exact("active".to_string())])],
        );

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
