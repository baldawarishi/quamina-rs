//! Mutable pattern matchers for single-threaded pattern building.
//!
//! This module contains the mutable (RefCell-based) matchers used during pattern building:
//! - `MutableFieldMatcher`: Mutable field matcher with RefCell-based interior mutability
//! - `MutableValueMatcher`: Mutable value matcher with singleton optimization
//! - `CoreMatcher`: Single-threaded core matcher that builds and matches patterns

use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};

use super::arena::{
    ArenaInsertRollback, ClosureScratch, NfaBuffers as ArenaNfaBuffers, StateArena, StateId,
    append_merge_arena_nfas, clone_arena_subset, insert_string, insert_suffix,
    make_anything_but_arena_fa, make_cidr_arena_fa, make_monocase_arena_fa,
    make_numeric_greater_arena_fa, make_numeric_less_arena_fa, make_numeric_range_arena_fa,
    make_prefix_arena_fa, make_shellstyle_arena_fa, make_string_arena_fa, make_suffix_dfa,
    make_wildcard_arena_fa, merge_start_states, traverse_arena_dfa, traverse_arena_dfa_backward,
    traverse_arena_nfa,
};
use super::small_table::{FieldMatcher, NfaBuffers, TL_MATCH_BUFS};
use crate::regexp::make_regexp_nfa_arena;

/// Wrap a byte slice in quotes: `val` → `"val"`.
///
/// Used by `add_transition()` to wrap string pattern values so that
/// the automaton can distinguish strings from numbers. Event values
/// from the flattener retain their JSON quotes, so pattern FAs must
/// also include quotes to match correctly.
#[inline]
fn quote_wrap(val: &[u8]) -> Vec<u8> {
    let mut result = Vec::with_capacity(val.len() + 2);
    result.push(b'"');
    result.extend_from_slice(val);
    result.push(b'"');
    result
}

/// A condition NFA for lookaround verification.
///
/// Each condition is an automaton that must match (or not match, if negative)
/// the full value for the overall pattern to succeed.
#[derive(Clone, Debug)]
pub struct ConditionNfa {
    pub arena: StateArena,
    pub start: StateId,
    /// True for negative conditions ((?!...) or (?<!...))
    pub is_negative: bool,
}

/// Arena NFA with conditions for multi-condition patterns (lookarounds).
///
/// The primary automaton is matched first. If it produces transitions,
/// all conditions are verified against the full value.
#[derive(Clone)]
pub struct MultiConditionNfa {
    pub primary_arena: StateArena,
    pub primary_start: StateId,
    /// Field matcher pointer for transition mapping
    pub field_matcher_ptr: *const FieldMatcher,
    /// Conditions to verify after primary matches
    pub conditions: Vec<ConditionNfa>,
}

/// Build a combined pattern for lookbehind verification.
///
/// For `(?<=foo)bar`: lookbehind="foo", primary="bar" -> combined="foobar"
/// The combined pattern is used to check if the full value matches.
///
/// Like every condition it is checked against the whole value, so when a
/// lookahead closes the pattern and accounts for a run past the primary, the
/// combined pattern ends in `.*` to leave room for it: `(?<=foo)bar(?=baz)`
/// gives `foobar.*`, not `foobar`.
fn build_lookbehind_combined_pattern(
    lookbehind: &crate::regexp::RegexpRoot,
    primary: &crate::regexp::RegexpRoot,
    trailing_lookahead: bool,
) -> crate::regexp::RegexpRoot {
    use crate::regexp::{QuantifiedAtom, concat_roots};

    let mut combined = concat_roots(lookbehind, primary);

    if trailing_lookahead {
        for branch in &mut combined {
            branch.push(QuantifiedAtom::any_run());
        }
    }

    combined
}

/// Build the pattern used to verify the primary against a whole value.
///
/// Satisfying the assertions is not enough for a match — the value has to match
/// the primary too. The primary alone rarely spans the whole value, though,
/// because assertion text counts toward it: a lookbehind covers a run ahead of
/// the primary, and a lookahead in final position covers a run past it. Each of
/// those gets a `.*` so verification holds the value to the primary without
/// re-imposing what the conditions already check.
fn build_primary_verify_pattern(
    primary: &crate::regexp::RegexpRoot,
    leading_slack: bool,
    trailing_slack: bool,
) -> crate::regexp::RegexpRoot {
    use crate::regexp::{QuantifiedAtom, RegexpBranch};

    // An all-lookaround pattern such as `(?=foo)` has no primary of its own; the
    // conditions are the whole constraint, so accept anything here.
    if primary.is_empty() {
        return vec![vec![QuantifiedAtom::any_run()]];
    }

    primary
        .iter()
        .map(|branch| {
            let mut padded: RegexpBranch = Vec::with_capacity(branch.len() + 2);
            if leading_slack {
                padded.push(QuantifiedAtom::any_run());
            }
            padded.extend(branch.iter().cloned());
            if trailing_slack {
                padded.push(QuantifiedAtom::any_run());
            }
            padded
        })
        .collect()
}

/// A mutable field matcher used during pattern building.
/// This is similar to Go's fieldMatcher with its updateable atomic pointer.
#[derive(Default)]
pub struct MutableFieldMatcher<X: Clone + Eq + std::hash::Hash> {
    /// Map from field paths to value matchers
    pub transitions: RefCell<FxHashMap<String, Rc<MutableValueMatcher<X>>>>,
    /// Pattern identifiers that match when arriving at this state
    pub matches: RefCell<Vec<X>>,
    /// exists:true patterns - map from field path to next field matcher
    pub exists_true: RefCell<FxHashMap<String, Rc<Self>>>,
    /// exists:false patterns - map from field path to next field matcher
    pub exists_false: RefCell<FxHashMap<String, Rc<Self>>>,
}

impl<X: Clone + Eq + std::hash::Hash> MutableFieldMatcher<X> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            transitions: RefCell::new(FxHashMap::default()),
            matches: RefCell::new(Vec::new()),
            exists_true: RefCell::new(FxHashMap::default()),
            exists_false: RefCell::new(FxHashMap::default()),
        }
    }

    /// Add a match identifier to this state
    pub fn add_match(&self, x: X) {
        self.matches.borrow_mut().push(x);
    }

    /// Add an exists transition (true or false)
    pub fn add_exists(&self, exists: bool, path: &str) -> Rc<Self> {
        let map = if exists {
            &self.exists_true
        } else {
            &self.exists_false
        };

        let mut map_borrow = map.borrow_mut();
        if let Some(existing) = map_borrow.get(path) {
            existing.clone()
        } else {
            let new_fm = Rc::new(Self::new());
            map_borrow.insert(path.to_string(), new_fm.clone());
            new_fm
        }
    }

    /// Add a value transition, returns the next field matchers
    pub fn add_transition(
        &self,
        path: &str,
        matchers: &[crate::json::Matcher],
        budget: usize,
    ) -> Result<Vec<Rc<Self>>, crate::QuaminaError> {
        use crate::json::Matcher;

        let mut transitions = self.transitions.borrow_mut();
        let vm = transitions
            .entry(path.to_string())
            .or_insert_with(|| Rc::new(MutableValueMatcher::new()));

        // Check if all matchers are Exact strings - use bulk optimization
        // Note: Exact values are pre-quoted for strings in json.rs value_to_string()
        let all_exact: Vec<&[u8]> = matchers
            .iter()
            .filter_map(|m| match m {
                Matcher::Exact(s) => Some(s.as_bytes()),
                _ => None,
            })
            .collect();

        if all_exact.len() == matchers.len() {
            // All matchers are Exact strings: insert them into one shared trie
            // so they share a single continuation.
            let next_fm = vm.add_string_transitions_bulk(&all_exact, budget)?;
            return Ok(vec![next_fm]);
        }

        // Fall back to one-by-one processing
        let mut next_states = Vec::new();
        for matcher in matchers {
            let next_fm = vm.add_transition(matcher, budget)?;
            next_states.push(next_fm);
        }
        Ok(next_states)
    }

    /// Transition on a field value during matching
    pub fn transition_on(
        &self,
        path: &str,
        value: &[u8],
        is_number: bool,
        bufs: &mut NfaBuffers,
    ) -> Vec<Rc<Self>> {
        let transitions = self.transitions.borrow();
        if let Some(vm) = transitions.get(path) {
            vm.transition_on(value, is_number, bufs)
        } else {
            vec![]
        }
    }
}

/// A mutable value matcher used during pattern building.
/// Similar to Go's valueMatcher with singleton optimization and automaton.
pub struct MutableValueMatcher<X: Clone + Eq + std::hash::Hash> {
    /// Optimization: for single exact match, store it directly
    pub(crate) singleton_match: RefCell<Option<Vec<u8>>>,
    /// Transition for singleton match
    pub(crate) singleton_transition: RefCell<Option<Rc<MutableFieldMatcher<X>>>>,
    /// Whether this matcher has numeric patterns (for Q-number conversion)
    pub(crate) has_numbers: Cell<bool>,
    /// Mapping from `Arc<FieldMatcher>` to `Rc<MutableFieldMatcher<X>>`
    /// This bridges the automaton's field transitions to our mutable field matchers
    pub(crate) transition_map: RefCell<FxHashMap<*const FieldMatcher, Rc<MutableFieldMatcher<X>>>>,
    /// Multi-condition NFAs for lookaround patterns
    /// NOTE: Kept separate from main_arena for lookaround verification
    pub(crate) multi_condition_nfas: RefCell<Vec<MultiConditionNfa>>,
    /// Buffers for arena NFA traversal
    pub(crate) arena_bufs: RefCell<ArenaNfaBuffers>,
    /// Unified arena-based FA for all pattern types
    pub(crate) main_arena: RefCell<Option<(StateArena, StateId)>>,
    /// Whether main_arena contains NFA states (epsilon transitions or spinout states).
    /// When false, the fast traverse_arena_dfa path can be used instead of traverse_arena_nfa.
    pub(crate) main_arena_is_nfa: RefCell<bool>,
    /// Separate DFA trie for suffix patterns, traversed backward (right-to-left).
    /// Contains reversed suffix bytes; uses traverse_arena_dfa_backward at match time.
    pub(crate) suffix_arena: RefCell<Option<(StateArena, StateId)>>,
    /// Reused scratch for the epsilon-closure pass run on each pattern add, so its
    /// buffers are grown once and reused rather than reallocated per add.
    pub(crate) closure_scratch: RefCell<ClosureScratch>,
}

struct TakenSingleton<X: Clone + Eq + std::hash::Hash> {
    arena: StateArena,
    start: StateId,
    value: Vec<u8>,
    transition: Rc<MutableFieldMatcher<X>>,
    transition_key: *const FieldMatcher,
}

struct RegisteredTransition<X: Clone + Eq + std::hash::Hash> {
    next_fm: Rc<MutableFieldMatcher<X>>,
    next_arc: Arc<FieldMatcher>,
    transition_key: *const FieldMatcher,
}

struct DirectMainArenaInsert<X: Clone + Eq + std::hash::Hash> {
    had_main_arena: bool,
    main_arena_snapshot: Option<(StateArena, StateId)>,
    singleton_match: Option<Vec<u8>>,
    singleton_transition: Option<Rc<MutableFieldMatcher<X>>>,
    transition_keys: Option<FxHashSet<*const FieldMatcher>>,
    main_arena_is_nfa: bool,
}

impl<X: Clone + Eq + std::hash::Hash> Default for MutableValueMatcher<X> {
    fn default() -> Self {
        Self::new()
    }
}

impl<X: Clone + Eq + std::hash::Hash> MutableValueMatcher<X> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            singleton_match: RefCell::new(None),
            singleton_transition: RefCell::new(None),
            has_numbers: Cell::new(false),
            transition_map: RefCell::new(FxHashMap::default()),
            multi_condition_nfas: RefCell::new(Vec::new()),
            arena_bufs: RefCell::new(ArenaNfaBuffers::new()),
            main_arena: RefCell::new(None),
            main_arena_is_nfa: RefCell::new(false),
            suffix_arena: RefCell::new(None),
            closure_scratch: RefCell::new(ClosureScratch::new()),
        }
    }

    /// Check whether the given arena size exceeds the budget.
    ///
    /// A budget of 0 means unlimited; otherwise it bounds how large any single
    /// value matcher's arena may grow — the binding constraint for complex
    /// regexps.
    fn check_budget(size: usize, budget: usize) -> Result<(), crate::QuaminaError> {
        if budget != 0 && size > budget {
            return Err(crate::QuaminaError::PatternTooComplex(format!(
                "automaton byte size ({size} bytes) exceeds budget ({budget} bytes)"
            )));
        }
        Ok(())
    }

    /// Check main_arena budget. Call after any in-place arena mutation.
    fn check_main_arena_budget(&self, budget: usize) -> Result<(), crate::QuaminaError> {
        let main = self.main_arena.borrow();
        if let Some((arena, _)) = main.as_ref() {
            Self::check_budget(arena.estimated_byte_size(), budget)
        } else {
            Ok(())
        }
    }

    fn check_suffix_arena_budget(&self, budget: usize) -> Result<(), crate::QuaminaError> {
        let suffix = self.suffix_arena.borrow();
        if let Some((arena, _)) = suffix.as_ref() {
            Self::check_budget(arena.estimated_byte_size(), budget)
        } else {
            Ok(())
        }
    }

    fn register_transition(
        &self,
        next_fm: Rc<MutableFieldMatcher<X>>,
        next_arc: Arc<FieldMatcher>,
    ) -> RegisteredTransition<X> {
        let transition_key = Arc::as_ptr(&next_arc);
        self.transition_map
            .borrow_mut()
            .insert(transition_key, next_fm.clone());
        RegisteredTransition {
            next_fm,
            next_arc,
            transition_key,
        }
    }

    fn new_registered_transition(&self) -> RegisteredTransition<X> {
        self.register_transition(
            Rc::new(MutableFieldMatcher::new()),
            Arc::new(FieldMatcher::new()),
        )
    }

    fn remove_transition(&self, transition_key: *const FieldMatcher) {
        self.transition_map.borrow_mut().remove(&transition_key);
    }

    fn begin_direct_main_arena_insert(&self) -> DirectMainArenaInsert<X> {
        let singleton_match = self.singleton_match.borrow().clone();
        let singleton_transition = self.singleton_transition.borrow().clone();
        let transition_keys = if singleton_match.is_some() {
            Some(self.transition_map.borrow().keys().copied().collect())
        } else {
            None
        };

        let main = self.main_arena.borrow();
        let had_main_arena = main.is_some();
        let main_arena_snapshot = if singleton_match.is_some() {
            if had_main_arena {
                main.as_ref().cloned()
            } else {
                None
            }
        } else {
            None
        };

        DirectMainArenaInsert {
            had_main_arena,
            main_arena_snapshot,
            singleton_match,
            singleton_transition,
            transition_keys,
            main_arena_is_nfa: *self.main_arena_is_nfa.borrow(),
        }
    }

    fn rollback_direct_main_arena_insert(
        &self,
        snapshot: DirectMainArenaInsert<X>,
        transition_key: Option<*const FieldMatcher>,
        insert_rollbacks: Vec<ArenaInsertRollback>,
    ) {
        let DirectMainArenaInsert {
            had_main_arena,
            main_arena_snapshot,
            singleton_match,
            singleton_transition,
            transition_keys,
            main_arena_is_nfa,
        } = snapshot;

        {
            let mut main = self.main_arena.borrow_mut();
            if let Some(snapshot) = main_arena_snapshot {
                *main = Some(snapshot);
            } else if !had_main_arena {
                *main = None;
            } else if let Some((arena, _)) = main.as_mut() {
                for rollback in insert_rollbacks.into_iter().rev() {
                    rollback.rollback(arena);
                }
            }
        }

        if let Some(value) = singleton_match {
            *self.singleton_match.borrow_mut() = Some(value);
            *self.singleton_transition.borrow_mut() = singleton_transition;
        }
        *self.main_arena_is_nfa.borrow_mut() = main_arena_is_nfa;

        if let Some(old_keys) = transition_keys {
            self.transition_map
                .borrow_mut()
                .retain(|key, _| old_keys.contains(key));
        } else if let Some(key) = transition_key {
            self.remove_transition(key);
        }
    }

    fn rollback_suffix_arena_insert(
        &self,
        had_suffix_arena: bool,
        insert_rollback: Option<ArenaInsertRollback>,
    ) {
        let mut suffix = self.suffix_arena.borrow_mut();
        if !had_suffix_arena {
            *suffix = None;
        } else if let (Some((arena, _)), Some(rollback)) = (suffix.as_mut(), insert_rollback) {
            rollback.rollback(arena);
        }
    }

    /// Helper to merge an arena FA into main_arena.
    /// If main_arena is empty, just set it. Otherwise, append a copy-on-write
    /// merge into the existing arena and advance the live start on success.
    /// Checks the arena byte budget before and after merging.
    fn merge_into_main_arena(
        &self,
        new_arena: StateArena,
        new_start: StateId,
        budget: usize,
    ) -> Result<(), crate::QuaminaError> {
        Self::check_budget(new_arena.estimated_byte_size(), budget)?;

        let mut main = self.main_arena.borrow_mut();
        if let Some((existing_arena, existing_start)) = main.as_mut() {
            let old_start = *existing_start;
            let old_len = existing_arena.len();
            let merged_start =
                append_merge_arena_nfas(existing_arena, old_start, &new_arena, new_start);
            if *self.main_arena_is_nfa.borrow() {
                existing_arena
                    .precompute_epsilon_closures_into(&mut self.closure_scratch.borrow_mut());
            }

            if budget != 0 {
                let merged_size = existing_arena.estimated_byte_size();
                if merged_size > budget {
                    // The appended arena keeps unreachable history, so weigh the
                    // pattern against the live reachable subset instead. Only the
                    // compacted automaton has to fit the budget.
                    let (compacted, compacted_start) =
                        clone_arena_subset(existing_arena, merged_start);
                    let compacted_size = compacted.estimated_byte_size();
                    if compacted_size > budget {
                        existing_arena.truncate_states(old_len);
                        *existing_start = old_start;
                        return Err(crate::QuaminaError::PatternTooComplex(format!(
                            "automaton byte size ({compacted_size} bytes) exceeds budget ({budget} bytes)"
                        )));
                    }
                    *existing_arena = compacted;
                    *existing_start = compacted_start;
                    return Ok(());
                }
            }
            *existing_start = merged_start;
        } else {
            *main = Some((new_arena, new_start));
        }
        Ok(())
    }

    /// Consume the pending singleton (if any) into a standalone arena.
    /// Returns `Some(TakenSingleton)` if there was a singleton, `None` otherwise.
    /// Registers the singleton's FieldMatcher in transition_map so the returned
    /// arena can be matched unless the caller restores it after a failed add.
    fn take_singleton_as_arena(&self) -> Option<TakenSingleton<X>> {
        let singleton_val = self.singleton_match.borrow_mut().take()?;
        let singleton_trans = self
            .singleton_transition
            .borrow_mut()
            .take()
            .expect("singleton transition must exist with singleton value");
        let singleton_arc = Arc::new(FieldMatcher::new());
        let transition_key = Arc::as_ptr(&singleton_arc);
        self.transition_map
            .borrow_mut()
            .insert(transition_key, singleton_trans.clone());

        let (arena, start) = make_string_arena_fa(&singleton_val, singleton_arc);

        Some(TakenSingleton {
            arena,
            start,
            value: singleton_val,
            transition: singleton_trans,
            transition_key,
        })
    }

    fn restore_singleton(&self, singleton: TakenSingleton<X>) {
        self.remove_transition(singleton.transition_key);
        *self.singleton_match.borrow_mut() = Some(singleton.value);
        *self.singleton_transition.borrow_mut() = Some(singleton.transition);
    }

    fn merge_taken_singleton_into_main(
        &self,
        singleton: TakenSingleton<X>,
        budget: usize,
    ) -> Result<(), crate::QuaminaError> {
        let result = self.merge_into_main_arena(singleton.arena.clone(), singleton.start, budget);
        if let Err(err) = result {
            self.restore_singleton(singleton);
            return Err(err);
        }
        Ok(())
    }

    /// Merge a new arena FA into main_arena, incorporating any pending singleton.
    ///
    /// This is the single entry point for all non-string pattern types
    /// (prefix, shellstyle, wildcard, anything-but, monocase, regexp,
    /// numeric-range, CIDR). It:
    ///   1. Consumes the singleton (if any) into an arena
    ///   2. Merges the singleton arena with the new arena
    ///   3. Merges the result into main_arena (with budget check)
    fn merge_with_singleton(
        &self,
        new_arena: StateArena,
        new_start: StateId,
        budget: usize,
    ) -> Result<(), crate::QuaminaError> {
        if let Some(singleton) = self.take_singleton_as_arena() {
            let (merged, merged_start) =
                merge_start_states(&singleton.arena, singleton.start, &new_arena, new_start);
            let result = self.merge_into_main_arena(merged, merged_start, budget);
            if let Err(err) = result {
                self.restore_singleton(singleton);
                return Err(err);
            }
            Ok(())
        } else {
            self.merge_into_main_arena(new_arena, new_start, budget)
        }
    }

    fn merge_registered_with_singleton(
        &self,
        registered: RegisteredTransition<X>,
        new_arena: StateArena,
        new_start: StateId,
        budget: usize,
        marks_main_arena_nfa: bool,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let old_main_arena_is_nfa = *self.main_arena_is_nfa.borrow();
        if marks_main_arena_nfa {
            *self.main_arena_is_nfa.borrow_mut() = true;
        }

        if let Err(err) = self.merge_with_singleton(new_arena, new_start, budget) {
            self.remove_transition(registered.transition_key);
            if marks_main_arena_nfa {
                *self.main_arena_is_nfa.borrow_mut() = old_main_arena_is_nfa;
            }
            return Err(err);
        }

        Ok(registered.next_fm)
    }

    /// Ensure main_arena exists, bootstrapping it from the singleton if needed.
    /// After this call, main_arena is guaranteed to be Some and singleton is cleared.
    fn ensure_main_arena_with_singleton(&self, budget: usize) -> Result<(), crate::QuaminaError> {
        if self.main_arena.borrow().is_some() {
            // Already exists — but if there's a pending singleton, fold it in.
            // Build a standalone arena from the singleton, then merge into main.
            if let Some(singleton) = self.take_singleton_as_arena() {
                self.merge_taken_singleton_into_main(singleton, budget)?;
            }
            return Ok(());
        }

        // No main_arena yet — create one
        if let Some(singleton) = self.take_singleton_as_arena() {
            self.merge_taken_singleton_into_main(singleton, budget)?;
        } else {
            // Create empty arena with a start state
            let mut arena = StateArena::new();
            let start = arena.alloc();
            arena.precompute_epsilon_closures();
            *self.main_arena.borrow_mut() = Some((arena, start));
        }
        Ok(())
    }

    /// Add a transition for a matcher, returns the next field matcher.
    ///
    /// String-based pattern values are wrapped in quotes before building FAs.
    /// This ensures string values (which retain quotes from the flattener) only
    /// match string patterns, not number events with identical digit content.
    pub fn add_transition(
        &self,
        matcher: &crate::json::Matcher,
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        use crate::json::Matcher;

        match matcher {
            // Exact values are pre-quoted for strings in json.rs value_to_string().
            // Boolean/null values remain unquoted, matching flattener output.
            Matcher::Exact(s) => self.add_string_transition(s.as_bytes(), budget),
            Matcher::NumericExact(n) => self.add_numeric_transition(*n, budget),
            Matcher::Prefix(s) => {
                // Prefix only needs opening quote — the FA matches prefix then
                // accepts anything after (including closing quote and VT)
                let mut quoted = Vec::with_capacity(s.len() + 1);
                quoted.push(b'"');
                quoted.extend_from_slice(s.as_bytes());
                self.add_prefix_transition(&quoted, budget)
            }
            Matcher::Shellstyle(s) => {
                self.add_shellstyle_transition(&quote_wrap(s.as_bytes()), budget)
            }
            Matcher::Wildcard(s) => self.add_wildcard_transition(&quote_wrap(s.as_bytes()), budget),
            Matcher::AnythingBut(excluded) => {
                let excluded_bytes: Vec<Vec<u8>> =
                    excluded.iter().map(|s| quote_wrap(s.as_bytes())).collect();
                self.add_anything_but_transition(&excluded_bytes, budget)
            }
            Matcher::AnythingButNumeric(excluded) => {
                // Mark as having numbers so values get Q-number conversion
                self.has_numbers.set(true);
                self.add_anything_but_numeric_transition(excluded, budget)
            }
            Matcher::EqualsIgnoreCase(s) => {
                self.add_monocase_transition(&quote_wrap(s.as_bytes()), budget)
            }
            Matcher::ParsedRegexp(tree) => self.add_regexp_transition(tree, budget),
            Matcher::MultiCondition(mc) => self.add_multi_condition_transition(mc, budget),
            Matcher::Suffix(s) => self.add_suffix_transition(s, budget),
            Matcher::Numeric(cmp) => {
                // Numeric ranges use Q-number ordering in the automaton
                self.has_numbers.set(true);
                self.add_numeric_range_transition(cmp, budget)
            }
            Matcher::Cidr(cidr) => self.add_cidr_transition(cidr, budget),
            // Exists is resolved at the field level; here it gets an empty
            // value matcher so the pattern still wires into the field tree.
            Matcher::Exists(_) => Ok(Rc::new(MutableFieldMatcher::new())),
        }
    }

    /// Add multiple string transitions.
    ///
    /// All values share the same next field matcher.
    /// Uses in-place arena insertion for O(n*L) total cost instead of O(n²).
    fn add_string_transitions_bulk(
        &self,
        values: &[&[u8]],
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        if values.is_empty() {
            return Ok(Rc::new(MutableFieldMatcher::new()));
        }

        // If only one value, use the normal path (singleton optimization)
        if values.len() == 1 {
            return self.add_string_transition(values[0], budget);
        }

        // Create a shared next state for all new values
        let snapshot = self.begin_direct_main_arena_insert();
        let registered = self.new_registered_transition();

        if let Err(err) = self.ensure_main_arena_with_singleton(budget) {
            self.rollback_direct_main_arena_insert(
                snapshot,
                Some(registered.transition_key),
                Vec::new(),
            );
            return Err(err);
        }
        let mut insert_rollbacks = Vec::with_capacity(values.len());
        {
            let mut main = self.main_arena.borrow_mut();
            let (arena, start) = main.as_mut().unwrap();
            for val in values {
                insert_rollbacks.push(insert_string(
                    arena,
                    *start,
                    val,
                    registered.next_arc.clone(),
                ));
            }
        }
        if let Err(err) = self.check_main_arena_budget(budget) {
            self.rollback_direct_main_arena_insert(
                snapshot,
                Some(registered.transition_key),
                insert_rollbacks,
            );
            return Err(err);
        }

        Ok(registered.next_fm)
    }

    fn add_string_transition(
        &self,
        val: &[u8],
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        // Check singleton optimization - only use if no automaton exists yet
        let singleton = self.singleton_match.borrow();
        let singleton_trans = self.singleton_transition.borrow();

        // Check if virgin state (no singleton, no main_arena, no suffix_arena)
        let is_virgin = singleton.is_none()
            && self.main_arena.borrow().is_none()
            && self.suffix_arena.borrow().is_none();

        if is_virgin {
            // Virgin state - use singleton optimization
            drop(singleton);
            drop(singleton_trans);

            let next_fm = Rc::new(MutableFieldMatcher::new());
            *self.singleton_match.borrow_mut() = Some(val.to_vec());
            *self.singleton_transition.borrow_mut() = Some(next_fm.clone());
            return Ok(next_fm);
        }

        // Check if singleton matches
        if let Some(ref existing) = *singleton
            && existing == val
        {
            return Ok(singleton_trans.as_ref().unwrap().clone());
        }
        drop(singleton);
        drop(singleton_trans);

        // Need to build arena-based automaton
        let snapshot = self.begin_direct_main_arena_insert();
        let registered = self.new_registered_transition();

        if let Err(err) = self.ensure_main_arena_with_singleton(budget) {
            self.rollback_direct_main_arena_insert(
                snapshot,
                Some(registered.transition_key),
                Vec::new(),
            );
            return Err(err);
        }
        let insert_rollback = {
            let mut main = self.main_arena.borrow_mut();
            let (arena, start) = main.as_mut().unwrap();
            insert_string(arena, *start, val, registered.next_arc.clone())
        };
        if let Err(err) = self.check_main_arena_budget(budget) {
            self.rollback_direct_main_arena_insert(
                snapshot,
                Some(registered.transition_key),
                vec![insert_rollback],
            );
            return Err(err);
        }

        Ok(registered.next_fm)
    }

    /// Add a numeric transition that supports Q-number matching.
    /// Builds both a string FA for the text representation and a Q-number FA.
    fn add_numeric_transition(
        &self,
        num: f64,
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        // Mark that this matcher has numeric patterns
        let old_has_numbers = self.has_numbers.get();
        self.has_numbers.set(true);

        let val_str = num.to_string();
        let val = val_str.as_bytes();

        // Get Q-number representation
        let q_num = crate::numbits::q_num_from_f64(num);

        let snapshot = self.begin_direct_main_arena_insert();
        let registered = self.new_registered_transition();

        if let Err(err) = self.ensure_main_arena_with_singleton(budget) {
            self.rollback_direct_main_arena_insert(
                snapshot,
                Some(registered.transition_key),
                Vec::new(),
            );
            self.has_numbers.set(old_has_numbers);
            return Err(err);
        }
        let mut insert_rollbacks = Vec::with_capacity(2);
        {
            let mut main = self.main_arena.borrow_mut();
            let (arena, start) = main.as_mut().unwrap();
            insert_rollbacks.push(insert_string(
                arena,
                *start,
                val,
                registered.next_arc.clone(),
            ));
            insert_rollbacks.push(insert_string(
                arena,
                *start,
                &q_num,
                registered.next_arc.clone(),
            ));
        }
        if let Err(err) = self.check_main_arena_budget(budget) {
            self.rollback_direct_main_arena_insert(
                snapshot,
                Some(registered.transition_key),
                insert_rollbacks,
            );
            self.has_numbers.set(old_has_numbers);
            return Err(err);
        }

        Ok(registered.next_fm)
    }

    fn add_prefix_transition(
        &self,
        prefix: &[u8],
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let registered = self.new_registered_transition();

        let (new_arena, new_start) = make_prefix_arena_fa(prefix, registered.next_arc.clone());
        self.merge_registered_with_singleton(registered, new_arena, new_start, budget, false)
    }

    fn add_shellstyle_transition(
        &self,
        pattern: &[u8],
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let registered = self.new_registered_transition();

        let (new_arena, new_start) = make_shellstyle_arena_fa(pattern, registered.next_arc.clone());
        self.merge_registered_with_singleton(registered, new_arena, new_start, budget, true)
    }

    /// Add a suffix pattern using a reversed DFA trie.
    ///
    /// Builds reversed bytes: `['"', reversed(suffix)]` (closing quote + reversed suffix).
    /// Inserts into a separate `suffix_arena` that is traversed backward at match time.
    /// This is O(max_suffix_len) instead of the O(value_len * NFA_states) shellstyle approach.
    fn add_suffix_transition(
        &self,
        suffix: &str,
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let main_snapshot = self.begin_direct_main_arena_insert();

        // If there's a pending singleton, fold it into main_arena first
        // so transition_on doesn't short-circuit past suffix_arena
        if self.singleton_match.borrow().is_some()
            && let Some(singleton) = self.take_singleton_as_arena()
        {
            self.merge_taken_singleton_into_main(singleton, budget)?;
        }

        let registered = self.new_registered_transition();

        // Build reversed suffix bytes: closing quote + reversed suffix
        let suffix_bytes = suffix.as_bytes();
        let mut reversed = Vec::with_capacity(suffix_bytes.len() + 1);
        reversed.push(b'"'); // closing JSON quote (first byte when scanning backward)
        reversed.extend(suffix_bytes.iter().rev());

        // Insert into suffix arena (separate DFA trie from main_arena)
        let had_suffix_arena = self.suffix_arena.borrow().is_some();
        let insert_rollback = {
            let mut suffix_arena = self.suffix_arena.borrow_mut();
            if let Some((ref mut arena, start)) = *suffix_arena {
                Some(insert_suffix(
                    arena,
                    start,
                    &reversed,
                    registered.next_arc.clone(),
                ))
            } else {
                let (arena, start) = make_suffix_dfa(&reversed, registered.next_arc.clone());
                *suffix_arena = Some((arena, start));
                None
            }
        };

        if let Err(err) = self.check_suffix_arena_budget(budget) {
            self.rollback_suffix_arena_insert(had_suffix_arena, insert_rollback);
            self.rollback_direct_main_arena_insert(
                main_snapshot,
                Some(registered.transition_key),
                Vec::new(),
            );
            return Err(err);
        }

        Ok(registered.next_fm)
    }

    fn add_wildcard_transition(
        &self,
        pattern: &[u8],
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let registered = self.new_registered_transition();

        let (new_arena, new_start) = make_wildcard_arena_fa(pattern, registered.next_arc.clone());
        self.merge_registered_with_singleton(registered, new_arena, new_start, budget, true)
    }

    fn add_anything_but_transition(
        &self,
        excluded: &[Vec<u8>],
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let registered = self.new_registered_transition();

        let (new_arena, new_start) =
            make_anything_but_arena_fa(excluded, registered.next_arc.clone());
        self.merge_registered_with_singleton(registered, new_arena, new_start, budget, false)
    }

    /// Add a numeric anything-but transition using Q-number FA.
    ///
    /// Matches any numeric value NOT in the excluded list.
    /// Values are compared using Q-number representation for proper numeric ordering.
    fn add_anything_but_numeric_transition(
        &self,
        excluded: &[f64],
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let registered = self.new_registered_transition();

        let excluded_q_nums: Vec<Vec<u8>> = excluded
            .iter()
            .map(|&n| crate::numbits::q_num_from_f64(n))
            .collect();

        let (new_arena, new_start) =
            make_anything_but_arena_fa(&excluded_q_nums, registered.next_arc.clone());
        self.merge_registered_with_singleton(registered, new_arena, new_start, budget, false)
    }

    fn add_monocase_transition(
        &self,
        val: &[u8],
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let registered = self.new_registered_transition();

        let (new_arena, new_start) = make_monocase_arena_fa(val, registered.next_arc.clone());
        self.merge_registered_with_singleton(registered, new_arena, new_start, budget, false)
    }

    fn add_regexp_transition(
        &self,
        tree: &crate::regexp::RegexpRoot,
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let next_fm = Rc::new(MutableFieldMatcher::new());

        let (arena, start, field_matcher_arc) = make_regexp_nfa_arena(tree.clone());
        let marks_main_arena_nfa = arena.is_nondeterministic();
        let registered = self.register_transition(next_fm, field_matcher_arc);

        self.merge_registered_with_singleton(registered, arena, start, budget, marks_main_arena_nfa)
    }

    /// Add a multi-condition transition for lookaround patterns.
    ///
    /// Multi-condition patterns have a primary pattern plus conditions (lookarounds):
    /// - Primary pattern is built as an arena NFA
    /// - Condition automata are built for verification during matching
    ///
    /// For lookahead: condition stores combined pattern (AB), build automaton directly.
    /// For lookbehind: condition stores B, combine with primary (A) to get BA.
    fn add_multi_condition_transition(
        &self,
        mc: &crate::json::MultiConditionPattern,
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        use crate::json::LookaroundCondition;

        let next_fm = Rc::new(MutableFieldMatcher::new());

        // Build primary pattern automaton (with quote transitions for field values).
        // A lookbehind runs ahead of the primary and a trailing lookahead runs past
        // it, so those stretches of the value are the conditions' to account for.
        let has_lookbehind = mc.conditions.iter().any(LookaroundCondition::is_lookbehind);
        let primary_verify =
            build_primary_verify_pattern(&mc.primary, has_lookbehind, mc.trailing_lookahead);
        let (primary_arena, primary_start, field_matcher_arc) =
            make_regexp_nfa_arena(primary_verify);
        let registered = self.register_transition(next_fm, field_matcher_arc);

        // Build condition automata
        let mut condition_nfas = Vec::new();

        for condition in &mc.conditions {
            let (combined_pattern, is_negative) = match condition {
                LookaroundCondition::PositiveLookahead(pattern) => {
                    // Lookahead already stores the combined pattern (primary + lookahead)
                    (pattern.clone(), false)
                }
                LookaroundCondition::NegativeLookahead(pattern) => {
                    // Same as positive, but negative check
                    (pattern.clone(), true)
                }
                LookaroundCondition::PositiveLookbehind { pattern, .. } => {
                    // Lookbehind stores just the prefix pattern, combine with primary
                    // (?<=foo)bar: pattern="foo", primary="bar" -> combined="foobar"
                    let combined = build_lookbehind_combined_pattern(
                        pattern,
                        &mc.primary,
                        mc.trailing_lookahead,
                    );
                    (combined, false)
                }
                LookaroundCondition::NegativeLookbehind { pattern, .. } => {
                    // Same as positive, but negative check
                    let combined = build_lookbehind_combined_pattern(
                        pattern,
                        &mc.primary,
                        mc.trailing_lookahead,
                    );
                    (combined, true)
                }
            };

            // Build automaton for the combined pattern (with quote transitions)
            let (arena, start, _) = make_regexp_nfa_arena(combined_pattern);
            condition_nfas.push(ConditionNfa {
                arena,
                start,
                is_negative,
            });
        }

        // Check budget for the primary arena and all condition arenas
        if let Err(err) = Self::check_budget(primary_arena.estimated_byte_size(), budget) {
            self.remove_transition(registered.transition_key);
            return Err(err);
        }
        for cond in &condition_nfas {
            if let Err(err) = Self::check_budget(cond.arena.estimated_byte_size(), budget) {
                self.remove_transition(registered.transition_key);
                return Err(err);
            }
        }

        // Store in multi_condition_nfas for condition verification during matching
        self.multi_condition_nfas
            .borrow_mut()
            .push(MultiConditionNfa {
                primary_arena,
                primary_start,
                field_matcher_ptr: registered.transition_key,
                conditions: condition_nfas,
            });

        Ok(registered.next_fm)
    }

    /// Add a numeric range transition using arena-based FA for better performance.
    ///
    /// For two-sided ranges (e.g., >= 5, < 100), we build a combined arena FA.
    /// For single-sided ranges (e.g., < 100), we build the relevant arena FA.
    /// Multiple numeric patterns are merged into main_arena using merge_arena_nfas.
    fn add_numeric_range_transition(
        &self,
        cmp: &crate::json::NumericComparison,
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let registered = self.new_registered_transition();

        // Build the arena FA based on the comparison operators
        let (new_arena, new_start) = match (&cmp.lower, &cmp.upper) {
            (Some((lower_incl, lower_val)), Some((upper_incl, upper_val))) => {
                // Two-sided range: build a combined arena FA
                make_numeric_range_arena_fa(
                    *lower_val,
                    *lower_incl,
                    *upper_val,
                    *upper_incl,
                    registered.next_arc.clone(),
                )
            }
            (Some((incl, val)), None) => {
                // Lower bound only: >= or >
                make_numeric_greater_arena_fa(*val, *incl, registered.next_arc.clone())
            }
            (None, Some((incl, val))) => {
                // Upper bound only: <= or <
                make_numeric_less_arena_fa(*val, *incl, registered.next_arc.clone())
            }
            (None, None) => {
                // No bounds specified - match any number
                // This shouldn't happen in practice
                self.remove_transition(registered.transition_key);
                return Ok(registered.next_fm);
            }
        };

        self.merge_registered_with_singleton(registered, new_arena, new_start, budget, false)
    }

    /// Add a CIDR pattern transition using automaton-based IP matching.
    ///
    /// Builds an arena-based FA that matches IP addresses in the specified CIDR range.
    fn add_cidr_transition(
        &self,
        cidr: &crate::json::CidrPattern,
        budget: usize,
    ) -> Result<Rc<MutableFieldMatcher<X>>, crate::QuaminaError> {
        let registered = self.new_registered_transition();

        let (new_arena, new_start) = make_cidr_arena_fa(cidr, registered.next_arc.clone());
        self.merge_registered_with_singleton(registered, new_arena, new_start, budget, true)
    }

    /// Transition on a value during matching
    pub fn transition_on(
        &self,
        value: &[u8],
        is_number: bool,
        _bufs: &mut NfaBuffers,
    ) -> Vec<Rc<MutableFieldMatcher<X>>> {
        // Singleton fast path: when no multi-condition NFAs coexist with the singleton,
        // we can short-circuit without touching transition_map.
        if self.multi_condition_nfas.borrow().is_empty()
            && let Some(ref singleton_val) = *self.singleton_match.borrow()
        {
            if singleton_val == value
                && let Some(ref trans) = *self.singleton_transition.borrow()
            {
                return vec![trans.clone()];
            }
            return vec![];
        }

        let transition_map = self.transition_map.borrow();
        let mut result = Vec::new();

        // Singleton coexisting with multi-condition NFAs — emit the singleton
        // transition here, but skip main/suffix arenas (which are empty in that mode).
        let has_singleton = if let Some(ref singleton_val) = *self.singleton_match.borrow() {
            if singleton_val == value
                && let Some(ref trans) = *self.singleton_transition.borrow()
            {
                result.push(trans.clone());
            }
            true
        } else {
            false
        };

        let q_num_storage = self.maybe_q_number(value, is_number);
        let value_to_match: &[u8] = match &q_num_storage {
            Some(q) => q.as_slice(),
            None => value,
        };

        if !has_singleton {
            self.collect_arena_transitions(value_to_match, &transition_map, &mut result);
        }

        self.collect_multi_condition_transitions(value_to_match, &transition_map, &mut result);

        result
    }

    /// Convert a numeric value to its Q-number representation when this matcher contains
    /// numeric automata, so byte-level traversal works on numeric ranges.
    fn maybe_q_number(
        &self,
        value: &[u8],
        is_number: bool,
    ) -> Option<crate::numbits::QNumberStack> {
        if !(self.has_numbers.get() && is_number) {
            return None;
        }
        let s = std::str::from_utf8(value).ok()?;
        let n = s.parse::<f64>().ok()?;
        Some(crate::numbits::q_num_stack(n))
    }

    /// Run main_arena (NFA or DFA) and suffix_arena (backward DFA), pushing each
    /// matched field-matcher into `result`.
    fn collect_arena_transitions(
        &self,
        value_to_match: &[u8],
        transition_map: &FxHashMap<*const FieldMatcher, Rc<MutableFieldMatcher<X>>>,
        result: &mut Vec<Rc<MutableFieldMatcher<X>>>,
    ) {
        if let Some((ref arena, start)) = *self.main_arena.borrow() {
            let mut arena_bufs = self.arena_bufs.borrow_mut();
            if *self.main_arena_is_nfa.borrow() {
                traverse_arena_nfa(arena, start, value_to_match, &mut arena_bufs);
            } else {
                arena_bufs.transitions.clear();
                traverse_arena_dfa(arena, start, value_to_match, &mut arena_bufs.transitions);
            }

            for &ptr in &arena_bufs.transitions {
                if let Some(mutable_fm) = transition_map.get(&(ptr as *const FieldMatcher)) {
                    result.push(mutable_fm.clone());
                }
            }
        }

        if let Some((ref arena, start)) = *self.suffix_arena.borrow() {
            let mut arena_bufs = self.arena_bufs.borrow_mut();
            arena_bufs.transitions.clear();
            traverse_arena_dfa_backward(arena, start, value_to_match, &mut arena_bufs.transitions);

            for &ptr in &arena_bufs.transitions {
                if let Some(mutable_fm) = transition_map.get(&(ptr as *const FieldMatcher)) {
                    result.push(mutable_fm.clone());
                }
            }
        }
    }

    /// Verify each multi-condition NFA against the full value (for lookaround patterns)
    /// and emit its field matcher when every condition passes.
    fn collect_multi_condition_transitions(
        &self,
        value_to_match: &[u8],
        transition_map: &FxHashMap<*const FieldMatcher, Rc<MutableFieldMatcher<X>>>,
        result: &mut Vec<Rc<MutableFieldMatcher<X>>>,
    ) {
        let multi_condition_nfas = self.multi_condition_nfas.borrow();
        if multi_condition_nfas.is_empty() {
            return;
        }

        let mut condition_bufs = self.arena_bufs.borrow_mut();

        for mc_nfa in multi_condition_nfas.iter() {
            // The assertions qualify a match, they don't stand in for one: unless
            // the value matches the primary there is nothing to qualify.
            traverse_arena_nfa(
                &mc_nfa.primary_arena,
                mc_nfa.primary_start,
                value_to_match,
                &mut condition_bufs,
            );
            if condition_bufs.transitions.is_empty() {
                continue;
            }

            let mut all_conditions_pass = true;

            for condition in &mc_nfa.conditions {
                traverse_arena_nfa(
                    &condition.arena,
                    condition.start,
                    value_to_match,
                    &mut condition_bufs,
                );

                let condition_matched = !condition_bufs.transitions.is_empty();
                let condition_passes = if condition.is_negative {
                    !condition_matched
                } else {
                    condition_matched
                };

                if !condition_passes {
                    all_conditions_pass = false;
                    break;
                }
            }

            if all_conditions_pass {
                let ptr = mc_nfa.field_matcher_ptr;
                if let Some(mutable_fm) = transition_map.get(&ptr) {
                    // Avoid duplicates if the same field matcher came in via multiple paths.
                    if !result.iter().any(|r| Rc::ptr_eq(r, mutable_fm)) {
                        result.push(mutable_fm.clone());
                    }
                }
            }
        }
    }
}

/// An event field for matching (simplified version of json::Field)
#[derive(Clone, Debug)]
pub struct EventField {
    pub path: String,
    pub value: String,
    pub array_trail: Vec<crate::json::ArrayPos>,
    /// True if the value is a JSON number (for Q-number conversion during matching)
    pub is_number: bool,
}

impl From<&crate::json::Field> for EventField {
    fn from(f: &crate::json::Field) -> Self {
        Self {
            path: f.path.clone(),
            value: f.value.clone(),
            array_trail: f.array_trail.clone(),
            is_number: f.is_number,
        }
    }
}

/// Zero-copy event field for matching.
/// Borrows path and value bytes directly from the flattened fields.
#[derive(Clone, Debug)]
pub struct EventFieldRef<'a> {
    /// Path as a string slice (converted from bytes)
    pub path: &'a str,
    /// Value as raw bytes
    pub value: &'a [u8],
    /// Array position tracking (borrowed slice)
    pub array_trail: &'a [crate::flatten_json::ArrayPos],
    /// True if the value is a JSON number
    pub is_number: bool,
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

/// A set of matches (deduplicated)
struct MatchSet<X: Clone + Eq + std::hash::Hash> {
    seen: FxHashSet<X>,
    matches: Vec<X>,
}

impl<X: Clone + Eq + std::hash::Hash> MatchSet<X> {
    fn new() -> Self {
        Self {
            seen: FxHashSet::default(),
            matches: Vec::new(),
        }
    }

    fn add(&mut self, x: X) {
        if !self.seen.contains(&x) {
            self.seen.insert(x.clone());
            self.matches.push(x);
        }
    }

    fn into_vec(self) -> Vec<X> {
        self.matches
    }
}

/// Core matcher that uses automaton-based matching for multiple fields.
///
/// This implements the Go quamina matching algorithm:
/// 1. Patterns are added by building a graph of FieldMatcher -> MutableValueMatcher -> FieldMatcher
/// 2. Event fields are sorted and matched against the automaton
/// 3. Matching recursively tries subsequent fields to find complete pattern matches
#[derive(Default)]
pub struct CoreMatcher<X: Clone + Eq + std::hash::Hash> {
    /// Root field matcher - the start state of the automaton
    root: Rc<MutableFieldMatcher<X>>,
    /// Arena byte budget for pattern complexity limiting (0 = unlimited).
    arena_byte_budget: usize,
}

impl<X: Clone + Eq + std::hash::Hash> CoreMatcher<X> {
    /// Create a new CoreMatcher with default arena budget (10 MB).
    #[must_use]
    pub fn new() -> Self {
        Self {
            root: Rc::new(MutableFieldMatcher::new()),
            arena_byte_budget: crate::PatternLimits::default().arena_byte_budget,
        }
    }

    /// Add a pattern with the given identifier.
    ///
    /// The pattern_fields should be a list of (path, matchers) tuples.
    /// Fields are automatically sorted by path for matching.
    pub fn add_pattern(
        &self,
        x: X,
        pattern_fields: &[(String, Vec<crate::json::Matcher>)],
    ) -> Result<(), crate::QuaminaError> {
        // Sort fields lexically by path (like Go)
        let mut sorted_fields: Vec<_> = pattern_fields.to_vec();
        sorted_fields.sort_by(|a, b| a.0.cmp(&b.0));

        // Start with the root state
        let mut states: Vec<Rc<MutableFieldMatcher<X>>> = vec![self.root.clone()];

        for (path, matchers) in &sorted_fields {
            if matchers.is_empty() {
                continue;
            }

            let mut next_states = Vec::new();

            for state in &states {
                // Check for exists patterns
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
                        // Value matcher transition
                        let nexts = state.add_transition(path, matchers, self.arena_byte_budget)?;
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
        Ok(())
    }

    /// Match fields against patterns and return matching pattern identifiers.
    ///
    /// Fields should already be sorted by path.
    #[must_use]
    pub fn matches_for_fields(&self, fields: &[EventField]) -> Vec<X> {
        if fields.is_empty() {
            // Still need to check exists:false patterns
            return Self::collect_exists_false_matches(&self.root);
        }

        let mut matches = MatchSet::new();
        TL_MATCH_BUFS.with(|bufs_cell| {
            let mut bufs = bufs_cell.borrow_mut();
            bufs.clear(); // Reset buffers for reuse

            // For each field, try to match from the start state
            for i in 0..fields.len() {
                self.try_to_match(fields, i, &self.root, &mut matches, &mut bufs);
            }
        });

        matches.into_vec()
    }

    /// Recursively try to match fields starting from the given index and state
    fn try_to_match(
        &self,
        fields: &[EventField],
        index: usize,
        state: &Rc<MutableFieldMatcher<X>>,
        matches: &mut MatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        let field = &fields[index];

        // Check exists:true transition
        if let Some(exists_trans) = state.exists_true.borrow().get(&field.path) {
            // Add matches from this state
            for m in exists_trans.matches.borrow().iter() {
                matches.add(m.clone());
            }
            // Try subsequent fields
            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict(&field.array_trail, &fields[next_idx].array_trail) {
                    self.try_to_match(fields, next_idx, exists_trans, matches, bufs);
                }
            }
            // Check exists:false at end
            self.check_exists_false(state, fields, index, matches, bufs);
        }

        // Check exists:false (field doesn't exist)
        self.check_exists_false(state, fields, index, matches, bufs);

        // Try value transitions
        let next_states =
            state.transition_on(&field.path, field.value.as_bytes(), field.is_number, bufs);

        for next_state in next_states {
            // Add matches from next state
            for m in next_state.matches.borrow().iter() {
                matches.add(m.clone());
            }

            // Try subsequent fields
            for next_idx in (index + 1)..fields.len() {
                if no_array_trail_conflict(&field.array_trail, &fields[next_idx].array_trail) {
                    self.try_to_match(fields, next_idx, &next_state, matches, bufs);
                }
            }

            // Check exists:false at end
            self.check_exists_false(&next_state, fields, index, matches, bufs);
        }
    }

    /// Check exists:false patterns - field must NOT exist
    fn check_exists_false(
        &self,
        state: &Rc<MutableFieldMatcher<X>>,
        fields: &[EventField],
        index: usize,
        matches: &mut MatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        for (path, exists_trans) in state.exists_false.borrow().iter() {
            // Check if this path exists in the fields
            let field_exists = fields
                .binary_search_by(|f| f.path.as_str().cmp(path.as_str()))
                .is_ok();

            if !field_exists {
                // Field doesn't exist - exists:false matches
                for m in exists_trans.matches.borrow().iter() {
                    matches.add(m.clone());
                }
                // Continue matching from this state
                self.try_to_match(fields, index, exists_trans, matches, bufs);
            }
        }
    }

    /// Collect matches from exists:false patterns when there are no fields.
    fn collect_exists_false_matches(state: &Rc<MutableFieldMatcher<X>>) -> Vec<X> {
        let mut result = Vec::new();
        for exists_trans in state.exists_false.borrow().values() {
            result.extend(exists_trans.matches.borrow().iter().cloned());
        }
        result
    }

    /// Match fields against patterns using zero-copy field references.
    ///
    /// Fields should already be sorted by path.
    /// The `bufs` parameter should be a reusable NfaBuffers instance for reduced allocations.
    pub fn matches_for_fields_ref(
        &self,
        fields: &[EventFieldRef<'_>],
        bufs: &mut NfaBuffers,
    ) -> Vec<X> {
        if fields.is_empty() {
            return Self::collect_exists_false_matches(&self.root);
        }

        let mut matches = MatchSet::new();
        bufs.clear(); // Reset buffers for reuse

        for i in 0..fields.len() {
            self.try_to_match_ref(fields, i, &self.root, &mut matches, bufs);
        }

        matches.into_vec()
    }

    /// Recursively try to match fields (zero-copy version)
    fn try_to_match_ref(
        &self,
        fields: &[EventFieldRef<'_>],
        index: usize,
        state: &Rc<MutableFieldMatcher<X>>,
        matches: &mut MatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        let field = &fields[index];

        // Check exists:true transition
        if let Some(exists_trans) = state.exists_true.borrow().get(field.path) {
            for m in exists_trans.matches.borrow().iter() {
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
            for m in next_state.matches.borrow().iter() {
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

    /// Check exists:false patterns (zero-copy version)
    fn check_exists_false_ref(
        &self,
        state: &Rc<MutableFieldMatcher<X>>,
        fields: &[EventFieldRef<'_>],
        index: usize,
        matches: &mut MatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        for (path, exists_trans) in state.exists_false.borrow().iter() {
            let field_exists = fields
                .binary_search_by(|f| f.path.cmp(path.as_str()))
                .is_ok();

            if !field_exists {
                for m in exists_trans.matches.borrow().iter() {
                    matches.add(m.clone());
                }
                self.try_to_match_ref(fields, index, exists_trans, matches, bufs);
            }
        }
    }

    /// Match fields against patterns using flattened fields directly.
    ///
    /// This avoids the intermediate EventFieldRef allocation by working
    /// directly with flatten_json::Field. Fields should already be sorted by path.
    pub fn matches_for_fields_direct(
        &self,
        fields: &[crate::flatten_json::Field<'_>],
        bufs: &mut NfaBuffers,
    ) -> Vec<X> {
        if fields.is_empty() {
            return Self::collect_exists_false_matches(&self.root);
        }

        let mut matches = MatchSet::new();
        bufs.clear();

        for i in 0..fields.len() {
            self.try_to_match_direct(fields, i, &self.root, &mut matches, bufs);
        }

        matches.into_vec()
    }

    /// Recursively try to match fields (direct Field version)
    fn try_to_match_direct(
        &self,
        fields: &[crate::flatten_json::Field<'_>],
        index: usize,
        state: &Rc<MutableFieldMatcher<X>>,
        matches: &mut MatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        let field = &fields[index];
        let path = field.path_str();
        let value = field.value_bytes();
        let array_trail = field.array_trail_slice();

        // Check exists:true transition
        if let Some(exists_trans) = state.exists_true.borrow().get(path) {
            for m in exists_trans.matches.borrow().iter() {
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
            for m in next_state.matches.borrow().iter() {
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

    /// Check exists:false patterns (direct Field version)
    fn check_exists_false_direct(
        &self,
        state: &Rc<MutableFieldMatcher<X>>,
        fields: &[crate::flatten_json::Field<'_>],
        index: usize,
        matches: &mut MatchSet<X>,
        bufs: &mut NfaBuffers,
    ) {
        for (path, exists_trans) in state.exists_false.borrow().iter() {
            let field_exists = fields
                .binary_search_by(|f| f.path.as_ref().cmp(path.as_bytes()))
                .is_ok();

            if !field_exists {
                for m in exists_trans.matches.borrow().iter() {
                    matches.add(m.clone());
                }
                self.try_to_match_direct(fields, index, exists_trans, matches, bufs);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::automaton::arena::merge_arena_nfas;
    use crate::json::Matcher;
    use crate::regexp::parse_regexp;

    #[test]
    fn test_value_matcher_regexp_with_plus() {
        // Test that MutableValueMatcher correctly uses arena for regexp with +
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();

        // Create a regexp pattern with + quantifier
        let regexp_tree = parse_regexp("[a-z]+@example~.com").unwrap();
        let matcher = Matcher::ParsedRegexp(regexp_tree);

        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        // Verify arena was used (all patterns now use main_arena)
        assert!(
            vm.main_arena.borrow().is_some(),
            "main_arena should be set for regexp"
        );
        // Test matching
        let mut bufs = NfaBuffers::new();
        let value = qv(b"alice@example.com");
        let results = vm.transition_on(&value, false, &mut bufs);

        assert_eq!(
            results.len(),
            1,
            "Should match 'alice@example.com', got {} results",
            results.len()
        );
        assert!(
            Rc::ptr_eq(&results[0], &next_fm),
            "Should return the next field matcher"
        );

        // Test non-matching
        bufs.clear();
        let no_match_value = qv(b"alice@exampleXcom");
        let no_results = vm.transition_on(&no_match_value, false, &mut bufs);
        assert!(
            no_results.is_empty(),
            "Should not match 'alice@exampleXcom'"
        );
    }

    #[test]
    fn test_value_matcher_regexp_without_plus() {
        // Test that MutableValueMatcher uses arena for all regexp patterns
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();

        // Create a regexp pattern without + or * quantifier
        let regexp_tree = parse_regexp("[abc]").unwrap();
        let matcher = Matcher::ParsedRegexp(regexp_tree);

        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        // Verify arena was used (all patterns now use main_arena)
        assert!(
            vm.main_arena.borrow().is_some(),
            "main_arena should be set for regexp"
        );
        // Test matching
        let mut bufs = NfaBuffers::new();
        let value = qv(b"a");
        let results = vm.transition_on(&value, false, &mut bufs);

        assert_eq!(results.len(), 1, "Should match 'a'");
        assert!(
            Rc::ptr_eq(&results[0], &next_fm),
            "Should return the next field matcher"
        );
    }

    #[test]
    fn test_core_matcher_with_arena_regexp() {
        // Test the full CoreMatcher path with a regexp pattern using arena
        let cm: CoreMatcher<String> = CoreMatcher::new();

        // Parse the pattern like Quamina would
        let pattern_json = r#"{"email": [{"regex": "[a-z]+@example~.com"}]}"#;
        let pattern =
            crate::json::parse_pattern(pattern_json, &crate::PatternLimits::default()).unwrap();
        let pattern_vec: Vec<_> = pattern.into_iter().collect();

        cm.add_pattern("p1".to_string(), &pattern_vec).unwrap();

        // Create a field like the flattener would (strings retain JSON quotes)
        let fields = vec![EventField {
            path: "email".to_string(),
            value: "\"alice@example.com\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];

        let matches = cm.matches_for_fields(&fields);
        assert_eq!(matches, vec!["p1".to_string()], "Should match the pattern");

        // Test non-match
        let fields_no_match = vec![EventField {
            path: "email".to_string(),
            value: "\"alice@exampleXcom\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];

        let no_matches = cm.matches_for_fields(&fields_no_match);
        assert!(no_matches.is_empty(), "Should not match");
    }

    #[test]
    fn test_core_matcher_direct_with_arena_regexp() {
        // Test matches_for_fields_direct specifically (the path used by Quamina::matches_for_event)
        use std::sync::Arc;

        let cm: CoreMatcher<String> = CoreMatcher::new();

        // Parse the pattern like Quamina would
        let pattern_json = r#"{"email": [{"regex": "[a-z]+@example~.com"}]}"#;
        let pattern =
            crate::json::parse_pattern(pattern_json, &crate::PatternLimits::default()).unwrap();
        let pattern_vec: Vec<_> = pattern.into_iter().collect();

        cm.add_pattern("p1".to_string(), &pattern_vec).unwrap();

        // Create fields like matches_for_fields_direct expects (strings retain JSON quotes)
        let fields = vec![crate::flatten_json::Field {
            path: Arc::from(b"email".as_slice()),
            val: crate::flatten_json::FieldValue::Borrowed(b"\"alice@example.com\""),
            array_trail: [].as_slice().into(),
            is_number: false,
        }];

        let mut bufs = NfaBuffers::new();
        let matches = cm.matches_for_fields_direct(&fields, &mut bufs);
        assert_eq!(
            matches,
            vec!["p1".to_string()],
            "Should match the pattern via matches_for_fields_direct"
        );
    }

    /// Helper: wrap a byte slice in quotes to simulate flattener output for strings.
    /// The flattener preserves JSON quotes on string values, so test values
    /// passed to `transition_on` must include them.
    fn qv(s: &[u8]) -> Vec<u8> {
        let mut v = Vec::with_capacity(s.len() + 2);
        v.push(b'"');
        v.extend_from_slice(s);
        v.push(b'"');
        v
    }

    // =========================================================================
    // Integration tests for Arena-Only Migration (Step 2.3)
    // These tests verify behavior that must remain unchanged after migration
    // =========================================================================

    #[test]
    fn test_arena_migration_string_single() {
        // Test single exact string match
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        // Matcher::Exact for strings contains quoted value (like json.rs value_to_string produces)
        let matcher = Matcher::Exact("\"hello\"".to_string());
        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        let mut bufs = NfaBuffers::new();

        // Should match (quoted, like flattener output for strings)
        let results = vm.transition_on(&qv(b"hello"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &next_fm));

        // Should not match
        bufs.clear();
        let results = vm.transition_on(&qv(b"world"), false, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_string_multiple() {
        // Test multiple exact string matches
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();

        // Matcher::Exact for strings contains quoted values (like json.rs value_to_string produces)
        let fm1 = vm
            .add_transition(&Matcher::Exact("\"foo\"".to_string()), 0)
            .unwrap();
        let fm2 = vm
            .add_transition(&Matcher::Exact("\"bar\"".to_string()), 0)
            .unwrap();
        let fm3 = vm
            .add_transition(&Matcher::Exact("\"baz\"".to_string()), 0)
            .unwrap();

        let mut bufs = NfaBuffers::new();

        // Each should match (quoted)
        let results = vm.transition_on(&qv(b"foo"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &fm1));

        bufs.clear();
        let results = vm.transition_on(&qv(b"bar"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &fm2));

        bufs.clear();
        let results = vm.transition_on(&qv(b"baz"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &fm3));

        // None should match
        bufs.clear();
        let results = vm.transition_on(&qv(b"qux"), false, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_prefix() {
        // Test prefix pattern
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let matcher = Matcher::Prefix("hello".to_string());
        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        let mut bufs = NfaBuffers::new();

        // Should match any string starting with "hello" (quoted)
        let results = vm.transition_on(&qv(b"hello"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &next_fm));

        bufs.clear();
        let results = vm.transition_on(&qv(b"helloworld"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        bufs.clear();
        let results = vm.transition_on(&qv(b"hello123"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        // Should not match
        bufs.clear();
        let results = vm.transition_on(&qv(b"hell"), false, &mut bufs);
        assert!(results.is_empty());

        bufs.clear();
        let results = vm.transition_on(&qv(b"world"), false, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_shellstyle() {
        // Test shellstyle wildcard pattern
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let matcher = Matcher::Shellstyle("hello*world".to_string());
        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        let mut bufs = NfaBuffers::new();

        // Should match (quoted)
        let results = vm.transition_on(&qv(b"helloworld"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &next_fm));

        bufs.clear();
        let results = vm.transition_on(&qv(b"hello_world"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        bufs.clear();
        let results = vm.transition_on(&qv(b"hello123world"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        // Should not match
        bufs.clear();
        let results = vm.transition_on(&qv(b"helloworl"), false, &mut bufs);
        assert!(results.is_empty());

        bufs.clear();
        let results = vm.transition_on(&qv(b"worldhello"), false, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_wildcard_escape() {
        // Test wildcard with escape sequences
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        // Pattern "foo\\*bar" should match literal "foo*bar"
        let matcher = Matcher::Wildcard("foo\\*bar".to_string());
        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        let mut bufs = NfaBuffers::new();

        // Should match literal * (quoted)
        let results = vm.transition_on(&qv(b"foo*bar"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &next_fm));

        // Should not match without *
        bufs.clear();
        let results = vm.transition_on(&qv(b"foobar"), false, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_anything_but() {
        // Test anything-but pattern
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let matcher = Matcher::AnythingBut(vec!["foo".to_string(), "bar".to_string()]);
        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        let mut bufs = NfaBuffers::new();

        // Should match anything except foo and bar (quoted)
        let results = vm.transition_on(&qv(b"baz"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &next_fm));

        bufs.clear();
        let results = vm.transition_on(&qv(b"qux"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        // Should not match excluded values
        bufs.clear();
        let results = vm.transition_on(&qv(b"foo"), false, &mut bufs);
        assert!(results.is_empty());

        bufs.clear();
        let results = vm.transition_on(&qv(b"bar"), false, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_monocase() {
        // Test case-insensitive matching
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let matcher = Matcher::EqualsIgnoreCase("Hello".to_string());
        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        let mut bufs = NfaBuffers::new();

        // Should match any case combination (quoted)
        let results = vm.transition_on(&qv(b"Hello"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &next_fm));

        bufs.clear();
        let results = vm.transition_on(&qv(b"hello"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        bufs.clear();
        let results = vm.transition_on(&qv(b"HELLO"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        bufs.clear();
        let results = vm.transition_on(&qv(b"hElLo"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        // Should not match different strings
        bufs.clear();
        let results = vm.transition_on(&qv(b"world"), false, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_numeric_range() {
        use crate::json::NumericComparison;

        // Test numeric range patterns
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();

        // Match 10 <= x < 100
        let cmp = NumericComparison {
            lower: Some((true, 10.0)),   // >= 10
            upper: Some((false, 100.0)), // < 100
        };
        let matcher = Matcher::Numeric(cmp);
        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        let mut bufs = NfaBuffers::new();

        // Should match values in range (as numbers)
        let results = vm.transition_on(b"10", true, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &next_fm));

        bufs.clear();
        let results = vm.transition_on(b"50", true, &mut bufs);
        assert_eq!(results.len(), 1);

        bufs.clear();
        let results = vm.transition_on(b"99", true, &mut bufs);
        assert_eq!(results.len(), 1);

        // Should not match values outside range
        bufs.clear();
        let results = vm.transition_on(b"9", true, &mut bufs);
        assert!(results.is_empty());

        bufs.clear();
        let results = vm.transition_on(b"100", true, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_mixed_patterns() {
        // Test mixing different pattern types in same value matcher
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();

        // Add various pattern types
        // Exact values contain quotes (like json.rs value_to_string produces for strings)
        let fm_exact = vm
            .add_transition(&Matcher::Exact("\"exact\"".to_string()), 0)
            .unwrap();
        let fm_prefix = vm
            .add_transition(&Matcher::Prefix("pre".to_string()), 0)
            .unwrap();
        let fm_shell = vm
            .add_transition(&Matcher::Shellstyle("*wild*".to_string()), 0)
            .unwrap();

        let mut bufs = NfaBuffers::new();

        // Test exact match (quoted like flattener output)
        let results = vm.transition_on(&qv(b"exact"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &fm_exact));

        // Test prefix match (quoted)
        bufs.clear();
        let results = vm.transition_on(&qv(b"prefix_value"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &fm_prefix));

        // Test shellstyle match (quoted)
        bufs.clear();
        let results = vm.transition_on(&qv(b"something_wild_here"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &fm_shell));

        // Test value matching multiple patterns (quoted)
        bufs.clear();
        let results = vm.transition_on(&qv(b"prewild"), false, &mut bufs);
        // Should match both prefix and shellstyle
        assert!(!results.is_empty());
    }

    // MIRI SKIP RATIONALE: CIDR /24 pattern construction + 4 IP traversals takes ~46s under
    // Miri. Coverage: test_cidr_arena_fa_ipv4_exact and test_cidr_arena_fa_ipv4_range
    // exercise the same arena CIDR construction/matching at the arena level.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_arena_migration_cidr_v4() {
        use crate::json::CidrPattern;

        // Test IPv4 CIDR pattern
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let cidr = CidrPattern::V4 {
            network: [192, 168, 1, 0],
            prefix_len: 24,
        };
        let matcher = Matcher::Cidr(cidr);
        let next_fm = vm.add_transition(&matcher, 0).unwrap();

        let mut bufs = NfaBuffers::new();

        // Should match IPs in the /24 range (quoted, like flattener output for strings)
        let results = vm.transition_on(&qv(b"192.168.1.1"), false, &mut bufs);
        assert_eq!(results.len(), 1);
        assert!(Rc::ptr_eq(&results[0], &next_fm));

        bufs.clear();
        let results = vm.transition_on(&qv(b"192.168.1.255"), false, &mut bufs);
        assert_eq!(results.len(), 1);

        // Should not match IPs outside the range
        bufs.clear();
        let results = vm.transition_on(&qv(b"192.168.2.1"), false, &mut bufs);
        assert!(results.is_empty());

        bufs.clear();
        let results = vm.transition_on(&qv(b"10.0.0.1"), false, &mut bufs);
        assert!(results.is_empty());
    }

    #[test]
    fn test_arena_migration_core_matcher_all_types() {
        // End-to-end test with CoreMatcher using various pattern types
        let cm: CoreMatcher<String> = CoreMatcher::new();

        // Add patterns of different types
        // Matcher::Exact for strings contains quoted values (like json.rs value_to_string produces)
        cm.add_pattern(
            "exact".to_string(),
            &[(
                "field".to_string(),
                vec![Matcher::Exact("\"hello\"".to_string())],
            )],
        )
        .unwrap();
        cm.add_pattern(
            "prefix".to_string(),
            &[(
                "field".to_string(),
                vec![Matcher::Prefix("pre".to_string())],
            )],
        )
        .unwrap();
        cm.add_pattern(
            "shell".to_string(),
            &[(
                "field".to_string(),
                vec![Matcher::Shellstyle("*wild*".to_string())],
            )],
        )
        .unwrap();

        // Test exact match (string values include quotes like flattener output)
        let fields = vec![EventField {
            path: "field".to_string(),
            value: "\"hello\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        let matches = cm.matches_for_fields(&fields);
        assert!(matches.contains(&"exact".to_string()));

        // Test prefix match
        let fields = vec![EventField {
            path: "field".to_string(),
            value: "\"prefix_value\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        let matches = cm.matches_for_fields(&fields);
        assert!(matches.contains(&"prefix".to_string()));

        // Test shellstyle match
        let fields = vec![EventField {
            path: "field".to_string(),
            value: "\"something_wild_here\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        let matches = cm.matches_for_fields(&fields);
        assert!(matches.contains(&"shell".to_string()));
    }

    #[test]
    fn test_match_set_dedup() {
        let mut ms = MatchSet::<String>::new();
        ms.add("a".to_string());
        ms.add("b".to_string());
        ms.add("a".to_string()); // duplicate
        ms.add("c".to_string());
        ms.add("b".to_string()); // duplicate

        let result = ms.into_vec();
        assert_eq!(result.len(), 3);
        assert_eq!(result, vec!["a", "b", "c"]);
    }

    // =========================================================================
    // Mutation coverage: array trail conflicts, exists:false, multi-field
    // matching across all three CoreMatcher code paths
    // =========================================================================

    /// Helper: create a CoreMatcher with a two-field pattern (level=high AND status=active).
    fn cm_two_field() -> CoreMatcher<String> {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[
                (
                    "level".to_string(),
                    vec![Matcher::Exact("\"high\"".to_string())],
                ),
                (
                    "status".to_string(),
                    vec![Matcher::Exact("\"active\"".to_string())],
                ),
            ],
        )
        .unwrap();
        cm
    }

    /// Helper: create a CoreMatcher with an exists:false pattern.
    fn cm_exists_false() -> CoreMatcher<String> {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[("gone".to_string(), vec![Matcher::Exists(false)])],
        )
        .unwrap();
        cm
    }

    // -- matches_for_fields (owned EventField) --

    #[test]
    fn test_core_matcher_array_trail_conflict() {
        use crate::json::ArrayPos;
        let cm = cm_two_field();

        // Same array, different pos → conflict → no match
        let conflicting = vec![
            EventField {
                path: "level".to_string(),
                value: "\"high\"".to_string(),
                array_trail: vec![ArrayPos { array: 1, pos: 0 }],
                is_number: false,
            },
            EventField {
                path: "status".to_string(),
                value: "\"active\"".to_string(),
                array_trail: vec![ArrayPos { array: 1, pos: 1 }],
                is_number: false,
            },
        ];
        assert!(cm.matches_for_fields(&conflicting).is_empty());

        // Same array, same pos → no conflict → match
        let compatible = vec![
            EventField {
                path: "level".to_string(),
                value: "\"high\"".to_string(),
                array_trail: vec![ArrayPos { array: 1, pos: 0 }],
                is_number: false,
            },
            EventField {
                path: "status".to_string(),
                value: "\"active\"".to_string(),
                array_trail: vec![ArrayPos { array: 1, pos: 0 }],
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields(&compatible), vec!["p1"]);
    }

    #[test]
    fn test_core_matcher_multi_field_owned() {
        let cm = cm_two_field();

        // Both fields present → match
        let fields = vec![
            EventField {
                path: "level".to_string(),
                value: "\"high\"".to_string(),
                array_trail: vec![],
                is_number: false,
            },
            EventField {
                path: "status".to_string(),
                value: "\"active\"".to_string(),
                array_trail: vec![],
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields(&fields), vec!["p1"]);

        // Only one field → no match (catches index + 1 → index * 1)
        let single = vec![EventField {
            path: "status".to_string(),
            value: "\"active\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert!(cm.matches_for_fields(&single).is_empty());
    }

    // -- matches_for_fields_ref (borrowed EventFieldRef) --

    #[test]
    fn test_core_matcher_ref_multi_field() {
        let cm = cm_two_field();
        let mut bufs = NfaBuffers::new();

        let fields = vec![
            EventFieldRef {
                path: "level",
                value: b"\"high\"",
                array_trail: &[],
                is_number: false,
            },
            EventFieldRef {
                path: "status",
                value: b"\"active\"",
                array_trail: &[],
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields_ref(&fields, &mut bufs), vec!["p1"]);

        // Only one field → no match
        let single = vec![EventFieldRef {
            path: "status",
            value: b"\"active\"",
            array_trail: &[],
            is_number: false,
        }];
        assert!(cm.matches_for_fields_ref(&single, &mut bufs).is_empty());
    }

    #[test]
    fn test_core_matcher_ref_array_trail_conflict() {
        use crate::flatten_json::ArrayPos;
        let cm = cm_two_field();
        let mut bufs = NfaBuffers::new();

        let trail_a = [ArrayPos { array: 1, pos: 0 }];
        let trail_b = [ArrayPos { array: 1, pos: 1 }];

        // Conflict → no match
        let conflicting = vec![
            EventFieldRef {
                path: "level",
                value: b"\"high\"",
                array_trail: &trail_a,
                is_number: false,
            },
            EventFieldRef {
                path: "status",
                value: b"\"active\"",
                array_trail: &trail_b,
                is_number: false,
            },
        ];
        assert!(
            cm.matches_for_fields_ref(&conflicting, &mut bufs)
                .is_empty()
        );

        // Compatible → match
        let compatible = vec![
            EventFieldRef {
                path: "level",
                value: b"\"high\"",
                array_trail: &trail_a,
                is_number: false,
            },
            EventFieldRef {
                path: "status",
                value: b"\"active\"",
                array_trail: &trail_a,
                is_number: false,
            },
        ];
        assert_eq!(
            cm.matches_for_fields_ref(&compatible, &mut bufs),
            vec!["p1"]
        );
    }

    #[test]
    fn test_core_matcher_ref_exists_false() {
        let cm = cm_exists_false();
        let mut bufs = NfaBuffers::new();

        // Field absent → match
        let without = vec![EventFieldRef {
            path: "other",
            value: b"123",
            array_trail: &[],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields_ref(&without, &mut bufs), vec!["p1"]);

        // Field present → no match
        let with = vec![EventFieldRef {
            path: "gone",
            value: b"here",
            array_trail: &[],
            is_number: false,
        }];
        assert!(cm.matches_for_fields_ref(&with, &mut bufs).is_empty());
    }

    // -- matches_for_fields_direct (flatten_json::Field) --

    #[test]
    fn test_core_matcher_direct_multi_field() {
        use std::sync::Arc;
        let cm = cm_two_field();
        let mut bufs = NfaBuffers::new();

        let fields = vec![
            crate::flatten_json::Field {
                path: Arc::from(b"level".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"high\""),
                array_trail: [].as_slice().into(),
                is_number: false,
            },
            crate::flatten_json::Field {
                path: Arc::from(b"status".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"active\""),
                array_trail: [].as_slice().into(),
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields_direct(&fields, &mut bufs), vec!["p1"]);

        // Only one field → no match
        let single = vec![crate::flatten_json::Field {
            path: Arc::from(b"status".as_slice()),
            val: crate::flatten_json::FieldValue::Borrowed(b"\"active\""),
            array_trail: [].as_slice().into(),
            is_number: false,
        }];
        assert!(cm.matches_for_fields_direct(&single, &mut bufs).is_empty());
    }

    #[test]
    fn test_core_matcher_direct_array_trail_conflict() {
        use crate::flatten_json::ArrayPos;
        use std::sync::Arc;
        let cm = cm_two_field();
        let mut bufs = NfaBuffers::new();

        let trail_a: crate::flatten_json::ArrayTrailVec =
            [ArrayPos { array: 1, pos: 0 }].as_slice().into();
        let trail_b: crate::flatten_json::ArrayTrailVec =
            [ArrayPos { array: 1, pos: 1 }].as_slice().into();

        // Conflict → no match
        let conflicting = vec![
            crate::flatten_json::Field {
                path: Arc::from(b"level".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"high\""),
                array_trail: trail_a.clone(),
                is_number: false,
            },
            crate::flatten_json::Field {
                path: Arc::from(b"status".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"active\""),
                array_trail: trail_b,
                is_number: false,
            },
        ];
        assert!(
            cm.matches_for_fields_direct(&conflicting, &mut bufs)
                .is_empty()
        );

        // Compatible → match
        let compatible = vec![
            crate::flatten_json::Field {
                path: Arc::from(b"level".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"high\""),
                array_trail: trail_a.clone(),
                is_number: false,
            },
            crate::flatten_json::Field {
                path: Arc::from(b"status".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"active\""),
                array_trail: trail_a,
                is_number: false,
            },
        ];
        assert_eq!(
            cm.matches_for_fields_direct(&compatible, &mut bufs),
            vec!["p1"]
        );
    }

    /// Helper: create a CoreMatcher with a pattern requiring TWO fields with the
    /// same path: a=1 then a=1. This requires two distinct field occurrences
    /// (e.g., from an array). A single occurrence should NOT match.
    fn cm_same_field_twice() -> CoreMatcher<String> {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[
                ("a".to_string(), vec![Matcher::Exact("\"1\"".to_string())]),
                ("a".to_string(), vec![Matcher::Exact("\"1\"".to_string())]),
            ],
        )
        .unwrap();
        cm
    }

    #[test]
    fn test_core_matcher_no_self_match_owned() {
        let cm = cm_same_field_twice();

        // Single field should NOT match a pattern requiring two occurrences
        let single = vec![EventField {
            path: "a".to_string(),
            value: "\"1\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert!(
            cm.matches_for_fields(&single).is_empty(),
            "single field must not self-match a two-condition pattern"
        );

        // Two fields should match
        let two = vec![
            EventField {
                path: "a".to_string(),
                value: "\"1\"".to_string(),
                array_trail: vec![],
                is_number: false,
            },
            EventField {
                path: "a".to_string(),
                value: "\"1\"".to_string(),
                array_trail: vec![],
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields(&two), vec!["p1"]);
    }

    #[test]
    fn test_core_matcher_no_self_match_ref() {
        let cm = cm_same_field_twice();
        let mut bufs = NfaBuffers::new();

        let single = vec![EventFieldRef {
            path: "a",
            value: b"\"1\"",
            array_trail: &[],
            is_number: false,
        }];
        assert!(
            cm.matches_for_fields_ref(&single, &mut bufs).is_empty(),
            "single field must not self-match"
        );

        let two = vec![
            EventFieldRef {
                path: "a",
                value: b"\"1\"",
                array_trail: &[],
                is_number: false,
            },
            EventFieldRef {
                path: "a",
                value: b"\"1\"",
                array_trail: &[],
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields_ref(&two, &mut bufs), vec!["p1"]);
    }

    #[test]
    fn test_core_matcher_no_self_match_direct() {
        use std::sync::Arc;
        let cm = cm_same_field_twice();
        let mut bufs = NfaBuffers::new();

        let single = vec![crate::flatten_json::Field {
            path: Arc::from(b"a".as_slice()),
            val: crate::flatten_json::FieldValue::Borrowed(b"\"1\""),
            array_trail: [].as_slice().into(),
            is_number: false,
        }];
        assert!(
            cm.matches_for_fields_direct(&single, &mut bufs).is_empty(),
            "single field must not self-match"
        );

        let two = vec![
            crate::flatten_json::Field {
                path: Arc::from(b"a".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"1\""),
                array_trail: [].as_slice().into(),
                is_number: false,
            },
            crate::flatten_json::Field {
                path: Arc::from(b"a".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"1\""),
                array_trail: [].as_slice().into(),
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields_direct(&two, &mut bufs), vec!["p1"]);
    }

    /// Helper: pattern `exists:true(a) AND a=1`. After the exists:true fires on
    /// field "a", the next state expects a value match on "a". A single-field
    /// event [a=1] should NOT satisfy both conditions because the SAME field
    /// occurrence must not be used twice.
    fn cm_exists_true_then_value() -> CoreMatcher<String> {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[
                ("a".to_string(), vec![Matcher::Exists(true)]),
                ("a".to_string(), vec![Matcher::Exact("\"1\"".to_string())]),
            ],
        )
        .unwrap();
        cm
    }

    #[test]
    fn test_core_matcher_exists_true_no_self_match_owned() {
        let cm = cm_exists_true_then_value();

        // Single a=1 should NOT match (exists:true consumes it, no second field)
        let single = vec![EventField {
            path: "a".to_string(),
            value: "\"1\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert!(
            cm.matches_for_fields(&single).is_empty(),
            "single field must not satisfy exists:true AND value on the same field"
        );

        // Two a=1 fields should match
        let two = vec![
            EventField {
                path: "a".to_string(),
                value: "\"1\"".to_string(),
                array_trail: vec![],
                is_number: false,
            },
            EventField {
                path: "a".to_string(),
                value: "\"1\"".to_string(),
                array_trail: vec![],
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields(&two), vec!["p1"]);
    }

    #[test]
    fn test_core_matcher_exists_true_no_self_match_ref() {
        let cm = cm_exists_true_then_value();
        let mut bufs = NfaBuffers::new();

        let single = vec![EventFieldRef {
            path: "a",
            value: b"\"1\"",
            array_trail: &[],
            is_number: false,
        }];
        assert!(
            cm.matches_for_fields_ref(&single, &mut bufs).is_empty(),
            "single field must not satisfy exists:true AND value"
        );

        let two = vec![
            EventFieldRef {
                path: "a",
                value: b"\"1\"",
                array_trail: &[],
                is_number: false,
            },
            EventFieldRef {
                path: "a",
                value: b"\"1\"",
                array_trail: &[],
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields_ref(&two, &mut bufs), vec!["p1"]);
    }

    #[test]
    fn test_core_matcher_exists_true_no_self_match_direct() {
        use std::sync::Arc;
        let cm = cm_exists_true_then_value();
        let mut bufs = NfaBuffers::new();

        let single = vec![crate::flatten_json::Field {
            path: Arc::from(b"a".as_slice()),
            val: crate::flatten_json::FieldValue::Borrowed(b"\"1\""),
            array_trail: [].as_slice().into(),
            is_number: false,
        }];
        assert!(
            cm.matches_for_fields_direct(&single, &mut bufs).is_empty(),
            "single field must not satisfy exists:true AND value"
        );

        let two = vec![
            crate::flatten_json::Field {
                path: Arc::from(b"a".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"1\""),
                array_trail: [].as_slice().into(),
                is_number: false,
            },
            crate::flatten_json::Field {
                path: Arc::from(b"a".as_slice()),
                val: crate::flatten_json::FieldValue::Borrowed(b"\"1\""),
                array_trail: [].as_slice().into(),
                is_number: false,
            },
        ];
        assert_eq!(cm.matches_for_fields_direct(&two, &mut bufs), vec!["p1"]);
    }

    #[test]
    fn test_core_matcher_direct_exists_false() {
        use std::sync::Arc;
        let cm = cm_exists_false();
        let mut bufs = NfaBuffers::new();

        // Field absent → match
        let without = vec![crate::flatten_json::Field {
            path: Arc::from(b"other".as_slice()),
            val: crate::flatten_json::FieldValue::Borrowed(b"123"),
            array_trail: [].as_slice().into(),
            is_number: false,
        }];
        assert_eq!(
            cm.matches_for_fields_direct(&without, &mut bufs),
            vec!["p1"]
        );

        // Field present → no match
        let with = vec![crate::flatten_json::Field {
            path: Arc::from(b"gone".as_slice()),
            val: crate::flatten_json::FieldValue::Borrowed(b"here"),
            array_trail: [].as_slice().into(),
            is_number: false,
        }];
        assert!(cm.matches_for_fields_direct(&with, &mut bufs).is_empty());
    }

    // =========================================================================
    // Mutation coverage: MutableValueMatcher::add_*_transition functions
    // =========================================================================

    #[test]
    fn test_add_string_transition_basic() {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[(
                "value".to_string(),
                vec![Matcher::Exact("\"hello\"".to_string())],
            )],
        )
        .unwrap();

        let fields = vec![EventField {
            path: "value".to_string(),
            value: "\"hello\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&fields), vec!["p1".to_string()]);

        // Wrong value → no match
        let wrong = vec![EventField {
            path: "value".to_string(),
            value: "\"world\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert!(cm.matches_for_fields(&wrong).is_empty());
    }

    #[test]
    fn test_add_string_transitions_bulk() {
        // Two exact strings in one field use bulk optimization
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[(
                "color".to_string(),
                vec![
                    Matcher::Exact("\"red\"".to_string()),
                    Matcher::Exact("\"blue\"".to_string()),
                ],
            )],
        )
        .unwrap();

        // Both values should match
        let red = vec![EventField {
            path: "color".to_string(),
            value: "\"red\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&red), vec!["p1".to_string()]);

        let blue = vec![EventField {
            path: "color".to_string(),
            value: "\"blue\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&blue), vec!["p1".to_string()]);

        // Different value → no match
        let green = vec![EventField {
            path: "color".to_string(),
            value: "\"green\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert!(cm.matches_for_fields(&green).is_empty());
    }

    #[test]
    fn test_add_numeric_transition() {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[("count".to_string(), vec![Matcher::NumericExact(42.0)])],
        )
        .unwrap();

        // Exact numeric match
        let exact = vec![EventField {
            path: "count".to_string(),
            value: "42".to_string(),
            array_trail: vec![],
            is_number: true,
        }];
        assert_eq!(cm.matches_for_fields(&exact), vec!["p1".to_string()]);

        // Different number → no match
        let different = vec![EventField {
            path: "count".to_string(),
            value: "43".to_string(),
            array_trail: vec![],
            is_number: true,
        }];
        assert!(cm.matches_for_fields(&different).is_empty());
    }

    #[test]
    fn test_add_prefix_transition() {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Prefix("active".to_string())],
            )],
        )
        .unwrap();

        // Prefix match
        let matches_prefix = vec![EventField {
            path: "status".to_string(),
            value: "\"active-now\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(
            cm.matches_for_fields(&matches_prefix),
            vec!["p1".to_string()]
        );

        // No prefix match
        let no_prefix = vec![EventField {
            path: "status".to_string(),
            value: "\"inactive\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert!(cm.matches_for_fields(&no_prefix).is_empty());
    }

    #[test]
    fn test_add_suffix_transition() {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[(
                "path".to_string(),
                vec![Matcher::Suffix("json".to_string())],
            )],
        )
        .unwrap();

        // Suffix match
        let matches_suffix = vec![EventField {
            path: "path".to_string(),
            value: "\"data.json\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(
            cm.matches_for_fields(&matches_suffix),
            vec!["p1".to_string()]
        );

        // No suffix match
        let no_suffix = vec![EventField {
            path: "path".to_string(),
            value: "\"data.txt\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert!(cm.matches_for_fields(&no_suffix).is_empty());
    }

    #[test]
    fn test_add_wildcard_transition() {
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[(
                "name".to_string(),
                vec![Matcher::Wildcard("*ello".to_string())],
            )],
        )
        .unwrap();

        // Wildcard match (anything ending with "ello")
        let matches = vec![EventField {
            path: "name".to_string(),
            value: "\"hello\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&matches), vec!["p1".to_string()]);

        // No match
        let no_match = vec![EventField {
            path: "name".to_string(),
            value: "\"world\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert!(cm.matches_for_fields(&no_match).is_empty());
    }

    #[test]
    fn test_add_string_transition_singleton_opt() {
        // Test singleton optimization: first string stays as singleton
        let mvm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let next = mvm.add_string_transition(b"hello", 0).unwrap();
        let next2 = mvm.add_string_transition(b"hello", 0).unwrap();
        // Same singleton - should return same next state
        assert!(std::ptr::eq(Rc::as_ptr(&next), Rc::as_ptr(&next2)));
    }

    #[test]
    fn test_add_numeric_has_numbers_flag() {
        // Adding a numeric transition should set has_numbers flag
        let mvm: MutableValueMatcher<String> = MutableValueMatcher::new();
        assert!(!mvm.has_numbers.get());
        mvm.add_numeric_transition(42.0, 0).unwrap();
        assert!(
            mvm.has_numbers.get(),
            "numeric flag should be set after adding numeric"
        );
    }

    #[test]
    fn test_maybe_q_number_requires_both_has_numbers_and_is_number() {
        // A Q-number stack is built only when the matcher already has numeric
        // transitions and the value is numeric.
        let mvm: MutableValueMatcher<String> = MutableValueMatcher::new();

        // No numeric transitions yet
        assert!(mvm.maybe_q_number(b"123", true).is_none());
        assert!(mvm.maybe_q_number(b"abc", false).is_none());

        mvm.has_numbers.set(true);

        // Now gated only on the value being numeric
        assert!(mvm.maybe_q_number(b"abc", false).is_none());
        assert!(mvm.maybe_q_number(b"123", true).is_some());
    }

    #[test]
    fn test_add_transition_all_exact_returns_one_shared_next_state() {
        // An all-Exact list of two or more collapses into a single shared
        // continuation instead of one per matcher.
        let fm: Rc<MutableFieldMatcher<String>> = Rc::new(MutableFieldMatcher::new());
        let matchers = vec![
            Matcher::Exact("\"a\"".to_string()),
            Matcher::Exact("\"b\"".to_string()),
            Matcher::Exact("\"c\"".to_string()),
        ];
        assert_eq!(fm.add_transition("p", &matchers, 0).unwrap().len(), 1);

        let fm2: Rc<MutableFieldMatcher<String>> = Rc::new(MutableFieldMatcher::new());
        let two = vec![
            Matcher::Exact("\"x\"".to_string()),
            Matcher::Exact("\"y\"".to_string()),
        ];
        assert_eq!(fm2.add_transition("p", &two, 0).unwrap().len(), 1);

        // A single Exact still yields exactly one continuation.
        let fm3: Rc<MutableFieldMatcher<String>> = Rc::new(MutableFieldMatcher::new());
        let one = vec![Matcher::Exact("\"only\"".to_string())];
        assert_eq!(fm3.add_transition("p", &one, 0).unwrap().len(), 1);
    }

    #[test]
    fn test_add_transition_mixed_exact_and_numeric_keeps_numeric_path() {
        // A non-Exact matcher in the list keeps everything on the one-by-one
        // path, so the numeric matcher survives.
        let fm: Rc<MutableFieldMatcher<String>> = Rc::new(MutableFieldMatcher::new());
        let matchers = vec![
            Matcher::Exact("\"a\"".to_string()),
            Matcher::NumericExact(42.0),
        ];
        assert_eq!(fm.add_transition("p", &matchers, 0).unwrap().len(), 2);

        let transitions = fm.transitions.borrow();
        let vm = transitions.get("p").expect("value matcher present");
        assert!(vm.has_numbers.get());
    }

    #[test]
    fn test_add_prefix_multiple_patterns() {
        // Multiple prefix patterns on same field
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[("verb".to_string(), vec![Matcher::Prefix("re".to_string())])],
        )
        .unwrap();
        cm.add_pattern(
            "p2".to_string(),
            &[("verb".to_string(), vec![Matcher::Prefix("un".to_string())])],
        )
        .unwrap();

        let re_match = vec![EventField {
            path: "verb".to_string(),
            value: "\"replace\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&re_match), vec!["p1".to_string()]);

        let un_match = vec![EventField {
            path: "verb".to_string(),
            value: "\"undo\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&un_match), vec!["p2".to_string()]);
    }

    #[test]
    fn test_add_suffix_multiple_patterns() {
        // Multiple suffix patterns
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[("ext".to_string(), vec![Matcher::Suffix("log".to_string())])],
        )
        .unwrap();
        cm.add_pattern(
            "p2".to_string(),
            &[("ext".to_string(), vec![Matcher::Suffix("txt".to_string())])],
        )
        .unwrap();

        let log_match = vec![EventField {
            path: "ext".to_string(),
            value: "\"app.log\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&log_match), vec!["p1".to_string()]);

        let txt_match = vec![EventField {
            path: "ext".to_string(),
            value: "\"readme.txt\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&txt_match), vec!["p2".to_string()]);
    }

    // =========================================================================
    // Mutation coverage: quote_wrap, check_budget, merge_into_main_arena,
    // take_singleton_as_arena
    // =========================================================================

    #[test]
    fn test_quote_wrap_via_wildcard() {
        // quote_wrap is used internally by wildcard patterns.
        // Mutating push(b'"') to vec![] would break wildcard matching.
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[(
                "name".to_string(),
                vec![Matcher::Wildcard("h*o".to_string())],
            )],
        )
        .unwrap();

        let matches = vec![EventField {
            path: "name".to_string(),
            value: "\"hello\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&matches), vec!["p1".to_string()]);
    }

    #[test]
    fn test_quote_wrap_via_shellstyle() {
        // quote_wrap also used in shellstyle patterns.
        let cm: CoreMatcher<String> = CoreMatcher::new();
        cm.add_pattern(
            "p1".to_string(),
            &[(
                "file".to_string(),
                vec![Matcher::Shellstyle("*.log".to_string())],
            )],
        )
        .unwrap();

        let matches = vec![EventField {
            path: "file".to_string(),
            value: "\"app.log\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&matches), vec!["p1".to_string()]);
    }

    #[test]
    fn test_check_budget_via_patterns() {
        // check_budget is exercised whenever patterns are added.
        // Default budget is 10 MB, should be plenty for test patterns.
        let cm = CoreMatcher::<String>::new();
        let result = cm.add_pattern(
            "p1".to_string(),
            &[(
                "field".to_string(),
                vec![Matcher::Exact("\"value\"".to_string())],
            )],
        );
        assert!(
            result.is_ok(),
            "simple pattern should pass default budget check"
        );

        // Another pattern to exercise multiple check_budget calls
        let result2 = cm.add_pattern(
            "p2".to_string(),
            &[(
                "other".to_string(),
                vec![Matcher::Exact("\"data\"".to_string())],
            )],
        );
        assert!(result2.is_ok(), "second pattern should also pass budget");
    }

    #[test]
    fn test_merge_into_main_arena_via_patterns() {
        // merge_into_main_arena is called when adding multiple non-string patterns.
        // It checks the budget before and after merging.
        let cm = CoreMatcher::<String>::new();

        // First pattern: wildcard (uses merge_with_singleton)
        let r1 = cm.add_pattern(
            "p1".to_string(),
            &[(
                "x".to_string(),
                vec![Matcher::Wildcard("*ello".to_string())],
            )],
        );
        assert!(r1.is_ok(), "first wildcard should succeed");

        // Second pattern: another wildcard (merges into main_arena)
        let r2 = cm.add_pattern(
            "p2".to_string(),
            &[(
                "x".to_string(),
                vec![Matcher::Wildcard("w*rld".to_string())],
            )],
        );
        assert!(r2.is_ok(), "second wildcard should merge successfully");

        // Verify first pattern still works after merge
        let hello = vec![EventField {
            path: "x".to_string(),
            value: "\"hello\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        let matches = cm.matches_for_fields(&hello);
        assert_eq!(
            matches,
            vec!["p1"],
            "first pattern should match after merge"
        );

        // Verify second pattern works
        let world = vec![EventField {
            path: "x".to_string(),
            value: "\"world\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        let matches2 = cm.matches_for_fields(&world);
        assert_eq!(
            matches2,
            vec!["p2"],
            "second pattern should match after merge"
        );
    }

    #[test]
    fn test_singleton_to_arena_transition() {
        // Test that singleton optimization is properly converted to arena when needed.
        // Pattern: first a string (uses singleton), then a wildcard (forces conversion).
        let cm = CoreMatcher::<String>::new();
        cm.add_pattern(
            "p1".to_string(),
            &[(
                "val".to_string(),
                vec![Matcher::Exact("\"hello\"".to_string())],
            )],
        )
        .unwrap();

        // Add wildcard on same field - should force singleton → arena conversion
        cm.add_pattern(
            "p2".to_string(),
            &[(
                "val".to_string(),
                vec![Matcher::Wildcard("h*o".to_string())],
            )],
        )
        .unwrap();

        // Both patterns should now match via merged arena
        let hello = vec![EventField {
            path: "val".to_string(),
            value: "\"hello\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        let matches = cm.matches_for_fields(&hello);
        assert!(
            !matches.is_empty(),
            "singleton-to-arena merge should preserve matches"
        );
    }

    #[test]
    fn test_budget_enforcement_via_matching() {
        // Verify that patterns built successfully match correctly.
        // This exercises check_budget during add_pattern.
        let cm = CoreMatcher::<String>::new();

        // Add prefix pattern (exercises check_main_arena_budget after insertion)
        let result = cm.add_pattern(
            "p1".to_string(),
            &[("f".to_string(), vec![Matcher::Prefix("test".to_string())])],
        );
        assert!(
            result.is_ok(),
            "prefix pattern should be built successfully"
        );

        // Verify the pattern matches as expected
        let fields = vec![EventField {
            path: "f".to_string(),
            value: "\"testing\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }];
        assert_eq!(cm.matches_for_fields(&fields), vec!["p1".to_string()]);
    }

    #[test]
    fn test_take_singleton_idempotent() {
        // take_singleton_as_arena should return None on second call
        let mvm: MutableValueMatcher<String> = MutableValueMatcher::new();
        mvm.add_string_transition(b"test", 0).ok();

        // First call returns Some
        let first = mvm.take_singleton_as_arena();
        assert!(first.is_some(), "first call should return Some");

        // Singleton is now consumed, second call returns None
        let second = mvm.take_singleton_as_arena();
        assert!(
            second.is_none(),
            "second call should return None (singleton consumed)"
        );
    }

    #[test]
    fn test_check_budget_boundary_condition() {
        // Mutation: check_budget `>` mutated to `>=`
        // This test verifies that size == budget is allowed (not rejected).
        // We create multiple string patterns to exercise budget checking.
        let cm = CoreMatcher::<String>::new();

        // Add multiple patterns - each one calls check_budget
        for i in 0..5 {
            let result = cm.add_pattern(
                format!("p{i}"),
                &[(
                    "f".to_string(),
                    vec![Matcher::Exact(format!("\"pattern{i}\""))],
                )],
            );
            assert!(
                result.is_ok(),
                "pattern {i} should succeed with default budget"
            );
        }

        // Verify all patterns match
        for i in 0..5 {
            let fields = vec![EventField {
                path: "f".to_string(),
                value: format!("\"pattern{i}\""),
                array_trail: vec![],
                is_number: false,
            }];
            assert_eq!(
                cm.matches_for_fields(&fields),
                vec![format!("p{i}")],
                "pattern {} should match",
                i
            );
        }
    }

    #[test]
    fn test_merge_into_main_arena_boundary() {
        // Mutation: merge_into_main_arena check `>` mutated to `==`
        // This verifies that merging is allowed when result is within budget.
        let cm = CoreMatcher::<String>::new();

        // Add several patterns that will be merged
        for i in 0..3 {
            let result = cm.add_pattern(
                format!("p{i}"),
                &[("x".to_string(), vec![Matcher::Prefix(format!("prefix{i}"))])],
            );
            assert!(result.is_ok(), "pattern {i} merge should succeed");
        }

        // Verify merged patterns all work
        for i in 0..3 {
            let fields = vec![EventField {
                path: "x".to_string(),
                value: format!("\"prefix{i}_test\""),
                array_trail: vec![],
                is_number: false,
            }];
            let matches = cm.matches_for_fields(&fields);
            assert_eq!(
                matches,
                vec![format!("p{i}")],
                "merged pattern {} should match",
                i
            );
        }
    }

    #[test]
    fn test_lookbehind_combined_keeps_primary_alternation() {
        // When the primary pattern has top-level alternation, combining it with
        // a lookbehind must produce one branch per primary alternative rather
        // than a single branch (which would drop the alternatives).
        let lb = parse_regexp("a").unwrap();
        let primary = parse_regexp("x|y").unwrap();
        assert_eq!(primary.len(), 2);
        let combined = build_lookbehind_combined_pattern(&lb, &primary, false);
        assert_eq!(combined.len(), 2, "both primary alternatives must survive");
    }

    #[test]
    fn test_add_transition_mixed_with_prefix_keeps_prefix() {
        // Exact strings alongside a non-Exact (Prefix) matcher must go
        // one-by-one rather than down the all-exact bulk path, which would drop
        // the Prefix. The Prefix must still match its value.
        let fm: MutableFieldMatcher<String> = MutableFieldMatcher::new();
        let matchers = vec![
            Matcher::Exact("\"aa\"".to_string()),
            Matcher::Exact("\"bb\"".to_string()),
            Matcher::Prefix("cc".to_string()),
        ];
        fm.add_transition("x", &matchers, 0).unwrap();
        let mut bufs = NfaBuffers::new();
        let results = fm.transition_on("x", &qv(b"ccZ"), false, &mut bufs);
        assert!(!results.is_empty(), "Prefix matcher must not be dropped");
    }

    #[test]
    fn test_singleton_coexisting_with_multicondition() {
        // A value matcher holding both a singleton exact and a multi-condition
        // (lookaround) NFA must still emit the singleton transition for the
        // exact value, alongside any multi-condition matches.
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let exact_fm = vm
            .add_transition(&Matcher::Exact("\"hello\"".to_string()), 0)
            .unwrap();
        let pat = crate::json::parse_pattern(
            r#"{"f": [{"regexp": "(?=h)hello"}]}"#,
            &crate::PatternLimits::default(),
        )
        .unwrap();
        let mc_matcher = pat
            .into_values()
            .next()
            .unwrap()
            .into_iter()
            .next()
            .unwrap();
        vm.add_transition(&mc_matcher, 0).unwrap();
        assert!(!vm.multi_condition_nfas.borrow().is_empty());
        let mut bufs = NfaBuffers::new();
        let results = vm.transition_on(&qv(b"hello"), false, &mut bufs);
        assert!(
            results.iter().any(|r| Rc::ptr_eq(r, &exact_fm)),
            "singleton transition must be emitted alongside multi-condition NFAs"
        );
    }

    #[test]
    fn test_core_matcher_lookaround_conditions() {
        // Positive and negative lookahead must be verified against the value:
        // (?=bar) accepts "foobar" and (?!bar) accepts "foobaz".
        let cm: CoreMatcher<String> = CoreMatcher::new();
        let pos = crate::json::parse_pattern(
            r#"{"v": [{"regexp": "foo(?=bar)bar"}]}"#,
            &crate::PatternLimits::default(),
        )
        .unwrap();
        cm.add_pattern("pos".to_string(), &pos.into_iter().collect::<Vec<_>>())
            .unwrap();
        let neg = crate::json::parse_pattern(
            r#"{"w": [{"regexp": "foo(?!bar)baz"}]}"#,
            &crate::PatternLimits::default(),
        )
        .unwrap();
        cm.add_pattern("neg".to_string(), &neg.into_iter().collect::<Vec<_>>())
            .unwrap();

        let m = cm.matches_for_fields(&[EventField {
            path: "v".to_string(),
            value: "\"foobar\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }]);
        assert_eq!(m, vec!["pos".to_string()], "positive lookahead must match");

        let m = cm.matches_for_fields(&[EventField {
            path: "w".to_string(),
            value: "\"foobaz\"".to_string(),
            array_trail: vec![],
            is_number: false,
        }]);
        assert_eq!(m, vec!["neg".to_string()], "negative lookahead must match");
    }

    #[test]
    fn test_merge_into_main_arena_budget_boundary() {
        // The merge budget check rejects only when the merged size strictly
        // exceeds the live reachable arena budget. Compute the compacted live
        // size so this stays valid across append-history capacity changes.
        let p1 = crate::json::parse_pattern(
            r#"{"x": [{"regexp": "aaaaaaaa"}]}"#,
            &crate::PatternLimits::default(),
        )
        .unwrap();
        let p2 = crate::json::parse_pattern(
            r#"{"x": [{"regexp": "bbbbbbbb"}]}"#,
            &crate::PatternLimits::default(),
        )
        .unwrap();
        let p1v: Vec<_> = p1.into_iter().collect();
        let p2v: Vec<_> = p2.into_iter().collect();

        let probe = CoreMatcher::<String> {
            root: Rc::new(MutableFieldMatcher::new()),
            arena_byte_budget: 0,
        };
        probe.add_pattern("a".to_string(), &p1v).unwrap();
        probe.add_pattern("b".to_string(), &p2v).unwrap();
        let merged_size = {
            let transitions = probe.root.transitions.borrow();
            let vm = transitions.get("x").expect("value matcher should exist");
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            clone_arena_subset(arena, *start).0.estimated_byte_size()
        };

        let cm = CoreMatcher::<String> {
            root: Rc::new(MutableFieldMatcher::new()),
            arena_byte_budget: merged_size,
        };
        cm.add_pattern("a".to_string(), &p1v).unwrap();
        cm.add_pattern("b".to_string(), &p2v)
            .expect("merge at budget == merged size must succeed");

        let cm2 = CoreMatcher::<String> {
            root: Rc::new(MutableFieldMatcher::new()),
            arena_byte_budget: merged_size - 1,
        };
        cm2.add_pattern("a".to_string(), &p1v).unwrap();
        assert!(
            cm2.add_pattern("b".to_string(), &p2v).is_err(),
            "merge exceeding budget by one byte must be rejected"
        );
    }

    // MIRI SKIP RATIONALE: This uses a shellstyle merge and full transition
    // traversal to prove the native rollback shape. Coverage:
    // test_failed_append_merge_rollbacks_miri_friendly exercises the same
    // post-merge budget failure with a shorter pattern.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_failed_append_merge_rolls_back_main_arena_state() {
        let accepted_prefix = b"\"keepkeepkeepkeep";
        let rejected_shell = quote_wrap(b"reject*");

        let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
        probe
            .add_prefix_transition(accepted_prefix, 0)
            .expect("first pattern should build");
        let standalone_rejected_size = {
            let (arena, _) =
                make_shellstyle_arena_fa(&rejected_shell, Arc::new(FieldMatcher::new()));
            arena.estimated_byte_size()
        };
        probe
            .add_shellstyle_transition(&rejected_shell, 0)
            .expect("probe merge should build");
        let merged_size = {
            let main = probe.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            clone_arena_subset(arena, *start).0.estimated_byte_size()
        };
        let budget = merged_size - 1;
        assert!(
            standalone_rejected_size <= budget,
            "test must fail after append merge, not on standalone arena size"
        );

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm
            .add_prefix_transition(accepted_prefix, 0)
            .expect("first pattern should build");
        let (old_start, old_len, old_transition_count, old_nfa_flag) = {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (
                *start,
                arena.len(),
                vm.transition_map.borrow().len(),
                *vm.main_arena_is_nfa.borrow(),
            )
        };

        let err = match vm.add_shellstyle_transition(&rejected_shell, budget) {
            Ok(_) => panic!("second pattern should exceed the merge budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            assert_eq!(*start, old_start, "live start must be restored");
            assert_eq!(arena.len(), old_len, "appended states must be truncated");
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected transition key must be removed"
        );
        assert_eq!(
            *vm.main_arena_is_nfa.borrow(),
            old_nfa_flag,
            "NFA flag must be restored"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&qv(b"keepkeepkeepkeep-going"), false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "accepted pattern must still match"
        );
        let rejected = vm.transition_on(&qv(b"reject-me"), false, &mut bufs);
        assert!(
            rejected.is_empty(),
            "rejected pattern must not become visible"
        );
    }

    // MIRI SKIP RATIONALE: This builds both a singleton arena and a prefix arena
    // to cover native rollback details. Coverage:
    // test_failed_append_merge_rollbacks_miri_friendly keeps the append-merge
    // failure path live under Miri with smaller inputs.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_failed_append_merge_rolls_back_singleton_state() {
        let rejected_prefix = b"\"reject";

        let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
        probe.add_string_transition(&qv(b"keep"), 0).unwrap();
        let singleton_size = {
            let (arena, _) = make_string_arena_fa(&qv(b"keep"), Arc::new(FieldMatcher::new()));
            arena.estimated_byte_size()
        };
        let merged_size = {
            let (prefix_arena, prefix_start) =
                make_prefix_arena_fa(rejected_prefix, Arc::new(FieldMatcher::new()));
            let singleton = probe
                .take_singleton_as_arena()
                .expect("singleton should be consumed");
            let (merged, merged_start) = merge_arena_nfas(
                &singleton.arena,
                singleton.start,
                &prefix_arena,
                prefix_start,
            );
            clone_arena_subset(&merged, merged_start)
                .0
                .estimated_byte_size()
        };
        let budget = merged_size - 1;
        assert!(
            singleton_size <= budget,
            "test must fail after consuming the singleton"
        );

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_string_transition(&qv(b"keep"), 0).unwrap();
        assert_eq!(vm.transition_map.borrow().len(), 0);
        assert!(vm.main_arena.borrow().is_none());

        let err = match vm.add_prefix_transition(rejected_prefix, budget) {
            Ok(_) => panic!("merged singleton plus prefix should exceed budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        assert_eq!(
            vm.singleton_match.borrow().as_deref(),
            Some(qv(b"keep").as_slice()),
            "singleton value must be restored"
        );
        assert!(
            vm.singleton_transition
                .borrow()
                .as_ref()
                .is_some_and(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "singleton transition must be restored"
        );
        assert_eq!(
            vm.transition_map.borrow().len(),
            0,
            "singleton and rejected transition keys must be removed"
        );
        assert!(
            vm.main_arena.borrow().is_none(),
            "failed singleton merge must not install a main arena"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&qv(b"keep"), false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "restored singleton must still match"
        );
        assert!(
            vm.transition_on(&qv(b"rejecting"), false, &mut bufs)
                .is_empty(),
            "rejected prefix must not match"
        );
    }

    // MIRI SKIP RATIONALE: The 256-byte rejected value is intentionally large
    // enough to prove appended state truncation in native tests. Coverage:
    // test_failed_direct_string_rollbacks_miri_friendly covers the same rollback
    // path with short strings.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_failed_direct_string_insert_rolls_back_main_arena_state() {
        let accepted_values = [qv(b"keep"), qv(b"stay")];
        let accepted_refs: Vec<_> = accepted_values.iter().map(Vec::as_slice).collect();
        let rejected_raw = vec![b'x'; 256];
        let rejected = qv(&rejected_raw);

        let budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe
                .add_string_transitions_bulk(&accepted_refs, 0)
                .unwrap();
            let before_size = probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size();
            probe.add_string_transition(&rejected, 0).unwrap();
            let after_size = probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size();
            assert!(
                before_size < after_size,
                "test data must grow the arena budget"
            );
            after_size - 1
        };

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_string_transitions_bulk(&accepted_refs, 0).unwrap();
        let (old_start, old_len, old_transition_count) = {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (*start, arena.len(), vm.transition_map.borrow().len())
        };

        let err = match vm.add_string_transition(&rejected, budget) {
            Ok(_) => panic!("direct string insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            assert_eq!(*start, old_start, "live start must be restored");
            assert_eq!(arena.len(), old_len, "appended states must be removed");
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected transition key must be removed"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&accepted_values[0], false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "accepted direct string pattern must still match"
        );
        assert!(
            vm.transition_on(&rejected, false, &mut bufs).is_empty(),
            "rejected direct string pattern must not become visible"
        );
    }

    #[test]
    fn test_failed_existing_string_insert_rolls_back_terminal_transition() {
        let accepted_values = [qv(b"same"), qv(b"other")];
        let accepted_refs: Vec<_> = accepted_values.iter().map(Vec::as_slice).collect();

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_string_transitions_bulk(&accepted_refs, 0).unwrap();
        let (old_len, old_transition_count, budget) = {
            let main = vm.main_arena.borrow();
            let (arena, _) = main.as_ref().expect("main arena should exist");
            let size = arena.estimated_byte_size();
            assert!(size > 1, "test budget must be able to reject current arena");
            (arena.len(), vm.transition_map.borrow().len(), size - 1)
        };

        let err = match vm.add_string_transition(&accepted_values[0], budget) {
            Ok(_) => panic!("existing-path string insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        {
            let main = vm.main_arena.borrow();
            let (arena, _) = main.as_ref().expect("main arena should exist");
            assert_eq!(
                arena.len(),
                old_len,
                "existing-path insert must not change state count"
            );
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected terminal transition key must be removed"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&accepted_values[0], false, &mut bufs);
        assert_eq!(
            accepted.len(),
            1,
            "failed duplicate insert must not leave an extra terminal transition"
        );
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "original exact pattern must still match"
        );
    }

    // MIRI SKIP RATIONALE: The long rejected value forces singleton promotion
    // and rollback after a budget failure. Coverage:
    // test_failed_direct_string_rollbacks_miri_friendly covers singleton restore
    // with smaller inputs.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_failed_direct_string_insert_restores_singleton_state() {
        let accepted = qv(b"keep");
        let rejected_raw = vec![b'r'; 256];
        let rejected = qv(&rejected_raw);

        let budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe.add_string_transition(&accepted, 0).unwrap();
            let singleton_size = {
                let (arena, _) = make_string_arena_fa(&accepted, Arc::new(FieldMatcher::new()));
                arena.estimated_byte_size()
            };
            probe.add_string_transition(&rejected, 0).unwrap();
            let after_size = probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size();
            assert!(
                singleton_size < after_size,
                "test data must grow past the singleton arena"
            );
            after_size - 1
        };

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_string_transition(&accepted, 0).unwrap();
        assert!(vm.main_arena.borrow().is_none());
        assert_eq!(vm.transition_map.borrow().len(), 0);

        let err = match vm.add_string_transition(&rejected, budget) {
            Ok(_) => panic!("direct string insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        assert_eq!(
            vm.singleton_match.borrow().as_deref(),
            Some(accepted.as_slice()),
            "singleton value must be restored"
        );
        assert!(
            vm.singleton_transition
                .borrow()
                .as_ref()
                .is_some_and(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "singleton transition must be restored"
        );
        assert!(
            vm.main_arena.borrow().is_none(),
            "failed direct insert must not install main_arena"
        );
        assert_eq!(
            vm.transition_map.borrow().len(),
            0,
            "temporary transition keys must be removed"
        );

        let mut bufs = NfaBuffers::new();
        let accepted_match = vm.transition_on(&accepted, false, &mut bufs);
        assert!(
            accepted_match.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "restored singleton must still match"
        );
        assert!(
            vm.transition_on(&rejected, false, &mut bufs).is_empty(),
            "rejected direct string pattern must not become visible"
        );
    }

    // MIRI SKIP RATIONALE: The long rejected values pin rollback of multiple
    // appended trie branches in native tests. Coverage:
    // test_failed_direct_string_rollbacks_miri_friendly covers bulk insertion
    // rollback with short strings.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_failed_bulk_string_insert_rolls_back_main_arena_state() {
        let accepted_values = [qv(b"alpha"), qv(b"omega")];
        let accepted_refs: Vec<_> = accepted_values.iter().map(Vec::as_slice).collect();
        let rejected_values = [qv(&vec![b'a'; 256]), qv(&vec![b'b'; 320])];
        let rejected_refs: Vec<_> = rejected_values.iter().map(Vec::as_slice).collect();

        let budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe
                .add_string_transitions_bulk(&accepted_refs, 0)
                .unwrap();
            let before_size = probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size();
            probe
                .add_string_transitions_bulk(&rejected_refs, 0)
                .unwrap();
            let after_size = probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size();
            assert!(
                before_size < after_size,
                "test data must grow the arena budget"
            );
            after_size - 1
        };

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_string_transitions_bulk(&accepted_refs, 0).unwrap();
        let (old_start, old_len, old_transition_count) = {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (*start, arena.len(), vm.transition_map.borrow().len())
        };

        let err = match vm.add_string_transitions_bulk(&rejected_refs, budget) {
            Ok(_) => panic!("bulk string insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            assert_eq!(*start, old_start, "live start must be restored");
            assert_eq!(arena.len(), old_len, "appended states must be removed");
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected transition key must be removed"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&accepted_values[1], false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "accepted bulk string pattern must still match"
        );
        for rejected in &rejected_values {
            assert!(
                vm.transition_on(rejected, false, &mut bufs).is_empty(),
                "rejected bulk string pattern must not become visible"
            );
        }
    }

    // MIRI SKIP RATIONALE: The large numeric literal creates long textual and
    // Q-number paths to prove native arena truncation. Coverage:
    // test_failed_numeric_and_suffix_rollbacks_miri_friendly covers numeric
    // rollback and has_numbers restore with a short value.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_failed_numeric_exact_insert_rolls_back_main_arena_state() {
        let accepted_prefix = b"\"keep";
        let rejected_num = 123_456_789_012_345_670_000_000_000_000_f64;

        let budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe.add_prefix_transition(accepted_prefix, 0).unwrap();
            let before_size = probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size();
            probe.add_numeric_transition(rejected_num, 0).unwrap();
            let after_size = probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size();
            assert!(
                before_size < after_size,
                "test data must grow the arena budget"
            );
            after_size - 1
        };

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_prefix_transition(accepted_prefix, 0).unwrap();
        assert!(!vm.has_numbers.get());
        let (old_start, old_len, old_transition_count) = {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (*start, arena.len(), vm.transition_map.borrow().len())
        };

        let err = match vm.add_numeric_transition(rejected_num, budget) {
            Ok(_) => panic!("numeric insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            assert_eq!(*start, old_start, "live start must be restored");
            assert_eq!(arena.len(), old_len, "appended states must be removed");
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected transition key must be removed"
        );
        assert!(
            !vm.has_numbers.get(),
            "failed numeric exact add must restore the numeric flag"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&qv(b"keep-going"), false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "accepted prefix pattern must still match"
        );
        let rejected_value = rejected_num.to_string();
        assert!(
            vm.transition_on(rejected_value.as_bytes(), true, &mut bufs)
                .is_empty(),
            "rejected numeric exact pattern must not become visible"
        );
    }

    // MIRI SKIP RATIONALE: The 256-byte suffix proves suffix-arena truncation in
    // the native test suite. Coverage:
    // test_failed_numeric_and_suffix_rollbacks_miri_friendly keeps suffix
    // rollback covered under Miri with a short suffix.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_failed_suffix_insert_rolls_back_suffix_arena_state() {
        let accepted_suffix = "ok";
        let rejected_suffix = "x".repeat(256);

        let budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe.add_suffix_transition(accepted_suffix, 0).unwrap();
            let before_size = probe
                .suffix_arena
                .borrow()
                .as_ref()
                .expect("suffix arena should exist")
                .0
                .estimated_byte_size();
            probe.add_suffix_transition(&rejected_suffix, 0).unwrap();
            let after_size = probe
                .suffix_arena
                .borrow()
                .as_ref()
                .expect("suffix arena should exist")
                .0
                .estimated_byte_size();
            assert!(
                before_size < after_size,
                "test data must grow the suffix arena budget"
            );
            after_size - 1
        };

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_suffix_transition(accepted_suffix, 0).unwrap();
        let (old_start, old_len, old_transition_count) = {
            let suffix = vm.suffix_arena.borrow();
            let (arena, start) = suffix.as_ref().expect("suffix arena should exist");
            (*start, arena.len(), vm.transition_map.borrow().len())
        };

        let err = match vm.add_suffix_transition(&rejected_suffix, budget) {
            Ok(_) => panic!("suffix insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        {
            let suffix = vm.suffix_arena.borrow();
            let (arena, start) = suffix.as_ref().expect("suffix arena should exist");
            assert_eq!(*start, old_start, "suffix start must be restored");
            assert_eq!(
                arena.len(),
                old_len,
                "suffix appended states must be removed"
            );
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected suffix transition key must be removed"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&qv(b"file.ok"), false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "accepted suffix pattern must still match"
        );
        let rejected_value = qv(format!("file.{rejected_suffix}").as_bytes());
        assert!(
            vm.transition_on(&rejected_value, false, &mut bufs)
                .is_empty(),
            "rejected suffix pattern must not become visible"
        );
    }

    #[test]
    #[cfg(miri)]
    fn test_failed_append_merge_rollbacks_miri_friendly() {
        let accepted_prefix = b"\"k";
        let rejected_shell = quote_wrap(b"r*");

        let budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe
                .add_prefix_transition(accepted_prefix, 0)
                .expect("first pattern should build");
            probe
                .add_shellstyle_transition(&rejected_shell, 0)
                .expect("probe merge should build");
            let main = probe.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            clone_arena_subset(arena, *start).0.estimated_byte_size() - 1
        };

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm
            .add_prefix_transition(accepted_prefix, 0)
            .expect("first pattern should build");
        let (old_start, old_len, old_transition_count) = {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (*start, arena.len(), vm.transition_map.borrow().len())
        };

        let err = match vm.add_shellstyle_transition(&rejected_shell, budget) {
            Ok(_) => panic!("second pattern should exceed the merge budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            assert_eq!(*start, old_start, "live start must be restored");
            assert_eq!(arena.len(), old_len, "appended states must be truncated");
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected transition key must be removed"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&qv(b"keep"), false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "accepted pattern must still match"
        );
        assert!(
            vm.transition_on(&qv(b"reject"), false, &mut bufs)
                .is_empty(),
            "rejected pattern must not become visible"
        );
    }

    #[test]
    #[cfg(miri)]
    fn test_failed_direct_string_rollbacks_miri_friendly() {
        let accepted_values = [qv(b"a"), qv(b"b")];
        let accepted_refs: Vec<_> = accepted_values.iter().map(Vec::as_slice).collect();
        let rejected = qv(b"zz");

        let budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe
                .add_string_transitions_bulk(&accepted_refs, 0)
                .unwrap();
            probe.add_string_transition(&rejected, 0).unwrap();
            probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size()
                - 1
        };

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_string_transitions_bulk(&accepted_refs, 0).unwrap();
        let (old_start, old_len, old_transition_count) = {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (*start, arena.len(), vm.transition_map.borrow().len())
        };

        let err = match vm.add_string_transition(&rejected, budget) {
            Ok(_) => panic!("direct string insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));

        {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            assert_eq!(*start, old_start, "live start must be restored");
            assert_eq!(arena.len(), old_len, "appended states must be removed");
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected transition key must be removed"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&accepted_values[0], false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "accepted direct string pattern must still match"
        );
        assert!(
            vm.transition_on(&rejected, false, &mut bufs).is_empty(),
            "rejected direct string pattern must not become visible"
        );

        let singleton_accepted = qv(b"k");
        let singleton_rejected = qv(b"q");
        let singleton_budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe.add_string_transition(&singleton_accepted, 0).unwrap();
            probe.add_string_transition(&singleton_rejected, 0).unwrap();
            probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size()
                - 1
        };

        let singleton_vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let singleton_fm = singleton_vm
            .add_string_transition(&singleton_accepted, 0)
            .unwrap();
        let err = match singleton_vm.add_string_transition(&singleton_rejected, singleton_budget) {
            Ok(_) => panic!("direct string insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));
        assert_eq!(
            singleton_vm.singleton_match.borrow().as_deref(),
            Some(singleton_accepted.as_slice()),
            "singleton value must be restored"
        );
        assert!(
            singleton_vm
                .singleton_transition
                .borrow()
                .as_ref()
                .is_some_and(|fm| Rc::ptr_eq(fm, &singleton_fm)),
            "singleton transition must be restored"
        );
        assert!(
            singleton_vm.main_arena.borrow().is_none(),
            "failed singleton promotion must not install main_arena"
        );

        let rejected_values = [qv(b"xx"), qv(b"yy")];
        let rejected_refs: Vec<_> = rejected_values.iter().map(Vec::as_slice).collect();
        let bulk_budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe
                .add_string_transitions_bulk(&accepted_refs, 0)
                .unwrap();
            probe
                .add_string_transitions_bulk(&rejected_refs, 0)
                .unwrap();
            probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size()
                - 1
        };

        let bulk_vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        bulk_vm
            .add_string_transitions_bulk(&accepted_refs, 0)
            .unwrap();
        let (bulk_old_start, bulk_old_len, bulk_old_transition_count) = {
            let main = bulk_vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (*start, arena.len(), bulk_vm.transition_map.borrow().len())
        };
        let err = match bulk_vm.add_string_transitions_bulk(&rejected_refs, bulk_budget) {
            Ok(_) => panic!("bulk string insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));
        {
            let main = bulk_vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            assert_eq!(*start, bulk_old_start, "live start must be restored");
            assert_eq!(arena.len(), bulk_old_len, "appended states must be removed");
        }
        assert_eq!(
            bulk_vm.transition_map.borrow().len(),
            bulk_old_transition_count,
            "bulk rejected transition key must be removed"
        );
    }

    #[test]
    #[cfg(miri)]
    fn test_failed_numeric_and_suffix_rollbacks_miri_friendly() {
        let accepted_prefix = b"\"k";
        let rejected_num = 7.0;

        let budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe.add_prefix_transition(accepted_prefix, 0).unwrap();
            probe.add_numeric_transition(rejected_num, 0).unwrap();
            probe
                .main_arena
                .borrow()
                .as_ref()
                .expect("main arena should exist")
                .0
                .estimated_byte_size()
                - 1
        };

        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let accepted_fm = vm.add_prefix_transition(accepted_prefix, 0).unwrap();
        let (old_start, old_len, old_transition_count) = {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (*start, arena.len(), vm.transition_map.borrow().len())
        };
        let err = match vm.add_numeric_transition(rejected_num, budget) {
            Ok(_) => panic!("numeric insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));
        {
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            assert_eq!(*start, old_start, "live start must be restored");
            assert_eq!(arena.len(), old_len, "appended states must be removed");
        }
        assert_eq!(
            vm.transition_map.borrow().len(),
            old_transition_count,
            "rejected transition key must be removed"
        );
        assert!(
            !vm.has_numbers.get(),
            "failed numeric exact add must restore the numeric flag"
        );

        let mut bufs = NfaBuffers::new();
        let accepted = vm.transition_on(&qv(b"keep"), false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &accepted_fm)),
            "accepted prefix pattern must still match"
        );
        assert!(
            vm.transition_on(rejected_num.to_string().as_bytes(), true, &mut bufs)
                .is_empty(),
            "rejected numeric exact pattern must not become visible"
        );

        let suffix_budget = {
            let probe: MutableValueMatcher<String> = MutableValueMatcher::new();
            probe.add_suffix_transition("ok", 0).unwrap();
            probe.add_suffix_transition("xx", 0).unwrap();
            probe
                .suffix_arena
                .borrow()
                .as_ref()
                .expect("suffix arena should exist")
                .0
                .estimated_byte_size()
                - 1
        };

        let suffix_vm: MutableValueMatcher<String> = MutableValueMatcher::new();
        let suffix_fm = suffix_vm.add_suffix_transition("ok", 0).unwrap();
        let (suffix_old_start, suffix_old_len, suffix_old_transition_count) = {
            let suffix = suffix_vm.suffix_arena.borrow();
            let (arena, start) = suffix.as_ref().expect("suffix arena should exist");
            (*start, arena.len(), suffix_vm.transition_map.borrow().len())
        };
        let err = match suffix_vm.add_suffix_transition("xx", suffix_budget) {
            Ok(_) => panic!("suffix insertion should exceed the budget"),
            Err(err) => err,
        };
        assert!(matches!(err, crate::QuaminaError::PatternTooComplex(_)));
        {
            let suffix = suffix_vm.suffix_arena.borrow();
            let (arena, start) = suffix.as_ref().expect("suffix arena should exist");
            assert_eq!(*start, suffix_old_start, "suffix start must be restored");
            assert_eq!(
                arena.len(),
                suffix_old_len,
                "suffix appended states must be removed"
            );
        }
        assert_eq!(
            suffix_vm.transition_map.borrow().len(),
            suffix_old_transition_count,
            "rejected suffix transition key must be removed"
        );

        let accepted = suffix_vm.transition_on(&qv(b"file.ok"), false, &mut bufs);
        assert!(
            accepted.iter().any(|fm| Rc::ptr_eq(fm, &suffix_fm)),
            "accepted suffix pattern must still match"
        );
        assert!(
            suffix_vm
                .transition_on(&qv(b"file.xx"), false, &mut bufs)
                .is_empty(),
            "rejected suffix pattern must not become visible"
        );
    }

    #[test]
    fn test_merge_at_uncompacted_budget_keeps_history() {
        // When the merged arena fits the budget exactly, the strict `>` skips
        // compaction and the live arena keeps its append history. A no-budget
        // probe running the same adds yields the uncompacted size to aim the
        // budget at, plus the live (with-history) vs compacted state counts.
        let p1: Vec<_> = crate::json::parse_pattern(
            r#"{"x": [{"regexp": "aaaa"}]}"#,
            &crate::PatternLimits::default(),
        )
        .unwrap()
        .into_iter()
        .collect();
        let p2: Vec<_> = crate::json::parse_pattern(
            r#"{"x": [{"regexp": "bbbb"}]}"#,
            &crate::PatternLimits::default(),
        )
        .unwrap()
        .into_iter()
        .collect();

        let probe = CoreMatcher::<String> {
            root: Rc::new(MutableFieldMatcher::new()),
            arena_byte_budget: 0,
        };
        probe.add_pattern("a".to_string(), &p1).unwrap();
        probe.add_pattern("b".to_string(), &p2).unwrap();
        let (uncompacted_size, live_states, compacted_states) = {
            let transitions = probe.root.transitions.borrow();
            let vm = transitions.get("x").expect("value matcher should exist");
            let main = vm.main_arena.borrow();
            let (arena, start) = main.as_ref().expect("main arena should exist");
            (
                arena.estimated_byte_size(),
                arena.len(),
                clone_arena_subset(arena, *start).0.len(),
            )
        };
        assert!(
            live_states > compacted_states,
            "append merge must leave history for compaction to remove"
        );

        let cm = CoreMatcher::<String> {
            root: Rc::new(MutableFieldMatcher::new()),
            arena_byte_budget: uncompacted_size,
        };
        cm.add_pattern("a".to_string(), &p1).unwrap();
        cm.add_pattern("b".to_string(), &p2)
            .expect("merge at budget == uncompacted size must fit");

        let transitions = cm.root.transitions.borrow();
        let vm = transitions.get("x").expect("value matcher should exist");
        let main = vm.main_arena.borrow();
        let live = main.as_ref().expect("main arena should exist").0.len();
        assert_eq!(
            live, live_states,
            "an exactly-fitting merge must not compact the live arena"
        );
    }
}
