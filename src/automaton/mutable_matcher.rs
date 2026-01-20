//! Mutable pattern matchers for single-threaded pattern building.
//!
//! This module contains the mutable (RefCell-based) matchers used during pattern building:
//! - `MutableFieldMatcher`: Mutable field matcher with RefCell-based interior mutability
//! - `MutableValueMatcher`: Mutable value matcher with singleton optimization
//! - `CoreMatcher`: Single-threaded core matcher that builds and matches patterns

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::sync::Arc;

use super::arena::{traverse_arena_nfa, ArenaNfaBuffers, StateArena, StateId};
use super::fa_builders::{
    make_anything_but_fa, make_anything_but_numeric_fa, make_cidr_fa, make_monocase_fa,
    make_numeric_greater_fa, make_numeric_less_fa, make_numeric_range_fa, make_prefix_fa,
    make_shellstyle_fa, make_string_fa, make_wildcard_fa, merge_fas,
};
use super::nfa::{traverse_dfa, traverse_nfa};
use super::small_table::{FieldMatcher, NfaBuffers, SmallTable};
use super::trie::ValueTrie;
use crate::regexp::make_regexp_nfa_arena;

/// A mutable field matcher used during pattern building.
/// This is similar to Go's fieldMatcher with its updateable atomic pointer.
#[derive(Default)]
pub struct MutableFieldMatcher<X: Clone + Eq + std::hash::Hash> {
    /// Map from field paths to value matchers
    pub transitions: RefCell<HashMap<String, Rc<MutableValueMatcher<X>>>>,
    /// Pattern identifiers that match when arriving at this state
    pub matches: RefCell<Vec<X>>,
    /// exists:true patterns - map from field path to next field matcher
    pub exists_true: RefCell<HashMap<String, Rc<MutableFieldMatcher<X>>>>,
    /// exists:false patterns - map from field path to next field matcher
    pub exists_false: RefCell<HashMap<String, Rc<MutableFieldMatcher<X>>>>,
}

impl<X: Clone + Eq + std::hash::Hash> MutableFieldMatcher<X> {
    pub fn new() -> Self {
        Self {
            transitions: RefCell::new(HashMap::new()),
            matches: RefCell::new(Vec::new()),
            exists_true: RefCell::new(HashMap::new()),
            exists_false: RefCell::new(HashMap::new()),
        }
    }

    /// Add a match identifier to this state
    pub fn add_match(&self, x: X) {
        self.matches.borrow_mut().push(x);
    }

    /// Add an exists transition (true or false)
    pub fn add_exists(&self, exists: bool, path: &str) -> Rc<MutableFieldMatcher<X>> {
        let map = if exists {
            &self.exists_true
        } else {
            &self.exists_false
        };

        let mut map_borrow = map.borrow_mut();
        if let Some(existing) = map_borrow.get(path) {
            existing.clone()
        } else {
            let new_fm = Rc::new(MutableFieldMatcher::new());
            map_borrow.insert(path.to_string(), new_fm.clone());
            new_fm
        }
    }

    /// Add a value transition, returns the next field matchers
    pub fn add_transition(
        &self,
        path: &str,
        matchers: &[crate::json::Matcher],
    ) -> Vec<Rc<MutableFieldMatcher<X>>> {
        use crate::json::Matcher;

        let mut transitions = self.transitions.borrow_mut();
        let vm = transitions
            .entry(path.to_string())
            .or_insert_with(|| Rc::new(MutableValueMatcher::new()));

        // Check if all matchers are Exact strings - use bulk optimization
        let all_exact: Vec<&[u8]> = matchers
            .iter()
            .filter_map(|m| match m {
                Matcher::Exact(s) => Some(s.as_bytes()),
                _ => None,
            })
            .collect();

        if all_exact.len() == matchers.len() && all_exact.len() > 1 {
            // All matchers are Exact strings and there's more than one - use bulk method
            let next_fm = vm.add_string_transitions_bulk(&all_exact);
            return vec![next_fm];
        }

        // Fall back to one-by-one processing
        let mut next_states = Vec::new();
        for matcher in matchers {
            let next_fm = vm.add_transition(matcher);
            next_states.push(next_fm);
        }
        next_states
    }

    /// Transition on a field value during matching
    pub fn transition_on(
        &self,
        path: &str,
        value: &[u8],
        is_number: bool,
        bufs: &mut NfaBuffers,
    ) -> Vec<Rc<MutableFieldMatcher<X>>> {
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
#[derive(Default)]
pub struct MutableValueMatcher<X: Clone + Eq + std::hash::Hash> {
    /// The automaton start table (once we have multiple values or complex patterns)
    pub(crate) start_table: RefCell<Option<SmallTable>>,
    /// Optimization: for single exact match, store it directly
    pub(crate) singleton_match: RefCell<Option<Vec<u8>>>,
    /// Transition for singleton match
    pub(crate) singleton_transition: RefCell<Option<Rc<MutableFieldMatcher<X>>>>,
    /// Whether this matcher requires NFA traversal
    pub(crate) is_nondeterministic: RefCell<bool>,
    /// Whether this matcher has numeric patterns (for Q-number conversion)
    pub(crate) has_numbers: RefCell<bool>,
    /// Mapping from Arc<FieldMatcher> to Rc<MutableFieldMatcher<X>>
    /// This bridges the automaton's field transitions to our mutable field matchers
    pub(crate) transition_map: RefCell<HashMap<*const FieldMatcher, Rc<MutableFieldMatcher<X>>>>,
    /// Arena-based NFAs for regexp patterns (2.5x faster than chain-based)
    pub(crate) arena_nfas: RefCell<Vec<(StateArena, StateId)>>,
    /// Buffers for arena NFA traversal
    pub(crate) arena_bufs: RefCell<ArenaNfaBuffers>,
}

impl<X: Clone + Eq + std::hash::Hash> MutableValueMatcher<X> {
    pub fn new() -> Self {
        Self {
            start_table: RefCell::new(None),
            singleton_match: RefCell::new(None),
            singleton_transition: RefCell::new(None),
            is_nondeterministic: RefCell::new(false),
            has_numbers: RefCell::new(false),
            transition_map: RefCell::new(HashMap::new()),
            arena_nfas: RefCell::new(Vec::new()),
            arena_bufs: RefCell::new(ArenaNfaBuffers::new()),
        }
    }

    /// Add a transition for a matcher, returns the next field matcher
    pub fn add_transition(&self, matcher: &crate::json::Matcher) -> Rc<MutableFieldMatcher<X>> {
        use crate::json::Matcher;

        match matcher {
            Matcher::Exact(s) => self.add_string_transition(s.as_bytes()),
            Matcher::NumericExact(n) => self.add_numeric_transition(*n),
            Matcher::Prefix(s) => self.add_prefix_transition(s.as_bytes()),
            Matcher::Shellstyle(s) => {
                *self.is_nondeterministic.borrow_mut() = true;
                self.add_shellstyle_transition(s.as_bytes())
            }
            Matcher::Wildcard(s) => {
                *self.is_nondeterministic.borrow_mut() = true;
                self.add_wildcard_transition(s.as_bytes())
            }
            Matcher::AnythingBut(excluded) => {
                let excluded_bytes: Vec<Vec<u8>> =
                    excluded.iter().map(|s| s.as_bytes().to_vec()).collect();
                self.add_anything_but_transition(&excluded_bytes)
            }
            Matcher::AnythingButNumeric(excluded) => {
                // Mark as having numbers so values get Q-number conversion
                *self.has_numbers.borrow_mut() = true;
                self.add_anything_but_numeric_transition(excluded)
            }
            Matcher::EqualsIgnoreCase(s) => self.add_monocase_transition(s.as_bytes()),
            Matcher::ParsedRegexp(ref tree) => {
                *self.is_nondeterministic.borrow_mut() = true;
                self.add_regexp_transition(tree)
            }
            Matcher::MultiCondition(ref mc) => {
                // Multi-condition patterns use arena-based NFA for primary pattern
                // Conditions are verified during matching
                *self.is_nondeterministic.borrow_mut() = true;
                self.add_multi_condition_transition(mc)
            }
            Matcher::Suffix(s) => {
                // Suffix "abc" is equivalent to shellstyle "*abc"
                *self.is_nondeterministic.borrow_mut() = true;
                let pattern = format!("*{}", s);
                self.add_shellstyle_transition(pattern.as_bytes())
            }
            Matcher::Numeric(cmp) => {
                // Numeric ranges use Q-number ordering in the automaton
                *self.has_numbers.borrow_mut() = true;
                self.add_numeric_range_transition(cmp)
            }
            Matcher::Cidr(ref cidr) => {
                // IPv6 CIDR patterns use epsilon transitions, so mark as nondeterministic
                if matches!(cidr, crate::json::CidrPattern::V6 { .. }) {
                    *self.is_nondeterministic.borrow_mut() = true;
                }
                self.add_cidr_transition(cidr)
            }
            // For Regex fallback, we create a simple next state
            // These would need runtime checking
            _ => Rc::new(MutableFieldMatcher::new()),
        }
    }

    /// Add multiple string transitions efficiently using trie-based construction.
    /// This is O(n) instead of O(n²) for n values.
    ///
    /// Uses a trie to naturally share common prefixes and convert to SmallTable
    /// in a single pass, avoiding repeated merge operations.
    fn add_string_transitions_bulk(&self, values: &[&[u8]]) -> Rc<MutableFieldMatcher<X>> {
        if values.is_empty() {
            return Rc::new(MutableFieldMatcher::new());
        }

        // If only one value, use the normal path (singleton optimization)
        if values.len() == 1 {
            return self.add_string_transition(values[0]);
        }

        // Create a shared next state for all new values
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());

        // Build trie from all new values
        let mut trie = ValueTrie::new();
        trie.insert_all(values, next_arc);

        // Include singleton in trie if present
        if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            trie.insert(&singleton_val, singleton_arc);
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        }

        // Convert trie to SmallTable
        let trie_table = trie.to_small_table();

        // Merge with existing start_table if present (single merge operation)
        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &trie_table));
        } else {
            *start_table = Some(trie_table);
        }

        next_fm
    }

    fn add_string_transition(&self, val: &[u8]) -> Rc<MutableFieldMatcher<X>> {
        // Check singleton optimization
        let singleton = self.singleton_match.borrow();
        let singleton_trans = self.singleton_transition.borrow();

        if singleton.is_none() && self.start_table.borrow().is_none() {
            // Virgin state - use singleton optimization
            drop(singleton);
            drop(singleton_trans);

            let next_fm = Rc::new(MutableFieldMatcher::new());
            *self.singleton_match.borrow_mut() = Some(val.to_vec());
            *self.singleton_transition.borrow_mut() = Some(next_fm.clone());
            return next_fm;
        }

        // Check if singleton matches
        if let Some(ref existing) = *singleton {
            if existing == val {
                return singleton_trans.as_ref().unwrap().clone();
            }
        }
        drop(singleton);
        drop(singleton_trans);

        // Need to build automaton
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        // Register the mapping from Arc<FieldMatcher> to Rc<MutableFieldMatcher>
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());
        let new_fa = make_string_fa(val, next_arc);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            // Merge with existing
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            // Convert singleton to automaton - need to create a new mapping for the singleton
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    /// Add a numeric transition that supports Q-number matching.
    /// Builds both a string FA for the text representation and a Q-number FA.
    fn add_numeric_transition(&self, num: f64) -> Rc<MutableFieldMatcher<X>> {
        // Mark that this matcher has numeric patterns
        *self.has_numbers.borrow_mut() = true;

        let val_str = num.to_string();
        let val = val_str.as_bytes();

        // Get Q-number representation
        let q_num = crate::numbits::q_num_from_f64(num);

        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());

        // Build string FA and Q-number FA, merge them
        let string_fa = make_string_fa(val, next_arc.clone());
        let q_num_fa = make_string_fa(&q_num, next_arc);
        let merged_fa = merge_fas(&string_fa, &q_num_fa);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &merged_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &merged_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(merged_fa);
        }

        next_fm
    }

    fn add_prefix_transition(&self, prefix: &[u8]) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());
        let new_fa = make_prefix_fa(prefix, next_arc);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    fn add_shellstyle_transition(&self, pattern: &[u8]) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());
        let new_fa = make_shellstyle_fa(pattern, next_arc);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    fn add_wildcard_transition(&self, pattern: &[u8]) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());
        let new_fa = make_wildcard_fa(pattern, next_arc);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    fn add_anything_but_transition(&self, excluded: &[Vec<u8>]) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());
        let new_fa = make_anything_but_fa(excluded, next_arc);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    /// Add a numeric anything-but transition using Q-number FA.
    ///
    /// Matches any numeric value NOT in the excluded list.
    /// Values are compared using Q-number representation for proper numeric ordering.
    fn add_anything_but_numeric_transition(&self, excluded: &[f64]) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());
        let new_fa = make_anything_but_numeric_fa(excluded, next_arc);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    fn add_monocase_transition(&self, val: &[u8]) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());
        let new_fa = make_monocase_fa(val, next_arc);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    fn add_regexp_transition(
        &self,
        tree: &crate::regexp::RegexpRoot,
    ) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());

        // Always use arena-based NFA for regexp patterns (2.5x faster)
        let (arena, start, field_matcher_arc) = make_regexp_nfa_arena(tree.clone(), false);

        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&field_matcher_arc), next_fm.clone());

        // Store arena NFA (supports multiple per value matcher)
        self.arena_nfas.borrow_mut().push((arena, start));

        next_fm
    }

    /// Add a multi-condition transition for lookaround patterns.
    ///
    /// Multi-condition patterns have a primary pattern plus conditions (lookarounds):
    /// - Primary pattern is built as an arena NFA
    /// - Conditions are stored for verification during matching
    ///
    /// For now, we build the primary automaton. Condition verification will be added
    /// when condition automata are built and stored.
    fn add_multi_condition_transition(
        &self,
        mc: &crate::json::MultiConditionPattern,
    ) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());

        // Build primary pattern automaton
        let (arena, start, field_matcher_arc) = make_regexp_nfa_arena(mc.primary.clone(), false);

        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&field_matcher_arc), next_fm.clone());

        // Store arena NFA for primary pattern
        self.arena_nfas.borrow_mut().push((arena, start));

        // TODO: Build and store condition automata for verification during matching
        // For positive lookahead A(?=B): build automaton for combined pattern AB
        // For negative lookahead A(?!B): build automaton for combined pattern AB
        // During matching, verify conditions after primary matches

        // For now, conditions are built but not verified yet.
        // The transformation in json.rs ensures:
        // - PositiveLookahead contains the combined pattern (AB)
        // - NegativeLookahead contains the combined pattern (AB)
        // Condition verification requires storing these separately and checking during match.

        next_fm
    }

    /// Add a numeric range transition that uses Q-number ordering in the automaton.
    ///
    /// For two-sided ranges (e.g., >= 5, < 100), we merge the lower and upper bound FAs.
    /// For single-sided ranges (e.g., < 100), we only use the relevant FA.
    fn add_numeric_range_transition(
        &self,
        cmp: &crate::json::NumericComparison,
    ) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());

        // Build the FA(s) based on the comparison operators
        let new_fa = match (&cmp.lower, &cmp.upper) {
            (Some((lower_incl, lower_val)), Some((upper_incl, upper_val))) => {
                // Two-sided range: build a combined FA that handles both bounds
                make_numeric_range_fa(*lower_val, *lower_incl, *upper_val, *upper_incl, next_arc)
            }
            (Some((incl, val)), None) => {
                // Lower bound only: >= or >
                make_numeric_greater_fa(*val, *incl, next_arc)
            }
            (None, Some((incl, val))) => {
                // Upper bound only: <= or <
                make_numeric_less_fa(*val, *incl, next_arc)
            }
            (None, None) => {
                // No bounds specified - match any number
                // This shouldn't happen in practice
                return next_fm;
            }
        };

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    /// Add a CIDR pattern transition using automaton-based IP matching.
    ///
    /// Builds an FA that matches IP addresses in the specified CIDR range.
    fn add_cidr_transition(&self, cidr: &crate::json::CidrPattern) -> Rc<MutableFieldMatcher<X>> {
        let next_fm = Rc::new(MutableFieldMatcher::new());
        let next_arc = Arc::new(FieldMatcher::new());
        self.transition_map
            .borrow_mut()
            .insert(Arc::as_ptr(&next_arc), next_fm.clone());
        let new_fa = make_cidr_fa(cidr, next_arc);

        let mut start_table = self.start_table.borrow_mut();
        if let Some(ref existing) = *start_table {
            *start_table = Some(merge_fas(existing, &new_fa));
        } else if self.singleton_match.borrow().is_some() {
            let singleton_val = self.singleton_match.borrow().clone().unwrap();
            let singleton_trans = self.singleton_transition.borrow().clone().unwrap();
            let singleton_arc = Arc::new(FieldMatcher::new());
            self.transition_map
                .borrow_mut()
                .insert(Arc::as_ptr(&singleton_arc), singleton_trans);
            let singleton_fa = make_string_fa(&singleton_val, singleton_arc);
            *start_table = Some(merge_fas(&singleton_fa, &new_fa));
            *self.singleton_match.borrow_mut() = None;
            *self.singleton_transition.borrow_mut() = None;
        } else {
            *start_table = Some(new_fa);
        }

        next_fm
    }

    /// Transition on a value during matching
    pub fn transition_on(
        &self,
        value: &[u8],
        is_number: bool,
        bufs: &mut NfaBuffers,
    ) -> Vec<Rc<MutableFieldMatcher<X>>> {
        // Check singleton first
        if let Some(ref singleton_val) = *self.singleton_match.borrow() {
            if singleton_val == value {
                if let Some(ref trans) = *self.singleton_transition.borrow() {
                    return vec![trans.clone()];
                }
            }
            return vec![];
        }

        let transition_map = self.transition_map.borrow();
        let mut result = Vec::new();

        // Try with Q-number conversion if this matcher has numbers and value is numeric
        // Use Cow to avoid allocation when not converting to Q-number
        let value_to_match: Cow<'_, [u8]> = if *self.has_numbers.borrow() && is_number {
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
        if let Some(ref table) = *self.start_table.borrow() {
            // Clear and reuse the transitions buffer
            bufs.transitions.clear();

            if *self.is_nondeterministic.borrow() {
                traverse_nfa(table, &value_to_match, bufs);
            } else {
                traverse_dfa(table, &value_to_match, &mut bufs.transitions);
            }

            // Map Arc<FieldMatcher> transitions to Rc<MutableFieldMatcher<X>>
            for arc_fm in &bufs.transitions {
                let ptr = Arc::as_ptr(arc_fm);
                if let Some(mutable_fm) = transition_map.get(&ptr) {
                    result.push(mutable_fm.clone());
                }
            }
        }

        // Traverse all arena NFAs (for regexp patterns)
        let arena_nfas = self.arena_nfas.borrow();
        if !arena_nfas.is_empty() {
            let mut arena_bufs = self.arena_bufs.borrow_mut();
            for (arena, start) in arena_nfas.iter() {
                traverse_arena_nfa(arena, *start, &value_to_match, &mut arena_bufs);

                // Map Arc<FieldMatcher> transitions to Rc<MutableFieldMatcher<X>>
                for arc_fm in &arena_bufs.transitions {
                    let ptr = Arc::as_ptr(arc_fm);
                    if let Some(mutable_fm) = transition_map.get(&ptr) {
                        // Avoid duplicates
                        if !result.iter().any(|r| Rc::ptr_eq(r, mutable_fm)) {
                            result.push(mutable_fm.clone());
                        }
                    }
                }
            }
        }

        result
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
        EventField {
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
    seen: HashSet<X>,
    matches: Vec<X>,
}

impl<X: Clone + Eq + std::hash::Hash> MatchSet<X> {
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

/// Core matcher that uses automaton-based matching for multiple fields.
///
/// This implements the Go quamina matching algorithm:
/// 1. Patterns are added by building a graph of FieldMatcher -> ValueMatcher -> FieldMatcher
/// 2. Event fields are sorted and matched against the automaton
/// 3. Matching recursively tries subsequent fields to find complete pattern matches
#[derive(Default)]
pub struct CoreMatcher<X: Clone + Eq + std::hash::Hash> {
    /// Root field matcher - the start state of the automaton
    root: Rc<MutableFieldMatcher<X>>,
}

impl<X: Clone + Eq + std::hash::Hash> CoreMatcher<X> {
    /// Create a new CoreMatcher
    pub fn new() -> Self {
        Self {
            root: Rc::new(MutableFieldMatcher::new()),
        }
    }

    /// Add a pattern with the given identifier.
    ///
    /// The pattern_fields should be a list of (path, matchers) tuples.
    /// Fields are automatically sorted by path for matching.
    pub fn add_pattern(&self, x: X, pattern_fields: &[(String, Vec<crate::json::Matcher>)]) {
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
    }

    /// Match fields against patterns and return matching pattern identifiers.
    ///
    /// Fields should already be sorted by path.
    pub fn matches_for_fields(&self, fields: &[EventField]) -> Vec<X> {
        if fields.is_empty() {
            // Still need to check exists:false patterns
            return self.collect_exists_false_matches(&self.root);
        }

        let mut matches = MatchSet::new();
        let mut bufs = NfaBuffers::new();

        // For each field, try to match from the start state
        for i in 0..fields.len() {
            self.try_to_match(fields, i, &self.root, &mut matches, &mut bufs);
        }

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
            let field_exists = fields.iter().any(|f| &f.path == path);

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

    /// Collect matches from exists:false patterns when there are no fields
    fn collect_exists_false_matches(&self, state: &Rc<MutableFieldMatcher<X>>) -> Vec<X> {
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
            return self.collect_exists_false_matches(&self.root);
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
            let field_exists = fields.iter().any(|f| f.path == path);

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
            return self.collect_exists_false_matches(&self.root);
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
            let field_exists = fields.iter().any(|f| f.path_str() == path);

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
    use crate::json::Matcher;
    use crate::regexp::parse_regexp;

    #[test]
    fn test_value_matcher_regexp_with_plus() {
        // Test that MutableValueMatcher correctly uses arena for regexp with +
        let vm: MutableValueMatcher<String> = MutableValueMatcher::new();

        // Create a regexp pattern with + quantifier
        let regexp_tree = parse_regexp("[a-z]+@example~.com").unwrap();
        let matcher = Matcher::ParsedRegexp(regexp_tree);

        let next_fm = vm.add_transition(&matcher);

        // Verify arena was used (all regexp patterns use arena now)
        assert!(
            !vm.arena_nfas.borrow().is_empty(),
            "Arena NFAs should be set for regexp"
        );
        assert!(
            vm.start_table.borrow().is_none(),
            "Start table should be None when using arena"
        );

        // Test matching
        let mut bufs = NfaBuffers::new();
        let value = b"alice@example.com";
        let results = vm.transition_on(value, false, &mut bufs);

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
        let no_match_value = b"alice@exampleXcom";
        let no_results = vm.transition_on(no_match_value, false, &mut bufs);
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

        let next_fm = vm.add_transition(&matcher);

        // Verify arena was used (all regexp patterns use arena now)
        assert!(
            !vm.arena_nfas.borrow().is_empty(),
            "Arena NFAs should be set for regexp"
        );
        assert!(
            vm.start_table.borrow().is_none(),
            "Start table should be None when using arena for regexp"
        );

        // Test matching
        let mut bufs = NfaBuffers::new();
        let value = b"a";
        let results = vm.transition_on(value, false, &mut bufs);

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
        let pattern = crate::json::parse_pattern(pattern_json).unwrap();
        let pattern_vec: Vec<_> = pattern.into_iter().collect();

        cm.add_pattern("p1".to_string(), &pattern_vec);

        // Create a field like the flattener would
        let fields = vec![EventField {
            path: "email".to_string(),
            value: "alice@example.com".to_string(),
            array_trail: vec![],
            is_number: false,
        }];

        let matches = cm.matches_for_fields(&fields);
        assert_eq!(matches, vec!["p1".to_string()], "Should match the pattern");

        // Test non-match
        let fields_no_match = vec![EventField {
            path: "email".to_string(),
            value: "alice@exampleXcom".to_string(),
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
        let pattern = crate::json::parse_pattern(pattern_json).unwrap();
        let pattern_vec: Vec<_> = pattern.into_iter().collect();

        cm.add_pattern("p1".to_string(), &pattern_vec);

        // Create fields like matches_for_fields_direct expects
        let fields = vec![crate::flatten_json::Field {
            path: Arc::from(b"email".as_slice()),
            val: crate::flatten_json::FieldValue::Borrowed(b"alice@example.com"),
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
}
