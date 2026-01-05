//! Automaton-based pattern matching engine
//!
//! This module implements an NFA/DFA-based pattern matching engine similar to
//! the Go quamina implementation. The key components are:
//!
//! - `SmallTable`: A compact byte-indexed transition table
//! - `FaState`: A state in the finite automaton
//! - `FieldMatcher`: Matches field names and dispatches to value matchers
//! - `ValueMatcher`: Matches field values using the automaton
//!
//! # Module Organization
//!
//! - `small_table`: Core data structures (FaState, SmallTable, FieldMatcher, etc.)
//! - `nfa`: NFA/DFA traversal functions
//! - `fa_builders`: FA construction functions (make_*_fa, merge_fas)
//! - `mutable_matcher`: Single-threaded mutable matchers (CoreMatcher)
//! - `thread_safe`: Thread-safe matchers (ThreadSafeCoreMatcher)

mod fa_builders;
mod mutable_matcher;
mod nfa;
mod small_table;
mod thread_safe;

// Re-export from small_table
pub use small_table::{
    FaState, FieldMatcher, NfaBuffers, SmallTable, ValueMatcher, BYTE_CEILING, VALUE_TERMINATOR,
};

// Re-export from nfa
pub use nfa::{traverse_dfa, traverse_nfa};

// Re-export from fa_builders
pub use fa_builders::{
    make_anything_but_fa, make_monocase_fa, make_prefix_fa, make_shellstyle_fa, make_string_fa,
    make_wildcard_fa, merge_fas,
};

// Re-export from mutable_matcher
pub use mutable_matcher::{
    CoreMatcher, EventField, EventFieldRef, MutableFieldMatcher, MutableValueMatcher,
};

// Re-export from thread_safe
pub use thread_safe::{
    AutomatonValueMatcher, FrozenFieldMatcher, FrozenValueMatcher, ThreadSafeCoreMatcher,
};

#[cfg(test)]
mod tests;
