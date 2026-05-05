//! Automaton-based pattern matching engine
//!
//! This module implements an NFA/DFA-based pattern matching engine similar to
//! the Go quamina implementation. The key components are:
//!
//! - `arena`: Arena-based state allocation and traversal for all pattern types
//! - `FieldMatcher`: Matches field names and dispatches to value matchers
//!
//! # Module Organization
//!
//! - `arena`: Arena-based FA construction and traversal
//! - `small_table`: Core data structures (FieldMatcher, AccelInfo, NfaBuffers)
//! - `mutable_matcher`: Single-threaded mutable matchers (CoreMatcher)
//! - `thread_safe`: Thread-safe matchers (ThreadSafeCoreMatcher)

#[doc(hidden)] // pub for benchmarks only
pub mod arena;
mod mutable_matcher;
mod small_table;
pub(crate) mod sparse_set;
mod thread_safe;

// Re-export from small_table
pub use small_table::{AccelInfo, BYTE_CEILING, FieldMatcher, NfaBuffers, VALUE_TERMINATOR};

// Re-export from mutable_matcher
pub use mutable_matcher::{
    ConditionNfa, CoreMatcher, EventField, EventFieldRef, MultiConditionNfa, MutableFieldMatcher,
    MutableValueMatcher,
};

// Re-export from thread_safe. The `AutomatonValueMatcher` alias disambiguates
// the working matcher from `MutableValueMatcher` and `FrozenValueMatcher`
// siblings at use sites.
pub use thread_safe::{
    FrozenFieldMatcher, FrozenValueMatcher, ThreadSafeCoreMatcher,
    ValueMatcher as AutomatonValueMatcher,
};

#[cfg(test)]
mod tests;
