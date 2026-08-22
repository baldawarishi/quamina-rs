//! Walking-skeleton experiment: fuzzy k-gram FST index vs. exact k-gram
//! inverted index for Type-3 (edited) clone recovery.
//!
//! See ../JOURNAL.md for the design decisions and ../FINDINGS.md for the
//! result. This crate is intentionally not wired into quamina's workspace.

pub mod corpus;
pub mod index_exact;
pub mod index_fst;
pub mod kgram;
pub mod metrics;
pub mod mutate;
pub mod tokenize;

/// Identifies one extracted function-level fragment (an "original").
pub type FragmentId = u32;
