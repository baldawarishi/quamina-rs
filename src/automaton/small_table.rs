//! Core data structures for the finite automaton.
//!
//! This module contains shared types used throughout the automaton:
//! - `FieldMatcher`: Matches field names and dispatches to value matchers
//! - `AccelInfo`: Acceleration info for spinout (wildcard) states
//! - `NfaBuffers`: Reusable buffers for NFA traversal
//!
//! State machines and transition tables are in the `arena` module.

use std::collections::HashMap;
use std::sync::Arc;

/// Maximum byte value we handle. UTF-8 bytes 0xF5-0xFF can't appear in valid strings.
/// We use 0xF5 as a value terminator.
pub const BYTE_CEILING: usize = 0xF6;

/// Marks the end of a value being matched. This simplifies handling of exact matches
/// vs prefix matches - we always add this terminator to both the pattern and the value.
pub const VALUE_TERMINATOR: u8 = 0xF5;

/// Acceleration info for spinout (wildcard) states.
///
/// Stores 1-3 exit bytes that would cause a transition out of the spin state.
/// This enables using memchr SIMD to skip directly to relevant bytes instead
/// of processing byte-by-byte.
#[derive(Clone, Debug, Default)]
pub struct AccelInfo {
    /// Exit bytes that leave this state (up to 3 bytes).
    /// VALUE_TERMINATOR is always an implicit exit byte.
    pub exit_bytes: [u8; 3],
    /// Number of valid exit bytes (0 = not accelerated, 1-3 = use memchr).
    /// Note: VALUE_TERMINATOR is handled separately, these are the "escape" bytes.
    pub len: u8,
}

/// Matches field names and dispatches to value matchers.
///
/// Used as the transition target in the arena FA. When the automaton reaches
/// the end of a value match, field transitions point to the next level of
/// field matching in the pattern tree.
#[derive(Clone, Default)]
pub struct FieldMatcher {
    /// Pattern identifiers that match when arriving at this state.
    /// Using a unique ID that survives FA merging.
    pub match_id: Option<u64>,
    /// exists:true patterns - map from field path to next field matcher
    pub exists_true: HashMap<String, Arc<Self>>,
    /// exists:false patterns - map from field path to next field matcher
    pub exists_false: HashMap<String, Arc<Self>>,
}

impl FieldMatcher {
    pub fn new() -> Self {
        Self {
            match_id: None,
            exists_true: HashMap::new(),
            exists_false: HashMap::new(),
        }
    }

    /// Create a new FieldMatcher with a match ID
    pub fn with_match_id(id: u64) -> Self {
        Self {
            match_id: Some(id),
            exists_true: HashMap::new(),
            exists_false: HashMap::new(),
        }
    }
}

/// Buffers reused during NFA/DFA traversal to minimize allocations.
#[derive(Default)]
pub struct NfaBuffers {
    /// Reusable buffers for arena-based NFA traversal
    pub arena_bufs: super::arena::ArenaNfaBuffers,
}

impl NfaBuffers {
    pub fn new() -> Self {
        Self {
            arena_bufs: super::arena::ArenaNfaBuffers::new(),
        }
    }

    pub fn clear(&mut self) {
        self.arena_bufs.clear();
    }
}
