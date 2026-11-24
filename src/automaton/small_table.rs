//! Core data structures for the finite automaton.
//!
//! This module contains shared types used throughout the automaton:
//! - `FieldMatcher`: Matches field names and dispatches to value matchers
//! - `AccelInfo`: Acceleration info for spinout (wildcard) states
//! - `NfaBuffers`: Reusable buffers for NFA traversal
//!
//! State machines and transition tables are in the `arena` module.

use std::cell::RefCell;
use std::num::NonZero;
use std::sync::Arc;

use rustc_hash::FxHashMap;

/// Maximum byte value we handle.
///
/// UTF-8 bytes 0xF5-0xFF can't appear in valid strings, and we steal 0xF5
/// as a value terminator, so 0xF6 and up never occur. Exposed in both
/// widths because some call sites want to index a 256-entry array (so
/// `usize`) while others want to slot the value straight into a `u8`
/// table entry; an explicit second constant beats scattering `as u8`
/// casts across the module.
pub const BYTE_CEILING: usize = 0xF6;
pub const BYTE_CEILING_U8: u8 = 0xF6;

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

impl AccelInfo {
    /// Marks a state worth checking for an accelerator while its automaton is
    /// being built; `StateArena::settle_accels` replaces it with a checked
    /// accelerator or removes it before anything is traversed. Having no exit
    /// bytes, it would never skip anyway.
    pub(crate) const CANDIDATE: Self = Self {
        exit_bytes: [0; 3],
        len: 0,
    };

    /// The accelerator for a state that its transitions keep returning to, if
    /// skipping ahead from it is sound.
    ///
    /// Skipping to the next exit byte has to land exactly where stepping one
    /// byte at a time would. So every ASCII byte except at most three exits,
    /// and every whole multi-byte character, must lead straight back to
    /// `state`. A character that leads anywhere else can't be made an exit,
    /// since memchr only finds single bytes.
    ///
    /// `row` gives a state's transitions as `(ceiling, target)` runs, each
    /// covering the bytes from the previous ceiling up to this one and the
    /// last ending at `BYTE_CEILING`, as in a `SmallTable`. A `None` target is
    /// where the match dies, which for an ASCII byte makes it an exit too.
    pub(crate) fn for_self_loop<S: Clone + PartialEq>(
        state: &S,
        mut row: impl FnMut(&S) -> Vec<(u8, Option<S>)>,
    ) -> Option<Self> {
        let mut accel = Self::default();
        // Many lead bytes reach the same state with the same continuations
        // still to read, so each such pair is checked once.
        let mut checked = Vec::new();
        let mut start = 0;
        for (ceiling, to) in row(state) {
            if to.as_ref() != Some(state) {
                for byte in start..ceiling.min(0x80) {
                    *accel.exit_bytes.get_mut(usize::from(accel.len))? = byte;
                    accel.len += 1;
                }
            }
            for lead in start.max(0xC2)..ceiling.min(0xF5) {
                let next = to.clone()?;
                if !Self::completes_to(
                    state,
                    next,
                    utf8_continuations(lead),
                    &mut row,
                    &mut checked,
                ) {
                    return None;
                }
            }
            start = ceiling;
        }
        (accel.len > 0).then_some(accel)
    }

    /// Whether every run of `rest` continuation bytes from `from` lands on `state`.
    fn completes_to<S: Clone + PartialEq>(
        state: &S,
        from: S,
        rest: &'static [(u8, u8)],
        row: &mut impl FnMut(&S) -> Vec<(u8, Option<S>)>,
        checked: &mut Vec<(S, &'static [(u8, u8)])>,
    ) -> bool {
        let Some((&(lo, hi), tail)) = rest.split_first() else {
            return from == *state;
        };
        if checked.iter().any(|(s, r)| *s == from && *r == rest) {
            return true;
        }
        let mut start = 0;
        for (ceiling, to) in row(&from) {
            if start <= hi
                && ceiling > lo
                && !to.is_some_and(|t| Self::completes_to(state, t, tail, row, checked))
            {
                return false;
            }
            start = ceiling;
        }
        checked.push((from, rest));
        true
    }

    /// Find the next exit byte in `remaining` using SIMD-accelerated memchr.
    ///
    /// Returns the strictly positive number of bytes the caller may skip
    /// before re-stepping. Both an exit byte at offset 0 and no exit byte
    /// at all collapse to `None`, because the caller in either case must
    /// step normally — skipping zero bytes would loop forever, and there
    /// is nothing to skip past when no exit was found.
    #[inline]
    #[must_use]
    pub fn try_accelerate(&self, remaining: &[u8]) -> Option<NonZero<usize>> {
        let offset = match self.len {
            1 => memchr::memchr(self.exit_bytes[0], remaining),
            2 => memchr::memchr2(self.exit_bytes[0], self.exit_bytes[1], remaining),
            3 => memchr::memchr3(
                self.exit_bytes[0],
                self.exit_bytes[1],
                self.exit_bytes[2],
                remaining,
            ),
            _ => None,
        }?;
        NonZero::new(offset)
    }
}

/// The continuation bytes allowed after a UTF-8 lead byte, one range per
/// position, leaving out overlong forms, surrogates and code points past
/// U+10FFFF.
const fn utf8_continuations(lead: u8) -> &'static [(u8, u8)] {
    const ANY: (u8, u8) = (0x80, 0xBF);
    match lead {
        0xC2..=0xDF => &[ANY],
        0xE0 => &[(0xA0, 0xBF), ANY],
        0xED => &[(0x80, 0x9F), ANY],
        0xE1..=0xEF => &[ANY, ANY],
        0xF0 => &[(0x90, 0xBF), ANY, ANY],
        0xF4 => &[(0x80, 0x8F), ANY, ANY],
        0xF1..=0xF3 => &[ANY, ANY, ANY],
        _ => &[],
    }
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
    pub exists_true: FxHashMap<String, Arc<Self>>,
    /// exists:false patterns - map from field path to next field matcher
    pub exists_false: FxHashMap<String, Arc<Self>>,
}

impl FieldMatcher {
    #[must_use]
    pub fn new() -> Self {
        Self {
            match_id: None,
            exists_true: FxHashMap::default(),
            exists_false: FxHashMap::default(),
        }
    }

    /// Create a new FieldMatcher with a match ID
    #[must_use]
    pub fn with_match_id(id: u64) -> Self {
        Self {
            match_id: Some(id),
            exists_true: FxHashMap::default(),
            exists_false: FxHashMap::default(),
        }
    }
}

/// Buffers reused during NFA/DFA traversal to minimize allocations.
#[derive(Default)]
pub struct NfaBuffers {
    /// Reusable buffers for arena-based NFA traversal
    pub arena_bufs: super::arena::NfaBuffers,
}

impl NfaBuffers {
    #[must_use]
    pub fn new() -> Self {
        Self {
            arena_bufs: super::arena::NfaBuffers::new(),
        }
    }

    pub fn clear(&mut self) {
        self.arena_bufs.clear();
    }
}

thread_local! {
    /// Per-thread reusable traversal buffers for the `matches_for_fields`
    /// convenience paths that don't take caller-provided buffers, so repeated
    /// calls keep their grown capacity instead of reallocating every time.
    pub(super) static TL_MATCH_BUFS: RefCell<NfaBuffers> = RefCell::new(NfaBuffers::new());
}
