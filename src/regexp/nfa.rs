//! NFA building for regexp matching.
//!
//! This module builds arena-based finite automata from parsed regexp trees.
//! The arena-based approach provides better cache locality and supports
//! patterns with + or * quantifiers efficiently.

use std::sync::Arc;

use smallvec::{smallvec, SmallVec};

use crate::automaton::{
    arena::{ArenaSmallTable, StateArena, StateId, ARENA_VALUE_TERMINATOR},
    FieldMatcher, BYTE_CEILING,
};

use super::parser::{QuantifiedAtom, RegexpBranch, RegexpRoot, RuneRange};

// ============================================================================
// UTF-8 Encoding Constants
// ============================================================================

/// Maximum code point for 1-byte UTF-8 encoding
const UTF8_1BYTE_MAX: u32 = 0x7F;
/// Maximum code point for 2-byte UTF-8 encoding
const UTF8_2BYTE_MAX: u32 = 0x7FF;
/// Maximum code point for 3-byte UTF-8 encoding
const UTF8_3BYTE_MAX: u32 = 0xFFFF;
/// Start of surrogate range (invalid in UTF-8)
const SURROGATE_START: u32 = 0xD800;
/// End of surrogate range (invalid in UTF-8)
const SURROGATE_END: u32 = 0xDFFF;

// ============================================================================
// Shared Utilities
// ============================================================================

/// Convert a rune to UTF-8 bytes.
fn rune_to_utf8(r: char) -> Vec<u8> {
    let mut buf = [0u8; 4];
    let s = r.encode_utf8(&mut buf);
    s.as_bytes().to_vec()
}

/// Check if a regexp tree has any `+` or `*` quantifiers that would benefit from arena-based NFA.
pub fn regexp_has_plus_star(root: &RegexpRoot) -> bool {
    for branch in root {
        for qa in branch {
            if qa.is_plus() || qa.is_star() {
                return true;
            }
            // Recursively check subtrees (parenthesized groups)
            if let Some(ref subtree) = qa.subtree {
                if regexp_has_plus_star(subtree) {
                    return true;
                }
            }
        }
    }
    false
}

// ============================================================================
// Arena-based NFA Building
// ============================================================================

/// Build an arena-based regexp NFA from a parsed tree.
///
/// # Arguments
/// * `root` - The parsed regexp tree
/// * `for_field` - If true, add " matching at start/end for field values
///
/// # Returns
/// A tuple of (arena, start_state_id, field_matcher)
pub fn make_regexp_nfa_arena(
    root: RegexpRoot,
    for_field: bool,
) -> (StateArena, StateId, Arc<FieldMatcher>) {
    let next_field = Arc::new(FieldMatcher::new());

    // Handle empty regexp specially - it matches any string
    if root.is_empty() {
        let mut arena = StateArena::with_capacity(2);

        // Create match state
        let match_state = arena.alloc();
        arena[match_state]
            .field_transitions
            .push(next_field.clone());

        // Create start state that transitions to match on VALUE_TERMINATOR
        let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));

        return (arena, start, next_field);
    }

    // Build the arena NFA
    let mut arena = StateArena::with_capacity(16);

    // Create match state (reached at end of value)
    let match_state = arena.alloc();
    arena[match_state]
        .field_transitions
        .push(next_field.clone());

    // Create VALUE_TERMINATOR transition state
    let vt_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &[ARENA_VALUE_TERMINATOR],
        &[match_state],
    ));

    // If for_field, add trailing quote handling
    let next_step = if for_field {
        arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            b"\"",
            &[vt_state],
        ))
    } else {
        vt_state
    };

    // Build the NFA from branches
    let start = make_arena_nfa_from_branches(&root, &mut arena, next_step, for_field);

    (arena, start, next_field)
}

/// Build arena NFA from branches (alternatives).
fn make_arena_nfa_from_branches(
    root: &RegexpRoot,
    arena: &mut StateArena,
    next_step: StateId,
    for_field: bool,
) -> StateId {
    if root.is_empty() {
        return next_step;
    }

    if root.len() == 1 {
        // Single branch - no alternation needed
        return make_one_arena_branch_fa(&root[0], arena, next_step, for_field);
    }

    // Multiple branches - create a start state with epsilons to each branch
    let mut branch_starts = Vec::with_capacity(root.len());
    for branch in root {
        if branch.is_empty() {
            // Empty branch means we can skip directly to next_step
            branch_starts.push(next_step);
        } else {
            let branch_start = make_one_arena_branch_fa(branch, arena, next_step, false);
            branch_starts.push(branch_start);
        }
    }

    // Create a start state that has epsilons to all branch starts
    let start = arena.alloc();
    arena[start].table.epsilons = SmallVec::from_vec(branch_starts);

    if for_field {
        // Wrap with leading quote
        arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            b"\"",
            &[start],
        ))
    } else {
        start
    }
}

/// Build arena NFA for one branch (sequence of atoms).
fn make_one_arena_branch_fa(
    branch: &RegexpBranch,
    arena: &mut StateArena,
    next_step: StateId,
    for_field: bool,
) -> StateId {
    let mut current_next = next_step;

    // Process atoms back to front
    for qa in branch.iter().rev() {
        let original_next = current_next;

        if qa.is_plus() || qa.is_star() {
            // Arena-based cyclic NFA for + and *
            current_next = create_arena_plus_star_loop(qa, arena, original_next, qa.is_star());
        } else if qa.is_qm() {
            // Optional: build atom FA with epsilon to skip
            let atom_state = make_arena_atom_fa(qa, arena, current_next);
            arena[atom_state].table.epsilons.push(original_next);
            current_next = atom_state;
        } else if qa.is_singleton() {
            // No quantifier - simple FA
            current_next = make_arena_atom_fa(qa, arena, current_next);
        } else {
            // General {n,m} quantifier
            let n = qa.quant_min as usize;
            let m = qa.quant_max as usize;

            // Special case: {0,0} means match zero times - pure epsilon transition
            if n == 0 && m == 0 {
                let epsilon_state = arena.alloc();
                arena[epsilon_state].table.epsilons.push(current_next);
                current_next = epsilon_state;
                continue;
            }

            // First, build the optional part (m-n copies, each with epsilon skip)
            for _ in n..m {
                let atom_state = make_arena_atom_fa(qa, arena, current_next);
                arena[atom_state].table.epsilons.push(current_next);
                current_next = atom_state;
            }

            // Then, build the required part (n copies, no epsilon skip)
            for _ in 0..n {
                current_next = make_arena_atom_fa(qa, arena, current_next);
            }
        }
    }

    if for_field {
        // Wrap with leading quote
        arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            b"\"",
            &[current_next],
        ))
    } else {
        current_next
    }
}

/// Create a cyclic arena NFA structure for + and * quantifiers.
///
/// Structure for [abc]+:
/// - start --(abc)--> loopback --epsilon--> start (cycle!)
///   --epsilon--> exit
///
/// Structure for [abc]*:
/// - Same as above, plus start has epsilon to exit (can match zero times)
fn create_arena_plus_star_loop(
    qa: &QuantifiedAtom,
    arena: &mut StateArena,
    exit_state: StateId,
    is_star: bool,
) -> StateId {
    // Loopback state - will have epsilons to exit and back to start
    let loopback = arena.alloc();

    // Start state - matches the atom, transitions to loopback
    let start = make_arena_atom_fa(qa, arena, loopback);

    // Set up loopback's epsilons: to exit AND back to start (CYCLE!)
    arena[loopback].table.epsilons = smallvec![exit_state, start];

    // For *, add epsilon from start to exit (can skip entirely)
    if is_star {
        arena[start].table.epsilons.push(exit_state);
    }

    // Compute acceleration for the loop.
    // Only ASCII-only negated patterns can be accelerated.
    // Unicode patterns have too many exit bytes (68+) for memchr to help.
    let accel = if let Some(ref bytes) = qa.ascii_negated_bytes {
        // ASCII-only negated pattern: use the negated bytes as exit bytes directly
        let mut accel = crate::automaton::AccelInfo {
            exit_bytes: [0; 3],
            len: bytes.len() as u8,
        };
        for (i, &b) in bytes.iter().enumerate() {
            accel.exit_bytes[i] = b;
        }
        Some(accel)
    } else {
        None
    };

    if let Some(accel) = accel {
        arena[start].table.accel = Some(accel.clone());
        arena[loopback].table.accel = Some(accel);
    }

    start
}

/// Build arena FA for a single atom.
fn make_arena_atom_fa(qa: &QuantifiedAtom, arena: &mut StateArena, next: StateId) -> StateId {
    if qa.is_dot {
        make_arena_dot_fa(arena, next)
    } else if let Some(ref subtree) = qa.subtree {
        make_arena_nfa_from_branches(subtree, arena, next, false)
    } else {
        make_arena_rune_range_fa(&qa.runes, arena, next)
    }
}

/// Build arena FA for a dot (any character).
fn make_arena_dot_fa(arena: &mut StateArena, dest: StateId) -> StateId {
    // Build continuation byte states (for multi-byte UTF-8)
    let s_last = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(dest);
        table.pack(&unpacked);
        table
    });

    let s_last_inter = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let s_first_inter = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    // Special states for specific lead bytes
    let target_e0 = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0xA0..0xC0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let target_ed = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xA0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let target_f0 = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x90..0xC0].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    let target_f4 = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0x90].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    // Main state with all lead byte transitions
    arena.alloc_with_table({
        let mut unpacked = [StateId::NONE; BYTE_CEILING];

        // ASCII (0x00-0x7F) -> dest directly
        unpacked[..0x80].fill(dest);

        // 2-byte sequences (0xC2-0xDF)
        unpacked[0xC2..0xE0].fill(s_last);

        // E0
        unpacked[0xE0] = target_e0;

        // E1-EC
        unpacked[0xE1..0xED].fill(s_last_inter);

        // ED
        unpacked[0xED] = target_ed;

        // EE-EF
        unpacked[0xEE..0xF0].fill(s_last_inter);

        // F0
        unpacked[0xF0] = target_f0;

        // F1-F3
        unpacked[0xF1..0xF4].fill(s_first_inter);

        // F4
        unpacked[0xF4] = target_f4;

        let mut table = ArenaSmallTable::new();
        table.pack(&unpacked);
        table
    })
}

// ============================================================================
// Rune Range to NFA (Arena version)
// ============================================================================

/// Arena version of the rune tree entry
struct ArenaRuneTreeEntry {
    next: Option<StateId>,
    child: Option<ArenaRuneTreeNode>,
}

type ArenaRuneTreeNode = Vec<Option<ArenaRuneTreeEntry>>;

fn new_arena_rune_tree_node() -> ArenaRuneTreeNode {
    (0..BYTE_CEILING).map(|_| None).collect()
}

fn arena_nfa_from_rune_tree(arena: &mut StateArena, root: &ArenaRuneTreeNode) -> StateId {
    arena_table_from_rune_tree_node(arena, root)
}

fn arena_table_from_rune_tree_node(arena: &mut StateArena, node: &ArenaRuneTreeNode) -> StateId {
    let mut unpacked: [StateId; BYTE_CEILING] = [StateId::NONE; BYTE_CEILING];

    for (b, entry_opt) in node.iter().enumerate() {
        if let Some(entry) = entry_opt {
            if let Some(next) = entry.next {
                unpacked[b] = next;
            } else if let Some(ref child) = entry.child {
                let child_state = arena_table_from_rune_tree_node(arena, child);
                unpacked[b] = child_state;
            }
        }
    }

    let mut table = ArenaSmallTable::new();
    table.pack(&unpacked);
    arena.alloc_with_table(table)
}

/// Build arena NFA for a rune range.
fn make_arena_rune_range_fa(rr: &RuneRange, arena: &mut StateArena, next: StateId) -> StateId {
    let mut root = new_arena_rune_tree_node();

    for pair in rr {
        add_arena_rune_pair_tree_entry(&mut root, pair.lo, pair.hi, next);
    }

    arena_nfa_from_rune_tree(arena, &root)
}

/// Add a range of runes [lo, hi] to the arena tree without iterating through each code point.
fn add_arena_rune_pair_tree_entry(root: &mut ArenaRuneTreeNode, lo: char, hi: char, dest: StateId) {
    let lo_u32 = lo as u32;
    let hi_u32 = hi as u32;

    let boundaries = [UTF8_1BYTE_MAX, UTF8_2BYTE_MAX, UTF8_3BYTE_MAX, u32::MAX];

    let mut current = lo_u32;
    for &boundary in &boundaries {
        if current > hi_u32 {
            break;
        }

        // Skip boundaries that are below current position
        if boundary < current {
            continue;
        }

        let segment_end = hi_u32.min(boundary);

        if current <= SURROGATE_END && segment_end >= SURROGATE_START {
            if current < SURROGATE_START {
                let pre_end = (SURROGATE_START - 1).min(segment_end);
                if let (Some(start), Some(end)) = (char::from_u32(current), char::from_u32(pre_end))
                {
                    add_arena_utf8_range_to_tree(root, start, end, dest);
                }
            }
            if segment_end > SURROGATE_END {
                let post_start = (SURROGATE_END + 1).max(current);
                if let (Some(start), Some(end)) =
                    (char::from_u32(post_start), char::from_u32(segment_end))
                {
                    add_arena_utf8_range_to_tree(root, start, end, dest);
                }
            }
        } else if let (Some(start), Some(end)) =
            (char::from_u32(current), char::from_u32(segment_end))
        {
            add_arena_utf8_range_to_tree(root, start, end, dest);
        }

        current = segment_end + 1;
    }
}

fn add_arena_utf8_range_to_tree(root: &mut ArenaRuneTreeNode, lo: char, hi: char, dest: StateId) {
    let lo_bytes = rune_to_utf8(lo);
    let hi_bytes = rune_to_utf8(hi);

    debug_assert_eq!(lo_bytes.len(), hi_bytes.len());

    add_arena_byte_range_recursive(root, &lo_bytes, &hi_bytes, 0, dest);
}

fn add_arena_byte_range_recursive(
    node: &mut ArenaRuneTreeNode,
    lo_bytes: &[u8],
    hi_bytes: &[u8],
    idx: usize,
    dest: StateId,
) {
    if idx >= lo_bytes.len() {
        return;
    }

    let lo_byte = lo_bytes[idx];
    let hi_byte = hi_bytes[idx];
    let is_last = idx == lo_bytes.len() - 1;

    if lo_byte == hi_byte {
        ensure_arena_tree_entry(node, lo_byte);
        let entry = node[lo_byte as usize].as_mut().unwrap();

        if is_last {
            entry.next = Some(dest);
        } else {
            if entry.child.is_none() {
                entry.child = Some(new_arena_rune_tree_node());
            }
            add_arena_byte_range_recursive(
                entry.child.as_mut().unwrap(),
                lo_bytes,
                hi_bytes,
                idx + 1,
                dest,
            );
        }
    } else {
        add_arena_lo_range_to_tree(node, lo_bytes, idx, dest);

        if hi_byte > lo_byte + 1 {
            add_arena_middle_range_to_tree(
                node,
                lo_byte + 1,
                hi_byte - 1,
                lo_bytes.len() - idx - 1,
                dest,
            );
        }

        add_arena_hi_range_to_tree(node, hi_bytes, idx, dest);
    }
}

fn add_arena_lo_range_to_tree(
    node: &mut ArenaRuneTreeNode,
    lo_bytes: &[u8],
    idx: usize,
    dest: StateId,
) {
    let lo_byte = lo_bytes[idx];
    let is_last = idx == lo_bytes.len() - 1;

    ensure_arena_tree_entry(node, lo_byte);
    let entry = node[lo_byte as usize].as_mut().unwrap();

    if is_last {
        entry.next = Some(dest);
    } else {
        if entry.child.is_none() {
            entry.child = Some(new_arena_rune_tree_node());
        }
        let child = entry.child.as_mut().unwrap();
        let next_byte = lo_bytes[idx + 1];

        add_arena_lo_range_to_tree(child, lo_bytes, idx + 1, dest);

        if next_byte < 0xBF {
            add_arena_middle_range_to_tree(
                child,
                next_byte + 1,
                0xBF,
                lo_bytes.len() - idx - 2,
                dest,
            );
        }
    }
}

fn add_arena_hi_range_to_tree(
    node: &mut ArenaRuneTreeNode,
    hi_bytes: &[u8],
    idx: usize,
    dest: StateId,
) {
    let hi_byte = hi_bytes[idx];
    let is_last = idx == hi_bytes.len() - 1;

    ensure_arena_tree_entry(node, hi_byte);
    let entry = node[hi_byte as usize].as_mut().unwrap();

    if is_last {
        entry.next = Some(dest);
    } else {
        if entry.child.is_none() {
            entry.child = Some(new_arena_rune_tree_node());
        }
        let child = entry.child.as_mut().unwrap();
        let next_byte = hi_bytes[idx + 1];

        if next_byte > 0x80 {
            add_arena_middle_range_to_tree(
                child,
                0x80,
                next_byte - 1,
                hi_bytes.len() - idx - 2,
                dest,
            );
        }

        add_arena_hi_range_to_tree(child, hi_bytes, idx + 1, dest);
    }
}

fn add_arena_middle_range_to_tree(
    node: &mut ArenaRuneTreeNode,
    lo: u8,
    hi: u8,
    depth: usize,
    dest: StateId,
) {
    if depth == 0 {
        for byte in lo..=hi {
            ensure_arena_tree_entry(node, byte);
            node[byte as usize].as_mut().unwrap().next = Some(dest);
        }
    } else {
        for byte in lo..=hi {
            ensure_arena_tree_entry(node, byte);
            let entry = node[byte as usize].as_mut().unwrap();
            if entry.child.is_none() {
                entry.child = Some(new_arena_rune_tree_node());
            }
            add_arena_middle_range_to_tree(
                entry.child.as_mut().unwrap(),
                0x80,
                0xBF,
                depth - 1,
                dest,
            );
        }
    }
}

fn ensure_arena_tree_entry(node: &mut ArenaRuneTreeNode, byte: u8) {
    let idx = byte as usize;
    if node[idx].is_none() {
        node[idx] = Some(ArenaRuneTreeEntry {
            next: None,
            child: None,
        });
    }
}
