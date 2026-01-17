//! NFA building for regexp matching.
//!
//! This module builds finite automata from parsed regexp trees.
//! Supports both Arc-based structures (for patterns without cycles) and
//! arena-based structures (for patterns with + or * quantifiers).
//!
//! ## Shell Caching
//!
//! For large Unicode categories like `~p{L}` (all letters, ~1.1M code points),
//! building the FA from scratch is expensive. We cache pre-built "shells" -
//! FAs built with a placeholder state as the destination. When a cached
//! category is needed, we copy the shell and replace the placeholder with
//! the actual next state.

use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

use crate::automaton::{
    arena::{ArenaSmallTable, StateArena, StateId, ARENA_VALUE_TERMINATOR},
    merge_fas, FaState, FieldMatcher, SmallTable, BYTE_CEILING, VALUE_TERMINATOR,
};

use super::parser::{QuantifiedAtom, RegexpBranch, RegexpRoot, RuneRange, REGEXP_QUANTIFIER_MAX};

// ============================================================================
// Shell Caching for Unicode Categories
// ============================================================================

/// Global placeholder state used as a sentinel when building cached FA shells.
/// We use Arc::ptr_eq to identify this placeholder during shell copying.
fn placeholder_state() -> &'static Arc<FaState> {
    static PLACEHOLDER: OnceLock<Arc<FaState>> = OnceLock::new();
    PLACEHOLDER.get_or_init(|| Arc::new(FaState::with_table(SmallTable::new())))
}

/// Global cache of pre-built FA shells for Unicode categories.
/// Key is the category name (e.g., "L", "Lu", "-L" for negated).
fn shell_cache() -> &'static Mutex<HashMap<String, SmallTable>> {
    static CACHE: OnceLock<Mutex<HashMap<String, SmallTable>>> = OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Build an FA from a cached shell by copying it and replacing the placeholder.
fn fa_from_shell(shell: &SmallTable, new_next: &Arc<FaState>) -> SmallTable {
    let placeholder = placeholder_state();
    copy_shell_table(shell, placeholder, new_next)
}

/// Copy a SmallTable, replacing transitions to placeholder with new_next.
fn copy_shell_table(
    shell: &SmallTable,
    placeholder: &Arc<FaState>,
    new_next: &Arc<FaState>,
) -> SmallTable {
    SmallTable {
        ceilings: shell.ceilings.clone(),
        steps: shell
            .steps
            .iter()
            .map(|step| {
                step.as_ref().map(|state| {
                    if Arc::ptr_eq(state, placeholder) {
                        new_next.clone()
                    } else {
                        copy_shell_node(state, placeholder, new_next)
                    }
                })
            })
            .collect(),
        epsilons: shell.epsilons.clone(),
        spinout: shell.spinout.clone(),
    }
}

/// Recursively copy an FaState, replacing placeholder with new_next.
fn copy_shell_node(
    shell: &Arc<FaState>,
    placeholder: &Arc<FaState>,
    new_next: &Arc<FaState>,
) -> Arc<FaState> {
    Arc::new(FaState {
        table: copy_shell_table(&shell.table, placeholder, new_next),
        field_transitions: shell.field_transitions.clone(),
    })
}

/// Build a rune range FA with optional caching.
/// If `cache_key` is provided, the FA is cached for reuse.
fn make_cached_rune_range_fa(
    rr: &RuneRange,
    next: &Arc<FaState>,
    cache_key: Option<&str>,
) -> SmallTable {
    if let Some(key) = cache_key {
        let cache = shell_cache();
        let mut cache_guard = cache.lock().unwrap();

        // Check if we have a cached shell
        if let Some(shell) = cache_guard.get(key) {
            return fa_from_shell(shell, next);
        }

        // Build shell with placeholder as destination
        let placeholder = placeholder_state();
        let shell = make_rune_range_nfa(rr, placeholder);

        // Cache the shell
        cache_guard.insert(key.to_string(), shell.clone());

        // Return a copy with the real next state
        fa_from_shell(&shell, next)
    } else {
        // No caching - build directly
        make_rune_range_nfa(rr, next)
    }
}

/// Convert a rune to UTF-8 bytes.
fn rune_to_utf8(r: char) -> Vec<u8> {
    let mut buf = [0u8; 4];
    let s = r.encode_utf8(&mut buf);
    s.as_bytes().to_vec()
}

/// Build a regexp NFA from a parsed tree.
///
/// # Arguments
/// * `root` - The parsed regexp tree
/// * `for_field` - If true, add " matching at start/end for field values
pub fn make_regexp_nfa(root: RegexpRoot, for_field: bool) -> (SmallTable, Arc<FieldMatcher>) {
    let next_field = Arc::new(FieldMatcher::new());

    // Handle empty regexp specially - it matches any string
    if root.is_empty() {
        let table = make_empty_regexp_fa(&next_field);
        return (table, next_field);
    }

    let next_step = make_nfa_trailer(next_field.clone());

    let mut next_step_state = next_step;
    if for_field {
        let table = SmallTable::with_mappings(None, b"\"", &[next_step_state]);
        next_step_state = Arc::new(FaState::with_table(table));
    }

    let table = make_nfa_from_branches(&root, &next_step_state, for_field);
    (table, next_field)
}

/// Generate the last steps in the NFA (field-matched state + valueTerminator).
fn make_nfa_trailer(next_field: Arc<FieldMatcher>) -> Arc<FaState> {
    let match_state = Arc::new(FaState {
        table: SmallTable::new(),
        field_transitions: vec![next_field],
    });
    let table = SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[match_state]);
    Arc::new(FaState::with_table(table))
}

/// Build NFA from branches (alternatives).
fn make_nfa_from_branches(
    root: &RegexpRoot,
    next_step: &Arc<FaState>,
    for_field: bool,
) -> SmallTable {
    let mut fa = SmallTable::new();
    for branch in root {
        let next_branch = if branch.is_empty() {
            // Empty branch - just go directly to next_step (VALUE_TERMINATOR transition)
            // This matches empty string at this position
            SmallTable::with_mappings(None, &[VALUE_TERMINATOR], std::slice::from_ref(next_step))
        } else {
            make_one_regexp_branch_fa(branch, next_step, for_field)
        };
        fa = merge_fas(&fa, &next_branch);
    }
    fa
}

/// Build an FA for empty regexp that matches only empty string.
fn make_empty_regexp_fa(next_field: &Arc<FieldMatcher>) -> SmallTable {
    // Empty regexp matches only empty string.
    // Create a match state and transition only on VALUE_TERMINATOR (end of value).
    let match_state = Arc::new(FaState {
        table: SmallTable::new(),
        field_transitions: vec![next_field.clone()],
    });

    // Only match on VALUE_TERMINATOR -> matches empty string only
    SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[match_state])
}

/// Build the FA for a single quantified atom.
/// Returns the SmallTable for matching this atom (pointing to next_step on match).
fn make_atom_fa(qa: &QuantifiedAtom, next_step: &Arc<FaState>) -> SmallTable {
    if qa.is_dot {
        make_dot_fa(next_step)
    } else if let Some(ref subtree) = qa.subtree {
        make_nfa_from_branches(subtree, next_step, false)
    } else {
        // Use caching for large Unicode categories
        make_cached_rune_range_fa(&qa.runes, next_step, qa.cache_key.as_deref())
    }
}

/// Create the cyclic NFA structure for + and * quantifiers.
///
/// This uses `std::sync::OnceLock` to break the chicken-and-egg problem
/// of creating mutually-referencing Arc structures.
///
/// Structure for [abc]+:
/// - loop_state.table: on 'a'/'b'/'c' -> loopback
/// - loopback.epsilons: [exit_state, loop_state]
///
/// Structure for [abc]*:
/// - Same as above, plus loop_state.table.epsilons includes exit_state
fn create_plus_star_loop(
    qa: &QuantifiedAtom,
    exit_state: &Arc<FaState>,
    is_star: bool,
) -> Arc<FaState> {
    // Create a chain of states to support up to REGEXP_QUANTIFIER_MAX iterations.
    // Each level: loop_table -> loopback -> (exit OR next level)
    //
    // For +: Must match at least once, so no epsilon to exit from entry
    // For *: Can match zero times, so entry has epsilon to exit
    //
    // Structure per level:
    // - loop_table: matches atom, transitions to loopback
    // - loopback: epsilon to exit_state AND epsilon to previous level (or exit for last)

    let depth = REGEXP_QUANTIFIER_MAX as usize;
    let mut next_level: Option<Arc<FaState>> = None;

    // Build from inside out (deepest level first)
    for i in 0..depth {
        // Loopback epsilons: always include exit, and include next_level if it exists
        let mut loopback_epsilons = vec![exit_state.clone()];
        if let Some(ref nl) = next_level {
            loopback_epsilons.push(nl.clone());
        }

        let loopback = Arc::new(FaState::with_table(SmallTable {
            ceilings: Vec::new(),
            steps: Vec::new(),
            epsilons: loopback_epsilons,
            spinout: None,
        }));

        let mut loop_table = make_atom_fa(qa, &loopback);

        // For *, add epsilon to exit (can skip this level entirely)
        if is_star && i == depth - 1 {
            // Only the outermost (returned) level needs the skip epsilon
            loop_table.epsilons.push(exit_state.clone());
        }

        next_level = Some(Arc::new(FaState::with_table(loop_table)));
    }

    next_level.unwrap()
}

/// Build NFA for one branch (sequence of atoms).
/// Implements Thompson construction for quantifiers.
fn make_one_regexp_branch_fa(
    branch: &RegexpBranch,
    next_step: &Arc<FaState>,
    for_field: bool,
) -> SmallTable {
    let mut current_next = next_step.clone();
    let mut table = SmallTable::new();

    // Process atoms back to front, like Go.
    // At the start of each iteration, current_next is "where to go after matching this atom".
    for qa in branch.iter().rev() {
        // The state we want to reach after this atom (before any quantifier modifications)
        let original_next = current_next.clone();

        if qa.is_plus() || qa.is_star() {
            // Thompson construction for + (one or more) and * (zero or more).
            // Uses a helper function that creates a chain of states to simulate
            // the loop (since Rust's Arc doesn't allow true cycles without interior mutability).
            let exit_state = original_next.clone();
            let is_star = qa.is_star();
            let final_loop_state = create_plus_star_loop(qa, &exit_state, is_star);
            table = final_loop_state.table.clone();
            current_next = final_loop_state;
        } else if qa.is_qm() {
            // Thompson construction for ? (optional):
            // Build FA with epsilon to skip
            table = make_atom_fa(qa, &current_next);
            table.epsilons.push(original_next);
            current_next = Arc::new(FaState::with_table(table.clone()));
        } else if qa.is_singleton() {
            // No quantifier - simple FA
            table = make_atom_fa(qa, &current_next);
            current_next = Arc::new(FaState::with_table(table.clone()));
        } else {
            // General {n,m} quantifier - Thompson construction
            // Build (m-n) optional copies first (back to front), then n required copies
            let n = qa.quant_min as usize;
            let m = qa.quant_max as usize;

            // Special case: {0,0} means match zero times - pure epsilon transition
            if n == 0 && m == 0 {
                // No state changes needed - just epsilon to current_next
                table = SmallTable::new();
                table.epsilons.push(current_next.clone());
                current_next = Arc::new(FaState::with_table(table.clone()));
                continue;
            }

            // First, build the optional part (m-n copies, each with epsilon skip)
            // Working back to front, so we build these first
            for _ in n..m {
                table = make_atom_fa(qa, &current_next);
                // Add epsilon to skip this optional match
                table.epsilons.push(current_next.clone());
                current_next = Arc::new(FaState::with_table(table.clone()));
            }

            // Then, build the required part (n copies, no epsilon skip)
            for _ in 0..n {
                table = make_atom_fa(qa, &current_next);
                current_next = Arc::new(FaState::with_table(table.clone()));
            }
        }
    }

    if for_field {
        let first_state = Arc::new(FaState::with_table(table.clone()));
        table = SmallTable::with_mappings(None, b"\"", &[first_state]);
    }

    table
}

/// Rune tree entry for building NFA from rune ranges.
struct RuneTreeEntry {
    next: Option<Arc<FaState>>,
    child: Option<RuneTreeNode>,
}

type RuneTreeNode = Vec<Option<RuneTreeEntry>>;

fn new_rune_tree_node() -> RuneTreeNode {
    (0..BYTE_CEILING).map(|_| None).collect()
}

/// Build NFA from rune tree.
fn nfa_from_rune_tree(root: &RuneTreeNode) -> SmallTable {
    table_from_rune_tree_node(root)
}

fn table_from_rune_tree_node(node: &RuneTreeNode) -> SmallTable {
    let mut unpacked: [Option<Arc<FaState>>; BYTE_CEILING] = std::array::from_fn(|_| None);

    for (b, entry_opt) in node.iter().enumerate() {
        if let Some(entry) = entry_opt {
            if let Some(ref next) = entry.next {
                unpacked[b] = Some(next.clone());
            } else if let Some(ref child) = entry.child {
                let table = table_from_rune_tree_node(child);
                unpacked[b] = Some(Arc::new(FaState::with_table(table)));
            }
        }
    }

    let mut st = SmallTable::new();
    st.pack(&unpacked);
    st
}

/// Build NFA for a rune range.
///
/// This optimized version adds entire RunePairs at once instead of iterating
/// through individual code points. This dramatically improves performance for
/// negated character classes like `[^abc]` which cover ~1.1M Unicode code points.
fn make_rune_range_nfa(rr: &RuneRange, next: &Arc<FaState>) -> SmallTable {
    let mut root = new_rune_tree_node();

    for pair in rr {
        add_rune_pair_tree_entry(&mut root, pair.lo, pair.hi, next);
    }

    nfa_from_rune_tree(&root)
}

// UTF-8 encoding boundaries
const UTF8_1BYTE_MAX: u32 = 0x7F;
const UTF8_2BYTE_MAX: u32 = 0x7FF;
const UTF8_3BYTE_MAX: u32 = 0xFFFF;
const SURROGATE_START: u32 = 0xD800;
const SURROGATE_END: u32 = 0xDFFF;

/// Add a range of runes [lo, hi] to the tree without iterating through each code point.
/// This is the key optimization for negated character classes.
fn add_rune_pair_tree_entry(root: &mut RuneTreeNode, lo: char, hi: char, dest: &Arc<FaState>) {
    let lo_u32 = lo as u32;
    let hi_u32 = hi as u32;

    // Split by UTF-8 encoding boundaries and handle each segment
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

        // Skip surrogate range for 3-byte sequences
        if current <= SURROGATE_END && segment_end >= SURROGATE_START {
            // Handle pre-surrogate part
            if current < SURROGATE_START {
                let pre_end = (SURROGATE_START - 1).min(segment_end);
                if let (Some(start), Some(end)) = (char::from_u32(current), char::from_u32(pre_end))
                {
                    add_utf8_range_to_tree(root, start, end, dest);
                }
            }
            // Handle post-surrogate part
            if segment_end > SURROGATE_END {
                let post_start = (SURROGATE_END + 1).max(current);
                if let (Some(start), Some(end)) =
                    (char::from_u32(post_start), char::from_u32(segment_end))
                {
                    add_utf8_range_to_tree(root, start, end, dest);
                }
            }
        } else if let (Some(start), Some(end)) =
            (char::from_u32(current), char::from_u32(segment_end))
        {
            add_utf8_range_to_tree(root, start, end, dest);
        }

        current = segment_end + 1;
    }
}

/// Add a range of characters with the same UTF-8 encoding length to the tree.
/// This assumes lo and hi are both valid (non-surrogate) characters with the same encoding length.
fn add_utf8_range_to_tree(root: &mut RuneTreeNode, lo: char, hi: char, dest: &Arc<FaState>) {
    let lo_bytes = rune_to_utf8(lo);
    let hi_bytes = rune_to_utf8(hi);

    debug_assert_eq!(
        lo_bytes.len(),
        hi_bytes.len(),
        "lo and hi must have same UTF-8 length"
    );

    add_byte_range_recursive(root, &lo_bytes, &hi_bytes, 0, dest);
}

/// Recursively add a range of UTF-8 byte sequences to the tree.
///
/// This handles the case where lo_bytes[0..=idx] equals hi_bytes[0..=idx] (common prefix),
/// and the case where they differ (need to split into three parts: lo-to-max, middle, min-to-hi).
fn add_byte_range_recursive(
    node: &mut RuneTreeNode,
    lo_bytes: &[u8],
    hi_bytes: &[u8],
    idx: usize,
    dest: &Arc<FaState>,
) {
    if idx >= lo_bytes.len() {
        return;
    }

    let lo_byte = lo_bytes[idx];
    let hi_byte = hi_bytes[idx];
    let is_last = idx == lo_bytes.len() - 1;

    if lo_byte == hi_byte {
        // Same first byte - recurse with remaining bytes
        ensure_tree_entry(node, lo_byte);
        let entry = node[lo_byte as usize].as_mut().unwrap();

        if is_last {
            entry.next = Some(dest.clone());
        } else {
            if entry.child.is_none() {
                entry.child = Some(new_rune_tree_node());
            }
            add_byte_range_recursive(
                entry.child.as_mut().unwrap(),
                lo_bytes,
                hi_bytes,
                idx + 1,
                dest,
            );
        }
    } else {
        // Different bytes - split into three parts:
        // 1. lo_byte with lo's remaining bytes to max continuation bytes
        // 2. Middle bytes (lo_byte+1 to hi_byte-1) with full continuation range
        // 3. hi_byte with min continuation bytes to hi's remaining bytes

        // Part 1: lo_byte with remaining lo_bytes but max out continuations
        add_lo_range_to_tree(node, lo_bytes, idx, dest);

        // Part 2: Middle range with full continuation bytes
        if hi_byte > lo_byte + 1 {
            add_middle_range_to_tree(
                node,
                lo_byte + 1,
                hi_byte - 1,
                lo_bytes.len() - idx - 1,
                dest,
            );
        }

        // Part 3: hi_byte with min continuation bytes to hi_bytes
        add_hi_range_to_tree(node, hi_bytes, idx, dest);
    }
}

/// Add the lower bound part: lo_bytes[idx] with remaining bytes going up to max continuation.
fn add_lo_range_to_tree(node: &mut RuneTreeNode, lo_bytes: &[u8], idx: usize, dest: &Arc<FaState>) {
    let lo_byte = lo_bytes[idx];
    let is_last = idx == lo_bytes.len() - 1;

    ensure_tree_entry(node, lo_byte);
    let entry = node[lo_byte as usize].as_mut().unwrap();

    if is_last {
        entry.next = Some(dest.clone());
    } else {
        if entry.child.is_none() {
            entry.child = Some(new_rune_tree_node());
        }
        let child = entry.child.as_mut().unwrap();
        let next_byte = lo_bytes[idx + 1];

        // lo_bytes[idx+1] with remaining bytes to max
        add_lo_range_to_tree(child, lo_bytes, idx + 1, dest);

        // Bytes from lo_bytes[idx+1]+1 to 0xBF get full continuation range
        if next_byte < 0xBF {
            add_middle_range_to_tree(child, next_byte + 1, 0xBF, lo_bytes.len() - idx - 2, dest);
        }
    }
}

/// Add the upper bound part: hi_bytes[idx] with min continuation bytes to hi_bytes.
fn add_hi_range_to_tree(node: &mut RuneTreeNode, hi_bytes: &[u8], idx: usize, dest: &Arc<FaState>) {
    let hi_byte = hi_bytes[idx];
    let is_last = idx == hi_bytes.len() - 1;

    ensure_tree_entry(node, hi_byte);
    let entry = node[hi_byte as usize].as_mut().unwrap();

    if is_last {
        entry.next = Some(dest.clone());
    } else {
        if entry.child.is_none() {
            entry.child = Some(new_rune_tree_node());
        }
        let child = entry.child.as_mut().unwrap();
        let next_byte = hi_bytes[idx + 1];

        // Bytes from 0x80 to hi_bytes[idx+1]-1 get full continuation range
        if next_byte > 0x80 {
            add_middle_range_to_tree(child, 0x80, next_byte - 1, hi_bytes.len() - idx - 2, dest);
        }

        // hi_bytes[idx+1] with remaining bytes
        add_hi_range_to_tree(child, hi_bytes, idx + 1, dest);
    }
}

/// Add a range of bytes [lo, hi] that all have `depth` continuation bytes after them.
fn add_middle_range_to_tree(
    node: &mut RuneTreeNode,
    lo: u8,
    hi: u8,
    depth: usize,
    dest: &Arc<FaState>,
) {
    if depth == 0 {
        // These bytes go directly to dest
        for byte in lo..=hi {
            ensure_tree_entry(node, byte);
            node[byte as usize].as_mut().unwrap().next = Some(dest.clone());
        }
    } else {
        // These bytes need continuation byte subtrees
        for byte in lo..=hi {
            ensure_tree_entry(node, byte);
            let entry = node[byte as usize].as_mut().unwrap();
            if entry.child.is_none() {
                entry.child = Some(new_rune_tree_node());
            }
            // Full continuation range 0x80-0xBF
            add_middle_range_to_tree(entry.child.as_mut().unwrap(), 0x80, 0xBF, depth - 1, dest);
        }
    }
}

/// Ensure a tree entry exists at the given byte position.
fn ensure_tree_entry(node: &mut RuneTreeNode, byte: u8) {
    let idx = byte as usize;
    if node[idx].is_none() {
        node[idx] = Some(RuneTreeEntry {
            next: None,
            child: None,
        });
    }
}

/// Build a dot FA that matches any valid UTF-8 character.
///
/// This is more complex than just accepting all bytes because we need to
/// validate UTF-8 encoding and reject surrogates.
pub fn make_dot_fa(dest: &Arc<FaState>) -> SmallTable {
    // Tables for continuation bytes
    let s_last = SmallTable {
        ceilings: vec![0x80, 0xc0, BYTE_CEILING as u8],
        steps: vec![None, Some(dest.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_last = Arc::new(FaState::with_table(s_last));

    let s_last_inter = SmallTable {
        ceilings: vec![0x80, 0xc0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_last_inter = Arc::new(FaState::with_table(s_last_inter));

    let s_first_inter = SmallTable {
        ceilings: vec![0x80, 0xc0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last_inter.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_first_inter = Arc::new(FaState::with_table(s_first_inter));

    // Special handling for E0 (3-byte sequences starting 0xE0)
    let s_e0 = SmallTable {
        ceilings: vec![0xa0, 0xc0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_e0 = Arc::new(FaState::with_table(s_e0));

    // Special handling for ED (surrogates - reject 0xED 0xA0-0xBF)
    let s_ed = SmallTable {
        ceilings: vec![0x80, 0xA0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_ed = Arc::new(FaState::with_table(s_ed));

    // Special handling for F0 (4-byte sequences starting 0xF0)
    let s_f0 = SmallTable {
        ceilings: vec![0x90, 0xC0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last_inter.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_f0 = Arc::new(FaState::with_table(s_f0));

    // Special handling for F4 (max Unicode)
    let s_f4 = SmallTable {
        ceilings: vec![0x80, 0x90, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last_inter.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_f4 = Arc::new(FaState::with_table(s_f4));

    // Main dot table
    SmallTable {
        ceilings: vec![
            0x80,               // 0: ASCII (single byte)
            0xC2,               // 1: invalid continuation or overlong
            0xE0,               // 2: 2-byte sequences
            0xE1,               // 3: E0 special case
            0xED,               // 4: 3-byte sequences E1-EC
            0xEE,               // 5: ED special case (surrogates)
            0xF0,               // 6: 3-byte sequences EE-EF
            0xF1,               // 7: F0 special case
            0xF4,               // 8: 4-byte sequences F1-F3
            0xF5,               // 9: F4 special case
            BYTE_CEILING as u8, // 10: invalid
        ],
        steps: vec![
            Some(dest.clone()),               // 0: ASCII
            None,                             // 1: invalid
            Some(target_last.clone()),        // 2: 2-byte
            Some(target_e0.clone()),          // 3: E0
            Some(target_last_inter.clone()),  // 4: E1-EC
            Some(target_ed.clone()),          // 5: ED
            Some(target_last_inter.clone()),  // 6: EE-EF
            Some(target_f0.clone()),          // 7: F0
            Some(target_first_inter.clone()), // 8: F1-F3
            Some(target_f4.clone()),          // 9: F4
            None,                             // 10: invalid
        ],
        epsilons: Vec::new(),
        spinout: None,
    }
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
/// This is more efficient than the chain-based approach for patterns with `*` and `+`
/// quantifiers because it uses true cyclic structures (4 states) instead of chained
/// states (100+ states).
///
/// # Arguments
/// * `root` - The parsed regexp tree
/// * `for_field` - If true, add " matching at start/end for field values
///
/// # Returns
/// A tuple of (StateArena, start_state_id, FieldMatcher)
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
    arena[start].table.epsilons = branch_starts;

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
    let start = make_arena_atom_fa_to(qa, arena, loopback);

    // Set up loopback's epsilons: to exit AND back to start (CYCLE!)
    arena[loopback].table.epsilons = vec![exit_state, start];

    // For *, add epsilon from start to exit (can skip entirely)
    if is_star {
        arena[start].table.epsilons.push(exit_state);
    }

    start
}

/// Build arena FA for a single quantified atom.
fn make_arena_atom_fa(qa: &QuantifiedAtom, arena: &mut StateArena, next_step: StateId) -> StateId {
    make_arena_atom_fa_to(qa, arena, next_step)
}

/// Build arena FA for a single atom, transitioning to a specific target state.
fn make_arena_atom_fa_to(qa: &QuantifiedAtom, arena: &mut StateArena, next: StateId) -> StateId {
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
    // For simplicity, use the same structure as SmallTable's dot FA
    // but with arena states. This matches any valid UTF-8 character.

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
///
/// This optimized version adds entire RunePairs at once instead of iterating
/// through individual code points. This dramatically improves performance for
/// negated character classes like `[^abc]` which cover ~1.1M Unicode code points.
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
