//! NFA building for regexp matching.
//!
//! This module builds arena-based finite automata from parsed regexp trees.
//! The arena-based approach provides better cache locality and supports
//! patterns with + or * quantifiers efficiently.

use std::cell::RefCell;
use std::sync::Arc;

use rustc_hash::FxHashMap;

use smallvec::{SmallVec, smallvec};

use crate::automaton::{
    BYTE_CEILING, FieldMatcher,
    arena::{ARENA_VALUE_TERMINATOR, SmallTable, StateArena, StateId},
};

use super::parser::{
    Branch as RegexpBranch, QuantifiedAtom, REGEXP_QUANTIFIER_MAX, Root as RegexpRoot, RuneRange,
};

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
#[must_use]
pub fn regexp_has_plus_star(root: &RegexpRoot) -> bool {
    for branch in root {
        for qa in branch {
            if qa.is_plus() || qa.is_star() {
                return true;
            }
            // Recursively check subtrees (parenthesized groups)
            if let Some(ref subtree) = qa.subtree
                && regexp_has_plus_star(subtree)
            {
                return true;
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
/// The NFA always includes leading and trailing `"` transitions to match
/// JSON string values as they appear from the flattener (with surrounding quotes).
///
/// # Returns
/// A tuple of (arena, start_state_id, field_matcher)
#[must_use]
pub fn make_regexp_nfa_arena(root: RegexpRoot) -> (StateArena, StateId, Arc<FieldMatcher>) {
    let next_field = Arc::new(FieldMatcher::new());

    // Handle empty regexp specially - it matches only the empty string
    let (mut arena, start) = if root.is_empty() {
        let mut arena = StateArena::with_capacity(4);

        // Create match state
        let match_state = arena.alloc();
        arena[match_state]
            .field_transitions
            .push(next_field.clone());

        // Create VALUE_TERMINATOR transition state
        let vt_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));

        // With quotes: start → " → closing_quote → " → vt_state
        // Matches the empty string value "" (two quote bytes)
        let closing_quote =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"\"", &[vt_state]));
        let start = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            b"\"",
            &[closing_quote],
        ));
        (arena, start)
    } else {
        // Build the arena NFA
        let mut arena = StateArena::with_capacity(16);

        // Create match state (reached at end of value)
        let match_state = arena.alloc();
        arena[match_state]
            .field_transitions
            .push(next_field.clone());

        // Create VALUE_TERMINATOR transition state
        let vt_state = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));

        // Add trailing quote: regexp content → " → VALUE_TERMINATOR → match
        let next_step =
            arena.alloc_with_table(SmallTable::with_mappings(StateId::NONE, b"\"", &[vt_state]));

        // Build the NFA from branches
        let branch_start = make_arena_nfa_from_branches(&root, &mut arena, next_step);

        // Wrap with leading quote at the top level only
        let start = arena.alloc_with_table(SmallTable::with_mappings(
            StateId::NONE,
            b"\"",
            &[branch_start],
        ));

        (arena, start)
    };

    arena.precompute_epsilon_closures();
    (arena, start, next_field)
}

/// Build arena NFA from branches (alternatives).
///
/// This function does NOT add quote wrapping — that is handled by the
/// top-level `make_regexp_nfa_arena` caller. Subtrees (groups) also call
/// this function and must not add quotes.
fn make_arena_nfa_from_branches(
    root: &RegexpRoot,
    arena: &mut StateArena,
    next_step: StateId,
) -> StateId {
    if root.is_empty() {
        return next_step;
    }

    if root.len() == 1 {
        // Single branch - no alternation needed
        return make_one_arena_branch_fa(&root[0], arena, next_step);
    }

    // Multiple branches - create a start state with epsilons to each branch
    let mut branch_starts = Vec::with_capacity(root.len());
    for branch in root {
        if branch.is_empty() {
            // Empty branch means we can skip directly to next_step
            branch_starts.push(next_step);
        } else {
            let branch_start = make_one_arena_branch_fa(branch, arena, next_step);
            branch_starts.push(branch_start);
        }
    }

    // Create a start state that has epsilons to all branch starts
    let start = arena.alloc();
    arena[start].table.epsilons = SmallVec::from_vec(branch_starts);

    start
}

/// Build arena NFA for one branch (sequence of atoms).
fn make_one_arena_branch_fa(
    branch: &RegexpBranch,
    arena: &mut StateArena,
    next_step: StateId,
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
            // General {n,m} quantifier. The parser only ever stores
            // non-negative counts here; if that ever stops being true we'd
            // rather panic than silently turn a stray -1 into a 4-billion-
            // iteration loop.
            let n = usize::try_from(qa.quant_min).expect("quant_min must be non-negative");
            let m = usize::try_from(qa.quant_max).expect("quant_max must be non-negative");

            // This expansion allocates one state per repetition, so it relies
            // on the parser bounding both counts. Note that quant_max ==
            // REGEXP_QUANTIFIER_MAX does NOT imply + or *: `{n,}` and
            // `{n,100}` land here whenever n > 1.
            debug_assert!(
                qa.quant_max <= REGEXP_QUANTIFIER_MAX,
                "parser must bound quantifier repetition counts"
            );

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

    current_next
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
    let accel = qa.ascii_negated_bytes.as_ref().map(|bytes| {
        // ASCII-only negated pattern: feed the negated bytes straight in
        // as exit bytes. `detect_ascii_negated_bytes` already declines
        // anything with more than 3 entries, so neither the `try_from`
        // nor the indexed write can overflow.
        let mut accel = crate::automaton::AccelInfo {
            exit_bytes: [0; 3],
            len: u8::try_from(bytes.len()).expect("ascii_negated_bytes is bounded to 3"),
        };
        for (i, &b) in bytes.iter().enumerate() {
            accel.exit_bytes[i] = b;
        }
        accel
    });

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
        make_arena_nfa_from_branches(subtree, arena, next)
    } else if let Some(ref cache_key) = qa.cache_key {
        // Use compact non-word char FA for word boundary expansion
        if cache_key == "wb_W" {
            return make_nonword_char_fa(arena, next);
        }
        make_cached_rune_range_fa(cache_key, &qa.runes, arena, next)
    } else {
        make_arena_rune_range_fa(&qa.runes, arena, next)
    }
}

/// Build an arena FA matching any single UTF-8 character, with an optional
/// ASCII filter applied to single-byte (0x00-0x7F) transitions.
///
/// If `ascii_filter` is None, all ASCII bytes transition to `dest` (dot behavior).
/// If provided, the filter is called with the pre-filled ASCII unpacked array to
/// exclude specific bytes (e.g., word chars for `~W`).
#[allow(clippy::type_complexity)]
// `target_e0`/`ed`/`f0`/`f4` name UTF-8 lead-byte handlers; the suffixes
// match the codepoint range each guards.
#[allow(clippy::similar_names)]
fn make_utf8_char_fa(
    arena: &mut StateArena,
    dest: StateId,
    ascii_filter: Option<&dyn Fn(&mut [StateId; BYTE_CEILING])>,
) -> StateId {
    // Continuation byte states for multi-byte UTF-8 sequences
    let s_last = arena.alloc_with_table({
        let mut table = SmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(dest);
        table.pack(&unpacked);
        table
    });

    let s_last_inter = arena.alloc_with_table({
        let mut table = SmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let s_first_inter = arena.alloc_with_table({
        let mut table = SmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    // Lead byte handler states for restricted continuation ranges
    let target_e0 = arena.alloc_with_table({
        let mut table = SmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0xA0..0xC0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let target_ed = arena.alloc_with_table({
        let mut table = SmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xA0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let target_f0 = arena.alloc_with_table({
        let mut table = SmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x90..0xC0].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    let target_f4 = arena.alloc_with_table({
        let mut table = SmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0x90].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    // Main state
    arena.alloc_with_table({
        let mut unpacked = [StateId::NONE; BYTE_CEILING];

        // ASCII (0x00-0x7F) -> dest
        unpacked[..0x80].fill(dest);

        // Apply optional ASCII filter (e.g., exclude word chars)
        if let Some(filter) = ascii_filter {
            filter(&mut unpacked);
        }

        // Multi-byte lead bytes
        unpacked[0xC2..0xE0].fill(s_last);
        unpacked[0xE0] = target_e0;
        unpacked[0xE1..0xED].fill(s_last_inter);
        unpacked[0xED] = target_ed;
        unpacked[0xEE..0xF0].fill(s_last_inter);
        unpacked[0xF0] = target_f0;
        unpacked[0xF1..0xF4].fill(s_first_inter);
        unpacked[0xF4] = target_f4;

        let mut table = SmallTable::new();
        table.pack(&unpacked);
        table
    })
}

/// Build arena FA for a dot (any character).
fn make_arena_dot_fa(arena: &mut StateArena, dest: StateId) -> StateId {
    make_utf8_char_fa(arena, dest, None)
}

/// Build arena FA for a non-word character (`~W`).
///
/// Like dot but excludes word char bytes (a-z, A-Z, 0-9, _) from ASCII transitions.
/// Multi-byte UTF-8 sequences are all non-word chars (word chars are ASCII-only).
pub fn make_nonword_char_fa(arena: &mut StateArena, dest: StateId) -> StateId {
    make_utf8_char_fa(
        arena,
        dest,
        Some(&|unpacked| {
            for b in b'a'..=b'z' {
                unpacked[b as usize] = StateId::NONE;
            }
            for b in b'A'..=b'Z' {
                unpacked[b as usize] = StateId::NONE;
            }
            for b in b'0'..=b'9' {
                unpacked[b as usize] = StateId::NONE;
            }
            unpacked[b'_' as usize] = StateId::NONE;
        }),
    )
}

// ============================================================================
// FA Shell Caching for Unicode Properties
// ============================================================================

/// A cached "shell" FA for a Unicode property (e.g., `~p{L}`, `~p{Nd}`).
///
/// The shell contains a set of `SmallTable`s built from the property's
/// rune ranges, with a placeholder destination at local index 0. To use the
/// shell, `instantiate_shell` clones the tables into a real `StateArena`,
/// remapping the placeholder to the actual next state.
struct CachedShell {
    /// Tables indexed by local ID; table at index 0 is the placeholder destination.
    tables: Vec<SmallTable>,
    /// Local index of the root state in `tables`.
    root: u32,
}

thread_local! {
    static FA_SHELL_CACHE: RefCell<FxHashMap<String, CachedShell>> = RefCell::new(FxHashMap::default());
}

/// Build a shell FA from rune ranges, using a placeholder destination (local index 0).
fn build_shell(rr: &RuneRange) -> CachedShell {
    let mut temp_arena = StateArena::with_capacity(16);

    // Local index 0 = placeholder destination
    let placeholder = temp_arena.alloc();

    // Build the rune range FA with the placeholder as the destination
    let root_id = make_arena_rune_range_fa(rr, &mut temp_arena, placeholder);

    // Extract all tables from the temp arena
    let mut tables = Vec::with_capacity(temp_arena.len());
    for i in 0..temp_arena.len() {
        let id = StateId::from_index(i);
        tables.push(temp_arena[id].table.clone());
    }

    CachedShell {
        tables,
        root: u32::try_from(root_id.index())
            .expect("shell arenas have far fewer than u32::MAX states"),
    }
}

/// Instantiate a cached shell into a real arena, replacing the placeholder
/// (local index 0) with `next` and allocating fresh states for all others.
fn instantiate_shell(shell: &CachedShell, arena: &mut StateArena, next: StateId) -> StateId {
    // Build local-to-real ID mapping
    let mut id_map: Vec<StateId> = Vec::with_capacity(shell.tables.len());
    // Local index 0 (placeholder) maps to the real next state
    id_map.push(next);
    // Allocate real states for all other locals
    for _ in 1..shell.tables.len() {
        id_map.push(arena.alloc());
    }

    // Clone each non-placeholder table, remapping all StateId references
    for (local_idx, src_table) in shell.tables.iter().enumerate() {
        if local_idx == 0 {
            // Placeholder — don't write into the real `next` state
            continue;
        }

        let real_id = id_map[local_idx];
        let mut table = src_table.clone();

        // Remap steps
        for step in &mut table.steps {
            if !step.is_none() {
                *step = id_map[step.index()];
            }
        }

        // Remap epsilons. Cached shells (built by `build_shell`) never insert
        // NONE epsilon entries.
        for eps in &mut table.epsilons {
            debug_assert!(
                !eps.is_none(),
                "cached shell epsilons must not contain NONE entries"
            );
            *eps = id_map[eps.index()];
        }

        arena[real_id].table = table;
    }

    id_map[shell.root as usize]
}

/// Build a rune range FA using the shell cache when a cache key is available.
///
/// On cache hit, instantiates from the cached shell. On miss, builds the shell,
/// caches it, then instantiates.
fn make_cached_rune_range_fa(
    cache_key: &str,
    rr: &RuneRange,
    arena: &mut StateArena,
    next: StateId,
) -> StateId {
    FA_SHELL_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some(shell) = cache.get(cache_key) {
            return instantiate_shell(shell, arena, next);
        }

        let shell = build_shell(rr);
        let result = instantiate_shell(&shell, arena, next);
        cache.insert(cache_key.to_string(), shell);
        result
    })
}

/// Clear the FA shell cache. Useful for testing to ensure isolation between tests.
pub fn clear_fa_shell_cache() {
    FA_SHELL_CACHE.with(|cache| {
        cache.borrow_mut().clear();
    });
}

/// Returns the current size of the FA shell cache. Test-only helper used to
/// observe the side-effect of `clear_fa_shell_cache`.
#[cfg(test)]
pub fn fa_shell_cache_size() -> usize {
    FA_SHELL_CACHE.with(|cache| cache.borrow().len())
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

    let mut table = SmallTable::new();
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

        if intersects_surrogate(current, segment_end) {
            if before_surrogate(current) {
                let pre_end = (SURROGATE_START - 1).min(segment_end);
                if let (Some(start), Some(end)) = (char::from_u32(current), char::from_u32(pre_end))
                {
                    add_arena_utf8_range_to_tree(root, start, end, dest);
                }
            }
            if after_surrogate(segment_end) {
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

        add_arena_middle_range_to_tree(
            node,
            lo_byte + 1,
            hi_byte - 1,
            remaining_byte_depth(lo_bytes.len(), idx, 1),
            dest,
        );

        add_arena_hi_range_to_tree(node, hi_bytes, idx, dest);
    }
}

/// Returns the number of remaining continuation bytes after peeling `idx + offset`
/// bytes from a `byte_len`-byte UTF-8 sequence, for use as the `depth` argument
/// to `add_arena_middle_range_to_tree`.
const fn remaining_byte_depth(byte_len: usize, idx: usize, offset: usize) -> usize {
    byte_len - (idx + offset)
}

/// Returns true when a `[current, segment_end]` codepoint range overlaps the
/// UTF-16 surrogate window (`SURROGATE_START..=SURROGATE_END`, U+D800..=U+DFFF).
const fn intersects_surrogate(current: u32, segment_end: u32) -> bool {
    current <= SURROGATE_END && segment_end >= SURROGATE_START
}

/// Returns true when `current` lies strictly before the surrogate window —
/// the pre-surrogate slice of an intersecting range.
const fn before_surrogate(current: u32) -> bool {
    current < SURROGATE_START
}

/// Returns true when `segment_end` lies strictly after the surrogate window —
/// the post-surrogate slice of an intersecting range.
const fn after_surrogate(segment_end: u32) -> bool {
    segment_end > SURROGATE_END
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

        add_arena_middle_range_to_tree(
            child,
            next_byte.wrapping_add(1),
            0xBF,
            remaining_byte_depth(lo_bytes.len(), idx, 2),
            dest,
        );
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

        add_arena_middle_range_to_tree(
            child,
            0x80,
            next_byte.wrapping_sub(1),
            remaining_byte_depth(hi_bytes.len(), idx, 2),
            dest,
        );

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
    // UTF-8 encodes at most 4 bytes, so the residual depth after peeling the
    // first byte is at most 3. Depth growing beyond this would cause
    // exponential state expansion.
    debug_assert!(
        depth <= 3,
        "rune-range middle depth bounded by UTF-8 byte length, got {depth}"
    );
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remaining_byte_depth() {
        // Verifies the residual-depth formula for 2-, 3-, and 4-byte UTF-8 encodings
        // across all recursion indices and per-call offsets used by the callers.
        // byte_len=4, idx=0..=3, offset=1
        assert_eq!(remaining_byte_depth(4, 0, 1), 3);
        assert_eq!(remaining_byte_depth(4, 1, 1), 2);
        assert_eq!(remaining_byte_depth(4, 2, 1), 1);
        assert_eq!(remaining_byte_depth(4, 3, 1), 0);
        // byte_len=4, idx=0..=2, offset=2
        assert_eq!(remaining_byte_depth(4, 0, 2), 2);
        assert_eq!(remaining_byte_depth(4, 1, 2), 1);
        assert_eq!(remaining_byte_depth(4, 2, 2), 0);
        // byte_len=2 (a 2-byte UTF-8 sequence), idx=0, offset=1
        assert_eq!(remaining_byte_depth(2, 0, 1), 1);
        assert_eq!(remaining_byte_depth(3, 1, 2), 0);
    }

    #[test]
    fn test_intersects_surrogate_boundary() {
        // Range strictly below the surrogate window.
        assert!(!intersects_surrogate(0, 0x100));
        assert!(!intersects_surrogate(0, SURROGATE_START - 1));
        // Range touches the lower edge.
        assert!(intersects_surrogate(0, SURROGATE_START));
        assert!(intersects_surrogate(SURROGATE_START, SURROGATE_END));
        // Range strictly above.
        assert!(!intersects_surrogate(SURROGATE_END + 1, 0xFFFF));
        // Range entirely surrounds the window.
        assert!(intersects_surrogate(0, 0xFFFF));
    }

    // Populate the per-thread shell cache by building a cache-keyed regex, then
    // verify `clear_fa_shell_cache` empties it. Shared by the native and
    // Miri-friendly tests, which differ only in the size of the cached category.
    fn verify_shell_cache_populate_then_clear(pattern: &str) {
        clear_fa_shell_cache();
        assert_eq!(fa_shell_cache_size(), 0);
        let root = crate::regexp::parse_regexp(pattern).unwrap();
        let _ = make_regexp_nfa_arena(root);
        let populated = fa_shell_cache_size();
        assert!(
            populated > 0,
            "building a cache-keyed regex must populate the cache"
        );
        clear_fa_shell_cache();
        assert_eq!(
            fa_shell_cache_size(),
            0,
            "clear_fa_shell_cache must drop all entries"
        );
    }

    // MIRI SKIP RATIONALE: `~i` is the XML-name-start Unicode category (~700
    // rune ranges); building its NFA takes ~58 min under Miri. Coverage: the
    // Miri-friendly variant below drives the same cache populate/clear path
    // with the tiny `~p{Cc}` (control-char) category.
    #[test]
    #[cfg_attr(miri, ignore)]
    fn test_clear_fa_shell_cache_drops_entries() {
        verify_shell_cache_populate_then_clear("~i");
    }

    /// Miri-friendly version — `~p{Cc}` is a two-range cache-keyed category, so
    /// the cache lifecycle is exercised without the large `~i` NFA build.
    #[cfg(miri)]
    #[test]
    fn test_clear_fa_shell_cache_drops_entries_miri_friendly() {
        verify_shell_cache_populate_then_clear("~p{Cc}");
    }

    #[test]
    fn test_before_after_surrogate() {
        // The surrogate window (U+D800..=U+DFFF) is invalid UTF-8, so codepoint
        // ranges that span it must be split at the window edges. These helpers
        // identify the pre-window and post-window slices; the comparisons are
        // strict (< / >), so D800 itself is not "before" and DFFF is not "after".
        assert!(before_surrogate(SURROGATE_START - 1));
        assert!(!before_surrogate(SURROGATE_START));
        assert!(!before_surrogate(SURROGATE_END));
        assert!(after_surrogate(SURROGATE_END + 1));
        assert!(!after_surrogate(SURROGATE_END));
        assert!(!after_surrogate(SURROGATE_START));
    }
}
