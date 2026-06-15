use super::sparse_set::SparseSet;
use super::*;

/// Shorthand for constructing an `EventField` for tests.
fn field(path: &str, value: &str) -> EventField {
    EventField {
        path: path.to_string(),
        value: value.to_string(),
        array_trail: vec![],
        is_number: false,
    }
}

// ========================================================================
// AutomatonValueMatcher Tests (arena-based)
// ========================================================================

#[test]
fn test_automaton_value_matcher_string() {
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_string_match(b"hello", "p1".to_string());
    matcher.add_string_match(b"world", "p2".to_string());

    let pattern_ids = matcher.match_value(b"hello");
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"p1".to_string()));

    let pattern_ids = matcher.match_value(b"world");
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"p2".to_string()));

    let pattern_ids = matcher.match_value(b"foo");
    assert!(pattern_ids.is_empty());
}

#[test]
fn test_automaton_value_matcher_prefix() {
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_prefix_match(b"prod-", "p1".to_string());
    matcher.add_prefix_match(b"test-", "p2".to_string());

    let pattern_ids = matcher.match_value(b"prod-123");
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"p1".to_string()));

    let pattern_ids = matcher.match_value(b"test-abc");
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"p2".to_string()));

    let pattern_ids = matcher.match_value(b"dev-xyz");
    assert!(pattern_ids.is_empty());
}

#[test]
fn test_automaton_value_matcher_shellstyle_single() {
    // Test with a single shellstyle pattern (no merging)
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_shellstyle_match(b"*.txt", "p1".to_string());

    let pattern_ids = matcher.match_value(b"file.txt");
    assert!(
        pattern_ids.contains(&"p1".to_string()),
        "file.txt should match *.txt"
    );

    let pattern_ids = matcher.match_value(b".txt");
    assert!(
        pattern_ids.contains(&"p1".to_string()),
        ".txt should match *.txt"
    );

    let pattern_ids = matcher.match_value(b"foo");
    assert!(pattern_ids.is_empty(), "foo should not match *.txt");
}

#[test]
fn test_automaton_value_matcher_shellstyle_multiple() {
    // Test with multiple shellstyle patterns (with merging)
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_shellstyle_match(b"*.txt", "p1".to_string());
    matcher.add_shellstyle_match(b"test*", "p2".to_string());

    let pattern_ids = matcher.match_value(b"random");
    assert!(
        pattern_ids.is_empty(),
        "random should not match any pattern"
    );
}

#[test]
fn test_automaton_value_matcher_mixed() {
    // Test mixing different pattern types
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_string_match(b"exact", "exact_match".to_string());
    matcher.add_prefix_match(b"pre-", "prefix_match".to_string());

    let pattern_ids = matcher.match_value(b"exact");
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"exact_match".to_string()));

    let pattern_ids = matcher.match_value(b"pre-fix");
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"prefix_match".to_string()));
}

// ========================================================================
// SmallTable Tests (ported from chain SmallTable tests)
// ========================================================================

#[test]
fn test_arena_small_table_step() {
    use arena::SmallTable;

    let table = SmallTable::new();

    // Test that all valid bytes return NONE for empty table
    for b in 0..BYTE_CEILING_U8 {
        assert!(
            table.dstep(b).is_none(),
            "byte {b} should have no transition in empty table"
        );
    }
    assert!(
        table.epsilons.is_empty(),
        "empty table should have no epsilons"
    );
}

#[test]
fn test_arena_small_table_with_mappings() {
    use arena::{SmallTable, StateArena, StateId};
    use std::sync::Arc;

    let mut arena = StateArena::new();
    let next_field = Arc::new(FieldMatcher::new());
    let next_state = arena.alloc_with_table(SmallTable::new());
    arena[next_state].field_transitions.push(next_field);

    let table = SmallTable::with_mappings(StateId::NONE, b"ab", &[next_state, next_state]);

    assert!(
        !table.dstep(b'a').is_none(),
        "byte 'a' should have a transition"
    );
    assert!(
        !table.dstep(b'b').is_none(),
        "byte 'b' should have a transition"
    );
    assert!(
        table.dstep(b'c').is_none(),
        "byte 'c' should have no transition"
    );
}

// ========================================================================
// CoreMatcher Tests
// ========================================================================

#[test]
fn test_core_matcher_single_field_exact() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    // Add pattern: {"status": ["active"]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Exact("active".to_string())],
            )],
        )
        .unwrap();

    // Create event fields (sorted by path)
    let fields = vec![field("status", "active")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"p1".to_string()));
}

#[test]
fn test_core_matcher_single_field_no_match() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    matcher
        .add_pattern(
            "p1".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Exact("active".to_string())],
            )],
        )
        .unwrap();

    let fields = vec![field("status", "inactive")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert!(pattern_ids.is_empty());
}

#[test]
fn test_core_matcher_exists_true() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    // Pattern: {"name": [{"exists": true}]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[("name".to_string(), vec![Matcher::Exists(true)])],
        )
        .unwrap();

    // Event with name field present
    let fields = vec![field("name", "anything")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(
        pattern_ids.len(),
        1,
        "exists:true should match when field exists"
    );
}

#[test]
fn test_core_matcher_exists_false() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    // Pattern: {"name": [{"exists": false}]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[("name".to_string(), vec![Matcher::Exists(false)])],
        )
        .unwrap();

    // Event without name field
    let fields = vec![field("other", "value")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(
        pattern_ids.len(),
        1,
        "exists:false should match when field is absent"
    );
}

#[test]
fn test_core_matcher_multi_field_and() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    // Pattern: {"status": ["active"], "type": ["user"]}
    // Both fields must match (AND semantics)
    matcher
        .add_pattern(
            "p1".to_string(),
            &[
                (
                    "status".to_string(),
                    vec![Matcher::Exact("active".to_string())],
                ),
                ("type".to_string(), vec![Matcher::Exact("user".to_string())]),
            ],
        )
        .unwrap();

    // Event with both fields matching
    let fields = vec![field("status", "active"), field("type", "user")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(
        pattern_ids.len(),
        1,
        "multi-field AND should match when all fields match"
    );
}

#[test]
fn test_core_matcher_multi_field_partial_no_match() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    // Pattern: {"status": ["active"], "type": ["user"]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[
                (
                    "status".to_string(),
                    vec![Matcher::Exact("active".to_string())],
                ),
                ("type".to_string(), vec![Matcher::Exact("user".to_string())]),
            ],
        )
        .unwrap();

    // Event with only status matching
    let fields = vec![field("status", "active"), field("type", "admin")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert!(
        pattern_ids.is_empty(),
        "multi-field AND should not match with partial field match"
    );
}

#[test]
fn test_core_matcher_or_within_field() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    // Pattern: {"status": ["active", "pending"]} - OR within field
    matcher
        .add_pattern(
            "p1".to_string(),
            &[(
                "status".to_string(),
                vec![
                    Matcher::Exact("active".to_string()),
                    Matcher::Exact("pending".to_string()),
                ],
            )],
        )
        .unwrap();

    // Should match "active"
    let fields1 = vec![field("status", "active")];
    let matches1 = matcher.matches_for_fields(&fields1);
    assert_eq!(matches1.len(), 1, "OR within field should match 'active'");

    // Should match "pending"
    let fields2 = vec![field("status", "pending")];
    let matches2 = matcher.matches_for_fields(&fields2);
    assert_eq!(matches2.len(), 1, "OR within field should match 'pending'");

    // Should not match "completed"
    let fields3 = vec![field("status", "completed")];
    let matches3 = matcher.matches_for_fields(&fields3);
    assert!(
        matches3.is_empty(),
        "OR within field should not match 'completed'"
    );
}

#[test]
fn test_core_matcher_multiple_patterns() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    // Pattern 1: {"status": ["active"]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Exact("active".to_string())],
            )],
        )
        .unwrap();

    // Pattern 2: {"status": ["pending"]}
    matcher
        .add_pattern(
            "p2".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Exact("pending".to_string())],
            )],
        )
        .unwrap();

    // Should match p1 only
    let fields = vec![field("status", "active")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"p1".to_string()));
}

// ========================================================================
// ThreadSafeCoreMatcher Tests
// ========================================================================

#[test]
fn test_thread_safe_core_matcher_send_sync() {
    // Compile-time check that ThreadSafeCoreMatcher is Send + Sync
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<ThreadSafeCoreMatcher<String>>();
}

#[test]
fn test_thread_safe_core_matcher_single_field() {
    use crate::json::Matcher;

    let matcher: ThreadSafeCoreMatcher<String> = ThreadSafeCoreMatcher::new();

    // Add pattern: {"status": ["active"]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Exact("active".to_string())],
            )],
        )
        .unwrap();

    // Create event fields
    let fields = vec![field("status", "active")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"p1".to_string()));
}

#[test]
fn test_thread_safe_core_matcher_no_match() {
    use crate::json::Matcher;

    let matcher: ThreadSafeCoreMatcher<String> = ThreadSafeCoreMatcher::new();

    matcher
        .add_pattern(
            "p1".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Exact("active".to_string())],
            )],
        )
        .unwrap();

    let fields = vec![field("status", "inactive")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert!(pattern_ids.is_empty());
}

#[test]
fn test_thread_safe_core_matcher_exists_true() {
    use crate::json::Matcher;

    let matcher: ThreadSafeCoreMatcher<String> = ThreadSafeCoreMatcher::new();

    // Pattern: {"name": [{"exists": true}]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[("name".to_string(), vec![Matcher::Exists(true)])],
        )
        .unwrap();

    // Event with name field present
    let fields = vec![field("name", "anything")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(
        pattern_ids.len(),
        1,
        "exists:true should match when field exists"
    );
}

#[test]
fn test_thread_safe_core_matcher_exists_false() {
    use crate::json::Matcher;

    let matcher: ThreadSafeCoreMatcher<String> = ThreadSafeCoreMatcher::new();

    // Pattern: {"name": [{"exists": false}]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[("name".to_string(), vec![Matcher::Exists(false)])],
        )
        .unwrap();

    // Event without name field
    let fields = vec![field("other", "value")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(
        pattern_ids.len(),
        1,
        "exists:false should match when field is absent"
    );
}

#[test]
fn test_thread_safe_core_matcher_multiple_patterns() {
    use crate::json::Matcher;

    let matcher: ThreadSafeCoreMatcher<String> = ThreadSafeCoreMatcher::new();

    // Pattern 1: {"status": ["active"]}
    matcher
        .add_pattern(
            "p1".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Exact("active".to_string())],
            )],
        )
        .unwrap();

    // Pattern 2: {"status": ["pending"]}
    matcher
        .add_pattern(
            "p2".to_string(),
            &[(
                "status".to_string(),
                vec![Matcher::Exact("pending".to_string())],
            )],
        )
        .unwrap();

    // Should match p1 only
    let fields = vec![field("status", "active")];

    let pattern_ids = matcher.matches_for_fields(&fields);
    assert_eq!(pattern_ids.len(), 1);
    assert!(pattern_ids.contains(&"p1".to_string()));
}

#[test]
fn test_nfa_buffers_clear() {
    let mut bufs = small_table::NfaBuffers::new();
    // Populate the inner arena buffers
    bufs.arena_bufs
        .current_states
        .push(arena::StateId::from_index(1));
    bufs.arena_bufs
        .next_states
        .push(arena::StateId::from_index(2));
    bufs.arena_bufs.transitions.push(42);

    bufs.clear();

    assert!(bufs.arena_bufs.current_states.is_empty());
    assert!(bufs.arena_bufs.next_states.is_empty());
    assert!(bufs.arena_bufs.transitions.is_empty());
}

/// Verify that `NfaBuffers::clear` (called by `traverse_arena_nfa`) prevents
/// stale state from leaking across successive match calls.
///
/// `CoreMatcher::matches_for_fields` reuses per-thread `NfaBuffers`, and
/// `MutableValueMatcher::transition_on` ignores that parameter (`_bufs`) anyway,
/// using its own `arena_bufs: RefCell<NfaBuffers>` which persists across calls.
/// `traverse_arena_nfa` calls `NfaBuffers::clear()` at the start of each
/// traversal — without it, stale transitions from one traversal would leak into
/// the next and produce false pattern_ids.
///
/// We use a `Shellstyle` pattern (not `Exact`) because `Exact` takes the singleton
/// fast path in `MutableValueMatcher::transition_on` and never touches `arena_bufs`.
/// `Shellstyle` patterns set `main_arena_is_nfa = true`, routing through
/// `traverse_arena_nfa` with the persistent `arena_bufs`.
#[test]
fn test_arena_nfa_bufs_clear_observable_through_matching() {
    use crate::json::Matcher;

    let matcher: CoreMatcher<String> = CoreMatcher::new();

    // Shellstyle "r*" pattern_ids any value starting with 'r'.
    // This avoids the singleton fast path and routes through the arena NFA.
    matcher
        .add_pattern(
            "p1".to_string(),
            &[(
                "color".to_string(),
                vec![Matcher::Shellstyle("r*".to_string())],
            )],
        )
        .unwrap();

    // Values must be quoted to match the arena FA format — Shellstyle patterns
    // are quote_wrap'd during add_transition, and the flattener retains JSON
    // quotes on string values.
    //
    // First match — "red" starts with 'r', should find p1
    let fields1 = vec![field("color", "\"red\"")];
    let matches1 = matcher.matches_for_fields(&fields1);
    assert_eq!(matches1.len(), 1);
    assert!(matches1.contains(&"p1".to_string()));

    // Second match with a non-matching value — must be empty.
    // MutableValueMatcher's persistent arena_bufs still holds transitions from
    // the "red" traversal. If NfaBuffers::clear() (called at the start of
    // traverse_arena_nfa) were a no-op, those stale transitions would cause a
    // false match here.
    let fields2 = vec![field("color", "\"blue\"")];
    let matches2 = matcher.matches_for_fields(&fields2);
    assert!(
        matches2.is_empty(),
        "stale arena NFA buffers should not cause false pattern_ids"
    );
}

// ========================================================================
// FieldMatcher unit tests
// ========================================================================

/// Verify that `FieldMatcher::with_match_id` stores the given ID as `Some(id)`.
/// If with_match_id were replaced with `Default::default()`, match_id would
/// be `None` and this test would fail.
#[test]
fn test_field_matcher_with_match_id_stores_id() {
    let fm = small_table::FieldMatcher::with_match_id(42);
    assert_eq!(
        fm.match_id,
        Some(42),
        "with_match_id(42) must set match_id to Some(42)"
    );
}

/// Verify that `with_match_id` works for different IDs and doesn't
/// confuse them with Default.
#[test]
fn test_field_matcher_with_match_id_vs_default() {
    let fm_with_id = small_table::FieldMatcher::with_match_id(99);
    let fm_default = small_table::FieldMatcher::new();

    assert_eq!(fm_with_id.match_id, Some(99));
    assert_eq!(fm_default.match_id, None);
}

// ========================================================================
// SparseSet tests
// ========================================================================

/// capacity() must reflect the value passed to new().
#[test]
fn test_sparse_set_capacity_returns_correct_value() {
    let set = SparseSet::new(5);
    assert_eq!(set.capacity(), 5);

    let set2 = SparseSet::new(2);
    assert_eq!(set2.capacity(), 2);
}

/// insert() returns true for new elements, false for duplicates.
#[test]
fn test_sparse_set_insert_return_value() {
    let mut set = SparseSet::new(10);
    assert!(set.insert(3));
    assert!(!set.insert(3));
}

/// Successive inserts must all remain visible — verifies that the internal
/// length counter advances correctly so earlier elements aren't lost.
#[test]
fn test_sparse_set_insert_increments_len() {
    let mut set = SparseSet::new(10);
    set.insert(5);
    assert!(set.contains(5));
    set.insert(6);
    assert!(set.contains(5));
    assert!(set.contains(6));
    set.insert(7);
    assert!(set.contains(5));
    assert!(set.contains(6));
    assert!(set.contains(7));
}

/// contains() must return false for absent IDs and true for present ones.
#[test]
fn test_sparse_set_contains_correctness() {
    let mut set = SparseSet::new(10);
    assert!(!set.contains(0));
    assert!(!set.contains(5));
    set.insert(5);
    assert!(set.contains(5));
    assert!(!set.contains(0));
}

/// An ID that wasn't inserted must not be reported as present, even when
/// its sparse slot happens to hold a valid-looking index from a different
/// insert (exercises both halves of the contains() conjunction).
#[test]
fn test_sparse_set_contains_rejects_uninserted_id() {
    let mut set = SparseSet::new(10);
    set.insert(3);
    assert!(!set.contains(0));
}

/// First-inserted element is still found after a second insert (catches
/// off-by-one in the `sparse[id] < len` bound check).
#[test]
fn test_sparse_set_contains_after_multiple_inserts() {
    let mut set = SparseSet::new(10);
    set.insert(3);
    set.insert(7);
    assert!(set.contains(3));
    assert!(set.contains(7));
}

/// A single inserted element is immediately findable.
#[test]
fn test_sparse_set_contains_single_element() {
    let mut set = SparseSet::new(10);
    set.insert(3);
    assert!(set.contains(3));
}

/// After clear + re-insert, stale sparse/dense entries from before the
/// clear must not cause false positives. This is the key invariant of
/// the O(1)-clear sparse set design.
#[test]
fn test_sparse_set_no_stale_entries_after_clear() {
    let mut set = SparseSet::new(10);
    set.insert(3);
    set.insert(5);
    set.clear();
    set.insert(8);
    assert!(!set.contains(5));
}

/// The dense[idx] == id cross-check rejects IDs whose sparse slot
/// points to a valid dense position that holds a *different* ID.
#[test]
fn test_sparse_set_contains_cross_check() {
    let mut set = SparseSet::new(10);
    set.insert(4);
    assert!(set.contains(4));
    assert!(!set.contains(0));
}

/// clear() must actually invalidate all elements.
#[test]
fn test_sparse_set_clear_is_effective() {
    let mut set = SparseSet::new(10);
    set.insert(2);
    set.insert(4);
    set.insert(6);

    set.clear();
    assert!(!set.contains(2));
    assert!(!set.contains(4));
    assert!(!set.contains(6));
    assert!(set.insert(2));
}

/// Elements can be re-inserted after clear, and cleared elements stay absent.
#[test]
fn test_sparse_set_insert_after_clear_correctness() {
    let mut set = SparseSet::new(10);
    set.insert(1);
    set.insert(2);
    set.insert(3);
    set.clear();

    assert!(set.insert(4));
    assert!(set.contains(4));
    assert!(!set.contains(1));

    assert!(set.insert(1));
    assert!(set.contains(1));
    assert!(set.contains(4));
}

/// Uses non-zero IDs to avoid zero-initialization coincidences.
#[test]
fn test_sparse_set_non_zero_ids_only() {
    let mut set = SparseSet::new(20);
    set.insert(15);
    set.insert(10);
    set.insert(18);

    assert!(set.contains(15));
    assert!(set.contains(10));
    assert!(set.contains(18));

    assert!(!set.contains(0));
    assert!(!set.contains(1));
    assert!(!set.contains(5));
    assert!(!set.contains(19));
}

/// Fill to capacity; every element must be findable and duplicates must return false.
#[test]
fn test_sparse_set_fill_to_capacity() {
    let cap = 8;
    let mut set = SparseSet::new(cap);

    for i in 0..cap {
        assert!(set.insert(i), "insert({i}) should return true");
    }

    for i in 0..cap {
        assert!(set.contains(i));
        assert!(!set.insert(i), "duplicate insert({i}) should return false");
    }

    assert_eq!(set.capacity(), cap);
}
