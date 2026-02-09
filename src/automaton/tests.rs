use super::*;

// ========================================================================
// AutomatonValueMatcher Tests (arena-based)
// ========================================================================

#[test]
fn test_automaton_value_matcher_string() {
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_string_match(b"hello", "p1".to_string());
    matcher.add_string_match(b"world", "p2".to_string());

    let matches = matcher.match_value(b"hello");
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"p1".to_string()));

    let matches = matcher.match_value(b"world");
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"p2".to_string()));

    let matches = matcher.match_value(b"foo");
    assert!(matches.is_empty());
}

#[test]
fn test_automaton_value_matcher_prefix() {
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_prefix_match(b"prod-", "p1".to_string());
    matcher.add_prefix_match(b"test-", "p2".to_string());

    let matches = matcher.match_value(b"prod-123");
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"p1".to_string()));

    let matches = matcher.match_value(b"test-abc");
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"p2".to_string()));

    let matches = matcher.match_value(b"dev-xyz");
    assert!(matches.is_empty());
}

#[test]
fn test_automaton_value_matcher_shellstyle_single() {
    // Test with a single shellstyle pattern (no merging)
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_shellstyle_match(b"*.txt", "p1".to_string());

    let matches = matcher.match_value(b"file.txt");
    assert!(
        matches.contains(&"p1".to_string()),
        "file.txt should match *.txt"
    );

    let matches = matcher.match_value(b".txt");
    assert!(
        matches.contains(&"p1".to_string()),
        ".txt should match *.txt"
    );

    let matches = matcher.match_value(b"foo");
    assert!(matches.is_empty(), "foo should not match *.txt");
}

#[test]
fn test_automaton_value_matcher_shellstyle_multiple() {
    // Test with multiple shellstyle patterns (with merging)
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_shellstyle_match(b"*.txt", "p1".to_string());
    matcher.add_shellstyle_match(b"test*", "p2".to_string());

    let matches = matcher.match_value(b"random");
    assert!(matches.is_empty(), "random should not match any pattern");
}

#[test]
fn test_automaton_value_matcher_mixed() {
    // Test mixing different pattern types
    let mut matcher: AutomatonValueMatcher<String> = AutomatonValueMatcher::new();
    matcher.add_string_match(b"exact", "exact_match".to_string());
    matcher.add_prefix_match(b"pre-", "prefix_match".to_string());

    let matches = matcher.match_value(b"exact");
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"exact_match".to_string()));

    let matches = matcher.match_value(b"pre-fix");
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"prefix_match".to_string()));
}

// ========================================================================
// ArenaSmallTable Tests (ported from chain SmallTable tests)
// ========================================================================

#[test]
fn test_arena_small_table_step() {
    use arena::ArenaSmallTable;

    let table = ArenaSmallTable::new();

    // Test that all valid bytes return NONE for empty table
    for b in 0..BYTE_CEILING as u8 {
        let (s, eps) = table.step(b);
        assert!(
            s.is_none(),
            "byte {b} should have no transition in empty table"
        );
        assert!(
            eps.is_empty(),
            "byte {b} should have no epsilons in empty table"
        );
    }
}

#[test]
fn test_arena_small_table_with_mappings() {
    use arena::{ArenaSmallTable, StateArena, StateId};
    use std::sync::Arc;

    let mut arena = StateArena::new();
    let next_field = Arc::new(FieldMatcher::new());
    let next_state = arena.alloc_with_table(ArenaSmallTable::new());
    arena[next_state].field_transitions.push(next_field);

    let table = ArenaSmallTable::with_mappings(StateId::NONE, b"ab", &[next_state, next_state]);

    let (step_a, _) = table.step(b'a');
    assert!(!step_a.is_none(), "byte 'a' should have a transition");

    let (step_b, _) = table.step(b'b');
    assert!(!step_b.is_none(), "byte 'b' should have a transition");

    let (step_c, _) = table.step(b'c');
    assert!(step_c.is_none(), "byte 'c' should have no transition");
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
    let fields = vec![EventField {
        path: "status".to_string(),
        value: "active".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"p1".to_string()));
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

    let fields = vec![EventField {
        path: "status".to_string(),
        value: "inactive".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert!(matches.is_empty());
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
    let fields = vec![EventField {
        path: "name".to_string(),
        value: "anything".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(
        matches.len(),
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
    let fields = vec![EventField {
        path: "other".to_string(),
        value: "value".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(
        matches.len(),
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
    let fields = vec![
        EventField {
            path: "status".to_string(),
            value: "active".to_string(),
            array_trail: vec![],
            is_number: false,
        },
        EventField {
            path: "type".to_string(),
            value: "user".to_string(),
            array_trail: vec![],
            is_number: false,
        },
    ];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(
        matches.len(),
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
    let fields = vec![
        EventField {
            path: "status".to_string(),
            value: "active".to_string(),
            array_trail: vec![],
            is_number: false,
        },
        EventField {
            path: "type".to_string(),
            value: "admin".to_string(),
            array_trail: vec![],
            is_number: false,
        },
    ];

    let matches = matcher.matches_for_fields(&fields);
    assert!(
        matches.is_empty(),
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
    let fields1 = vec![EventField {
        path: "status".to_string(),
        value: "active".to_string(),
        array_trail: vec![],
        is_number: false,
    }];
    let matches1 = matcher.matches_for_fields(&fields1);
    assert_eq!(matches1.len(), 1, "OR within field should match 'active'");

    // Should match "pending"
    let fields2 = vec![EventField {
        path: "status".to_string(),
        value: "pending".to_string(),
        array_trail: vec![],
        is_number: false,
    }];
    let matches2 = matcher.matches_for_fields(&fields2);
    assert_eq!(matches2.len(), 1, "OR within field should match 'pending'");

    // Should not match "completed"
    let fields3 = vec![EventField {
        path: "status".to_string(),
        value: "completed".to_string(),
        array_trail: vec![],
        is_number: false,
    }];
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
    let fields = vec![EventField {
        path: "status".to_string(),
        value: "active".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"p1".to_string()));
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
    let fields = vec![EventField {
        path: "status".to_string(),
        value: "active".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"p1".to_string()));
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

    let fields = vec![EventField {
        path: "status".to_string(),
        value: "inactive".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert!(matches.is_empty());
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
    let fields = vec![EventField {
        path: "name".to_string(),
        value: "anything".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(
        matches.len(),
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
    let fields = vec![EventField {
        path: "other".to_string(),
        value: "value".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(
        matches.len(),
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
    let fields = vec![EventField {
        path: "status".to_string(),
        value: "active".to_string(),
        array_trail: vec![],
        is_number: false,
    }];

    let matches = matcher.matches_for_fields(&fields);
    assert_eq!(matches.len(), 1);
    assert!(matches.contains(&"p1".to_string()));
}
