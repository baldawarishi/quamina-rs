//! Core tests for quamina-rs
//!
//! Go lineage: core_matcher_test.go, arrays_test.go, flatten_json_test.go
//!
//! This module covers:
//! - Basic matching (exact, numeric, boolean, null)
//! - Exists operator (true/false, empty array)
//! - Nested fields and deeply nested patterns
//! - Array element matching
//! - Delete/rebuild, pruner stats
//! - Builder API tests
//! - Clone, Send+Sync
//! - Custom flattener
//! - Error handling

use super::*;

// ============================================================================
// Basic Matching Tests
// ============================================================================

#[test]
fn test_exact_match() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

    let matches = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"]);
}

#[test]
fn test_no_match() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

    let matches = q
        .matches_for_event(r#"{"status": "inactive"}"#.as_bytes())
        .unwrap();
    assert!(matches.is_empty());
}

#[test]
fn test_numeric_match() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"count": [42]}"#).unwrap();

    let matches = q.matches_for_event(r#"{"count": 42}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"], "Should match numeric value 42");
}

#[test]
fn test_numeric_variant_matching() {
    // All these numeric representations of 35 should match pattern [35]
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [35]}"#).unwrap();

    // Integer form
    let m1 = q.matches_for_event(r#"{"x": 35}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["p1"], "35 should match");

    // Decimal with trailing zero
    let m2 = q.matches_for_event(r#"{"x": 35.0}"#.as_bytes()).unwrap();
    assert_eq!(m2, vec!["p1"], "35.0 should match [35]");

    // Scientific notation
    let m3 = q.matches_for_event(r#"{"x": 3.5e1}"#.as_bytes()).unwrap();
    assert_eq!(m3, vec!["p1"], "3.5e1 should match [35]");

    // Additional variants from Go's TestMatcherNumerics (numbers_test.go:174)
    let m4 = q.matches_for_event(r#"{"x": 35.000}"#.as_bytes()).unwrap();
    assert_eq!(m4, vec!["p1"], "35.000 should match [35]");

    let m5 = q
        .matches_for_event(r#"{"x": 0.000035e6}"#.as_bytes())
        .unwrap();
    assert_eq!(m5, vec!["p1"], "0.000035e6 should match [35]");
}

#[test]
fn test_boolean_match() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"enabled": [true]}"#).unwrap();

    let matches = q
        .matches_for_event(r#"{"enabled": true}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match boolean true");
}

#[test]
fn test_null_match() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"value": [null]}"#).unwrap();

    let matches = q
        .matches_for_event(r#"{"value": null}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match null value");
}

// ============================================================================
// Exists Operator Tests
// ============================================================================

#[test]
fn test_exists_true() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"name": [{"exists": true}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"name": "anything", "other": 1}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match when field exists");

    let no_match = q.matches_for_event(r#"{"other": 1}"#.as_bytes()).unwrap();
    assert!(
        no_match.is_empty(),
        "Should not match when field is missing"
    );
}

#[test]
fn test_exists_false() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"name": [{"exists": false}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"other": 1}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"], "Should match when field is absent");

    let no_match = q
        .matches_for_event(r#"{"name": "value"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty(), "Should not match when field exists");
}

#[test]
fn test_exists_with_empty_array() {
    // Per Go quamina: {"a": []} with exists:true does NOT match
    // but exists:false DOES match (no leaf values)
    let mut q_true = Quamina::new();
    q_true
        .add_pattern("p1", r#"{"a": [{"exists": true}]}"#)
        .unwrap();

    let mut q_false = Quamina::new();
    q_false
        .add_pattern("p2", r#"{"a": [{"exists": false}]}"#)
        .unwrap();

    // Event with empty array
    let event = r#"{"a": []}"#;

    // exists:true should NOT match (no leaf values in empty array)
    let matches_true = q_true.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        matches_true.is_empty(),
        "exists:true should not match empty array"
    );

    // exists:false SHOULD match (no leaf values means field effectively absent)
    let matches_false = q_false.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches_false,
        vec!["p2"],
        "exists:false should match empty array"
    );
}

// ============================================================================
// Nested Field Tests
// ============================================================================

#[test]
fn test_nested_object_pattern() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"user": {"role": ["admin"]}}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"user": {"role": "admin", "name": "alice"}}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match nested field");

    let no_match = q
        .matches_for_event(r#"{"user": {"role": "guest"}}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_deeply_nested() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"a": {"b": {"c": ["value"]}}}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"a": {"b": {"c": "value"}}}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"]);
}

// ============================================================================
// Array Element Matching Tests
// ============================================================================

#[test]
fn test_array_element_matching() {
    // Pattern should match if value is ANY element of the array
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"ids": [943]}"#).unwrap();

    // Event has array - should match if 943 is in the array
    let event = r#"{"ids": [116, 943, 234]}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches,
        vec!["p1"],
        "Should match when pattern value is in event array"
    );
}

#[test]
fn test_array_cross_element_matching() {
    // Test cross-element array matching behavior (matches Go quamina behavior)
    // Pattern {"members": {"given": ["Mick"], "surname": ["Strummer"]}}
    // Event: members=[{given: "Joe", surname: "Strummer"}, {given: "Mick", surname: "Jones"}]
    //
    // Should NOT match because no single array element has both given=Mick AND surname=Strummer

    let mut q = Quamina::new();
    q.add_pattern(
        "cross",
        r#"{"members": {"given": ["Mick"], "surname": ["Strummer"]}}"#,
    )
    .unwrap();

    let event = r#"{"members": [
        {"given": "Joe", "surname": "Strummer"},
        {"given": "Mick", "surname": "Jones"}
    ]}"#;

    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    // Should NOT match - cross-element matching is correctly prevented
    assert!(
        matches.is_empty(),
        "Should not match across different array elements"
    );
}

#[test]
fn test_array_cross_element_comprehensive() {
    // Comprehensive test from Go's arrays_test.go TestArrayCorrectness
    let bands = r#"{
        "bands": [
            {
                "name": "The Clash",
                "members": [
                    {"given": "Joe", "surname": "Strummer", "role": ["guitar", "vocals"]},
                    {"given": "Mick", "surname": "Jones", "role": ["guitar", "vocals"]},
                    {"given": "Paul", "surname": "Simonon", "role": ["bass"]},
                    {"given": "Topper", "surname": "Headon", "role": ["drums"]}
                ]
            },
            {
                "name": "Boris",
                "members": [
                    {"given": "Wata", "role": ["guitar", "vocals"]},
                    {"given": "Atsuo", "role": ["drums"]},
                    {"given": "Takeshi", "role": ["bass", "vocals"]}
                ]
            }
        ]
    }"#;

    let mut q = Quamina::new();
    // Pattern 1: Mick with surname Strummer - SHOULD NOT match (cross-element)
    q.add_pattern(
        "mick_strummer",
        r#"{"bands": {"members": {"given": ["Mick"], "surname": ["Strummer"]}}}"#,
    )
    .unwrap();
    // Pattern 2: Wata with role drums - SHOULD NOT match (cross-element)
    q.add_pattern(
        "wata_drums",
        r#"{"bands": {"members": {"given": ["Wata"], "role": ["drums"]}}}"#,
    )
    .unwrap();
    // Pattern 3: Wata with role guitar - SHOULD match (same element)
    q.add_pattern(
        "wata_guitar",
        r#"{"bands": {"members": {"given": ["Wata"], "role": ["guitar"]}}}"#,
    )
    .unwrap();

    let matches = q.matches_for_event(bands.as_bytes()).unwrap();

    assert_eq!(
        matches.len(),
        1,
        "Expected exactly one match, got: {:?}",
        matches
    );
    assert!(
        matches.contains(&"wata_guitar"),
        "wata_guitar should match (same array element)"
    );
    assert!(
        !matches.contains(&"mick_strummer"),
        "mick_strummer should NOT match (cross-element)"
    );
    assert!(
        !matches.contains(&"wata_drums"),
        "wata_drums should NOT match (cross-element)"
    );
}

// ============================================================================
// Multiple Patterns Tests
// ============================================================================

#[test]
fn test_multiple_patterns_same_id() {
    // Multiple patterns with same ID - any match counts
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
    q.add_pattern("p1", r#"{"status": ["pending"]}"#).unwrap();

    let m1 = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"]);

    let m2 = q
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert_eq!(m2, vec!["p1"]);
}

#[test]
fn test_or_within_field() {
    // Multiple values in array = OR
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active", "pending", "review"]}"#)
        .unwrap();

    for status in &["active", "pending", "review"] {
        let event = format!(r#"{{"status": "{}"}}"#, status);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match {}", status);
    }

    let no_match = q
        .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_and_across_fields() {
    // Multiple fields = AND
    let mut q = Quamina::new();
    q.add_pattern(
        "p1",
        r#"{"type": ["order"], "status": ["pending"], "priority": ["high"]}"#,
    )
    .unwrap();

    let matches = q
        .matches_for_event(
            r#"{"type": "order", "status": "pending", "priority": "high"}"#.as_bytes(),
        )
        .unwrap();
    assert_eq!(matches, vec!["p1"]);

    // Missing one field
    let no_match = q
        .matches_for_event(r#"{"type": "order", "status": "pending"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
}

// ============================================================================
// Delete and Rebuild Tests
// ============================================================================

#[test]
fn test_delete_patterns() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
    q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();

    // Both match initially
    let m1 = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert!(m1.contains(&"p1"));

    // Delete p1
    q.delete_patterns(&"p1").unwrap();

    // p1 no longer matches
    let m2 = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert!(m2.is_empty());

    // p2 still works
    let m3 = q
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert!(m3.contains(&"p2"));
}

#[test]
fn test_rebuild_after_delete() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
    q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();
    q.add_pattern("p3", r#"{"status": ["review"]}"#).unwrap();

    // Initial count
    assert_eq!(q.pattern_count(), 3);

    // Delete p1
    q.delete_patterns(&"p1").unwrap();
    assert_eq!(q.pattern_count(), 2);

    // p1 is in deleted set
    assert!(q.deleted_patterns.contains(&"p1"));

    // Rebuild should purge deleted patterns
    let purged = q.rebuild();
    assert_eq!(purged, 1);

    // After rebuild, deleted set is clear
    assert!(q.deleted_patterns.is_empty());
    assert_eq!(q.pattern_count(), 2);

    // p2 and p3 still work
    let m2 = q
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert!(m2.contains(&"p2"));

    let m3 = q
        .matches_for_event(r#"{"status": "review"}"#.as_bytes())
        .unwrap();
    assert!(m3.contains(&"p3"));

    // p1 does not match (and is not in deleted set, was purged)
    let m1 = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert!(m1.is_empty());
}

#[test]
fn test_pruner_stats() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
    q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();

    // Initially stats are zero
    assert_eq!(q.pruner_stats().emitted(), 0);
    assert_eq!(q.pruner_stats().filtered(), 0);

    // Match - should increment emitted
    let _ = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(q.pruner_stats().emitted(), 1);
    assert_eq!(q.pruner_stats().filtered(), 0);

    // Delete p1
    q.delete_patterns(&"p1").unwrap();

    // Match active - should increment filtered (was deleted)
    let _ = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(q.pruner_stats().emitted(), 1);
    assert_eq!(q.pruner_stats().filtered(), 1);

    // Match pending - should increment emitted
    let _ = q
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert_eq!(q.pruner_stats().emitted(), 2);
    assert_eq!(q.pruner_stats().filtered(), 1);

    // Rebuild resets stats
    q.rebuild();
    assert_eq!(q.pruner_stats().emitted(), 0);
    assert_eq!(q.pruner_stats().filtered(), 0);
}

// MIRI SKIP RATIONALE: 500 iterations of matches_for_event with 5 patterns takes ~48s under
// Miri. Coverage: test_should_rebuild_threshold_miri_friendly exercises the same rebuild
// threshold logic with fewer iterations.
#[test]
#[cfg_attr(miri, ignore)]
fn test_should_rebuild_threshold() {
    let mut q = Quamina::new();

    // Add patterns that will match many events
    q.add_pattern("p1", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p2", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p3", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p4", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p5", r#"{"x": ["a"]}"#).unwrap();

    // Delete half
    q.delete_patterns(&"p1").unwrap();
    q.delete_patterns(&"p2").unwrap();

    // Not enough activity yet - should not trigger rebuild
    assert!(!q.should_rebuild());

    // Simulate lots of matches
    let event = br#"{"x": "a"}"#;
    for _ in 0..500 {
        let _ = q.matches_for_event(event).unwrap();
    }

    // After 500 matches with 5 patterns, 3 emit, 2 filtered
    // filtered = 500 * 2 = 1000
    // emitted = 500 * 3 = 1500
    // Total activity = 2500 > 1000 threshold
    // Ratio = 1000/1500 = 0.67 > 0.2
    assert!(q.should_rebuild());

    // maybe_rebuild should trigger
    let purged = q.maybe_rebuild();
    assert_eq!(purged, 2);

    // After rebuild, no longer needs rebuild
    assert!(!q.should_rebuild());
}

/// Miri-only: exercises the same rebuild threshold logic with 100 iterations instead of 500.
/// With 3 patterns (2 deleted, 1 remaining), 100 matches yields:
///   filtered = 100 * 2 = 200, emitted = 100 * 1 = 100, total = 300.
/// We use 3 patterns total so threshold math still triggers (needs total > 1000 with
/// ratio > 0.2). We run 400 iterations: filtered=800, emitted=400, total=1200 > 1000,
/// ratio=800/400=2.0 > 0.2.
#[test]
#[cfg(miri)]
fn test_should_rebuild_threshold_miri_friendly() {
    let mut q = Quamina::new();

    q.add_pattern("p1", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p2", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p3", r#"{"x": ["a"]}"#).unwrap();

    q.delete_patterns(&"p1").unwrap();
    q.delete_patterns(&"p2").unwrap();

    assert!(!q.should_rebuild());

    let event = br#"{"x": "a"}"#;
    // 400 iterations: filtered=800, emitted=400, total=1200 > 1000
    for _ in 0..400 {
        let _ = q.matches_for_event(event).unwrap();
    }

    assert!(q.should_rebuild());

    let purged = q.maybe_rebuild();
    assert_eq!(purged, 2);

    assert!(!q.should_rebuild());
}

// MIRI SKIP RATIONALE: 2000 iterations of matches_for_event is slow under Miri (~100s).
// Coverage: test_auto_rebuild_disabled_miri_friendly exercises same logic with 5 iterations.
#[test]
#[cfg_attr(miri, ignore)]
fn test_auto_rebuild_disabled() {
    let mut q = Quamina::new();
    q.set_auto_rebuild(false);

    q.add_pattern("p1", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p2", r#"{"x": ["a"]}"#).unwrap();

    q.delete_patterns(&"p1").unwrap();

    // Simulate enough activity to trigger
    let event = br#"{"x": "a"}"#;
    for _ in 0..2000 {
        let _ = q.matches_for_event(event).unwrap();
    }

    // Should want rebuild but auto is disabled
    assert!(q.should_rebuild());

    // maybe_rebuild returns 0 when disabled
    let purged = q.maybe_rebuild();
    assert_eq!(purged, 0);
}

/// Miri-friendly version of test_auto_rebuild_disabled
#[test]
fn test_auto_rebuild_disabled_miri_friendly() {
    let mut q = Quamina::new();
    q.set_auto_rebuild(false);

    q.add_pattern("p1", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p2", r#"{"x": ["a"]}"#).unwrap();

    q.delete_patterns(&"p1").unwrap();

    // Small number of iterations
    let event = br#"{"x": "a"}"#;
    for _ in 0..5 {
        let _ = q.matches_for_event(event).unwrap();
    }

    // maybe_rebuild returns 0 when disabled
    let purged = q.maybe_rebuild();
    assert_eq!(purged, 0);
}

// ============================================================================
// Clone and Thread Safety Tests
// ============================================================================

#[test]
fn test_clone_for_snapshot() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

    // Clone creates an independent snapshot
    let snapshot = q.clone();

    // Modify original
    q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();

    // Snapshot doesn't have p2
    let snap_matches = snapshot
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert!(snap_matches.is_empty());

    // Original has p2
    let orig_matches = q
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert!(orig_matches.contains(&"p2"));
}

#[test]
fn test_send_sync() {
    // Verify Quamina is Send + Sync for thread safety
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<Quamina<String>>();
}

#[test]
fn test_has_matches() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

    assert!(q.has_matches(r#"{"status": "active"}"#.as_bytes()).unwrap());
    assert!(!q
        .has_matches(r#"{"status": "inactive"}"#.as_bytes())
        .unwrap());
}

#[test]
fn test_count_matches() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
    q.add_pattern("p2", r#"{"status": ["active"]}"#).unwrap();
    q.add_pattern("p3", r#"{"status": ["pending"]}"#).unwrap();

    assert_eq!(
        q.count_matches(r#"{"status": "active"}"#.as_bytes())
            .unwrap(),
        2
    );
    assert_eq!(
        q.count_matches(r#"{"status": "pending"}"#.as_bytes())
            .unwrap(),
        1
    );
    assert_eq!(
        q.count_matches(r#"{"status": "deleted"}"#.as_bytes())
            .unwrap(),
        0
    );
}

#[test]
fn test_pattern_count_and_clear() {
    let mut q = Quamina::new();
    assert!(q.is_empty());
    assert_eq!(q.pattern_count(), 0);

    q.add_pattern("p1", r#"{"a": ["1"]}"#).unwrap();
    q.add_pattern("p2", r#"{"b": ["2"]}"#).unwrap();
    assert!(!q.is_empty());
    assert_eq!(q.pattern_count(), 2);

    q.clear();
    assert!(q.is_empty());
    assert_eq!(q.pattern_count(), 0);
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_invalid_json_events() {
    // Based on Go quamina's TestFJErrorCases
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"a": [1]}"#).unwrap();

    // Truncated JSON
    assert!(
        q.matches_for_event(r#"{"a"#.as_bytes()).is_err(),
        "Truncated JSON should error"
    );
    assert!(
        q.matches_for_event(r#"{"a": "#.as_bytes()).is_err(),
        "Truncated value should error"
    );
    assert!(
        q.matches_for_event(r#"{"a": ["#.as_bytes()).is_err(),
        "Truncated array should error"
    );

    // Empty input
    assert!(
        q.matches_for_event(r#""#.as_bytes()).is_err(),
        "Empty input should error"
    );

    // Non-object at top level
    assert!(
        q.matches_for_event(r#""string""#.as_bytes()).is_err(),
        "String at top level should error"
    );
    assert!(
        q.matches_for_event(r#"[1, 2]"#.as_bytes()).is_err(),
        "Array at top level should error"
    );
    assert!(
        q.matches_for_event(r#"123"#.as_bytes()).is_err(),
        "Number at top level should error"
    );

    // Malformed JSON
    assert!(
        q.matches_for_event(r#"{ "a" : }"#.as_bytes()).is_err(),
        "Missing value should error"
    );

    // Invalid escape sequences
    assert!(
        q.matches_for_event(r#"{"a": "a\zb"}"#.as_bytes()).is_err(),
        "Invalid escape \\z should error"
    );
    assert!(
        q.matches_for_event(r#"{"a\zb": 2}"#.as_bytes()).is_err(),
        "Invalid escape in field name should error"
    );

    // Invalid value identifier
    assert!(
        q.matches_for_event(r#"{"a": xx}"#.as_bytes()).is_err(),
        "Invalid value xx should error"
    );

    // Truncated/invalid literals
    assert!(
        q.matches_for_event(r#"{"a": tru}"#.as_bytes()).is_err(),
        "Truncated 'tru' should error"
    );
    assert!(
        q.matches_for_event(r#"{"a": truse}"#.as_bytes()).is_err(),
        "Invalid 'truse' should error"
    );
}

#[test]
fn test_invalid_pattern_handling() {
    let mut q = Quamina::new();

    // Empty pattern
    assert!(q.add_pattern("p1", "").is_err());

    // Non-object at top level
    assert!(q.add_pattern("p2", "33").is_err());
    assert!(q.add_pattern("p3", "[1,2]").is_err());

    // Malformed JSON
    assert!(q.add_pattern("p4", "{").is_err());
    assert!(q.add_pattern("p5", r#"{"foo": }"#).is_err());

    // Pattern field must be array or nested object
    assert!(q.add_pattern("p6", r#"{"foo": "string"}"#).is_err());
    assert!(q.add_pattern("p7", r#"{"foo": 123}"#).is_err());
    assert!(q.add_pattern("p8", r#"{"foo": true}"#).is_err());

    // Valid patterns should work
    assert!(q.add_pattern("valid1", r#"{"x": [1]}"#).is_ok());
    assert!(q.add_pattern("valid2", r#"{"x": ["string"]}"#).is_ok());
    assert!(q.add_pattern("valid3", r#"{"x": {"y": [1]}}"#).is_ok());
}

#[test]
fn test_bad_pattern_error_handling() {
    let mut q = Quamina::new();

    // Go quamina returns errors for these patterns (anything_but_test.go:134)
    // Empty anything-but
    assert!(q
        .add_pattern("p1", r#"{"x": [{"anything-but": []}]}"#)
        .is_err());

    // Mixed types in anything-but
    assert!(q
        .add_pattern("p2", r#"{"x": [{"anything-but": ["a", 1]}]}"#)
        .is_err());
}

#[test]
fn test_bad_event_error_handling() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [1]}"#).unwrap();

    // Invalid JSON
    assert!(q.matches_for_event(b"not json").is_err());
    assert!(q.matches_for_event(b"{").is_err());
    assert!(q.matches_for_event(b"").is_err());
}

#[test]
fn test_rebuild_zero_filtered_denominator() {
    let mut q = Quamina::new();

    // Add and immediately delete a pattern
    q.add_pattern("p1", r#"{"likes": ["tacos"]}"#).unwrap();
    q.delete_patterns(&"p1").unwrap();

    // Matching should not panic with zero patterns
    let result = q.matches_for_event(r#"{"likes": "tacos"}"#.as_bytes());
    assert!(result.is_ok(), "Should not panic with empty matcher");
    assert!(result.unwrap().is_empty(), "No matches expected");
}

// ============================================================================
// Builder API Tests
// ============================================================================

#[test]
fn test_builder_basic() {
    let q = QuaminaBuilder::<String>::new().build().unwrap();
    assert!(q.is_empty(), "New builder should create empty matcher");
    assert!(
        q.auto_rebuild_enabled(),
        "Auto-rebuild should be enabled by default"
    );
}

#[test]
fn test_builder_with_media_type_json() {
    let q = QuaminaBuilder::<String>::new()
        .with_media_type("application/json")
        .unwrap()
        .build()
        .unwrap();
    assert!(q.is_empty());
}

#[test]
fn test_builder_with_invalid_media_type() {
    let result = QuaminaBuilder::<String>::new().with_media_type("text/html");
    assert!(result.is_err(), "Should reject text/html");

    if let Err(QuaminaError::UnsupportedMediaType(mt)) = result {
        assert_eq!(mt, "text/html");
    } else {
        panic!("Expected UnsupportedMediaType error");
    }

    // Test other invalid types
    let result = QuaminaBuilder::<String>::new().with_media_type("application/xml");
    assert!(result.is_err(), "Should reject application/xml");

    let result = QuaminaBuilder::<String>::new().with_media_type("");
    assert!(result.is_err(), "Should reject empty media type");
}

#[test]
fn test_builder_with_auto_rebuild() {
    // Disable auto-rebuild
    let q = QuaminaBuilder::<String>::new()
        .with_auto_rebuild(false)
        .build()
        .unwrap();
    assert!(!q.auto_rebuild_enabled(), "Auto-rebuild should be disabled");

    // Enable auto-rebuild (explicit)
    let q = QuaminaBuilder::<String>::new()
        .with_auto_rebuild(true)
        .build()
        .unwrap();
    assert!(q.auto_rebuild_enabled(), "Auto-rebuild should be enabled");
}

#[test]
fn test_builder_combined_options() {
    let mut q = QuaminaBuilder::<String>::new()
        .with_media_type("application/json")
        .unwrap()
        .with_auto_rebuild(false)
        .build()
        .unwrap();

    q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
        .unwrap();
    let matches = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1".to_string()]);
    assert!(!q.auto_rebuild_enabled());
}

#[test]
fn test_builder_default() {
    let q = QuaminaBuilder::<String>::default().build().unwrap();
    assert!(q.is_empty());
    assert!(q.auto_rebuild_enabled());
}

#[test]
fn test_builder_generic_type() {
    // With i32 as pattern ID
    let mut q = QuaminaBuilder::<i32>::new().build().unwrap();
    q.add_pattern(42, r#"{"x": [1]}"#).unwrap();
    let matches = q.matches_for_event(r#"{"x": 1}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec![42]);

    // With &str as pattern ID
    let mut q = QuaminaBuilder::<&str>::new().build().unwrap();
    q.add_pattern("test", r#"{"x": [1]}"#).unwrap();
    let matches = q.matches_for_event(r#"{"x": 1}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["test"]);
}

// ============================================================================
// Custom Flattener Tests
// ============================================================================

/// A simple custom flattener that returns hardcoded fields for testing
struct MockFlattener {
    fields: Vec<OwnedField>,
}

impl MockFlattener {
    fn new(fields: Vec<OwnedField>) -> Self {
        Self { fields }
    }
}

impl Flattener for MockFlattener {
    fn flatten(
        &mut self,
        _event: &[u8],
        _tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        Ok(self.fields.clone())
    }

    fn copy(&self) -> Box<dyn Flattener> {
        Box::new(MockFlattener {
            fields: self.fields.clone(),
        })
    }
}

#[test]
fn test_custom_flattener_basic() {
    // Create a custom flattener that always returns a specific field
    // Note: path doesn't have trailing newline, string values need quotes
    let flattener = MockFlattener::new(vec![OwnedField {
        path: b"status".to_vec(),
        val: b"\"active\"".to_vec(),
        array_trail: vec![],
        is_number: false,
    }]);

    let mut q = QuaminaBuilder::<String>::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
        .unwrap();

    // The custom flattener ignores the event and returns "status": "active"
    let matches = q.matches_for_event(b"ignored event data").unwrap();
    assert_eq!(matches, vec!["p1".to_string()]);
}

#[test]
fn test_custom_flattener_no_match() {
    // Create a custom flattener that returns a different field
    let flattener = MockFlattener::new(vec![OwnedField {
        path: b"status".to_vec(),
        val: b"\"inactive\"".to_vec(),
        array_trail: vec![],
        is_number: false,
    }]);

    let mut q = QuaminaBuilder::<String>::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
        .unwrap();

    let matches = q.matches_for_event(b"ignored").unwrap();
    assert!(matches.is_empty());
}

#[test]
fn test_custom_flattener_with_numbers() {
    let flattener = MockFlattener::new(vec![OwnedField {
        path: b"count".to_vec(),
        val: b"42".to_vec(),
        array_trail: vec![],
        is_number: true,
    }]);

    let mut q = QuaminaBuilder::<String>::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("p1".to_string(), r#"{"count": [42]}"#)
        .unwrap();

    let matches = q.matches_for_event(b"ignored").unwrap();
    assert_eq!(matches, vec!["p1".to_string()]);
}

#[test]
fn test_custom_flattener_clone() {
    let flattener = MockFlattener::new(vec![OwnedField {
        path: b"x".to_vec(),
        val: b"\"y\"".to_vec(),
        array_trail: vec![],
        is_number: false,
    }]);

    let mut q = QuaminaBuilder::<String>::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("p1".to_string(), r#"{"x": ["y"]}"#).unwrap();

    // Clone the Quamina instance
    let q_clone = q.clone();

    // Both should match
    let m1 = q.matches_for_event(b"ignored").unwrap();
    let m2 = q_clone.matches_for_event(b"ignored").unwrap();
    assert_eq!(m1, vec!["p1".to_string()]);
    assert_eq!(m2, vec!["p1".to_string()]);
}

#[test]
fn test_with_flattener_conflicts_with_media_type() {
    let flattener = MockFlattener::new(vec![]);

    // Should fail if media type is set first
    let result = QuaminaBuilder::<String>::new()
        .with_media_type("application/json")
        .unwrap()
        .with_flattener(Box::new(flattener));

    assert!(result.is_err());
}

#[test]
fn test_with_flattener_cannot_be_set_twice() {
    let flattener1 = MockFlattener::new(vec![]);
    let flattener2 = MockFlattener::new(vec![]);

    let result = QuaminaBuilder::<String>::new()
        .with_flattener(Box::new(flattener1))
        .unwrap()
        .with_flattener(Box::new(flattener2));

    assert!(result.is_err());
}

#[test]
fn test_json_flattener_through_trait() {
    // Test that the built-in JsonFlattener works through the Flattener trait
    use crate::flattener::JsonFlattener;

    let mut q = QuaminaBuilder::<String>::new()
        .with_flattener(Box::new(JsonFlattener::new()))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"]);
}

// ============================================================================
// Additional Core Tests (recovered from original)
// ============================================================================

#[test]
fn test_same_pattern_id_multiple_value_types() {
    // Based on Go quamina's TestExerciseSingletonReplacement and TestMergeNfaAndNumeric
    // Same pattern ID can match via different value types (string OR number)
    let mut q = Quamina::new();
    // Add two patterns with same ID but different value types
    q.add_pattern("x", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("x", r#"{"x": [1]}"#).unwrap();

    // Both string and number should match pattern "x"
    let matches1 = q.matches_for_event(r#"{"x": 1}"#.as_bytes()).unwrap();
    assert_eq!(matches1, vec!["x"], "number 1 should match");

    let matches2 = q.matches_for_event(r#"{"x": "a"}"#.as_bytes()).unwrap();
    assert_eq!(matches2, vec!["x"], "string 'a' should match");

    // Test wildcard OR number for same pattern ID
    let mut q2 = Quamina::new();
    q2.add_pattern("x", r#"{"x": [{"wildcard": "x*y"}]}"#)
        .unwrap();
    q2.add_pattern("x", r#"{"x": [3]}"#).unwrap();

    let m1 = q2.matches_for_event(r#"{"x": 3}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["x"], "number 3 should match");

    let m2 = q2
        .matches_for_event(r#"{"x": "xasdfy"}"#.as_bytes())
        .unwrap();
    assert_eq!(m2, vec!["x"], "wildcard pattern should match");
}

#[test]
fn test_field_name_ordering_with_exists() {
    // Based on Go quamina's TestFieldNameOrdering
    // Tests patterns with exists:false against a simple event with field "b"
    // All patterns should match because the absent fields (a, c) don't exist
    let event = r#"{"b": 1}"#;

    let patterns = [
        // b=1 AND a doesn't exist (true - a is absent)
        (r#"{"b": [1], "a": [{"exists": false}]}"#, "p0"),
        // b=1 AND c doesn't exist (true - c is absent)
        (r#"{"b": [1], "c": [{"exists": false}]}"#, "p1"),
        // b=1 (true)
        (r#"{"b": [1]}"#, "p2"),
        // a doesn't exist (true - a is absent)
        (r#"{"a": [{"exists": false}]}"#, "p3"),
    ];

    // Add all patterns and verify all match
    let mut q = Quamina::new();
    for (pattern, name) in &patterns {
        q.add_pattern(*name, pattern).unwrap();
    }

    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches.len(),
        patterns.len(),
        "All {} patterns should match, got {:?}",
        patterns.len(),
        matches
    );

    for (_, name) in &patterns {
        assert!(matches.contains(name), "Pattern {} should match", name);
    }
}

#[test]
fn test_invalid_pattern_validation() {
    // Based on Go quamina's TestPatternFromJSON
    // Tests that various invalid patterns are properly rejected
    let invalid_patterns = [
        // Value not in array (must be array or object)
        (r#"{"foo": 11}"#, "number not in array"),
        (r#"{"foo": "x"}"#, "string not in array"),
        (r#"{"foo": true}"#, "boolean not in array"),
        (r#"{"foo": null}"#, "null not in array"),
        // Invalid exists operator
        (r#"{"x": [{"exists": 23}]}"#, "exists with number"),
        (r#"{"x": [{"exists": "yes"}]}"#, "exists with string"),
        // Invalid shellstyle
        (r#"{"x": [{"shellstyle": 15}]}"#, "shellstyle with number"),
        (r#"{"x": [{"shellstyle": "a**b"}]}"#, "shellstyle with **"),
        // Invalid prefix
        (r#"{"x": [{"prefix": 23}]}"#, "prefix with number"),
        // Invalid suffix
        (r#"{"x": [{"suffix": 23}]}"#, "suffix with number"),
        // Invalid equals-ignore-case
        (
            r#"{"x": [{"equals-ignore-case": 5}]}"#,
            "equals-ignore-case with number",
        ),
        // Invalid numeric
        (r#"{"x": [{"numeric": ">=5"}]}"#, "numeric with string"),
        // Invalid regex
        (
            r#"{"x": [{"regex": "[invalid"}]}"#,
            "regex with invalid pattern",
        ),
        // Unknown operator
        (r#"{"x": [{"unknown-op": "val"}]}"#, "unknown operator"),
    ];

    for (pattern, desc) in &invalid_patterns {
        let mut q = Quamina::new();
        let result = q.add_pattern("test", pattern);
        assert!(result.is_err(), "{} should be rejected: {}", desc, pattern);
    }
}

#[test]
fn test_numbits_boundary_values() {
    // Test float64 boundary values for numeric matching
    use crate::numbits::{numbits_from_f64, q_num_from_f64, to_q_number};

    // Float64 boundary categories:
    // - Subnormal (smallest positive): 2^-1074 to 2^-1022
    // - Normal minimum: 2^-1022 ≈ 2.225e-308
    // - Normal maximum: (2 - 2^-52) × 2^1023 ≈ 1.798e+308

    // Test zero
    let nb_zero = numbits_from_f64(0.0);
    let q_zero = q_num_from_f64(0.0);
    assert!(nb_zero > 0, "Zero should have non-zero numbits");
    assert!(!q_zero.is_empty(), "Zero should have non-empty Q-number");

    // Test smallest positive subnormal: f64::MIN_POSITIVE / 2^52 ≈ 4.94e-324
    let smallest_subnormal = 5e-324_f64;
    let nb_small = numbits_from_f64(smallest_subnormal);
    let q_small = q_num_from_f64(smallest_subnormal);
    assert!(nb_small > nb_zero, "Smallest subnormal > 0");
    assert!(
        q_small > q_zero,
        "Smallest subnormal Q-number > zero Q-number"
    );

    // Test smallest normal: f64::MIN_POSITIVE ≈ 2.225e-308
    let smallest_normal = f64::MIN_POSITIVE;
    let nb_min_normal = numbits_from_f64(smallest_normal);
    let q_min_normal = q_num_from_f64(smallest_normal);
    assert!(
        nb_min_normal > nb_small,
        "Smallest normal > smallest subnormal"
    );
    assert!(q_min_normal > q_small, "Q-number ordering preserved");

    // Test largest normal: f64::MAX ≈ 1.798e+308
    let largest_normal = f64::MAX;
    let nb_max = numbits_from_f64(largest_normal);
    let q_max = q_num_from_f64(largest_normal);
    assert!(nb_max > nb_min_normal, "Max > min positive");
    assert!(q_max > q_min_normal, "Q-number ordering preserved");

    // Test negative boundaries
    let nb_neg_max = numbits_from_f64(-f64::MAX);
    let nb_neg_min = numbits_from_f64(-f64::MIN_POSITIVE);
    let nb_neg_small = numbits_from_f64(-5e-324_f64);

    // Negative ordering: -MAX < -MIN_POSITIVE < -subnormal < 0
    assert!(nb_neg_max < nb_neg_min, "-MAX < -MIN_POSITIVE");
    assert!(nb_neg_min < nb_neg_small, "-MIN_POSITIVE < -subnormal");
    assert!(nb_neg_small < nb_zero, "-subnormal < 0");

    // Test that all Q-numbers are valid (bytes in 0-127 range)
    let test_values = [
        0.0,
        1.0,
        -1.0,
        f64::MIN_POSITIVE,
        f64::MAX,
        -f64::MAX,
        5e-324,
        -5e-324,
        1e100,
        -1e100,
        0.5,
        -0.5,
    ];
    for &val in &test_values {
        let q = q_num_from_f64(val);
        for &byte in &q {
            assert!(
                byte < 128,
                "Q-number byte {} >= 128 for value {}",
                byte,
                val
            );
        }
    }

    // Test numbits round-trip consistency
    for &val in &test_values {
        let nb = numbits_from_f64(val);
        let q1 = q_num_from_f64(val);
        let q2 = to_q_number(nb);
        assert_eq!(q1, q2, "Q-number should match via both paths for {}", val);
    }
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_numbits_to_qnumber_utf8() {
    // Test that Q-numbers are valid for automaton processing
    // Q-numbers use base-128 encoding (bytes 0-127), which is ASCII-compatible
    use crate::numbits::q_num_from_f64;

    // Generate 10K random floats and verify Q-number properties
    let mut rng_state = 0xDEADBEEF_u64;

    for i in 0..10_000 {
        // Simple LCG for reproducibility
        rng_state = rng_state
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);

        // Generate a random f64 in a wide range
        let sign = if rng_state & 1 == 0 { 1.0 } else { -1.0 };
        let exp = ((rng_state >> 1) % 600) as i32 - 300; // -300 to +299
        let mantissa = ((rng_state >> 10) as f64) / (1u64 << 54) as f64;
        let val = sign * (1.0 + mantissa) * 10f64.powi(exp);

        // Skip if not finite (shouldn't happen with our construction, but be safe)
        if !val.is_finite() {
            continue;
        }

        let q = q_num_from_f64(val);

        // Property 1: Non-empty
        assert!(
            !q.is_empty(),
            "Q-number should be non-empty for value at index {}",
            i
        );

        // Property 2: All bytes < 128 (valid for automaton)
        for (j, &byte) in q.iter().enumerate() {
            assert!(
                byte < 128,
                "Q-number byte {} at pos {} >= 128 for value at index {}",
                byte,
                j,
                i
            );
        }

        // Property 3: Valid UTF-8 (since all bytes are ASCII)
        assert!(
            std::str::from_utf8(&q).is_ok(),
            "Q-number should be valid UTF-8 for value at index {}",
            i
        );

        // Property 4: Length bounded
        assert!(
            q.len() <= 10,
            "Q-number length {} exceeds max 10 for value at index {}",
            q.len(),
            i
        );
    }

    // Test ordering preservation across 1000 random pairs
    let mut prev_val = f64::NEG_INFINITY;
    let mut prev_q = q_num_from_f64(-1e308);

    rng_state = 0x12345678_u64;
    let mut ordered_vals: Vec<f64> = Vec::new();

    for _ in 0..1000 {
        rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1);
        let val = ((rng_state as f64) / (u64::MAX as f64)) * 2e100 - 1e100;
        if val.is_finite() {
            ordered_vals.push(val);
        }
    }

    ordered_vals.sort_by(|a, b| a.partial_cmp(b).unwrap());

    for val in ordered_vals {
        let q = q_num_from_f64(val);
        if prev_val < val {
            assert!(
                prev_q <= q,
                "Q-number ordering violated: {} ({:?}) should be <= {} ({:?})",
                prev_val,
                prev_q,
                val,
                q
            );
        }
        prev_val = val;
        prev_q = q;
    }
}

#[test]
fn test_multi_condition_pattern_fields() {
    // Test MultiConditionPattern structure
    use crate::json::{LookaroundCondition, MultiConditionPattern};
    use crate::regexp::parse_regexp;

    // Create a multi-condition pattern manually
    let primary = parse_regexp("foo").unwrap();
    let combined = parse_regexp("foobar").unwrap();
    let conditions = vec![LookaroundCondition::PositiveLookahead(combined)];

    let mc = MultiConditionPattern::new(primary, conditions);

    // Verify structure
    assert_eq!(mc.primary.len(), 1, "Primary should have 1 branch");
    assert_eq!(mc.conditions.len(), 1, "Should have 1 condition");
    assert!(!mc.conditions[0].is_negative(), "Should be positive");
    assert!(
        !mc.conditions[0].is_lookbehind(),
        "Should not be lookbehind"
    );
}

#[test]
fn test_condition_cost_ordering() {
    // Test that conditions are sorted by cost
    use crate::json::{LookaroundCondition, MultiConditionPattern};
    use crate::regexp::parse_regexp;

    let primary = parse_regexp("test").unwrap();
    let pattern1 = parse_regexp("a").unwrap();
    let pattern2 = parse_regexp("b").unwrap();
    let pattern3 = parse_regexp("c").unwrap();

    // Create conditions in reverse cost order
    let conditions = vec![
        LookaroundCondition::NegativeLookbehind {
            pattern: pattern3.clone(),
            byte_length: 1,
        }, // cost 40
        LookaroundCondition::PositiveLookbehind {
            pattern: pattern2.clone(),
            byte_length: 1,
        }, // cost 30
        LookaroundCondition::NegativeLookahead(pattern1.clone()), // cost 20
    ];

    let mc = MultiConditionPattern::new(primary, conditions);

    // Verify conditions are sorted by cost (lowest first)
    assert_eq!(
        mc.conditions[0].cost_estimate(),
        20,
        "First should be cost 20"
    );
    assert_eq!(
        mc.conditions[1].cost_estimate(),
        30,
        "Second should be cost 30"
    );
    assert_eq!(
        mc.conditions[2].cost_estimate(),
        40,
        "Third should be cost 40"
    );
}
