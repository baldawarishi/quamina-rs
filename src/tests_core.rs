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
    let q = q!("p1" => r#"{"status": ["active"]}"#);
    assert_matches!(q, r#"{"status": "active"}"#, vec!["p1"]);
}

#[test]
fn test_no_match() {
    let q = q!("p1" => r#"{"status": ["active"]}"#);
    assert_no_match!(q, r#"{"status": "inactive"}"#);
}

#[test]
fn test_numeric_match() {
    let q = q!("p1" => r#"{"count": [42]}"#);
    assert_matches!(
        q,
        r#"{"count": 42}"#,
        vec!["p1"],
        "Should match numeric value 42"
    );
}

#[test]
fn test_numeric_variant_matching() {
    // All these numeric representations of 35 should match pattern [35]
    let q = q!("p1" => r#"{"x": [35]}"#);

    // All numeric representations of 35 should match (Go's numbers_test.go:174)
    for event in [
        r#"{"x": 35}"#,
        r#"{"x": 35.0}"#,
        r#"{"x": 3.5e1}"#,
        r#"{"x": 35.000}"#,
        r#"{"x": 0.000035e6}"#,
    ] {
        assert_matches!(q, event, vec!["p1"]);
    }
}

#[test]
fn test_boolean_match() {
    let q = q!("p1" => r#"{"enabled": [true]}"#);
    assert_matches!(
        q,
        r#"{"enabled": true}"#,
        vec!["p1"],
        "Should match boolean true"
    );
}

#[test]
fn test_null_match() {
    let q = q!("p1" => r#"{"value": [null]}"#);
    assert_matches!(
        q,
        r#"{"value": null}"#,
        vec!["p1"],
        "Should match null value"
    );
}

// ============================================================================
// Exists Operator Tests
// ============================================================================

#[test]
fn test_exists_true() {
    let q = q!("p1" => r#"{"name": [{"exists": true}]}"#);

    assert_matches!(
        q,
        r#"{"name": "anything", "other": 1}"#,
        vec!["p1"],
        "Should match when field exists"
    );
    assert_no_match!(
        q,
        r#"{"other": 1}"#,
        "Should not match when field is missing"
    );
}

#[test]
fn test_exists_false() {
    let q = q!("p1" => r#"{"name": [{"exists": false}]}"#);

    assert_matches!(q, r#"{"other": 1}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"name": "value"}"#);
}

#[test]
fn test_exists_with_empty_array() {
    // Per Go quamina: {"a": []} with exists:true does NOT match
    // but exists:false DOES match (no leaf values)
    let q_true = q!("p1" => r#"{"a": [{"exists": true}]}"#);
    let q_false = q!("p2" => r#"{"a": [{"exists": false}]}"#);

    // Event with empty array
    let event = r#"{"a": []}"#;

    // exists:true should NOT match (no leaf values in empty array)
    assert_no_match!(q_true, event, "exists:true should not match empty array");

    // exists:false SHOULD match (no leaf values means field effectively absent)
    assert_matches!(
        q_false,
        event,
        vec!["p2"],
        "exists:false should match empty array"
    );
}

// ============================================================================
// Nested Field Tests
// ============================================================================

#[test]
fn test_nested_object_pattern() {
    let q = q!("p1" => r#"{"user": {"role": ["admin"]}}"#);

    assert_matches!(
        q,
        r#"{"user": {"role": "admin", "name": "alice"}}"#,
        vec!["p1"],
        "Should match nested field"
    );
    assert_no_match!(q, r#"{"user": {"role": "guest"}}"#);
}

#[test]
fn test_deeply_nested() {
    let q = q!("p1" => r#"{"a": {"b": {"c": ["value"]}}}"#);
    assert_matches!(q, r#"{"a": {"b": {"c": "value"}}}"#, vec!["p1"]);
}

// ============================================================================
// Array Element Matching Tests
// ============================================================================

#[test]
fn test_array_element_matching() {
    // Pattern should match if value is ANY element of the array
    let q = q!("p1" => r#"{"ids": [943]}"#);

    // Event has array - should match if 943 is in the array
    let event = r#"{"ids": [116, 943, 234]}"#;
    assert_matches!(
        q,
        event,
        vec!["p1"],
        "Should match when pattern value is in event array"
    );
}

#[test]
fn test_array_cross_element_matching() {
    // Test cross-element array matching behavior (pattern_ids Go quamina behavior)
    // Pattern {"members": {"given": ["Mick"], "surname": ["Strummer"]}}
    // Event: members=[{given: "Joe", surname: "Strummer"}, {given: "Mick", surname: "Jones"}]
    //
    // Should NOT match because no single array element has both given=Mick AND surname=Strummer

    let q = q!("cross" => r#"{"members": {"given": ["Mick"], "surname": ["Strummer"]}}"#);

    let event = r#"{"members": [
        {"given": "Joe", "surname": "Strummer"},
        {"given": "Mick", "surname": "Jones"}
    ]}"#;

    // Should NOT match - cross-element matching is correctly prevented
    assert_no_match!(q, event, "Should not match across different array elements");
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

    let q = q!(
        // Pattern 1: Mick with surname Strummer - SHOULD NOT match (cross-element)
        "mick_strummer" => r#"{"bands": {"members": {"given": ["Mick"], "surname": ["Strummer"]}}}"#,
        // Pattern 2: Wata with role drums - SHOULD NOT match (cross-element)
        "wata_drums" => r#"{"bands": {"members": {"given": ["Wata"], "role": ["drums"]}}}"#,
        // Pattern 3: Wata with role guitar - SHOULD match (same element)
        "wata_guitar" => r#"{"bands": {"members": {"given": ["Wata"], "role": ["guitar"]}}}"#
    );

    assert_match_count!(q, bands, 1);
    assert_has_match!(q, bands, "wata_guitar");
    assert_no_has_match!(q, bands, "mick_strummer");
    assert_no_has_match!(q, bands, "wata_drums");
}

// ============================================================================
// Multiple Patterns Tests
// ============================================================================

#[test]
fn test_multiple_patterns_same_id() {
    // Multiple patterns with same ID - any match counts
    let q = q!(
        "p1" => r#"{"status": ["active"]}"#,
        "p1" => r#"{"status": ["pending"]}"#
    );

    assert_matches!(q, r#"{"status": "active"}"#, vec!["p1"]);
    assert_matches!(q, r#"{"status": "pending"}"#, vec!["p1"]);
}

#[test]
fn test_or_within_field() {
    // Multiple values in array = OR
    let q = q!("p1" => r#"{"status": ["active", "pending", "review"]}"#);

    for status in &["active", "pending", "review"] {
        let event = format!(r#"{{"status": "{status}"}}"#);
        assert_matches!(q, event, vec!["p1"]);
    }

    assert_no_match!(q, r#"{"status": "deleted"}"#);
}

#[test]
fn test_and_across_fields() {
    // Multiple fields = AND
    let q = q!(
        "p1" => r#"{"type": ["order"], "status": ["pending"], "priority": ["high"]}"#
    );

    assert_matches!(
        q,
        r#"{"type": "order", "status": "pending", "priority": "high"}"#,
        vec!["p1"]
    );

    // Missing one field
    assert_no_match!(q, r#"{"type": "order", "status": "pending"}"#);
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
    assert_has_match!(q, r#"{"status": "active"}"#, "p1");

    // Delete p1
    q.delete_patterns(&"p1").unwrap();

    // p1 no longer pattern_ids
    assert_no_match!(q, r#"{"status": "active"}"#);

    // p2 still works
    assert_has_match!(q, r#"{"status": "pending"}"#, "p2");
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
    assert_has_match!(q, r#"{"status": "pending"}"#, "p2");
    assert_has_match!(q, r#"{"status": "review"}"#, "p3");

    // p1 does not match (and is not in deleted set, was purged)
    assert_no_match!(q, r#"{"status": "active"}"#);
}

/// A delete drops its definitions for good. Adding the id again must not bring
/// them back, or the definition the caller deleted would keep matching in every
/// matcher built from the stored definitions afterwards.
#[test]
fn test_re_add_does_not_revive_deleted_definition() {
    let mut q = Quamina::new();
    q.add_pattern("p", r#"{"x": ["old"]}"#).unwrap();
    q.delete_patterns(&"p").unwrap();
    q.add_pattern("p", r#"{"x": ["new"]}"#).unwrap();

    let clone = q.clone();
    assert_no_match!(
        clone,
        r#"{"x": "old"}"#,
        "a clone must not revive the deleted definition"
    );
    assert_has_match!(clone, r#"{"x": "new"}"#, "p");

    // The re-add left nothing in the deleted set, so the rebuild has to notice
    // the stale definition some other way.
    q.rebuild();
    assert_no_match!(
        q,
        r#"{"x": "old"}"#,
        "rebuild must purge the deleted definition"
    );
    assert_has_match!(q, r#"{"x": "new"}"#, "p");
}

/// The segments tree decides which fields the flattener bothers to extract, so
/// a rebuild or a clear that leaves it alone keeps paying for fields no live
/// pattern mentions any more.
#[test]
fn test_rebuild_and_clear_reset_segments_tree() {
    let mut q = Quamina::new();
    q.add_pattern("keep", r#"{"a": ["x"]}"#).unwrap();
    q.add_pattern("drop", r#"{"b": ["y"]}"#).unwrap();
    assert_eq!(q.segments_tree().fields_count(), 2);

    q.delete_patterns(&"drop").unwrap();
    assert_eq!(
        q.clone().segments_tree().fields_count(),
        1,
        "a clone's tree must cover only the fields its live patterns use"
    );

    q.rebuild();
    assert_eq!(
        q.segments_tree().fields_count(),
        1,
        "rebuild must drop the field only the deleted pattern used"
    );

    q.clear();
    assert_eq!(
        q.segments_tree().fields_count(),
        0,
        "clear must reset the tree along with the patterns"
    );
}

/// The automaton merges a pattern's fields one at a time, so a pattern rejected
/// part-way through has already merged the fields ahead of the one that failed.
/// Those states belong to no pattern the caller holds, and a rebuild is the only
/// thing that can reclaim them.
#[test]
fn test_rebuild_reclaims_states_left_by_a_rejected_add() {
    const LIVE: &str = r#"{"c": [{"prefix": "q"}]}"#;
    fn budgeted() -> Quamina<&'static str> {
        QuaminaBuilder::<&'static str>::new()
            .with_arena_byte_budget(4_000)
            .build()
            .unwrap()
    }

    let mut q = budgeted();
    // Fields are merged in lexical order, so "a" lands before "zb" runs the
    // arena past the budget.
    let rejected = format!(
        r#"{{"a": [{{"prefix": "p"}}], "zb": [{{"prefix": "{}"}}]}}"#,
        "z".repeat(20)
    );
    assert!(
        matches!(
            q.add_pattern("rejected", &rejected),
            Err(QuaminaError::PatternTooComplex(_))
        ),
        "the oversized field has to be rejected for this test to mean anything"
    );
    q.add_pattern("live", LIVE).unwrap();

    q.rebuild();

    let mut live_only = budgeted();
    live_only.add_pattern("live", LIVE).unwrap();
    assert_eq!(
        q.matcher_stats(),
        live_only.matcher_stats(),
        "rebuild must leave only what the accepted pattern built"
    );
}

/// Patterns whose automaton depends on the order they are added in: merging
/// these four in a different order yields a matcher that accepts the same
/// events but has a different state count. Replaying them out of order would
/// therefore hand back a matcher the caller never built.
const ORDER_SENSITIVE_PATTERNS: [(&str, &str); 4] = [
    ("p1", r#"{"a": [{"regexp": "a+b"}]}"#),
    ("p2", r#"{"a": [{"wildcard": "*foo*"}]}"#),
    ("p3", r#"{"a": ["literal"]}"#),
    ("p4", r#"{"a": [{"regexp": "a.*c"}]}"#),
];

/// Drop `deleted` from a matcher holding all of `ORDER_SENSITIVE_PATTERNS`,
/// rebuild, and require the result to be the matcher that adding only the
/// survivors — in the same order — would have built.
fn assert_rebuild_matches_fresh_adds(deleted: &'static str) {
    let mut q = Quamina::new();
    for (id, pattern) in ORDER_SENSITIVE_PATTERNS {
        q.add_pattern(id, pattern).unwrap();
    }
    q.delete_patterns(&deleted).unwrap();
    q.rebuild();

    let mut survivors_only = Quamina::new();
    for (id, pattern) in ORDER_SENSITIVE_PATTERNS {
        if id != deleted {
            survivors_only.add_pattern(id, pattern).unwrap();
        }
    }

    assert_eq!(
        q.matcher_stats(),
        survivors_only.matcher_stats(),
        "rebuild after deleting {deleted} must produce the matcher the surviving adds built"
    );
}

// MIRI SKIP RATIONALE: builds eight regexp/wildcard matchers, ~58s under Miri.
// Coverage: test_rebuild_replays_patterns_in_add_order_miri_friendly runs the
// same check for a single deletion.
#[test]
#[cfg_attr(miri, ignore)]
fn test_rebuild_replays_patterns_in_add_order() {
    for (deleted, _) in ORDER_SENSITIVE_PATTERNS {
        assert_rebuild_matches_fresh_adds(deleted);
    }
}

/// Miri-only: checks one deletion instead of all four. Dropping "p1" is the
/// case where the surviving patterns' hashed order and their add order
/// disagree, so it is the one that catches an unordered replay.
#[test]
#[cfg(miri)]
fn test_rebuild_replays_patterns_in_add_order_miri_friendly() {
    assert_rebuild_matches_fresh_adds("p1");
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
    let _ = q.matches_for_event(br#"{"status": "active"}"#).unwrap();
    assert_eq!(q.pruner_stats().emitted(), 1);
    assert_eq!(q.pruner_stats().filtered(), 0);

    // Delete p1
    q.delete_patterns(&"p1").unwrap();

    // Match active - should increment filtered (was deleted)
    let _ = q.matches_for_event(br#"{"status": "active"}"#).unwrap();
    assert_eq!(q.pruner_stats().emitted(), 1);
    assert_eq!(q.pruner_stats().filtered(), 1);

    // Match pending - should increment emitted
    let _ = q.matches_for_event(br#"{"status": "pending"}"#).unwrap();
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

    // Simulate lots of pattern_ids
    let event = br#"{"x": "a"}"#;
    for _ in 0..500 {
        let _ = q.matches_for_event(event).unwrap();
    }

    // After 500 pattern_ids with 5 patterns, 3 emit, 2 filtered
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
/// With 3 patterns (2 deleted, 1 remaining), 100 pattern_ids yields:
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

/// Unit test for the exact boundary of the 1000-activity threshold in PrunerStats::should_rebuild.
/// Exercises emitted + filtered == 999 (below), == 1000 (at), and == 1001 (above).
#[test]
fn test_should_rebuild_boundary() {
    use super::PrunerStats;

    // Below threshold: 999 total, high ratio → should NOT rebuild
    let stats = PrunerStats::new();
    stats.add_emitted(599);
    stats.add_filtered(400);
    // total = 999, ratio = 400/599 = 0.67 > 0.2, but under minimum
    assert!(!stats.should_rebuild());

    // Exactly at threshold: 1000 total, high ratio → SHOULD rebuild
    stats.add_emitted(1); // now emitted=600, filtered=400, total=1000
    assert!(stats.should_rebuild());

    // Reset and test just above threshold
    stats.reset();
    stats.add_emitted(601);
    stats.add_filtered(400);
    // total = 1001, ratio = 400/601 = 0.67 > 0.2
    assert!(stats.should_rebuild());

    // Above threshold but ratio below 0.2 → should NOT rebuild
    stats.reset();
    stats.add_emitted(900);
    stats.add_filtered(100);
    // total = 1000, ratio = 100/900 = 0.11 < 0.2
    assert!(!stats.should_rebuild());

    // Exactly at ratio boundary: ratio == 0.2 → should NOT rebuild (strict >)
    stats.reset();
    stats.add_emitted(1000);
    stats.add_filtered(200);
    // total = 1200 > 1000 threshold, ratio = 200/1000 = 0.2 exactly
    assert!(!stats.should_rebuild());
}

/// Cloning PrunerStats preserves non-default field values and leaves the
/// original intact. Uses `.clone()` explicitly — a move bypasses the Clone
/// impl entirely and would not catch a mutated impl returning Default.
#[test]
fn test_pruner_stats_clone() {
    use super::PrunerStats;

    let stats = PrunerStats::new();
    stats.add_emitted(42);
    stats.add_filtered(17);

    let cloned = stats.clone();
    assert_eq!(cloned.emitted(), 42);
    assert_eq!(cloned.filtered(), 17);
    assert_eq!(stats.emitted(), 42);
    assert_eq!(stats.filtered(), 17);

    stats.add_emitted(10);
    stats.add_filtered(5);
    assert_eq!(
        cloned.emitted(),
        42,
        "clone must not see mutations to original"
    );
    assert_eq!(
        cloned.filtered(),
        17,
        "clone must not see mutations to original"
    );
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
    assert_no_match!(snapshot, r#"{"status": "pending"}"#);

    // Original has p2
    assert_has_match!(q, r#"{"status": "pending"}"#, "p2");
}

#[test]
fn test_clone_replays_patterns_in_add_order() {
    let mut q = Quamina::new();
    for (id, pattern) in ORDER_SENSITIVE_PATTERNS {
        q.add_pattern(id, pattern).unwrap();
    }

    assert_eq!(
        q.clone().matcher_stats(),
        q.matcher_stats(),
        "a clone must be the same matcher, not a re-merge in another order"
    );
}

#[test]
fn test_send_sync() {
    // Verify Quamina is Send + Sync for thread safety
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<Quamina<String>>();
}

#[test]
fn test_has_matches() {
    let q = q!("p1" => r#"{"status": ["active"]}"#);

    assert!(q.has_matches(br#"{"status": "active"}"#).unwrap());
    assert!(!q.has_matches(br#"{"status": "inactive"}"#).unwrap());
}

#[test]
fn test_count_matches() {
    let q = q!(
        "p1" => r#"{"status": ["active"]}"#,
        "p2" => r#"{"status": ["active"]}"#,
        "p3" => r#"{"status": ["pending"]}"#
    );

    assert_eq!(q.count_matches(br#"{"status": "active"}"#).unwrap(), 2);
    assert_eq!(q.count_matches(br#"{"status": "pending"}"#).unwrap(), 1);
    assert_eq!(q.count_matches(br#"{"status": "deleted"}"#).unwrap(), 0);
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
    assert!(q.list_pattern_ids().is_empty(), "clear must drop all ids");
}

// ============================================================================
// Error Handling Tests
// ============================================================================

#[test]
fn test_invalid_json_events() {
    // Based on Go quamina's TestFJErrorCases
    let q = q!("p1" => r#"{"a": [1]}"#);

    let bad_events: &[(&[u8], &str)] = &[
        // Truncated JSON
        (br#"{"a"#, "Truncated JSON"),
        (br#"{"a": "#, "Truncated value"),
        (br#"{"a": ["#, "Truncated array"),
        // Empty input
        (b"", "Empty input"),
        // Non-object at top level
        (br#""string""#, "String at top level"),
        (br"[1, 2]", "Array at top level"),
        (b"123", "Number at top level"),
        // Malformed JSON
        (br#"{ "a" : }"#, "Missing value"),
        // Invalid escape sequences
        (br#"{"a": "a\zb"}"#, "Invalid escape \\z in value"),
        (br#"{"a\zb": 2}"#, "Invalid escape in field name"),
        // Invalid value identifier
        (br#"{"a": xx}"#, "Invalid value xx"),
        // Truncated/invalid literals
        (br#"{"a": tru}"#, "Truncated 'tru'"),
        (br#"{"a": truse}"#, "Invalid 'truse'"),
    ];

    for (event, desc) in bad_events {
        assert!(q.matches_for_event(event).is_err(), "{desc} should error");
    }
}

#[test]
fn test_invalid_pattern_handling() {
    let mut q = Quamina::new();

    // Empty pattern
    assert_add_err!(q, "p1", "");

    // Non-object at top level
    assert_add_err!(q, "p2", "33");
    assert_add_err!(q, "p3", "[1,2]");

    // Malformed JSON
    assert_add_err!(q, "p4", "{");
    assert_add_err!(q, "p5", r#"{"foo": }"#);

    // Pattern field must be array or nested object
    assert_add_err!(q, "p6", r#"{"foo": "string"}"#);
    assert_add_err!(q, "p7", r#"{"foo": 123}"#);
    assert_add_err!(q, "p8", r#"{"foo": true}"#);

    // Valid patterns should work
    assert_add_ok!(q, "valid1", r#"{"x": [1]}"#);
    assert_add_ok!(q, "valid2", r#"{"x": ["string"]}"#);
    assert_add_ok!(q, "valid3", r#"{"x": {"y": [1]}}"#);
}

#[test]
fn test_bad_pattern_error_handling() {
    let mut q = Quamina::new();

    // Go quamina returns errors for these patterns (anything_but_test.go:134)
    // Empty anything-but
    assert_add_err!(q, "p1", r#"{"x": [{"anything-but": []}]}"#);

    // Mixed types in anything-but
    assert_add_err!(q, "p2", r#"{"x": [{"anything-but": ["a", 1]}]}"#);
}

#[test]
fn test_bad_event_error_handling() {
    let q = q!("p1" => r#"{"x": [1]}"#);

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
    let result = q.matches_for_event(br#"{"likes": "tacos"}"#);
    assert!(result.is_ok(), "Should not panic with empty matcher");
    assert!(result.unwrap().is_empty(), "No pattern_ids expected");
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
    let pattern_ids = q.matches_for_event(br#"{"status": "active"}"#).unwrap();
    assert_eq!(pattern_ids, vec!["p1".to_string()]);
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
    let pattern_ids = q.matches_for_event(br#"{"x": 1}"#).unwrap();
    assert_eq!(pattern_ids, vec![42]);

    // With &str as pattern ID
    let mut q = QuaminaBuilder::<&str>::new().build().unwrap();
    q.add_pattern("test", r#"{"x": [1]}"#).unwrap();
    let pattern_ids = q.matches_for_event(br#"{"x": 1}"#).unwrap();
    assert_eq!(pattern_ids, vec!["test"]);
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
        Box::new(Self {
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
    let pattern_ids = q.matches_for_event(b"ignored event data").unwrap();
    assert_eq!(pattern_ids, vec!["p1".to_string()]);
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

    let pattern_ids = q.matches_for_event(b"ignored").unwrap();
    assert!(pattern_ids.is_empty());
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

    let pattern_ids = q.matches_for_event(b"ignored").unwrap();
    assert_eq!(pattern_ids, vec!["p1".to_string()]);
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

    let pattern_ids = q.matches_for_event(br#"{"status": "active"}"#).unwrap();
    assert_eq!(pattern_ids, vec!["p1"]);
}

// ============================================================================
// Additional Core Tests (recovered from original)
// ============================================================================

#[test]
fn test_same_pattern_id_multiple_value_types() {
    // Based on Go quamina's TestExerciseSingletonReplacement and TestMergeNfaAndNumeric
    // Same pattern ID can match via different value types (string OR number)
    let q = q!("x" => r#"{"x": ["a"]}"#, "x" => r#"{"x": [1]}"#);

    // Both string and number should match pattern "x"
    assert_matches!(q, r#"{"x": 1}"#, vec!["x"], "number 1 should match");
    assert_matches!(q, r#"{"x": "a"}"#, vec!["x"], "string 'a' should match");

    // Test wildcard OR number for same pattern ID
    let q2 = q!("x" => r#"{"x": [{"wildcard": "x*y"}]}"#, "x" => r#"{"x": [3]}"#);

    assert_matches!(q2, r#"{"x": 3}"#, vec!["x"], "number 3 should match");
    assert_matches!(
        q2,
        r#"{"x": "xasdfy"}"#,
        vec!["x"],
        "wildcard pattern should match"
    );
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

    assert_match_count!(q, event, patterns.len());
    for (_, name) in &patterns {
        assert_has_match!(q, event, *name);
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
        assert!(result.is_err(), "{desc} should be rejected: {pattern}");
    }
}

#[test]
fn test_numbits_boundary_values() {
    // Test float64 boundary values for numeric matching
    use crate::numbits::{numbits_from_f64, q_num_from_f64, to_q_number};

    // Float64 boundary categories:
    // - Subnormal (smallest positive): 2^-1074 to 2^-1022
    // - Normal minimum: 2^-1022 ~ 2.225e-308
    // - Normal maximum: (2 - 2^-52) x 2^1023 ~ 1.798e+308

    // Test zero
    let nb_zero = numbits_from_f64(0.0);
    let q_zero = q_num_from_f64(0.0);
    assert!(nb_zero > 0, "Zero should have non-zero numbits");
    assert!(!q_zero.is_empty(), "Zero should have non-empty Q-number");

    // Test smallest positive subnormal: f64::MIN_POSITIVE / 2^52 ~ 4.94e-324
    let smallest_subnormal = 5e-324_f64;
    let nb_small = numbits_from_f64(smallest_subnormal);
    let q_small = q_num_from_f64(smallest_subnormal);
    assert!(nb_small > nb_zero, "Smallest subnormal > 0");
    assert!(
        q_small > q_zero,
        "Smallest subnormal Q-number > zero Q-number"
    );

    // Test smallest normal: f64::MIN_POSITIVE ~ 2.225e-308
    let smallest_normal = f64::MIN_POSITIVE;
    let nb_min_normal = numbits_from_f64(smallest_normal);
    let q_min_normal = q_num_from_f64(smallest_normal);
    assert!(
        nb_min_normal > nb_small,
        "Smallest normal > smallest subnormal"
    );
    assert!(q_min_normal > q_small, "Q-number ordering preserved");

    // Test largest normal: f64::MAX ~ 1.798e+308
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
        // First byte must be Q_NUMBER_PREFIX (0x80)
        assert_eq!(
            q[0],
            crate::numbits::Q_NUMBER_PREFIX,
            "Q-number should start with prefix for value {val}"
        );
        // Content bytes (after prefix) must be < 128 (base-128 encoding)
        for &byte in &q[1..] {
            assert!(
                byte < 128,
                "Q-number content byte {byte} >= 128 for value {val}"
            );
        }
    }

    // Test numbits round-trip consistency
    for &val in &test_values {
        let nb = numbits_from_f64(val);
        let q1 = q_num_from_f64(val);
        let q2 = to_q_number(nb);
        assert_eq!(q1, q2, "Q-number should match via both paths for {val}");
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

        // Generate a random f64 in a wide range. We build the mantissa via
        // the standard IEEE-754 trick: stuff 52 random bits into a known
        // exponent of 1.0, giving a uniform [1.0, 2.0); subtracting 1.0
        // yields a uniform [0.0, 1.0) without any lossy `as f64` casts.
        let sign = if rng_state & 1 == 0 { 1.0 } else { -1.0 };
        let exp = i32::try_from((rng_state >> 1) % 600).expect("range mod 600 fits in i32") - 300;
        let mantissa = f64::from_bits((rng_state >> 12) | 0x3FF0_0000_0000_0000) - 1.0;
        let val = sign * (1.0 + mantissa) * 10f64.powi(exp);

        // Skip if not finite (shouldn't happen with our construction, but be safe)
        if !val.is_finite() {
            continue;
        }

        let q = q_num_from_f64(val);

        // Property 1: Non-empty
        assert!(
            !q.is_empty(),
            "Q-number should be non-empty for value at index {i}"
        );

        // Property 2: First byte is prefix, content bytes < 128
        assert_eq!(
            q[0],
            crate::numbits::Q_NUMBER_PREFIX,
            "Q-number should start with prefix for value at index {i}"
        );
        for (j, &byte) in q[1..].iter().enumerate() {
            assert!(
                byte < 128,
                "Q-number content byte {} at pos {} >= 128 for value at index {}",
                byte,
                j + 1,
                i
            );
        }

        // Property 3: Content bytes (after prefix) are valid ASCII
        assert!(
            std::str::from_utf8(&q[1..]).is_ok(),
            "Q-number content should be valid UTF-8 for value at index {i}"
        );

        // Property 4: Length bounded (1 prefix + up to 10 content bytes)
        assert!(
            q.len() <= 11,
            "Q-number length {} exceeds max 11 for value at index {}",
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
        // Same IEEE-754 mantissa trick as above for a uniform [0.0, 1.0).
        let unit = f64::from_bits((rng_state >> 12) | 0x3FF0_0000_0000_0000) - 1.0;
        let val = unit.mul_add(2e100, -1e100);
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
                "Q-number ordering violated: {prev_val} ({prev_q:?}) should be <= {val} ({q:?})"
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
            pattern: pattern3,
            byte_length: 1,
        }, // cost 40
        LookaroundCondition::PositiveLookbehind {
            pattern: pattern2,
            byte_length: 1,
        }, // cost 30
        LookaroundCondition::NegativeLookahead(pattern1), // cost 20
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

#[test]
fn test_string_number_type_distinction() {
    // Verify that string patterns don't match number events and vice versa.
    // In Go, the outer quotes on string values act as an implicit type tag
    // (NFA expects `"123"` for strings vs `123` for numbers).
    // In Rust, quotes are stripped by value_bytes(), so we need to verify
    // the type distinction is maintained by other means.

    let q = q!("string_pat" => r#"{"key": ["123"]}"#);

    // String "123" SHOULD match
    assert_matches!(
        q,
        r#"{"key": "123"}"#,
        vec!["string_pat"],
        "String '123' should match string pattern '123'"
    );

    // Number 123 should NOT match string pattern "123"
    assert_no_match!(
        q,
        r#"{"key": 123}"#,
        "Number 123 should NOT match string pattern '123' - type distinction must be preserved"
    );
}

#[test]
fn test_numeric_pattern_should_not_match_string_event() {
    // The reverse: numeric pattern should not match a string with the same digits
    let q = q!("num_pat" => r#"{"key": [42]}"#);

    // Number 42 SHOULD match
    assert_matches!(
        q,
        r#"{"key": 42}"#,
        vec!["num_pat"],
        "Number 42 should match numeric pattern"
    );

    // String "42" should NOT match numeric pattern
    assert_no_match!(
        q,
        r#"{"key": "42"}"#,
        "String '42' should NOT match numeric pattern 42"
    );
}

#[test]
fn test_mixed_string_and_number_patterns_same_digits() {
    // Both a string pattern and a numeric pattern for "123"/123
    let q = q!("str" => r#"{"key": ["123"]}"#, "num" => r#"{"key": [123]}"#);

    // String event should only match string pattern
    assert_matches!(
        q,
        r#"{"key": "123"}"#,
        vec!["str"],
        "String '123' should match only string pattern"
    );

    // Number event should only match numeric pattern
    assert_matches!(
        q,
        r#"{"key": 123}"#,
        vec!["num"],
        "Number 123 should match only numeric pattern"
    );
}

#[test]
fn test_mixed_number_and_string_in_same_value_array() {
    // Go's TestBasicMatching: a single pattern field with both numbers and strings
    // e.g. {"b": [1, "3"]} should match number 1 OR string "3" but NOT number 3
    let q = q!("p1" => r#"{"a": [1, 2], "b": [1, "3"]}"#);

    // String "3" on field b should match
    assert_matches!(
        q,
        r#"{"a": 1, "b": "3"}"#,
        vec!["p1"],
        "String '3' should match the string literal in [1, \"3\"]"
    );

    // Number 1 on field b should match
    assert_matches!(
        q,
        r#"{"a": 2, "b": 1}"#,
        vec!["p1"],
        "Number 1 should match the numeric literal in [1, \"3\"]"
    );

    // Number 3 on field b should NOT match (it's string "3" in the pattern, not number 3)
    assert_no_match!(
        q,
        r#"{"a": 1, "b": 3}"#,
        "Number 3 should NOT match string '3' in [1, \"3\"]"
    );

    // Reversed field order in the event should still work
    assert_matches!(
        q,
        r#"{"b": "3", "a": 1}"#,
        vec!["p1"],
        "Reversed field order should still match"
    );

    // Extra fields in the event should not interfere
    assert_matches!(
        q,
        r#"{"a": 2, "b": "3", "x": 99}"#,
        vec!["p1"],
        "Extra fields should not prevent match"
    );

    // Missing field b should not match
    assert_no_match!(q, r#"{"a": 1}"#, "Missing field b should not match");

    // Missing field a should not match
    assert_no_match!(q, r#"{"b": "3"}"#, "Missing field a should not match");

    // Wrong value on field a should not match
    assert_no_match!(
        q,
        r#"{"b": "3", "a": 6}"#,
        "Wrong value on field a should not match"
    );
}

// Regression tests for numeric range false positives on string values.
// Before the Q_NUMBER_PREFIX fix, numeric range FAs would match raw ASCII bytes
// because Q-number first bytes were small (e.g. 1 for 10.0), causing any byte
// > bound[0] to match — including all printable ASCII.

#[test]
fn test_numeric_range_should_not_match_string_event() {
    let mut q = Quamina::<&str>::new();
    q.add_pattern("gt10", r#"{"val": [{"numeric": [">", 10]}]}"#)
        .unwrap();

    // String values must NOT match numeric range patterns
    assert_no_match!(
        q,
        r#"{"val": "hello"}"#,
        "String 'hello' should NOT match numeric range > 10"
    );
    assert_no_match!(
        q,
        r#"{"val": "999"}"#,
        "String '999' should NOT match numeric range > 10"
    );
}

#[test]
fn test_numeric_range_should_match_numeric_event() {
    let mut q = Quamina::<&str>::new();
    q.add_pattern("gt10", r#"{"val": [{"numeric": [">", 10]}]}"#)
        .unwrap();

    assert_matches!(q, r#"{"val": 50}"#, vec!["gt10"]);
    assert_matches!(q, r#"{"val": 100.5}"#, vec!["gt10"]);
    assert_no_match!(q, r#"{"val": 5}"#);
    assert_no_match!(q, r#"{"val": 10}"#); // strictly greater
}

#[test]
fn test_numeric_exact_still_works_with_prefix() {
    let q = q!("n42" => r#"{"key": [42]}"#);
    assert_matches!(q, r#"{"key": 42}"#, vec!["n42"]);
    assert_no_match!(q, r#"{"key": "42"}"#);
    assert_no_match!(q, r#"{"key": 43}"#);
}

#[test]
fn test_empty_matcher_returns_no_matches() {
    // A brand-new Quamina with no patterns should return empty pattern_ids for any event
    let q = Quamina::<&str>::new();

    assert_no_match!(
        q,
        r#"{"status": "active"}"#,
        "Empty matcher should return no pattern_ids"
    );
    assert_no_match!(
        q,
        r#"{"a": 1, "b": "hello"}"#,
        "Empty matcher should return no pattern_ids for any event"
    );
}

#[test]
fn test_idempotent_add_and_delete() {
    // Adding the same pattern ID with the same pattern twice, and deleting twice
    let mut q = Quamina::new();

    // Add same ID + same pattern twice
    q.add_pattern("p1", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("p1", r#"{"x": ["a"]}"#).unwrap();

    // Should still match (and only return "p1" once, not duplicated)
    assert_matches!(
        q,
        r#"{"x": "a"}"#,
        vec!["p1"],
        "Duplicate add should still match"
    );

    // Delete once
    q.delete_patterns(&"p1").unwrap();
    assert_no_match!(q, r#"{"x": "a"}"#, "After delete, should not match");

    // Delete again (idempotent) -- should not panic
    q.delete_patterns(&"p1").unwrap();
    assert_no_match!(q, r#"{"x": "a"}"#, "Second delete should be idempotent");

    // Rebuild after double delete should not panic
    let purged = q.rebuild();
    // Both adds registered under "p1", so rebuild purges them
    assert!(purged >= 1, "Rebuild should purge the deleted pattern(s)");
}

#[test]
fn test_delete_multi_pattern_id_removes_all() {
    // Add multiple different patterns under the same ID, then delete that ID
    let mut q = Quamina::new();
    q.add_pattern("shared", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("shared", r#"{"x": [1]}"#).unwrap();
    q.add_pattern("shared", r#"{"y": [{"prefix": "b"}]}"#)
        .unwrap();

    // All three patterns should match under "shared"
    assert_matches!(
        q,
        r#"{"x": "a"}"#,
        vec!["shared"],
        "String pattern should match"
    );
    assert_matches!(
        q,
        r#"{"x": 1}"#,
        vec!["shared"],
        "Numeric pattern should match"
    );
    assert_matches!(
        q,
        r#"{"y": "bcd"}"#,
        vec!["shared"],
        "Prefix pattern should match"
    );

    // Delete "shared" -- should remove ALL three patterns
    q.delete_patterns(&"shared").unwrap();

    assert_no_match!(
        q,
        r#"{"x": "a"}"#,
        "String pattern should be gone after delete"
    );
    assert_no_match!(
        q,
        r#"{"x": 1}"#,
        "Numeric pattern should be gone after delete"
    );
    assert_no_match!(
        q,
        r#"{"y": "bcd"}"#,
        "Prefix pattern should be gone after delete"
    );

    // Rebuild should purge the one deleted ID
    let purged = q.rebuild();
    assert_eq!(purged, 1, "Rebuild should purge 1 deleted ID");

    // After rebuild, still no pattern_ids (patterns are permanently gone)
    assert_no_match!(
        q,
        r#"{"x": "a"}"#,
        "String pattern should stay gone after rebuild"
    );
    assert_no_match!(
        q,
        r#"{"x": 1}"#,
        "Numeric pattern should stay gone after rebuild"
    );
    assert_no_match!(
        q,
        r#"{"y": "bcd"}"#,
        "Prefix pattern should stay gone after rebuild"
    );
}

// ============================================================================
// list_pattern_ids / contains_pattern / delete interaction tests
// ============================================================================

#[test]
fn test_list_pattern_ids_basic() {
    let mut q: Quamina<String> = Quamina::new();
    assert!(q.list_pattern_ids().is_empty());

    q.add_pattern("p1".into(), r#"{"x": [1]}"#).unwrap();
    q.add_pattern("p2".into(), r#"{"y": [2]}"#).unwrap();

    let mut ids: Vec<&String> = q.list_pattern_ids();
    ids.sort();
    assert_eq!(ids, vec!["p1", "p2"]);
}

#[test]
fn test_list_pattern_ids_excludes_deleted() {
    let mut q: Quamina<String> = Quamina::new();
    q.add_pattern("p1".into(), r#"{"x": [1]}"#).unwrap();
    q.add_pattern("p2".into(), r#"{"y": [2]}"#).unwrap();
    q.add_pattern("p3".into(), r#"{"z": [3]}"#).unwrap();

    q.delete_patterns(&"p2".into()).unwrap();

    let mut ids: Vec<&String> = q.list_pattern_ids();
    ids.sort();
    assert_eq!(ids, vec!["p1", "p3"]);
}

#[test]
fn test_contains_pattern_basic() {
    let mut q: Quamina<String> = Quamina::new();
    let p1: String = "p1".into();
    let p2: String = "p2".into();
    let missing: String = "missing".into();

    assert!(!q.contains_pattern(&p1));

    q.add_pattern(p1.clone(), r#"{"x": [1]}"#).unwrap();
    q.add_pattern(p2.clone(), r#"{"y": [2]}"#).unwrap();

    assert!(q.contains_pattern(&p1));
    assert!(q.contains_pattern(&p2));
    assert!(!q.contains_pattern(&missing));
}

#[test]
fn test_contains_pattern_after_delete() {
    let mut q: Quamina<String> = Quamina::new();
    let p1: String = "p1".into();

    q.add_pattern(p1.clone(), r#"{"x": [1]}"#).unwrap();
    assert!(q.contains_pattern(&p1));

    q.delete_patterns(&p1).unwrap();
    assert!(!q.contains_pattern(&p1));
}

#[test]
fn test_delete_nonexistent_pattern_is_noop() {
    let mut q: Quamina<String> = Quamina::new();
    q.add_pattern("p1".into(), r#"{"x": [1]}"#).unwrap();

    // Deleting a pattern that was never added should be a no-op
    q.delete_patterns(&"ghost".into()).unwrap();

    // p1 is still there
    assert!(q.contains_pattern(&"p1".into()));
    assert_eq!(q.list_pattern_ids().len(), 1);

    // rebuild should report 0 purged (nothing was actually deleted)
    assert_eq!(q.rebuild(), 0);
}

#[test]
fn test_delete_already_deleted_pattern_is_noop() {
    let mut q: Quamina<String> = Quamina::new();
    q.add_pattern("p1".into(), r#"{"x": [1]}"#).unwrap();

    q.delete_patterns(&"p1".into()).unwrap();
    assert!(!q.contains_pattern(&"p1".into()));

    // Deleting again should not panic or change state
    q.delete_patterns(&"p1".into()).unwrap();
    assert!(!q.contains_pattern(&"p1".into()));
    assert!(q.list_pattern_ids().is_empty());
}

// ============================================================================
// Pattern Complexity Limit Tests
// ============================================================================

// --- Depth Limit Tests ---

#[test]
#[cfg_attr(miri, ignore)]
fn test_pattern_depth_at_limit() {
    // Pattern nested exactly 256 levels deep should succeed with default limits
    let mut q = Quamina::new();
    let mut pattern = String::new();
    let mut closing = String::new();
    for i in 0..256 {
        pattern.push_str(&format!("{{\"f{i}\": "));
        closing.push('}');
    }
    pattern.push_str("[\"val\"]");
    pattern.push_str(&closing);

    assert!(
        q.add_pattern("deep", &pattern).is_ok(),
        "Pattern at exactly max depth (256) should succeed"
    );
}

/// Miri-friendly variant of test_pattern_depth_at_limit.
///
/// Uses 8 nesting levels instead of 256 to keep Miri runtime manageable while
/// still exercising deep-pattern parsing and automaton construction.
#[test]
fn test_pattern_depth_at_limit_miri_friendly() {
    let mut q = Quamina::new();
    let mut pattern = String::new();
    let mut closing = String::new();
    for i in 0..8 {
        pattern.push_str(&format!("{{\"f{i}\": "));
        closing.push('}');
    }
    pattern.push_str("[\"val\"]");
    pattern.push_str(&closing);

    assert!(
        q.add_pattern("deep", &pattern).is_ok(),
        "8-level nested pattern should succeed"
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_pattern_depth_exceeds_limit() {
    // Pattern nested 257 levels should fail
    let mut q = Quamina::new();
    let mut pattern = String::new();
    let mut closing = String::new();
    for i in 0..257 {
        pattern.push_str(&format!("{{\"f{i}\": "));
        closing.push('}');
    }
    pattern.push_str("[\"val\"]");
    pattern.push_str(&closing);

    let result = q.add_pattern("deep", &pattern);
    assert!(result.is_err(), "Pattern exceeding max depth should fail");
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("depth"),
        "Error should mention depth: {err_msg}"
    );
    assert!(
        err_msg.contains("257"),
        "Error should mention actual depth 257: {err_msg}"
    );
    assert!(
        err_msg.contains("256"),
        "Error should mention max depth 256: {err_msg}"
    );
}

#[test]
fn test_pattern_depth_custom_limit() {
    // Builder with max_depth=5, pattern at depth 6 should fail
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_pattern_depth(5)
        .build()
        .unwrap();

    let pattern = r#"{"a": {"b": {"c": {"d": {"e": {"f": ["val"]}}}}}}"#;
    let result = q.add_pattern("deep", pattern);
    assert!(
        result.is_err(),
        "Pattern at depth 6 should fail with max_depth=5"
    );
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("depth"),
        "Error should mention depth: {err_msg}"
    );
    // The message embeds the actual and configured depths. Asserting them here
    // keeps that formatting covered under Miri, which skips the 256/257-deep
    // test_pattern_depth_{at,exceeds}_limit tests.
    assert!(
        err_msg.contains('6'),
        "Error should mention actual depth 6: {err_msg}"
    );
    assert!(
        err_msg.contains('5'),
        "Error should mention max depth 5: {err_msg}"
    );
}

#[test]
fn test_pattern_depth_shallow_ok() {
    // Normal 3-level nesting with defaults should succeed
    let mut q = Quamina::new();
    let result = q.add_pattern("p1", r#"{"a": {"b": {"c": ["value"]}}}"#);
    assert!(
        result.is_ok(),
        "Normal 3-level nesting should succeed with defaults"
    );
}

// --- Field Count Limit Tests ---

#[test]
#[cfg_attr(miri, ignore)]
fn test_pattern_fields_at_limit() {
    // Pattern with exactly 256 fields should succeed
    let mut q = Quamina::new();
    let mut fields: Vec<String> = Vec::new();
    for i in 0..256 {
        fields.push(format!("\"f{i}\": [\"v\"]"));
    }
    let pattern = format!("{{{}}}", fields.join(", "));
    assert!(
        q.add_pattern("wide", &pattern).is_ok(),
        "Pattern with exactly 256 fields should succeed"
    );
}

/// Miri-friendly variant of test_pattern_fields_at_limit.
///
/// Uses 8 fields instead of 256 to keep Miri runtime manageable while
/// still exercising the multi-field pattern parsing and automaton construction.
#[test]
fn test_pattern_fields_at_limit_miri_friendly() {
    let mut q = Quamina::new();
    let mut fields: Vec<String> = Vec::new();
    for i in 0..8 {
        fields.push(format!("\"f{i}\": [\"v\"]"));
    }
    let pattern = format!("{{{}}}", fields.join(", "));
    assert!(
        q.add_pattern("wide", &pattern).is_ok(),
        "Pattern with 8 fields should succeed"
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_pattern_fields_exceeds_limit() {
    // Pattern with 257 fields should fail
    let mut q = Quamina::new();
    let mut fields: Vec<String> = Vec::new();
    for i in 0..257 {
        fields.push(format!("\"f{i}\": [\"v\"]"));
    }
    let pattern = format!("{{{}}}", fields.join(", "));

    let result = q.add_pattern("wide", &pattern);
    assert!(
        result.is_err(),
        "Pattern with 257 fields should exceed limit"
    );
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("257"),
        "Error should mention actual count 257: {err_msg}"
    );
    assert!(
        err_msg.contains("256"),
        "Error should mention max count 256: {err_msg}"
    );
}

#[test]
fn test_pattern_fields_custom_limit() {
    // Builder with max_fields=3, pattern with 4 fields should fail
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_fields_per_pattern(3)
        .build()
        .unwrap();

    let pattern = r#"{"a": ["1"], "b": ["2"], "c": ["3"], "d": ["4"]}"#;
    let result = q.add_pattern("wide", pattern);
    assert!(
        result.is_err(),
        "Pattern with 4 fields should fail with max_fields=3"
    );
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("fields"),
        "Error should mention fields: {err_msg}"
    );
}

// --- Arena Byte Budget Tests ---

#[test]
fn test_arena_budget_exceeded() {
    // Builder with tiny budget (1KB), pattern triggering arena construction should fail
    let mut q = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(1024)
        .build()
        .unwrap();

    // Add many patterns to trigger arena growth beyond 1KB
    // First pattern may succeed (singleton optimization), but subsequent ones will build arena
    let _ = q.add_pattern("p1", r#"{"x": ["a"]}"#);
    let _ = q.add_pattern("p2", r#"{"x": ["b"]}"#);

    // Add enough patterns to exceed the tiny budget
    let mut exceeded = false;
    for i in 0..100 {
        let pattern = format!("{{\"x\": [\"value_that_is_long_enough_{i}\"]}}");
        if q.add_pattern("px", &pattern).is_err() {
            exceeded = true;
            break;
        }
    }
    assert!(exceeded, "Arena budget should be exceeded with 1KB limit");
}

#[test]
fn test_arena_budget_sufficient() {
    // Default budget (10MB), normal patterns should work fine
    let mut q = Quamina::new();
    for i in 0..50 {
        let pattern = format!("{{\"field{i}\": [\"value{i}\"]}}");
        assert!(
            q.add_pattern("p1", &pattern).is_ok(),
            "Normal patterns should work within default 10MB budget"
        );
    }
}

#[test]
fn test_arena_budget_custom() {
    // Builder with 1MB budget, patterns work within it
    let mut q = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(1024 * 1024)
        .build()
        .unwrap();

    for i in 0..20 {
        let pattern = format!("{{\"field{i}\": [\"value{i}\"]}}");
        assert!(
            q.add_pattern("p1", &pattern).is_ok(),
            "Moderate patterns should work within 1MB budget"
        );
    }
}

// --- Error Message Quality Tests ---

#[test]
fn test_depth_error_includes_path() {
    // The error should contain the field path where depth was exceeded
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_pattern_depth(2)
        .build()
        .unwrap();

    let pattern = r#"{"a": {"b": {"c": ["val"]}}}"#;
    let result = q.add_pattern("deep", pattern);
    assert!(result.is_err());
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains("pattern too complex"),
        "Error should start with 'pattern too complex': {err_msg}"
    );
}

#[test]
fn test_field_count_error_includes_count() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_fields_per_pattern(2)
        .build()
        .unwrap();

    let pattern = r#"{"a": ["1"], "b": ["2"], "c": ["3"]}"#;
    let result = q.add_pattern("wide", pattern);
    assert!(result.is_err());
    let err_msg = format!("{}", result.unwrap_err());
    assert!(
        err_msg.contains('3'),
        "Error should contain actual field count: {err_msg}"
    );
    assert!(
        err_msg.contains('2'),
        "Error should contain max field count: {err_msg}"
    );
}

#[test]
fn test_arena_error_includes_bytes() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(1)
        .build()
        .unwrap();

    // This should fail because budget is 1 byte
    let _ = q.add_pattern("p1", r#"{"x": ["a"]}"#);
    let result = q.add_pattern("p2", r#"{"x": ["b"]}"#);
    if let Err(e) = result {
        let err_msg = format!("{e}");
        assert!(
            err_msg.contains("bytes") && err_msg.contains("budget"),
            "Error should mention bytes and budget: {err_msg}"
        );
    }
    // With a 1-byte budget, at least one of the two patterns should fail
}

// --- Integration Tests ---

#[test]
#[cfg_attr(miri, ignore)]
fn test_default_limits_allow_normal_patterns() {
    // All operator types should work under default limits
    let mut q = Quamina::new();

    assert_add_ok!(q, "exact", r#"{"x": ["hello"]}"#);
    assert_add_ok!(q, "num", r#"{"x": [42]}"#);
    assert_add_ok!(q, "prefix", r#"{"x": [{"prefix": "he"}]}"#);
    assert_add_ok!(q, "suffix", r#"{"x": [{"suffix": "lo"}]}"#);
    assert_add_ok!(q, "shell", r#"{"x": [{"shellstyle": "h*o"}]}"#);
    assert_add_ok!(q, "wild", r#"{"x": [{"wildcard": "h*o"}]}"#);
    assert_add_ok!(q, "ab", r#"{"x": [{"anything-but": ["no"]}]}"#);
    assert_add_ok!(q, "eic", r#"{"x": [{"equals-ignore-case": "HELLO"}]}"#);
    assert_add_ok!(q, "re", r#"{"x": [{"regex": "[a-z]+"}]}"#);
    assert_add_ok!(q, "numr", r#"{"x": [{"numeric": [">=", 1, "<", 100]}]}"#);
    assert_add_ok!(q, "cidr", r#"{"x": [{"cidr": "10.0.0.0/8"}]}"#);
    assert_add_ok!(q, "exists", r#"{"x": [{"exists": true}]}"#);
}

/// Miri-friendly variant of test_default_limits_allow_normal_patterns.
///
/// Drops CIDR and regex (the two most expensive arena builders under Miri)
/// while still exercising the other 10 operator types.
#[test]
fn test_default_limits_allow_normal_patterns_miri_friendly() {
    let mut q = Quamina::new();

    assert_add_ok!(q, "exact", r#"{"x": ["hello"]}"#);
    assert_add_ok!(q, "num", r#"{"x": [42]}"#);
    assert_add_ok!(q, "prefix", r#"{"x": [{"prefix": "he"}]}"#);
    assert_add_ok!(q, "suffix", r#"{"x": [{"suffix": "lo"}]}"#);
    assert_add_ok!(q, "shell", r#"{"x": [{"shellstyle": "h*o"}]}"#);
    assert_add_ok!(q, "wild", r#"{"x": [{"wildcard": "h*o"}]}"#);
    assert_add_ok!(q, "ab", r#"{"x": [{"anything-but": ["no"]}]}"#);
    assert_add_ok!(q, "eic", r#"{"x": [{"equals-ignore-case": "HELLO"}]}"#);
    assert_add_ok!(q, "numr", r#"{"x": [{"numeric": [">=", 1, "<", 100]}]}"#);
    assert_add_ok!(q, "exists", r#"{"x": [{"exists": true}]}"#);
}

/// Patterns with many distinct values on the same field must be rejected
/// once the arena byte budget is exhausted. This is a regression test for C3
/// (add_string_transition previously skipped the budget check entirely).
#[test]
fn test_arena_budget_enforced_on_repeated_exact_strings() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(4096)
        .build()
        .unwrap();

    let mut rejected = false;
    for i in 0..500 {
        let pattern = format!(r#"{{"x": ["long_value_string_number_{i}"]}}"#);
        if q.add_pattern("p", &pattern).is_err() {
            rejected = true;
            break;
        }
    }
    assert!(
        rejected,
        "Budget should be enforced when many exact strings are added to the same field"
    );
}

/// After a rejected add_pattern, existing patterns must still match correctly.
/// This is a regression test for C1 (rejected patterns must not corrupt state)
/// and for M4 (partial transitions must not produce false positives).
#[test]
fn test_matcher_correct_after_rejected_pattern() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(4096)
        .build()
        .unwrap();

    // Add a pattern that succeeds
    q.add_pattern("good", r#"{"x": ["hello"]}"#).unwrap();

    // Keep adding until one is rejected
    let mut rejected = false;
    for i in 0..500 {
        let pattern = format!(r#"{{"x": ["overflow_value_{i}"]}}"#);
        if q.add_pattern("bad", &pattern).is_err() {
            rejected = true;
            break;
        }
    }
    assert!(rejected, "Should have hit budget limit");

    // The original "good" pattern must still match
    assert_has_match!(q, r#"{"x": "hello"}"#, "good");

    // A non-matching event must still return empty
    assert_no_match!(
        q,
        r#"{"x": "nope"}"#,
        "Non-matching event must not produce false positives"
    );
}

/// Clone must preserve the configured arena budget.
/// This is a regression test for C2 (clone previously used usize::MAX).
#[test]
fn test_clone_preserves_arena_budget() {
    let mut q = QuaminaBuilder::<String>::new()
        .with_arena_byte_budget(4096)
        .build()
        .unwrap();

    q.add_pattern("a".into(), r#"{"x": ["val"]}"#).unwrap();
    let mut cloned = q.clone();

    // The clone should enforce the same budget
    let mut rejected = false;
    for i in 0..500 {
        let pattern = format!(r#"{{"x": ["clone_test_value_{i}"]}}"#);
        if cloned.add_pattern("b".into(), &pattern).is_err() {
            rejected = true;
            break;
        }
    }
    assert!(
        rejected,
        "Cloned instance must enforce the original arena budget"
    );
}

/// Errors must return the PatternTooComplex variant specifically.
#[test]
fn test_errors_return_pattern_too_complex_variant() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_pattern_depth(1)
        .build()
        .unwrap();

    let result = q.add_pattern("deep", r#"{"a": {"b": ["val"]}}"#);
    assert!(
        matches!(result, Err(QuaminaError::PatternTooComplex(_))),
        "Depth violation must return PatternTooComplex, got {result:?}"
    );

    let mut q2 = QuaminaBuilder::<&str>::new()
        .with_max_fields_per_pattern(1)
        .build()
        .unwrap();
    let result = q2.add_pattern("wide", r#"{"a": ["1"], "b": ["2"]}"#);
    assert!(
        matches!(result, Err(QuaminaError::PatternTooComplex(_))),
        "Field count violation must return PatternTooComplex, got {result:?}"
    );
}

/// Zero limits must panic at build time, not silently reject all patterns.
#[test]
#[should_panic(expected = "max_pattern_depth must be at least 1")]
fn test_zero_depth_panics() {
    let _ = QuaminaBuilder::<&str>::new().with_max_pattern_depth(0);
}

#[test]
#[should_panic(expected = "max_fields_per_pattern must be at least 1")]
fn test_zero_fields_panics() {
    let _ = QuaminaBuilder::<&str>::new().with_max_fields_per_pattern(0);
}

#[test]
#[should_panic(expected = "arena_byte_budget must be at least 1")]
fn test_zero_budget_panics() {
    let _ = QuaminaBuilder::<&str>::new().with_arena_byte_budget(0);
}

#[test]
#[should_panic(expected = "max_states_per_pattern must be at least 1")]
fn test_zero_states_panics() {
    let _ = QuaminaBuilder::<&str>::new().with_max_states_per_pattern(0);
}

// --- State Count Limit Tests ---

#[test]
fn test_state_limit_exceeded() {
    // With a tiny state limit of 2, a pattern with 2 mixed-type fields
    // each having 2 matchers would produce 4 states (2^2), exceeding the limit.
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_states_per_pattern(2)
        .build()
        .unwrap();

    // Single field with mixed matchers: exact + prefix → 2 states (within limit)
    let r1 = q.add_pattern("ok", r#"{"a": ["x", {"prefix": "y"}]}"#);
    assert!(r1.is_ok(), "2 states should be within limit of 2");

    // Two fields with mixed matchers: 2 * 2 = 4 states (exceeds limit)
    let r2 = q.add_pattern(
        "bad",
        r#"{"a": ["x", {"prefix": "y"}], "b": ["m", {"prefix": "n"}]}"#,
    );
    assert!(r2.is_err(), "4 states should exceed limit of 2");
    assert!(
        r2.unwrap_err()
            .to_string()
            .contains("field-matcher state count"),
        "error should mention state count"
    );
}

#[test]
fn test_state_limit_default_allows_normal_patterns() {
    // Default limit (1024) should easily handle normal mixed-type patterns
    let mut q = Quamina::new();

    // Mixed exact + prefix on one field
    assert_add_ok!(q, "p1", r#"{"status": ["active", {"prefix": "pend"}]}"#);

    // Multiple fields with single matchers (no multiplication)
    assert_add_ok!(q, "p2", r#"{"a": ["1"], "b": ["2"], "c": ["3"]}"#);

    // Verify matching still works
    let pattern_ids = q.matches_for_event(br#"{"status": "active"}"#).unwrap();
    assert!(pattern_ids.contains(&"p1"));
    let pattern_ids = q.matches_for_event(br#"{"status": "pending"}"#).unwrap();
    assert!(pattern_ids.contains(&"p1"));
}

// ============================================================================
// Flatten-Only Tests
// ============================================================================

#[test]
fn test_flatten_only_returns_field_count() {
    // Register two tracked fields via patterns
    let q = q!("p1" => r#"{"x": [1], "y": [2]}"#);

    // Event with both tracked fields → should return 2
    let count = q.flatten_only(br#"{"x": 1, "y": 2}"#).unwrap();
    assert_eq!(count, 2, "two tracked fields should produce count 2");
}

#[test]
fn test_flatten_only_single_field() {
    let q = q!("p1" => r#"{"status": ["ok"]}"#);

    let count = q.flatten_only(br#"{"status": "ok"}"#).unwrap();
    assert_eq!(count, 1, "single tracked field should produce count 1");
}

#[test]
fn test_flatten_only_untracked_fields_ignored() {
    // Only "x" is tracked via the pattern
    let q = q!("p1" => r#"{"x": [1]}"#);

    // Event has x (tracked) and y (untracked)
    let count = q.flatten_only(br#"{"x": 1, "y": 2}"#).unwrap();
    assert_eq!(count, 1, "untracked field y should not be counted");
}

// ============================================================================
// PrunerStats tests
// ============================================================================

/// Verify that add_emitted, add_filtered, emitted, filtered, and reset all
/// behave correctly — initial values are zero, counters accumulate, and
/// reset zeroes both.
#[test]
fn test_pruner_stats_unit_add_and_read() {
    use super::PrunerStats;

    let stats = PrunerStats::new();

    // Initial state: both zero
    assert_eq!(stats.emitted(), 0, "initial emitted must be 0");
    assert_eq!(stats.filtered(), 0, "initial filtered must be 0");

    // add_emitted must actually increment
    stats.add_emitted(5);
    assert_eq!(stats.emitted(), 5, "emitted must be 5 after add_emitted(5)");
    assert_eq!(stats.filtered(), 0, "filtered must still be 0");

    // add_filtered must actually increment
    stats.add_filtered(3);
    assert_eq!(
        stats.filtered(),
        3,
        "filtered must be 3 after add_filtered(3)"
    );
    assert_eq!(stats.emitted(), 5, "emitted must still be 5");

    // Accumulation works
    stats.add_emitted(2);
    assert_eq!(stats.emitted(), 7, "emitted must accumulate to 7");

    stats.add_filtered(4);
    assert_eq!(stats.filtered(), 7, "filtered must accumulate to 7");

    // reset must zero both counters
    stats.reset();
    assert_eq!(stats.emitted(), 0, "emitted must be 0 after reset");
    assert_eq!(stats.filtered(), 0, "filtered must be 0 after reset");
}

/// Exhaustive boundary tests for should_rebuild: covers the 1000-activity
/// threshold (below, at, above), the 0.2 ratio boundary (below, at, above),
/// the division-by-zero guard (emitted == 0), and edge cases where arithmetic
/// operator changes (+→-, +→*) would produce different results.
#[test]
fn test_should_rebuild_boundary_cases() {
    use super::PrunerStats;

    // No activity → should NOT rebuild
    let s = PrunerStats::new();
    assert!(!s.should_rebuild(), "no activity: must not rebuild");

    // Below 1000 threshold with high ratio → should NOT rebuild
    // total = 999 < 1000
    let s = PrunerStats::new();
    s.add_emitted(500);
    s.add_filtered(499);
    assert!(
        !s.should_rebuild(),
        "total=999 below threshold: must not rebuild"
    );

    // Exactly at 1000 threshold with high ratio → SHOULD rebuild
    // total = 1000, ratio = 400/600 = 0.67 > 0.2
    let s = PrunerStats::new();
    s.add_emitted(600);
    s.add_filtered(400);
    assert!(s.should_rebuild(), "total=1000, ratio=0.67: must rebuild");

    // Above threshold but low ratio → should NOT rebuild
    // total = 1000, ratio = 100/900 = 0.111 < 0.2
    let s = PrunerStats::new();
    s.add_emitted(900);
    s.add_filtered(100);
    assert!(
        !s.should_rebuild(),
        "ratio=0.111 below 0.2: must not rebuild"
    );

    // Ratio exactly 0.2 → should NOT rebuild (strict >)
    // total = 1200, ratio = 200/1000 = 0.2 exactly
    let s = PrunerStats::new();
    s.add_emitted(1000);
    s.add_filtered(200);
    assert!(
        !s.should_rebuild(),
        "ratio=0.2 exactly: strict > means no rebuild"
    );

    // Ratio just above 0.2 → SHOULD rebuild
    // total = 1201, ratio = 201/1000 = 0.201 > 0.2
    let s = PrunerStats::new();
    s.add_emitted(1000);
    s.add_filtered(201);
    assert!(s.should_rebuild(), "ratio=0.201 above 0.2: must rebuild");

    // emitted == 0 → should NOT rebuild (division by zero guard)
    // total = 1500 but emitted == 0
    let s = PrunerStats::new();
    s.add_filtered(1500);
    assert!(
        !s.should_rebuild(),
        "emitted=0: must not rebuild (div-by-zero guard)"
    );

    // Both operands contribute to threshold: 600+500=1100 >= 1000
    // ratio = 500/600 = 0.83 > 0.2
    let s = PrunerStats::new();
    s.add_emitted(600);
    s.add_filtered(500);
    assert!(s.should_rebuild(), "total=1100, ratio=0.83: must rebuild");

    // Asymmetric operands: 1+999=1000 >= 1000
    // ratio = 999/1 = 999.0 > 0.2
    let s = PrunerStats::new();
    s.add_emitted(1);
    s.add_filtered(999);
    assert!(s.should_rebuild(), "total=1000, emitted=1: must rebuild");
}

// ============================================================================
// QuaminaBuilder tests
// ============================================================================

/// After with_media_type, with_flattener must fail due to conflict.
/// This verifies with_media_type actually sets media_type_validated.
#[test]
fn test_builder_with_media_type_sets_validated_flag() {
    let flattener = MockFlattener::new(vec![]);

    // After with_media_type, with_flattener should fail due to conflict
    let result = QuaminaBuilder::<String>::new()
        .with_media_type("application/json")
        .unwrap()
        .with_flattener(Box::new(flattener));

    assert!(
        result.is_err(),
        "with_flattener must fail after with_media_type"
    );
}

/// Verify that a custom flattener set via with_flattener is actually used
/// at match time. If it were lost (e.g., build ignores it), the default
/// JSON flattener would reject "not json" as invalid.
#[test]
fn test_builder_with_flattener_is_used() {
    let flattener = MockFlattener::new(vec![OwnedField {
        path: b"k".to_vec(),
        val: b"\"v\"".to_vec(),
        array_trail: vec![],
        is_number: false,
    }]);

    let mut q = QuaminaBuilder::<String>::new()
        .with_flattener(Box::new(flattener))
        .unwrap()
        .build()
        .unwrap();

    q.add_pattern("p".to_string(), r#"{"k": ["v"]}"#).unwrap();

    // Custom flattener makes this work even though "not json" is invalid JSON
    let pattern_ids = q.matches_for_event(b"not json").unwrap();
    assert_eq!(
        pattern_ids,
        vec!["p".to_string()],
        "custom flattener must be used"
    );
}

/// A second with_flattener call must be rejected — the first call should
/// mark that a custom flattener is already set.
#[test]
fn test_builder_with_flattener_blocks_second_call() {
    let f1 = MockFlattener::new(vec![]);
    let f2 = MockFlattener::new(vec![]);

    let result = QuaminaBuilder::<String>::new()
        .with_flattener(Box::new(f1))
        .unwrap()
        .with_flattener(Box::new(f2));

    assert!(result.is_err(), "second with_flattener must fail");
}

/// Verify with_max_pattern_depth is applied: default is 256, setting to 2
/// should reject depth-3 patterns but accept depth-2.
#[test]
fn test_builder_with_max_pattern_depth_is_applied() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_pattern_depth(2)
        .build()
        .unwrap();

    // Depth 3: {"a": {"b": {"c": ["v"]}}} — should fail with limit=2
    let result = q.add_pattern("p", r#"{"a": {"b": {"c": ["v"]}}}"#);
    assert!(
        result.is_err(),
        "depth-3 pattern must be rejected with max_depth=2"
    );

    // Depth 2: {"a": {"b": ["v"]}} — should succeed with limit=2
    let result = q.add_pattern("p", r#"{"a": {"b": ["v"]}}"#);
    assert!(
        result.is_ok(),
        "depth-2 pattern must succeed with max_depth=2"
    );
}

/// Verify with_max_fields_per_pattern is applied: default is 256, setting
/// to 2 should reject 3-field patterns but accept 2-field.
#[test]
fn test_builder_with_max_fields_per_pattern_is_applied() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_fields_per_pattern(2)
        .build()
        .unwrap();

    // 3 fields: should fail with limit=2
    let result = q.add_pattern("p", r#"{"a": ["1"], "b": ["2"], "c": ["3"]}"#);
    assert!(
        result.is_err(),
        "3-field pattern must be rejected with max_fields=2"
    );

    // 2 fields: should succeed
    let result = q.add_pattern("p", r#"{"a": ["1"], "b": ["2"]}"#);
    assert!(
        result.is_ok(),
        "2-field pattern must succeed with max_fields=2"
    );
}

/// Verify with_arena_byte_budget is applied: default is 10MB, setting to 1
/// byte means patterns should exceed the budget.
#[test]
fn test_builder_with_arena_byte_budget_is_applied() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(1)
        .build()
        .unwrap();

    // With a 1-byte budget, the first pattern may or may not fit depending on
    // implementation details, but by the second pattern the budget is exceeded.
    let r1 = q.add_pattern("p1", r#"{"x": ["a"]}"#);
    let r2 = q.add_pattern("p2", r#"{"x": [{"prefix": "b"}]}"#);
    assert!(
        r1.is_err() || r2.is_err(),
        "at least one pattern must be rejected with 1-byte arena budget"
    );
}

/// Verify with_max_states_per_pattern is applied: default is 1024, setting
/// to 1 means mixed-type fields should exceed the limit.
#[test]
fn test_builder_with_max_states_per_pattern_is_applied() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_max_states_per_pattern(1)
        .build()
        .unwrap();

    // Mixed exact + prefix on one field → 2 states → exceeds limit of 1
    let result = q.add_pattern("p", r#"{"a": ["x", {"prefix": "y"}]}"#);
    assert!(
        result.is_err(),
        "mixed-type pattern must be rejected with max_states=1"
    );
}

/// Verify with_auto_rebuild is applied: default is true, setting to false
/// must be observable via auto_rebuild_enabled().
#[test]
fn test_builder_with_auto_rebuild_is_applied() {
    let q = QuaminaBuilder::<String>::new()
        .with_auto_rebuild(false)
        .build()
        .unwrap();

    assert!(!q.auto_rebuild_enabled(), "auto_rebuild must be false");
}

// ============================================================================
// Arena stats regression tests for complex multi-pattern workloads
// (Inspired by Go's TestTablePointerDedup — ecfe50f)
// ============================================================================

struct StatsWorkload {
    name: &'static str,
    patterns: &'static [&'static str], // shellstyle patterns
    regexps: &'static [&'static str],  // regexp patterns
    state_count: u32,
    // Total explicit closure entries; self-only states contribute 0 (sentinel).
    total_closure_entries: u32,
    // Maximum explicit closure size; self-only closures count as 0.
    max_closure_len: u16,
    pattern_ids: [usize; 3], // expected match counts for 3 events
}

const STATS_WORKLOADS: &[StatsWorkload] = &[
    StatsWorkload {
        name: "6-regexps-12-shell",
        patterns: &[
            "*a*b*c*", "*x*y*z*", "*e*f*g*", "*m*n*o*", "*p*q*r*", "*s*t*u*", "*a*e*i*", "*b*d*f*",
            "*c*g*k*", "*d*h*l*", "*i*o*u*", "*r*s*t*",
        ],
        regexps: &[
            "(([abc]?)*)+",
            "([abc]+)*d",
            "(a*)*b",
            "([xyz]?)*end",
            "(([mno]?)*)+",
            "([pqr]+)*s",
        ],
        state_count: 153,
        total_closure_entries: 235,
        max_closure_len: 33,
        pattern_ids: [3, 2, 7],
    },
    StatsWorkload {
        name: "20-nested-regexps",
        patterns: &[],
        regexps: &[
            "(([abc]?)*)+",
            "([abc]+)*d",
            "(a*)*b",
            "([xyz]?)*end",
            "(([mno]?)*)+",
            "([pqr]+)*s",
            "(([def]?)*)+",
            "([ghi]+)*j",
            "(([stu]?)*)+",
            "([vwx]+)*y",
            "(b*)*c",
            "(d*)*e",
            "(([fg]?)*)+",
            "([hi]+)*k",
            "(([jk]?)*)+",
            "([lm]+)*n",
            "(([op]?)*)+",
            "([qr]+)*t",
            "(e*)*f",
            "(g*)*h",
        ],
        state_count: 143,
        total_closure_entries: 313,
        max_closure_len: 68,
        pattern_ids: [0, 0, 0],
    },
    StatsWorkload {
        name: "deeply-nested",
        patterns: &[],
        regexps: &[
            "(((a?)*b?)*c?)*",
            "(((x?)*y?)*z?)*",
            "(((d?)*e?)*f?)*",
            "(((m?)*n?)*o?)*",
            "((((a?)*b?)*c?)*d?)*",
            "((((x?)*y?)*z?)*w?)*",
        ],
        state_count: 74,
        total_closure_entries: 613,
        max_closure_len: 61,
        pattern_ids: [0, 0, 0],
    },
    StatsWorkload {
        name: "overlapping-char-classes",
        patterns: &[],
        regexps: &[
            "(([abc]?)*)+",
            "(([bcd]?)*)+",
            "(([cde]?)*)+",
            "(([def]?)*)+",
            "(([efg]?)*)+",
            "(([fgh]?)*)+",
            "(([ghi]?)*)+",
            "(([hij]?)*)+",
            "(([ijk]?)*)+",
            "(([jkl]?)*)+",
            "(([klm]?)*)+",
            "(([lmn]?)*)+",
        ],
        state_count: 86,
        total_closure_entries: 301,
        max_closure_len: 61,
        pattern_ids: [0, 0, 0],
    },
    StatsWorkload {
        name: "shell+deep-overlap",
        patterns: &[
            "*a*b*", "*b*c*", "*c*d*", "*d*e*", "*e*f*", "*a*c*", "*b*d*", "*c*e*", "*d*f*",
            "*a*d*",
        ],
        regexps: &[
            "(((a?)*b?)*c?)*",
            "(((b?)*c?)*d?)*",
            "(((c?)*d?)*e?)*",
            "(((d?)*e?)*f?)*",
            "(([abcd]?)*)+",
            "(([cdef]?)*)+",
        ],
        state_count: 130,
        total_closure_entries: 465,
        max_closure_len: 57,
        pattern_ids: [10, 10, 10],
    },
];

fn stats_events() -> Vec<Vec<u8>> {
    vec![
        br#"{"val": "abcdefgh"}"#.to_vec(),
        format!(r#"{{"val": "{}"}}"#, "abcdef".repeat(5)).into_bytes(),
        format!(r#"{{"val": "{}"}}"#, "abcdefghijklmnop".repeat(3)).into_bytes(),
    ]
}

fn build_stats_matcher(wl: &StatsWorkload) -> Quamina<String> {
    let mut q = Quamina::new();
    let mut i = 0;
    for ss in wl.patterns {
        let pattern = format!(r#"{{"val": [{{"shellstyle": "{ss}"}}]}}"#);
        q.add_pattern(format!("s{i}"), &pattern).unwrap();
        i += 1;
    }
    for re in wl.regexps {
        let pattern = format!(r#"{{"val": [{{"regexp": "{re}"}}]}}"#);
        q.add_pattern(format!("r{i}"), &pattern).unwrap();
        i += 1;
    }
    q
}

/// Verify arena stats and match correctness for complex multi-pattern workloads.
/// Exact stats assertions catch regressions in NFA construction, merging, and
/// epsilon closure computation. Match counts verify end-to-end correctness.
/// Stats reflect the default `BuiltForComfort` mode, so every workload freezes
/// to an NFA (non-zero closures); `BuiltForSpeed` would convert to DFAs and
/// change these figures.
/// (Inspired by Go's TestTablePointerDedup — ecfe50f)
#[test]
#[cfg_attr(miri, ignore)]
fn test_arena_stats_workloads() {
    let events = stats_events();
    for wl in STATS_WORKLOADS {
        let q = build_stats_matcher(wl);
        let stats = q.arena_stats();

        assert_eq!(
            stats.state_count, wl.state_count,
            "{}: state_count = {}, want {}",
            wl.name, stats.state_count, wl.state_count,
        );
        assert_eq!(
            stats.total_closure_entries, wl.total_closure_entries,
            "{}: total_closure_entries = {}, want {}",
            wl.name, stats.total_closure_entries, wl.total_closure_entries,
        );
        assert_eq!(
            stats.max_closure_len, wl.max_closure_len,
            "{}: max_closure_len = {}, want {}",
            wl.name, stats.max_closure_len, wl.max_closure_len,
        );

        for (ei, event) in events.iter().enumerate() {
            let pattern_ids = q.matches_for_event(event).unwrap();
            assert_eq!(
                pattern_ids.len(),
                wl.pattern_ids[ei],
                "{}: event[{ei}] expected {} pattern_ids, got {}",
                wl.name,
                wl.pattern_ids[ei],
                pattern_ids.len()
            );
        }
    }
}

/// The build mode is a performance knob only: `BuiltForComfort` (NFA) and
/// `BuiltForSpeed` (DFA) must return identical matches for the same patterns
/// and events. Mirrors Go upstream's TestBuildModeCalls, which checks that both
/// modes work end-to-end. Runs under Miri too — there `BuiltForSpeed` also
/// stays an NFA, so the two runs are trivially equal.
#[test]
fn test_matcher_build_mode_matching_parity() {
    let pattern = r#"{"x": [{"regexp": "a.*z.j*"}]}"#;
    let events: &[&[u8]] = &[
        br#"{"x": "abczxj"}"#,
        br#"{"x": "azq"}"#,
        br#"{"x": "nope"}"#,
        br#"{"x": "azzzj"}"#,
    ];

    let match_counts = |mode: MatcherBuildMode| {
        let mut q: Quamina<String> = Quamina::new();
        q.set_matcher_build_mode(mode);
        assert_eq!(q.matcher_build_mode(), mode);
        q.add_pattern("p".to_string(), pattern).unwrap();
        events
            .iter()
            .map(|ev| q.matches_for_event(ev).unwrap().len())
            .collect::<Vec<_>>()
    };

    let comfort = match_counts(MatcherBuildMode::BuiltForComfort);
    let speed = match_counts(MatcherBuildMode::BuiltForSpeed);
    assert_eq!(
        comfort, speed,
        "BuiltForComfort and BuiltForSpeed must match identically",
    );
    assert!(
        comfort.iter().any(|&n| n > 0),
        "test pattern should match at least one event",
    );
}

// ============================================================================
// Mutation coverage: LookaroundCondition methods & byte length computation
// ============================================================================

// `pos_la`/`neg_la`/`pos_lb`/`neg_lb` are content-named test fixtures.
#[allow(clippy::similar_names)]
#[test]
fn test_lookaround_condition_is_negative_true() {
    use crate::json::LookaroundCondition;
    use crate::regexp::parse_regexp;

    let pat = parse_regexp("x").unwrap();

    // NegativeLookahead must be negative
    let neg_la = LookaroundCondition::NegativeLookahead(pat.clone());
    assert!(neg_la.is_negative());

    // NegativeLookbehind must be negative
    let neg_lb = LookaroundCondition::NegativeLookbehind {
        pattern: pat.clone(),
        byte_length: 1,
    };
    assert!(neg_lb.is_negative());

    // Positive variants must NOT be negative
    let pos_la = LookaroundCondition::PositiveLookahead(pat.clone());
    assert!(!pos_la.is_negative());

    let pos_lb = LookaroundCondition::PositiveLookbehind {
        pattern: pat,
        byte_length: 1,
    };
    assert!(!pos_lb.is_negative());
}

// `pos_la`/`neg_la`/`pos_lb`/`neg_lb` are content-named test fixtures.
#[allow(clippy::similar_names)]
#[test]
fn test_lookaround_condition_is_lookbehind_true() {
    use crate::json::LookaroundCondition;
    use crate::regexp::parse_regexp;

    let pat = parse_regexp("x").unwrap();

    // Lookbehind variants must return true
    let pos_lb = LookaroundCondition::PositiveLookbehind {
        pattern: pat.clone(),
        byte_length: 1,
    };
    assert!(pos_lb.is_lookbehind());

    let neg_lb = LookaroundCondition::NegativeLookbehind {
        pattern: pat.clone(),
        byte_length: 1,
    };
    assert!(neg_lb.is_lookbehind());

    // Lookahead variants must return false
    let pos_la = LookaroundCondition::PositiveLookahead(pat.clone());
    assert!(!pos_la.is_lookbehind());

    let neg_la = LookaroundCondition::NegativeLookahead(pat);
    assert!(!neg_la.is_lookbehind());
}

#[test]
fn test_transform_lookaround_lookbehind_byte_length() {
    // Tests compute_lookbehind_byte_length and compute_branch_byte_length
    // via transform_lookaround_pattern with lookbehind patterns.
    use crate::json::LookaroundCondition;
    use crate::json::transform_lookaround_pattern;
    use crate::regexp::parse_regexp;

    // (?<=abc)d — lookbehind "abc" has 3 ASCII chars = 3 bytes
    let tree = parse_regexp("(?<=abc)d").unwrap();
    let mc = transform_lookaround_pattern(&tree).unwrap();
    assert_eq!(mc.conditions.len(), 1);
    match &mc.conditions[0] {
        LookaroundCondition::PositiveLookbehind { byte_length, .. } => {
            assert_eq!(*byte_length, 3, "abc = 3 bytes");
        }
        other => panic!("Expected PositiveLookbehind, got {other:?}"),
    }

    // (?<!xy)z — lookbehind "xy" has 2 bytes
    let tree = parse_regexp("(?<!xy)z").unwrap();
    let mc = transform_lookaround_pattern(&tree).unwrap();
    assert_eq!(mc.conditions.len(), 1);
    match &mc.conditions[0] {
        LookaroundCondition::NegativeLookbehind { byte_length, .. } => {
            assert_eq!(*byte_length, 2, "xy = 2 bytes");
        }
        other => panic!("Expected NegativeLookbehind, got {other:?}"),
    }
}

#[test]
fn test_transform_lookbehind_single_char_class() {
    // Tests compute_branch_byte_length with rune ranges (the !atom.runes.is_empty() path)
    // and the atom_len * count multiplication.
    use crate::json::LookaroundCondition;
    use crate::json::transform_lookaround_pattern;
    use crate::regexp::parse_regexp;

    // (?<=[a-z])X — single char class, each char is 1 byte, count=1 → 1 byte
    let tree = parse_regexp("(?<=[a-z])X").unwrap();
    let mc = transform_lookaround_pattern(&tree).unwrap();
    match &mc.conditions[0] {
        LookaroundCondition::PositiveLookbehind { byte_length, .. } => {
            assert_eq!(*byte_length, 1, "[a-z] = 1 byte per char");
        }
        other => panic!("Expected PositiveLookbehind, got {other:?}"),
    }

    // (?<=[a-z]{3})X — char class repeated 3 times → 3 bytes
    // This exercises atom_len * count where count > 1
    let tree = parse_regexp("(?<=[a-z]{3})X").unwrap();
    let mc = transform_lookaround_pattern(&tree).unwrap();
    match &mc.conditions[0] {
        LookaroundCondition::PositiveLookbehind { byte_length, .. } => {
            assert_eq!(*byte_length, 3, "[a-z]{{3}} = 3 bytes");
        }
        other => panic!("Expected PositiveLookbehind, got {other:?}"),
    }
}

#[test]
fn test_transform_lookbehind_dot_byte_length() {
    // Tests the is_dot branch: dot → 4 bytes (conservative UTF-8 max)
    use crate::json::LookaroundCondition;
    use crate::json::transform_lookaround_pattern;
    use crate::regexp::parse_regexp;

    // (?<=.)X — dot = 4 bytes (worst-case UTF-8)
    let tree = parse_regexp("(?<=.)X").unwrap();
    let mc = transform_lookaround_pattern(&tree).unwrap();
    match &mc.conditions[0] {
        LookaroundCondition::PositiveLookbehind { byte_length, .. } => {
            assert_eq!(*byte_length, 4, ". = 4 bytes (UTF-8 max)");
        }
        other => panic!("Expected PositiveLookbehind, got {other:?}"),
    }
}

#[test]
fn test_transform_lookaround_no_lookarounds_error() {
    // Exercising the error path of transform_lookaround_pattern
    use crate::json::transform_lookaround_pattern;
    use crate::regexp::parse_regexp;

    let tree = parse_regexp("abc").unwrap();
    let err = transform_lookaround_pattern(&tree).unwrap_err();
    assert!(err.contains("no lookarounds"), "Got: {err}");
}

#[test]
fn test_transform_lookbehind_alternation_same_length() {
    // Tests compute_lookbehind_byte_length with multi-branch (alternation) lookbehind.
    // (?<=ab|cd)x — two branches, both 2 bytes → Ok(2)
    // Catches line 436 (!= → ==): mutant would Err on equal-length branches.
    use crate::json::LookaroundCondition;
    use crate::json::transform_lookaround_pattern;
    use crate::regexp::parse_regexp;

    let tree = parse_regexp("(?<=ab|cd)x").unwrap();
    let mc = transform_lookaround_pattern(&tree).unwrap();
    assert_eq!(mc.conditions.len(), 1);
    match &mc.conditions[0] {
        LookaroundCondition::PositiveLookbehind { byte_length, .. } => {
            assert_eq!(*byte_length, 2, "ab|cd both = 2 bytes");
        }
        other => panic!("Expected PositiveLookbehind, got {other:?}"),
    }
}

#[test]
fn test_transform_lookaround_alternation_rejected() {
    // tree.len() != 1 path — top-level alternation
    use crate::json::transform_lookaround_pattern;
    use crate::regexp::parse_regexp;

    let tree = parse_regexp("a(?=b)|c(?=d)").unwrap();
    let err = transform_lookaround_pattern(&tree).unwrap_err();
    assert!(err.contains("alternation"), "Got: {err}");
}

// ============================================================================
// parse_numeric_comparison / value_to_string / validate_wildcard mutation tests
// ============================================================================

#[test]
fn test_numeric_comparison_missing_value_after_operator() {
    // Odd-length numeric arrays — operator without a paired value — must error.
    let mut q = Quamina::<&str>::new();
    let result = q.add_pattern("bad", r#"{"x": [{"numeric": [">"]}]}"#);
    assert!(result.is_err(), "Single-element numeric array should fail");
}

#[test]
fn test_numeric_comparison_all_operators() {
    // Test each comparison operator individually
    let mut q = Quamina::<&str>::new();
    q.add_pattern("gt", r#"{"x": [{"numeric": [">", 10]}]}"#)
        .unwrap();
    q.add_pattern("gte", r#"{"x": [{"numeric": [">=", 10]}]}"#)
        .unwrap();
    q.add_pattern("lt", r#"{"x": [{"numeric": ["<", 10]}]}"#)
        .unwrap();
    q.add_pattern("lte", r#"{"x": [{"numeric": ["<=", 10]}]}"#)
        .unwrap();
    q.add_pattern("eq", r#"{"x": [{"numeric": ["=", 10]}]}"#)
        .unwrap();

    // x=10 should match >=, <=, =
    let m = q.matches_for_event(br#"{"x": 10}"#).unwrap();
    assert!(m.contains(&"gte"), "10 should match >= 10");
    assert!(m.contains(&"lte"), "10 should match <= 10");
    assert!(m.contains(&"eq"), "10 should match = 10");
    assert!(!m.contains(&"gt"), "10 should NOT match > 10");
    assert!(!m.contains(&"lt"), "10 should NOT match < 10");

    // x=11 should match >, >=
    let m = q.matches_for_event(br#"{"x": 11}"#).unwrap();
    assert!(m.contains(&"gt"), "11 should match > 10");
    assert!(m.contains(&"gte"), "11 should match >= 10");
    assert!(!m.contains(&"lt"), "11 should NOT match < 10");
    assert!(!m.contains(&"lte"), "11 should NOT match <= 10");
    assert!(!m.contains(&"eq"), "11 should NOT match = 10");

    // x=9 should match <, <=
    let m = q.matches_for_event(br#"{"x": 9}"#).unwrap();
    assert!(m.contains(&"lt"), "9 should match < 10");
    assert!(m.contains(&"lte"), "9 should match <= 10");
    assert!(!m.contains(&"gt"), "9 should NOT match > 10");
    assert!(!m.contains(&"gte"), "9 should NOT match >= 10");
}

#[test]
fn test_numeric_comparison_range() {
    // Combined range: > 5 AND <= 100
    let mut q = Quamina::<&str>::new();
    q.add_pattern("range", r#"{"x": [{"numeric": [">", 5, "<=", 100]}]}"#)
        .unwrap();

    assert_matches!(q, r#"{"x": 50}"#, vec!["range"]);
    assert_matches!(q, r#"{"x": 100}"#, vec!["range"]);
    assert_matches!(q, r#"{"x": 6}"#, vec!["range"]);
    assert_no_match!(q, r#"{"x": 5}"#); // not strictly > 5
    assert_no_match!(q, r#"{"x": 101}"#); // exceeds upper bound
    assert_no_match!(q, r#"{"x": 4}"#); // below lower bound
}

/// An exclusive upper bound (`<`) must reject the boundary value itself.
/// `[">=", 1, "<", 100]` — one inclusive, one exclusive bound — exercises the
/// `&&` check in `make_range_arena_fa_step`; value 100 must not match.
#[test]
fn test_numeric_comparison_exclusive_upper_bound_boundary() {
    let mut q = Quamina::<&str>::new();
    q.add_pattern("r", r#"{"x": [{"numeric": [">=", 1, "<", 100]}]}"#)
        .unwrap();
    assert_matches!(q, r#"{"x": 99}"#, vec!["r"]);
    assert_matches!(q, r#"{"x": 1}"#, vec!["r"]);
    assert_no_match!(
        q,
        r#"{"x": 100}"#,
        "exclusive upper bound must reject value == upper"
    );
}

/// An exclusive lower bound (`>`) must reject the boundary value itself.
/// Mirror of `test_numeric_comparison_exclusive_upper_bound_boundary` with
/// the inclusive and exclusive sides swapped.
#[test]
fn test_numeric_comparison_exclusive_lower_bound_boundary() {
    let mut q = Quamina::<&str>::new();
    q.add_pattern("r", r#"{"x": [{"numeric": [">", 1, "<=", 100]}]}"#)
        .unwrap();
    assert_matches!(q, r#"{"x": 2}"#, vec!["r"]);
    assert_matches!(q, r#"{"x": 100}"#, vec!["r"]);
    assert_no_match!(
        q,
        r#"{"x": 1}"#,
        "exclusive lower bound must reject value == lower"
    );
}

/// Both bounds exclusive (`>`, `<`): both boundary values must be rejected.
#[test]
fn test_numeric_comparison_both_bounds_exclusive() {
    let mut q = Quamina::<&str>::new();
    q.add_pattern("r", r#"{"x": [{"numeric": [">", 1, "<", 100]}]}"#)
        .unwrap();
    assert_matches!(q, r#"{"x": 2}"#, vec!["r"]);
    assert_matches!(q, r#"{"x": 99}"#, vec!["r"]);
    assert_no_match!(
        q,
        r#"{"x": 1}"#,
        "exclusive lower must reject value == lower"
    );
    assert_no_match!(
        q,
        r#"{"x": 100}"#,
        "exclusive upper must reject value == upper"
    );
}

#[test]
fn test_numeric_comparison_invalid_patterns() {
    let mut q = Quamina::<&str>::new();

    // Unknown operator
    assert_add_err!(q, "bad", r#"{"x": [{"numeric": ["!=", 5]}]}"#);

    // Non-number value after operator
    assert_add_err!(q, "bad", r#"{"x": [{"numeric": [">", "five"]}]}"#);

    // Non-string in operator position (number where operator expected)
    assert_add_err!(q, "bad", r#"{"x": [{"numeric": [5, 10]}]}"#);
}

#[test]
fn test_value_to_string_types() {
    // Test that different JSON value types in patterns are correctly converted.
    // String values are quote-wrapped, others are not.
    let q = q!(
        "str" => r#"{"x": ["hello"]}"#,
        "num" => r#"{"x": [42]}"#,
        "bool_t" => r#"{"x": [true]}"#,
        "bool_f" => r#"{"x": [false]}"#,
        "null_v" => r#"{"x": [null]}"#
    );

    assert_matches!(q, r#"{"x": "hello"}"#, vec!["str"]);
    assert_matches!(q, r#"{"x": 42}"#, vec!["num"]);
    assert_matches!(q, r#"{"x": true}"#, vec!["bool_t"]);
    assert_matches!(q, r#"{"x": false}"#, vec!["bool_f"]);
    assert_matches!(q, r#"{"x": null}"#, vec!["null_v"]);

    // Cross-type: string "true" should NOT match boolean true
    assert_no_match!(q, r#"{"x": "true"}"#);
    // Cross-type: string "null" should NOT match null
    assert_no_match!(q, r#"{"x": "null"}"#);
    // Cross-type: string "42" should NOT match number 42
    assert_no_match!(q, r#"{"x": "42"}"#);
}

#[test]
fn test_validate_wildcard_escapes() {
    let mut q = Quamina::<&str>::new();

    // Valid escaped star
    assert_add_ok!(q, "esc", r#"{"x": [{"wildcard": "a\\*b"}]}"#);

    // Trailing backslash — invalid
    assert_add_err!(q, "bad", r#"{"x": [{"wildcard": "a\\"}]}"#);

    // Invalid escape character (not * or \)
    assert_add_err!(q, "bad2", r#"{"x": [{"wildcard": "a\\nb"}]}"#);

    // Adjacent ** in wildcard — invalid
    assert_add_err!(q, "bad3", r#"{"x": [{"wildcard": "a**b"}]}"#);
}

// Mutation coverage: json.rs parse_value guard, parse_number scientific notation
#[test]
fn test_parse_value_rejects_invalid_value_start() {
    // `.5` is not valid JSON (numbers must start with digit or `-`).
    // Catches mutation: line 887 guard `c == '-' || c.is_ascii_digit()` → `true`
    // (with mutation, `.5` is parsed via parse_number as "0.5" and pattern succeeds)
    let mut q = crate::Quamina::new();
    assert_add_err!(q, "bad", r#"{"x": [{"numeric": [">", .5]}]}"#);
}

#[test]
fn test_numeric_pattern_with_scientific_notation() {
    // Catches mutations in parse_number scientific notation handling (lines 1060, 1062)
    let mut q = crate::Quamina::new();

    // Pattern using scientific notation: 1e2 = 100
    q.add_pattern("sci", r#"{"x": [{"numeric": [">=", 1e2, "<=", 1e2]}]}"#)
        .unwrap();
    assert_matches!(q, r#"{"x": 100}"#, vec!["sci"]);
    assert_no_match!(q, r#"{"x": 99}"#);

    // Pattern with explicit + sign in exponent: 1e+2 = 100
    q.add_pattern("plus", r#"{"x": [{"numeric": [">=", 1e+2, "<=", 1e+2]}]}"#)
        .unwrap();
    assert_matches!(q, r#"{"x": 100}"#, vec!["sci", "plus"]);

    // Pattern with - sign in exponent: 1e-1 = 0.1
    q.add_pattern("neg", r#"{"y": [{"numeric": [">=", 1e-1, "<=", 1e-1]}]}"#)
        .unwrap();
    assert_matches!(q, r#"{"y": 0.1}"#, vec!["neg"]);
}

// ============================================================================
// matcher_stats() — resource-consumption reporting — and the build-time
// arena byte budget that caps pattern complexity at construction.
// ============================================================================

/// Build a string of `n` 'i' bytes.
fn i_string(n: usize) -> String {
    "i".repeat(n)
}

/// A pattern whose arena would exceed the build-time budget is rejected, and
/// the rejection leaves the matcher unchanged: smaller patterns still build.
#[test]
fn test_arena_budget_rejects_oversized_pattern() {
    // A 1-byte budget can't fit any real arena, so a prefix matcher (which
    // always builds an arena) is rejected outright — the literal's length is
    // irrelevant once the arena exceeds a single byte.
    let mut q = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(1)
        .build()
        .unwrap();
    let big = i_string(16);
    let pat = format!(r#"{{"x": [{{"prefix": "{big}"}}]}}"#);
    let err = q.add_pattern("big", &pat).unwrap_err();
    assert!(matches!(err, QuaminaError::PatternTooComplex(_)));
}

/// The build-time budget gates pattern acceptance on the size of the arena
/// that would be built: a 1 KB budget rejects a prefix literal whose arena is
/// a few KB, while a generous budget accepts the same pattern.
#[test]
fn test_arena_budget_gates_on_arena_size() {
    let big = i_string(8);
    let big_pat = format!(r#"{{"x": [{{"prefix": "{big}"}}]}}"#);

    let mut tight = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(1024)
        .build()
        .unwrap();
    tight.add_pattern("seed", r#"{"x": ["x"]}"#).unwrap();
    assert!(
        tight.add_pattern("big", &big_pat).is_err(),
        "prefix pattern must be rejected under a 1 KB arena budget"
    );

    let mut generous = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(10_000_000)
        .build()
        .unwrap();
    generous.add_pattern("seed", r#"{"x": ["x"]}"#).unwrap();
    generous.add_pattern("big", &big_pat).unwrap();
}

/// A pattern that fails the budget check must not leave bookkeeping state
/// (segments tree, pattern_defs) populated, and the matcher must stay usable:
/// a subsequent in-budget pattern still builds and matches. Verifies the
/// add_pattern → automaton ordering invariant is preserved.
#[test]
fn test_arena_budget_failure_leaves_no_state() {
    let mut q = QuaminaBuilder::<&str>::new()
        .with_arena_byte_budget(1)
        .build()
        .unwrap();
    let before = q.pattern_count();
    assert!(
        q.add_pattern("rejected", r#"{"x": [{"prefix": "abc"}]}"#)
            .is_err()
    );
    assert_eq!(
        q.pattern_count(),
        before,
        "rejected pattern must not be recorded in pattern_defs"
    );

    // The failed add left no partial state, so a singleton-path exact pattern
    // (which never touches an arena) still builds and matches.
    q.add_pattern("ok", r#"{"x": ["v"]}"#).unwrap();
    assert_matches!(q, r#"{"x": "v"}"#, vec!["ok"]);
}

/// `matcher_stats().bytes` is zero before any patterns and grows once a
/// pattern is added. Adding a structurally identical pattern on a *different*
/// field builds a distinct arena (no dedup), so bytes roughly doubles.
#[test]
fn test_matcher_stats_cost_grows() {
    let mut q = QuaminaBuilder::<&str>::new().build().unwrap();
    assert_eq!(q.matcher_stats().bytes, 0);

    q.add_pattern("x", r#"{"x":[{"wildcard": "*z"}]}"#).unwrap();
    let one = q.matcher_stats();
    assert!(one.bytes > 0, "bytes must grow after adding a pattern");
    assert!(one.states > 0, "wildcard pattern must allocate states");
    assert!(one.fanouts > 0, "epsilon closures must contribute fanout");

    // A structurally identical wildcard on a different field gets its own
    // arena, so total bytes roughly doubles rather than deduplicating.
    q.add_pattern("y", r#"{"y":[{"wildcard": "*z"}]}"#).unwrap();
    let two = q.matcher_stats();
    assert!(
        two.bytes >= one.bytes * 2 - one.bytes / 4,
        "two distinct arenas should roughly double bytes: {} vs {}",
        two.bytes,
        one.bytes,
    );
    assert!(two.states >= one.states * 2 - 1);
}

/// Regression: reporting stats for a value matcher that took the singleton
/// optimization (a stored comparison buffer, no automaton) must not panic and
/// must still report non-zero bytes via the buffer's capacity.
///
/// That optimization fires for any field with a single string or literal
/// value: the matcher stores the literal bytes and compares them directly
/// instead of building an arena, so it has no automaton to walk. Minimal
/// repro: `{"Animated": [false]}`.
#[test]
fn test_matcher_stats_singleton() {
    let mut q = QuaminaBuilder::<&str>::new().build().unwrap();
    q.add_pattern("p", r#"{"Animated": [false]}"#).unwrap();

    let stats = q.matcher_stats();
    assert!(
        stats.bytes > 0,
        "singleton matcher must report its buffer capacity, got {}",
        stats.bytes,
    );
    // A single string literal takes the same path; it must also report bytes.
    let mut q = QuaminaBuilder::<&str>::new().build().unwrap();
    q.add_pattern("p", r#"{"x": ["hello"]}"#).unwrap();
    assert!(q.matcher_stats().bytes > 0);
}

/// A `*z` wildcard's multi-state epsilon closures yield small positive fanout
/// figures. Self-only closures contribute zero via the sentinel;
/// `max_fanout` is the largest stored closure and never exceeds `fanouts`.
#[test]
fn test_matcher_stats_nfa_shape() {
    let mut q = QuaminaBuilder::<&str>::new().build().unwrap();
    q.add_pattern("x", r#"{"x":[{"wildcard": "*z"}]}"#).unwrap();

    let stats = q.matcher_stats();
    assert!(stats.states > 0);
    assert!(
        stats.fanouts > 0,
        "wildcard closures must contribute fanout"
    );
    assert!(stats.max_fanout > 0);
    assert!(
        stats.max_fanout <= stats.fanouts,
        "max closure ({}) cannot exceed summed closures ({})",
        stats.max_fanout,
        stats.fanouts,
    );
}

/// The byte accounting handles shared sub-graphs without double-counting.
/// Adding the same field/value pattern twice with different ids reuses the
/// same value matcher, and the DAG walk deduplicates it, so bytes is stable.
#[test]
fn test_matcher_stats_dedups_shared_subgraphs() {
    let mut q = QuaminaBuilder::<&str>::new().build().unwrap();
    q.add_pattern("a", r#"{"x": ["v"]}"#).unwrap();
    let one = q.matcher_stats().bytes;

    // Same field/value with a different id reuses the same value matcher, so
    // the DAG walk must dedup it instead of double-counting its buffer.
    q.add_pattern("b", r#"{"x": ["v"]}"#).unwrap();
    let two = q.matcher_stats().bytes;
    assert_eq!(
        one, two,
        "identical patterns should not inflate accounted bytes"
    );
}

/// `matcher_stats()` measures the materialized (frozen) matcher, so it reflects
/// the build mode: `BuiltForComfort` keeps an NFA (non-zero epsilon-closure
/// fanouts) while `BuiltForSpeed` converts to a DFA (no epsilon closures, so
/// zero fanouts). Mirrors Go upstream's TestBuildModeCalls, which asserts the
/// two modes report different matcher sizes. Skipped under Miri, which never
/// runs the DFA conversion (so speed would also report an NFA).
#[cfg(not(miri))]
#[test]
fn test_matcher_build_mode_reflected_in_stats() {
    let pattern = r#"{"x": [{"regexp": "a.*z.j*"}]}"#;

    let stats_for = |mode: MatcherBuildMode| {
        let mut q: Quamina<String> = Quamina::new();
        q.set_matcher_build_mode(mode);
        q.add_pattern("p".to_string(), pattern).unwrap();
        q.matcher_stats()
    };

    let comfort = stats_for(MatcherBuildMode::BuiltForComfort);
    let speed = stats_for(MatcherBuildMode::BuiltForSpeed);

    assert!(
        comfort.fanouts > 0 && comfort.max_fanout > 0,
        "BuiltForComfort should keep an NFA with epsilon closures",
    );
    assert_eq!(
        (speed.fanouts, speed.max_fanout),
        (0, 0),
        "BuiltForSpeed should convert to a DFA with no epsilon closures",
    );
    assert_ne!(
        (comfort.states, comfort.bytes, comfort.fanouts),
        (speed.states, speed.bytes, speed.fanouts),
        "the two modes must report different matcher sizes",
    );
}

/// Four wildcard shapes — interior, prefix, suffix, and a second interior —
/// merged into one field's value matcher, so a mode applied to them covers every
/// automaton that matcher builds.
const BUILD_MODE_WILDCARDS: [(&str, &str); 4] = [
    ("p1", r#"{"x": [{"wildcard": "t*ortilla"}]}"#),
    ("p2", r#"{"x": [{"wildcard": "tortilla*"}]}"#),
    ("p3", r#"{"x": [{"wildcard": "*tortilla"}]}"#),
    ("p4", r#"{"x": [{"wildcard": "tortil*la"}]}"#),
];

/// Replaying the live patterns — into a clone, or into the matcher a rebuild
/// installs — has to carry the build mode along, or the replayed matcher freezes
/// back to comfort's NFAs and comes out a different size than the one the caller
/// built. Mirrors Go upstream's TestPrunerBuildMode, which pins the byte count
/// across a forced rebuild. Go stores a mode per pattern because it applies the
/// mode as each pattern is added; Quamina applies it at freeze, so whichever mode
/// is set when the matcher freezes covers all of it.
///
/// Skipped under Miri, which never runs the DFA conversion: both sides would be
/// NFAs and the comparison would hold for the wrong reason.
#[cfg(not(miri))]
#[test]
fn test_replay_preserves_build_mode() {
    let mut q = Quamina::new();
    q.set_matcher_build_mode(MatcherBuildMode::BuiltForSpeed);
    for (id, pattern) in BUILD_MODE_WILDCARDS {
        q.add_pattern(id, pattern).unwrap();
    }
    let built = q.matcher_stats();
    assert_eq!(
        built.fanouts, 0,
        "BuiltForSpeed should have converted the merged wildcards to a DFA",
    );

    let snapshot = q.clone();
    assert_eq!(
        snapshot.matcher_build_mode(),
        MatcherBuildMode::BuiltForSpeed,
        "a clone must inherit the mode it was replayed under",
    );
    assert_eq!(
        snapshot.matcher_stats(),
        built,
        "a clone must be the same matcher, not a comfort-mode re-merge",
    );

    // A pattern on a field of its own, added and dropped, is what leaves the
    // automaton holding states no live pattern needs — the condition a rebuild
    // reclaims. All four wildcards stay live across it, so the rebuild has real
    // work to do and still has to land back on the matcher they built.
    q.add_pattern("throwaway", r#"{"y": [{"wildcard": "*gone*"}]}"#)
        .unwrap();
    q.delete_patterns(&"throwaway").unwrap();
    assert_eq!(q.rebuild(), 1);
    assert_eq!(
        q.matcher_build_mode(),
        MatcherBuildMode::BuiltForSpeed,
        "a rebuild must keep the mode the caller set",
    );
    assert_eq!(
        q.matcher_stats(),
        built,
        "a rebuild must give back the matcher the live patterns built",
    );
}

/// The mode is read when the matcher freezes, so setting it on a matcher that
/// already holds patterns re-freezes them under the new mode. Setting it back has
/// to land exactly where it started: a `BuiltForSpeed` freeze converts a copy and
/// leaves the mutable automaton the next freeze reads untouched. That the
/// conversion is freeze-local is what lets a replay carry the mode at all, so it
/// is worth pinning on its own. Go has no analogue — there a pattern's mode is
/// fixed when it is added.
///
/// Skipped under Miri, which never runs the DFA conversion, leaving nothing for
/// either direction to change.
#[cfg(not(miri))]
#[test]
fn test_build_mode_change_refreezes_existing_patterns() {
    let mut q = Quamina::new();
    for (id, pattern) in BUILD_MODE_WILDCARDS {
        q.add_pattern(id, pattern).unwrap();
    }
    let comfort = q.matcher_stats();
    assert!(
        comfort.fanouts > 0,
        "BuiltForComfort is the default, so the wildcards should still be an NFA",
    );

    q.set_matcher_build_mode(MatcherBuildMode::BuiltForSpeed);
    let speed = q.matcher_stats();
    assert_eq!(
        speed.fanouts, 0,
        "switching to BuiltForSpeed must convert patterns that were already added",
    );
    assert_ne!(
        speed.states, comfort.states,
        "the converted DFA should be a different size than the NFA it replaced",
    );

    q.set_matcher_build_mode(MatcherBuildMode::BuiltForComfort);
    assert_eq!(
        q.matcher_stats(),
        comfort,
        "switching back must give the NFA again, the conversion having been \
         applied to a copy",
    );
}
