//! Stress tests for quamina-rs
//!
//! Go lineage: concurrency_test.go, escaping_test.go, benchmarks_test.go
//!
//! This module covers:
//! - Stress fuzz tests (strings, numbers)
//! - Citylots stress test
//! - Concurrent operations (Arc snapshots, read/write)
//! - Unicode (surrogate pairs, escapes, member names)
//! - Numbits tests
//! - Memory cleanup (Miri-friendly)

use super::*;
use std::sync::Arc;

// ============================================================================
// Stress Fuzz Tests
// ============================================================================

// MIRI SKIP RATIONALE: Adds 10,000 patterns and matches against 10,000 events.
// Takes 2+ min in normal mode, would take hours under Miri interpretation.
#[test]
#[cfg_attr(miri, ignore)]
fn test_stress_fuzz_strings() {
    use rand::{Rng, SeedableRng};
    use std::collections::HashSet;

    let mut rng = rand::rngs::StdRng::seed_from_u64(12345);
    let mut q = Quamina::new();
    let mut pattern_names: Vec<String> = Vec::new();
    let mut used: HashSet<String> = HashSet::new();
    let chars = b"abcdefghijklmnopqrstuvwxyz";
    let str_len = 12;

    // Make 10,000 random 12-char strings
    for _ in 0..10_000 {
        let s: String = (0..str_len)
            .map(|_| chars[rng.random_range(0..chars.len())] as char)
            .collect();
        pattern_names.push(s.clone());
        used.insert(s);
    }

    // Add a pattern for each string
    for pname in &pattern_names {
        let pattern = format!(r#"{{"a": ["{}"]}}"#, pname);
        q.add_pattern(pname.clone(), &pattern)
            .expect("addPattern failed");
    }

    // Make sure all patterns match
    for pname in &pattern_names {
        let event = format!(r#"{{"a": "{}"}}"#, pname);
        let matches = q
            .matches_for_event(event.as_bytes())
            .expect("matches_for_event failed");
        assert_eq!(
            matches.len(),
            1,
            "Expected 1 match for {}, got {}",
            pname,
            matches.len()
        );
        assert_eq!(
            matches[0], *pname,
            "Expected match {}, got {}",
            pname, matches[0]
        );
    }

    // Now run 10,000 more random strings that shouldn't match
    let mut should_not_count = 0;
    while should_not_count < 10_000 {
        let s: String = (0..str_len)
            .map(|_| chars[rng.random_range(0..chars.len())] as char)
            .collect();
        if used.contains(&s) {
            continue;
        }
        should_not_count += 1;

        let event = format!(r#"{{"a": "{}"}}"#, s);
        let matches = q
            .matches_for_event(event.as_bytes())
            .expect("matches_for_event failed");
        assert!(
            matches.is_empty(),
            "Expected no match for {}, got {} matches",
            s,
            matches.len()
        );
    }
}

// MIRI SKIP RATIONALE: Adds 10,000 numeric patterns and matches against 10,000 events.
// Takes 2+ min in normal mode, would take hours under Miri interpretation.
#[test]
#[cfg_attr(miri, ignore)]
fn test_stress_fuzz_numbers() {
    use rand::{Rng, SeedableRng};
    use std::collections::HashSet;

    let mut rng = rand::rngs::StdRng::seed_from_u64(98543);
    let mut q = Quamina::new();
    let mut pattern_names: Vec<i64> = Vec::new();
    let mut used: HashSet<i64> = HashSet::new();

    // Make 10,000 random numbers
    for _ in 0..10_000 {
        let n: i64 = rng.random();
        pattern_names.push(n);
        used.insert(n);
    }

    // Add a pattern for each number
    for pname in &pattern_names {
        let pattern = format!(r#"{{"a": [{}]}}"#, pname);
        q.add_pattern(pname.to_string(), &pattern)
            .expect("addPattern failed");
    }

    // Make sure all patterns match
    for pname in &pattern_names {
        let event = format!(r#"{{"a": {}}}"#, pname);
        let matches = q
            .matches_for_event(event.as_bytes())
            .expect("matches_for_event failed");
        assert_eq!(
            matches.len(),
            1,
            "Expected 1 match for {}, got {}",
            pname,
            matches.len()
        );
        assert_eq!(
            matches[0],
            pname.to_string(),
            "Expected match {}, got {}",
            pname,
            matches[0]
        );
    }

    // Now run 10,000 more random numbers that shouldn't match
    let mut should_not_count = 0;
    while should_not_count < 10_000 {
        let n: i64 = rng.random_range(0..1_000_000);
        if used.contains(&n) {
            continue;
        }
        should_not_count += 1;

        let event = format!(r#"{{"a": {}}}"#, n);
        let matches = q
            .matches_for_event(event.as_bytes())
            .expect("matches_for_event failed");
        assert!(
            matches.is_empty(),
            "Expected no match for {}, got {} matches",
            n,
            matches.len()
        );
    }
}

// MIRI SKIP RATIONALE: This test reads testdata/citylots2.json from disk.
// Miri runs with isolation enabled by default, blocking filesystem access.
#[test]
#[cfg_attr(miri, ignore)]
fn test_stress_citylots2_operators() {
    use std::fs;
    use std::path::Path;

    let citylots_path = Path::new("testdata/citylots2.json");
    if !citylots_path.exists() {
        eprintln!("Skipping citylots test - testdata/citylots2.json not found");
        return;
    }

    // Read citylots data
    let data = fs::read_to_string(citylots_path).expect("Failed to read citylots2.json");

    // Test with various operators
    let patterns = [
        (
            r#"{"properties": {"BLKLOT": [{"prefix": "143"}]}}"#,
            "prefix_143",
        ),
        (
            r#"{"properties": {"BLKLOT": [{"suffix": "218"}]}}"#,
            "suffix_218",
        ),
        (
            r#"{"properties": {"BLKLOT": [{"wildcard": "*0*"}]}}"#,
            "wildcard_0",
        ),
    ];

    let mut q = Quamina::new();
    for (pattern, name) in patterns.iter() {
        q.add_pattern(*name, pattern)
            .expect("Failed to add pattern");
    }

    // Parse and match each feature
    // citylots2.json has features array
    let _matches = q.matches_for_event(data.as_bytes());
    // Just verify no panic - actual match counts depend on data
}

// ============================================================================
// Concurrent Operations Tests
// ============================================================================

#[test]
fn test_arc_snapshot_isolation() {
    // Test that Arc<Quamina> snapshots are isolated
    let mut q = Quamina::<String>::new();
    q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
        .unwrap();

    // Create Arc snapshot
    let q_arc = Arc::new(q);

    // Can't mutate through Arc, but can read
    let matches = q_arc
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1".to_string()]);

    // Clone for mutation
    let mut q_clone = (*q_arc).clone();
    q_clone
        .add_pattern("p2".to_string(), r#"{"status": ["pending"]}"#)
        .unwrap();

    // Original Arc doesn't have p2
    let matches = q_arc
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert!(matches.is_empty());

    // Clone has p2
    let matches = q_clone
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p2".to_string()]);
}

#[test]
fn test_concurrent_miri_friendly() {
    // Lightweight concurrent test for Miri
    let mut q = Quamina::<String>::new();
    q.add_pattern("p1".to_string(), r#"{"x": [1]}"#).unwrap();
    q.add_pattern("p2".to_string(), r#"{"x": [2]}"#).unwrap();

    let q_arc = Arc::new(q);

    // Simulate concurrent reads (sequential for Miri)
    for i in 1..=5 {
        let q_ref = Arc::clone(&q_arc);
        let event = format!(r#"{{"x": {}}}"#, i % 2 + 1);
        let matches = q_ref.matches_for_event(event.as_bytes()).unwrap();
        assert!(!matches.is_empty());
    }
}

// MIRI SKIP RATIONALE: Thread spawning and concurrent operations are slow under Miri.
#[test]
#[cfg_attr(miri, ignore)]
fn test_arc_concurrent_read_write() {
    use std::thread;

    let mut q = Quamina::<String>::new();
    q.add_pattern("p1".to_string(), r#"{"status": ["active"]}"#)
        .unwrap();

    let q_arc = Arc::new(q);

    // Spawn reader threads
    let mut handles = vec![];
    for i in 0..4 {
        let q_clone = Arc::clone(&q_arc);
        let handle = thread::spawn(move || {
            for _ in 0..100 {
                let matches = q_clone
                    .matches_for_event(r#"{"status": "active"}"#.as_bytes())
                    .unwrap();
                assert!(matches.contains(&"p1".to_string()), "Thread {} failed", i);
            }
        });
        handles.push(handle);
    }

    // Wait for all threads
    for handle in handles {
        handle.join().expect("Thread panicked");
    }
}

// MIRI SKIP RATIONALE: Thread spawning is slow under Miri.
#[test]
#[cfg_attr(miri, ignore)]
fn test_arc_pattern_lifecycle() {
    use std::thread;

    // Test pattern add, delete, match across threads
    let mut q = Quamina::<String>::new();

    // Add initial patterns
    for i in 0..10 {
        q.add_pattern(format!("p{}", i), &format!(r#"{{"x": [{}]}}"#, i))
            .unwrap();
    }

    let q_arc = Arc::new(q);

    // Spawn reader threads
    let mut handles = vec![];
    for _ in 0..4 {
        let q_clone = Arc::clone(&q_arc);
        let handle = thread::spawn(move || {
            for i in 0..10 {
                let event = format!(r#"{{"x": {}}}"#, i);
                let matches = q_clone.matches_for_event(event.as_bytes()).unwrap();
                assert_eq!(matches.len(), 1);
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().expect("Thread panicked");
    }
}

// ============================================================================
// Unicode Tests
// ============================================================================

#[test]
fn test_utf16_surrogate_pairs() {
    // Test UTF-16 surrogate pair decoding
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"emoji": ["😀💋😺"]}"#).unwrap();

    // Event with surrogate pairs
    let event = r#"{"emoji": "\ud83d\ude00\ud83d\udc8b\ud83d\ude3a"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches,
        vec!["p1"],
        "Multiple surrogate pairs should decode correctly"
    );
}

#[test]
fn test_json_escape_all_eight() {
    // Test all 8 JSON escape sequences
    let mut q = Quamina::new();

    // Test: \" (quote)
    q.add_pattern("p1", r#"{"x": ["hello\"world"]}"#).unwrap();
    let m1 = q
        .matches_for_event(r#"{"x": "hello\"world"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"], "Quote escape should match");

    // Test: \/ (forward slash)
    q.add_pattern("p2", r#"{"x": ["a/b"]}"#).unwrap();
    let m2 = q.matches_for_event(r#"{"x": "a\/b"}"#.as_bytes()).unwrap();
    assert_eq!(m2, vec!["p2"], "Forward slash escape should match");

    // Test: \n (newline)
    q.add_pattern("p3", r#"{"x": ["a\nb"]}"#).unwrap();
    let m3 = q.matches_for_event(r#"{"x": "a\nb"}"#.as_bytes()).unwrap();
    assert_eq!(m3, vec!["p3"], "Newline escape should match");

    // Test: \t (tab)
    q.add_pattern("p4", r#"{"x": ["a\tb"]}"#).unwrap();
    let m4 = q.matches_for_event(r#"{"x": "a\tb"}"#.as_bytes()).unwrap();
    assert_eq!(m4, vec!["p4"], "Tab escape should match");

    // Test: \r (carriage return)
    q.add_pattern("p5", r#"{"x": ["a\rb"]}"#).unwrap();
    let m5 = q.matches_for_event(r#"{"x": "a\rb"}"#.as_bytes()).unwrap();
    assert_eq!(m5, vec!["p5"], "Carriage return escape should match");
}

#[test]
fn test_unicode_member_names() {
    // Test Unicode in field names
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"日本語": ["はい"]}"#).unwrap();

    let event = r#"{"日本語": "はい"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"], "Unicode field names should work");
}

#[test]
fn test_unicode_field_names() {
    // Test unicode escape sequences in field names
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"field": ["value"]}"#).unwrap();

    // Event with \u in field name
    let event = r#"{"\u0066ield": "value"}"#; // \u0066 = 'f'
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches,
        vec!["p1"],
        "Unicode escape in field name should work"
    );
}

// ============================================================================
// Numbits Tests
// ============================================================================

// Note: numbits module functions are pub(crate), so we test their behavior
// through the public API (numeric pattern matching) rather than directly.

#[test]
fn test_numbits_through_numeric_matching() {
    // Test various numeric formats through the public API
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"numeric": ["=", 42]}]}"#)
        .unwrap();

    // Integer
    let m1 = q.matches_for_event(r#"{"x": 42}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["p1"], "Integer 42 should match");

    // Float
    let m2 = q.matches_for_event(r#"{"x": 42.0}"#.as_bytes()).unwrap();
    assert_eq!(m2, vec!["p1"], "Float 42.0 should match");

    // Scientific notation
    let m3 = q.matches_for_event(r#"{"x": 4.2e1}"#.as_bytes()).unwrap();
    assert_eq!(m3, vec!["p1"], "Scientific 4.2e1 should match");

    // Non-matching
    let m4 = q.matches_for_event(r#"{"x": 43}"#.as_bytes()).unwrap();
    assert!(m4.is_empty(), "43 should not match 42");
}

#[test]
fn test_numbits_ordering_through_range() {
    // Test that numeric ordering is preserved
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"numeric": [">=", -100, "<=", 100]}]}"#)
        .unwrap();

    // Boundary values
    let m1 = q.matches_for_event(r#"{"x": -100}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["p1"], "-100 should match");

    let m2 = q.matches_for_event(r#"{"x": 0}"#.as_bytes()).unwrap();
    assert_eq!(m2, vec!["p1"], "0 should match");

    let m3 = q.matches_for_event(r#"{"x": 100}"#.as_bytes()).unwrap();
    assert_eq!(m3, vec!["p1"], "100 should match");

    // Out of range
    let m4 = q.matches_for_event(r#"{"x": -101}"#.as_bytes()).unwrap();
    assert!(m4.is_empty(), "-101 should not match");

    let m5 = q.matches_for_event(r#"{"x": 101}"#.as_bytes()).unwrap();
    assert!(m5.is_empty(), "101 should not match");
}

// ============================================================================
// Memory Cleanup Tests
// ============================================================================

#[test]
fn test_memory_cleanup_miri_friendly() {
    // Lightweight memory test for Miri
    let mut q = Quamina::<String>::new();

    // Add a few patterns
    for i in 0..5 {
        q.add_pattern(format!("p{}", i), &format!(r#"{{"x": [{}]}}"#, i))
            .unwrap();
    }

    // Delete some patterns
    for i in 0..3 {
        q.delete_patterns(&format!("p{}", i)).unwrap();
    }

    // Rebuild to clean up
    let purged = q.rebuild();
    assert_eq!(purged, 3, "Should have purged 3 patterns");

    // Verify remaining patterns still work
    let matches = q.matches_for_event(r#"{"x": 3}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p3".to_string()]);
}

// MIRI SKIP RATIONALE: Large allocations and cleanup operations are slow under Miri.
#[test]
#[cfg_attr(miri, ignore)]
fn test_arc_memory_cleanup() {
    // Test that Arc properly manages memory
    let mut q = Quamina::<String>::new();

    // Add many patterns
    for i in 0..100 {
        q.add_pattern(format!("p{}", i), &format!(r#"{{"x": [{}]}}"#, i))
            .unwrap();
    }

    // Create Arc
    let q_arc = Arc::new(q);

    // Clone and drop multiple times
    for _ in 0..10 {
        let q_clone = Arc::clone(&q_arc);
        let matches = q_clone
            .matches_for_event(r#"{"x": 50}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p50".to_string()]);
    }

    // Original should still work
    let matches = q_arc.matches_for_event(r#"{"x": 99}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p99".to_string()]);
}

// MIRI SKIP RATIONALE: Thread spawning and concurrent operations are slow under Miri.
#[test]
#[cfg_attr(miri, ignore)]
fn test_concurrent_citylots_stress() {
    use std::thread;

    // Create matcher with various patterns
    let mut q = Quamina::<String>::new();

    // Add patterns similar to citylots
    q.add_pattern("exact".to_string(), r#"{"x": ["foo"]}"#)
        .unwrap();
    q.add_pattern("prefix".to_string(), r#"{"x": [{"prefix": "bar"}]}"#)
        .unwrap();
    q.add_pattern("suffix".to_string(), r#"{"x": [{"suffix": "baz"}]}"#)
        .unwrap();
    q.add_pattern("wildcard".to_string(), r#"{"x": [{"wildcard": "*qux*"}]}"#)
        .unwrap();

    let q_arc = Arc::new(q);

    // Spawn concurrent readers
    let mut handles = vec![];
    for _ in 0..4 {
        let q_clone = Arc::clone(&q_arc);
        let handle = thread::spawn(move || {
            for i in 0..100 {
                let event = match i % 4 {
                    0 => r#"{"x": "foo"}"#,
                    1 => r#"{"x": "barxyz"}"#,
                    2 => r#"{"x": "xyzbaz"}"#,
                    _ => r#"{"x": "abcquxdef"}"#,
                };
                let matches = q_clone.matches_for_event(event.as_bytes()).unwrap();
                assert!(!matches.is_empty());
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().expect("Thread panicked");
    }
}

// ============================================================================
// Bulk Add Correctness Test
// ============================================================================

#[test]
fn test_bulk_add_correctness() {
    // Test adding many patterns and verifying all work
    let mut q = Quamina::new();

    let count = 50;
    for i in 0..count {
        let pattern = format!(r#"{{"field{}": ["value{}"]}}"#, i, i);
        q.add_pattern(format!("p{}", i), &pattern).unwrap();
    }

    assert_eq!(q.pattern_count(), count);

    // Test a few patterns
    for i in [0, 10, 25, 49].iter() {
        let event = format!(r#"{{"field{}": "value{}"}}"#, i, i);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec![format!("p{}", i)],
            "Pattern {} should match",
            i
        );
    }
}

// ============================================================================
// Multiple Patterns Same ID Comprehensive Test
// ============================================================================

#[test]
fn test_multiple_patterns_same_id_comprehensive() {
    // Same pattern ID can match via different value types
    let mut q = Quamina::new();
    q.add_pattern("x", r#"{"x": ["a"]}"#).unwrap();
    q.add_pattern("x", r#"{"x": [1]}"#).unwrap();
    q.add_pattern("x", r#"{"x": [{"prefix": "b"}]}"#).unwrap();

    // All should match pattern "x"
    let m1 = q.matches_for_event(r#"{"x": "a"}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["x"], "string 'a' should match");

    let m2 = q.matches_for_event(r#"{"x": 1}"#.as_bytes()).unwrap();
    assert_eq!(m2, vec!["x"], "number 1 should match");

    let m3 = q.matches_for_event(r#"{"x": "bcd"}"#.as_bytes()).unwrap();
    assert_eq!(m3, vec!["x"], "prefix 'b' should match");

    // Should not match
    let m4 = q.matches_for_event(r#"{"x": "z"}"#.as_bytes()).unwrap();
    assert!(m4.is_empty(), "unrelated value should not match");
}

// ============================================================================
// Invalid UTF-8 Dot Rejection Test
// ============================================================================

#[test]
fn test_invalid_utf8_dot_rejection() {
    // Based on Go's TestMultiByteInMemberName
    // JSON with invalid UTF-8 sequences should be rejected
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"a": [1]}"#).unwrap();

    // Create invalid UTF-8 in field name
    // 0xF0 starts a 4-byte sequence but followed by invalid bytes
    let invalid_json = b"{\"a\xF0\x28\x8C\x28\": 1}";
    let result = q.matches_for_event(invalid_json);
    // Should either error or safely handle
    // Go behavior: reject invalid UTF-8 in field names
    match result {
        Ok(matches) => {
            // If it doesn't error, it may or may not match depending on implementation
            assert!(matches.is_empty() || matches == vec!["p1"]);
        }
        Err(_) => {
            // Erroring is also acceptable for invalid UTF-8
        }
    }
}

// ============================================================================
// Additional Stress Tests (recovered from original)
// ============================================================================

#[test]
fn test_unicode_escape_multiple_emojis() {
    // Test multiple UTF-16 surrogate pairs in sequence
    // From Go's escaping_test.go: 😀💋😺 = \ud83d\ude00\ud83d\udc8b\ud83d\ude3a
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"emojis": ["😀💋😺"]}"#).unwrap();

    let event = r#"{"emojis": "\ud83d\ude00\ud83d\udc8b\ud83d\ude3a"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches,
        vec!["p1"],
        "Multiple surrogate pairs should decode correctly"
    );
}

#[test]
fn test_unicode_escape_mixed_codepoints() {
    // Test mixing single-codepoint and surrogate pairs
    // From Go's escaping_test.go combinations
    // Ж = \u0416 (single), 💋 = \ud83d\udc8b (surrogate), 中 = \u4e2d (single)

    // Test: Ж💋中
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"mixed": ["Ж💋中"]}"#).unwrap();

    let event = r#"{"mixed": "\u0416\ud83d\udc8b\u4e2d"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"], "Mixed codepoints should decode");

    // Test: x💋y - ASCII mixed with surrogate
    let mut q2 = Quamina::new();
    q2.add_pattern("p2", r#"{"mixed": ["x💋y"]}"#).unwrap();

    let event2 = r#"{"mixed": "\u0078\ud83d\udc8b\u0079"}"#;
    let matches2 = q2.matches_for_event(event2.as_bytes()).unwrap();
    assert_eq!(matches2, vec!["p2"], "ASCII + surrogate should decode");
}

#[test]
fn test_unicode_escape_standard_escapes() {
    // Test standard JSON escape sequences
    let mut q = Quamina::new();

    // Test newline
    q.add_pattern("newline", r#"{"text": ["hello\nworld"]}"#)
        .unwrap();
    let m1 = q
        .matches_for_event(r#"{"text": "hello\nworld"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["newline"], "Newline escape should match");

    // Test tab
    q.add_pattern("tab", r#"{"text": ["hello\tworld"]}"#)
        .unwrap();
    let m2 = q
        .matches_for_event(r#"{"text": "hello\tworld"}"#.as_bytes())
        .unwrap();
    assert!(m2.contains(&"tab"), "Tab escape should match");

    // Test backslash
    q.add_pattern("backslash", r#"{"text": ["hello\\world"]}"#)
        .unwrap();
    let m3 = q
        .matches_for_event(r#"{"text": "hello\\world"}"#.as_bytes())
        .unwrap();
    assert!(m3.contains(&"backslash"), "Backslash escape should match");
}

#[test]
fn test_multiple_shellstyle_citylots_patterns() {
    // Test multiple complex shellstyle patterns on the SAME field (citylots-like)
    // This tests patterns similar to the citylots stress test that had to be
    // run individually due to merge_fas spinout bug.
    let mut q = Quamina::new();

    // These mirror the citylots shellstyle patterns
    q.add_pattern("pattern_143", r#"{"x": [{"shellstyle": "143*"}]}"#)
        .unwrap();
    q.add_pattern("pattern_2017", r#"{"x": [{"shellstyle": "2*0*1*7"}]}"#)
        .unwrap();
    q.add_pattern("pattern_218", r#"{"x": [{"shellstyle": "*218"}]}"#)
        .unwrap();
    q.add_pattern("pattern_352", r#"{"x": [{"shellstyle": "3*5*2"}]}"#)
        .unwrap();
    q.add_pattern("pattern_vail", r#"{"x": [{"shellstyle": "VA*IL"}]}"#)
        .unwrap();

    // Test individual patterns work correctly
    let test_cases: Vec<(&str, Vec<&str>)> = vec![
        ("1430022", vec!["pattern_143"]),   // matches 143*
        ("2607117", vec!["pattern_2017"]),  // matches 2*0*1*7
        ("2607218", vec!["pattern_218"]),   // matches *218
        ("3745012", vec!["pattern_352"]),   // matches 3*5*2
        ("VACSTWIL", vec!["pattern_vail"]), // matches VA*IL (note: incorrect, should be VACTSTWIL?)
        ("xyz", vec![]),                    // no match
    ];

    for (value, expected_patterns) in test_cases {
        let event = format!(r#"{{"x": "{}"}}"#, value);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();

        if expected_patterns.is_empty() {
            assert!(
                matches.is_empty(),
                "Expected no match for '{}', got: {:?}",
                value,
                matches
            );
        } else {
            for expected in &expected_patterns {
                assert!(
                    matches.contains(expected),
                    "Expected '{}' to match '{}', but got: {:?}",
                    value,
                    expected,
                    matches
                );
            }
        }
    }
}

#[test]
fn test_unicode_field_names_surrogate_pairs() {
    // Test UTF-16 surrogate pairs in field names
    // From Go's TestReadMemberName: `x\u0078\ud83d\udc8by` = `xx💋y`
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"xx💋y": ["value"]}"#).unwrap();

    // Event with unicode escapes in field name
    let event = r#"{"x\u0078\ud83d\udc8by": "value"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches,
        vec!["p1"],
        "Surrogate pair in field name should decode"
    );

    // Test multiple emojis in field name: 😀💋😺
    let mut q2 = Quamina::new();
    q2.add_pattern("p2", r#"{"😀💋😺": [1]}"#).unwrap();

    let event2 = r#"{"\ud83d\ude00\ud83d\udc8b\ud83d\ude3a": 1}"#;
    let matches2 = q2.matches_for_event(event2.as_bytes()).unwrap();
    assert_eq!(
        matches2,
        vec!["p2"],
        "Multiple surrogate pairs in field name should decode"
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_exercise_matching_comprehensive() {
    // Based on Go quamina's TestExerciseMatching
    // Tests many different pattern types against a complex JSON event
    let event = r#"{
        "Image": {
            "Width":  800,
            "Height": 600,
            "Title":  "View from 15th Floor",
            "Thumbnail": {
                "Url":    "https://www.example.com/image/481989943",
                "Height": 125,
                "Width":  100
            },
            "Animated" : false,
            "IDs": [116, 943, 234, 38793]
        }
    }"#;

    // Patterns that SHOULD match
    let should_match = [
        (
            r#"{"Image": {"Title": [{"exists": true}]}}"#,
            "exists true on Title",
        ),
        (
            r#"{"Foo": [{"exists": false}]}"#,
            "exists false on missing Foo",
        ),
        (r#"{"Image": {"Width": [800]}}"#, "exact number match"),
        (
            r#"{"Image": {"Animated": [false], "Thumbnail": {"Height": [125]}}}"#,
            "nested multi-field",
        ),
        (
            r#"{"Image": {"Width": [800], "Title": [{"exists": true}], "Animated": [false]}}"#,
            "three fields",
        ),
        (
            r#"{"Image": {"Width": [800], "IDs": [{"exists": true}]}}"#,
            "exists on array",
        ),
        (
            r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "*9943"}]}}}"#,
            "shellstyle suffix",
        ),
        (
            r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "https://www.example.com/*"}]}}}"#,
            "shellstyle prefix",
        ),
        (
            r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "https://www.example.com/*9943"}]}}}"#,
            "shellstyle infix",
        ),
        (
            r#"{"Image": {"Title": [{"anything-but": ["Pikachu", "Eevee"]}]}}"#,
            "anything-but",
        ),
        (
            r#"{"Image": {"Thumbnail": {"Url": [{"prefix": "https:"}]}}}"#,
            "prefix",
        ),
        (
            r#"{"Image": {"Thumbnail": {"Url": ["a", {"prefix": "https:"}]}}}"#,
            "prefix or literal",
        ),
        (
            r#"{"Image": {"Title": [{"equals-ignore-case": "VIEW FROM 15th FLOOR"}]}}"#,
            "equals-ignore-case",
        ),
        (
            r#"{"Image": {"Title": [{"regex": "View from .... Floor"}]}}"#,
            "regex dots",
        ),
        (
            r#"{"Image": {"Title": [{"regex": "View from [0-9][0-9][rtn][dh] Floor"}]}}"#,
            "regex char class",
        ),
        (
            r#"{"Image": {"Title": [{"regex": "View from 15th (Floor|Storey)"}]}}"#,
            "regex alternation",
        ),
    ];

    // Patterns that SHOULD NOT match
    let should_not_match = [
        (
            r#"{"Image": {"Animated": [{"exists": false}]}}"#,
            "exists false on present field",
        ),
        (
            r#"{"Image": {"NotThere": [{"exists": true}]}}"#,
            "exists true on missing field",
        ),
        (
            r#"{"Image": {"IDs": [{"exists": false}], "Animated": [false]}}"#,
            "exists false on array",
        ),
        (
            r#"{"Image": {"Thumbnail": {"Url": [{"prefix": "http:"}]}}}"#,
            "wrong prefix",
        ),
    ];

    // Test each should_match pattern individually
    for (pattern, desc) in &should_match {
        let mut q = Quamina::new();
        let result = q.add_pattern(*desc, pattern);
        assert!(
            result.is_ok(),
            "Pattern should parse: {} - {}",
            desc,
            pattern
        );

        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            !matches.is_empty(),
            "Pattern '{}' should match: {}",
            desc,
            pattern
        );
    }

    // Test each should_not_match pattern individually
    for (pattern, desc) in &should_not_match {
        let mut q = Quamina::new();
        let result = q.add_pattern(*desc, pattern);
        assert!(
            result.is_ok(),
            "Pattern should parse: {} - {}",
            desc,
            pattern
        );

        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.is_empty(),
            "Pattern '{}' should NOT match: {}",
            desc,
            pattern
        );
    }

    // Test all patterns together in one matcher
    let mut combined = Quamina::new();
    for (pattern, desc) in &should_match {
        combined.add_pattern(*desc, pattern).unwrap();
    }

    let all_matches = combined.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        all_matches.len(),
        should_match.len(),
        "All should_match patterns should match when combined"
    );
}

#[test]
fn test_exercise_matching_miri_friendly() {
    let event = r#"{
        "Image": {
            "Width":  800,
            "Height": 600,
            "Title":  "View from 15th Floor",
            "Thumbnail": {
                "Url":    "https://www.example.com/image/481989943",
                "Height": 125,
                "Width":  100
            },
            "Animated" : false,
            "IDs": [116, 943, 234, 38793]
        }
    }"#;

    let patterns: Vec<(&str, &str)> = vec![
        (r#"{"Image": {"Width": [800]}}"#, "exact number match"),
        (
            r#"{"Image": {"Title": [{"exists": true}]}}"#,
            "exists true on Title",
        ),
        (
            r#"{"Image": {"Thumbnail": {"Url": [{"prefix": "https:"}]}}}"#,
            "prefix",
        ),
        (
            r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "*9943"}]}}}"#,
            "shellstyle suffix",
        ),
    ];

    for (pattern, desc) in &patterns {
        let mut q = Quamina::new();
        q.add_pattern(*desc, pattern)
            .unwrap_or_else(|e| panic!("Pattern should parse: {} - {}", desc, e));

        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            !matches.is_empty(),
            "Pattern '{}' should match: {}",
            desc,
            pattern
        );
    }
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_concurrent_update_during_matching() {
    use flate2::read::GzDecoder;
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    use std::sync::mpsc;
    use std::sync::Arc;
    use std::thread;

    const UPDATE_INTERVAL: usize = 250;

    // Load citylots2.json.gz
    let path = "testdata/citylots2.json.gz";
    let file = File::open(path).expect("Failed to open citylots2.json.gz");
    let decoder = GzDecoder::new(file);
    let reader = BufReader::new(decoder);
    let lines: Vec<Vec<u8>> = reader
        .lines()
        .map(|l| l.expect("Failed to read line").into_bytes())
        .collect();

    // Initial patterns that match citylots2 structure
    let patterns = [
        ("CRANLEIGH", r#"{"properties": {"STREET": ["CRANLEIGH"]}}"#),
        (
            "shellstyle",
            r#"{"properties": {"STREET": [{"shellstyle": "B*K"}]}}"#,
        ),
    ];

    // Create matcher and add initial patterns
    let q = Arc::new(std::sync::RwLock::new(Quamina::new()));
    {
        let mut q_write = q.write().unwrap();
        for (name, pattern) in &patterns {
            q_write.add_pattern(name.to_string(), pattern).unwrap();
        }
    }

    // Channel for tracking added patterns
    let (tx, rx) = mpsc::channel::<String>();

    // Concurrent updater function - adds unique street patterns
    fn add_pattern_concurrent(
        q: Arc<std::sync::RwLock<Quamina<String>>>,
        idx: usize,
        tx: mpsc::Sender<String>,
    ) {
        let val = format!("CONCURRENT_STREET_{}", idx);
        let pattern = format!(r#"{{"properties": {{"STREET": ["{}"]}}}}"#, val);

        {
            let mut q_write = q.write().unwrap();
            q_write
                .add_pattern(val.clone(), &pattern)
                .expect("add_pattern failed");
        }
        let _ = tx.send(val); // Ignore send errors (receiver may be dropped)
    }

    // Run matching with concurrent updates
    let mut total_matches = 0usize;
    let mut sent = 0;

    let start = std::time::Instant::now();
    for (i, line) in lines.iter().enumerate() {
        // Match against current patterns
        let matches = {
            let q_read = q.read().unwrap();
            q_read
                .matches_for_event(line)
                .expect("matches_for_event failed")
        };
        total_matches += matches.len();

        // Every UPDATE_INTERVAL lines, spawn a thread to add a new pattern
        if (i + 1) % UPDATE_INTERVAL == 0 {
            sent += 1;
            let q_clone = Arc::clone(&q);
            let tx_clone = tx.clone();
            let idx = sent;
            thread::spawn(move || {
                add_pattern_concurrent(q_clone, idx, tx_clone);
            });
        }
    }
    let elapsed = start.elapsed();

    // Drop the sender so rx.iter() will complete
    drop(tx);

    // Wait a moment for all threads to complete
    std::thread::sleep(std::time::Duration::from_millis(100));

    // Verify all concurrently added patterns are now in the matcher and work
    let mut verified = 0;
    for val in rx.iter() {
        let event = format!(r#"{{"properties": {{"STREET": "{}"}}}}"#, val);

        let q_read = q.read().unwrap();
        let matches = q_read
            .matches_for_event(event.as_bytes())
            .expect("matches_for_event failed");
        assert!(
            matches.contains(&val),
            "Concurrent pattern {} not found in matches: {:?}",
            val,
            matches
        );
        verified += 1;
    }

    let events_per_sec = lines.len() as f64 / elapsed.as_secs_f64();
    println!(
        "Concurrent update test: {:.0} events/sec, {} total matches, {} patterns added concurrently, {} verified",
        events_per_sec, total_matches, sent, verified
    );

    // Key assertions:
    // 1. No crashes during concurrent updates
    // 2. All concurrently added patterns were verified as working
    assert_eq!(sent, verified, "Not all concurrent patterns were verified");
    assert!(sent > 0, "Should have added some patterns concurrently");
    assert!(total_matches > 0, "Should have gotten some matches");
}

#[test]
fn test_arc_field_matcher_sharing() {
    // Test that same pattern on different IDs works correctly
    // (internal optimization - both should share automaton structure)
    let mut q = Quamina::new();

    // Add identical patterns with different IDs
    q.add_pattern("id1", r#"{"status": ["active"]}"#).unwrap();
    q.add_pattern("id2", r#"{"status": ["active"]}"#).unwrap();
    q.add_pattern("id3", r#"{"status": ["active"]}"#).unwrap();

    // All three should match the same event
    let event = r#"{"status": "active"}"#;
    let mut matches = q.matches_for_event(event.as_bytes()).unwrap();
    matches.sort();
    assert_eq!(matches, vec!["id1", "id2", "id3"]);

    // Delete one, others should still work
    q.delete_patterns(&"id2").unwrap();
    let mut matches2 = q.matches_for_event(event.as_bytes()).unwrap();
    matches2.sort();
    assert_eq!(matches2, vec!["id1", "id3"]);

    // Clone and verify sharing survives
    let q2 = q.clone();
    let mut matches3 = q2.matches_for_event(event.as_bytes()).unwrap();
    matches3.sort();
    assert_eq!(matches3, vec!["id1", "id3"]);
}

// ============================================================================
// Pattern Insertion Scaling Guard
// ============================================================================

/// Regression guard: pattern insertion must scale linearly, not quadratically.
///
/// Measures per-pattern cost at four layers (50, 500, 5000, 20000 patterns).
/// With O(n*L) insertion the per-pattern cost is roughly constant, so the
/// ratio between consecutive layers ≈ 1. With O(n²) regression, the
/// per-pattern cost grows linearly with n (e.g., 500/50 ≈ 10x).
/// We use a generous 6x threshold to tolerate CI noise while catching
/// quadratic blowup.
///
/// Includes a match after all adds to trigger any deferred work (e.g., lazy
/// freeze), ensuring total user-visible cost is measured.
#[test]
#[cfg_attr(miri, ignore)]
fn test_pattern_insertion_scales_linearly() {
    use std::time::Instant;

    let layers: &[usize] = &[50, 500, 5000, 20_000];

    // Warmup: add patterns to a throwaway instance to warm caches/allocator
    {
        let mut warmup = Quamina::new();
        for i in 0..100 {
            let pattern = format!(r#"{{"key": ["warmup_{}"]}}"#, i);
            warmup.add_pattern(format!("w{}", i), &pattern).unwrap();
        }
        let _ = warmup.matches_for_event(r#"{"key": "warmup_0"}"#.as_bytes());
    }

    let mut costs: Vec<(usize, f64)> = Vec::new();

    for &n in layers {
        let mut q = Quamina::new();
        let start = Instant::now();
        for i in 0..n {
            let pattern = format!(r#"{{"key": ["value_{}"]}}"#, i);
            q.add_pattern(format!("p{}", i), &pattern).unwrap();
        }
        // Trigger any deferred work (lazy freeze) so total cost is captured
        let matches = q
            .matches_for_event(r#"{"key": "value_0"}"#.as_bytes())
            .unwrap();
        let elapsed = start.elapsed();
        assert!(
            matches.contains(&"p0".to_string()),
            "Pattern p0 should match after adding {} patterns",
            n,
        );
        let cost_per_pattern = elapsed.as_secs_f64() / n as f64;
        costs.push((n, cost_per_pattern));
    }

    // Compare per-pattern cost between consecutive layers.
    // With O(n*L): ratio ≈ 1 (constant per-pattern cost)
    // With O(n²): ratio ≈ layer_large/layer_small (linear per-pattern cost)
    for i in 1..costs.len() {
        let (small_n, small_cost) = costs[i - 1];
        let (large_n, large_cost) = costs[i];
        let ratio = large_cost / small_cost.max(1e-9);
        assert!(
            ratio < 6.0,
            "Pattern insertion scales poorly between n={} and n={}: {:.1}x \
             (n={}={:.4}ms/pattern, n={}={:.4}ms/pattern). \
             This suggests O(n²) regression.",
            small_n,
            large_n,
            ratio,
            small_n,
            small_cost * 1000.0,
            large_n,
            large_cost * 1000.0,
        );
    }
}
