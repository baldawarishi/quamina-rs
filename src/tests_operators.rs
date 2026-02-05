//! Operator tests for quamina-rs
//!
//! Go lineage: anything_but_test.go, shellstyle_test.go, regexp_test.go, monocase_test.go
//!
//! This module covers:
//! - Prefix/suffix operators
//! - Wildcard/shellstyle (comprehensive, escapes, multi-patterns)
//! - Anything-but (strings, numbers, validation, merging)
//! - Equals-ignore-case
//! - Numeric comparisons (greater than, ranges, equals)
//! - Regex/regexp (validity, end-to-end, samples)
//! - CIDR matching (IPv4, IPv6)
//! - Lookaround patterns

use super::*;

// ============================================================================
// Helper Functions
// ============================================================================

/// Helper for multi-pattern wildcard tests (mirrors Go's exerciseMultiPatterns)
fn exercise_multi_patterns(
    should_not_match_any: &[&str],
    patterns_with_matches: &[(&str, &[&str])],
) {
    let mut q = Quamina::new();

    // Add all patterns
    for (pattern, _) in patterns_with_matches {
        q.add_pattern(*pattern, pattern)
            .unwrap_or_else(|e| panic!("Failed to add pattern {}: {:?}", pattern, e));
    }

    // Verify each pattern matches its expected values
    for (pattern, should_match) in patterns_with_matches {
        for val in *should_match {
            let event = format!(r#"{{"x":"{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.contains(pattern),
                "Pattern '{}' should match '{}', got {:?}",
                pattern,
                val,
                matches
            );
        }
    }

    // Verify none of the should_not_match values match any pattern
    for val in should_not_match_any {
        let event = format!(r#"{{"x":"{}"}}"#, val);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.is_empty(),
            "'{}' should not match any pattern, got {:?}",
            val,
            matches
        );
    }
}

// ============================================================================
// Prefix/Suffix Operator Tests
// ============================================================================

#[test]
fn test_prefix_match() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"name": [{"prefix": "prod-"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"name": "prod-server-1"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match prefix");

    let no_match = q
        .matches_for_event(r#"{"name": "dev-server-1"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty(), "Should not match different prefix");
}

#[test]
fn test_suffix() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"file": [{"suffix": ".jpg"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"file": "photo.jpg"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"]);

    let no_match = q
        .matches_for_event(r#"{"file": "photo.png"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
}

// ============================================================================
// Wildcard Operator Tests
// ============================================================================

#[test]
fn test_wildcard_suffix() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"file": [{"wildcard": "*.txt"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"file": "document.txt"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match *.txt");

    let no_match = q
        .matches_for_event(r#"{"file": "document.pdf"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty(), "Should not match .pdf");
}

#[test]
fn test_wildcard_prefix() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"name": [{"wildcard": "prod-*"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"name": "prod-server"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match prod-*");
}

#[test]
fn test_wildcard_contains() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"msg": [{"wildcard": "*error*"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"msg": "an error occurred"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match *error*");

    let no_match = q
        .matches_for_event(r#"{"msg": "all good"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_wildcard_matches_empty_string() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"wildcard": "*"}]}"#)
        .unwrap();

    let m1 = q.matches_for_event(r#"{"x": ""}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["p1"], "* should match empty string");

    let m2 = q.matches_for_event(r#"{"x": "hello"}"#.as_bytes()).unwrap();
    assert_eq!(m2, vec!["p1"], "* should match any string");
}

#[test]
fn test_wildcard_escape_star() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"val": [{"wildcard": "a\\*b"}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"val": "a*b"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"], "\\* should match literal *");

    let no_match = q.matches_for_event(r#"{"val": "aXb"}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty(), "Escaped * should not be wildcard");
}

#[test]
fn test_wildcard_escape_backslash() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"path": [{"wildcard": "a\\\\b"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"path": "a\\b"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "\\\\ should match literal \\");
}

#[test]
fn test_wildcard_invalid_patterns() {
    let mut q = Quamina::new();
    let result = q.add_pattern("p1", r#"{"x": [{"wildcard": "foo**bar"}]}"#);
    assert!(result.is_err(), "Adjacent ** should be rejected");

    let mut q2 = Quamina::new();
    let result2 = q2.add_pattern("p2", r#"{"x": [{"wildcard": "he\\llo"}]}"#);
    assert!(result2.is_err(), "Invalid escape \\l should be rejected");

    let mut q3 = Quamina::new();
    let result3 = q3.add_pattern("p3", r#"{"x": [{"wildcard": "x\\"}]}"#);
    assert!(result3.is_err(), "Trailing backslash should be rejected");
}

// ============================================================================
// Shellstyle Tests
// ============================================================================

#[test]
fn test_shellstyle_suffix() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"a": [{"shellstyle": "*bc"}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"a": "bc"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let matches = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let no_match = q.matches_for_event(r#"{"a": "xyz"}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_shellstyle_prefix() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"c": [{"shellstyle": "xy*"}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"c": "xyzzz"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let matches = q.matches_for_event(r#"{"c": "xy"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);
}

#[test]
fn test_shellstyle_infix() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"b": [{"shellstyle": "d*f"}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"b": "dexef"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let matches = q.matches_for_event(r#"{"b": "df"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);
}

#[test]
fn test_shellstyle_multiple_wildcards() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"d": [{"shellstyle": "12*4*"}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"d": "12345"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let matches = q.matches_for_event(r#"{"d": "1244"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let no_match = q.matches_for_event(r#"{"d": "1235"}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_shellstyle_contains() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"shellstyle": "*foo*"}]}"#)
        .unwrap();

    for text in ["xxfooyy", "fooyy", "xxfoo", "foo"] {
        let event = format!(r#"{{"x": "{}"}}"#, text);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match {}", text);
    }

    let no_match = q.matches_for_event(r#"{"x": "bar"}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_multiple_overlapping_shellstyle_patterns() {
    let mut q = Quamina::new();
    q.add_pattern("suffix_bc", r#"{"a": [{"shellstyle": "*bc"}]}"#)
        .unwrap();
    q.add_pattern("infix_ef", r#"{"b": [{"shellstyle": "d*f"}]}"#)
        .unwrap();
    q.add_pattern("prefix_xy", r#"{"c": [{"shellstyle": "xy*"}]}"#)
        .unwrap();

    let m1 = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
    assert!(m1.contains(&"suffix_bc"), "*bc should match abc");

    let m2 = q.matches_for_event(r#"{"b": "dexef"}"#.as_bytes()).unwrap();
    assert!(m2.contains(&"infix_ef"), "d*f should match dexef");

    let m3 = q.matches_for_event(r#"{"c": "xyzzz"}"#.as_bytes()).unwrap();
    assert!(m3.contains(&"prefix_xy"), "xy* should match xyzzz");
}

// ============================================================================
// Anything-But Operator Tests
// ============================================================================

#[test]
fn test_anything_but() {
    let mut q = Quamina::new();
    q.add_pattern(
        "p1",
        r#"{"status": [{"anything-but": ["deleted", "archived"]}]}"#,
    )
    .unwrap();

    let matches = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match non-excluded value");

    let no_match = q
        .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty(), "Should not match excluded value");
}

#[test]
fn test_anything_but_validation() {
    let mut q = Quamina::new();
    let result = q.add_pattern("p1", r#"{"status": [{"anything-but": []}]}"#);
    assert!(
        result.is_err(),
        "Empty anything-but array should be rejected"
    );

    let mut q2 = Quamina::new();
    let result2 = q2.add_pattern("p2", r#"{"x": [{"anything-but": [true, null]}]}"#);
    assert!(
        result2.is_err(),
        "anything-but with only booleans/nulls should be rejected"
    );

    let mut q3 = Quamina::new();
    let result3 = q3.add_pattern("p3", r#"{"x": [{"anything-but": ["a", 1]}]}"#);
    assert!(
        result3.is_err(),
        "anything-but with mixed strings and numbers should be rejected"
    );
}

#[test]
fn test_anything_but_single_string() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": [{"anything-but": "deleted"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"]);

    let m2 = q
        .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
        .unwrap();
    assert!(m2.is_empty());
}

#[test]
fn test_anything_but_numeric() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"code": [{"anything-but": 404}]}"#)
        .unwrap();

    let m1 = q.matches_for_event(r#"{"code": 200}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["p1"], "Should match non-excluded number");

    let m2 = q.matches_for_event(r#"{"code": 404}"#.as_bytes()).unwrap();
    assert!(m2.is_empty(), "Should not match excluded number");
}

#[test]
fn test_anything_but_prefix_relationship() {
    // Tests that anything-but ["foo"] matches "foot" (since "foot" != "foo")
    let mut q = Quamina::new();
    q.add_pattern("not_foo", r#"{"z": [{"anything-but": ["foo"]}]}"#)
        .unwrap();

    let m1 = q.matches_for_event(r#"{"z": "foot"}"#.as_bytes()).unwrap();
    assert_eq!(
        m1,
        vec!["not_foo"],
        "anything-but ['foo'] should match 'foot'"
    );

    let m2 = q.matches_for_event(r#"{"z": "foo"}"#.as_bytes()).unwrap();
    assert!(m2.is_empty(), "anything-but ['foo'] should not match 'foo'");
}

#[test]
fn test_anything_but_with_exact_match() {
    let mut q = Quamina::new();
    q.add_pattern("pFoo", r#"{"z": ["foo"]}"#).unwrap();
    q.add_pattern("pAbFoot", r#"{"z": [{"anything-but": ["foot"]}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"z": "foo"}"#.as_bytes()).unwrap();
    assert_eq!(matches.len(), 2, "foo should match both patterns");

    let matches2 = q.matches_for_event(r#"{"z": "foot"}"#.as_bytes()).unwrap();
    assert!(matches2.is_empty(), "foot should match nothing");
}

// ============================================================================
// Equals-Ignore-Case Tests
// ============================================================================

#[test]
fn test_equals_ignore_case() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"name": [{"equals-ignore-case": "Test"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"name": "test"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"], "Should match lowercase");

    let m2 = q
        .matches_for_event(r#"{"name": "TEST"}"#.as_bytes())
        .unwrap();
    assert_eq!(m2, vec!["p1"], "Should match uppercase");

    let m3 = q
        .matches_for_event(r#"{"name": "TeSt"}"#.as_bytes())
        .unwrap();
    assert_eq!(m3, vec!["p1"], "Should match mixed case");

    let no_match = q
        .matches_for_event(r#"{"name": "other"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_equals_ignore_case_multiple_patterns() {
    let mut q = Quamina::new();
    q.add_pattern("r1", r#"{"a": [{"equals-ignore-case": "aBc"}]}"#)
        .unwrap();
    q.add_pattern("r2", r#"{"b": [{"equals-ignore-case": "XyZ"}]}"#)
        .unwrap();
    q.add_pattern("r3", r#"{"b": [{"equals-ignore-case": "xyZ"}]}"#)
        .unwrap();

    let m1 = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["r1"]);

    let m2 = q.matches_for_event(r#"{"b": "XYZ"}"#.as_bytes()).unwrap();
    assert_eq!(m2.len(), 2, "Both r2 and r3 should match XYZ");
}

#[test]
fn test_equals_ignore_case_unicode() {
    // Test Greek sigma case folding
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"word": [{"equals-ignore-case": "Σοφία"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"word": "σοφία"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"], "Greek sigma case folding should work");

    let m2 = q
        .matches_for_event(r#"{"word": "ΣΟΦΊΑ"}"#.as_bytes())
        .unwrap();
    assert_eq!(m2, vec!["p1"], "Greek uppercase should match");
}

// ============================================================================
// Numeric Comparison Tests
// ============================================================================

#[test]
fn test_numeric_greater_than() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"age": [{"numeric": [">", 18]}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"age": 25}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let no_match = q.matches_for_event(r#"{"age": 18}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty());

    let no_match2 = q.matches_for_event(r#"{"age": 15}"#.as_bytes()).unwrap();
    assert!(no_match2.is_empty());
}

#[test]
fn test_numeric_range() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"score": [{"numeric": [">=", 0, "<=", 100]}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"score": 50}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let edge1 = q.matches_for_event(r#"{"score": 0}"#.as_bytes()).unwrap();
    assert_eq!(edge1, vec!["p1"]);

    let edge2 = q.matches_for_event(r#"{"score": 100}"#.as_bytes()).unwrap();
    assert_eq!(edge2, vec!["p1"]);

    let no_match = q.matches_for_event(r#"{"score": 101}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_numeric_equals() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"count": [{"numeric": ["=", 42]}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"count": 42}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"]);

    let no_match = q.matches_for_event(r#"{"count": 43}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_numeric_scientific_notation() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"value": [{"numeric": [">=", 300, "<=", 400]}]}"#)
        .unwrap();

    let m1 = q.matches_for_event(r#"{"value": 350}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["p1"], "Integer 350 should match");

    let m2 = q
        .matches_for_event(r#"{"value": 3.5e2}"#.as_bytes())
        .unwrap();
    assert_eq!(m2, vec!["p1"], "Scientific 3.5e2 should match");
}

// ============================================================================
// Regex Tests
// ============================================================================

#[test]
fn test_regex_match() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"code": [{"regex": "[A-Z]{3}-[0-9]{3}"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"code": "ABC-123"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"]);

    let no_match = q
        .matches_for_event(r#"{"code": "invalid"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_regex_with_escape() {
    let mut q = Quamina::new();
    // I-Regexp uses ~ as escape, not \
    q.add_pattern("p1", r#"{"email": [{"regex": "[a-z]+@example~.com"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"email": "alice@example.com"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"]);

    let no_match = q
        .matches_for_event(r#"{"email": "alice@exampleXcom"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
}

#[test]
fn test_regex_alternation() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"a": [{"regex": "a|b"}]}"#).unwrap();

    assert!(q
        .matches_for_event(r#"{"a": "a"}"#.as_bytes())
        .unwrap()
        .contains(&"p1"));
    assert!(q
        .matches_for_event(r#"{"a": "b"}"#.as_bytes())
        .unwrap()
        .contains(&"p1"));
    assert!(q
        .matches_for_event(r#"{"a": "c"}"#.as_bytes())
        .unwrap()
        .is_empty());
}

#[test]
fn test_regex_character_class() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"a": [{"regex": "[hij]"}]}"#)
        .unwrap();

    assert!(q
        .matches_for_event(r#"{"a": "h"}"#.as_bytes())
        .unwrap()
        .contains(&"p1"));
    assert!(q
        .matches_for_event(r#"{"a": "i"}"#.as_bytes())
        .unwrap()
        .contains(&"p1"));
    assert!(q
        .matches_for_event(r#"{"a": "x"}"#.as_bytes())
        .unwrap()
        .is_empty());
}

#[test]
fn test_regexp_simple_optional() {
    let mut q = Quamina::new();
    q.add_pattern("test", r#"{"a": [{"regexp": "a?b"}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"a": "ab"}"#.as_bytes()).unwrap();
    assert!(matches.contains(&"test"), "'a?b' should match 'ab'");

    let matches = q.matches_for_event(r#"{"a": "b"}"#.as_bytes()).unwrap();
    assert!(matches.contains(&"test"), "'a?b' should match 'b'");

    let matches = q.matches_for_event(r#"{"a": "aab"}"#.as_bytes()).unwrap();
    assert!(matches.is_empty(), "'a?b' should NOT match 'aab'");
}

#[test]
fn test_empty_regex_matches_empty_string() {
    let mut q = Quamina::new();
    q.add_pattern("a", r#"{"a": [{"regex": ""}]}"#).unwrap();

    let matches = q.matches_for_event(r#"{"a": ""}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["a"], "empty regex should match empty string");

    let matches2 = q.matches_for_event(r#"{"a": "hello"}"#.as_bytes()).unwrap();
    assert!(
        matches2.is_empty(),
        "empty regex should NOT match non-empty string"
    );
}

// ============================================================================
// CIDR Matching Tests
// ============================================================================

// MIRI SKIP RATIONALE: CIDR matching involves IP parsing and automaton traversal that
// is slow under Miri interpretation.
#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_ipv4_basic() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"sourceIP": [{"cidr": "10.0.0.0/24"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"sourceIP": "10.0.0.1"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"], "10.0.0.1 should match 10.0.0.0/24");

    let m2 = q
        .matches_for_event(r#"{"sourceIP": "10.0.0.255"}"#.as_bytes())
        .unwrap();
    assert_eq!(m2, vec!["p1"], "10.0.0.255 should match 10.0.0.0/24");

    let m3 = q
        .matches_for_event(r#"{"sourceIP": "10.0.1.1"}"#.as_bytes())
        .unwrap();
    assert!(m3.is_empty(), "10.0.1.1 should NOT match 10.0.0.0/24");
}

#[test]
fn test_cidr_miri_lightweight() {
    // Lightweight CIDR test for Miri
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"sourceIP": [{"cidr": "10.0.0.0/8"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"sourceIP": "10.1.2.3"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"], "10.1.2.3 should match 10.0.0.0/8");

    let m2 = q
        .matches_for_event(r#"{"sourceIP": "192.168.1.1"}"#.as_bytes())
        .unwrap();
    assert!(m2.is_empty(), "192.168.1.1 should NOT match 10.0.0.0/8");
}

#[test]
fn test_cidr_invalid_patterns() {
    let mut q = Quamina::new();

    let result = q.add_pattern("p1", r#"{"ip": [{"cidr": "not-an-ip/24"}]}"#);
    assert!(result.is_err(), "Invalid IP should be rejected");

    let result = q.add_pattern("p2", r#"{"ip": [{"cidr": "10.0.0.0/33"}]}"#);
    assert!(result.is_err(), "Invalid prefix length should be rejected");
}

// ============================================================================
// Lookaround Tests
// ============================================================================

#[test]
fn test_lookaround_pattern_add_to_quamina() {
    let mut q = Quamina::<String>::new();

    // Add a lookahead pattern
    let pattern = r#"{"status": [{"regexp": "foo(?=bar)"}]}"#;
    let result = q.add_pattern("test".to_string(), pattern);
    assert!(
        result.is_ok(),
        "Lookahead pattern should be accepted: {:?}",
        result.err()
    );
}

#[test]
fn test_positive_lookahead_match() {
    // foo(?=bar) matches the "foo" in "foobar" (zero-width, so only matches "foo" position)
    let mut q = Quamina::<String>::new();
    let pattern = r#"{"status": [{"regexp": "foo(?=bar)bar"}]}"#;
    q.add_pattern("test".to_string(), pattern).unwrap();

    let event = r#"{"status": "foobar"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        matches.contains(&"test".to_string()),
        "foo(?=bar)bar should match 'foobar'"
    );

    let event = r#"{"status": "foobaz"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        !matches.contains(&"test".to_string()),
        "foo(?=bar)bar should NOT match 'foobaz'"
    );
}

#[test]
fn test_negative_lookahead_match() {
    // foo(?!bar) matches "foo" not followed by "bar"
    let mut q = Quamina::<String>::new();
    let pattern = r#"{"status": [{"regexp": "foo(?!bar)..."}]}"#;
    q.add_pattern("test".to_string(), pattern).unwrap();

    let event = r#"{"status": "foobaz"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        matches.contains(&"test".to_string()),
        "foo(?!bar)... should match 'foobaz'"
    );

    let event = r#"{"status": "foobar"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        !matches.contains(&"test".to_string()),
        "foo(?!bar)... should NOT match 'foobar'"
    );
}

#[test]
fn test_lookbehind_match() {
    // (?<=foo)bar matches "bar" preceded by "foo"
    let mut q = Quamina::<String>::new();
    let pattern = r#"{"status": [{"regexp": "(?<=foo)bar"}]}"#;
    q.add_pattern("test".to_string(), pattern).unwrap();

    let event = r#"{"status": "foobar"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        matches.contains(&"test".to_string()),
        "(?<=foo)bar should match 'foobar'"
    );

    let event = r#"{"status": "xxxbar"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        !matches.contains(&"test".to_string()),
        "(?<=foo)bar should NOT match 'xxxbar'"
    );
}

#[test]
fn test_negative_lookbehind_match() {
    // (?<!foo)bar matches "bar" not preceded by "foo"
    let mut q = Quamina::<String>::new();
    let pattern = r#"{"status": [{"regexp": "(?<!foo)bar"}]}"#;
    q.add_pattern("test".to_string(), pattern).unwrap();

    let event = r#"{"status": "xxxbar"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        matches.contains(&"test".to_string()),
        "(?<!foo)bar should match 'xxxbar'"
    );

    let event = r#"{"status": "foobar"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        !matches.contains(&"test".to_string()),
        "(?<!foo)bar should NOT match 'foobar'"
    );
}

// ============================================================================
// JSON Escape Sequences Tests
// ============================================================================

#[test]
fn test_json_escape_sequences() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"msg": ["line1\nline2"]}"#).unwrap();

    let event = r#"{"msg": "line1\nline2"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(matches, vec!["p1"], "Should match \\n escape sequence");
}

#[test]
fn test_unicode_escape_in_event() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"greeting": ["Hello"]}"#).unwrap();

    let event = r#"{"greeting": "\u0048\u0065\u006c\u006c\u006f"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches,
        vec!["p1"],
        "Unicode escape should decode to 'Hello'"
    );
}

#[test]
fn test_unicode_escape_emoji() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"emoji": ["💋"]}"#).unwrap();

    let event = r#"{"emoji": "\ud83d\udc8b"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(
        matches,
        vec!["p1"],
        "UTF-16 surrogate pair should decode to emoji"
    );
}

// ============================================================================
// Exists False Ordering Tests
// ============================================================================

#[test]
fn test_exists_false_ordering() {
    let event = r#"{"aField": "a", "bField": "b", "cField": "c"}"#;

    // All these patterns should NOT match because each requires a field to be absent
    let should_not_patterns = [
        r#"{"aField": ["a"], "bField": [{"exists": false}], "cField": ["c"]}"#,
        r#"{"aField": [{"exists": false}], "bField": ["b"], "cField": ["c"]}"#,
        r#"{"aField": ["a"], "bField": ["b"], "cField": [{"exists": false}]}"#,
    ];

    for (i, pattern) in should_not_patterns.iter().enumerate() {
        let mut q = Quamina::new();
        q.add_pattern(format!("p{}", i), pattern).unwrap();
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.is_empty(),
            "Pattern {} should NOT match: {}",
            i,
            pattern
        );
    }
}

// ============================================================================
// Overlapping Values Tests
// ============================================================================

#[test]
fn test_overlapping_exact_match_patterns() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"a": ["foo"]}"#).unwrap();
    q.add_pattern("p2", r#"{"a": ["football"]}"#).unwrap();
    q.add_pattern("p3", r#"{"a": ["footballer"]}"#).unwrap();

    let matches1 = q
        .matches_for_event(r#"{"x": 3, "a": "foo"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches1, vec!["p1"], "foo should only match p1");

    let matches2 = q
        .matches_for_event(r#"{"x": 3, "a": "football"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches2, vec!["p2"], "football should only match p2");

    let matches3 = q
        .matches_for_event(r#"{"x": 3, "a": "footballer"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches3, vec!["p3"], "footballer should only match p3");

    let no_match = q.matches_for_event(r#"{"a": "foot"}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty(), "foot should not match any pattern");
}

// ============================================================================
// Additional Operator Tests (recovered from original)
// ============================================================================

#[test]
fn test_anything_but_numeric_single() {
    // Test single number: {"anything-but": 123}
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"code": [{"anything-but": 404}]}"#)
        .unwrap();

    // Should match non-excluded numbers
    let m1 = q.matches_for_event(r#"{"code": 200}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["p1"], "Should match non-excluded number");

    let m2 = q.matches_for_event(r#"{"code": 404}"#.as_bytes()).unwrap();
    assert!(m2.is_empty(), "Should not match excluded number");

    // Non-numeric string doesn't match excluded number, so passes
    let m3 = q
        .matches_for_event(r#"{"code": "not-a-number"}"#.as_bytes())
        .unwrap();
    assert_eq!(
        m3,
        vec!["p1"],
        "Non-numeric value passes numeric anything-but"
    );
}

#[test]
fn test_anything_but_numeric_array() {
    // Test array of numbers: {"anything-but": [100, 200, 300]}
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"code": [{"anything-but": [400, 404, 500]}]}"#)
        .unwrap();

    // Should match non-excluded numbers
    let m1 = q.matches_for_event(r#"{"code": 200}"#.as_bytes()).unwrap();
    assert_eq!(m1, vec!["p1"], "Should match non-excluded number");

    let m2 = q.matches_for_event(r#"{"code": 404}"#.as_bytes()).unwrap();
    assert!(m2.is_empty(), "Should not match excluded number");

    let m3 = q.matches_for_event(r#"{"code": 500}"#.as_bytes()).unwrap();
    assert!(m3.is_empty(), "Should not match another excluded number");
}

#[test]
fn test_anything_but_numeric_float() {
    // Test with floating point numbers
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"price": [{"anything-but": [9.99, 19.99]}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"price": 14.99}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"], "Should match non-excluded float");

    let m2 = q
        .matches_for_event(r#"{"price": 9.99}"#.as_bytes())
        .unwrap();
    assert!(m2.is_empty(), "Should not match excluded float");
}

#[test]
fn test_equals_ignore_case_with_exact_match() {
    // Based on Go's TestSingletonMonocaseMerge (monocase_test.go:48)
    // Tests that exact match and equals-ignore-case patterns can coexist and merge correctly
    let mut q = Quamina::new();

    // Add exact match pattern
    q.add_pattern("singleton", r#"{"x": ["singleton"]}"#)
        .unwrap();

    // Add equals-ignore-case pattern on same field
    q.add_pattern("mono", r#"{"x": [{"equals-ignore-case": "foo"}]}"#)
        .unwrap();

    // Exact match should work
    let m1 = q
        .matches_for_event(r#"{"x": "singleton"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["singleton"], "Exact match should work");

    // Case-insensitive match should work
    let m2 = q.matches_for_event(r#"{"x": "FoO"}"#.as_bytes()).unwrap();
    assert_eq!(m2, vec!["mono"], "Case-insensitive match should work");

    // Neither should match different values
    let m3 = q.matches_for_event(r#"{"x": "bar"}"#.as_bytes()).unwrap();
    assert!(m3.is_empty(), "Unrelated value should not match");
}

#[test]
fn test_regex_various_patterns() {
    // Based on Go quamina's TestRegexpEnd2End
    // Test various regex patterns for correctness

    // Alternation
    let mut q1 = Quamina::new();
    q1.add_pattern("p1", r#"{"a": [{"regex": "a|b"}]}"#)
        .unwrap();
    assert!(q1
        .matches_for_event(r#"{"a": "a"}"#.as_bytes())
        .unwrap()
        .contains(&"p1"));
    assert!(q1
        .matches_for_event(r#"{"a": "b"}"#.as_bytes())
        .unwrap()
        .contains(&"p1"));
    assert!(q1
        .matches_for_event(r#"{"a": "c"}"#.as_bytes())
        .unwrap()
        .is_empty());

    // Character class
    let mut q2 = Quamina::new();
    q2.add_pattern("p2", r#"{"a": [{"regex": "[hij]"}]}"#)
        .unwrap();
    assert!(q2
        .matches_for_event(r#"{"a": "h"}"#.as_bytes())
        .unwrap()
        .contains(&"p2"));
    assert!(q2
        .matches_for_event(r#"{"a": "i"}"#.as_bytes())
        .unwrap()
        .contains(&"p2"));
    assert!(q2
        .matches_for_event(r#"{"a": "j"}"#.as_bytes())
        .unwrap()
        .contains(&"p2"));
    assert!(q2
        .matches_for_event(r#"{"a": "x"}"#.as_bytes())
        .unwrap()
        .is_empty());

    // Character range
    let mut q3 = Quamina::new();
    q3.add_pattern("p3", r#"{"a": [{"regex": "a[e-g]x"}]}"#)
        .unwrap();
    assert!(q3
        .matches_for_event(r#"{"a": "aex"}"#.as_bytes())
        .unwrap()
        .contains(&"p3"));
    assert!(q3
        .matches_for_event(r#"{"a": "afx"}"#.as_bytes())
        .unwrap()
        .contains(&"p3"));
    assert!(q3
        .matches_for_event(r#"{"a": "agx"}"#.as_bytes())
        .unwrap()
        .contains(&"p3"));
    assert!(q3
        .matches_for_event(r#"{"a": "ax"}"#.as_bytes())
        .unwrap()
        .is_empty());

    // Ordinal suffix pattern (like 11th, 23rd)
    let mut q4 = Quamina::new();
    q4.add_pattern("p4", r#"{"a": [{"regex": "[0-9][0-9][rtn][dh]"}]}"#)
        .unwrap();
    assert!(q4
        .matches_for_event(r#"{"a": "11th"}"#.as_bytes())
        .unwrap()
        .contains(&"p4"));
    assert!(q4
        .matches_for_event(r#"{"a": "23rd"}"#.as_bytes())
        .unwrap()
        .contains(&"p4"));
    assert!(q4
        .matches_for_event(r#"{"a": "22nd"}"#.as_bytes())
        .unwrap()
        .contains(&"p4"));
    assert!(q4
        .matches_for_event(r#"{"a": "first"}"#.as_bytes())
        .unwrap()
        .is_empty());
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_regexp_end2end() {
    // Comprehensive regexp tests ported from Go's TestRegexpEnd2End
    use crate::regexp_samples::RegexpSample;

    let tests = [
        RegexpSample {
            regex: "(xyz)?a?b",
            matches: &["xyzb", "xyzab", "ab", "b"],
            nomatches: &["xyzc", "c", "xyza"],
            valid: true,
        },
        RegexpSample {
            regex: "a|b",
            matches: &["a", "b"],
            nomatches: &["x"],
            valid: true,
        },
        RegexpSample {
            regex: "a",
            matches: &["a"],
            nomatches: &["b", ""],
            valid: true,
        },
        RegexpSample {
            regex: "a.b",
            matches: &["axb", "a.b"],
            nomatches: &["ab", "axxb"],
            valid: true,
        },
        RegexpSample {
            regex: "abc|def",
            matches: &["abc", "def"],
            nomatches: &["x"],
            valid: true,
        },
        RegexpSample {
            regex: "[hij]",
            matches: &["h", "i", "j"],
            nomatches: &["x"],
            valid: true,
        },
        RegexpSample {
            regex: "a[e-g]x",
            matches: &["aex", "afx", "agx"],
            nomatches: &["ax", "axx"],
            valid: true,
        },
        RegexpSample {
            regex: "[ae-gx]",
            matches: &["a", "e", "f", "g", "x"],
            nomatches: &["b"],
            valid: true,
        },
        RegexpSample {
            regex: "[-ab]",
            matches: &["-", "a", "b"],
            nomatches: &["c"],
            valid: true,
        },
        RegexpSample {
            regex: "[ab-]",
            matches: &["-", "a", "b"],
            nomatches: &["c"],
            valid: true,
        },
        RegexpSample {
            regex: "[~[~]]",
            matches: &["[", "]"],
            nomatches: &["a"],
            valid: true,
        },
        // Note: Go tests [~r~t~n] matching \r, \t, \n literal bytes
        // In JSON these would be escaped, so we test differently
        RegexpSample {
            regex: "[a-c]|[xz]",
            matches: &["a", "b", "c", "x", "z"],
            nomatches: &["w"],
            valid: true,
        },
        RegexpSample {
            regex: "[ac-e]h|p[xy]",
            matches: &["ah", "ch", "dh", "eh", "px", "py"],
            nomatches: &["xp"],
            valid: true,
        },
        RegexpSample {
            regex: "[0-9][0-9][rtn][dh]",
            matches: &["11th", "23rd", "22nd"],
            nomatches: &["first", "9th"],
            valid: true,
        },
        RegexpSample {
            regex: "a(h|i)z",
            matches: &["ahz", "aiz"],
            nomatches: &["a.z"],
            valid: true,
        },
        RegexpSample {
            regex: "a([1-3]|ac)z",
            matches: &["a1z", "a2z", "a3z", "aacz"],
            nomatches: &["a.z", "a0z"],
            valid: true,
        },
        RegexpSample {
            regex: "a(h|([x-z]|(1|2)))z",
            matches: &["ahz", "axz", "a1z", "a2z"],
            nomatches: &["a.z"],
            valid: true,
        },
    ];

    // Test each pattern individually
    for test in &tests {
        let mut q = Quamina::new();
        let pattern = format!(r#"{{"a": [{{"regexp": "{}"}}]}}"#, test.regex);
        if let Err(e) = q.add_pattern("test", &pattern) {
            panic!("Failed to add pattern '{}': {}", test.regex, e);
        }

        for m in test.matches {
            let event = format!(r#"{{"a": "{}"}}"#, m);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.contains(&"test"),
                "Pattern '{}' should match '{}', but didn't",
                test.regex,
                m
            );
        }

        for m in test.nomatches {
            let event = format!(r#"{{"a": "{}"}}"#, m);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.is_empty(),
                "Pattern '{}' should NOT match '{}', but did",
                test.regex,
                m
            );
        }
    }

    // Test merged FA (all patterns together) - like Go does
    let mut all_patterns = Quamina::new();
    for (i, test) in tests.iter().enumerate() {
        let pattern = format!(r#"{{"a": [{{"regexp": "{}"}}]}}"#, test.regex);
        let name = format!("p{}", i);
        if let Err(e) = all_patterns.add_pattern(name, &pattern) {
            panic!("Failed to add pattern '{}': {}", test.regex, e);
        }
    }

    for (i, test) in tests.iter().enumerate() {
        let expected_name = format!("p{}", i);
        for m in test.matches {
            let event = format!(r#"{{"a": "{}"}}"#, m);
            let matches = all_patterns.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.contains(&expected_name),
                "Merged FA: Pattern '{}' should match '{}', but didn't",
                test.regex,
                m
            );
        }
    }
}

#[test]
fn test_shellstyle_long_case() {
    // Test the "abab" suffix case from Go
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"shellstyle": "*abab"}]}"#)
        .unwrap();

    for text in ["abaabab", "ababab", "ababaabab", "abab"] {
        let event = format!(r#"{{"x": "{}"}}"#, text);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match {}", text);
    }
}

#[test]
fn test_multiple_shellstyle_same_field() {
    // Test multiple shellstyle patterns on the SAME field
    // This is the merge_fas spinout bug reproduction test
    let mut q = Quamina::new();

    // Add multiple shellstyle patterns on the same field
    q.add_pattern("suffix_bc", r#"{"x": [{"shellstyle": "*bc"}]}"#)
        .unwrap();
    q.add_pattern("suffix_xc", r#"{"x": [{"shellstyle": "*xc"}]}"#)
        .unwrap();
    q.add_pattern("prefix_ab", r#"{"x": [{"shellstyle": "ab*"}]}"#)
        .unwrap();

    // Test suffix_bc pattern
    let m1 = q.matches_for_event(r#"{"x": "abc"}"#.as_bytes()).unwrap();
    assert!(
        m1.contains(&"suffix_bc"),
        "*bc should match abc, got: {:?}",
        m1
    );
    // abc also matches ab* prefix pattern
    assert!(
        m1.contains(&"prefix_ab"),
        "ab* should match abc, got: {:?}",
        m1
    );

    // Test suffix_xc pattern
    let m2 = q.matches_for_event(r#"{"x": "axc"}"#.as_bytes()).unwrap();
    assert!(
        m2.contains(&"suffix_xc"),
        "*xc should match axc, got: {:?}",
        m2
    );

    // Test prefix_ab pattern
    let m3 = q
        .matches_for_event(r#"{"x": "abcdef"}"#.as_bytes())
        .unwrap();
    assert!(
        m3.contains(&"prefix_ab"),
        "ab* should match abcdef, got: {:?}",
        m3
    );

    // Test non-match
    let m4 = q.matches_for_event(r#"{"x": "xyz"}"#.as_bytes()).unwrap();
    assert!(m4.is_empty(), "Nothing should match xyz, got: {:?}", m4);
}

#[test]
fn test_anything_but_with_shellstyle() {
    // Based on Go quamina's TestAnythingButMerging (second part)
    // Tests that anything-but can be merged with shellstyle (NFA) patterns
    let mut q = Quamina::new();

    // Add shellstyle pattern for "foo*"
    q.add_pattern("pFooStar", r#"{"z": [{"shellstyle": "foo*"}]}"#)
        .unwrap();
    // Add anything-but for "foot"
    q.add_pattern("pAbFoot", r#"{"z": [{"anything-but": ["foot"]}]}"#)
        .unwrap();

    // "foo" should match BOTH patterns:
    // - pFooStar: matches "foo*"
    // - pAbFoot: "foo" is not "foot"
    let matches = q.matches_for_event(r#"{"z": "foo"}"#.as_bytes()).unwrap();
    assert_eq!(
        matches.len(),
        2,
        "foo should match both pFooStar and pAbFoot, got {:?}",
        matches
    );
    assert!(matches.contains(&"pFooStar"));
    assert!(matches.contains(&"pAbFoot"));

    // "foot" should match only pFooStar:
    // - pFooStar: matches "foo*"
    // - pAbFoot: excluded (is "foot")
    let matches2 = q.matches_for_event(r#"{"z": "foot"}"#.as_bytes()).unwrap();
    assert_eq!(
        matches2.len(),
        1,
        "foot should only match pFooStar, got {:?}",
        matches2
    );
    assert!(matches2.contains(&"pFooStar"));

    // "bar" should match only pAbFoot:
    // - pFooStar: doesn't match "foo*"
    // - pAbFoot: "bar" is not "foot"
    let matches3 = q.matches_for_event(r#"{"z": "bar"}"#.as_bytes()).unwrap();
    assert_eq!(
        matches3.len(),
        1,
        "bar should only match pAbFoot, got {:?}",
        matches3
    );
    assert!(matches3.contains(&"pAbFoot"));
}

#[test]
fn test_anything_but_with_overlapping_exclusions() {
    // Based on Go quamina's TestAnythingButAlgo
    // Tests anything-but with overlapping prefix exclusions
    let mut q = Quamina::new();
    q.add_pattern(
        "notTTT",
        r#"{"x": [{"anything-but": ["tim", "time", "timed"]}]}"#,
    )
    .unwrap();

    // All excluded values should not match
    let excluded = ["tim", "time", "timed"];
    for val in excluded {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(matches.is_empty(), "{} should be excluded", val);
    }

    // Similar but non-excluded values should match
    let included = ["t", "ti", "timer", "timely", "timekeeper"];
    for val in included {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches.len(), 1, "{} should match notTTT", val);
    }
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_anything_but_wordle_words() {
    // Based on Go quamina's TestAnythingButMatching (anything_but_test.go:150)
    // Tests anything-but against wordle word list with edge case "problem words"
    use std::fs;
    use std::path::Path;

    // Problem words designed to test edge cases:
    // - 4-letter prefix of existing wordle
    // - 4-letter suffix of existing wordle
    // - 5-letter non-wordle
    // - 6-letter where wordle might match at start or end
    let problem_words = ["bloo", "aper", "fnord", "doubts", "astern"];

    let mut q = Quamina::new();
    // Build pattern with quoted problem words for JSON array
    let problem_json: Vec<String> = problem_words.iter().map(|w| format!("\"{}\"", w)).collect();
    let pattern = format!(
        r#"{{"a": [{{"anything-but": [{}]}}]}}"#,
        problem_json.join(",")
    );
    q.add_pattern("not_problems", &pattern).unwrap();

    // Problem words should NOT match (they're excluded)
    for word in &problem_words {
        let event = format!(r#"{{"a": "{}"}}"#, word);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.is_empty(),
            "Problem word '{}' should be excluded",
            word
        );
    }

    // All wordle words should match (they're not in the exclusion list)
    let wwords_path = Path::new("testdata/wwords.txt");
    if wwords_path.exists() {
        let contents = fs::read_to_string(wwords_path).unwrap();
        for word in contents.lines() {
            let word = word.trim();
            if word.is_empty() {
                continue;
            }
            let event = format!(r#"{{"a": "{}"}}"#, word);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(
                matches.len(),
                1,
                "Wordle word '{}' should match anything-but pattern",
                word
            );
        }
    }
}

#[test]
fn test_shellstyle_repeated_sequences() {
    // Based on Go quamina's TestLongCase
    // Tests shellstyle suffix patterns with overlapping sequences
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"shellstyle": "*abab"}]}"#)
        .unwrap();

    // These should all match *abab
    let should_match = ["abab", "abaabab", "ababab", "ababaabab", "xxabab"];
    for val in should_match {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "*abab should match '{}'", val);
    }

    // These should not match
    let should_not = ["abab_", "aba", "ab", "xaba"];
    for val in should_not {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(matches.is_empty(), "*abab should NOT match '{}'", val);
    }
}

#[test]
fn test_shellstyle_suffix_merged_bug() {
    // Based on Go quamina's TestSuffixBug
    // Tests that multiple merged suffix patterns all match properly
    let j = r#"{"Url": "xy9"}"#;
    let patterns = [
        (r#"{"Url": [{"shellstyle": "*9"}]}"#, "p0"),
        (r#"{"Url": [{"shellstyle": "x*9"}]}"#, "p1"),
    ];

    // Verify each pattern works individually
    for (pattern, name) in &patterns {
        let mut q = Quamina::new();
        q.add_pattern(*name, pattern).unwrap();
        let matches = q.matches_for_event(j.as_bytes()).unwrap();
        assert_eq!(
            matches.len(),
            1,
            "Pattern {} should match individually",
            name
        );
        assert!(matches.contains(name));
    }

    // Verify both patterns work when merged
    let mut q = Quamina::new();
    for (pattern, name) in &patterns {
        q.add_pattern(*name, pattern).unwrap();
    }
    let matches = q.matches_for_event(j.as_bytes()).unwrap();
    assert_eq!(
        matches.len(),
        2,
        "Both patterns should match when merged, got {:?}",
        matches
    );
    assert!(matches.contains(&"p0"));
    assert!(matches.contains(&"p1"));
}

#[test]
fn test_shellstyle_complex_wildcards() {
    // Based on Go quamina's TestMakeShellStyleFA
    // Tests shellstyle patterns with multiple wildcards in complex positions
    let test_cases = [
        // Pattern with two wildcards
        (
            r#"{"x": [{"shellstyle": "xx*yy*zz"}]}"#,
            vec!["xxabyycdzz", "xxyyzz", "xxyyzzzzz"],
            vec!["xyzyxzy yy zz", "zz yy xx"],
        ),
        // Pattern with wildcards at both ends
        (
            r#"{"x": [{"shellstyle": "*xx*yy*"}]}"#,
            vec!["xxyy", "xxyyef", "abxxyy", "abxxcdyy"],
            vec!["ayybyyzxx", "xyzzy"],
        ),
    ];

    for (pattern, should_match, should_not) in test_cases {
        let mut q = Quamina::new();
        q.add_pattern("p1", pattern).unwrap();

        for val in should_match {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "{} should match '{}'", pattern, val);
        }

        for val in should_not {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(matches.is_empty(), "{} should NOT match '{}'", pattern, val);
        }
    }
}

#[test]
fn test_wildcard_comprehensive() {
    // Based on Go quamina's exercisePattern tests from wildcard_test.go
    // Tests wildcard patterns (which support escaping, unlike shellstyle)

    // Helper to run wildcard pattern tests
    fn exercise_wildcard(pattern: &str, should_match: &[&str], should_not_match: &[&str]) {
        let mut q = Quamina::new();
        let full_pattern = format!(r#"{{"x": [{{"wildcard": "{}"}}]}}"#, pattern);
        q.add_pattern(pattern, &full_pattern)
            .unwrap_or_else(|_| panic!("Pattern should be valid: {}", pattern));

        for text in should_match {
            let event = format!(r#"{{"x": "{}"}}"#, text);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.contains(&pattern),
                "Pattern '{}' should match '{}', got {:?}",
                pattern,
                text,
                matches
            );
        }

        for text in should_not_match {
            let event = format!(r#"{{"x": "{}"}}"#, text);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                !matches.contains(&pattern),
                "Pattern '{}' should NOT match '{}'",
                pattern,
                text
            );
        }
    }

    // Test * (matches everything)
    exercise_wildcard("*", &["", "*", "h", "hello"], &[]);

    // Test *hello (suffix matching)
    exercise_wildcard(
        "*hello",
        &["hello", "hhello", "xxxhello", "*hello"],
        &["", "ello", "hellx", "xhellx"],
    );

    // Test h*llo (infix matching)
    exercise_wildcard(
        "h*llo",
        &["hllo", "hello", "hxxxllo"],
        &["", "hlo", "hll", "hellol"],
    );

    // Test hel*o
    exercise_wildcard(
        "hel*o",
        &["helo", "hello", "helxxxo"],
        &["", "hell", "helox", "hellox"],
    );

    // Test hello* (prefix matching)
    exercise_wildcard(
        "hello*",
        &["hello", "hellox", "hellooo", "hello*"],
        &["", "hell", "hellx", "hellxo"],
    );

    // Test h*l*o (multiple wildcards)
    exercise_wildcard(
        "h*l*o",
        &["hlo", "helo", "hllo", "hloo", "hello", "hxxxlxxxo", "h*l*o"],
        &["", "ho", "heeo", "helx", "llo"],
    );

    // Test he*l*
    exercise_wildcard(
        "he*l*",
        &["hel", "hexl", "helx", "helxx", "helxl", "helxlx", "helxxl"],
        &["", "he", "hex", "hexxx"],
    );

    // Test *l* (contains l)
    exercise_wildcard(
        "*l*",
        &["l", "xl", "lx", "xlx", "xxl", "lxx", "xxlxx", "xlxlxlxlxl"],
        &["", "x", "xx", "xtx"],
    );
}

#[test]
fn test_wildcard_escape_sequences_comprehensive() {
    // Based on Go quamina's exercisePattern tests for escaping

    fn exercise_wildcard(pattern: &str, should_match: &[&str], should_not_match: &[&str]) {
        let mut q = Quamina::new();
        let full_pattern = format!(r#"{{"x": [{{"wildcard": "{}"}}]}}"#, pattern);
        q.add_pattern(pattern, &full_pattern)
            .unwrap_or_else(|_| panic!("Pattern should be valid: {}", pattern));

        for text in should_match {
            let event = format!(r#"{{"x": "{}"}}"#, text);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.contains(&pattern),
                "Pattern '{}' should match '{}', got {:?}",
                pattern,
                text,
                matches
            );
        }

        for text in should_not_match {
            let event = format!(r#"{{"x": "{}"}}"#, text);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                !matches.contains(&pattern),
                "Pattern '{}' should NOT match '{}'",
                pattern,
                text
            );
        }
    }

    // Test hel\*o (escaped star = literal *)
    // Pattern: "hel\\\\*o" -> JSON "hel\\*o" -> parsed: hel\*o
    // In wildcard: hel + \* (escaped star = literal *) + o = matches "hel*o" exactly
    // Note: event "hel*o" doesn't need escaping as * is not a JSON escape char
    exercise_wildcard("hel\\\\*o", &["hel*o"], &["helo", "hello"]);

    // Test he\**o - \* is literal *, then * is wildcard
    // Pattern: "he\\\\**o" -> JSON "he\\**o" -> parsed: he\**o
    // In wildcard: he + \* (literal *) + * (wildcard) + o
    // Matches: he*o, he*llo, he*hello (the * between he and o is literal, then wildcard *o)
    exercise_wildcard(
        "he\\\\**o",
        &["he*o", "he*llo", "he*hello"],
        &["heo", "helo"],
    );

    // Test he\\llo - matches "he\llo" (escaped backslash in pattern = literal \)
    // Pattern: "he\\\\\\\\llo" -> JSON "he\\\\llo" -> parsed: he\\llo
    // In wildcard: he + \\ (escaped backslash = literal \) + llo = matches "he\llo"
    // Event also needs JSON escaping: "he\\\\llo" -> JSON "he\\llo" -> parsed: "he\llo"
    exercise_wildcard("he\\\\\\\\llo", &["he\\\\llo"], &["hello"]);
}

#[test]
fn test_wildcard_invalid_escape_sequences() {
    // Based on Go quamina's TestWildcardInvalidEscape
    let mut q = Quamina::new();

    // Valid pattern from Go: he*\\**
    // Go raw string `he*\\**` -> JSON string "he*\\**" -> after JSON parsing: he*\**
    // In wildcard pattern: he, *, \*, * = he + wildcard + escaped_star + wildcard
    // This is valid because \* is an escaped star (literal *), not adjacent **
    // In Rust raw string, we write the exact JSON content:
    let valid_result = q.add_pattern("valid", r#"{"x": [{"wildcard": "he*\\**"}]}"#);
    assert!(
        valid_result.is_ok(),
        "he*\\** should be valid: {:?}",
        valid_result
    );

    // Invalid patterns
    let invalid_patterns = [
        (r#"{"x": [{"wildcard": "he\\llo"}]}"#, "invalid escape \\l"),
        (r#"{"x": [{"wildcard": "foo**bar"}]}"#, "adjacent **"),
        (r#"{"x": [{"wildcard": "**f"}]}"#, "leading **"),
        (r#"{"x": [{"wildcard": "x**"}]}"#, "trailing **"),
        (r#"{"x": [{"wildcard": "x\\"}]}"#, "trailing backslash"),
    ];

    for (pattern, desc) in invalid_patterns {
        let mut q2 = Quamina::new();
        let result = q2.add_pattern("p", pattern);
        assert!(result.is_err(), "{} should be rejected: {}", desc, pattern);
    }
}

#[test]
fn test_wildcard_syntax_errors() {
    // Based on Go quamina's TestWildcardSyntax
    let invalid_patterns = [
        r#"{"x": [{"wildcard": . }]}"#,    // dot instead of string
        r#"{"x": [{"wildcard": 3}]}"#,     // number instead of string
        r#"{"x": [{"wildcard": "x" ]}"#,   // missing closing brace
        r#"{"x": [{"wildcard": true}]}"#,  // boolean instead of string
        r#"{"x": [{"wildcard": null}]}"#,  // null instead of string
        r#"{"x": [{"wildcard": ["a"]}]}"#, // array instead of string
    ];

    for pattern in invalid_patterns {
        let mut q = Quamina::new();
        let result = q.add_pattern("p", pattern);
        assert!(
            result.is_err(),
            "Should reject invalid pattern: {}",
            pattern
        );
    }
}

#[test]
fn test_wildcard_multi_patterns_basic() {
    // Go line 42-45: *, h*o, exact match
    exercise_multi_patterns(
        &[],
        &[
            (
                r#"{"x":[{"wildcard": "*"}]}"#,
                &["", "*", "h", "ho", "hello"],
            ),
            (r#"{"x":[{"wildcard": "h*o"}]}"#, &["ho", "hello"]),
            (r#"{"x":["hello"]}"#, &["hello"]),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_suffix_exact() {
    // Go line 46-48
    exercise_multi_patterns(
        &["", "hellox", "blahabc"],
        &[
            (
                r#"{"x":[{"wildcard": "*hello"}]}"#,
                &["hello", "xhello", "hehello"],
            ),
            (r#"{"x":["abc"]}"#, &["abc"]),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_suffix_infix() {
    // Go line 49-51
    exercise_multi_patterns(
        &["", "h", "ello", "hel", "hlo", "hell"],
        &[
            (
                r#"{"x":[{"wildcard": "*hello"}]}"#,
                &["hello", "xhello", "hehello"],
            ),
            (
                r#"{"x":[{"wildcard": "h*llo"}]}"#,
                &["hllo", "hello", "hehello"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_suffix_infix2() {
    // Go line 52-54
    exercise_multi_patterns(
        &["", "h", "ello", "hel", "heo", "hell"],
        &[
            (
                r#"{"x":[{"wildcard": "*hello"}]}"#,
                &["hello", "xhello", "hehello"],
            ),
            (
                r#"{"x":[{"wildcard": "he*lo"}]}"#,
                &["helo", "hello", "hehello"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_suffix_double() {
    // Go line 55-57
    exercise_multi_patterns(
        &["", "e", "l", "lo", "hel"],
        &[
            (r#"{"x":[{"wildcard": "*elo"}]}"#, &["elo", "helo", "xhelo"]),
            (
                r#"{"x":[{"wildcard": "e*l*"}]}"#,
                &["el", "elo", "exl", "elx", "exlx", "exxl", "elxx", "exxlxx"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_suffix_double2() {
    // Go line 58-60
    exercise_multi_patterns(
        &["", "he", "hexxo", "ello"],
        &[
            (
                r#"{"x":[{"wildcard": "*hello"}]}"#,
                &["hello", "xhello", "xxhello"],
            ),
            (
                r#"{"x":[{"wildcard": "he*l*"}]}"#,
                &[
                    "hel", "hello", "helo", "hexl", "hexlx", "hexxl", "helxx", "hexxlxx",
                ],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_infix_pair() {
    // Go line 61-63
    exercise_multi_patterns(
        &["", "hlo", "heo", "hllol", "helol"],
        &[
            (
                r#"{"x":[{"wildcard": "h*llo"}]}"#,
                &["hllo", "hello", "hxxxllo", "hexxxllo"],
            ),
            (
                r#"{"x":[{"wildcard": "he*lo"}]}"#,
                &["helo", "hello", "hexxxlo", "hexxxllo"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_suffix_pair() {
    // Go line 64-66
    exercise_multi_patterns(
        &[
            "", "hlox", "hllo", "helo", "heox", "helx", "hellx", "helloxx", "heloxx",
        ],
        &[
            (
                r#"{"x":[{"wildcard": "h*llox"}]}"#,
                &["hllox", "hellox", "hxxxllox", "helhllox", "hheloxllox"],
            ),
            (
                r#"{"x":[{"wildcard": "hel*ox"}]}"#,
                &["helox", "hellox", "helxxxox", "helhllox", "helhlloxox"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_complex1() {
    // Go line 67-69
    exercise_multi_patterns(
        &[
            "", "h", "he", "hl", "el", "hlo", "llo", "hllol", "hxll", "hexxx",
        ],
        &[
            (
                r#"{"x":[{"wildcard": "h*llo"}]}"#,
                &["hllo", "hello", "hxxxllo", "hexxxllo", "hexxxlllo"],
            ),
            (
                r#"{"x":[{"wildcard": "he*l*"}]}"#,
                &[
                    "hel",
                    "helo",
                    "hexl",
                    "hello",
                    "helol",
                    "hexxxlo",
                    "hexxxllo",
                    "hexxxlllo",
                ],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_complex2() {
    // Go line 70-72
    exercise_multi_patterns(
        &[
            "", "h", "hex", "hl", "exl", "hxlo", "xllo", "hxllol", "hxxll", "hexxx",
        ],
        &[
            (
                r#"{"x":[{"wildcard": "h*xllo"}]}"#,
                &["hxllo", "hexllo", "hxxxllo", "hexxxllo"],
            ),
            (
                r#"{"x":[{"wildcard": "hex*l*"}]}"#,
                &[
                    "hexl",
                    "hexlo",
                    "hexxl",
                    "hexllo",
                    "hexlol",
                    "hexxxlo",
                    "hexxxllo",
                    "hexxxlllo",
                ],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_overlap1() {
    // Go line 73-75
    exercise_multi_patterns(
        &["", "hel", "heo", "hlo", "hellxox"],
        &[
            (
                r#"{"x":[{"wildcard": "he*lo"}]}"#,
                &["helo", "hello", "hexxxlo", "helxxxlo"],
            ),
            (
                r#"{"x":[{"wildcard": "hel*o"}]}"#,
                &["helo", "hello", "hellxo", "helxxxo", "helxxxlo"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_overlap2() {
    // Go line 76-78
    exercise_multi_patterns(
        &["", "hlo", "hll", "hel", "helox"],
        &[
            (
                r#"{"x":[{"wildcard": "h*llo"}]}"#,
                &["hllo", "hello", "hxxxllo", "helllo"],
            ),
            (
                r#"{"x":[{"wildcard": "hel*o"}]}"#,
                &["helo", "hello", "helxo", "helllo"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_prefix_suffix() {
    // Go line 79-81
    exercise_multi_patterns(
        &["", "he", "hel", "helox", "helx", "hxlo"],
        &[
            (
                r#"{"x":[{"wildcard": "he*lo"}]}"#,
                &["helo", "hello", "helllo", "helxlo"],
            ),
            (
                r#"{"x":[{"wildcard": "hell*"}]}"#,
                &["hell", "hello", "helllo", "hellx", "hellxxx"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_prefix_suffix2() {
    // Go line 82-84
    exercise_multi_patterns(
        &["", "hel", "helox", "helxox", "hexo"],
        &[
            (
                r#"{"x":[{"wildcard": "hel*o"}]}"#,
                &["helo", "hello", "helllo", "hellloo", "helloo", "heloo"],
            ),
            (
                r#"{"x":[{"wildcard": "hell*"}]}"#,
                &["hell", "hello", "helllo", "hellloo", "helloo", "hellox"],
            ),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_prefix_pair() {
    // Go line 85-87
    exercise_multi_patterns(
        &["", "he", "hex", "hexlo"],
        &[
            (
                r#"{"x":[{"wildcard": "hel*"}]}"#,
                &["hel", "helx", "hello", "hellox"],
            ),
            (r#"{"x":[{"wildcard": "hello*"}]}"#, &["hello", "hellox"]),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_suffix_exact2() {
    // Go line 88-90
    exercise_multi_patterns(
        &["", "he", "hex", "hexlo"],
        &[
            (
                r#"{"x":[{"wildcard": "*hello"}]}"#,
                &["hello", "hhello", "hhhello"],
            ),
            (r#"{"x":["hello"]}"#, &["hello"]),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_infix_exact() {
    // Go line 91-93
    exercise_multi_patterns(
        &["", "he", "hel", "heo", "heloz", "hellox", "heloxo"],
        &[
            (
                r#"{"x":[{"wildcard": "he*lo"}]}"#,
                &["helo", "hello", "helllo"],
            ),
            (r#"{"x":["helox"]}"#, &["helox"]),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_infix_exact2() {
    // Go line 94-96
    exercise_multi_patterns(
        &["", "he", "helx", "helo", "hexlx", "hellox", "heloxx"],
        &[
            (
                r#"{"x":[{"wildcard": "he*l"}]}"#,
                &["hel", "hexl", "hexxxl"],
            ),
            (r#"{"x":["helox"]}"#, &["helox"]),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_prefix_exact() {
    // Go line 97-99
    exercise_multi_patterns(
        &["", "h", "hxlox", "hxelox"],
        &[
            (
                r#"{"x":[{"wildcard": "he*"}]}"#,
                &["he", "helo", "helox", "heloxx"],
            ),
            (r#"{"x":["helox"]}"#, &["helox"]),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_double_exact() {
    // Go line 100-102
    exercise_multi_patterns(
        &["", "h", "he", "hel", "hexxo", "hexxohexxo"],
        &[
            (
                r#"{"x":[{"wildcard": "h*l*o"}]}"#,
                &[
                    "hlo",
                    "helo",
                    "hllo",
                    "hello",
                    "hexloo",
                    "hellohello",
                    "hellohellxo",
                ],
            ),
            (r#"{"x":["hellohello"]}"#, &["hellohello"]),
        ],
    );
}

#[test]
fn test_wildcard_multi_patterns_double_exact2() {
    // Go line 103-105
    exercise_multi_patterns(
        &["", "h", "he", "hlo", "hexxo", "hexxohexxo"],
        &[
            (
                r#"{"x":[{"wildcard": "he*l*"}]}"#,
                &[
                    "hel",
                    "helo",
                    "hexl",
                    "hello",
                    "hexloo",
                    "hellohellx",
                    "hellohello",
                ],
            ),
            (r#"{"x":["hellohello"]}"#, &["hellohello"]),
        ],
    );
}

#[test]
fn test_wildcard_escape_backslash_star() {
    // Go line 40: `he\\\\\\*llo` (raw string = he\\\\\\*llo, 11 chars)
    // After JSON parse: he\\\*llo (escaped backslash + escaped star)
    // Wildcard meaning: he + literal_backslash + literal_star + llo
    // Should match literal string "he\*llo" (6 chars)
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"wildcard": "he\\\\\\*llo"}]}"#)
        .unwrap();

    // Should match "he\*llo" - in JSON, backslash needs escaping: "he\\*llo"
    let matches = q
        .matches_for_event(r#"{"x": "he\\*llo"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "Should match he\\*llo");

    // Should NOT match - use raw strings for JSON to avoid double-escaping confusion
    let no_match_events = [
        r#"{"x": "hello"}"#,
        r#"{"x": "he\\\\llo"}"#, // he\\llo (2 backslashes)
        r#"{"x": "he\\llo"}"#,   // he\llo (1 backslash)
        r#"{"x": "he\\xxllo"}"#, // he\xxllo
    ];
    for event in no_match_events {
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(matches.is_empty(), "Should not match {}", event);
    }
}

#[test]
fn test_wildcard_escape_backslash_wildcard() {
    // Go line 41: `he\\\\*llo` (raw string = he\\\\*llo, 10 chars)
    // After JSON parse: he\\*llo (escaped backslash + wildcard)
    // Wildcard meaning: he + literal_backslash + wildcard + llo
    // Should match "he\" followed by anything followed by "llo"
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"wildcard": "he\\\\*llo"}]}"#)
        .unwrap();

    // Should match - values with "he\" prefix and "llo" suffix
    let match_events = [
        r#"{"x": "he\\llo"}"#,   // he\llo (1 backslash, matches wildcard)
        r#"{"x": "he\\*llo"}"#,  // he\*llo
        r#"{"x": "he\\\\llo"}"#, // he\\llo (2 backslashes)
        r#"{"x": "he\\xxllo"}"#, // he\xxllo
    ];
    for event in match_events {
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match {}", event);
    }

    // Should NOT match
    let no_match_events = [
        r#"{"x": "hello"}"#,  // no backslash after he
        r#"{"x": "he\\ll"}"#, // doesn't end with llo
    ];
    for event in no_match_events {
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(matches.is_empty(), "Should not match {}", event);
    }
}

#[test]
fn test_shellstyle_duplicate_pattern() {
    // Go TestWildCardRuler: r4 and r5 are identical patterns
    let mut q = Quamina::new();
    q.add_pattern("r4", r#"{"c": [{"shellstyle": "xy*"}]}"#)
        .unwrap();
    q.add_pattern("r5", r#"{"c": [{"shellstyle": "xy*"}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"c": "xyzzz"}"#.as_bytes()).unwrap();
    assert_eq!(matches.len(), 2, "Both r4 and r5 should match");
    assert!(matches.contains(&"r4"));
    assert!(matches.contains(&"r5"));
}

#[test]
fn test_shellstyle_double_wildcard() {
    // Go TestWildCardRuler: r6 = 12*4*
    let mut q = Quamina::new();
    q.add_pattern("r6", r#"{"d": [{"shellstyle": "12*4*"}]}"#)
        .unwrap();

    // Should match
    let matches = q.matches_for_event(r#"{"d": "12345"}"#.as_bytes()).unwrap();
    assert_eq!(matches, vec!["r6"], "12*4* should match 12345");

    // Should NOT match
    let no_match = q.matches_for_event(r#"{"d": "1235"}"#.as_bytes()).unwrap();
    assert!(no_match.is_empty(), "12*4* should not match 1235");
}

#[test]
fn test_shellstyle_zero_length_prefix() {
    // Go TestWildCardRuler: {"a": "bc"} should match *bc
    let mut q = Quamina::new();
    q.add_pattern("r1", r#"{"a": [{"shellstyle": "*bc"}]}"#)
        .unwrap();

    let matches = q.matches_for_event(r#"{"a": "bc"}"#.as_bytes()).unwrap();
    assert_eq!(
        matches,
        vec!["r1"],
        "*bc should match bc (zero-length prefix)"
    );
}

#[test]
fn test_shellstyle_ruler_negative_cases() {
    // Go TestWildCardRuler: additional negative test cases
    let mut q = Quamina::new();
    q.add_pattern("r2", r#"{"b": [{"shellstyle": "d*f"}]}"#)
        .unwrap();
    q.add_pattern("r4", r#"{"c": [{"shellstyle": "xy*"}]}"#)
        .unwrap();

    // Should NOT match
    let cases = [
        (r#"{"c": "abc"}"#, "xy* should not match abc"),
        (r#"{"c": "abcxyz"}"#, "xy* should not match abcxyz"),
        (r#"{"b": "de"}"#, "d*f should not match de"),
    ];

    for (event, msg) in cases {
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(matches.is_empty(), "{}", msg);
    }
}

#[test]
fn test_wildcard_unicode_strings() {
    // Go TestWildcardMatching includes Unicode strings with Őz
    let mut q = Quamina::new();

    // Test *hello with Unicode prefix
    q.add_pattern("p1", r#"{"x": [{"wildcard": "*hello"}]}"#)
        .unwrap();
    let matches = q
        .matches_for_event(r#"{"x": "23Őzhello"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "*hello should match 23Őzhello");

    // Test h*llo with Unicode in middle
    let mut q2 = Quamina::new();
    q2.add_pattern("p2", r#"{"x": [{"wildcard": "h*llo"}]}"#)
        .unwrap();
    let matches2 = q2
        .matches_for_event(r#"{"x": "hel23Őzlllo"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches2, vec!["p2"], "h*llo should match hel23Őzlllo");

    // Test hello* with Unicode suffix
    let mut q3 = Quamina::new();
    q3.add_pattern("p3", r#"{"x": [{"wildcard": "hello*"}]}"#)
        .unwrap();
    let matches3 = q3
        .matches_for_event(r#"{"x": "hello23Őzlllo"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches3, vec!["p3"], "hello* should match hello23Őzlllo");

    // Test h*l*o with Unicode
    let mut q4 = Quamina::new();
    q4.add_pattern("p4", r#"{"x": [{"wildcard": "h*l*o"}]}"#)
        .unwrap();
    let matches4 = q4
        .matches_for_event(r#"{"x": "hel23Őzlllo"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches4, vec!["p4"], "h*l*o should match hel23Őzlllo");
}

#[test]
fn test_shellstyle_suffix_with_space() {
    // Go TestMakeShellStyleFA: *ST should match "STA ST"
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"shellstyle": "*ST"}]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"x": "STA ST"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["p1"], "*ST should match 'STA ST'");

    let matches2 = q.matches_for_event(r#"{"x": "1ST"}"#.as_bytes()).unwrap();
    assert_eq!(matches2, vec!["p1"], "*ST should match '1ST'");

    // Negative cases
    let no1 = q.matches_for_event(r#"{"x": "STA"}"#.as_bytes()).unwrap();
    assert!(no1.is_empty(), "*ST should not match 'STA'");

    let no2 = q
        .matches_for_event(r#"{"x": "STAST "}"#.as_bytes())
        .unwrap();
    assert!(
        no2.is_empty(),
        "*ST should not match 'STAST ' (trailing space)"
    );
}

#[test]
fn test_shellstyle_prefix_negative() {
    // Go TestMakeShellStyleFA: foo* negative cases
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"shellstyle": "foo*"}]}"#)
        .unwrap();

    let no1 = q.matches_for_event(r#"{"x": "afoo"}"#.as_bytes()).unwrap();
    assert!(no1.is_empty(), "foo* should not match 'afoo'");

    let no2 = q.matches_for_event(r#"{"x": "fofo"}"#.as_bytes()).unwrap();
    assert!(no2.is_empty(), "foo* should not match 'fofo'");
}

#[test]
fn test_shellstyle_suffix_negative() {
    // Go TestMakeShellStyleFA: *foo negative cases
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"shellstyle": "*foo"}]}"#)
        .unwrap();

    let no1 = q.matches_for_event(r#"{"x": "foox"}"#.as_bytes()).unwrap();
    assert!(no1.is_empty(), "*foo should not match 'foox'");

    let no2 = q.matches_for_event(r#"{"x": "afooo"}"#.as_bytes()).unwrap();
    assert!(no2.is_empty(), "*foo should not match 'afooo'");
}

#[test]
fn test_shellstyle_contains_negative() {
    // Go TestMakeShellStyleFA: *foo* negative cases
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"shellstyle": "*foo*"}]}"#)
        .unwrap();

    let no1 = q.matches_for_event(r#"{"x": "afoa"}"#.as_bytes()).unwrap();
    assert!(no1.is_empty(), "*foo* should not match 'afoa'");

    let no2 = q
        .matches_for_event(r#"{"x": "fofofoxooxoo"}"#.as_bytes())
        .unwrap();
    assert!(no2.is_empty(), "*foo* should not match 'fofofoxooxoo'");
}

#[test]
fn test_shellstyle_double_wildcard_variations() {
    // Go TestMakeShellStyleFA: xx*yy*zz and *xx*yy* additional cases
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": [{"shellstyle": "xx*yy*zz"}]}"#)
        .unwrap();

    // Additional positive cases from Go
    for val in ["xxyycdzz", "xxabyyzz"] {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "xx*yy*zz should match {}", val);
    }

    // Test *xx*yy* additional cases
    let mut q2 = Quamina::new();
    q2.add_pattern("p2", r#"{"x": [{"shellstyle": "*xx*yy*"}]}"#)
        .unwrap();

    for val in ["abxxcdyyef", "xxcdyyef", "abxxyyef", "xxcdyy", "xxyyef"] {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        let matches = q2.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p2"], "*xx*yy* should match {}", val);
    }
}

#[test]
fn test_regexp_samples_exist() {
    assert!(
        !crate::regexp_samples::REGEXP_SAMPLES.is_empty(),
        "No regexp samples found"
    );
    assert_eq!(
        crate::regexp_samples::REGEXP_SAMPLES.len(),
        992,
        "Expected 992 samples"
    );
}

// MIRI SKIP RATIONALE: Iterates over 992 REGEXP_SAMPLES, building and traversing NFAs for
// each. Under Miri this takes 8+ minutes and causes the CI job to time out.
#[test]
#[cfg_attr(miri, ignore)]
fn test_regexp_validity() {
    use crate::automaton::arena::{traverse_arena_nfa, ArenaNfaBuffers, ARENA_VALUE_TERMINATOR};
    use crate::regexp::{make_regexp_nfa_arena, parse_regexp};
    use crate::regexp_samples::REGEXP_SAMPLES;
    use std::sync::Arc;

    /// Regexps with * that should match empty string
    fn star_samples_matching_empty(regex: &str) -> bool {
        matches!(
            regex,
            "(([~.~~~?~*~+~{~}~[~]~(~)~|]?)*)+"
                | "[~~~|~.~?~*~+~(~)~{~}~-~[~]~^]*"
                | "[~*a]*"
                | "[a-]*"
                | "[~n~r~t~~~|~.~-~^~?~*~+~{~}~[~]~(~)]*"
                | "[a~*]*"
                | "[0-9]*"
                | "(([a-d]*)|([a-z]*))"
                | "(([d-f]*)|([c-e]*))"
                | "(([c-e]*)|([d-f]*))"
                | "(([a-d]*)|(.*))"
                | "(([d-f]*)|(.*))"
                | "(([c-e]*)|(.*))"
                | "(.*)"
                | "([^~?])*"
        )
    }

    let mut problems = 0;
    let mut tests = 0;
    let mut implemented = 0;
    let mut correctly_matched = 0;
    let mut correctly_not_matched = 0;

    for sample in REGEXP_SAMPLES.iter() {
        tests += 1;

        fn should_skip(re: &str) -> bool {
            if re.contains("-[") {
                return true;
            }
            let chars: Vec<char> = re.chars().collect();
            for i in 0..chars.len().saturating_sub(1) {
                if chars[i] == '~' {
                    let next = chars[i + 1];
                    if matches!(next, 'b' | 'B') {
                        return true;
                    }
                }
            }
            false
        }

        fn is_known_extension(re: &str) -> bool {
            let chars: Vec<char> = re.chars().collect();
            for i in 0..chars.len() {
                if i + 1 < chars.len() && chars[i] == '~' {
                    let next = chars[i + 1];
                    if matches!(
                        next,
                        'd' | 'D' | 'w' | 'W' | 's' | 'S' | 'p' | 'P' | 'i' | 'I' | 'c' | 'C'
                    ) {
                        return true;
                    }
                }
                if i + 1 < chars.len()
                    && matches!(chars[i], '*' | '+' | '?' | '}')
                    && chars[i + 1] == '?'
                {
                    return true;
                }
                if i + 2 < chars.len()
                    && chars[i] == '('
                    && chars[i + 1] == '?'
                    && chars[i + 2] == ':'
                {
                    return true;
                }
            }
            false
        }

        if should_skip(sample.regex) {
            continue;
        }

        if sample.matches.iter().any(|s| s.len() > 50)
            || sample.nomatches.iter().any(|s| s.len() > 50)
        {
            continue;
        }

        let parse_result = parse_regexp(sample.regex);

        if sample.valid {
            match parse_result {
                Ok(tree) => {
                    implemented += 1;

                    let (arena, start, field_matcher) = make_regexp_nfa_arena(tree, false);
                    let mut bufs = ArenaNfaBuffers::new();

                    for should_match in sample.matches {
                        let mut value: Vec<u8> = should_match.as_bytes().to_vec();
                        value.push(ARENA_VALUE_TERMINATOR);
                        bufs.clear();
                        traverse_arena_nfa(&arena, start, &value, &mut bufs);
                        let matched = bufs
                            .transitions
                            .iter()
                            .any(|m| Arc::ptr_eq(m, &field_matcher));
                        if !matched && !should_match.is_empty() {
                            problems += 1;
                        } else if matched {
                            correctly_matched += 1;
                        }
                    }

                    for should_not_match in sample.nomatches {
                        let mut value: Vec<u8> = should_not_match.as_bytes().to_vec();
                        value.push(ARENA_VALUE_TERMINATOR);
                        bufs.clear();
                        traverse_arena_nfa(&arena, start, &value, &mut bufs);
                        let matched = bufs
                            .transitions
                            .iter()
                            .any(|m| Arc::ptr_eq(m, &field_matcher));
                        if matched {
                            if should_not_match.is_empty()
                                && star_samples_matching_empty(sample.regex)
                            {
                                // Expected
                            } else if !should_not_match.is_empty() {
                                problems += 1;
                            }
                        } else {
                            correctly_not_matched += 1;
                        }
                    }
                }
                Err(_) => {}
            }
        } else {
            if parse_result.is_ok() {
                let is_extension = is_known_extension(sample.regex);
                if is_extension {
                    implemented += 1;
                } else {
                    problems += 1;
                }
            }
        }

        if problems >= 10 {
            break;
        }
    }

    assert!(
        problems <= 4,
        "Found {} regexp validation problems (expected <= 4)",
        problems
    );
}

/// Miri-only: exercises parse_regexp + make_regexp_nfa_arena + traverse_arena_nfa
#[test]
#[cfg(miri)]
fn test_regexp_validity_miri_minimal() {
    use crate::automaton::arena::{traverse_arena_nfa, ArenaNfaBuffers, ARENA_VALUE_TERMINATOR};
    use crate::regexp::{make_regexp_nfa_arena, parse_regexp};
    use std::sync::Arc;

    let mut bufs = ArenaNfaBuffers::new();

    let root = parse_regexp("a|b").unwrap();
    let (arena, start, fm) = make_regexp_nfa_arena(root, false);
    bufs.clear();
    traverse_arena_nfa(&arena, start, &[b'a', ARENA_VALUE_TERMINATOR], &mut bufs);
    assert!(bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm)));
    bufs.clear();
    traverse_arena_nfa(&arena, start, &[b'x', ARENA_VALUE_TERMINATOR], &mut bufs);
    assert!(!bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm)));

    let root = parse_regexp("a(h|i)z").unwrap();
    let (arena, start, fm) = make_regexp_nfa_arena(root, false);
    bufs.clear();
    traverse_arena_nfa(
        &arena,
        start,
        &[b'a', b'h', b'z', ARENA_VALUE_TERMINATOR],
        &mut bufs,
    );
    assert!(bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm)));

    let root = parse_regexp("[a-c]").unwrap();
    let (arena, start, fm) = make_regexp_nfa_arena(root, false);
    bufs.clear();
    traverse_arena_nfa(&arena, start, &[b'b', ARENA_VALUE_TERMINATOR], &mut bufs);
    assert!(bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm)));
    bufs.clear();
    traverse_arena_nfa(&arena, start, &[b'z', ARENA_VALUE_TERMINATOR], &mut bufs);
    assert!(!bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm)));

    let root = parse_regexp("a.b").unwrap();
    let (arena, start, fm) = make_regexp_nfa_arena(root, false);
    bufs.clear();
    traverse_arena_nfa(
        &arena,
        start,
        &[b'a', b'x', b'b', ARENA_VALUE_TERMINATOR],
        &mut bufs,
    );
    assert!(bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm)));
}

/// Miri-only: exercises regexp end-to-end through Quamina
#[test]
#[cfg(miri)]
fn test_regexp_end2end_miri_minimal() {
    let mut q = Quamina::new();

    q.add_pattern("p0", r#"{"a": [{"regexp": "abc|def"}]}"#)
        .unwrap();
    let m = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
    assert!(m.contains(&"p0"));
    let m = q.matches_for_event(r#"{"a": "xyz"}"#.as_bytes()).unwrap();
    assert!(!m.contains(&"p0"));

    q.add_pattern("p1", r#"{"a": [{"regexp": "a(h|i)z"}]}"#)
        .unwrap();
    let m = q.matches_for_event(r#"{"a": "ahz"}"#.as_bytes()).unwrap();
    assert!(m.contains(&"p1"));

    q.add_pattern("p2", r#"{"a": [{"regexp": "[a-c]"}]}"#)
        .unwrap();
    let m = q.matches_for_event(r#"{"a": "b"}"#.as_bytes()).unwrap();
    assert!(m.contains(&"p2"));
}

// ============================================================================
// Additional Missing Tests
// ============================================================================

#[test]
fn test_json_all_escape_sequences() {
    // Based on Go's TestOneEscape (escaping_test.go:45)
    // Tests all 8 standard JSON escape sequences plus unicode escapes

    // Test: \" (quote)
    let mut q1 = Quamina::new();
    q1.add_pattern("p1", r#"{"x": ["hello\"world"]}"#).unwrap();
    let m1 = q1
        .matches_for_event(r#"{"x": "hello\"world"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"], "Quote escape should match");

    // Test: \/ (forward slash - optional in JSON but must be handled)
    let mut q2 = Quamina::new();
    q2.add_pattern("p2", r#"{"x": ["a/b"]}"#).unwrap();
    let m2 = q2.matches_for_event(r#"{"x": "a\/b"}"#.as_bytes()).unwrap();
    assert_eq!(m2, vec!["p2"], "Forward slash escape should match");

    // Test: \b (backspace, 0x08)
    let mut q3 = Quamina::new();
    let pattern_with_backspace = format!(r#"{{"x": ["a{}b"]}}"#, '\x08');
    q3.add_pattern("p3", &pattern_with_backspace).unwrap();
    let m3 = q3.matches_for_event(r#"{"x": "a\bb"}"#.as_bytes()).unwrap();
    assert_eq!(m3, vec!["p3"], "Backspace escape should match");

    // Test: \f (form feed, 0x0c)
    let mut q4 = Quamina::new();
    let pattern_with_formfeed = format!(r#"{{"x": ["a{}b"]}}"#, '\x0c');
    q4.add_pattern("p4", &pattern_with_formfeed).unwrap();
    let m4 = q4.matches_for_event(r#"{"x": "a\fb"}"#.as_bytes()).unwrap();
    assert_eq!(m4, vec!["p4"], "Form feed escape should match");

    // Test: \r (carriage return)
    let mut q5 = Quamina::new();
    q5.add_pattern("p5", r#"{"x": ["a\rb"]}"#).unwrap();
    let m5 = q5.matches_for_event(r#"{"x": "a\rb"}"#.as_bytes()).unwrap();
    assert_eq!(m5, vec!["p5"], "Carriage return escape should match");
}

// MIRI SKIP RATIONALE: CIDR tests with multiple prefixes are slow under Miri
#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_ipv4_various_prefixes() {
    let mut q = Quamina::new();

    // /8 - Class A
    q.add_pattern("class_a", r#"{"ip": [{"cidr": "10.0.0.0/8"}]}"#)
        .unwrap();

    // /16 - Class B
    q.add_pattern("class_b", r#"{"ip": [{"cidr": "172.16.0.0/16"}]}"#)
        .unwrap();

    // /24 - Class C
    q.add_pattern("class_c", r#"{"ip": [{"cidr": "192.168.1.0/24"}]}"#)
        .unwrap();

    // /32 - Single host
    q.add_pattern("single", r#"{"ip": [{"cidr": "8.8.8.8/32"}]}"#)
        .unwrap();

    // Test /8
    let m1 = q
        .matches_for_event(r#"{"ip": "10.255.255.255"}"#.as_bytes())
        .unwrap();
    assert!(m1.contains(&"class_a"));

    // Test /16
    let m2 = q
        .matches_for_event(r#"{"ip": "172.16.255.255"}"#.as_bytes())
        .unwrap();
    assert!(m2.contains(&"class_b"));

    // Test /24
    let m3 = q
        .matches_for_event(r#"{"ip": "192.168.1.100"}"#.as_bytes())
        .unwrap();
    assert!(m3.contains(&"class_c"));

    // Test /32
    let m4 = q
        .matches_for_event(r#"{"ip": "8.8.8.8"}"#.as_bytes())
        .unwrap();
    assert!(m4.contains(&"single"));

    // Test non-match
    let m5 = q
        .matches_for_event(r#"{"ip": "8.8.8.9"}"#.as_bytes())
        .unwrap();
    assert!(!m5.contains(&"single"));
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_ipv6_basic() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"sourceIP": [{"cidr": "2001:db8::/32"}]}"#)
        .unwrap();

    // IPs in range should match (using full form, not shorthand)
    let m1 = q
        .matches_for_event(r#"{"sourceIP": "2001:db8:0:0:0:0:0:1"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["p1"], "IPv6 in range should match");

    let m2 = q
        .matches_for_event(r#"{"sourceIP": "2001:db8:ffff:ffff:ffff:ffff:ffff:ffff"}"#.as_bytes())
        .unwrap();
    assert_eq!(m2, vec!["p1"], "IPv6 at end of range should match");

    let m3 = q
        .matches_for_event(r#"{"sourceIP": "2001:db9:0:0:0:0:0:1"}"#.as_bytes())
        .unwrap();
    assert!(m3.is_empty(), "IPv6 outside range should not match");
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_ipv6_shorthand() {
    let mut q = Quamina::new();
    q.add_pattern("loopback", r#"{"ip": [{"cidr": "::1/128"}]}"#)
        .unwrap();

    // Loopback should match (using full form)
    let m1 = q
        .matches_for_event(r#"{"ip": "0:0:0:0:0:0:0:1"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["loopback"], "Loopback should match");

    // Different IP should not match
    let m2 = q
        .matches_for_event(r#"{"ip": "0:0:0:0:0:0:0:2"}"#.as_bytes())
        .unwrap();
    assert!(m2.is_empty(), "Non-loopback should not match /128");
}

#[test]
fn test_cidr_non_ip_values() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"ip": [{"cidr": "10.0.0.0/8"}]}"#)
        .unwrap();

    // Non-IP string should not match (and not panic)
    let m1 = q
        .matches_for_event(r#"{"ip": "not-an-ip"}"#.as_bytes())
        .unwrap();
    assert!(m1.is_empty(), "Non-IP string should not match CIDR");

    // Empty string
    let m2 = q.matches_for_event(r#"{"ip": ""}"#.as_bytes()).unwrap();
    assert!(m2.is_empty(), "Empty string should not match CIDR");

    // Number (not a string)
    let m3 = q.matches_for_event(r#"{"ip": 12345}"#.as_bytes()).unwrap();
    assert!(m3.is_empty(), "Number should not match CIDR");
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_with_other_matchers() {
    let mut q = Quamina::new();

    // CIDR pattern
    q.add_pattern("internal", r#"{"sourceIP": [{"cidr": "10.0.0.0/8"}]}"#)
        .unwrap();

    // Exact match pattern on same field
    q.add_pattern("specific", r#"{"sourceIP": ["10.0.0.1"]}"#)
        .unwrap();

    // Different field
    q.add_pattern("status", r#"{"status": ["active"]}"#)
        .unwrap();

    // Event matching CIDR and exact
    let m1 = q
        .matches_for_event(r#"{"sourceIP": "10.0.0.1", "status": "active"}"#.as_bytes())
        .unwrap();
    assert!(m1.contains(&"internal"));
    assert!(m1.contains(&"specific"));
    assert!(m1.contains(&"status"));

    // Event matching only CIDR
    let m2 = q
        .matches_for_event(r#"{"sourceIP": "10.0.0.2"}"#.as_bytes())
        .unwrap();
    assert!(m2.contains(&"internal"));
    assert!(!m2.contains(&"specific"));
}

#[test]
fn test_lookaround_pattern_parsing() {
    use crate::regexp::parse_regexp;

    // Positive lookahead
    let result = parse_regexp("foo(?=bar)");
    assert!(result.is_ok(), "Positive lookahead should parse");

    // Negative lookahead
    let result = parse_regexp("foo(?!bar)");
    assert!(result.is_ok(), "Negative lookahead should parse");

    // Positive lookbehind
    let result = parse_regexp("(?<=foo)bar");
    assert!(result.is_ok(), "Positive lookbehind should parse");

    // Negative lookbehind
    let result = parse_regexp("(?<!foo)bar");
    assert!(result.is_ok(), "Negative lookbehind should parse");
}

#[test]
fn test_lookaround_transformation() {
    // Test that lookaround patterns are properly transformed for matching
    let mut q = Quamina::new();

    // foo followed by bar (lookahead)
    q.add_pattern("la", r#"{"x": [{"regexp": "foo(?=bar)bar"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"x": "foobar"}"#.as_bytes())
        .unwrap();
    assert!(m1.contains(&"la"), "foo(?=bar)bar should match foobar");
}

#[test]
fn test_lookaround_rejected_patterns() {
    use crate::regexp::parse_regexp;

    // Nested lookaround should be rejected
    let result = parse_regexp("(?=foo(?=bar))");
    assert!(result.is_err(), "Nested lookahead should be rejected");

    // Variable-length lookbehind should be rejected
    let result = parse_regexp("(?<=foo*)bar");
    assert!(
        result.is_err(),
        "Variable-length lookbehind should be rejected"
    );

    let result = parse_regexp("(?<=foo+)bar");
    assert!(
        result.is_err(),
        "Variable-length lookbehind with + should be rejected"
    );
}

#[test]
fn test_lookaround_primary_match() {
    // Test that lookaround patterns match with condition verification
    let mut q = Quamina::<String>::new();

    // Add pattern where primary is "foo"
    // foo(?=bar) has primary="foo", condition=PositiveLookahead("foobar")
    let pattern = r#"{"status": [{"regexp": "foo(?=bar)"}]}"#;
    q.add_pattern("lookahead".to_string(), pattern).unwrap();

    // Event with "foobar" - primary "foo" matches and condition "foobar" matches
    let event = r#"{"status": "foobar"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        matches.contains(&"lookahead".to_string()),
        "foo(?=bar) should match 'foobar'"
    );

    // Event with just "foo" - primary matches but condition fails
    let event = r#"{"status": "foo"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        !matches.contains(&"lookahead".to_string()),
        "foo(?=bar) should NOT match 'foo' (lookahead fails)"
    );
}
