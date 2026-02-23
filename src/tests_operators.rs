//! Operator tests for quamina-rs
//!
//! Go lineage: anything_but_test.go, shellstyle_test.go, regexp_test.go, monocase_test.go, nfa_test.go
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
//! - Word boundary (~b/~B)

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
    let q = q!("p1" => r#"{"name": [{"prefix": "prod-"}]}"#);
    assert_matches!(
        q,
        r#"{"name": "prod-server-1"}"#,
        vec!["p1"],
        "Should match prefix"
    );
    assert_no_match!(
        q,
        r#"{"name": "dev-server-1"}"#,
        "Should not match different prefix"
    );
}

#[test]
fn test_suffix() {
    let q = q!("p1" => r#"{"file": [{"suffix": ".jpg"}]}"#);
    assert_matches!(q, r#"{"file": "photo.jpg"}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"file": "photo.png"}"#);
}

// ============================================================================
// Wildcard Operator Tests
// ============================================================================

#[test]
fn test_wildcard_suffix() {
    let q = q!("p1" => r#"{"file": [{"wildcard": "*.txt"}]}"#);
    assert_matches!(
        q,
        r#"{"file": "document.txt"}"#,
        vec!["p1"],
        "Should match *.txt"
    );
    assert_no_match!(q, r#"{"file": "document.pdf"}"#, "Should not match .pdf");
}

#[test]
fn test_wildcard_prefix() {
    let q = q!("p1" => r#"{"name": [{"wildcard": "prod-*"}]}"#);
    assert_matches!(
        q,
        r#"{"name": "prod-server"}"#,
        vec!["p1"],
        "Should match prod-*"
    );
}

#[test]
fn test_wildcard_contains() {
    let q = q!("p1" => r#"{"msg": [{"wildcard": "*error*"}]}"#);
    assert_matches!(
        q,
        r#"{"msg": "an error occurred"}"#,
        vec!["p1"],
        "Should match *error*"
    );
    assert_no_match!(q, r#"{"msg": "all good"}"#);
}

#[test]
fn test_wildcard_matches_empty_string() {
    let q = q!("p1" => r#"{"x": [{"wildcard": "*"}]}"#);
    assert_matches!(q, r#"{"x": ""}"#, vec!["p1"], "* should match empty string");
    assert_matches!(
        q,
        r#"{"x": "hello"}"#,
        vec!["p1"],
        "* should match any string"
    );
}

#[test]
fn test_wildcard_escape_star() {
    let q = q!("p1" => r#"{"val": [{"wildcard": "a\\*b"}]}"#);
    assert_matches!(
        q,
        r#"{"val": "a*b"}"#,
        vec!["p1"],
        "\\* should match literal *"
    );
    assert_no_match!(q, r#"{"val": "aXb"}"#, "Escaped * should not be wildcard");
}

#[test]
fn test_wildcard_escape_backslash() {
    let q = q!("p1" => r#"{"path": [{"wildcard": "a\\\\b"}]}"#);
    assert_matches!(
        q,
        r#"{"path": "a\\b"}"#,
        vec!["p1"],
        "\\\\ should match literal \\"
    );
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
    let q = q!("p1" => r#"{"a": [{"shellstyle": "*bc"}]}"#);
    assert_matches!(q, r#"{"a": "bc"}"#, vec!["p1"]);
    assert_matches!(q, r#"{"a": "abc"}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"a": "xyz"}"#);
}

#[test]
fn test_shellstyle_prefix() {
    let q = q!("p1" => r#"{"c": [{"shellstyle": "xy*"}]}"#);
    assert_matches!(q, r#"{"c": "xyzzz"}"#, vec!["p1"]);
    assert_matches!(q, r#"{"c": "xy"}"#, vec!["p1"]);
}

#[test]
fn test_shellstyle_infix() {
    let q = q!("p1" => r#"{"b": [{"shellstyle": "d*f"}]}"#);
    assert_matches!(q, r#"{"b": "dexef"}"#, vec!["p1"]);
    assert_matches!(q, r#"{"b": "df"}"#, vec!["p1"]);
}

#[test]
fn test_shellstyle_multiple_wildcards() {
    let q = q!("p1" => r#"{"d": [{"shellstyle": "12*4*"}]}"#);
    assert_matches!(q, r#"{"d": "12345"}"#, vec!["p1"]);
    assert_matches!(q, r#"{"d": "1244"}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"d": "1235"}"#);
}

#[test]
fn test_shellstyle_contains() {
    let q = q!("p1" => r#"{"x": [{"shellstyle": "*foo*"}]}"#);

    for text in ["xxfooyy", "fooyy", "xxfoo", "foo"] {
        let event = format!(r#"{{"x": "{}"}}"#, text);
        assert_matches!(q, event, vec!["p1"]);
    }

    assert_no_match!(q, r#"{"x": "bar"}"#);
}

#[test]
fn test_multiple_overlapping_shellstyle_patterns() {
    let q = q!(
        "suffix_bc" => r#"{"a": [{"shellstyle": "*bc"}]}"#,
        "infix_ef"  => r#"{"b": [{"shellstyle": "d*f"}]}"#,
        "prefix_xy" => r#"{"c": [{"shellstyle": "xy*"}]}"#
    );

    assert_has_match!(q, r#"{"a": "abc"}"#, "suffix_bc");
    assert_has_match!(q, r#"{"b": "dexef"}"#, "infix_ef");
    assert_has_match!(q, r#"{"c": "xyzzz"}"#, "prefix_xy");
}

/// Go lineage: nfa_test.go TestNestedTransmapSafety
///
/// Verifies that multi-field shellstyle patterns match correctly when nested
/// NFA traversals occur. In Go, this caught a bug where nested traverseNFA
/// calls corrupted the outer transmap buffer. The Rust architecture avoids
/// this bug by returning owned Vecs from transition_on(), but this test
/// validates the matching correctness of multi-field shellstyle patterns.
#[test]
#[cfg_attr(miri, ignore)]
fn test_nested_transmap_safety() {
    let q = q!(
        "P0" => r#"{"a": [{"shellstyle": "foo*"}], "b": [{"shellstyle": "bar*"}]}"#,
        "P1" => r#"{"a": [{"shellstyle": "foo*"}], "b": [{"shellstyle": "baz*"}]}"#,
        "P2" => r#"{"a": [{"shellstyle": "fox*"}], "b": [{"shellstyle": "bar*"}]}"#
    );

    // Matches P0: a=foo*, b=bar*
    assert_has_match!(q, r#"{"a": "fooXYZ", "b": "barXYZ"}"#, "P0");
    // Matches P1: a=foo*, b=baz*
    assert_has_match!(q, r#"{"a": "fooABC", "b": "bazABC"}"#, "P1");
    // Matches P2: a=fox*, b=bar*
    assert_has_match!(q, r#"{"a": "foxDEF", "b": "barDEF"}"#, "P2");
    // a=foo* matches P0 and P1, b=bar matches only P0
    assert_has_match!(q, r#"{"a": "fooXYZ", "b": "bar"}"#, "P0");
    assert_no_has_match!(q, r#"{"a": "fooXYZ", "b": "bar"}"#, "P1");
    // a=foo* matches P0 and P1, b=baz matches only P1
    assert_has_match!(q, r#"{"a": "fooXYZ", "b": "baz"}"#, "P1");
    assert_no_has_match!(q, r#"{"a": "fooXYZ", "b": "baz"}"#, "P0");
    // No match
    assert_no_match!(q, r#"{"a": "nomatch", "b": "nomatch"}"#);
}

/// Go lineage: nfa_test.go TestOverlappingShellStyleNesting
///
/// Validates that overlapping shellstyle patterns on multiple fields produce
/// correct matches when nested NFA traversals occur. The key scenario: field
/// "a" has both `*` and `foo*` patterns, which BOTH match `"fooX"`, producing
/// two separate fieldMatcher transitions. Each of those then traverses field
/// "b" which also has overlapping `*` and `bar*` patterns. In Go, a naive
/// single-buffer transmap would corrupt the outer buffer when the inner
/// traversal overwrites it. Rust avoids this structurally via owned Vec
/// returns, but this test validates the matching correctness.
#[test]
#[cfg_attr(miri, ignore)]
fn test_overlapping_shellstyle_nesting() {
    let q = q!(
        // Two patterns go through a:* (sharing one fieldMatcher after field "a")
        // with overlapping b patterns, so the inner traversal returns 2 results.
        "P1" => r#"{"a": [{"shellstyle": "*"}], "b": [{"shellstyle": "*"}]}"#,
        "P2" => r#"{"a": [{"shellstyle": "*"}], "b": [{"shellstyle": "bar*"}]}"#,
        // Two patterns go through a:foo* (sharing a different fieldMatcher after "a")
        // with overlapping b patterns, so this branch also produces 2 inner results.
        "P3" => r#"{"a": [{"shellstyle": "foo*"}], "b": [{"shellstyle": "*"}]}"#,
        "P4" => r#"{"a": [{"shellstyle": "foo*"}], "b": [{"shellstyle": "bar*"}]}"#
    );

    let event = r#"{"a": "fooX", "b": "barY"}"#;
    assert_has_match!(q, event, "P1");
    assert_has_match!(q, event, "P2");
    assert_has_match!(q, event, "P3");
    assert_has_match!(q, event, "P4");
    assert_match_count!(q, event, 4);
}

/// Go lineage: nfa_test.go TestThreeLevelNesting
///
/// Exercises 3 levels of nested NFA traversals. Field "a" has overlapping
/// patterns producing 2 outer fieldMatchers. One branch goes through fields
/// "b" then "c" (each with overlapping patterns), creating depth-3 nesting.
/// A separate branch through a:foo* reaches field "d" only if the outer
/// buffer survives the nested traversals.
#[test]
#[cfg_attr(miri, ignore)]
fn test_three_level_nesting() {
    let q = q!(
        // Branch through a:* → b → c (3 levels of NFA nesting)
        "deep-1" => r#"{"a": [{"shellstyle": "*"}], "b": [{"shellstyle": "*"}], "c": [{"shellstyle": "cat*"}]}"#,
        "deep-2" => r#"{"a": [{"shellstyle": "*"}], "b": [{"shellstyle": "bar*"}], "c": [{"shellstyle": "cow*"}]}"#,
        // Branch through a:foo* → d (only reachable if outer buffer is intact)
        "side"   => r#"{"a": [{"shellstyle": "foo*"}], "d": [{"shellstyle": "dog*"}]}"#
    );

    let event = r#"{"a": "fooX", "b": "barY", "c": "catZ", "d": "dogW"}"#;

    // Run multiple iterations: Rust uses deterministic HashMap iteration
    // (unlike Go's randomized map order), but repeated runs still validate
    // that the matching logic is stable.
    for i in 0..100 {
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.contains(&"deep-1"),
            "iter {i}: missing deep-1, got {matches:?}"
        );
        assert!(
            matches.contains(&"side"),
            "iter {i}: missing side, got {matches:?}"
        );
        assert!(
            !matches.contains(&"deep-2"),
            "iter {i}: unexpected deep-2 (c=catZ should not match cow*)"
        );
    }
}

/// Miri-friendly variant of test_nested_transmap_safety.
///
/// Exercises multi-field shellstyle matching with a single match/no-match
/// assertion per pattern to keep Miri runtime manageable.
#[test]
fn test_nested_transmap_safety_miri_friendly() {
    let q = q!(
        "P0" => r#"{"a": [{"shellstyle": "foo*"}], "b": [{"shellstyle": "bar*"}]}"#,
        "P1" => r#"{"a": [{"shellstyle": "foo*"}], "b": [{"shellstyle": "baz*"}]}"#
    );

    assert_has_match!(q, r#"{"a": "fooX", "b": "barX"}"#, "P0");
    assert_has_match!(q, r#"{"a": "fooX", "b": "bazX"}"#, "P1");
    assert_no_match!(q, r#"{"a": "nomatch", "b": "nomatch"}"#);
}

/// Miri-friendly variant of test_overlapping_shellstyle_nesting.
///
/// Uses 2 overlapping patterns (one `*`, one `foo*`) on a single field pair
/// to exercise the overlapping NFA traversal with minimal Miri cost.
#[test]
fn test_overlapping_shellstyle_nesting_miri_friendly() {
    let q = q!(
        "P1" => r#"{"a": [{"shellstyle": "*"}], "b": [{"shellstyle": "bar*"}]}"#,
        "P2" => r#"{"a": [{"shellstyle": "foo*"}], "b": [{"shellstyle": "bar*"}]}"#
    );

    let event = r#"{"a": "fooX", "b": "barY"}"#;
    assert_has_match!(q, event, "P1");
    assert_has_match!(q, event, "P2");
    assert_match_count!(q, event, 2);
}

/// Miri-friendly variant of test_three_level_nesting.
///
/// Single iteration (Rust HashMap iteration is deterministic, so repeating
/// adds no coverage). Validates the 3-level nesting and side branch survive.
#[test]
fn test_three_level_nesting_miri_friendly() {
    let q = q!(
        "deep-1" => r#"{"a": [{"shellstyle": "*"}], "b": [{"shellstyle": "*"}], "c": [{"shellstyle": "cat*"}]}"#,
        "side"   => r#"{"a": [{"shellstyle": "foo*"}], "d": [{"shellstyle": "dog*"}]}"#
    );

    let event = r#"{"a": "fooX", "b": "barY", "c": "catZ", "d": "dogW"}"#;
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert!(
        matches.contains(&"deep-1"),
        "missing deep-1, got {matches:?}"
    );
    assert!(matches.contains(&"side"), "missing side, got {matches:?}");
}

// ============================================================================
// Anything-But Operator Tests
// ============================================================================

#[test]
fn test_anything_but() {
    let q = q!("p1" => r#"{"status": [{"anything-but": ["deleted", "archived"]}]}"#);
    assert_matches!(
        q,
        r#"{"status": "active"}"#,
        vec!["p1"],
        "Should match non-excluded value"
    );
    assert_no_match!(
        q,
        r#"{"status": "deleted"}"#,
        "Should not match excluded value"
    );
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
    let q = q!("p1" => r#"{"status": [{"anything-but": "deleted"}]}"#);
    assert_matches!(q, r#"{"status": "active"}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"status": "deleted"}"#);
}

#[test]
fn test_anything_but_numeric() {
    let q = q!("p1" => r#"{"code": [{"anything-but": 404}]}"#);
    assert_matches!(
        q,
        r#"{"code": 200}"#,
        vec!["p1"],
        "Should match non-excluded number"
    );
    assert_no_match!(q, r#"{"code": 404}"#, "Should not match excluded number");
}

#[test]
fn test_anything_but_prefix_relationship() {
    // Tests that anything-but ["foo"] matches "foot" (since "foot" != "foo")
    let q = q!("not_foo" => r#"{"z": [{"anything-but": ["foo"]}]}"#);
    assert_matches!(
        q,
        r#"{"z": "foot"}"#,
        vec!["not_foo"],
        "anything-but ['foo'] should match 'foot'"
    );
    assert_no_match!(
        q,
        r#"{"z": "foo"}"#,
        "anything-but ['foo'] should not match 'foo'"
    );
}

#[test]
fn test_anything_but_with_exact_match() {
    let q =
        q!("pFoo" => r#"{"z": ["foo"]}"#, "pAbFoot" => r#"{"z": [{"anything-but": ["foot"]}]}"#);
    assert_match_count!(q, r#"{"z": "foo"}"#, 2, "foo should match both patterns");
    assert_no_match!(q, r#"{"z": "foot"}"#, "foot should match nothing");
}

// ============================================================================
// Equals-Ignore-Case Tests
// ============================================================================

#[test]
fn test_equals_ignore_case() {
    let q = q!("p1" => r#"{"name": [{"equals-ignore-case": "Test"}]}"#);
    for (event, desc) in [
        (r#"{"name": "test"}"#, "lowercase"),
        (r#"{"name": "TEST"}"#, "uppercase"),
        (r#"{"name": "TeSt"}"#, "mixed case"),
    ] {
        assert_matches!(q, event, vec!["p1"], desc);
    }
    assert_no_match!(q, r#"{"name": "other"}"#);
}

#[test]
fn test_equals_ignore_case_multiple_patterns() {
    let q = q!(
        "r1" => r#"{"a": [{"equals-ignore-case": "aBc"}]}"#,
        "r2" => r#"{"b": [{"equals-ignore-case": "XyZ"}]}"#,
        "r3" => r#"{"b": [{"equals-ignore-case": "xyZ"}]}"#
    );
    assert_matches!(q, r#"{"a": "abc"}"#, vec!["r1"]);
    assert_match_count!(q, r#"{"b": "XYZ"}"#, 2, "Both r2 and r3 should match XYZ");
}

#[test]
fn test_equals_ignore_case_unicode() {
    let q = q!("p1" => r#"{"word": [{"equals-ignore-case": "Σοφία"}]}"#);
    assert_matches!(
        q,
        r#"{"word": "σοφία"}"#,
        vec!["p1"],
        "Greek sigma case folding"
    );
    assert_matches!(q, r#"{"word": "ΣΟΦΊΑ"}"#, vec!["p1"], "Greek uppercase");
}

// ============================================================================
// Numeric Comparison Tests
// ============================================================================

#[test]
fn test_numeric_greater_than() {
    let q = q!("p1" => r#"{"age": [{"numeric": [">", 18]}]}"#);
    assert_matches!(q, r#"{"age": 25}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"age": 18}"#);
    assert_no_match!(q, r#"{"age": 15}"#);
}

#[test]
fn test_numeric_range() {
    let q = q!("p1" => r#"{"score": [{"numeric": [">=", 0, "<=", 100]}]}"#);
    assert_matches!(q, r#"{"score": 50}"#, vec!["p1"]);
    assert_matches!(q, r#"{"score": 0}"#, vec!["p1"]);
    assert_matches!(q, r#"{"score": 100}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"score": 101}"#);
}

#[test]
fn test_numeric_equals() {
    let q = q!("p1" => r#"{"count": [{"numeric": ["=", 42]}]}"#);
    assert_matches!(q, r#"{"count": 42}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"count": 43}"#);
}

#[test]
fn test_numeric_scientific_notation() {
    let q = q!("p1" => r#"{"value": [{"numeric": [">=", 300, "<=", 400]}]}"#);
    assert_matches!(q, r#"{"value": 350}"#, vec!["p1"], "Integer 350");
    assert_matches!(q, r#"{"value": 3.5e2}"#, vec!["p1"], "Scientific 3.5e2");
}

// ============================================================================
// Regex Tests
// ============================================================================

#[test]
fn test_regex_match() {
    let q = q!("p1" => r#"{"code": [{"regex": "[A-Z]{3}-[0-9]{3}"}]}"#);
    assert_matches!(q, r#"{"code": "ABC-123"}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"code": "invalid"}"#);
}

#[test]
fn test_regex_with_escape() {
    // I-Regexp uses ~ as escape, not \
    let q = q!("p1" => r#"{"email": [{"regex": "[a-z]+@example~.com"}]}"#);
    assert_matches!(q, r#"{"email": "alice@example.com"}"#, vec!["p1"]);
    assert_no_match!(q, r#"{"email": "alice@exampleXcom"}"#);
}

#[test]
fn test_regex_alternation() {
    let q = q!("p1" => r#"{"a": [{"regex": "a|b"}]}"#);
    assert_has_match!(q, r#"{"a": "a"}"#, "p1");
    assert_has_match!(q, r#"{"a": "b"}"#, "p1");
    assert_no_match!(q, r#"{"a": "c"}"#);
}

#[test]
fn test_regex_character_class() {
    let q = q!("p1" => r#"{"a": [{"regex": "[hij]"}]}"#);
    assert_has_match!(q, r#"{"a": "h"}"#, "p1");
    assert_has_match!(q, r#"{"a": "i"}"#, "p1");
    assert_no_match!(q, r#"{"a": "x"}"#);
}

#[test]
fn test_regexp_simple_optional() {
    let q = q!("test" => r#"{"a": [{"regexp": "a?b"}]}"#);
    assert_has_match!(q, r#"{"a": "ab"}"#, "test", "'a?b' should match 'ab'");
    assert_has_match!(q, r#"{"a": "b"}"#, "test", "'a?b' should match 'b'");
    assert_no_match!(q, r#"{"a": "aab"}"#, "'a?b' should NOT match 'aab'");
}

#[test]
fn test_empty_regex_matches_empty_string() {
    let q = q!("a" => r#"{"a": [{"regex": ""}]}"#);
    assert_matches!(
        q,
        r#"{"a": ""}"#,
        vec!["a"],
        "empty regex should match empty string"
    );
    assert_no_match!(
        q,
        r#"{"a": "hello"}"#,
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
    let q = q!("p1" => r#"{"sourceIP": [{"cidr": "10.0.0.0/24"}]}"#);
    assert_matches!(
        q,
        r#"{"sourceIP": "10.0.0.1"}"#,
        vec!["p1"],
        "10.0.0.1 in /24"
    );
    assert_matches!(
        q,
        r#"{"sourceIP": "10.0.0.255"}"#,
        vec!["p1"],
        "10.0.0.255 in /24"
    );
    assert_no_match!(q, r#"{"sourceIP": "10.0.1.1"}"#, "10.0.1.1 NOT in /24");
}

// MIRI SKIP RATIONALE: Even a single CIDR /8 pattern + match takes ~160s under Miri.
#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_miri_lightweight() {
    let q = q!("p1" => r#"{"sourceIP": [{"cidr": "10.0.0.0/8"}]}"#);
    assert_matches!(
        q,
        r#"{"sourceIP": "10.1.2.3"}"#,
        vec!["p1"],
        "10.1.2.3 in /8"
    );
    assert_no_match!(q, r#"{"sourceIP": "192.168.1.1"}"#, "192.168.1.1 NOT in /8");
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
// Lookaround + Other Pattern Coexistence Tests
// ============================================================================

#[test]
fn test_lookahead_with_exact_on_same_field() {
    // Regression: lookahead pattern must still match when an exact pattern
    // is also registered on the same field (singleton must not short-circuit).
    let mut q = Quamina::<String>::new();
    q.add_pattern(
        "look".to_string(),
        r#"{"v": [{"regexp": "foo(?=bar)bar"}]}"#,
    )
    .unwrap();
    q.add_pattern("exact".to_string(), r#"{"v": ["hello"]}"#)
        .unwrap();

    // Lookahead pattern should still match "foobar"
    let m = q
        .matches_for_event(r#"{"v": "foobar"}"#.as_bytes())
        .unwrap();
    assert!(
        m.contains(&"look".to_string()),
        "lookahead pattern should match 'foobar' even with exact pattern on same field"
    );
    assert!(
        !m.contains(&"exact".to_string()),
        "exact pattern 'hello' should NOT match 'foobar'"
    );

    // Exact pattern should still match "hello"
    let m = q.matches_for_event(r#"{"v": "hello"}"#.as_bytes()).unwrap();
    assert!(
        m.contains(&"exact".to_string()),
        "exact pattern should match 'hello'"
    );
    assert!(
        !m.contains(&"look".to_string()),
        "lookahead pattern should NOT match 'hello'"
    );

    // Neither should match an unrelated value
    let m = q.matches_for_event(r#"{"v": "other"}"#.as_bytes()).unwrap();
    assert!(m.is_empty(), "no pattern should match 'other'");
}

#[test]
fn test_exact_added_before_lookahead_on_same_field() {
    // Same bug but with reversed add order: exact first, then lookahead.
    let mut q = Quamina::<String>::new();
    q.add_pattern("exact".to_string(), r#"{"v": ["hello"]}"#)
        .unwrap();
    q.add_pattern(
        "look".to_string(),
        r#"{"v": [{"regexp": "foo(?=bar)bar"}]}"#,
    )
    .unwrap();

    let m = q
        .matches_for_event(r#"{"v": "foobar"}"#.as_bytes())
        .unwrap();
    assert!(
        m.contains(&"look".to_string()),
        "lookahead pattern should match 'foobar' when exact was added first"
    );

    let m = q.matches_for_event(r#"{"v": "hello"}"#.as_bytes()).unwrap();
    assert!(
        m.contains(&"exact".to_string()),
        "exact pattern should match 'hello'"
    );
}

#[test]
fn test_singleton_hit_and_multi_condition_hit_same_value() {
    // When the exact pattern and the lookahead pattern both match the same
    // input value, both results must be returned. This exercises the
    // singleton-match + multi-condition path (not just singleton-miss).
    let mut q = Quamina::<String>::new();
    q.add_pattern(
        "look".to_string(),
        r#"{"v": [{"regexp": "foo(?=bar)bar"}]}"#,
    )
    .unwrap();
    q.add_pattern("exact".to_string(), r#"{"v": ["foobar"]}"#)
        .unwrap();

    let m = q
        .matches_for_event(r#"{"v": "foobar"}"#.as_bytes())
        .unwrap();
    assert!(
        m.contains(&"look".to_string()),
        "lookahead should match 'foobar'"
    );
    assert!(
        m.contains(&"exact".to_string()),
        "exact should also match 'foobar'"
    );
    assert_eq!(m.len(), 2, "both patterns should match");
}

// ============================================================================
// Word Boundary (~b/~B) Tests
// ============================================================================

#[test]
fn test_wb_start_word_char() {
    // "hello" starts with 'h' (word char) → ~b at start matches
    let q = q!("test" => r#"{"name": [{"regexp": "~bhello"}]}"#);
    assert_has_match!(q, r#"{"name": "hello"}"#, "test");
}

#[test]
fn test_wb_start_non_word_char() {
    // " hello" starts with ' ' → ~bhello doesn't match
    let q = q!("test" => r#"{"name": [{"regexp": "~bhello"}]}"#);
    assert_no_has_match!(q, r#"{"name": " hello"}"#, "test");
}

#[test]
fn test_wb_end_word_char() {
    let q = q!("test" => r#"{"name": [{"regexp": "hello~b"}]}"#);
    assert_has_match!(q, r#"{"name": "hello"}"#, "test");
}

#[test]
fn test_wb_end_non_word_char() {
    let q = q!("test" => r#"{"name": [{"regexp": "hello~b"}]}"#);
    assert_no_has_match!(q, r#"{"name": "hello "}"#, "test");
}

#[test]
fn test_wb_middle_word_to_nonword() {
    let q = q!("test" => r#"{"name": [{"regexp": "hello~b world"}]}"#);
    assert_has_match!(q, r#"{"name": "hello world"}"#, "test");
}

#[test]
fn test_wb_middle_nonword_to_word() {
    let q = q!("test" => r#"{"name": [{"regexp": "hello ~bworld"}]}"#);
    assert_has_match!(q, r#"{"name": "hello world"}"#, "test");
}

#[test]
fn test_wb_middle_word_to_word_err() {
    // 'o' and 'w' are both word chars → ~b impossible → error at pattern addition
    let mut q = Quamina::new();
    assert!(q
        .add_pattern("test", r#"{"name": [{"regexp": "hello~bworld"}]}"#)
        .is_err());
}

#[test]
fn test_nwb_word_to_word() {
    let q = q!("test" => r#"{"name": [{"regexp": "hello~Bworld"}]}"#);
    assert_has_match!(q, r#"{"name": "helloworld"}"#, "test");
}

#[test]
fn test_nwb_word_to_nonword_err() {
    // 'o' is word, ' ' is non-word → ~B impossible → error
    let mut q = Quamina::new();
    assert!(q
        .add_pattern("test", r#"{"name": [{"regexp": "hello~B world"}]}"#)
        .is_err());
}

#[test]
fn test_nwb_start_nonword() {
    // ~B at start: `"` is non-word, ' ' is non-word → same class → matches
    let q = q!("test" => r#"{"name": [{"regexp": "~B hello"}]}"#);
    assert_has_match!(q, r#"{"name": " hello"}"#, "test");
}

#[test]
fn test_nwb_start_word_err() {
    // ~B at start: `"` is non-word, 'h' is word → different → impossible → error
    let mut q = Quamina::new();
    assert!(q
        .add_pattern("test", r#"{"name": [{"regexp": "~Bhello"}]}"#)
        .is_err());
}

#[test]
fn test_wb_whole_word_match() {
    let q = q!("test" => r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#);
    assert_has_match!(q, r#"{"name": "the cat sat"}"#, "test");
}

#[test]
fn test_wb_whole_word_no_match() {
    let q = q!("test" => r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#);
    assert_no_has_match!(q, r#"{"name": "concatenate"}"#, "test");
}

#[test]
fn test_wb_whole_word_at_start() {
    let q = q!("test" => r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#);
    assert_has_match!(q, r#"{"name": "cat is here"}"#, "test");
}

#[test]
fn test_wb_whole_word_at_end() {
    let q = q!("test" => r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#);
    assert_has_match!(q, r#"{"name": "the cat"}"#, "test");
}

#[test]
fn test_wb_whole_word_only() {
    let q = q!("test" => r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#);
    assert_has_match!(q, r#"{"name": "cat"}"#, "test");
}

#[test]
fn test_wb_underscore_is_word_char() {
    // '_' is a word char, so no boundary between 'a' and '_' → error
    let mut q = Quamina::new();
    assert!(q
        .add_pattern("test", r#"{"name": [{"regexp": "a~b_"}]}"#)
        .is_err());
}

#[test]
fn test_nwb_underscore_is_word_char() {
    // '_' and 'a' are both word chars → ~B matches
    let q = q!("test" => r#"{"name": [{"regexp": "a~B_"}]}"#);
    assert_has_match!(q, r#"{"name": "a_"}"#, "test");
}

#[test]
fn test_wb_digit_to_space() {
    // '3'→' ' = word→nonword = boundary
    let q = q!("test" => r#"{"name": [{"regexp": "abc3~b end"}]}"#);
    assert_has_match!(q, r#"{"name": "abc3 end"}"#, "test");
}

#[test]
fn test_wb_with_char_class() {
    let q = q!("test" => r#"{"name": [{"regexp": "[0-9]~b "}]}"#);
    assert_has_match!(q, r#"{"name": "5 "}"#, "test");
}

#[test]
fn test_wb_with_dot() {
    let q = q!("test" => r#"{"name": [{"regexp": ".~b."}]}"#);
    assert_has_match!(q, r#"{"name": "a "}"#, "test");
    assert_no_has_match!(q, r#"{"name": "ab"}"#, "test");
}

#[test]
fn test_wb_plus_quantifier() {
    // a+~b exercises the Split path (quant_min=1), not SplitOrAbsent
    let q = q!("test" => r#"{"v": [{"regexp": "a+~b "}]}"#);
    assert_has_match!(q, r#"{"v": "aaa "}"#, "test");
    assert_has_match!(q, r#"{"v": "a "}"#, "test");
    assert_no_has_match!(q, r#"{"v": "aab"}"#, "test");
}

#[test]
fn test_wb_optional_quantifier() {
    // a?~b exercises SplitOrAbsent (quant_min=0): when a? matches 'a',
    // the boundary is between 'a' (word) and ' ' (non-word)
    let q = q!("test" => r#"{"v": [{"regexp": "xa?~b "}]}"#);
    assert_has_match!(q, r#"{"v": "xa "}"#, "test");
    assert_no_has_match!(q, r#"{"v": "xab"}"#, "test");
}

#[test]
fn test_wb_range_quantifier() {
    // a{2,4}~b exercises Split with quant_min=2
    let q = q!("test" => r#"{"v": [{"regexp": "a{2,4}~b "}]}"#);
    assert_has_match!(q, r#"{"v": "aa "}"#, "test");
    assert_has_match!(q, r#"{"v": "aaaa "}"#, "test");
    assert_no_has_match!(q, r#"{"v": "a "}"#, "test");
}

#[test]
fn test_wb_utf8_multibyte() {
    // Multi-byte UTF-8 chars (é = 0xC3 0xA9) are non-word, so
    // word→multi-byte = boundary, multi-byte→word = boundary
    let q = q!("test" => r#"{"v": [{"regexp": "caf~bé"}]}"#);
    assert_has_match!(q, r#"{"v": "café"}"#, "test");
}

#[test]
fn test_wb_utf8_emoji_boundary() {
    // Emoji (4-byte UTF-8) is non-word; word char→emoji = boundary
    // Use .~b. to test: word char then boundary then non-word (emoji)
    let q = q!("test" => r#"{"v": [{"regexp": ".~b."}]}"#);
    assert_has_match!(q, "{\"v\": \"a😀\"}", "test");
    assert_no_has_match!(q, r#"{"v": "ab"}"#, "test");
}

#[test]
fn test_wb_utf8_nonword_to_word() {
    // Non-ASCII char (non-word) followed by boundary then ASCII word char
    let q = q!("test" => r#"{"v": [{"regexp": ".~bcat"}]}"#);
    assert_has_match!(q, "{\"v\": \"écat\"}", "test");
    assert_no_has_match!(q, r#"{"v": "acat"}"#, "test");
}

// ============================================================================
// JSON Escape Sequences Tests
// ============================================================================

#[test]
fn test_json_escape_sequences() {
    let q = q!("p1" => r#"{"msg": ["line1\nline2"]}"#);
    assert_matches!(
        q,
        r#"{"msg": "line1\nline2"}"#,
        vec!["p1"],
        "Should match \\n escape sequence"
    );
}

#[test]
fn test_unicode_escape_in_event() {
    let q = q!("p1" => r#"{"greeting": ["Hello"]}"#);
    assert_matches!(
        q,
        r#"{"greeting": "\u0048\u0065\u006c\u006c\u006f"}"#,
        vec!["p1"],
        "Unicode escape should decode to 'Hello'"
    );
}

#[test]
fn test_unicode_escape_emoji() {
    let q = q!("p1" => r#"{"emoji": ["💋"]}"#);
    assert_matches!(
        q,
        r#"{"emoji": "\ud83d\udc8b"}"#,
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
    let q = q!(
        "p1" => r#"{"a": ["foo"]}"#,
        "p2" => r#"{"a": ["football"]}"#,
        "p3" => r#"{"a": ["footballer"]}"#
    );

    assert_matches!(
        q,
        r#"{"x": 3, "a": "foo"}"#,
        vec!["p1"],
        "foo should only match p1"
    );
    assert_matches!(
        q,
        r#"{"x": 3, "a": "football"}"#,
        vec!["p2"],
        "football should only match p2"
    );
    assert_matches!(
        q,
        r#"{"x": 3, "a": "footballer"}"#,
        vec!["p3"],
        "footballer should only match p3"
    );
    assert_no_match!(q, r#"{"a": "foot"}"#, "foot should not match any pattern");
}

// ============================================================================
// Additional Operator Tests (recovered from original)
// ============================================================================

#[test]
fn test_anything_but_numeric_single() {
    // Test single number: {"anything-but": 123}
    let q = q!("p1" => r#"{"code": [{"anything-but": 404}]}"#);

    assert_matches!(
        q,
        r#"{"code": 200}"#,
        vec!["p1"],
        "Should match non-excluded number"
    );
    assert_no_match!(q, r#"{"code": 404}"#, "Should not match excluded number");
    // Non-numeric string doesn't match excluded number, so passes
    assert_matches!(
        q,
        r#"{"code": "not-a-number"}"#,
        vec!["p1"],
        "Non-numeric value passes numeric anything-but"
    );
}

#[test]
fn test_anything_but_numeric_array() {
    // Test array of numbers: {"anything-but": [100, 200, 300]}
    let q = q!("p1" => r#"{"code": [{"anything-but": [400, 404, 500]}]}"#);

    assert_matches!(
        q,
        r#"{"code": 200}"#,
        vec!["p1"],
        "Should match non-excluded number"
    );
    assert_no_match!(q, r#"{"code": 404}"#, "Should not match excluded number");
    assert_no_match!(
        q,
        r#"{"code": 500}"#,
        "Should not match another excluded number"
    );
}

#[test]
fn test_anything_but_numeric_float() {
    // Test with floating point numbers
    let q = q!("p1" => r#"{"price": [{"anything-but": [9.99, 19.99]}]}"#);
    assert_matches!(
        q,
        r#"{"price": 14.99}"#,
        vec!["p1"],
        "Should match non-excluded float"
    );
    assert_no_match!(q, r#"{"price": 9.99}"#, "Should not match excluded float");
}

#[test]
fn test_equals_ignore_case_with_exact_match() {
    // Based on Go's TestSingletonMonocaseMerge (monocase_test.go:48)
    // Tests that exact match and equals-ignore-case patterns can coexist and merge correctly
    let q = q!(
        "singleton" => r#"{"x": ["singleton"]}"#,
        "mono"      => r#"{"x": [{"equals-ignore-case": "foo"}]}"#
    );

    assert_matches!(
        q,
        r#"{"x": "singleton"}"#,
        vec!["singleton"],
        "Exact match should work"
    );
    assert_matches!(
        q,
        r#"{"x": "FoO"}"#,
        vec!["mono"],
        "Case-insensitive match should work"
    );
    assert_no_match!(q, r#"{"x": "bar"}"#, "Unrelated value should not match");
}

#[test]
fn test_regex_various_patterns() {
    // Based on Go quamina's TestRegexpEnd2End — table-driven regex pattern tests
    let cases: &[(&str, &str, &[&str], &[&str])] = &[
        ("p1", "a|b", &["a", "b"], &["c"]),
        ("p2", "[hij]", &["h", "i", "j"], &["x"]),
        ("p3", "a[e-g]x", &["aex", "afx", "agx"], &["ax"]),
        (
            "p4",
            "[0-9][0-9][rtn][dh]",
            &["11th", "23rd", "22nd"],
            &["first"],
        ),
    ];

    for &(name, regex, matches, no_matches) in cases {
        let mut q = Quamina::new();
        let pattern = format!(r#"{{"a": [{{"regex": "{}"}}]}}"#, regex);
        q.add_pattern(name, &pattern).unwrap();

        for m in matches {
            let event = format!(r#"{{"a": "{}"}}"#, m);
            assert_has_match!(
                q,
                &event,
                name,
                &format!("'{}' should match '{}'", regex, m)
            );
        }
        for m in no_matches {
            let event = format!(r#"{{"a": "{}"}}"#, m);
            assert_no_match!(q, &event, &format!("'{}' should NOT match '{}'", regex, m));
        }
    }
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
    let q = q!("p1" => r#"{"x": [{"shellstyle": "*abab"}]}"#);
    for text in ["abaabab", "ababab", "ababaabab", "abab"] {
        let event = format!(r#"{{"x": "{}"}}"#, text);
        assert_matches!(q, &event, vec!["p1"], &format!("Should match {}", text));
    }
}

#[test]
fn test_multiple_shellstyle_same_field() {
    // Test multiple shellstyle patterns on the SAME field
    // This is the merge_fas spinout bug reproduction test
    let q = q!(
        "suffix_bc" => r#"{"x": [{"shellstyle": "*bc"}]}"#,
        "suffix_xc" => r#"{"x": [{"shellstyle": "*xc"}]}"#,
        "prefix_ab" => r#"{"x": [{"shellstyle": "ab*"}]}"#
    );

    // Test suffix_bc pattern
    assert_has_match!(q, r#"{"x": "abc"}"#, "suffix_bc");
    // abc also matches ab* prefix pattern
    assert_has_match!(q, r#"{"x": "abc"}"#, "prefix_ab");

    // Test suffix_xc pattern
    assert_has_match!(q, r#"{"x": "axc"}"#, "suffix_xc");

    // Test prefix_ab pattern
    assert_has_match!(q, r#"{"x": "abcdef"}"#, "prefix_ab");

    // Test non-match
    assert_no_match!(q, r#"{"x": "xyz"}"#);
}

#[test]
fn test_anything_but_with_shellstyle() {
    // Based on Go quamina's TestAnythingButMerging (second part)
    // Tests that anything-but can be merged with shellstyle (NFA) patterns
    let q = q!(
        "pFooStar" => r#"{"z": [{"shellstyle": "foo*"}]}"#,
        "pAbFoot"  => r#"{"z": [{"anything-but": ["foot"]}]}"#
    );

    // "foo" should match BOTH patterns:
    // - pFooStar: matches "foo*"
    // - pAbFoot: "foo" is not "foot"
    assert_match_count!(q, r#"{"z": "foo"}"#, 2);
    assert_has_match!(q, r#"{"z": "foo"}"#, "pFooStar");
    assert_has_match!(q, r#"{"z": "foo"}"#, "pAbFoot");

    // "foot" should match only pFooStar:
    // - pFooStar: matches "foo*"
    // - pAbFoot: excluded (is "foot")
    assert_match_count!(q, r#"{"z": "foot"}"#, 1);
    assert_has_match!(q, r#"{"z": "foot"}"#, "pFooStar");

    // "bar" should match only pAbFoot:
    // - pFooStar: doesn't match "foo*"
    // - pAbFoot: "bar" is not "foot"
    assert_match_count!(q, r#"{"z": "bar"}"#, 1);
    assert_has_match!(q, r#"{"z": "bar"}"#, "pAbFoot");
}

#[test]
fn test_anything_but_with_overlapping_exclusions() {
    // Based on Go quamina's TestAnythingButAlgo
    // Tests anything-but with overlapping prefix exclusions
    let q = q!("notTTT" => r#"{"x": [{"anything-but": ["tim", "time", "timed"]}]}"#);

    // All excluded values should not match
    let excluded = ["tim", "time", "timed"];
    for val in excluded {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        assert_no_match!(q, event);
    }

    // Similar but non-excluded values should match
    let included = ["t", "ti", "timer", "timely", "timekeeper"];
    for val in included {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        assert_match_count!(q, event, 1);
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
    let q = q!("p1" => r#"{"x": [{"shellstyle": "*abab"}]}"#);

    // These should all match *abab
    let should_match = ["abab", "abaabab", "ababab", "ababaabab", "xxabab"];
    for val in should_match {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        assert_matches!(q, event, vec!["p1"]);
    }

    // These should not match
    let should_not = ["abab_", "aba", "ab", "xaba"];
    for val in should_not {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        assert_no_match!(q, event);
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
    for &(pattern, name) in &patterns {
        let q = q!(name => pattern);
        assert_match_count!(q, j, 1);
        assert_has_match!(q, j, name);
    }

    // Verify both patterns work when merged
    let mut q = Quamina::new();
    for (pattern, name) in &patterns {
        q.add_pattern(*name, pattern).unwrap();
    }
    assert_match_count!(q, j, 2);
    assert_has_match!(q, j, "p0");
    assert_has_match!(q, j, "p1");
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
        let q = q!("p1" => pattern);

        for val in should_match {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            assert_matches!(q, event, vec!["p1"]);
        }

        for val in should_not {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            assert_no_match!(q, event);
        }
    }
}

#[test]
fn test_wildcard_comprehensive() {
    // Based on Go quamina's exercisePattern tests from wildcard_test.go
    use crate::test_helpers::exercise_wildcard;

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
    use crate::test_helpers::exercise_wildcard;

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
    let q = q!("p1" => r#"{"x": [{"wildcard": "he\\\\\\*llo"}]}"#);

    // Should match "he\*llo" - in JSON, backslash needs escaping: "he\\*llo"
    assert_matches!(q, r#"{"x": "he\\*llo"}"#, vec!["p1"]);

    // Should NOT match - use raw strings for JSON to avoid double-escaping confusion
    let no_match_events = [
        r#"{"x": "hello"}"#,
        r#"{"x": "he\\\\llo"}"#, // he\\llo (2 backslashes)
        r#"{"x": "he\\llo"}"#,   // he\llo (1 backslash)
        r#"{"x": "he\\xxllo"}"#, // he\xxllo
    ];
    for event in no_match_events {
        assert_no_match!(q, event);
    }
}

#[test]
fn test_wildcard_escape_backslash_wildcard() {
    // Go line 41: `he\\\\*llo` (raw string = he\\\\*llo, 10 chars)
    // After JSON parse: he\\*llo (escaped backslash + wildcard)
    // Wildcard meaning: he + literal_backslash + wildcard + llo
    // Should match "he\" followed by anything followed by "llo"
    let q = q!("p1" => r#"{"x": [{"wildcard": "he\\\\*llo"}]}"#);

    // Should match - values with "he\" prefix and "llo" suffix
    let match_events = [
        r#"{"x": "he\\llo"}"#,   // he\llo (1 backslash, matches wildcard)
        r#"{"x": "he\\*llo"}"#,  // he\*llo
        r#"{"x": "he\\\\llo"}"#, // he\\llo (2 backslashes)
        r#"{"x": "he\\xxllo"}"#, // he\xxllo
    ];
    for event in match_events {
        assert_matches!(q, event, vec!["p1"]);
    }

    // Should NOT match
    let no_match_events = [
        r#"{"x": "hello"}"#,  // no backslash after he
        r#"{"x": "he\\ll"}"#, // doesn't end with llo
    ];
    for event in no_match_events {
        assert_no_match!(q, event);
    }
}

#[test]
fn test_shellstyle_duplicate_pattern() {
    // Go TestWildCardRuler: r4 and r5 are identical patterns
    let q = q!(
        "r4" => r#"{"c": [{"shellstyle": "xy*"}]}"#,
        "r5" => r#"{"c": [{"shellstyle": "xy*"}]}"#
    );

    assert_match_count!(q, r#"{"c": "xyzzz"}"#, 2);
    assert_has_match!(q, r#"{"c": "xyzzz"}"#, "r4");
    assert_has_match!(q, r#"{"c": "xyzzz"}"#, "r5");
}

#[test]
fn test_shellstyle_double_wildcard() {
    // Go TestWildCardRuler: r6 = 12*4*
    let q = q!("r6" => r#"{"d": [{"shellstyle": "12*4*"}]}"#);
    assert_matches!(
        q,
        r#"{"d": "12345"}"#,
        vec!["r6"],
        "12*4* should match 12345"
    );
    assert_no_match!(q, r#"{"d": "1235"}"#, "12*4* should not match 1235");
}

#[test]
fn test_shellstyle_zero_length_prefix() {
    // Go TestWildCardRuler: {"a": "bc"} should match *bc
    let q = q!("r1" => r#"{"a": [{"shellstyle": "*bc"}]}"#);
    assert_matches!(
        q,
        r#"{"a": "bc"}"#,
        vec!["r1"],
        "*bc should match bc (zero-length prefix)"
    );
}

#[test]
fn test_shellstyle_ruler_negative_cases() {
    // Go TestWildCardRuler: additional negative test cases
    let q = q!(
        "r2" => r#"{"b": [{"shellstyle": "d*f"}]}"#,
        "r4" => r#"{"c": [{"shellstyle": "xy*"}]}"#
    );

    // Should NOT match
    let cases = [
        (r#"{"c": "abc"}"#, "xy* should not match abc"),
        (r#"{"c": "abcxyz"}"#, "xy* should not match abcxyz"),
        (r#"{"b": "de"}"#, "d*f should not match de"),
    ];

    for (event, msg) in cases {
        assert_no_match!(q, event, msg);
    }
}

#[test]
fn test_wildcard_unicode_strings() {
    // Go TestWildcardMatching includes Unicode strings with Őz

    // Test *hello with Unicode prefix
    let q = q!("p1" => r#"{"x": [{"wildcard": "*hello"}]}"#);
    assert_matches!(
        q,
        r#"{"x": "23Őzhello"}"#,
        vec!["p1"],
        "*hello should match 23Őzhello"
    );

    // Test h*llo with Unicode in middle
    let q2 = q!("p2" => r#"{"x": [{"wildcard": "h*llo"}]}"#);
    assert_matches!(
        q2,
        r#"{"x": "hel23Őzlllo"}"#,
        vec!["p2"],
        "h*llo should match hel23Őzlllo"
    );

    // Test hello* with Unicode suffix
    let q3 = q!("p3" => r#"{"x": [{"wildcard": "hello*"}]}"#);
    assert_matches!(
        q3,
        r#"{"x": "hello23Őzlllo"}"#,
        vec!["p3"],
        "hello* should match hello23Őzlllo"
    );

    // Test h*l*o with Unicode
    let q4 = q!("p4" => r#"{"x": [{"wildcard": "h*l*o"}]}"#);
    assert_matches!(
        q4,
        r#"{"x": "hel23Őzlllo"}"#,
        vec!["p4"],
        "h*l*o should match hel23Őzlllo"
    );
}

#[test]
fn test_shellstyle_suffix_with_space() {
    // Go TestMakeShellStyleFA: *ST should match "STA ST"
    let q = q!("p1" => r#"{"x": [{"shellstyle": "*ST"}]}"#);

    assert_matches!(
        q,
        r#"{"x": "STA ST"}"#,
        vec!["p1"],
        "*ST should match 'STA ST'"
    );
    assert_matches!(q, r#"{"x": "1ST"}"#, vec!["p1"], "*ST should match '1ST'");
    assert_no_match!(q, r#"{"x": "STA"}"#, "*ST should not match 'STA'");
    assert_no_match!(
        q,
        r#"{"x": "STAST "}"#,
        "*ST should not match 'STAST ' (trailing space)"
    );
}

#[test]
fn test_shellstyle_prefix_negative() {
    // Go TestMakeShellStyleFA: foo* negative cases
    let q = q!("p1" => r#"{"x": [{"shellstyle": "foo*"}]}"#);
    assert_no_match!(q, r#"{"x": "afoo"}"#, "foo* should not match 'afoo'");
    assert_no_match!(q, r#"{"x": "fofo"}"#, "foo* should not match 'fofo'");
}

#[test]
fn test_shellstyle_suffix_negative() {
    // Go TestMakeShellStyleFA: *foo negative cases
    let q = q!("p1" => r#"{"x": [{"shellstyle": "*foo"}]}"#);
    assert_no_match!(q, r#"{"x": "foox"}"#, "*foo should not match 'foox'");
    assert_no_match!(q, r#"{"x": "afooo"}"#, "*foo should not match 'afooo'");
}

#[test]
fn test_shellstyle_contains_negative() {
    // Go TestMakeShellStyleFA: *foo* negative cases
    let q = q!("p1" => r#"{"x": [{"shellstyle": "*foo*"}]}"#);
    assert_no_match!(q, r#"{"x": "afoa"}"#, "*foo* should not match 'afoa'");
    assert_no_match!(
        q,
        r#"{"x": "fofofoxooxoo"}"#,
        "*foo* should not match 'fofofoxooxoo'"
    );
}

#[test]
fn test_shellstyle_double_wildcard_variations() {
    // Go TestMakeShellStyleFA: xx*yy*zz and *xx*yy* additional cases
    let q = q!("p1" => r#"{"x": [{"shellstyle": "xx*yy*zz"}]}"#);

    // Additional positive cases from Go
    for val in ["xxyycdzz", "xxabyyzz"] {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        assert_matches!(q, event, vec!["p1"]);
    }

    // Test *xx*yy* additional cases
    let q2 = q!("p2" => r#"{"x": [{"shellstyle": "*xx*yy*"}]}"#);

    for val in ["abxxcdyyef", "xxcdyyef", "abxxyyef", "xxcdyy", "xxyyef"] {
        let event = format!(r#"{{"x": "{}"}}"#, val);
        assert_matches!(q2, event, vec!["p2"]);
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

    for sample in REGEXP_SAMPLES.iter() {
        fn should_skip(re: &str) -> bool {
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
            if let Ok(tree) = parse_result {
                let (arena, start, field_matcher) = make_regexp_nfa_arena(tree);
                let mut bufs = ArenaNfaBuffers::new();

                for should_match in sample.matches {
                    let mut value: Vec<u8> = Vec::new();
                    value.push(b'"');
                    value.extend_from_slice(should_match.as_bytes());
                    value.push(b'"');
                    value.push(ARENA_VALUE_TERMINATOR);
                    bufs.clear();
                    traverse_arena_nfa(&arena, start, &value, &mut bufs);
                    let matched = bufs
                        .transitions
                        .iter()
                        .any(|&m| m == Arc::as_ptr(&field_matcher) as usize);
                    if !matched && !should_match.is_empty() {
                        problems += 1;
                    }
                }

                for should_not_match in sample.nomatches {
                    let mut value: Vec<u8> = Vec::new();
                    value.push(b'"');
                    value.extend_from_slice(should_not_match.as_bytes());
                    value.push(b'"');
                    value.push(ARENA_VALUE_TERMINATOR);
                    bufs.clear();
                    traverse_arena_nfa(&arena, start, &value, &mut bufs);
                    let matched = bufs
                        .transitions
                        .iter()
                        .any(|&m| m == Arc::as_ptr(&field_matcher) as usize);
                    if matched
                        && !(should_not_match.is_empty()
                            && star_samples_matching_empty(sample.regex))
                        && !should_not_match.is_empty()
                    {
                        problems += 1;
                    }
                }
            }
        } else if parse_result.is_ok() {
            let is_extension = is_known_extension(sample.regex);
            if !is_extension {
                problems += 1;
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
    let (arena, start, fm) = make_regexp_nfa_arena(root);
    bufs.clear();
    traverse_arena_nfa(
        &arena,
        start,
        &[b'"', b'a', b'"', ARENA_VALUE_TERMINATOR],
        &mut bufs,
    );
    assert!(bufs
        .transitions
        .iter()
        .any(|&m| m == Arc::as_ptr(&fm) as usize));
    bufs.clear();
    traverse_arena_nfa(
        &arena,
        start,
        &[b'"', b'x', b'"', ARENA_VALUE_TERMINATOR],
        &mut bufs,
    );
    assert!(!bufs
        .transitions
        .iter()
        .any(|&m| m == Arc::as_ptr(&fm) as usize));

    let root = parse_regexp("a(h|i)z").unwrap();
    let (arena, start, fm) = make_regexp_nfa_arena(root);
    bufs.clear();
    traverse_arena_nfa(
        &arena,
        start,
        &[b'"', b'a', b'h', b'z', b'"', ARENA_VALUE_TERMINATOR],
        &mut bufs,
    );
    assert!(bufs
        .transitions
        .iter()
        .any(|&m| m == Arc::as_ptr(&fm) as usize));

    let root = parse_regexp("[a-c]").unwrap();
    let (arena, start, fm) = make_regexp_nfa_arena(root);
    bufs.clear();
    traverse_arena_nfa(
        &arena,
        start,
        &[b'"', b'b', b'"', ARENA_VALUE_TERMINATOR],
        &mut bufs,
    );
    assert!(bufs
        .transitions
        .iter()
        .any(|&m| m == Arc::as_ptr(&fm) as usize));
    bufs.clear();
    traverse_arena_nfa(
        &arena,
        start,
        &[b'"', b'z', b'"', ARENA_VALUE_TERMINATOR],
        &mut bufs,
    );
    assert!(!bufs
        .transitions
        .iter()
        .any(|&m| m == Arc::as_ptr(&fm) as usize));

    let root = parse_regexp("a.b").unwrap();
    let (arena, start, fm) = make_regexp_nfa_arena(root);
    bufs.clear();
    traverse_arena_nfa(
        &arena,
        start,
        &[b'"', b'a', b'x', b'b', b'"', ARENA_VALUE_TERMINATOR],
        &mut bufs,
    );
    assert!(bufs
        .transitions
        .iter()
        .any(|&m| m == Arc::as_ptr(&fm) as usize));
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
    let q1 = q!("p1" => r#"{"x": ["hello\"world"]}"#);
    assert_matches!(
        q1,
        r#"{"x": "hello\"world"}"#,
        vec!["p1"],
        "Quote escape should match"
    );

    // Test: \/ (forward slash - optional in JSON but must be handled)
    let q2 = q!("p2" => r#"{"x": ["a/b"]}"#);
    assert_matches!(
        q2,
        r#"{"x": "a\/b"}"#,
        vec!["p2"],
        "Forward slash escape should match"
    );

    // Test: \b (backspace, 0x08)
    let pattern_with_backspace = format!(r#"{{"x": ["a{}b"]}}"#, '\x08');
    let q3 = q!("p3" => &pattern_with_backspace);
    assert_matches!(
        q3,
        r#"{"x": "a\bb"}"#,
        vec!["p3"],
        "Backspace escape should match"
    );

    // Test: \f (form feed, 0x0c)
    let pattern_with_formfeed = format!(r#"{{"x": ["a{}b"]}}"#, '\x0c');
    let q4 = q!("p4" => &pattern_with_formfeed);
    assert_matches!(
        q4,
        r#"{"x": "a\fb"}"#,
        vec!["p4"],
        "Form feed escape should match"
    );

    // Test: \r (carriage return)
    let q5 = q!("p5" => r#"{"x": ["a\rb"]}"#);
    assert_matches!(
        q5,
        r#"{"x": "a\rb"}"#,
        vec!["p5"],
        "Carriage return escape should match"
    );
}

// MIRI SKIP RATIONALE: CIDR tests with multiple prefixes are slow under Miri
#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_ipv4_various_prefixes() {
    let q = q!(
        "class_a" => r#"{"ip": [{"cidr": "10.0.0.0/8"}]}"#,
        "class_b" => r#"{"ip": [{"cidr": "172.16.0.0/16"}]}"#,
        "class_c" => r#"{"ip": [{"cidr": "192.168.1.0/24"}]}"#,
        "single"  => r#"{"ip": [{"cidr": "8.8.8.8/32"}]}"#
    );

    assert_has_match!(q, r#"{"ip": "10.255.255.255"}"#, "class_a");
    assert_has_match!(q, r#"{"ip": "172.16.255.255"}"#, "class_b");
    assert_has_match!(q, r#"{"ip": "192.168.1.100"}"#, "class_c");
    assert_has_match!(q, r#"{"ip": "8.8.8.8"}"#, "single");
    assert_no_has_match!(q, r#"{"ip": "8.8.8.9"}"#, "single");
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_ipv6_basic() {
    let q = q!("p1" => r#"{"sourceIP": [{"cidr": "2001:db8::/32"}]}"#);
    assert_matches!(
        q,
        r#"{"sourceIP": "2001:db8:0:0:0:0:0:1"}"#,
        vec!["p1"],
        "IPv6 in range"
    );
    assert_matches!(
        q,
        r#"{"sourceIP": "2001:db8:ffff:ffff:ffff:ffff:ffff:ffff"}"#,
        vec!["p1"],
        "IPv6 at end of range"
    );
    assert_no_match!(
        q,
        r#"{"sourceIP": "2001:db9:0:0:0:0:0:1"}"#,
        "IPv6 outside range"
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_ipv6_shorthand() {
    let q = q!("loopback" => r#"{"ip": [{"cidr": "::1/128"}]}"#);
    assert_matches!(
        q,
        r#"{"ip": "0:0:0:0:0:0:0:1"}"#,
        vec!["loopback"],
        "Loopback should match"
    );
    assert_no_match!(
        q,
        r#"{"ip": "0:0:0:0:0:0:0:2"}"#,
        "Non-loopback should not match /128"
    );
}

// MIRI SKIP RATIONALE: CIDR pattern construction + matching against non-IP values takes ~151s
// under Miri. Coverage: test_cidr_non_ip_values_miri_friendly exercises the same non-IP
// rejection logic using the cidr_invalid_patterns path which avoids full automaton traversal.
#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_non_ip_values() {
    let q = q!("p1" => r#"{"ip": [{"cidr": "10.0.0.0/8"}]}"#);
    assert_no_match!(
        q,
        r#"{"ip": "not-an-ip"}"#,
        "Non-IP string should not match CIDR"
    );
    assert_no_match!(q, r#"{"ip": ""}"#, "Empty string should not match CIDR");
    assert_no_match!(q, r#"{"ip": 12345}"#, "Number should not match CIDR");
}

/// Miri-only: verifies CIDR pattern rejects invalid inputs without full automaton traversal.
#[test]
#[cfg(miri)]
fn test_cidr_non_ip_values_miri_friendly() {
    let q = q!("p1" => r#"{"ip": [{"cidr": "127.0.0.1/32"}]}"#);
    assert_no_match!(q, r#"{"ip": "not-an-ip"}"#, "Non-IP string");
    assert_no_match!(q, r#"{"ip": 12345}"#, "Number");
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_cidr_with_other_matchers() {
    let q = q!(
        "internal" => r#"{"sourceIP": [{"cidr": "10.0.0.0/8"}]}"#,
        "specific" => r#"{"sourceIP": ["10.0.0.1"]}"#,
        "status"   => r#"{"status": ["active"]}"#
    );

    // Event matching CIDR, exact, and status
    assert_has_match!(
        q,
        r#"{"sourceIP": "10.0.0.1", "status": "active"}"#,
        "internal"
    );
    assert_has_match!(
        q,
        r#"{"sourceIP": "10.0.0.1", "status": "active"}"#,
        "specific"
    );
    assert_has_match!(
        q,
        r#"{"sourceIP": "10.0.0.1", "status": "active"}"#,
        "status"
    );

    // Event matching only CIDR
    assert_has_match!(q, r#"{"sourceIP": "10.0.0.2"}"#, "internal");
    assert_no_has_match!(q, r#"{"sourceIP": "10.0.0.2"}"#, "specific");
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
    let q = q!("la" => r#"{"x": [{"regexp": "foo(?=bar)bar"}]}"#);
    assert_has_match!(q, r#"{"x": "foobar"}"#, "la");
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

/// Regression test: lookaround buffer reuse must not carry stale state.
///
/// Mixes lookahead and exact patterns on the same field, then matches
/// repeatedly. If arena_bufs carries stale transitions between calls,
/// later iterations will produce wrong results.
#[test]
#[cfg_attr(miri, ignore)]
fn test_lookaround_buffer_reuse_no_stale_state() {
    let mut q = Quamina::<String>::new();

    // Lookahead pattern (uses multi_condition_nfas path)
    q.add_pattern(
        "look".to_string(),
        r#"{"v": [{"regexp": "foo(?=bar)bar"}]}"#,
    )
    .unwrap();

    // Exact pattern on a DIFFERENT field (avoids arena merge interference)
    q.add_pattern("exact".to_string(), r#"{"w": ["hello"]}"#)
        .unwrap();

    for _ in 0..200 {
        let m = q
            .matches_for_event(r#"{"v": "foobar"}"#.as_bytes())
            .unwrap();
        assert!(
            m.contains(&"look".to_string()),
            "foobar should match lookahead"
        );

        let m = q.matches_for_event(r#"{"w": "hello"}"#.as_bytes()).unwrap();
        assert!(m.contains(&"exact".to_string()), "hello should match exact");

        let m = q
            .matches_for_event(r#"{"v": "nomatch"}"#.as_bytes())
            .unwrap();
        assert!(m.is_empty(), "nomatch should match nothing");
    }
}

/// Regression test: multiple lookaround conditions must not cross-contaminate.
///
/// When checking condition A then condition B, the buffer must be fully cleared
/// between traversals so condition A's transitions don't leak into condition B.
#[test]
#[cfg_attr(miri, ignore)]
fn test_lookaround_multiple_conditions_no_cross_contamination() {
    let mut q = Quamina::<String>::new();

    // Positive lookahead: matches "foobar" but not "foobaz"
    q.add_pattern("pos".to_string(), r#"{"v": [{"regexp": "foo(?=bar)bar"}]}"#)
        .unwrap();

    // Negative lookahead: matches "foobaz" but not "foobar"
    q.add_pattern("neg".to_string(), r#"{"v": [{"regexp": "foo(?!bar)baz"}]}"#)
        .unwrap();

    // Run many times — cross-contamination would show up as intermittent failures
    for _ in 0..200 {
        let m = q
            .matches_for_event(r#"{"v": "foobar"}"#.as_bytes())
            .unwrap();
        assert!(m.contains(&"pos".to_string()), "foobar should match pos");
        assert!(
            !m.contains(&"neg".to_string()),
            "foobar should not match neg"
        );

        let m = q
            .matches_for_event(r#"{"v": "foobaz"}"#.as_bytes())
            .unwrap();
        assert!(
            !m.contains(&"pos".to_string()),
            "foobaz should not match pos"
        );
        assert!(m.contains(&"neg".to_string()), "foobaz should match neg");
    }
}

/// Miri-friendly variant of test_lookaround_buffer_reuse_no_stale_state.
///
/// Single iteration to verify buffer clearing between lookahead and exact
/// pattern matches without the 200-iteration loop.
#[test]
fn test_lookaround_buffer_reuse_no_stale_state_miri_friendly() {
    let mut q = Quamina::<String>::new();

    q.add_pattern(
        "look".to_string(),
        r#"{"v": [{"regexp": "foo(?=bar)bar"}]}"#,
    )
    .unwrap();
    q.add_pattern("exact".to_string(), r#"{"w": ["hello"]}"#)
        .unwrap();

    let m = q
        .matches_for_event(r#"{"v": "foobar"}"#.as_bytes())
        .unwrap();
    assert!(
        m.contains(&"look".to_string()),
        "foobar should match lookahead"
    );

    let m = q.matches_for_event(r#"{"w": "hello"}"#.as_bytes()).unwrap();
    assert!(m.contains(&"exact".to_string()), "hello should match exact");

    let m = q
        .matches_for_event(r#"{"v": "nomatch"}"#.as_bytes())
        .unwrap();
    assert!(m.is_empty(), "nomatch should match nothing");
}

/// Miri-friendly variant of test_lookaround_multiple_conditions_no_cross_contamination.
///
/// Single iteration verifying positive and negative lookahead patterns
/// don't cross-contaminate each other's match results.
#[test]
fn test_lookaround_multiple_conditions_no_cross_contamination_miri_friendly() {
    let mut q = Quamina::<String>::new();

    q.add_pattern("pos".to_string(), r#"{"v": [{"regexp": "foo(?=bar)bar"}]}"#)
        .unwrap();
    q.add_pattern("neg".to_string(), r#"{"v": [{"regexp": "foo(?!bar)baz"}]}"#)
        .unwrap();

    let m = q
        .matches_for_event(r#"{"v": "foobar"}"#.as_bytes())
        .unwrap();
    assert!(m.contains(&"pos".to_string()), "foobar should match pos");
    assert!(
        !m.contains(&"neg".to_string()),
        "foobar should not match neg"
    );

    let m = q
        .matches_for_event(r#"{"v": "foobaz"}"#.as_bytes())
        .unwrap();
    assert!(
        !m.contains(&"pos".to_string()),
        "foobaz should not match pos"
    );
    assert!(m.contains(&"neg".to_string()), "foobaz should match neg");
}

#[test]
fn test_shellstyle_subset_overlap_same_field() {
    // Go's TestWildCardRuler: two shellstyle patterns on the same field where
    // one's match set is a subset of the other (d*f matches everything d*ff does, plus more)
    let q = q!(
        "r1" => r#"{"b": [{"shellstyle": "d*f"}]}"#,
        "r2" => r#"{"b": [{"shellstyle": "d*ff"}]}"#
    );

    // "dexeff" matches both: d*f (ends in f) and d*ff (ends in ff)
    assert_has_match!(q, r#"{"b": "dexeff"}"#, "r1");
    assert_has_match!(q, r#"{"b": "dexeff"}"#, "r2");
    assert_match_count!(q, r#"{"b": "dexeff"}"#, 2);

    // "def" matches d*f only (ends in single f, not ff)
    assert_has_match!(q, r#"{"b": "def"}"#, "r1");
    assert_no_has_match!(q, r#"{"b": "def"}"#, "r2");

    // "df" matches d*f (wildcard matches zero chars)
    assert_has_match!(q, r#"{"b": "df"}"#, "r1");
    assert_no_has_match!(q, r#"{"b": "df"}"#, "r2");

    // "dff" matches both: d*f (wildcard matches "f", ending in f) and d*ff (ends in ff)
    assert_has_match!(q, r#"{"b": "dff"}"#, "r1");
    assert_has_match!(q, r#"{"b": "dff"}"#, "r2");

    // "hello" matches neither
    assert_no_match!(q, r#"{"b": "hello"}"#);
}

#[test]
fn test_equals_ignore_case_length_boundaries() {
    // Go's TestEqualsIgnoreCaseMatching: verify length-sensitive matching
    let q = q!("p1" => r#"{"name": [{"equals-ignore-case": "XyZ"}]}"#);

    // Exact match (various cases) should match
    assert_matches!(q, r#"{"name": "xyz"}"#, vec!["p1"], "Lowercase");
    assert_matches!(q, r#"{"name": "XYZ"}"#, vec!["p1"], "Uppercase");

    // Rejects: longer, shorter, superstring, different, empty
    for (event, desc) in [
        (r#"{"name": "xyzz"}"#, "extra trailing char"),
        (r#"{"name": "xy"}"#, "shorter"),
        (r#"{"name": "ABCXYZ"}"#, "contains but not equal"),
        (r#"{"name": "abc"}"#, "different"),
        (r#"{"name": ""}"#, "empty"),
    ] {
        assert_no_match!(q, event, desc);
    }
}
