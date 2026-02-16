//! Fuzz target for event matching (full integration).
//!
//! This target exercises the complete matching pipeline:
//! 1. JSON event parsing (flatten_json)
//! 2. Field extraction based on pattern paths
//! 3. Automaton traversal for matching
//!
//! We pre-load a diverse set of patterns covering all matcher types,
//! then fuzz with arbitrary event bytes. This tests:
//! - Event parsing with realistic field filtering
//! - Automaton state transitions with various field values
//! - Interaction between multiple pattern types
//! - Edge cases in the matching logic
//!
//! The matcher should never panic - only return Ok or Err.

#![no_main]

use libfuzzer_sys::fuzz_target;
use once_cell::sync::Lazy;
use quamina::Quamina;

/// Pre-built Quamina instance with diverse patterns.
/// Using Lazy to initialize once across all fuzz iterations.
/// This gives the fuzzer a realistic matching environment.
static MATCHER: Lazy<Quamina<&'static str>> = Lazy::new(|| {
    let mut q = Quamina::new();

    // Exact string matches - tests literal value comparison
    let _ = q.add_pattern("exact-status", r#"{"status": ["active"]}"#);
    let _ = q.add_pattern("exact-multi", r#"{"status": ["pending", "shipped"]}"#);

    // Existence checks - tests field presence detection
    let _ = q.add_pattern("exists-true", r#"{"name": [{"exists": true}]}"#);
    let _ = q.add_pattern("exists-false", r#"{"optional": [{"exists": false}]}"#);

    // Prefix/suffix - tests string boundary matching
    let _ = q.add_pattern("prefix", r#"{"env": [{"prefix": "prod-"}]}"#);
    let _ = q.add_pattern("suffix", r#"{"file": [{"suffix": ".json"}]}"#);

    // Wildcards - tests glob-style pattern matching
    let _ = q.add_pattern("wildcard", r#"{"path": [{"wildcard": "*/src/*.rs"}]}"#);
    let _ = q.add_pattern("shellstyle", r#"{"cmd": [{"shellstyle": "test*"}]}"#);

    // Anything-but - tests negation logic
    let _ = q.add_pattern("anything-but", r#"{"type": [{"anything-but": ["deleted"]}]}"#);

    // Case-insensitive - tests case folding
    let _ = q.add_pattern("ignore-case", r#"{"user": [{"equals-ignore-case": "Admin"}]}"#);

    // Numeric comparisons - tests number parsing and comparison
    let _ = q.add_pattern("numeric-lt", r#"{"price": [{"numeric": ["<", 100]}]}"#);
    let _ = q.add_pattern("numeric-range", r#"{"score": [{"numeric": [">=", 0, "<=", 100]}]}"#);

    // Regex - tests regular expression matching
    let _ = q.add_pattern("regex", r#"{"code": [{"regex": "^[A-Z]{3}-[0-9]+$"}]}"#);

    // Word boundary - tests ~b/~B automaton paths
    let _ = q.add_pattern("word-boundary", r#"{"tag": [{"regexp": "~bcat~b"}]}"#);

    // CIDR - tests IP address matching
    let _ = q.add_pattern("cidr", r#"{"ip": [{"cidr": "10.0.0.0/8"}]}"#);

    // Nested fields - tests path traversal
    let _ = q.add_pattern("nested", r#"{"user": {"role": ["admin"]}}"#);
    let _ = q.add_pattern("deep", r#"{"a": {"b": {"c": {"d": ["value"]}}}}"#);

    // Multiple fields (AND) - tests multi-field matching
    let _ = q.add_pattern("multi-field", r#"{"status": ["active"], "priority": ["high"]}"#);

    q
});

fuzz_target!(|data: &[u8]| {
    // Match the arbitrary bytes as a JSON event.
    // The matcher handles JSON parsing internally.
    // Invalid JSON will return an error, which is fine.
    let _ = MATCHER.matches_for_event(data);

    // Also test the other matching APIs for coverage
    let _ = MATCHER.has_matches(data);
    let _ = MATCHER.count_matches(data);
});
