//! Benchmarks for quamina-rs pattern matching
//!
//! Comparable benchmarks to Go's flatten_json_bench_test.go and citylots_bench_test.go

use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};
use flate2::read::GzDecoder;
use quamina::automaton::arena::{
    traverse_arena_nfa, ArenaNfaBuffers, ArenaSmallTable, StateArena, StateId,
    ARENA_VALUE_TERMINATOR,
};
use quamina::automaton::{EventField, FieldMatcher, ThreadSafeCoreMatcher};
use quamina::flatten_json::FlattenJsonState;
use quamina::json::Matcher;
use quamina::segments_tree::SegmentsTree;
use quamina::Quamina;
use std::io::{BufRead, BufReader};
use std::sync::Arc;

// Status.json patterns (matching Go benchmarks)
const PATTERN_CONTEXT: &str = r#"{ "context": { "user_id": [9034], "friends_count": [158] } }"#;
const PATTERN_MIDDLE_NESTED: &str = r#"{ "payload": { "user": { "id_str": ["903487807"] } } }"#;
const PATTERN_LAST_FIELD: &str = r#"{ "payload": { "lang_value": ["ja"] } }"#;

fn load_status_json() -> Vec<u8> {
    std::fs::read("testdata/status.json").expect("Failed to read testdata/status.json")
}

fn bench_exact_match(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

    let event = r#"{"status": "active", "id": 123}"#.as_bytes();

    c.bench_function("exact_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_multiple_patterns(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..100 {
        q.add_pattern(
            format!("p{}", i),
            &format!(r#"{{"status": ["status_{}"]}}"#, i),
        )
        .unwrap();
    }

    let event = r#"{"status": "status_50"}"#.as_bytes();

    c.bench_function("100_patterns", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_complex_event(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"user": {"role": ["admin"]}}"#)
        .unwrap();

    let event =
        r#"{"user": {"role": "admin", "name": "alice", "id": 123}, "timestamp": 1234567890}"#
            .as_bytes();

    c.bench_function("nested_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_regex_match(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "p1",
        r#"{"email": [{"regex": "^[a-z]+@[a-z]+\\.[a-z]+$"}]}"#,
    )
    .unwrap();

    let event = r#"{"email": "alice@example.com"}"#.as_bytes();

    c.bench_function("regex_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_no_match(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..100 {
        q.add_pattern(
            format!("p{}", i),
            &format!(r#"{{"status": ["status_{}"]}}"#, i),
        )
        .unwrap();
    }

    let event = r#"{"status": "no_match_here"}"#.as_bytes();

    c.bench_function("100_patterns_no_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_has_matches(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..100 {
        q.add_pattern(
            format!("p{}", i),
            &format!(r#"{{"status": ["status_{}"]}}"#, i),
        )
        .unwrap();
    }

    // First pattern matches - early exit
    let event = r#"{"status": "status_0"}"#.as_bytes();

    c.bench_function("has_matches_early_exit", |b| {
        b.iter(|| q.has_matches(black_box(event)).unwrap())
    });
}

/// Benchmark with diverse patterns - each pattern uses a DIFFERENT field
/// This is a realistic scenario where field indexing would help
fn bench_diverse_patterns(c: &mut Criterion) {
    let mut q = Quamina::new();
    // Add 100 patterns, each using a unique field name
    for i in 0..100 {
        q.add_pattern(
            format!("p{}", i),
            &format!(r#"{{"field_{}": ["value_{}"]}}"#, i, i),
        )
        .unwrap();
    }

    // Event only has field_50, so only 1 of 100 patterns could match
    let event = r#"{"field_50": "value_50", "other": "data"}"#.as_bytes();

    c.bench_function("100_diverse_patterns_1_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Benchmark with diverse patterns and no match
fn bench_diverse_no_match(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..100 {
        q.add_pattern(
            format!("p{}", i),
            &format!(r#"{{"field_{}": ["value_{}"]}}"#, i, i),
        )
        .unwrap();
    }

    // Event has a field that doesn't match any pattern
    let event = r#"{"unrelated_field": "some_value"}"#.as_bytes();

    c.bench_function("100_diverse_patterns_no_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

// === Benchmarks comparable to Go's flatten_json_bench_test.go ===

/// Flatten-only benchmark for context fields (compare to Go's Benchmark_JsonFlattener_ContextFields)
fn bench_flatten_context_fields(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("context", PATTERN_CONTEXT).unwrap();
    let event = load_status_json();

    c.bench_function("flatten_context_fields", |b| {
        b.iter(|| q.flatten_only(black_box(&event)).unwrap())
    });
}

/// Direct flattener benchmark without Mutex overhead
fn bench_flatten_direct_context_fields(c: &mut Criterion) {
    let mut tree = SegmentsTree::new();
    tree.add("context\nuser_id");
    tree.add("context\nfriends_count");
    let mut flattener = FlattenJsonState::new();
    let event = load_status_json();

    c.bench_function("flatten_direct_context_fields", |b| {
        b.iter(|| {
            let fields = flattener.flatten(black_box(&event), &tree).unwrap();
            black_box(fields.len())
        })
    });
}

/// Flatten-only benchmark for middle nested field
fn bench_flatten_middle_nested(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("middle", PATTERN_MIDDLE_NESTED).unwrap();
    let event = load_status_json();

    c.bench_function("flatten_middle_nested", |b| {
        b.iter(|| q.flatten_only(black_box(&event)).unwrap())
    });
}

/// Flatten-only benchmark for last field
fn bench_flatten_last_field(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("last", PATTERN_LAST_FIELD).unwrap();
    let event = load_status_json();

    c.bench_function("flatten_last_field", |b| {
        b.iter(|| q.flatten_only(black_box(&event)).unwrap())
    });
}

/// Match on context fields (early in large JSON)
fn bench_status_context_fields(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("context", PATTERN_CONTEXT).unwrap();
    let event = load_status_json();

    // Verify it matches
    let matches = q.matches_for_event(&event).unwrap();
    assert_eq!(matches.len(), 1);

    c.bench_function("status_context_fields", |b| {
        b.iter(|| q.matches_for_event(black_box(&event)).unwrap())
    });
}

/// Match on middle nested field (deep in large JSON)
fn bench_status_middle_nested(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("middle", PATTERN_MIDDLE_NESTED).unwrap();
    let event = load_status_json();

    let matches = q.matches_for_event(&event).unwrap();
    assert_eq!(matches.len(), 1);

    c.bench_function("status_middle_nested", |b| {
        b.iter(|| q.matches_for_event(black_box(&event)).unwrap())
    });
}

/// Match on last field (end of large JSON)
fn bench_status_last_field(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("last", PATTERN_LAST_FIELD).unwrap();
    let event = load_status_json();

    let matches = q.matches_for_event(&event).unwrap();
    assert_eq!(matches.len(), 1);

    c.bench_function("status_last_field", |b| {
        b.iter(|| q.matches_for_event(black_box(&event)).unwrap())
    });
}

/// Combined benchmark: all three patterns on large JSON
fn bench_status_all_patterns(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("context", PATTERN_CONTEXT).unwrap();
    q.add_pattern("middle", PATTERN_MIDDLE_NESTED).unwrap();
    q.add_pattern("last", PATTERN_LAST_FIELD).unwrap();
    let event = load_status_json();

    let matches = q.matches_for_event(&event).unwrap();
    assert_eq!(matches.len(), 3);

    c.bench_function("status_all_three_patterns", |b| {
        b.iter(|| q.matches_for_event(black_box(&event)).unwrap())
    });
}

/// Shellstyle patterns benchmark (comparable to Go's TestBigShellStyle)
fn bench_shellstyle_alphabet(c: &mut Criterion) {
    let mut q = Quamina::new();
    // Add 26 shellstyle patterns (A* through Z*)
    for letter in 'A'..='Z' {
        q.add_pattern(
            letter.to_string(),
            &format!(r#"{{"name": [{{"shellstyle": "{}*"}}]}}"#, letter),
        )
        .unwrap();
    }

    let event = r#"{"name": "BELVEDERE", "other": "data"}"#.as_bytes();

    c.bench_function("shellstyle_26_patterns", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Comprehensive shellstyle benchmark (comparable to Go's BenchmarkShellstyleMultiMatch)
/// Tests multiple character sets including ASCII, CJK, and emoji
fn bench_shellstyle_multi_match(c: &mut Criterion) {
    let mut q = Quamina::new();

    // Add 16 letter patterns (A* through P*)
    for letter in [
        "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
    ] {
        q.add_pattern(
            letter.to_string(),
            &format!(r#"{{"STREET": [{{"shellstyle": "{}*"}}]}}"#, letter),
        )
        .unwrap();
    }

    // Add funky patterns with multiple wildcards (trigger complex NFA traversal)
    let funky_patterns = [
        ("funky1", "*E*E*E*"),
        ("funky2", "*A*B*"),
        ("funky3", "*N*P*"),
        ("funky4", "*O*O*O*"),
    ];
    for (name, shellstyle) in funky_patterns {
        q.add_pattern(
            name.to_string(),
            &format!(r#"{{"STREET": [{{"shellstyle": "{}"}}]}}"#, shellstyle),
        )
        .unwrap();
    }

    // Add CJK patterns to test Unicode handling
    let cjk_patterns = [
        ("jp1", "*東京*"),
        ("jp2", "新*"),
        ("cn1", "*北京*"),
        ("cn2", "上海*"),
        ("kr1", "*서울*"),
    ];
    for (name, shellstyle) in cjk_patterns {
        q.add_pattern(
            name.to_string(),
            &format!(r#"{{"STREET": [{{"shellstyle": "{}"}}]}}"#, shellstyle),
        )
        .unwrap();
    }

    // Add emoji patterns to test multi-byte UTF-8 sequences
    let emoji_patterns = [
        ("emoji1", "*🎉*"),
        ("emoji2", "🚀*"),
        ("emoji3", "*❤️*"),
        ("emoji4", "*🌟*🎯*"),
    ];
    for (name, shellstyle) in emoji_patterns {
        q.add_pattern(
            name.to_string(),
            &format!(r#"{{"STREET": [{{"shellstyle": "{}"}}]}}"#, shellstyle),
        )
        .unwrap();
    }

    // Events that will match and require NFA traversal
    let events: Vec<Vec<u8>> = vec![
        // English streets
        r#"{"STREET": "ASHBURY"}"#.into(),
        r#"{"STREET": "BELVEDERE"}"#.into(),
        r#"{"STREET": "CRANLEIGH"}"#.into(),
        r#"{"STREET": "DEER PARK"}"#.into(),
        r#"{"STREET": "EMBARCADERO"}"#.into(),
        r#"{"STREET": "FULTON"}"#.into(),
        r#"{"STREET": "GEARY"}"#.into(),
        r#"{"STREET": "HAIGHT"}"#.into(),
        r#"{"STREET": "IRVING"}"#.into(),
        r#"{"STREET": "JUDAH"}"#.into(),
        r#"{"STREET": "KEARNY"}"#.into(),
        r#"{"STREET": "LOMBARD"}"#.into(),
        r#"{"STREET": "MARKET"}"#.into(),
        r#"{"STREET": "NORIEGA"}"#.into(),
        r#"{"STREET": "OCTAVIA"}"#.into(),
        r#"{"STREET": "POLK"}"#.into(),
        // Streets with multiple vowels for funky patterns
        r#"{"STREET": "EMBARCADERO STREET"}"#.into(),
        r#"{"STREET": "ALABAMA"}"#.into(),
        r#"{"STREET": "NAPOLEON"}"#.into(),
        r#"{"STREET": "COLORADO"}"#.into(),
        // CJK streets
        r#"{"STREET": "東京タワー通り"}"#.into(),
        r#"{"STREET": "新宿駅前"}"#.into(),
        r#"{"STREET": "北京路"}"#.into(),
        r#"{"STREET": "上海南京路"}"#.into(),
        r#"{"STREET": "서울대로"}"#.into(),
        // Emoji streets
        r#"{"STREET": "Party Street 🎉"}"#.into(),
        r#"{"STREET": "🚀 Rocket Road"}"#.into(),
        r#"{"STREET": "Love ❤️ Lane"}"#.into(),
        r#"{"STREET": "Star 🌟 Plaza 🎯"}"#.into(),
        // Mixed
        r#"{"STREET": "Tokyo 東京 Street"}"#.into(),
        r#"{"STREET": "Happy 😊 Avenue"}"#.into(),
    ];

    // Verify all events match at least one pattern
    for event in &events {
        let matches = q.matches_for_event(event).unwrap();
        assert!(
            !matches.is_empty(),
            "no matches for event: {}",
            String::from_utf8_lossy(event)
        );
    }

    c.bench_function("shellstyle_multi_match", |b| {
        b.iter(|| {
            for event in &events {
                let _ = q.matches_for_event(black_box(event)).unwrap();
            }
        })
    });
}

/// Prefix patterns benchmark
fn bench_prefix_patterns(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..100 {
        q.add_pattern(
            format!("p{}", i),
            &format!(r#"{{"path": [{{"prefix": "/api/v{}/users"}}]}}"#, i),
        )
        .unwrap();
    }

    let event = r#"{"path": "/api/v50/users/123"}"#.as_bytes();

    c.bench_function("100_prefix_patterns", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Anything-but patterns benchmark
fn bench_anything_but(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "not_error",
        r#"{"status": [{"anything-but": ["error", "failed", "timeout"]}]}"#,
    )
    .unwrap();

    let event = r#"{"status": "success", "code": 200}"#.as_bytes();

    c.bench_function("anything_but_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Multi-field AND patterns
fn bench_multi_field_and(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "specific",
        r#"{"region": ["us-east-1"], "service": ["lambda"], "level": ["error"]}"#,
    )
    .unwrap();

    let event =
        r#"{"region": "us-east-1", "service": "lambda", "level": "error", "message": "timeout"}"#
            .as_bytes();

    c.bench_function("multi_field_and_3_fields", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Numeric range patterns benchmark (single-sided: < 100)
fn bench_numeric_range_single(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("below_100", r#"{"score": [{"numeric": ["<", 100]}]}"#)
        .unwrap();

    let event = r#"{"score": 50, "name": "test"}"#.as_bytes();

    c.bench_function("numeric_range_single", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Numeric range patterns benchmark (two-sided: >= 0 AND <= 100)
fn bench_numeric_range_two_sided(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "range_0_100",
        r#"{"score": [{"numeric": [">=", 0, "<=", 100]}]}"#,
    )
    .unwrap();

    let event = r#"{"score": 50, "name": "test"}"#.as_bytes();

    c.bench_function("numeric_range_two_sided", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Multiple numeric range patterns
fn bench_numeric_range_multiple(c: &mut Criterion) {
    let mut q = Quamina::new();
    // Add 10 non-overlapping ranges
    for i in 0..10 {
        let lower = i * 100;
        let upper = (i + 1) * 100;
        q.add_pattern(
            format!("range_{}", i),
            &format!(
                r#"{{"score": [{{"numeric": [">=", {}, "<", {}]}}]}}"#,
                lower, upper
            ),
        )
        .unwrap();
    }

    // Event value 550 should match range_5 (500-600)
    let event = r#"{"score": 550}"#.as_bytes();

    c.bench_function("numeric_range_10_patterns", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Exact float matching benchmark (comparable to Go's BenchmarkNumberMatching)
/// Tests matching exact float literals like {"x": [0.123456, 0.789012, ...]}
fn bench_number_matching(c: &mut Criterion) {
    use rand::prelude::*;

    // Use fixed seed for reproducibility (Go uses 2325)
    let mut rng = rand::rngs::StdRng::seed_from_u64(2325);

    // Generate 10 random float values for the pattern
    let targets: Vec<f64> = (0..10).map(|_| rng.random::<f64>()).collect();

    // Build pattern with 10 exact float values
    let values: String = targets
        .iter()
        .map(|f| format!("{:.6}", f))
        .collect::<Vec<_>>()
        .join(", ");
    let pattern = format!(r#"{{"x": [{}]}}"#, values);

    let mut q = Quamina::new();
    q.add_pattern("P", &pattern).unwrap();

    // Pre-generate events: alternating between matching (target value) and non-matching (random)
    let events: Vec<Vec<u8>> = (0..100)
        .map(|i| {
            if i % 2 == 0 {
                // Matching event - use one of the target values
                let val = format!("{:.6}", targets[i % 10]);
                format!(r#"{{"x": {}}}"#, val).into_bytes()
            } else {
                // Non-matching event - use a different random value
                let val = format!("{:.6}", rng.random::<f64>() + 10.0); // +10 ensures no collision
                format!(r#"{{"x": {}}}"#, val).into_bytes()
            }
        })
        .collect();

    c.bench_function("number_matching", |b| {
        let mut i = 0;
        b.iter(|| {
            let event = &events[i % events.len()];
            i += 1;
            q.matches_for_event(black_box(event)).unwrap()
        })
    });
}

/// Regexp with + quantifier on short string
fn bench_regexp_plus_short(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("letters", r#"{"value": [{"regex": "[a-z]+"}]}"#)
        .unwrap();

    let event = r#"{"value": "hello"}"#.as_bytes();

    c.bench_function("regexp_plus_short", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Regexp with + quantifier on long string
fn bench_regexp_plus_long(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("letters", r#"{"value": [{"regex": "[a-z]+"}]}"#)
        .unwrap();

    // 100-character string
    let long_value = "a".repeat(100);
    let event = format!(r#"{{"value": "{}"}}"#, long_value).into_bytes();

    c.bench_function("regexp_plus_long", |b| {
        b.iter(|| q.matches_for_event(black_box(&event)).unwrap())
    });
}

/// Regexp with * quantifier on empty string (should match)
fn bench_regexp_star_empty(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("maybe_letters", r#"{"value": [{"regex": "[a-z]*"}]}"#)
        .unwrap();

    let event = r#"{"value": ""}"#.as_bytes();

    c.bench_function("regexp_star_empty", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Regexp with * quantifier on long string
fn bench_regexp_star_long(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("maybe_letters", r#"{"value": [{"regex": "[a-z]*"}]}"#)
        .unwrap();

    // 100-character string
    let long_value = "a".repeat(100);
    let event = format!(r#"{{"value": "{}"}}"#, long_value).into_bytes();

    c.bench_function("regexp_star_long", |b| {
        b.iter(|| q.matches_for_event(black_box(&event)).unwrap())
    });
}

/// Complex regexp with nested quantifiers (like Go's TestToxicStack)
fn bench_regexp_complex(c: &mut Criterion) {
    let mut q = Quamina::new();
    // Pattern: [a-z]+ followed by optional digits
    q.add_pattern("complex", r#"{"value": [{"regex": "[a-z]+[0-9]?"}]}"#)
        .unwrap();

    let event = r#"{"value": "hello5"}"#.as_bytes();

    c.bench_function("regexp_complex", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Regexp with negated character class [^x]+ - exit byte at end
/// Pattern: prefix[^x]+suffix where memchr can skip to 'x'
fn bench_regexp_negated_short(c: &mut Criterion) {
    let mut q = Quamina::new();
    // Pattern: "a" followed by [^x]+ followed by "x" followed by more text
    q.add_pattern("not_x", r#"{"value": [{"regex": "a[^x]+x"}]}"#)
        .unwrap();

    // String with 'x' at position 10 - memchr should skip to it
    let event = r#"{"value": "aaaaaaaaaxend"}"#.as_bytes();

    c.bench_function("regexp_negated_short", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Regexp with negated character class [^x]+ on long string
/// memchr should skip 100 chars to find 'x'
fn bench_regexp_negated_long(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("not_x", r#"{"value": [{"regex": "a[^x]+x"}]}"#)
        .unwrap();

    // 100 'a's followed by 'x' - memchr skips to position 100
    let long_value = format!("{}x", "a".repeat(100));
    let event = format!(r#"{{"value": "{}"}}"#, long_value).into_bytes();

    c.bench_function("regexp_negated_long", |b| {
        b.iter(|| q.matches_for_event(black_box(&event)).unwrap())
    });
}

/// Regexp with negated character class [^x]+ on very long string (1000 chars)
/// memchr should skip 1000 chars to find 'x'
fn bench_regexp_negated_1k(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("not_x", r#"{"value": [{"regex": "a[^x]+x"}]}"#)
        .unwrap();

    // 1000 'a's followed by 'x' - memchr skips to position 1000
    let long_value = format!("{}x", "a".repeat(1000));
    let event = format!(r#"{{"value": "{}"}}"#, long_value).into_bytes();

    c.bench_function("regexp_negated_1k", |b| {
        b.iter(|| q.matches_for_event(black_box(&event)).unwrap())
    });
}

/// Regexp with dot-star pattern (match anything)
fn bench_regexp_dot_star(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("anything", r#"{"value": [{"regex": ".*"}]}"#)
        .unwrap();

    let event = r#"{"value": "hello world 123"}"#.as_bytes();

    c.bench_function("regexp_dot_star", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

// === Unicode Category Benchmarks ===

/// Unicode category pattern ~p{L} - matches any Unicode letter
fn bench_unicode_category_letter(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("letter", r#"{"value": [{"regex": "~p{L}"}]}"#)
        .unwrap();

    // Test with various inputs
    let events = [
        r#"{"value": "a"}"#.as_bytes().to_vec(),
        r#"{"value": "Z"}"#.as_bytes().to_vec(),
        r#"{"value": "α"}"#.as_bytes().to_vec(),
        r#"{"value": "日"}"#.as_bytes().to_vec(),
        r#"{"value": "5"}"#.as_bytes().to_vec(), // non-match
    ];

    c.bench_function("unicode_category_letter", |b| {
        let mut i = 0;
        b.iter(|| {
            let event = &events[i % events.len()];
            i += 1;
            q.matches_for_event(black_box(event)).unwrap()
        })
    });
}

/// Unicode category compilation time (pattern add, not cached)
fn bench_unicode_category_compile(c: &mut Criterion) {
    c.bench_function("unicode_category_compile", |b| {
        b.iter(|| {
            let mut q = Quamina::new();
            q.add_pattern("letter", r#"{"value": [{"regex": "~p{L}"}]}"#)
                .unwrap();
        })
    });
}

/// Negated character class [^abc] - tests large rune range handling
fn bench_negated_char_class(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("not_abc", r#"{"value": [{"regex": "[^abc]"}]}"#)
        .unwrap();

    let events = [
        r#"{"value": "x"}"#.as_bytes().to_vec(),
        r#"{"value": "日"}"#.as_bytes().to_vec(),
        r#"{"value": "a"}"#.as_bytes().to_vec(), // non-match
    ];

    c.bench_function("negated_char_class", |b| {
        let mut i = 0;
        b.iter(|| {
            let event = &events[i % events.len()];
            i += 1;
            q.matches_for_event(black_box(event)).unwrap()
        })
    });
}

/// Multiple Unicode categories combined - tests merging large FAs
fn bench_unicode_categories_combined(c: &mut Criterion) {
    let mut q = Quamina::new();
    // Match letter followed by digit
    q.add_pattern("letter_digit", r#"{"value": [{"regex": "~p{L}~p{Nd}"}]}"#)
        .unwrap();

    let events = [
        r#"{"value": "a1"}"#.as_bytes().to_vec(),
        r#"{"value": "日5"}"#.as_bytes().to_vec(),
        r#"{"value": "12"}"#.as_bytes().to_vec(), // non-match
    ];

    c.bench_function("unicode_categories_combined", |b| {
        let mut i = 0;
        b.iter(|| {
            let event = &events[i % events.len()];
            i += 1;
            q.matches_for_event(black_box(event)).unwrap()
        })
    });
}

// === Arena NFA benchmarks ===

/// Helper: Build arena-based NFA for [a-z]+ (cyclic, ~4 states)
fn build_arena_nfa_plus() -> (StateArena, StateId, Arc<FieldMatcher>) {
    let mut arena = StateArena::new();
    let field_matcher = Arc::new(FieldMatcher::new());

    // final state with field_transitions
    let final_state = arena.alloc();
    arena[final_state]
        .field_transitions
        .push(field_matcher.clone());

    // exit state: VALUE_TERMINATOR -> final_state
    let exit_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &[ARENA_VALUE_TERMINATOR],
        &[final_state],
    ));

    // loopback state (placeholder)
    let loopback = arena.alloc();

    // start state: 'a'-'z' -> loopback
    let mut bytes = Vec::new();
    let mut targets = Vec::new();
    for b in b'a'..=b'z' {
        bytes.push(b);
        targets.push(loopback);
    }
    let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &bytes,
        &targets,
    ));

    // Set up loopback: epsilon to exit AND back to start (CYCLE!)
    arena[loopback].table.epsilons = smallvec::smallvec![exit_state, start];

    // Precompute epsilon closures for all states
    arena.precompute_epsilon_closures();

    (arena, start, field_matcher)
}

/// Benchmark: Arena-based NFA traversal for [a-z]+ pattern
fn bench_arena_nfa_traversal(c: &mut Criterion) {
    let (arena, start, _field_matcher) = build_arena_nfa_plus();
    let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

    // Test with 100-character string
    let value: Vec<u8> = (0..100).map(|_| b'a').collect();

    c.bench_function("arena_nfa_100chars", |b| {
        b.iter(|| {
            bufs.clear();
            traverse_arena_nfa(&arena, start, black_box(&value), &mut bufs);
            black_box(bufs.transitions.len())
        })
    });
}

/// Benchmark: Arena-based NFA traversal with short string
fn bench_arena_nfa_short(c: &mut Criterion) {
    let (arena, start, _field_matcher) = build_arena_nfa_plus();
    let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

    // Test with 5-character string
    let value: Vec<u8> = (0..5).map(|_| b'a').collect();

    c.bench_function("arena_nfa_5chars", |b| {
        b.iter(|| {
            bufs.clear();
            traverse_arena_nfa(&arena, start, black_box(&value), &mut bufs);
            black_box(bufs.transitions.len())
        })
    });
}

// === Bulk Pattern Add benchmarks (for optimization work) ===

/// Benchmark for bulk pattern adding (100 patterns × 10 values each)
/// This measures the O(n²) problem from repeated merge_fas calls
fn bench_bulk_100x10(c: &mut Criterion) {
    let patterns: Vec<String> = (0..100)
        .map(|i| {
            let values: String = (0..10)
                .map(|j| format!("\"value_{}_{}\"", i, j))
                .collect::<Vec<_>>()
                .join(", ");
            format!(r#"{{"field": [{}]}}"#, values)
        })
        .collect();
    c.bench_function("bulk_100x10", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for (i, pattern) in patterns.iter().enumerate() {
                q.add_pattern(i, pattern).unwrap();
            }
        })
    });
}

/// Larger bulk benchmark (1000 patterns × 10 values)
fn bench_bulk_1000x10(c: &mut Criterion) {
    let patterns: Vec<String> = (0..1000)
        .map(|i| {
            let values: String = (0..10)
                .map(|j| format!("\"value_{}_{}\"", i, j))
                .collect::<Vec<_>>()
                .join(", ");
            format!(r#"{{"field": [{}]}}"#, values)
        })
        .collect();
    c.bench_function("bulk_1000x10", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for (i, pattern) in patterns.iter().enumerate() {
                q.add_pattern(i, pattern).unwrap();
            }
        })
    });
}

/// Smaller bulk benchmark (100 patterns × 100 values) for faster iteration
fn bench_bulk_100x100(c: &mut Criterion) {
    let patterns: Vec<String> = (0..100)
        .map(|i| {
            let values: String = (0..100)
                .map(|j| format!("\"value_{}_{}\"", i, j))
                .collect::<Vec<_>>()
                .join(", ");
            format!(r#"{{"field": [{}]}}"#, values)
        })
        .collect();
    c.bench_function("bulk_100x100", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for (i, pattern) in patterns.iter().enumerate() {
                q.add_pattern(i, pattern).unwrap();
            }
        })
    });
}

/// Bulk benchmark with multiple fields per pattern
fn bench_bulk_100x10_multifield(c: &mut Criterion) {
    let patterns: Vec<String> = (0..100)
        .map(|i| {
            let values: String = (0..10)
                .map(|j| format!("\"value_{}_{}\"", i, j))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                r#"{{"field1": [{}], "field2": [{}], "field3": [{}]}}"#,
                values, values, values
            )
        })
        .collect();
    c.bench_function("bulk_100x10_multifield", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for (i, pattern) in patterns.iter().enumerate() {
                q.add_pattern(i, pattern).unwrap();
            }
        })
    });
}

// === 10K+ Pattern Stress Benchmarks ===
// Note: bench_bulk_10000x1 is in benches/bulk_10k.rs (too slow for CI).

/// Benchmark for matching against 10,000 patterns on same field
/// Tests automaton traversal at scale
fn bench_10k_patterns_match(c: &mut Criterion) {
    let mut q = Quamina::<usize>::new();
    for i in 0..10_000 {
        q.add_pattern(i, &format!(r#"{{"status": ["status_{}"]}}"#, i))
            .unwrap();
    }

    // Event that matches pattern 5000 (middle of the set)
    let event_match = r#"{"status": "status_5000"}"#.as_bytes();
    // Event that doesn't match any pattern
    let event_no_match = r#"{"status": "no_match"}"#.as_bytes();

    // Verify
    let matches = q.matches_for_event(event_match).unwrap();
    assert_eq!(matches.len(), 1);
    assert_eq!(matches[0], 5000);

    c.bench_function("10k_patterns_1_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event_match)).unwrap())
    });

    c.bench_function("10k_patterns_no_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event_no_match)).unwrap())
    });
}

/// Benchmark for matching against 10,000 diverse patterns (different fields)
/// Tests field indexing at scale - should be much faster than same-field
fn bench_10k_diverse_patterns(c: &mut Criterion) {
    let mut q = Quamina::<usize>::new();
    for i in 0..10_000 {
        q.add_pattern(i, &format!(r#"{{"field_{}": ["value_{}"]}}"#, i, i))
            .unwrap();
    }

    // Event with only field_5000, so only 1 of 10k patterns could match
    let event = r#"{"field_5000": "value_5000", "other": "data"}"#.as_bytes();

    // Verify
    let matches = q.matches_for_event(event).unwrap();
    assert_eq!(matches.len(), 1);

    c.bench_function("10k_diverse_patterns_1_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

/// Benchmark for 10,000 patterns with mixed types
/// Realistic scenario with exact, prefix, and numeric patterns
fn bench_10k_mixed_patterns(c: &mut Criterion) {
    let mut q = Quamina::<usize>::new();

    // Add mix of pattern types
    for i in 0..3_334 {
        // Exact match patterns
        q.add_pattern(i, &format!(r#"{{"type": ["exact_{}"]}}"#, i))
            .unwrap();
    }
    for i in 3_334..6_667 {
        // Prefix patterns
        q.add_pattern(i, &format!(r#"{{"path": [{{"prefix": "/api/v{}"}}]}}"#, i))
            .unwrap();
    }
    for i in 6_667..10_000 {
        // Numeric patterns
        q.add_pattern(i, &format!(r#"{{"score": [{{"numeric": ["=", {}]}}]}}"#, i))
            .unwrap();
    }

    // Events for each type
    let event_exact = r#"{"type": "exact_1000"}"#.as_bytes();
    let event_prefix = r#"{"path": "/api/v5000/users"}"#.as_bytes();
    let event_numeric = r#"{"score": 8000}"#.as_bytes();

    c.bench_function("10k_mixed_exact_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event_exact)).unwrap())
    });

    c.bench_function("10k_mixed_prefix_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event_prefix)).unwrap())
    });

    c.bench_function("10k_mixed_numeric_match", |b| {
        b.iter(|| q.matches_for_event(black_box(event_numeric)).unwrap())
    });
}

// === CityLots benchmarks (comparable to Go's citylots_bench_test.go) ===

fn load_citylots_lines() -> Vec<Vec<u8>> {
    let file = std::fs::File::open("testdata/citylots.jlines.gz")
        .expect("Failed to open testdata/citylots.jlines.gz");
    let decoder = GzDecoder::new(file);
    let reader = BufReader::new(decoder);

    reader
        .lines()
        .map(|line| line.expect("Failed to read line").into_bytes())
        .collect()
}

/// CityLots benchmark - matches Go's BenchmarkCityLots
/// Tests 4 patterns against 206k GeoJSON features from San Francisco parcel data
fn bench_citylots(c: &mut Criterion) {
    // Same patterns as Go benchmark
    let patterns = [
        r#"{ "properties": { "STREET": [ "CRANLEIGH" ] } }"#,
        r#"{ "properties": { "STREET": [ "17TH" ], "ODD_EVEN": [ "E"] } }"#,
        r#"{ "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
        r#"{ "properties": { "MAPBLKLOT": ["0011008"], "BLKLOT": ["0011008"]},  "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
    ];
    let names = ["CRANLEIGH", "17TH Even", "Geometry", "0011008"];

    let mut q = Quamina::new();
    for (name, pattern) in names.iter().zip(patterns.iter()) {
        q.add_pattern(*name, pattern).unwrap();
    }

    let lines = load_citylots_lines();
    let num_lines = lines.len();

    c.bench_function("citylots", |b| {
        let mut i = 0;
        b.iter(|| {
            let line_index = i % num_lines;
            i += 1;
            q.matches_for_event(black_box(&lines[line_index])).unwrap()
        })
    });
}

/// CityLots core benchmark - matches Go's BenchmarkCityLotsCore
/// Uses ThreadSafeCoreMatcher directly (no Quamina wrapper overhead)
fn bench_citylots_core(c: &mut Criterion) {
    // Same patterns as Go benchmark, but we need to parse them into Matcher format
    #[allow(clippy::type_complexity)]
    let pattern_fields: Vec<(&str, Vec<(String, Vec<Matcher>)>)> = vec![
        (
            "CRANLEIGH",
            vec![(
                "properties.STREET".to_string(),
                vec![Matcher::Exact("CRANLEIGH".to_string())],
            )],
        ),
        (
            "17TH Even",
            vec![
                (
                    "properties.STREET".to_string(),
                    vec![Matcher::Exact("17TH".to_string())],
                ),
                (
                    "properties.ODD_EVEN".to_string(),
                    vec![Matcher::Exact("E".to_string())],
                ),
            ],
        ),
        (
            "Geometry",
            vec![(
                "geometry.coordinates".to_string(),
                vec![Matcher::Exact("37.807807921694092".to_string())],
            )],
        ),
        (
            "0011008",
            vec![
                (
                    "properties.MAPBLKLOT".to_string(),
                    vec![Matcher::Exact("0011008".to_string())],
                ),
                (
                    "properties.BLKLOT".to_string(),
                    vec![Matcher::Exact("0011008".to_string())],
                ),
                (
                    "geometry.coordinates".to_string(),
                    vec![Matcher::Exact("37.807807921694092".to_string())],
                ),
            ],
        ),
    ];

    // Build the core matcher directly
    let matcher: ThreadSafeCoreMatcher<&str> = ThreadSafeCoreMatcher::new();
    let mut segments_tree = SegmentsTree::new();

    for (name, fields) in &pattern_fields {
        for (path, _) in fields {
            segments_tree.add(&path.replace('.', "\n"));
        }
        matcher.add_pattern(*name, fields);
    }

    // Pre-flatten all lines to avoid flattening overhead in the benchmark
    let lines = load_citylots_lines();
    let mut flattener = FlattenJsonState::new();
    let pre_flattened: Vec<Vec<EventField>> = lines
        .iter()
        .map(|line| {
            flattener
                .flatten(line, &segments_tree)
                .unwrap()
                .iter()
                .map(|f| EventField {
                    path: f.path_str().to_string(),
                    value: String::from_utf8_lossy(f.value_bytes()).to_string(),
                    array_trail: f
                        .array_trail
                        .iter()
                        .map(|a| quamina::json::ArrayPos {
                            array: a.array,
                            pos: a.pos,
                        })
                        .collect(),
                    is_number: f.is_number,
                })
                .collect()
        })
        .collect();
    let num_lines = pre_flattened.len();

    c.bench_function("citylots_core", |b| {
        let mut i = 0;
        b.iter(|| {
            let line_index = i % num_lines;
            i += 1;
            matcher.matches_for_fields(black_box(&pre_flattened[line_index]))
        })
    });
}

/// Array-heavy JSON benchmark to measure array_trail cloning overhead
/// This specifically exercises the code path where array_trail is cloned per element
fn bench_array_heavy(c: &mut Criterion) {
    let mut q = Quamina::new();
    // Pattern matching a value in an array
    q.add_pattern("tags", r#"{"tags": ["important"]}"#).unwrap();

    // Event with many array elements to stress array_trail cloning
    let many_tags = (0..100)
        .map(|i| {
            if i == 50 {
                "important".to_string()
            } else {
                format!("tag{}", i)
            }
        })
        .map(|t| format!(r#""{}""#, t))
        .collect::<Vec<_>>()
        .join(", ");
    let event_many_tags = format!(r#"{{"tags": [{}]}}"#, many_tags);

    // Verify match
    let matches = q.matches_for_event(event_many_tags.as_bytes()).unwrap();
    assert!(!matches.is_empty());

    c.bench_function("array_heavy_100_elements", |b| {
        b.iter(|| {
            q.matches_for_event(black_box(event_many_tags.as_bytes()))
                .unwrap()
        })
    });
}

/// Deeply nested object benchmark with arrays at each level
fn bench_deep_nesting_with_arrays(c: &mut Criterion) {
    let mut q = Quamina::new();
    // Pattern matching a value in a deeply nested structure with arrays
    q.add_pattern(
        "deep",
        r#"{"level1": {"level2": {"level3": {"values": ["target"]}}}}"#,
    )
    .unwrap();

    // Build an event with arrays at different nesting levels
    // Each array has multiple elements to exercise array_trail
    let event = r#"{"level1": {"level2": {"level3": {"values": ["a", "b", "target", "c"]}}}}"#;

    // Verify match
    let matches = q.matches_for_event(event.as_bytes()).unwrap();
    assert_eq!(matches.len(), 1);

    c.bench_function("deep_nesting_with_arrays", |b| {
        b.iter(|| q.matches_for_event(black_box(event.as_bytes())).unwrap())
    });
}

/// State acceleration benchmark with wildcard patterns on long strings.
/// Tests the memchr optimization for suffix patterns like `*X` where
/// X appears late in a long string.
fn bench_state_acceleration(c: &mut Criterion) {
    let mut q = Quamina::new();

    // Pattern `*X` - matches any string ending with X
    q.add_pattern("suffix_X", r#"{"value": [{"wildcard": "*X"}]}"#)
        .unwrap();

    // Long string with X near the end (acceleration should skip most bytes)
    let long_value = "A".repeat(10000) + "X";
    let event_long = format!(r#"{{"value": "{}"}}"#, long_value);

    // Medium string with X near the end
    let medium_value = "A".repeat(1000) + "X";
    let event_medium = format!(r#"{{"value": "{}"}}"#, medium_value);

    // Short string
    let event_short = r#"{"value": "AAAAAX"}"#;

    // Verify matches
    assert!(!q
        .matches_for_event(event_long.as_bytes())
        .unwrap()
        .is_empty());
    assert!(!q
        .matches_for_event(event_medium.as_bytes())
        .unwrap()
        .is_empty());
    assert!(!q
        .matches_for_event(event_short.as_bytes())
        .unwrap()
        .is_empty());

    c.bench_function("accel_suffix_10k_chars", |b| {
        b.iter(|| {
            q.matches_for_event(black_box(event_long.as_bytes()))
                .unwrap()
        })
    });

    c.bench_function("accel_suffix_1k_chars", |b| {
        b.iter(|| {
            q.matches_for_event(black_box(event_medium.as_bytes()))
                .unwrap()
        })
    });

    c.bench_function("accel_suffix_short", |b| {
        b.iter(|| {
            q.matches_for_event(black_box(event_short.as_bytes()))
                .unwrap()
        })
    });
}

/// Benchmark: 10,000 number matching events to test linear scaling
fn bench_number_matching_10k(c: &mut Criterion) {
    use rand::prelude::*;

    let mut rng = rand::rngs::StdRng::seed_from_u64(2325);
    let targets: Vec<f64> = (0..10).map(|_| rng.random::<f64>()).collect();

    let values: String = targets
        .iter()
        .map(|f| format!("{:.6}", f))
        .collect::<Vec<_>>()
        .join(", ");
    let pattern = format!(r#"{{"x": [{}]}}"#, values);

    let mut q = Quamina::new();
    q.add_pattern("P", &pattern).unwrap();

    // 10,000 events
    let events: Vec<Vec<u8>> = (0..10_000)
        .map(|i| {
            if i % 2 == 0 {
                let val = format!("{:.6}", targets[i % 10]);
                format!(r#"{{"x": {}}}"#, val).into_bytes()
            } else {
                let val = format!("{:.6}", rng.random::<f64>() + 10.0);
                format!(r#"{{"x": {}}}"#, val).into_bytes()
            }
        })
        .collect();

    c.bench_function("number_matching_10k", |b| {
        b.iter(|| {
            for event in &events {
                let _ = q.matches_for_event(black_box(event)).unwrap();
            }
        })
    });
}

// Configure longer benchmarks with minimum sample count and short warm-up.
// bulk_10000x1 takes ~28s per iteration, so 10 samples ≈ 280s total.
fn configure_bulk_benchmarks() -> Criterion {
    Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(10))
        .warm_up_time(std::time::Duration::from_secs(3))
}

criterion_group! {
    name = bulk_benches;
    config = configure_bulk_benchmarks();
    targets = bench_bulk_100x10, bench_bulk_1000x10, bench_bulk_100x100, bench_bulk_100x10_multifield
}

// Configure 10k pattern benchmarks with minimum sample count.
fn configure_10k_benchmarks() -> Criterion {
    Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(10))
        .warm_up_time(std::time::Duration::from_secs(3))
}

criterion_group! {
    name = stress_benches;
    config = configure_10k_benchmarks();
    targets = bench_10k_patterns_match, bench_10k_diverse_patterns, bench_10k_mixed_patterns
}

criterion_group!(
    benches,
    bench_exact_match,
    bench_multiple_patterns,
    bench_complex_event,
    bench_regex_match,
    bench_no_match,
    bench_has_matches,
    bench_diverse_patterns,
    bench_diverse_no_match,
    // Flatten-only benchmarks (comparable to Go's Benchmark_JsonFlattener_*)
    bench_flatten_context_fields,
    bench_flatten_direct_context_fields,
    bench_flatten_middle_nested,
    bench_flatten_last_field,
    // Status.json full matching benchmarks (comparable to Go's Benchmark_JsonFlattner_Evaluate_*)
    bench_status_context_fields,
    bench_status_middle_nested,
    bench_status_last_field,
    bench_status_all_patterns,
    // Pattern type benchmarks
    bench_shellstyle_alphabet,
    bench_shellstyle_multi_match,
    bench_prefix_patterns,
    bench_anything_but,
    bench_multi_field_and,
    // Numeric range benchmarks
    bench_numeric_range_single,
    bench_numeric_range_two_sided,
    bench_numeric_range_multiple,
    // Exact float matching (comparable to Go's BenchmarkNumberMatching)
    bench_number_matching,
    // Regexp benchmarks (quantifier performance)
    bench_regexp_plus_short,
    bench_regexp_plus_long,
    bench_regexp_star_empty,
    bench_regexp_star_long,
    bench_regexp_complex,
    bench_regexp_dot_star,
    // Regexp negated class benchmarks (ASCII fast path - Phase 1)
    bench_regexp_negated_short,
    bench_regexp_negated_long,
    bench_regexp_negated_1k,
    // Unicode category benchmarks
    bench_unicode_category_letter,
    bench_unicode_category_compile,
    bench_negated_char_class,
    bench_unicode_categories_combined,
    // Arena NFA benchmarks
    bench_arena_nfa_traversal,
    bench_arena_nfa_short,
    // CityLots benchmarks (comparable to Go)
    bench_citylots,
    bench_citylots_core,
    // Array-heavy benchmarks (for Phase 5 evaluation)
    bench_array_heavy,
    bench_deep_nesting_with_arrays,
    // State acceleration benchmark (Phase 3)
    bench_state_acceleration,
    // Number matching at scale
    bench_number_matching_10k,
);
criterion_main!(benches, bulk_benches, stress_benches);
