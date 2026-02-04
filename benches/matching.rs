//! Benchmarks for quamina-rs pattern matching
//!
//! Comparable benchmarks to Go's flatten_json_bench_test.go and citylots_bench_test.go

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use flate2::read::GzDecoder;
use quamina::automaton::arena::{
    traverse_arena_nfa, ArenaNfaBuffers, ArenaSmallTable, StateArena, StateId,
    ARENA_VALUE_TERMINATOR,
};
use quamina::automaton::{EventField, FieldMatcher, ThreadSafeCoreMatcher};
use quamina::flatten_json::FlattenJsonState;
use quamina::json::Matcher;
use quamina::numbits::{q_num_from_f64, q_num_stack};
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
    let targets: Vec<f64> = (0..10).map(|_| rng.gen::<f64>()).collect();

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
                let val = format!("{:.6}", rng.gen::<f64>() + 10.0); // +10 ensures no collision
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
    let events = vec![
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

    let events = vec![
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

    let events = vec![
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
    arena[loopback].table.epsilons = vec![exit_state, start];

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
    c.bench_function("bulk_100x10", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for i in 0..100 {
                let values: String = (0..10)
                    .map(|j| format!("\"value_{}_{}\"", i, j))
                    .collect::<Vec<_>>()
                    .join(", ");
                let pattern = format!(r#"{{"field": [{}]}}"#, values);
                q.add_pattern(i, &pattern).unwrap();
            }
        })
    });
}

/// Larger bulk benchmark (1000 patterns × 10 values)
fn bench_bulk_1000x10(c: &mut Criterion) {
    c.bench_function("bulk_1000x10", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for i in 0..1000 {
                let values: String = (0..10)
                    .map(|j| format!("\"value_{}_{}\"", i, j))
                    .collect::<Vec<_>>()
                    .join(", ");
                let pattern = format!(r#"{{"field": [{}]}}"#, values);
                q.add_pattern(i, &pattern).unwrap();
            }
        })
    });
}

/// Smaller bulk benchmark (100 patterns × 100 values) for faster iteration
fn bench_bulk_100x100(c: &mut Criterion) {
    c.bench_function("bulk_100x100", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for i in 0..100 {
                let values: String = (0..100)
                    .map(|j| format!("\"value_{}_{}\"", i, j))
                    .collect::<Vec<_>>()
                    .join(", ");
                let pattern = format!(r#"{{"field": [{}]}}"#, values);
                q.add_pattern(i, &pattern).unwrap();
            }
        })
    });
}

/// Bulk benchmark with multiple fields per pattern
fn bench_bulk_100x10_multifield(c: &mut Criterion) {
    c.bench_function("bulk_100x10_multifield", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for i in 0..100 {
                let values: String = (0..10)
                    .map(|j| format!("\"value_{}_{}\"", i, j))
                    .collect::<Vec<_>>()
                    .join(", ");
                let pattern = format!(
                    r#"{{"field1": [{}], "field2": [{}], "field3": [{}]}}"#,
                    values, values, values
                );
                q.add_pattern(i, &pattern).unwrap();
            }
        })
    });
}

// === 10K+ Pattern Stress Benchmarks ===

/// Benchmark for adding 10,000 patterns (single value each)
/// Tests scaling behavior beyond typical workloads
fn bench_bulk_10000x1(c: &mut Criterion) {
    c.bench_function("bulk_10000x1", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for i in 0..10_000 {
                let pattern = format!(r#"{{"field": ["value_{}"]}}"#, i);
                q.add_pattern(i, &pattern).unwrap();
            }
        })
    });
}

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

// === Q-Number Conversion Micro-benchmarks ===
// Phase 0 of Hybrid Q-Number investigation

/// Generate test values with citylots-representative distribution:
/// - 23% zero (0.0)
/// - 17% small int (1-999)
/// - 13% large int (1000+)
/// - 47% high-precision float
fn citylots_representative_values() -> Vec<f64> {
    let mut values = Vec::with_capacity(100);

    // 23% zero
    for _ in 0..23 {
        values.push(0.0);
    }

    // 17% small int
    let mut rng = 12345u64;
    for _ in 0..17 {
        rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
        values.push((rng % 999 + 1) as f64);
    }

    // 13% large int
    for _ in 0..13 {
        rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
        values.push((rng % 1_000_000 + 1000) as f64);
    }

    // 47% high-precision float
    for _ in 0..47 {
        rng = rng.wrapping_mul(6364136223846793005).wrapping_add(1);
        let f = (rng as f64) / (u64::MAX as f64) * 1000.0;
        values.push(f);
    }

    values
}

/// Micro-benchmark: Q-number conversion using Vec (baseline)
fn bench_q_num_conversion_vec(c: &mut Criterion) {
    let values = citylots_representative_values();

    c.bench_function("q_num_conversion_vec", |b| {
        let mut i = 0;
        b.iter(|| {
            let val = values[i % values.len()];
            i += 1;
            black_box(q_num_from_f64(black_box(val)))
        })
    });
}

/// Micro-benchmark: Q-number conversion using stack buffer
fn bench_q_num_conversion_stack(c: &mut Criterion) {
    let values = citylots_representative_values();

    c.bench_function("q_num_conversion_stack", |b| {
        let mut i = 0;
        b.iter(|| {
            let val = values[i % values.len()];
            i += 1;
            black_box(q_num_stack(black_box(val)))
        })
    });
}

/// Micro-benchmark: Q-number conversion + slice access (simulates real usage)
fn bench_q_num_vec_with_slice(c: &mut Criterion) {
    let values = citylots_representative_values();

    c.bench_function("q_num_vec_with_slice", |b| {
        let mut i = 0;
        b.iter(|| {
            let val = values[i % values.len()];
            i += 1;
            let q = q_num_from_f64(black_box(val));
            // Access length to simulate slice usage without borrow issues
            black_box(q.len())
        })
    });
}

/// Micro-benchmark: Q-number conversion + slice access (stack version)
fn bench_q_num_stack_with_slice(c: &mut Criterion) {
    let values = citylots_representative_values();

    c.bench_function("q_num_stack_with_slice", |b| {
        let mut i = 0;
        b.iter(|| {
            let val = values[i % values.len()];
            i += 1;
            let q = q_num_stack(black_box(val));
            black_box(q.len())
        })
    });
}

/// Benchmark comparing all two approaches on zeros only (23% of citylots)
fn bench_q_num_zeros(c: &mut Criterion) {
    let mut group = c.benchmark_group("q_num_zeros");

    group.bench_function("vec", |b| {
        b.iter(|| black_box(q_num_from_f64(black_box(0.0))))
    });

    group.bench_function("stack", |b| {
        b.iter(|| black_box(q_num_stack(black_box(0.0))))
    });

    group.finish();
}

/// Benchmark: Compare Vec vs Stack Q-number approaches at 100k scale
/// This directly tests conversion + slice access to isolate Q-number performance
fn bench_q_num_100k_comparison(c: &mut Criterion) {
    use rand::prelude::*;

    let mut rng = rand::rngs::StdRng::seed_from_u64(99999);

    // Generate 100k float values with citylots-representative distribution
    let values: Vec<f64> = (0..100_000)
        .map(|i| {
            match i % 100 {
                0..=22 => 0.0,                                           // 23% zero
                23..=39 => (rng.gen::<u64>() % 999 + 1) as f64,          // 17% small int
                40..=52 => (rng.gen::<u64>() % 1_000_000 + 1000) as f64, // 13% large int
                _ => rng.gen::<f64>() * 1000.0,                          // 47% high-precision
            }
        })
        .collect();

    let mut group = c.benchmark_group("q_num_100k");

    group.bench_function("vec", |b| {
        b.iter(|| {
            let mut total_len = 0usize;
            for &val in &values {
                let q = q_num_from_f64(val);
                total_len += q.len();
            }
            black_box(total_len)
        })
    });

    group.bench_function("stack", |b| {
        b.iter(|| {
            let mut total_len = 0usize;
            for &val in &values {
                let q = q_num_stack(val);
                total_len += q.len();
            }
            black_box(total_len)
        })
    });

    group.finish();
}

/// Benchmark: 10,000 number matching events to test linear scaling
fn bench_number_matching_10k(c: &mut Criterion) {
    use rand::prelude::*;

    let mut rng = rand::rngs::StdRng::seed_from_u64(2325);
    let targets: Vec<f64> = (0..10).map(|_| rng.gen::<f64>()).collect();

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
                let val = format!("{:.6}", rng.gen::<f64>() + 10.0);
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

/// Benchmark comparing Vec vs Stack on high-precision floats (47% of citylots)
fn bench_q_num_high_precision(c: &mut Criterion) {
    let mut group = c.benchmark_group("q_num_high_precision");

    // High-precision value that uses all 10 bytes
    let val = 3.141592653589793;

    group.bench_function("vec", |b| {
        b.iter(|| black_box(q_num_from_f64(black_box(val))))
    });

    group.bench_function("stack", |b| {
        b.iter(|| black_box(q_num_stack(black_box(val))))
    });

    group.finish();
}

// === DFA Traversal Micro-benchmarks ===
// Phase 0 of FA Traversal optimization investigation

use quamina::automaton::{traverse_dfa, FaState, SmallTable, VALUE_TERMINATOR};
use std::cmp::Ordering;

/// Build a simple string-matching FA for testing dstep performance
fn build_test_fa(value: &[u8]) -> SmallTable {
    fn make_step(val: &[u8], index: usize, final_fm: Arc<FieldMatcher>) -> SmallTable {
        if index >= val.len() {
            let final_state = Arc::new(FaState {
                table: SmallTable::new(),
                field_transitions: vec![final_fm],
            });
            return SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[final_state]);
        }
        let next_table = make_step(val, index + 1, final_fm);
        let next_state = Arc::new(FaState::with_table(next_table));
        SmallTable::with_mappings(None, &[val[index]], &[next_state])
    }
    make_step(value, 0, Arc::new(FieldMatcher::new()))
}

/// Micro-benchmark: DFA traversal on Q-number bytes
/// Tests the core FA traversal hot path for numeric matching
fn bench_dfa_traversal_qnum(c: &mut Criterion) {
    // Build FA for a specific Q-number
    let q = q_num_stack(50.0);
    let q_bytes = q.as_slice();
    let fa = build_test_fa(q_bytes);

    let mut transitions = Vec::with_capacity(4);

    c.bench_function("dfa_traversal_qnum", |b| {
        b.iter(|| {
            transitions.clear();
            traverse_dfa(black_box(&fa), black_box(q_bytes), &mut transitions);
            black_box(transitions.len())
        })
    });
}

/// Micro-benchmark: DFA traversal on various Q-number lengths
fn bench_dfa_traversal_qnum_lengths(c: &mut Criterion) {
    let mut group = c.benchmark_group("dfa_traversal_lengths");

    // Test different Q-number lengths (1, 3, 5, 10 bytes)
    let test_values = [
        (0.0, "1_byte"),                 // Q-number for 0.0 is 1 byte
        (50.0, "3_bytes"),               // Small int
        (12345.0, "5_bytes"),            // Medium int
        (3.141592653589793, "10_bytes"), // High precision float
    ];

    for (val, name) in test_values {
        let q = q_num_stack(val);
        let q_bytes = q.as_slice();
        let fa = build_test_fa(q_bytes);
        let mut transitions = Vec::with_capacity(4);

        group.bench_function(name, |b| {
            b.iter(|| {
                transitions.clear();
                traverse_dfa(black_box(&fa), black_box(q_bytes), &mut transitions);
                black_box(transitions.len())
            })
        });
    }

    group.finish();
}

/// Micro-benchmark: dstep with typical ceiling sizes
fn bench_dstep_ceiling_sizes(c: &mut Criterion) {
    let mut group = c.benchmark_group("dstep_ceilings");

    // Create tables with different ceiling counts
    // Typical for single-byte mapping: 3 ceilings [byte, byte+1, BYTE_CEILING]
    let table_3 = SmallTable::with_mappings(
        None,
        &[0x50], // Single byte mapping
        &[Arc::new(FaState::new())],
    );

    // Multiple byte mappings: more ceilings
    let table_5 = SmallTable::with_mappings(
        None,
        &[0x30, 0x50], // Two byte mappings
        &[Arc::new(FaState::new()), Arc::new(FaState::new())],
    );

    // Many mappings (like after merging patterns)
    let table_9 = SmallTable::with_mappings(
        None,
        &[0x20, 0x30, 0x40, 0x50], // Four byte mappings
        &[
            Arc::new(FaState::new()),
            Arc::new(FaState::new()),
            Arc::new(FaState::new()),
            Arc::new(FaState::new()),
        ],
    );

    group.bench_function("3_ceilings", |b| {
        b.iter(|| black_box(table_3.dstep(black_box(0x50))))
    });

    group.bench_function("5_ceilings", |b| {
        b.iter(|| black_box(table_5.dstep(black_box(0x50))))
    });

    group.bench_function("9_ceilings", |b| {
        b.iter(|| black_box(table_9.dstep(black_box(0x50))))
    });

    group.finish();
}

// === Direct Byte Comparison vs FA Benchmarks ===

/// Direct byte comparison for numeric less-than
#[inline]
fn direct_less_than(value_q: &[u8], bound_q: &[u8], inclusive: bool) -> bool {
    match value_q.cmp(bound_q) {
        Ordering::Less => true,
        Ordering::Equal => inclusive,
        Ordering::Greater => false,
    }
}

/// Direct byte comparison for numeric range
#[inline]
fn direct_in_range(
    value_q: &[u8],
    lower_q: &[u8],
    upper_q: &[u8],
    lower_incl: bool,
    upper_incl: bool,
) -> bool {
    let above_lower = match value_q.cmp(lower_q) {
        Ordering::Greater => true,
        Ordering::Equal => lower_incl,
        Ordering::Less => false,
    };
    let below_upper = match value_q.cmp(upper_q) {
        Ordering::Less => true,
        Ordering::Equal => upper_incl,
        Ordering::Greater => false,
    };
    above_lower && below_upper
}

/// Benchmark: Direct byte comparison vs FA for less-than
fn bench_numeric_comparison_methods(c: &mut Criterion) {
    let mut group = c.benchmark_group("numeric_cmp_method");

    // Test values
    let bound = 100.0;
    let bound_q = q_num_stack(bound);
    let bound_bytes = bound_q.as_slice();

    // Build FA for comparison
    let fa = build_test_fa(bound_bytes);

    // Test values: some below, some above
    let test_values: Vec<f64> = vec![0.0, 50.0, 99.0, 100.0, 101.0, 500.0, 1000.0];
    let test_qs: Vec<_> = test_values.iter().map(|&v| q_num_stack(v)).collect();

    // Benchmark FA traversal approach
    group.bench_function("fa_traversal", |b| {
        let mut transitions = Vec::with_capacity(4);
        let mut i = 0;
        b.iter(|| {
            let q = &test_qs[i % test_qs.len()];
            i += 1;
            transitions.clear();
            traverse_dfa(black_box(&fa), black_box(q.as_slice()), &mut transitions);
            black_box(!transitions.is_empty())
        })
    });

    // Benchmark direct comparison approach
    group.bench_function("direct_cmp", |b| {
        let mut i = 0;
        b.iter(|| {
            let q = &test_qs[i % test_qs.len()];
            i += 1;
            black_box(direct_less_than(
                black_box(q.as_slice()),
                black_box(bound_bytes),
                true,
            ))
        })
    });

    group.finish();
}

/// Benchmark: Direct range comparison vs FA
fn bench_numeric_range_methods(c: &mut Criterion) {
    let mut group = c.benchmark_group("numeric_range_method");

    // Range: 0 <= x <= 100
    let lower = 0.0;
    let upper = 100.0;
    let lower_q = q_num_stack(lower);
    let upper_q = q_num_stack(upper);

    // Test values
    let test_values: Vec<f64> = vec![-10.0, 0.0, 50.0, 100.0, 150.0];
    let test_qs: Vec<_> = test_values.iter().map(|&v| q_num_stack(v)).collect();

    // Benchmark direct comparison
    group.bench_function("direct_range", |b| {
        let mut i = 0;
        b.iter(|| {
            let q = &test_qs[i % test_qs.len()];
            i += 1;
            black_box(direct_in_range(
                black_box(q.as_slice()),
                black_box(lower_q.as_slice()),
                black_box(upper_q.as_slice()),
                true,
                true,
            ))
        })
    });

    group.finish();
}

// Configure longer benchmarks with reduced sample count
fn configure_bulk_benchmarks() -> Criterion {
    Criterion::default()
        .sample_size(20)
        .measurement_time(std::time::Duration::from_secs(10))
}

criterion_group! {
    name = bulk_benches;
    config = configure_bulk_benchmarks();
    targets = bench_bulk_100x10, bench_bulk_1000x10, bench_bulk_100x100, bench_bulk_100x10_multifield, bench_bulk_10000x1
}

// Configure 10k pattern benchmarks with even longer measurement
fn configure_10k_benchmarks() -> Criterion {
    Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(15))
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
    // Q-number conversion micro-benchmarks
    bench_q_num_conversion_vec,
    bench_q_num_conversion_stack,
    bench_q_num_vec_with_slice,
    bench_q_num_stack_with_slice,
    bench_q_num_zeros,
    bench_q_num_high_precision,
    bench_q_num_100k_comparison,
    bench_number_matching_10k,
    // DFA traversal micro-benchmarks (FA Traversal optimization Phase 0)
    bench_dfa_traversal_qnum,
    bench_dfa_traversal_qnum_lengths,
    bench_dstep_ceiling_sizes,
    // Direct byte comparison vs FA (Phase 1 investigation)
    bench_numeric_comparison_methods,
    bench_numeric_range_methods,
);
criterion_main!(benches, bulk_benches, stress_benches);
