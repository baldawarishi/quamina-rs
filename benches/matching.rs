//! Benchmarks for quamina-rs pattern matching
//!
//! Comparable benchmarks to Go's flatten_json_bench_test.go

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use quamina::Quamina;

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

    let event = r#"{"region": "us-east-1", "service": "lambda", "level": "error", "message": "timeout"}"#.as_bytes();

    c.bench_function("multi_field_and_3_fields", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
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
    // Status.json benchmarks (comparable to Go)
    bench_status_context_fields,
    bench_status_middle_nested,
    bench_status_last_field,
    bench_status_all_patterns,
    // Pattern type benchmarks
    bench_shellstyle_alphabet,
    bench_prefix_patterns,
    bench_anything_but,
    bench_multi_field_and,
);
criterion_main!(benches);
