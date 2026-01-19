//! Benchmarks for quamina-rs pattern matching
//!
//! Comparable benchmarks to Go's flatten_json_bench_test.go and citylots_bench_test.go

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use flate2::read::GzDecoder;
use quamina::automaton::arena::{
    traverse_arena_nfa, ArenaNfaBuffers, ArenaSmallTable, StateArena, StateId,
    ARENA_VALUE_TERMINATOR,
};
use quamina::automaton::FieldMatcher;
use quamina::flatten_json::FlattenJsonState;
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

// Configure longer benchmarks with reduced sample count
fn configure_bulk_benchmarks() -> Criterion {
    Criterion::default()
        .sample_size(20)
        .measurement_time(std::time::Duration::from_secs(10))
}

criterion_group! {
    name = bulk_benches;
    config = configure_bulk_benchmarks();
    targets = bench_bulk_100x10, bench_bulk_1000x10, bench_bulk_100x100, bench_bulk_100x10_multifield
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
    bench_prefix_patterns,
    bench_anything_but,
    bench_multi_field_and,
    // Numeric range benchmarks
    bench_numeric_range_single,
    bench_numeric_range_two_sided,
    bench_numeric_range_multiple,
    // Regexp benchmarks (quantifier performance)
    bench_regexp_plus_short,
    bench_regexp_plus_long,
    bench_regexp_star_empty,
    bench_regexp_star_long,
    bench_regexp_complex,
    bench_regexp_dot_star,
    // Arena NFA benchmarks
    bench_arena_nfa_traversal,
    bench_arena_nfa_short,
    // CityLots benchmark (comparable to Go)
    bench_citylots,
);
criterion_main!(benches, bulk_benches);
