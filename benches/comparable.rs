//! Cross-language comparable benchmarks
//!
//! These benchmarks use identical patterns and events as the Go (quamina)
//! and Java (event-ruler) benchmark suites, enabling fair cross-language
//! performance comparison.
//!
//! Run:
//!   cargo bench --bench comparable
//!
//! Run a specific benchmark:
//!   cargo bench --bench comparable -- exact_single
//!
//! For the full comparison suite, see benchmark-comparison/README.md.

use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion};
use flate2::read::GzDecoder;
use quamina::Quamina;
use std::io::{BufRead, BufReader};

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

// =========================================================================
// All 3 libraries: quamina-rs, quamina (Go), event-ruler (Java)
// =========================================================================

fn bench_exact_single(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
    let event = r#"{"status": "active", "id": 123}"#.as_bytes();

    c.bench_function("exact_single", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_exact_100(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..100 {
        q.add_pattern(
            format!("p{}", i),
            &format!(r#"{{"status": ["status_{}"]}}"#, i),
        )
        .unwrap();
    }
    let event = r#"{"status": "status_50"}"#.as_bytes();

    c.bench_function("exact_100", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_prefix_100(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..100 {
        q.add_pattern(
            format!("p{}", i),
            &format!(r#"{{"path": [{{"prefix": "/api/v{}/users"}}]}}"#, i),
        )
        .unwrap();
    }
    let event = r#"{"path": "/api/v50/users/123"}"#.as_bytes();

    c.bench_function("prefix_100", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_wildcard_26(c: &mut Criterion) {
    let mut q = Quamina::new();
    for letter in 'A'..='Z' {
        q.add_pattern(
            letter.to_string(),
            &format!(r#"{{"name": [{{"wildcard": "{}*"}}]}}"#, letter),
        )
        .unwrap();
    }
    let event = r#"{"name": "BELVEDERE", "other": "data"}"#.as_bytes();

    c.bench_function("wildcard_26", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_anything_but(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "not_error",
        r#"{"status": [{"anything-but": ["error", "failed", "timeout"]}]}"#,
    )
    .unwrap();
    let event = r#"{"status": "success", "code": 200}"#.as_bytes();

    c.bench_function("anything_but", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_equals_ignore_case(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "level_error",
        r#"{"level": [{"equals-ignore-case": "ERROR"}]}"#,
    )
    .unwrap();
    let event = r#"{"level": "error", "source": "app"}"#.as_bytes();

    c.bench_function("equals_ignore_case", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_multi_field_3(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "specific",
        r#"{"region": ["us-east-1"], "service": ["lambda"], "level": ["error"]}"#,
    )
    .unwrap();
    let event =
        r#"{"region": "us-east-1", "service": "lambda", "level": "error", "message": "timeout"}"#
            .as_bytes();

    c.bench_function("multi_field_3", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_exists_true(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("has_email", r#"{"email": [{"exists": true}]}"#)
        .unwrap();
    let event = r#"{"email": "alice@example.com", "name": "Alice"}"#.as_bytes();

    c.bench_function("exists_true", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_numeric_exact(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("exact_42", r#"{"count": [42]}"#).unwrap();
    let event = r#"{"count": 42, "name": "test"}"#.as_bytes();

    c.bench_function("numeric_exact", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_citylots(c: &mut Criterion) {
    let patterns = [
        r#"{ "properties": { "STREET": [ "CRANLEIGH" ] } }"#,
        r#"{ "properties": { "STREET": [ "17TH" ], "ODD_EVEN": [ "E"] } }"#,
        r#"{ "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
        r#"{ "properties": { "MAPBLKLOT": ["0011008"], "BLKLOT": ["0011008"]},  "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
    ];
    let names = ["CRANLEIGH", "17TH_Even", "Geometry", "0011008"];

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

// =========================================================================
// quamina-rs + quamina Go only (event-ruler does NOT support these)
// =========================================================================

fn bench_regexp_simple(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("letters", r#"{"value": [{"regexp": "[a-z]+"}]}"#)
        .unwrap();
    let event = r#"{"value": "hello"}"#.as_bytes();

    c.bench_function("regexp_simple", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_shellstyle_26(c: &mut Criterion) {
    let mut q = Quamina::new();
    for letter in 'A'..='Z' {
        q.add_pattern(
            letter.to_string(),
            &format!(r#"{{"name": [{{"shellstyle": "{}*"}}]}}"#, letter),
        )
        .unwrap();
    }
    let event = r#"{"name": "BELVEDERE", "other": "data"}"#.as_bytes();

    c.bench_function("shellstyle_26", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

// =========================================================================
// quamina-rs + event-ruler only (quamina Go does NOT support these)
// =========================================================================

fn bench_suffix_100(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..100 {
        q.add_pattern(
            format!("sfx{}", i),
            &format!(r#"{{"file": [{{"suffix": ".ext{}"}}]}}"#, i),
        )
        .unwrap();
    }
    let event = r#"{"file": "document.ext50"}"#.as_bytes();

    c.bench_function("suffix_100", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_numeric_range(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "range_0_100",
        r#"{"score": [{"numeric": [">=", 0, "<=", 100]}]}"#,
    )
    .unwrap();
    let event = r#"{"score": 50, "name": "test"}"#.as_bytes();

    c.bench_function("numeric_range", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

fn bench_numeric_range_10(c: &mut Criterion) {
    let mut q = Quamina::new();
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
    let event = r#"{"score": 550}"#.as_bytes();

    c.bench_function("numeric_range_10", |b| {
        b.iter(|| q.matches_for_event(black_box(event)).unwrap())
    });
}

criterion_group!(
    comparable_benches,
    // All 3 libraries
    bench_exact_single,
    bench_exact_100,
    bench_prefix_100,
    bench_wildcard_26,
    bench_anything_but,
    bench_equals_ignore_case,
    bench_multi_field_3,
    bench_exists_true,
    bench_numeric_exact,
    bench_citylots,
    // quamina-rs + quamina Go
    bench_regexp_simple,
    bench_shellstyle_26,
    // quamina-rs + event-ruler
    bench_suffix_100,
    bench_numeric_range,
    bench_numeric_range_10,
);
criterion_main!(comparable_benches);
