//! Benchmarks for quamina-rs pattern matching

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use quamina::Quamina;

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

criterion_group!(
    benches,
    bench_exact_match,
    bench_multiple_patterns,
    bench_complex_event,
    bench_regex_match,
    bench_no_match,
    bench_has_matches,
);
criterion_main!(benches);
