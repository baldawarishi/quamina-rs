//! Scaling benchmark for incremental pattern addition.
//!
//! Run locally with:
//! cargo bench --bench add_pattern_scaling
//!
//! Criterion will render wall-time-vs-K plots in target/criterion. The
//! `prefix_same_field` case exercises the arena merge path used by
//! `merge_into_main_arena`; `exact_same_field` is a control for the in-place
//! string insertion path.

use std::hint::black_box;
use std::time::Duration;

use criterion::{
    BenchmarkId, Criterion, PlotConfiguration, Throughput, criterion_group, criterion_main,
};
use quamina::Quamina;

const PATTERN_COUNTS: [usize; 8] = [64, 128, 256, 512, 1_024, 2_048, 4_096, 8_192];

fn exact_patterns(count: usize) -> Vec<String> {
    (0..count)
        .map(|i| format!(r#"{{"field": ["value_{i:06}"]}}"#))
        .collect()
}

fn prefix_patterns(count: usize) -> Vec<String> {
    (0..count)
        .map(|i| format!(r#"{{"field": [{{"prefix": "value_{i:06}"}}]}}"#))
        .collect()
}

fn add_patterns(patterns: &[String]) {
    let mut q = Quamina::<usize>::new();
    for (i, pattern) in patterns.iter().enumerate() {
        q.add_pattern(i, black_box(pattern.as_str())).unwrap();
    }
    black_box(q);
}

fn add_patterns_then_first_match(patterns: &[String], event: &[u8]) {
    let mut q = Quamina::<usize>::new();
    for (i, pattern) in patterns.iter().enumerate() {
        q.add_pattern(i, black_box(pattern.as_str())).unwrap();
    }
    let build_stats = q.matcher_stats();
    let matches = q.matches_for_event(black_box(event)).unwrap();
    let frozen_stats = q.arena_stats();
    black_box((q, build_stats, matches, frozen_stats));
}

fn bench_add_pattern_scaling(c: &mut Criterion) {
    let mut group = c.benchmark_group("add_pattern_scaling");
    group.plot_config(PlotConfiguration::default());

    for count in PATTERN_COUNTS {
        let patterns = prefix_patterns(count);
        group.throughput(Throughput::Elements(
            u64::try_from(count).expect("pattern count fits in u64"),
        ));
        group.bench_with_input(
            BenchmarkId::new("prefix_same_field", count),
            &patterns,
            |b, patterns| b.iter(|| add_patterns(black_box(patterns))),
        );
    }

    for count in PATTERN_COUNTS {
        let patterns = prefix_patterns(count);
        let event = format!(r#"{{"field": "value_{:06}_tail"}}"#, count - 1).into_bytes();
        group.throughput(Throughput::Elements(
            u64::try_from(count).expect("pattern count fits in u64"),
        ));
        group.bench_with_input(
            BenchmarkId::new("prefix_add_then_first_match", count),
            &(patterns, event),
            |b, (patterns, event)| {
                b.iter(|| add_patterns_then_first_match(black_box(patterns), black_box(event)));
            },
        );
    }

    for count in PATTERN_COUNTS {
        let patterns = exact_patterns(count);
        group.throughput(Throughput::Elements(
            u64::try_from(count).expect("pattern count fits in u64"),
        ));
        group.bench_with_input(
            BenchmarkId::new("exact_same_field", count),
            &patterns,
            |b, patterns| b.iter(|| add_patterns(black_box(patterns))),
        );
    }

    for count in PATTERN_COUNTS {
        let patterns = exact_patterns(count);
        let event = format!(r#"{{"field": "value_{:06}"}}"#, count - 1).into_bytes();
        group.throughput(Throughput::Elements(
            u64::try_from(count).expect("pattern count fits in u64"),
        ));
        group.bench_with_input(
            BenchmarkId::new("exact_add_then_first_match", count),
            &(patterns, event),
            |b, (patterns, event)| {
                b.iter(|| add_patterns_then_first_match(black_box(patterns), black_box(event)));
            },
        );
    }

    group.finish();
}

criterion_group! {
    name = add_pattern_scaling;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(Duration::from_secs(5))
        .warm_up_time(Duration::from_secs(1));
    targets = bench_add_pattern_scaling
}
criterion_main!(add_pattern_scaling);
