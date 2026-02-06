//! Separate benchmark for bulk 10,000 pattern addition.
//!
//! This is split out from matching.rs because a single iteration takes ~28s,
//! making it too slow for CI. Run locally with: cargo bench --bench bulk_10k

use criterion::{criterion_group, criterion_main, Criterion};
use quamina::Quamina;

/// Benchmark for adding 10,000 patterns (single value each).
/// Tests scaling behavior beyond typical workloads.
fn bench_bulk_10000x1(c: &mut Criterion) {
    let patterns: Vec<String> = (0..10_000)
        .map(|i| format!(r#"{{"field": ["value_{}"]}}"#, i))
        .collect();
    c.bench_function("bulk_10000x1", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for (i, pattern) in patterns.iter().enumerate() {
                q.add_pattern(i, pattern).unwrap();
            }
        })
    });
}

criterion_group! {
    name = bulk_10k;
    config = Criterion::default()
        .sample_size(10)
        .measurement_time(std::time::Duration::from_secs(10))
        .warm_up_time(std::time::Duration::from_secs(3));
    targets = bench_bulk_10000x1
}
criterion_main!(bulk_10k);
