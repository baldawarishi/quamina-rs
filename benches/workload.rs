//! Workload characterization benchmarks. These exercise representative
//! match-time workloads against the current matcher implementation. They
//! are intended as stable baselines: subsequent matcher work (e.g., a
//! shared lazy DFA cache, eager NFA-to-DFA conversion, or other
//! optimizations) can be evaluated by re-running these benchmarks
//! unchanged and comparing. Run with: cargo bench --bench workload
//!
//! Each benchmark warms the matcher with a few hundred iterations of the
//! chosen event(s) before measurement so allocations from first-call
//! laziness do not pollute the steady-state measurement.

use std::hint::black_box;
use std::time::Instant;

use criterion::{Criterion, criterion_group, criterion_main};
use quamina::Quamina;

/// Runs a few hundred unmeasured match calls so lazily-built matcher state
/// (DFA conversion, buffers) is in place before the timed loop starts.
fn warm(q: &Quamina<String>, events: &[Vec<u8>], iterations: usize) {
    for i in 0..iterations {
        let _ = q.matches_for_event(&events[i % events.len()]);
    }
}

/// Builds the overlapping-wildcard pattern set: `n` shellstyle patterns of
/// the form `*a*b*c*` whose anchor letters cycle through a 13-letter
/// alphabet, so consecutive patterns overlap heavily.
fn add_overlapping_wildcards(q: &mut Quamina<String>, n: usize) {
    for i in 0..n {
        let anchor = |offset: usize| char::from(b'a' + u8::try_from((i + offset) % 13).unwrap());
        let (a1, a2, a3) = (anchor(0), anchor(1), anchor(2));
        let p = format!(r#"{{"x": [{{"shellstyle": "*{a1}*{a2}*{a3}*"}}]}}"#);
        q.add_pattern(format!("p{i}"), &p).unwrap();
    }
}

/// One exact pattern, uniform event.
fn bench_exact_string(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("p".to_string(), r#"{"x": ["foobar"]}"#)
        .unwrap();
    let events = vec![br#"{"x":"foobar"}"#.to_vec()];
    warm(&q, &events, 100);
    c.bench_function("workload_exact_string", |b| {
        b.iter(|| black_box(q.matches_for_event(black_box(&events[0])).unwrap()));
    });
}

/// One wildcard pattern, uniform matching event. Hot single-NFA traversal.
fn bench_single_shellstyle(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("p".to_string(), r#"{"x": [{"shellstyle": "*foo*"}]}"#)
        .unwrap();
    let events = vec![br#"{"x":"abcdefoobarghi"}"#.to_vec()];
    warm(&q, &events, 100);
    c.bench_function("workload_single_shellstyle", |b| {
        b.iter(|| black_box(q.matches_for_event(black_box(&events[0])).unwrap()));
    });
}

/// N overlapping shellstyle wildcards on the same field. Cost scales with N
/// as the merged NFA's epsilon closure grows; this is the textbook
/// NFA-explosion workload.
fn bench_many_overlapping_wildcards(c: &mut Criterion) {
    for n in [8usize, 16, 32, 64, 128] {
        let mut q = Quamina::new();
        add_overlapping_wildcards(&mut q, n);
        let events = vec![br#"{"x":"abcdefghijklm"}"#.to_vec()];
        warm(&q, &events, 100);
        c.bench_function(&format!("workload_many_overlapping_wildcards/N={n}"), |b| {
            b.iter(|| black_box(q.matches_for_event(black_box(&events[0])).unwrap()));
        });
    }
}

/// 20 regex patterns with alternation (foo|bar|...). Exercises dense
/// epsilon closure structure.
fn bench_regex_alternation(c: &mut Criterion) {
    let mut q = Quamina::new();
    let keywords = ["foo", "bar", "baz", "quux", "xyzzy"];
    for i in 0..20 {
        let kw = keywords[i % keywords.len()];
        let p = format!(r#"{{"x": [{{"regex": "({kw}|alt{i})\\d+"}}]}}"#);
        q.add_pattern(format!("p{i}"), &p).unwrap();
    }
    let events = vec![br#"{"x":"foo42"}"#.to_vec()];
    warm(&q, &events, 100);
    c.bench_function("workload_regex_alternation", |b| {
        b.iter(|| black_box(q.matches_for_event(black_box(&events[0])).unwrap()));
    });
}

/// Long literal substring inside a regex. Real log-line shape.
fn bench_literal_in_regex(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("p".to_string(), r#"{"x": [{"regex": ".*ERROR.*\\d+.*"}]}"#)
        .unwrap();
    let events =
        vec![br#"{"x":"2026-05-18T10:00:00 ERROR request_id=42 connection refused"}"#.to_vec()];
    warm(&q, &events, 100);
    c.bench_function("workload_literal_in_regex", |b| {
        b.iter(|| black_box(q.matches_for_event(black_box(&events[0])).unwrap()));
    });
}

/// Regex with {n,m} quantifier on a character class. Eager NFA-to-DFA
/// conversion blows up here.
fn bench_quantified_char_class(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..5 {
        let p = format!(r#"{{"x": [{{"regex": "[a-z]{{8,16}}sfx{i}"}}]}}"#);
        q.add_pattern(format!("p{i}"), &p).unwrap();
    }
    let events = vec![br#"{"x":"abcdefghijksfx3"}"#.to_vec()];
    warm(&q, &events, 100);
    c.bench_function("workload_quantified_char_class", |b| {
        b.iter(|| black_box(q.matches_for_event(black_box(&events[0])).unwrap()));
    });
}

/// 200 anchored regex patterns with shared prefix/suffix. Cost scales with
/// the cross-product of merged regex paths.
fn bench_many_anchored_regex(c: &mut Criterion) {
    let mut q = Quamina::new();
    for i in 0..200 {
        let p = format!(r#"{{"x": [{{"regex": "PFX[0-9]+SFX{i}"}}]}}"#);
        q.add_pattern(format!("p{i}"), &p).unwrap();
    }
    let events: Vec<Vec<u8>> = [
        br#"{"x":"PFX42SFX17"}"#.to_vec(),
        br#"{"x":"PFX99SFX42"}"#.to_vec(),
        br#"{"x":"PFX1SFX199"}"#.to_vec(),
        br#"{"x":"PFX9999SFX0"}"#.to_vec(),
    ]
    .to_vec();
    warm(&q, &events, 100);
    c.bench_function("workload_many_anchored_regex", |b| {
        let mut i = 0usize;
        b.iter(|| {
            let matches = q
                .matches_for_event(black_box(&events[i % events.len()]))
                .unwrap();
            i += 1;
            black_box(matches)
        });
    });
}

/// Regex designed to maximize epsilon closure depth via nested alternation
/// and quantifiers.
fn bench_deep_epsilon_nest(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern(
        "p".to_string(),
        r#"{"x": [{"regex": "((a|b|c)*(d|e|f)*)+"}]}"#,
    )
    .unwrap();
    let events = vec![br#"{"x":"abcdefabcdefabcdef"}"#.to_vec()];
    warm(&q, &events, 100);
    c.bench_function("workload_deep_epsilon_nest", |b| {
        b.iter(|| black_box(q.matches_for_event(black_box(&events[0])).unwrap()));
    });
}

/// Pattern admits a huge state space (5 wildcards) and events are permuted
/// so each visits different state trajectories. Adversarial input for any
/// state-set-caching strategy.
fn bench_cache_thrashing(c: &mut Criterion) {
    let mut q = Quamina::new();
    q.add_pattern("p".to_string(), r#"{"x": [{"shellstyle": "*X*Y*Z*W*V*"}]}"#)
        .unwrap();
    let perms: Vec<Vec<u8>> = [
        br#"{"x":"XYZWVabcdefghij"}"#.to_vec(),
        br#"{"x":"jihgfedcbaVWZYX"}"#.to_vec(),
        br#"{"x":"aXbYcZdWeVfghij"}"#.to_vec(),
        br#"{"x":"VWZXYjihgfedcba"}"#.to_vec(),
        br#"{"x":"ZYXWVbacdefghij"}"#.to_vec(),
    ]
    .to_vec();
    c.bench_function("workload_cache_thrashing", |b| {
        let mut i = 0usize;
        b.iter(|| {
            let matches = q
                .matches_for_event(black_box(&perms[i % perms.len()]))
                .unwrap();
            i += 1;
            black_box(matches)
        });
    });
}

/// G threads run matches_for_event against one shared matcher (matching is
/// lock-free on a frozen snapshot, so threads share the instance directly).
/// Measures contention behavior under concurrent match load; reported time
/// is wall-clock per match across all threads.
fn bench_parallel_matchers(c: &mut Criterion) {
    for threads in [8usize, 16, 32, 64] {
        // Reuse the overlapping-wildcard pattern set at N=64.
        let mut q = Quamina::new();
        add_overlapping_wildcards(&mut q, 64);
        let events = vec![br#"{"x":"abcdefghijklm"}"#.to_vec()];
        warm(&q, &events, 200);
        c.bench_function(&format!("workload_parallel_matchers/G={threads}"), |b| {
            b.iter_custom(|iters| {
                let q = &q;
                let ev = &events[0];
                let start = Instant::now();
                std::thread::scope(|s| {
                    for t in 0..threads {
                        // Distribute iterations evenly; early threads pick up
                        // the remainder.
                        let share =
                            iters / threads as u64 + u64::from((t as u64) < iters % threads as u64);
                        s.spawn(move || {
                            for _ in 0..share {
                                black_box(q.matches_for_event(black_box(ev)).unwrap());
                            }
                        });
                    }
                });
                start.elapsed()
            });
        });
    }
}

criterion_group!(
    workload,
    bench_exact_string,
    bench_single_shellstyle,
    bench_many_overlapping_wildcards,
    bench_regex_alternation,
    bench_literal_in_regex,
    bench_quantified_char_class,
    bench_many_anchored_regex,
    bench_deep_epsilon_nest,
    bench_cache_thrashing,
    bench_parallel_matchers,
);
criterion_main!(workload);
