//! Reader throughput and latency while one writer churns patterns.
//!
//! Compares `parking_lot::RwLock<Quamina>` against `SharedQuamina`. The writer
//! loops delete(id) + add(id) and rebuilds every `REBUILD_EVERY` cycles, either
//! flat out ("hot"), once per millisecond ("paced"), or not at all ("none").
//!
//! Run: `cargo bench --bench concurrent_update`
//! Env: `QUAMINA_BENCH_SECS` (default 2) seconds per configuration.

use parking_lot::RwLock;
use quamina::{Quamina, SharedQuamina};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::time::{Duration, Instant};

const PATTERNS: usize = 2000;
const FIELDS: usize = 20;
const EVENTS: usize = 256;
const REBUILD_EVERY: usize = 100;
const SAMPLE_EVERY: usize = 8;

fn pattern(i: usize) -> String {
    let field = i % FIELDS;
    match i % 10 {
        8 => format!(r#"{{"f{field}": [{{"prefix": "v{i}"}}]}}"#),
        9 => format!(r#"{{"f{field}": [{{"shellstyle": "v*{i}"}}]}}"#),
        _ => format!(r#"{{"f{field}": ["v{i}"]}}"#),
    }
}

fn events() -> Vec<Vec<u8>> {
    (0..EVENTS)
        .map(|e| {
            let body: Vec<String> = (0..FIELDS)
                .map(|f| {
                    let i = (e * 37 + f * 101) % PATTERNS;
                    // Land on the pattern for this field about half the time.
                    let i = if e % 2 == 0 { i - i % FIELDS + f } else { i };
                    format!(r#""f{f}": "v{i}""#)
                })
                .collect();
            format!("{{{}}}", body.join(", ")).into_bytes()
        })
        .collect()
}

trait Target: Sync {
    fn matches(&self, event: &[u8]) -> usize;
    fn churn(&self, id: usize, rebuild: bool);
}

impl Target for RwLock<Quamina<String>> {
    fn matches(&self, event: &[u8]) -> usize {
        self.read().matches_for_event(event).unwrap().len()
    }
    fn churn(&self, id: usize, rebuild: bool) {
        let key = format!("p{id}");
        self.write().delete_patterns(&key).unwrap();
        self.write().add_pattern(key, &pattern(id)).unwrap();
        if rebuild {
            self.write().rebuild();
        }
    }
}

impl Target for SharedQuamina<String> {
    fn matches(&self, event: &[u8]) -> usize {
        self.matches_for_event(event).unwrap().len()
    }
    fn churn(&self, id: usize, rebuild: bool) {
        let key = format!("p{id}");
        self.delete_patterns(&key).unwrap();
        self.add_pattern(key, &pattern(id)).unwrap();
        if rebuild {
            self.rebuild();
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Writer {
    None,
    Paced,
    Hot,
}

impl Writer {
    const fn name(self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Paced => "paced",
            Self::Hot => "hot",
        }
    }
}

struct Outcome {
    reads_per_sec: u128,
    p50: u64,
    p99: u64,
    max: u64,
    writes_per_sec: u128,
}

fn run(
    target: &dyn Target,
    events: &[Vec<u8>],
    readers: usize,
    mode: Writer,
    dur: Duration,
) -> Outcome {
    let stop = AtomicBool::new(false);
    let reads = AtomicUsize::new(0);
    let writes = AtomicUsize::new(0);
    let mut samples: Vec<u64> = Vec::new();

    std::thread::scope(|s| {
        let handles: Vec<_> = (0..readers)
            .map(|r| {
                let (stop, reads) = (&stop, &reads);
                s.spawn(move || {
                    let mut local = Vec::with_capacity(1 << 20);
                    let mut n = 0;
                    let mut sink = 0usize;
                    while !stop.load(Ordering::Relaxed) {
                        let event = &events[(n + r * 17) % events.len()];
                        if n.is_multiple_of(SAMPLE_EVERY) {
                            let t = Instant::now();
                            sink += target.matches(event);
                            local.push(u64::try_from(t.elapsed().as_nanos()).unwrap_or(u64::MAX));
                        } else {
                            sink += target.matches(event);
                        }
                        n += 1;
                    }
                    std::hint::black_box(sink);
                    reads.fetch_add(n, Ordering::Relaxed);
                    local
                })
            })
            .collect();

        if mode != Writer::None {
            let (stop, writes) = (&stop, &writes);
            s.spawn(move || {
                let mut cycle = 0;
                while !stop.load(Ordering::Relaxed) {
                    cycle += 1;
                    let id = (cycle * 7919) % PATTERNS;
                    target.churn(id, cycle.is_multiple_of(REBUILD_EVERY));
                    if mode == Writer::Paced {
                        std::thread::sleep(Duration::from_millis(1));
                    }
                }
                writes.store(cycle, Ordering::Relaxed);
            });
        }

        std::thread::sleep(dur);
        stop.store(true, Ordering::Relaxed);
        for h in handles {
            samples.extend(h.join().unwrap());
        }
    });

    samples.sort_unstable();
    let pct = |p: usize| samples[(samples.len() - 1) * p / 100];
    let per_sec = |n: &AtomicUsize| {
        u128::try_from(n.load(Ordering::Relaxed)).unwrap() * 1000 / dur.as_millis()
    };
    Outcome {
        reads_per_sec: per_sec(&reads),
        p50: pct(50),
        p99: pct(99),
        max: *samples.last().unwrap(),
        writes_per_sec: per_sec(&writes),
    }
}

fn build() -> Quamina<String> {
    let mut q = Quamina::new();
    q.set_auto_rebuild(false);
    for i in 0..PATTERNS {
        q.add_pattern(format!("p{i}"), &pattern(i)).unwrap();
    }
    q
}

fn main() {
    let secs: f64 = std::env::var("QUAMINA_BENCH_SECS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(2.0);
    let dur = Duration::from_secs_f64(secs);
    let events = events();

    println!(
        "{:<7} {:>3} {:<6} {:>12} {:>9} {:>9} {:>11} {:>10}",
        "target", "rd", "writer", "reads/s", "p50 ns", "p99 ns", "max ns", "writes/s"
    );
    for readers in [1, 2, 4, 8] {
        for writer in [Writer::None, Writer::Paced, Writer::Hot] {
            let rwlock = RwLock::new(build());
            let shared = SharedQuamina::new(build()).unwrap();
            let targets: [(&str, &dyn Target); 2] = [("rwlock", &rwlock), ("shared", &shared)];
            for (name, target) in targets {
                let o = run(target, &events, readers, writer, dur);
                println!(
                    "{:<7} {:>3} {:<6} {:>12} {:>9} {:>9} {:>11} {:>10}",
                    name,
                    readers,
                    writer.name(),
                    o.reads_per_sec,
                    o.p50,
                    o.p99,
                    o.max,
                    o.writes_per_sec
                );
            }
        }
    }
}
