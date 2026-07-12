//! Emits a growth-curve CSV describing how the matcher scales as shellstyle
//! patterns accumulate. Builds patterns incrementally from `testdata/wwords.txt`
//! (a star inserted at a deterministic position in each word) and, every 100
//! `add_pattern` calls, samples `matcher_stats()` and a short matching run.
//!
//! Each CSV row reports, against the running pattern count: milliseconds spent
//! on the last 100 `add_pattern` calls, total state count, estimated byte size,
//! average stored fanout (explicit epsilon-closure entries, with self-only
//! closures contributing zero), max stored fanout, and matches/sec measured over
//! the previous 100 words. The result is an offline-analysis artifact for
//! plotting build cost and automaton complexity versus pattern count.
//!
//! Run with: cargo run --release --example research_growth > growth.csv
//!
//! Pass `--cpuprofile <file>` to also capture a CPU profile of the whole run in
//! pprof's protobuf format (inspect with `go tool pprof <file>`):
//!   cargo run --release --example research_growth -- --cpuprofile prof.pb > growth.csv

use std::fs::File;
use std::time::Instant;

use pprof::protos::Message;
use quamina::Quamina;
use rand::{RngExt, SeedableRng};

const MAX_WORDS: usize = 10_000;

fn main() {
    // Start sampling before any work so the profile covers the full harness.
    // 1 kHz trades a little overhead for finer resolution over this
    // seconds-long run (Go's runtime/pprof samples at 100 Hz).
    let cpuprofile = parse_cpuprofile_arg();
    let guard = cpuprofile
        .as_ref()
        .map(|_| pprof::ProfilerGuard::new(1000).expect("failed to start CPU profiler"));

    let contents =
        std::fs::read_to_string("testdata/wwords.txt").expect("Failed to read testdata/wwords.txt");
    let words: Vec<&str> = contents
        .lines()
        .filter(|l| !l.trim().is_empty())
        .take(MAX_WORDS)
        .collect();
    eprintln!("WC {}", words.len());

    // Same seed and star-placement as the Go research tool so curves line up.
    let mut rng = rand::rngs::StdRng::seed_from_u64(293591);
    let mut star_words = Vec::with_capacity(words.len());
    let mut patterns = Vec::with_capacity(words.len());
    for word in &words {
        let star_at = ((rng.random_range(0u64..u64::MAX) % 6) as usize).min(word.len());
        let star_word = format!("{}*{}", &word[..star_at], &word[star_at..]);
        let pattern = format!(r#"{{"x": [{{"shellstyle": "{star_word}"}}]}}"#);
        star_words.push(star_word);
        patterns.push(pattern);
    }

    let mut q = Quamina::new();
    let overall_start = Instant::now();
    let mut window_start = overall_start;

    // CSV header, then one row per 100-pattern sample point.
    println!(
        "patterns,ms/100 AddP calls,state count,byte count,average fanout,max fanout,matches/sec"
    );

    for i in 0..words.len() {
        q.add_pattern(star_words[i].clone(), &patterns[i]).unwrap();

        if i % 100 == 0 {
            let add_ms = window_start.elapsed().as_millis();
            let stats = q.matcher_stats();
            let avg_fanout = if stats.states == 0 {
                0.0
            } else {
                count_f64(stats.fanouts) / count_f64(stats.states)
            };

            // Measure matches/sec over the previous 100 words. Each was turned
            // into a pattern, so every word must match at least its own pattern.
            let per_second = if i < 100 {
                0.0
            } else {
                let match_start = Instant::now();
                for word in &words[i - 100..i] {
                    let event = format!(r#"{{"x": "{word}"}}"#);
                    let matches = q.matches_for_event(event.as_bytes()).unwrap();
                    assert!(!matches.is_empty(), "0 matches for {word}");
                }
                100.0 / match_start.elapsed().as_secs_f64()
            };

            println!(
                "{},{},{},{},{:.1},{},{:.1}",
                i + 1,
                add_ms,
                stats.states,
                stats.bytes,
                avg_fanout,
                stats.max_fanout,
                per_second,
            );

            window_start = Instant::now();
        }
    }

    let elapsed = overall_start.elapsed().as_secs_f64();
    eprintln!("Done adding {} patterns", words.len());
    eprintln!("Patterns/sec: {:.1}", count_f64(words.len()) / elapsed);

    if let (Some(guard), Some(path)) = (guard, cpuprofile) {
        write_cpu_profile(&guard, &path);
    }
}

/// Scans the command line for `--cpuprofile <file>` (or `--cpuprofile=<file>`),
/// mirroring the `-cpuprofile` flag of the Go research harness, and returns the
/// destination path when present. Exits if the flag is given without a path.
fn parse_cpuprofile_arg() -> Option<String> {
    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        if let Some(path) = arg.strip_prefix("--cpuprofile=") {
            return Some(path.to_owned());
        }
        if arg == "--cpuprofile" {
            let Some(path) = args.next() else {
                eprintln!("--cpuprofile requires a file path");
                std::process::exit(2);
            };
            return Some(path);
        }
    }
    None
}

/// Writes the sampled CPU profile to `path` in pprof's protobuf format, matching
/// the artifact Go's `runtime/pprof` produces so the same tooling can read it.
fn write_cpu_profile(guard: &pprof::ProfilerGuard, path: &str) {
    let report = guard
        .report()
        .build()
        .expect("failed to build CPU profile report");
    let profile = report.pprof().expect("failed to encode pprof profile");
    let mut file = File::create(path).expect("failed to create CPU profile file");
    profile
        .write_to_writer(&mut file)
        .expect("failed to write CPU profile");
    eprintln!("wrote CPU profile to {path}");
}

/// Converts a count to `f64` losslessly. Pattern, state, and fanout counts in
/// this artifact stay far below 2^32, so the `u32` round-trip never truncates
/// while keeping clippy's precision lint satisfied.
fn count_f64(n: usize) -> f64 {
    f64::from(u32::try_from(n).expect("count fits in u32"))
}
