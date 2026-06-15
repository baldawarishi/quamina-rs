//! Emits a growth-curve CSV describing how the matcher scales as shellstyle
//! patterns accumulate. Builds patterns incrementally from `testdata/wwords.txt`
//! (a star inserted at a deterministic position in each word) and, every 100
//! `add_pattern` calls, samples `matcher_stats()` and a short matching run.
//!
//! Each CSV row reports, against the running pattern count: milliseconds spent
//! on the last 100 `add_pattern` calls, total state count, estimated byte size,
//! average fanout (epsilon-closure size), max fanout, and matches/sec measured
//! over the previous 100 words. The result is an offline-analysis artifact for
//! plotting build cost and automaton complexity versus pattern count.
//!
//! Run with: cargo run --release --example research_growth > growth.csv

use std::time::Instant;

use quamina::Quamina;
use rand::{RngExt, SeedableRng};

const MAX_WORDS: usize = 10_000;

fn main() {
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
}

/// Converts a count to `f64` losslessly. Pattern, state, and fanout counts in
/// this artifact stay far below 2^32, so the `u32` round-trip never truncates
/// while keeping clippy's precision lint satisfied.
fn count_f64(n: usize) -> f64 {
    f64::from(u32::try_from(n).expect("count fits in u32"))
}
