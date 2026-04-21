//! Sweep SIMD gate thresholds for `skip_string_value` / `skip_block` to find
//! the point where SIMD setup cost breaks even against the scalar tail.
//!
//! Run with:
//!   cargo run --release --example profile_simd_threshold
//!
//! Workloads exercised:
//!   - citylots (json events with many skippable string/object fields)
//!   - status-like pattern (nested objects, small events)
//!
//! For each threshold value we run N iterations of the full matching pass and
//! print ns/op. Lower is better. The threshold that wins for one workload is
//! not necessarily the threshold that wins for another — the output lets you
//! eyeball the trade-off.

use flate2::read::GzDecoder;
use quamina::Quamina;
use quamina::flatten_json_simd::{BLOCK_SIMD_THRESHOLD, STRING_SIMD_THRESHOLD};
use std::io::{BufRead, BufReader};
use std::sync::atomic::Ordering;

fn load_citylots_lines() -> Vec<Vec<u8>> {
    let file = std::fs::File::open("testdata/citylots.jlines.gz")
        .expect("Failed to open testdata/citylots.jlines.gz");
    let decoder = GzDecoder::new(file);
    BufReader::new(decoder)
        .lines()
        .map(|l| l.unwrap().into_bytes())
        .collect()
}

fn build_citylots_matcher() -> Quamina {
    let patterns = [
        (
            r#"CRANLEIGH"#,
            r#"{ "properties": { "STREET": [ "CRANLEIGH" ] } }"#,
        ),
        (
            r#"17TH"#,
            r#"{ "properties": { "STREET": [ "17TH" ], "ODD_EVEN": [ "E"] } }"#,
        ),
        (
            r#"Geom"#,
            r#"{ "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
        ),
        (
            r#"0011008"#,
            r#"{ "properties": { "MAPBLKLOT": ["0011008"], "BLKLOT": ["0011008"]},  "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
        ),
    ];
    let mut q = Quamina::new();
    for (name, pat) in patterns {
        q.add_pattern(name.to_string(), pat).unwrap();
    }
    q
}

fn build_synthetic_workload() -> (Quamina, Vec<Vec<u8>>) {
    // Pattern only cares about one shallow field; the rest of the event is
    // pure "skip material" of varying sizes — good for exercising both gates.
    let mut q = Quamina::new();
    q.add_pattern("status".to_string(), r#"{ "status": [ "ok" ] }"#)
        .unwrap();

    // Vary the "junk" payload size so different thresholds are exercised.
    let mut events = Vec::new();
    for junk_len in [0usize, 16, 48, 96, 200, 512, 1024, 4096] {
        let junk_str: String = "x".repeat(junk_len);
        let event = format!(
            r#"{{"a":"{s}","nested":{{"b":"{s}","c":[1,2,3,"{s}"]}},"status":"ok","trailing":"{s}"}}"#,
            s = junk_str
        );
        events.push(event.into_bytes());
    }
    (q, events)
}

fn measure<F: Fn()>(iterations: usize, f: F) -> f64 {
    // Warm-up
    for _ in 0..(iterations / 10).max(1) {
        f();
    }
    let start = std::time::Instant::now();
    for _ in 0..iterations {
        f();
    }
    let elapsed = start.elapsed();
    elapsed.as_nanos() as f64 / iterations as f64
}

fn run_sweep(label: &str, thresholds: &[usize], gate: &str, run: impl Fn() -> usize) {
    println!("\n=== {label} (gate: {gate}) ===");
    println!("{:>8}  {:>12}  {:>10}", "thresh", "ns/call", "rel");
    let mut baseline: Option<f64> = None;
    for &t in thresholds {
        match gate {
            "string" => STRING_SIMD_THRESHOLD.store(t, Ordering::Relaxed),
            "block" => BLOCK_SIMD_THRESHOLD.store(t, Ordering::Relaxed),
            "both" => {
                STRING_SIMD_THRESHOLD.store(t, Ordering::Relaxed);
                BLOCK_SIMD_THRESHOLD.store(t, Ordering::Relaxed);
            }
            _ => unreachable!(),
        }
        let ops_per_call = run();
        let iterations = 5;
        let ns = measure(iterations, || {
            let _ = run();
        }) / ops_per_call as f64;
        let rel = match baseline {
            Some(b) => format!("{:+.1}%", (ns / b - 1.0) * 100.0),
            None => {
                baseline = Some(ns);
                "baseline".into()
            }
        };
        println!("{t:>8}  {ns:>12.2}  {rel:>10}");
    }
}

fn main() {
    println!("Loading citylots...");
    let lines = load_citylots_lines();
    println!("  {} lines", lines.len());
    let citylots_q = build_citylots_matcher();
    let (synth_q, synth_events) = build_synthetic_workload();

    // Thresholds to sweep. Powers of two from 8 → 2048, plus the extremes.
    let thresholds: Vec<usize> = vec![0, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, usize::MAX];

    // --- citylots: string gate ---
    BLOCK_SIMD_THRESHOLD.store(0, Ordering::Relaxed);
    run_sweep(
        "citylots · skip_string_value gate",
        &thresholds,
        "string",
        || {
            for line in &lines {
                let _ = citylots_q.matches_for_event(line);
            }
            lines.len()
        },
    );

    // --- citylots: block gate ---
    STRING_SIMD_THRESHOLD.store(64, Ordering::Relaxed);
    run_sweep("citylots · skip_block gate", &thresholds, "block", || {
        for line in &lines {
            let _ = citylots_q.matches_for_event(line);
        }
        lines.len()
    });

    // --- citylots: both gates ---
    run_sweep("citylots · both gates tied", &thresholds, "both", || {
        for line in &lines {
            let _ = citylots_q.matches_for_event(line);
        }
        lines.len()
    });

    // --- synthetic varying-size events ---
    BLOCK_SIMD_THRESHOLD.store(0, Ordering::Relaxed);
    run_sweep(
        "synthetic · skip_string_value gate",
        &thresholds,
        "string",
        || {
            for ev in &synth_events {
                let _ = synth_q.matches_for_event(ev);
            }
            synth_events.len()
        },
    );

    STRING_SIMD_THRESHOLD.store(64, Ordering::Relaxed);
    run_sweep("synthetic · skip_block gate", &thresholds, "block", || {
        for ev in &synth_events {
            let _ = synth_q.matches_for_event(ev);
        }
        synth_events.len()
    });

    // Reset to defaults so this binary's closing allocs don't mislead anyone.
    STRING_SIMD_THRESHOLD.store(64, Ordering::Relaxed);
    BLOCK_SIMD_THRESHOLD.store(0, Ordering::Relaxed);
}
