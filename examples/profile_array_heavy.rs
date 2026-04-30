//! Reproduce the `array_heavy_100_elements` benchmark workload and sweep the
//! SIMD gate thresholds to see whether the pinned `STRING_SIMD_THRESHOLD=0`
//! is costing us on arrays of very short strings.
//!
//! Run: cargo run --release --example profile_array_heavy

use quamina::Quamina;
use quamina::flatten_json_simd::{BLOCK_SIMD_THRESHOLD, STRING_SIMD_THRESHOLD};
use std::sync::atomic::Ordering;

fn build_event() -> Vec<u8> {
    let many_tags = (0..100)
        .map(|i| {
            if i == 50 {
                "important".to_string()
            } else {
                format!("tag{i}")
            }
        })
        .map(|t| format!(r#""{t}""#))
        .collect::<Vec<_>>()
        .join(", ");
    format!(r#"{{"tags": [{many_tags}]}}"#).into_bytes()
}

fn build_q() -> Quamina {
    let mut q = Quamina::new();
    q.add_pattern("tags".to_string(), r#"{"tags": ["important"]}"#)
        .unwrap();
    q
}

fn measure<F: Fn()>(iterations: usize, f: F) -> f64 {
    for _ in 0..(iterations / 10).max(1) {
        f();
    }
    let start = std::time::Instant::now();
    for _ in 0..iterations {
        f();
    }
    start.elapsed().as_nanos() as f64 / iterations as f64
}

fn main() {
    let q = build_q();
    let event = build_event();
    println!("event size: {} bytes", event.len());
    // Sanity: match works.
    let matches = q.matches_for_event(&event).unwrap();
    assert!(!matches.is_empty());

    let iterations = 200_000;
    let thresholds = [0usize, 16, 32, 48, 64, 96, 128, 256, 512, usize::MAX];

    println!("\n=== array_heavy_100_elements · STRING gate (BLOCK=0) ===");
    println!("{:>10}  {:>12}  {:>10}", "thresh", "ns/call", "rel");
    BLOCK_SIMD_THRESHOLD.store(0, Ordering::Relaxed);
    let mut baseline: Option<f64> = None;
    for &t in &thresholds {
        STRING_SIMD_THRESHOLD.store(t, Ordering::Relaxed);
        let ns = measure(iterations, || {
            let _ = q.matches_for_event(&event);
        });
        let rel = match baseline {
            Some(b) => format!("{:+.2}%", (ns / b - 1.0) * 100.0),
            None => {
                baseline = Some(ns);
                "baseline".into()
            }
        };
        let label = if t == usize::MAX {
            "MAX".into()
        } else {
            t.to_string()
        };
        println!("{label:>10}  {ns:>12.2}  {rel:>10}");
    }

    println!("\n=== array_heavy_100_elements · BLOCK gate (STRING=0) ===");
    println!("{:>10}  {:>12}  {:>10}", "thresh", "ns/call", "rel");
    STRING_SIMD_THRESHOLD.store(0, Ordering::Relaxed);
    baseline = None;
    for &t in &thresholds {
        BLOCK_SIMD_THRESHOLD.store(t, Ordering::Relaxed);
        let ns = measure(iterations, || {
            let _ = q.matches_for_event(&event);
        });
        let rel = match baseline {
            Some(b) => format!("{:+.2}%", (ns / b - 1.0) * 100.0),
            None => {
                baseline = Some(ns);
                "baseline".into()
            }
        };
        let label = if t == usize::MAX {
            "MAX".into()
        } else {
            t.to_string()
        };
        println!("{label:>10}  {ns:>12.2}  {rel:>10}");
    }

    STRING_SIMD_THRESHOLD.store(0, Ordering::Relaxed);
    BLOCK_SIMD_THRESHOLD.store(0, Ordering::Relaxed);
}
