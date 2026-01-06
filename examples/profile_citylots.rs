//! Profile citylots matching for flamegraph analysis
//!
//! Run with: cargo flamegraph --example profile_citylots

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

fn main() {
    let patterns = [
        r#"{ "properties": { "STREET": [ "CRANLEIGH" ] } }"#,
        r#"{ "properties": { "STREET": [ "17TH" ], "ODD_EVEN": [ "E"] } }"#,
        r#"{ "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
        r#"{ "properties": { "MAPBLKLOT": ["0011008"], "BLKLOT": ["0011008"]},  "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
    ];
    let names = ["CRANLEIGH", "17TH Even", "Geometry", "0011008"];

    let mut q = Quamina::new();
    for (name, pattern) in names.iter().zip(patterns.iter()) {
        q.add_pattern(*name, pattern).unwrap();
    }

    let lines = load_citylots_lines();
    eprintln!("Loaded {} lines", lines.len());

    // Run many iterations for good profiling data
    let iterations = 10;
    let start = std::time::Instant::now();

    for _ in 0..iterations {
        for line in &lines {
            let _ = q.matches_for_event(line);
        }
    }

    let elapsed = start.elapsed();
    let total_ops = iterations * lines.len();
    let ns_per_op = elapsed.as_nanos() / total_ops as u128;
    eprintln!("{} total operations in {:?}", total_ops, elapsed);
    eprintln!("{} ns/op", ns_per_op);
}
