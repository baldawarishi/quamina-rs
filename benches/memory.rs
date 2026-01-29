//! Memory profiling benchmarks using dhat
//!
//! Run with: cargo bench --bench memory --features dhat-heap
//!
//! For JSON output (easier diffing): cargo bench --bench memory --features dhat-heap -- --json
//!
//! This measures heap allocations for key operations:
//! - Pattern compilation (add_pattern)
//! - Event matching (matches_for_event)
//! - Steady-state memory usage
//!
//! To compare with Go quamina, run equivalent Go benchmarks with:
//!   go test -bench=. -benchmem ./...

use flate2::read::GzDecoder;
use quamina::Quamina;
use rand::prelude::*;
use std::env;
use std::io::{BufRead, BufReader};

#[cfg(feature = "dhat-heap")]
#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

struct MemoryStats {
    name: String,
    total_bytes: usize,
    max_bytes: usize,
    total_allocs: usize,
    max_allocs: usize,
}

impl MemoryStats {
    #[cfg(feature = "dhat-heap")]
    fn capture(name: &str) -> Self {
        let stats = dhat::HeapStats::get();
        Self {
            name: name.to_string(),
            total_bytes: stats.total_bytes as usize,
            max_bytes: stats.max_bytes,
            total_allocs: stats.total_blocks as usize,
            max_allocs: stats.max_blocks,
        }
    }

    #[cfg(not(feature = "dhat-heap"))]
    fn capture(name: &str) -> Self {
        Self {
            name: name.to_string(),
            total_bytes: 0,
            max_bytes: 0,
            total_allocs: 0,
            max_allocs: 0,
        }
    }

    fn print_human(&self) {
        println!("  Total bytes allocated: {}", self.total_bytes);
        println!("  Max bytes live (peak): {}", self.max_bytes);
        println!("  Total allocations:     {}", self.total_allocs);
        println!("  Max blocks live:       {}", self.max_allocs);
    }

    fn to_json(&self) -> String {
        format!(
            r#"  "{}": {{"total_bytes": {}, "max_bytes": {}, "total_allocs": {}, "max_allocs": {}}}"#,
            self.name, self.total_bytes, self.max_bytes, self.total_allocs, self.max_allocs
        )
    }
}

/// Profile: Adding 100 simple patterns (one field, one value each)
fn profile_pattern_add_simple() -> MemoryStats {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    let mut q = Quamina::<usize>::new();
    for i in 0..100 {
        q.add_pattern(i, &format!(r#"{{"field_{}": ["value_{}"]}}"#, i, i))
            .unwrap();
    }

    let stats = MemoryStats::capture("pattern_add_100_simple");
    drop(q);
    stats
}

/// Profile: Adding 100 patterns with 10 values each (tests automaton merging)
fn profile_pattern_add_multivalue() -> MemoryStats {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    let mut q = Quamina::<usize>::new();
    for i in 0..100 {
        let values: String = (0..10)
            .map(|j| format!("\"value_{}_{}\"", i, j))
            .collect::<Vec<_>>()
            .join(", ");
        let pattern = format!(r#"{{"field": [{}]}}"#, values);
        q.add_pattern(i, &pattern).unwrap();
    }

    let stats = MemoryStats::capture("pattern_add_100x10_multivalue");
    drop(q);
    stats
}

/// Profile: Adding 10 regex patterns (NFA construction)
fn profile_regex_patterns() -> MemoryStats {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    let mut q = Quamina::<usize>::new();
    let patterns = [
        r#"{"email": [{"regex": "^[a-z]+@[a-z]+\\.[a-z]+$"}]}"#,
        r#"{"path": [{"regex": "/api/v[0-9]+/.*"}]}"#,
        r#"{"id": [{"regex": "[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}"}]}"#,
        r#"{"phone": [{"regex": "\\+?[0-9]{1,3}[0-9]{10}"}]}"#,
        r#"{"ip": [{"regex": "[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}"}]}"#,
        r#"{"url": [{"regex": "https?://[a-z]+\\.[a-z]+"}]}"#,
        r#"{"code": [{"regex": "[A-Z]{2,4}-[0-9]{3,6}"}]}"#,
        r#"{"hash": [{"regex": "[a-f0-9]{40}"}]}"#,
        r#"{"version": [{"regex": "v?[0-9]+\\.[0-9]+\\.[0-9]+"}]}"#,
        r#"{"date": [{"regex": "[0-9]{4}-[0-9]{2}-[0-9]{2}"}]}"#,
    ];

    for (i, pattern) in patterns.iter().enumerate() {
        q.add_pattern(i, pattern).unwrap();
    }

    let stats = MemoryStats::capture("pattern_add_10_regex");
    drop(q);
    stats
}

/// Profile: Adding 100 numeric range patterns
fn profile_numeric_patterns() -> MemoryStats {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    let mut q = Quamina::<usize>::new();
    for i in 0..100 {
        let lower = i * 100;
        let upper = (i + 1) * 100;
        q.add_pattern(
            i,
            &format!(
                r#"{{"score": [{{"numeric": [">=", {}, "<", {}]}}]}}"#,
                lower, upper
            ),
        )
        .unwrap();
    }

    let stats = MemoryStats::capture("pattern_add_100_numeric_range");
    drop(q);
    stats
}

/// Profile: Steady-state memory for 1000 loaded patterns
fn profile_steady_state_1000() -> MemoryStats {
    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    let mut q = Quamina::<usize>::new();
    for i in 0..1000 {
        q.add_pattern(i, &format!(r#"{{"field_{}": ["value_{}"]}}"#, i, i))
            .unwrap();
    }

    let stats = MemoryStats::capture("steady_state_1000_patterns");
    drop(q);
    stats
}

/// Profile: Matching hot path (1000 matches against 100 patterns)
fn profile_matching_hot_path() -> MemoryStats {
    // Build matcher outside profiling
    let mut q = Quamina::<usize>::new();
    for i in 0..100 {
        q.add_pattern(i, &format!(r#"{{"status": ["status_{}"]}}"#, i))
            .unwrap();
    }

    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    for i in 0..1000 {
        let event = format!(r#"{{"status": "status_{}"}}"#, i % 100);
        let _ = q.matches_for_event(event.as_bytes()).unwrap();
    }

    MemoryStats::capture("matching_1000_events_100_patterns")
}

/// Profile: Matching large JSON events (status.json)
fn profile_matching_large_json() -> MemoryStats {
    let event = std::fs::read("testdata/status.json").expect("testdata/status.json required");

    let mut q = Quamina::new();
    q.add_pattern(
        "context",
        r#"{ "context": { "user_id": [9034], "friends_count": [158] } }"#,
    )
    .unwrap();

    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    for _ in 0..100 {
        let _ = q.matches_for_event(&event).unwrap();
    }

    MemoryStats::capture("matching_100_large_json_events")
}

/// Profile: Matching with no matches (worst case traversal)
fn profile_matching_no_match() -> MemoryStats {
    let mut q = Quamina::<usize>::new();
    for i in 0..100 {
        q.add_pattern(i, &format!(r#"{{"status": ["status_{}"]}}"#, i))
            .unwrap();
    }

    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    for _ in 0..1000 {
        let event = r#"{"status": "no_match_here"}"#;
        let _ = q.matches_for_event(event.as_bytes()).unwrap();
    }

    MemoryStats::capture("matching_1000_events_no_match")
}

/// Load citylots.jlines.gz dataset
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

/// Profile: CityLots dataset matching (comparable to Go's BenchmarkCityLots)
/// Go baseline: 968 B/op, 55 allocs/op
fn profile_citylots_matching() -> MemoryStats {
    // Same patterns as Go benchmark
    let patterns = [
        r#"{ "properties": { "STREET": [ "CRANLEIGH" ] } }"#,
        r#"{ "properties": { "STREET": [ "17TH" ], "ODD_EVEN": [ "E"] } }"#,
        r#"{ "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
        r#"{ "properties": { "MAPBLKLOT": ["0011008"], "BLKLOT": ["0011008"]},  "geometry": { "coordinates": [ 37.807807921694092 ] } }"#,
    ];
    let names = ["CRANLEIGH", "17TH Even", "Geometry", "0011008"];

    // Build matcher outside profiling
    let mut q = Quamina::new();
    for (name, pattern) in names.iter().zip(patterns.iter()) {
        q.add_pattern(*name, pattern).unwrap();
    }

    // Load events outside profiling
    let lines = load_citylots_lines();
    let num_events = 100.min(lines.len()); // Profile 100 events

    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    // Profile only the matching phase
    for line in lines.iter().take(num_events) {
        let _ = q.matches_for_event(line).unwrap();
    }

    MemoryStats::capture("citylots_matching_100_events")
}

/// Profile: Exact float matching (comparable to Go's BenchmarkNumberMatching)
/// Go baseline: 1908 B/op, 10 allocs/op
fn profile_number_matching() -> MemoryStats {
    // Use fixed seed for reproducibility (Go uses 2325)
    let mut rng = rand::rngs::StdRng::seed_from_u64(2325);

    // Generate 10 random float values for the pattern
    let targets: Vec<f64> = (0..10).map(|_| rng.gen::<f64>()).collect();

    // Build pattern with 10 exact float values
    let values: String = targets
        .iter()
        .map(|f| format!("{:.6}", f))
        .collect::<Vec<_>>()
        .join(", ");
    let pattern = format!(r#"{{"x": [{}]}}"#, values);

    // Build matcher outside profiling
    let mut q = Quamina::new();
    q.add_pattern("P", &pattern).unwrap();

    // Pre-generate events: alternating between matching (target value) and non-matching (random)
    // 50% hit rate like the Go benchmark
    let events: Vec<Vec<u8>> = (0..100)
        .map(|i| {
            if i % 2 == 0 {
                // Matching event - use one of the target values
                let val = format!("{:.6}", targets[i % 10]);
                format!(r#"{{"x": {}}}"#, val).into_bytes()
            } else {
                // Non-matching event - use a different random value
                let val = format!("{:.6}", rng.gen::<f64>() + 10.0); // +10 ensures no collision
                format!(r#"{{"x": {}}}"#, val).into_bytes()
            }
        })
        .collect();

    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    // Profile matching with 50% hit rate
    for event in &events {
        let _ = q.matches_for_event(event).unwrap();
    }

    MemoryStats::capture("number_matching_100_events")
}

/// Profile: Shellstyle pattern matching (26 patterns A* through Z*)
fn profile_shellstyle_matching() -> MemoryStats {
    // Build matcher outside profiling
    let mut q = Quamina::new();
    // Add 26 shellstyle patterns (A* through Z*)
    for letter in 'A'..='Z' {
        q.add_pattern(
            letter.to_string(),
            &format!(r#"{{"name": [{{"shellstyle": "{}*"}}]}}"#, letter),
        )
        .unwrap();
    }

    // Sample events - mix of matches and non-matches
    let events: Vec<Vec<u8>> = vec![
        r#"{"name": "ALICE"}"#.as_bytes().to_vec(),
        r#"{"name": "BELVEDERE"}"#.as_bytes().to_vec(),
        r#"{"name": "CALIFORNIA"}"#.as_bytes().to_vec(),
        r#"{"name": "DOWNTOWN"}"#.as_bytes().to_vec(),
        r#"{"name": "EMBARCADERO"}"#.as_bytes().to_vec(),
        r#"{"name": "FOLSOM"}"#.as_bytes().to_vec(),
        r#"{"name": "GEARY"}"#.as_bytes().to_vec(),
        r#"{"name": "HAIGHT"}"#.as_bytes().to_vec(),
        r#"{"name": "lowercase"}"#.as_bytes().to_vec(), // no match
        r#"{"name": "123NUMERIC"}"#.as_bytes().to_vec(), // no match
    ];

    #[cfg(feature = "dhat-heap")]
    let _profiler = dhat::Profiler::builder().testing().build();

    // Profile 100 iterations through the events
    for _ in 0..10 {
        for event in &events {
            let _ = q.matches_for_event(event).unwrap();
        }
    }

    MemoryStats::capture("shellstyle_matching_100_events")
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let json_output = args.iter().any(|a| a == "--json");

    #[cfg(not(feature = "dhat-heap"))]
    {
        eprintln!("WARNING: dhat-heap feature not enabled!");
        eprintln!("Run with: cargo bench --bench memory --features dhat-heap");
        eprintln!();
    }

    // Run all profiles
    let results = vec![
        (
            "Pattern Add: 100 simple patterns",
            profile_pattern_add_simple(),
        ),
        (
            "Pattern Add: 100 × 10 multivalue",
            profile_pattern_add_multivalue(),
        ),
        ("Pattern Add: 10 regex patterns", profile_regex_patterns()),
        (
            "Pattern Add: 100 numeric ranges",
            profile_numeric_patterns(),
        ),
        (
            "Steady State: 1000 patterns loaded",
            profile_steady_state_1000(),
        ),
        (
            "Matching: 1000 events × 100 patterns",
            profile_matching_hot_path(),
        ),
        (
            "Matching: 100 large JSON events",
            profile_matching_large_json(),
        ),
        (
            "Matching: 1000 events no match",
            profile_matching_no_match(),
        ),
        // Go parity benchmarks
        (
            "Matching: CityLots 100 events (Go: 968 B/op)",
            profile_citylots_matching(),
        ),
        (
            "Matching: Number 100 events (Go: 1908 B/op)",
            profile_number_matching(),
        ),
        (
            "Matching: Shellstyle 100 events",
            profile_shellstyle_matching(),
        ),
    ];

    if json_output {
        println!("{{");
        let json_lines: Vec<String> = results.iter().map(|(_, s)| s.to_json()).collect();
        println!("{}", json_lines.join(",\n"));
        println!("}}");
    } else {
        println!("===========================================");
        println!("  quamina-rs Memory Profile Report");
        println!("===========================================");

        for (title, stats) in &results {
            println!("\n=== {} ===", title);
            stats.print_human();
        }

        println!("\n===========================================");
        println!("  Summary (peak memory per operation)");
        println!("===========================================");
        for (_, stats) in &results {
            println!("  {:40} {:>10} bytes peak", stats.name, stats.max_bytes);
        }

        println!("\n===========================================");
        println!("Tip: Use --json for machine-readable output");
        println!("Compare with Go: go test -bench=. -benchmem");
        println!("===========================================");
    }
}
