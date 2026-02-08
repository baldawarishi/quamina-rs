//! Profile status_context_fields matching for flamegraph analysis
//!
//! Run with:
//!   cargo flamegraph --example profile_status
//!   cargo build --example profile_status --release && samply record ./target/release/examples/profile_status

use quamina::Quamina;

fn main() {
    let event =
        std::fs::read("testdata/status.json").expect("Failed to read testdata/status.json");
    let pattern = r#"{ "context": { "user_id": [9034], "friends_count": [158] } }"#;

    let mut q = Quamina::new();
    q.add_pattern("context", pattern).unwrap();

    // Verify
    let matches = q.matches_for_event(&event).unwrap();
    assert_eq!(matches.len(), 1);

    let iterations = 1_000_000;
    let start = std::time::Instant::now();
    for _ in 0..iterations {
        let _ = q.matches_for_event(&event);
    }
    let elapsed = start.elapsed();
    eprintln!(
        "{} iterations in {:?} ({} ns/op)",
        iterations,
        elapsed,
        elapsed.as_nanos() / iterations as u128
    );
}
