//! Profile negated character class matching for samply analysis
//!
//! Tests the `a[^x]+x` pattern against a 1000-char string where memchr
//! acceleration should skip to the exit byte.
//!
//! Run with: samply record cargo run --release --example profile_negated

use quamina::Quamina;

fn main() {
    let mut q = Quamina::new();
    q.add_pattern("not_x", r#"{"value": [{"regex": "a[^x]+x"}]}"#)
        .unwrap();

    // 1000 'a's followed by 'x'
    let long_value = format!("{}x", "a".repeat(1000));
    let event = format!(r#"{{"value": "{long_value}"}}"#).into_bytes();

    let iterations = 1_000_000;
    let start = std::time::Instant::now();

    for _ in 0..iterations {
        let _ = std::hint::black_box(q.matches_for_event(std::hint::black_box(&event)));
    }

    let elapsed = start.elapsed();
    let ns_per_op = elapsed.as_nanos() / iterations as u128;
    eprintln!("{iterations} iterations in {elapsed:.2?} ({ns_per_op} ns/op)",);
}
