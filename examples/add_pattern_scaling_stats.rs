use std::env;
use std::time::Instant;

use quamina::Quamina;

fn exact_pattern(i: usize) -> String {
    format!(r#"{{"field": ["value_{i:06}"]}}"#)
}

fn prefix_pattern(i: usize) -> String {
    format!(r#"{{"field": [{{"prefix": "value_{i:06}"}}]}}"#)
}

fn run_scenario(name: &str, count: usize, pattern: fn(usize) -> String, event: Vec<u8>) {
    let mut q = Quamina::<usize>::new();

    let add_start = Instant::now();
    for i in 0..count {
        q.add_pattern(i, &pattern(i)).unwrap();
    }
    let add_ms = add_start.elapsed().as_secs_f64() * 1_000.0;
    let build_stats = q.matcher_stats();

    let first_match_start = Instant::now();
    let matches = q.matches_for_event(&event).unwrap();
    let first_match_ms = first_match_start.elapsed().as_secs_f64() * 1_000.0;
    let frozen_stats = q.arena_stats();

    println!(
        "{name},{count},{add_ms:.3},{first_match_ms:.3},{},{},{},{},{}",
        build_stats.states,
        build_stats.bytes,
        frozen_stats.state_count,
        frozen_stats.estimated_bytes,
        matches.len()
    );
}

fn main() {
    let count = env::args().nth(1).map_or(8_192, |arg| {
        arg.parse().expect("count must be a positive integer")
    });

    println!(
        "scenario,count,add_ms,first_match_ms,build_states,build_bytes,frozen_states,frozen_bytes,matches"
    );
    run_scenario(
        "prefix_same_field",
        count,
        prefix_pattern,
        format!(r#"{{"field": "value_{:06}_tail"}}"#, count - 1).into_bytes(),
    );
    run_scenario(
        "exact_same_field",
        count,
        exact_pattern,
        format!(r#"{{"field": "value_{:06}"}}"#, count - 1).into_bytes(),
    );
}
