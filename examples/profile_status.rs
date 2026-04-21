//! Profile flattener vs automaton cost on a real-world 9KB JSON document.
//!
//! Run with: samply record cargo run --release --example profile_status
//!
//! This example isolates three phases so samply can show their relative cost:
//!   - `run_flatten_only`: pure JSON parsing/flattening (no automaton)
//!   - `run_match_only`:   pure automaton matching (pre-flattened data)
//!   - `run_full`:         full pipeline (flatten + match, as in production)
//!
//! The status.json is ~9KB with realistic nested JSON, giving a meaningful
//! flattener workload (field skipping, string scanning, structural char detection).

use quamina::Quamina;
use quamina::automaton::{EventField, ThreadSafeCoreMatcher};
use quamina::flatten_json::FlattenJsonState;
use quamina::json::ArrayPos;
use quamina::segments_tree::SegmentsTree;
use std::hint::black_box;

const PATTERN_CONTEXT: &str = r#"{ "context": { "user_id": [9034], "friends_count": [158] } }"#;
const PATTERN_MIDDLE: &str = r#"{ "payload": { "user": { "id_str": ["903487807"] } } }"#;
const PATTERN_LAST: &str = r#"{ "payload": { "lang_value": ["ja"] } }"#;

fn load_event() -> Vec<u8> {
    std::fs::read("testdata/status.json").expect("testdata/status.json not found")
}

/// Pure flattener: parses all three fields from 9KB JSON, no automaton.
#[inline(never)]
fn run_flatten_only(
    flattener: &mut FlattenJsonState,
    event: &[u8],
    tree: &SegmentsTree,
    iters: u64,
) {
    for _ in 0..iters {
        let fields = flattener.flatten(black_box(event), tree).unwrap();
        black_box(fields.len());
    }
}

/// Pure automaton: matches pre-flattened fields, no JSON parsing.
#[inline(never)]
fn run_match_only(
    automaton: &ThreadSafeCoreMatcher<&'static str>,
    fields: &[EventField],
    iters: u64,
) {
    for _ in 0..iters {
        black_box(automaton.matches_for_fields(black_box(fields)));
    }
}

/// Full pipeline: flatten + match, as used in production.
#[inline(never)]
fn run_full(q: &Quamina<&'static str>, event: &[u8], iters: u64) {
    for _ in 0..iters {
        black_box(q.matches_for_event(black_box(event)).unwrap());
    }
}

fn main() {
    let event = load_event();

    let mut q = Quamina::new();
    q.add_pattern("context", PATTERN_CONTEXT).unwrap();
    q.add_pattern("middle", PATTERN_MIDDLE).unwrap();
    q.add_pattern("last", PATTERN_LAST).unwrap();

    let automaton = q.automaton();
    let segments_tree = q.segments_tree();

    // Pre-flatten for match_only phase
    let mut flattener = FlattenJsonState::new();
    let fields = flattener.flatten(&event, segments_tree).unwrap();
    fields.sort_unstable_by(|a, b| a.path.cmp(&b.path));
    let owned_fields: Vec<EventField> = fields
        .iter()
        .map(|f| EventField {
            path: f.path_str().to_string(),
            value: String::from_utf8_lossy(f.value_bytes()).to_string(),
            array_trail: f
                .array_trail_slice()
                .iter()
                .map(|ap| ArrayPos {
                    array: ap.array,
                    pos: ap.pos,
                })
                .collect(),
            is_number: f.is_number,
        })
        .collect();

    let matches = automaton.matches_for_fields(&owned_fields);
    assert_eq!(matches.len(), 3, "all 3 patterns should match");

    // Warm up
    run_full(&q, &event, 10_000);

    let iters = 2_000_000u64;
    let t0 = std::time::Instant::now();
    run_flatten_only(&mut flattener, &event, segments_tree, iters);
    let flatten_time = t0.elapsed();

    let t0 = std::time::Instant::now();
    run_match_only(automaton, &owned_fields, iters);
    let match_time = t0.elapsed();

    let t0 = std::time::Instant::now();
    run_full(&q, &event, iters);
    let full_time = t0.elapsed();

    let ns = |d: std::time::Duration| d.as_nanos() / iters as u128;
    eprintln!("flatten_only : {:>6} ns/op", ns(flatten_time));
    eprintln!("match_only   : {:>6} ns/op", ns(match_time));
    eprintln!("full_pipeline: {:>6} ns/op", ns(full_time));
    eprintln!(
        "flattener fraction of full: {:.0}%",
        ns(flatten_time) as f64 / ns(full_time) as f64 * 100.0
    );
}
