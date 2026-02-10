//! Fuzz target for the add/delete/match/rebuild lifecycle.
//!
//! Unlike fuzz_add_pattern (parser only) and fuzz_match_event (static patterns),
//! this target exercises the stateful API surface:
//! - Arbitrary pattern JSON fed into add_pattern (parser + automaton builder)
//! - Deletion of arbitrary IDs, including ones never added
//! - Arbitrary event bytes fed into matches_for_event (flattener + matcher)
//! - Seed pattern/event pairs so the "match found" code path is exercised
//! - Rebuild and clear operations
//!
//! Uses u16 pattern IDs to keep the ID space compact (2 bytes) while still
//! allowing deletes of never-added IDs.

#![no_main]

use libfuzzer_sys::fuzz_target;
use quamina::Quamina;

/// Known-valid (pattern, matching_event) pairs.
/// When a seed pattern is active, matching its event exercises the match-found
/// and deleted_patterns filter paths -- code that pure-random events rarely reach.
const SEEDS: &[(&str, &[u8])] = &[
    (
        r#"{"status": ["active"]}"#,
        br#"{"status": "active"}"#,
    ),
    (
        r#"{"env": [{"prefix": "prod-"}]}"#,
        br#"{"env": "prod-east"}"#,
    ),
    (
        r#"{"name": [{"exists": true}]}"#,
        br#"{"name": "x"}"#,
    ),
    (
        r#"{"price": [{"numeric": ["<", 100]}]}"#,
        br#"{"price": 42}"#,
    ),
    (
        r#"{"path": [{"wildcard": "*/src/*.rs"}]}"#,
        br#"{"path": "a/src/b.rs"}"#,
    ),
    (
        r#"{"code": [{"regex": "^[A-Z]{3}$"}]}"#,
        br#"{"code": "ABC"}"#,
    ),
];

/// Seed pattern IDs live in a range that won't collide with sequential IDs.
const SEED_BASE: u16 = 0xF000;

/// Consume `n` bytes from `data` at `*pos`, advancing the cursor.
/// Returns None (and leaves pos unchanged) if not enough bytes remain.
fn take<'a>(data: &'a [u8], pos: &mut usize, n: usize) -> Option<&'a [u8]> {
    if *pos + n > data.len() {
        return None;
    }
    let slice = &data[*pos..*pos + n];
    *pos += n;
    Some(slice)
}

fuzz_target!(|data: &[u8]| {
    let mut q = Quamina::<u16>::new();
    let mut pos = 0;
    let mut next_id: u16 = 0;

    while pos < data.len() {
        let op = data[pos] % 7;
        pos += 1;

        match op {
            // Add: fuzzer-provided pattern bytes (like fuzz_add_pattern)
            // Exercises the parser and automaton builder with arbitrary input.
            0 => {
                let len = match take(data, &mut pos, 1) {
                    Some(b) => b[0] as usize,
                    None => break,
                };
                let pat = match take(data, &mut pos, len) {
                    Some(b) => b,
                    None => break,
                };
                let s = String::from_utf8_lossy(pat);
                let _ = q.add_pattern(next_id, &s);
                next_id = next_id.wrapping_add(1);
            }

            // Add: seed pattern (ensures automaton has real states to traverse)
            1 => {
                let idx = match take(data, &mut pos, 1) {
                    Some(b) => b[0] as usize % SEEDS.len(),
                    None => break,
                };
                let _ = q.add_pattern(SEED_BASE + idx as u16, SEEDS[idx].0);
            }

            // Delete: arbitrary u16 ID -- may target a pattern that was never added
            2 => {
                let id_bytes = match take(data, &mut pos, 2) {
                    Some(b) => b,
                    None => break,
                };
                let id = u16::from_le_bytes([id_bytes[0], id_bytes[1]]);
                let _ = q.delete_patterns(&id);
            }

            // Match: fuzzer-provided event bytes (like fuzz_match_event)
            // Exercises the JSON flattener and matching engine with arbitrary input.
            3 => {
                let len = match take(data, &mut pos, 1) {
                    Some(b) => b[0] as usize,
                    None => break,
                };
                let event = match take(data, &mut pos, len) {
                    Some(b) => b,
                    None => break,
                };
                let _ = q.matches_for_event(event);
            }

            // Match: seed event (exercises the match-found + filter paths)
            4 => {
                let idx = match take(data, &mut pos, 1) {
                    Some(b) => b[0] as usize % SEEDS.len(),
                    None => break,
                };
                let _ = q.matches_for_event(SEEDS[idx].1);
            }

            // Rebuild: compact automaton after deletions
            5 => {
                q.rebuild();
            }

            // Clear: reset all state
            _ => {
                q.clear();
            }
        }
    }
});
