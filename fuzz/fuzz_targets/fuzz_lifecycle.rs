//! Fuzz target for the add/delete/match/rebuild lifecycle.
//!
//! This target exercises state transitions that the other fuzz targets miss:
//! - Adding patterns then deleting some before matching
//! - The deleted_patterns filter path in matches_for_event
//! - Rebuild (compacting the automaton after deletions)
//! - Re-adding a previously deleted pattern
//!
//! The fuzzer provides structured input: a sequence of operations on a Quamina
//! instance, ensuring the full lifecycle is explored.

#![no_main]

use libfuzzer_sys::fuzz_target;

/// Fixed set of patterns the fuzzer can add/remove by index.
/// Covers diverse matcher types so deletions interact with varied automaton states.
const PATTERNS: &[(&str, &str)] = &[
    ("exact", r#"{"status": ["active"]}"#),
    ("prefix", r#"{"env": [{"prefix": "prod-"}]}"#),
    ("exists", r#"{"name": [{"exists": true}]}"#),
    ("numeric", r#"{"price": [{"numeric": ["<", 100]}]}"#),
    ("wildcard", r#"{"path": [{"wildcard": "*/src/*.rs"}]}"#),
    ("suffix", r#"{"file": [{"suffix": ".json"}]}"#),
    ("nested", r#"{"user": {"role": ["admin"]}}"#),
    ("multi", r#"{"status": ["active"], "priority": ["high"]}"#),
];

/// Events to match against (the fuzzer picks by index).
const EVENTS: &[&[u8]] = &[
    br#"{"status": "active", "priority": "high"}"#,
    br#"{"env": "prod-us-east-1", "name": "svc"}"#,
    br#"{"price": 42, "file": "data.json"}"#,
    br#"{"path": "repo/src/main.rs", "user": {"role": "admin"}}"#,
    br#"{"status": "deleted"}"#,
    br#"{}"#,
];

fuzz_target!(|data: &[u8]| {
    let mut q = quamina::Quamina::<String>::new();

    // Interpret each byte as an operation:
    //   0x00..0x3F  → add pattern [byte % PATTERNS.len()]
    //   0x40..0x7F  → delete pattern [byte % PATTERNS.len()]
    //   0x80..0xBF  → match event [byte % EVENTS.len()]
    //   0xC0..0xFF  → rebuild
    for &b in data {
        match b {
            0x00..=0x3F => {
                let idx = (b as usize) % PATTERNS.len();
                let (id, pat) = PATTERNS[idx];
                let _ = q.add_pattern(id.to_string(), pat);
            }
            0x40..=0x7F => {
                let idx = ((b - 0x40) as usize) % PATTERNS.len();
                let (id, _) = PATTERNS[idx];
                let _ = q.delete_patterns(&id.to_string());
            }
            0x80..=0xBF => {
                let idx = ((b - 0x80) as usize) % EVENTS.len();
                let _ = q.matches_for_event(EVENTS[idx]);
            }
            _ => {
                q.rebuild();
            }
        }
    }

    // Final match to exercise whatever state we ended up in
    for event in EVENTS {
        let _ = q.matches_for_event(event);
    }
});
