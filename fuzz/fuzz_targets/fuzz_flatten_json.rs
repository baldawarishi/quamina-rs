//! Fuzz target for the JSON flattening parser.
//!
//! This target exercises the low-level JSON parser with arbitrary byte sequences.
//! The parser must handle:
//! - Valid JSON of any structure (objects, arrays, nested, escapes, unicode)
//! - Invalid JSON (truncated, malformed, invalid UTF-8)
//! - Edge cases (deep nesting, large strings, numeric limits)
//!
//! The fuzzer provides raw bytes, which tests the parser's robustness against
//! malformed input. The parser should never panic - only return Ok or Err.

#![no_main]

use libfuzzer_sys::fuzz_target;
use quamina::flatten_json;
use quamina::segments_tree::SegmentsTree;

fuzz_target!(|data: &[u8]| {
    // Create a fresh flattener state for each input.
    // Using a fresh state ensures we test initialization paths too.
    let mut state = flatten_json::State::new();

    // Empty tree: the flattener will extract all fields it encounters.
    // This maximizes code coverage since no fields are skipped.
    let tree = SegmentsTree::new();

    // Attempt to flatten the arbitrary input.
    // We don't care about the result - only that it doesn't panic.
    // The parser should gracefully handle any byte sequence.
    let _ = state.flatten(data, &tree);
});
