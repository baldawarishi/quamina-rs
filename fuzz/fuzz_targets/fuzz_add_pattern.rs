//! Fuzz target for pattern parsing via add_pattern.
//!
//! This target exercises the pattern parser with arbitrary strings.
//! Patterns are JSON objects where field values specify matchers:
//!
//! Valid pattern examples:
//!   {"status": ["active"]}                    - Exact string match
//!   {"priority": [{"exists": true}]}          - Existence check
//!   {"name": [{"prefix": "test-"}]}           - Prefix matcher
//!   {"code": [{"regex": "[A-Z]+"}]}           - Regular expression
//!   {"price": [{"numeric": ["<", 100]}]}      - Numeric comparison
//!   {"ip": [{"cidr": "10.0.0.0/8"}]}          - CIDR matching
//!
//! The parser must handle:
//! - Valid patterns with all supported operators
//! - Invalid JSON syntax
//! - Valid JSON but invalid pattern structure
//! - Invalid operator arguments (bad regex, invalid CIDR, etc.)
//! - Deep nesting, large arrays, unicode field names
//!
//! The parser should never panic - only return Ok or Err.

#![no_main]

use libfuzzer_sys::fuzz_target;
use quamina::Quamina;

fuzz_target!(|data: &[u8]| {
    // Pattern input is expected to be a string, not arbitrary bytes.
    // Convert to UTF-8, lossy conversion handles invalid sequences.
    let pattern_str = String::from_utf8_lossy(data);

    // Create a fresh Quamina instance for each input.
    // This ensures we test the full parsing path without accumulated state.
    let mut q: Quamina<String> = Quamina::new();

    // Attempt to add the pattern.
    // We use a fixed ID since we only care about parsing, not matching.
    // The parser should gracefully handle any string input.
    let _ = q.add_pattern("fuzz".to_string(), &pattern_str);
});
