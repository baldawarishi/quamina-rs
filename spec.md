# quamina-rs

<!-- Checkpoint: Unicode case folding done. Full Unicode support for equals-ignore-case via case_folding.rs. -->

A Rust port of [quamina](https://github.com/timbray/quamina) - a fast pattern-matching library for filtering JSON events.

## Overview

quamina-rs provides the same core functionality as the Go version:
- Add patterns (JSON templates) to a matcher instance
- Match events (JSON data) against stored patterns
- Return identifiers of all matching patterns

**Design principles:**
- Minimal dependencies (only `regex` crate for regex support)
- Idiomatic Rust with strong typing and ownership semantics
- Similar API surface adapted to Rust conventions

## Current Status

All core pattern operators implemented (142 tests passing).

| Feature | Status | Path |
|---------|--------|------|
| Exact match | ✅ | automaton |
| Prefix | ✅ | automaton |
| Shellstyle | ✅ | automaton |
| Wildcard (with escaping) | ✅ | automaton |
| Anything-but | ✅ | automaton |
| Exists | ✅ | automaton |
| Suffix | ✅ | fallback |
| Equals-ignore-case | ✅ | automaton (Unicode) |
| Numeric exact | ✅ | automaton (Q-numbers) |
| Numeric comparisons | ✅ | fallback |
| Regex | ✅ | fallback (`regex` crate) |
| Multiple values (OR) | ✅ | both |
| Multiple fields (AND) | ✅ | both |
| Nested objects | ✅ | both |
| Array element matching | ✅ | both |
| Delete patterns | ✅ | HashSet filtering |
| Clone (thread-safe) | ✅ | Send + Sync |

## API Reference

```rust
use quamina::Quamina;

let mut q = Quamina::new();
q.add_pattern("order", r#"{"status": ["pending", "shipped"]}"#)?;
q.add_pattern("urgent", r#"{"priority": ["high"], "status": ["pending"]}"#)?;

let event = r#"{"status": "pending", "priority": "high", "id": 123}"#;
let matches = q.matches_for_event(event.as_bytes())?;
// matches: ["order", "urgent"]

q.delete_patterns(&"order")?;
```

```rust
impl<X: Clone + Eq + Hash + Send + Sync> Quamina<X> {
    pub fn new() -> Self;
    pub fn add_pattern(&mut self, x: X, pattern: &str) -> Result<(), QuaminaError>;
    pub fn matches_for_event(&self, event: &[u8]) -> Result<Vec<X>, QuaminaError>;
    pub fn delete_patterns(&mut self, x: &X) -> Result<(), QuaminaError>;
    // Also: has_matches, count_matches, pattern_count, is_empty, clear
}
```

## Pattern Syntax

Patterns are JSON objects. Leaf values are arrays (OR within array, AND across fields):

| Type | Syntax | Example |
|------|--------|---------|
| Exact | `[value]` | `{"status": ["active"]}` |
| Multiple | `[v1, v2]` | `{"status": ["pending", "shipped"]}` |
| Exists | `[{"exists": bool}]` | `{"field": [{"exists": true}]}` |
| Prefix | `[{"prefix": "s"}]` | `{"name": [{"prefix": "prod-"}]}` |
| Suffix | `[{"suffix": "s"}]` | `{"file": [{"suffix": ".txt"}]}` |
| Wildcard | `[{"wildcard": "p"}]` | `{"id": [{"wildcard": "*-123"}]}` |
| Shellstyle | `[{"shellstyle": "p"}]` | `{"id": [{"shellstyle": "foo*"}]}` |
| Anything-but | `[{"anything-but": [...]}]` | `{"status": [{"anything-but": ["deleted"]}]}` |
| Equals-ignore-case | `[{"equals-ignore-case": "s"}]` | `{"name": [{"equals-ignore-case": "test"}]}` |
| Numeric | `[{"numeric": [...]}]` | `{"price": [{"numeric": [">=", 10, "<", 100]}]}` |
| Regex | `[{"regex": "pat"}]` | `{"code": [{"regex": "^[A-Z]+$"}]}` |

Note: Suffix is Rust-only (Go doesn't support it).

## Parity Tracking

### Functional Parity

| Feature | Go | Rust | Notes |
|---------|:--:|:----:|-------|
| Automaton core | ✅ | ✅ | SmallTable, NFA/DFA, FieldMatcher |
| Numeric Q-numbers | ✅ | ✅ | `numbits.rs` - IEEE 754 to ordered bytes |
| Segment-based flattener | ✅ | ✅ | `segments_tree.rs` + `flatten_json.rs` |
| Unicode case folding | ✅ | ✅ | `case_folding.rs` + automaton monocase FA |
| Custom regex NFA | ✅ | ❌ | Using `regex` crate instead |
| Pruner rebuilding | ✅ | ❌ | Using HashSet deletion |
| Custom flatteners | ✅ | ❌ | JSON-only in Rust |
| Stats/debug output | ✅ | ❌ | `stats.go`, `prettyprinter.go` |

### Benchmark Parity

| Go Benchmark | Rust Equivalent | Status |
|--------------|-----------------|--------|
| `Benchmark_JsonFlattner_Evaluate_ContextFields` | `bench_status_context_fields` | ✅ |
| `Benchmark_JsonFlattner_Evaluate_MiddleNestedField` | `bench_status_middle_nested` | ✅ |
| `Benchmark_JsonFlattner_Evaluate_LastField` | `bench_status_last_field` | ✅ |
| `BenchmarkCityLots` | `bench_citylots` | ✅ |
| `BenchmarkNumberMatching` | - | ❌ Not started |
| `TestBigShellStyle` (26 patterns) | `bench_shellstyle_alphabet` | ✅ |

### Performance Baseline

Run with: `cargo bench` (Rust) and `go test -run=NONE -bench=. -benchmem` (Go)

| Benchmark | Go (ns/op) | Rust (ns/op) | Notes |
|-----------|------------|--------------|-------|
| status_context_fields | 404 | 566 | Go 1.40x faster (early field) |
| status_middle_nested | 6,745 | 4,972 | Rust 1.36x faster! |
| status_last_field | 7,217 | 5,213 | Rust 1.38x faster! |
| citylots | 3,812 | 4,937 | Go 1.29x faster |
| shellstyle_26 | - | 1,394 | - |
| anything_but_match | - | 227 | - |
| multi_field_and_3 | - | 395 | - |

**Analysis**: Zero-copy field matching with borrowed bytes has been implemented:
- ✅ Streaming JSON parser with field skipping (SegmentsTree)
- ✅ Early termination when all needed fields found
- ✅ Reusable flattener state with Go's reset() pattern
- ✅ Whitespace lookup table for O(1) checks
- ✅ Zero-copy `EventFieldRef<'a>` using borrowed bytes (eliminates String allocations)
- ✅ Path separator aligned with Go (uses `\n` throughout, no conversion needed)
- ✅ Reusable NfaBuffers stored in Quamina struct (like Go's `q.bufs`)
- ✅ Cow<[u8]> in transition_on to avoid value.to_vec() allocations
- Rust now faster than Go on middle/last field benchmarks!
- Citylots gap reduced from 1.5x to 1.29x

## Next Steps

1. ~~**Benchmark baseline**~~ - ✅ Done. See Performance Baseline table above.
2. ~~**Port numbits.go**~~ - ✅ Done. `numbits.rs` with IEEE 754 to ordered bytes.
3. ~~**Integrate Q-numbers**~~ - ✅ Done. NumericExact patterns use automaton-based Q-number matching.
4. ~~**JSON parsing optimization**~~ - ✅ Done. `segments_tree.rs` + `flatten_json.rs` with 10-12x speedup.
5. ~~**CityLots benchmark**~~ - ✅ Done. Go ~1.45x faster on large GeoJSON documents.
6. ~~**Reduce allocation overhead**~~ - ✅ Done. 29% improvement on status benchmarks:
   - ✅ Reusable `FlattenJsonState` with Go's reset() pattern
   - ✅ Pre-allocated and reused array_trail vector
   - ✅ `[u8; 256]` lookup table for whitespace
   - Rust now faster than Go on status_middle_nested and status_last_field!
7. ~~**Unicode case folding**~~ - ✅ Done. Full Unicode support via `case_folding.rs`:
   - ✅ Generated 2876 Unicode case folding pairs from Go's `case_folding.go`
   - ✅ Monocase automaton handles multi-byte UTF-8 with common prefix optimization
   - ✅ EqualsIgnoreCase now automaton-compatible (was fallback-only)
8. ~~**Evaluate regex approach**~~ - ✅ Decision: Keep `regex` crate
   - Go's custom NFA is ~1000+ lines; porting adds complexity with marginal gains
   - `regex` crate is battle-tested, well-maintained, and optimized
   - Regex patterns are typically a small fraction of total patterns
9. ~~**Eliminate string conversions**~~ - ✅ Done. Citylots gap reduced from 1.5x to 1.25x:
   - ✅ Path separator unified to `\n` (no `.replace()` conversion needed)
   - ✅ Zero-copy `EventFieldRef<'a>` with borrowed `&str` path and `&[u8]` value
   - ✅ Automaton `matches_for_fields_ref()` uses borrowed fields directly
   - ✅ Skip intermediate `json::Field` format for automaton matching
   - Fallback patterns still use legacy `Field` format (lazy conversion only when needed)

## Test Data

| File | Status |
|------|--------|
| `testdata/status.json` | ✅ Present |
| `testdata/citylots.jlines.gz` | ✅ Present (206k GeoJSON lines) |
| `testdata/wwords.txt` | ✅ Present |
