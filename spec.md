# quamina-rs

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

✅ **All core pattern operators implemented** (98 tests passing)

| Feature | Status |
|---------|--------|
| Exact match | ✅ |
| Numeric exact match (35 = 35.0 = 3.5e1) | ✅ |
| Multiple values (OR) | ✅ |
| Multiple fields (AND) | ✅ |
| Exists | ✅ |
| Prefix | ✅ |
| Suffix | ✅ |
| Wildcard (with escaping) | ✅ |
| Shellstyle (simple wildcard) | ✅ |
| Anything-but (with validation) | ✅ |
| Equals-ignore-case | ✅ |
| Numeric comparisons | ✅ |
| Regex | ✅ |
| Nested objects | ✅ |
| Array element matching | ✅ |
| Array cross-element correctness | ✅ |
| Delete patterns | ✅ |
| Clone (thread-safe snapshots) | ✅ |
| Unicode escapes (`\uXXXX`) | ✅ |

## User-Facing API

```rust
use quamina::Quamina;

// Create a new instance
let mut q = Quamina::new();

// Add patterns with an identifier
q.add_pattern("order-pattern", r#"{"status": ["pending", "shipped"]}"#)?;
q.add_pattern("urgent", r#"{"priority": ["high"], "status": ["pending"]}"#)?;

// Match an event
let event = r#"{"status": "pending", "priority": "high", "id": 123}"#;
let matches = q.matches_for_event(event.as_bytes())?;
// matches: ["order-pattern", "urgent"]

// Delete patterns by identifier
q.delete_patterns(&"order-pattern")?;
```

## Core Types

```rust
/// The main pattern matcher
pub struct Quamina<X = String> {
    // internal state
}

impl<X: Clone + Eq + Hash> Quamina<X> {
    pub fn new() -> Self;
    pub fn add_pattern(&mut self, x: X, pattern: &str) -> Result<(), QuaminaError>;
    pub fn matches_for_event(&self, event: &[u8]) -> Result<Vec<X>, QuaminaError>;
    pub fn has_matches(&self, event: &[u8]) -> Result<bool, QuaminaError>;
    pub fn count_matches(&self, event: &[u8]) -> Result<usize, QuaminaError>;
    pub fn delete_patterns(&mut self, x: &X) -> Result<(), QuaminaError>;
    pub fn pattern_count(&self) -> usize;
    pub fn is_empty(&self) -> bool;
    pub fn clear(&mut self);
}

#[derive(Debug)]
pub enum QuaminaError {
    InvalidJson(String),
    InvalidPattern(String),
    InvalidUtf8,
}
```

## Pattern Syntax

Patterns are JSON objects where leaf values are arrays (OR semantics within array, AND across fields):

| Pattern Type | Syntax | Example |
|-------------|--------|---------|
| Exact match | `[value]` | `{"status": ["active"]}` |
| Multiple values | `[v1, v2]` | `{"status": ["pending", "shipped"]}` |
| Exists | `[{"exists": bool}]` | `{"field": [{"exists": true}]}` |
| Prefix | `[{"prefix": "str"}]` | `{"name": [{"prefix": "prod-"}]}` |
| Suffix | `[{"suffix": "str"}]` | `{"file": [{"suffix": ".txt"}]}` |
| Wildcard | `[{"wildcard": "pat"}]` | `{"id": [{"wildcard": "*-123"}]}` |
| Shellstyle | `[{"shellstyle": "pat"}]` | `{"id": [{"shellstyle": "foo*bar"}]}` |
| Anything-but | `[{"anything-but": [...]}]` | `{"status": [{"anything-but": ["deleted"]}]}` |
| Equals-ignore-case | `[{"equals-ignore-case": "str"}]` | `{"name": [{"equals-ignore-case": "test"}]}` |
| Numeric | `[{"numeric": ["op", val, ...]}]` | `{"price": [{"numeric": [">=", 10, "<", 100]}]}` |
| Regex | `[{"regex": "pattern"}]` | `{"code": [{"regex": "^[A-Z]{3}-[0-9]+$"}]}` |
| Nested fields | nested objects | `{"user": {"role": ["admin"]}}` |

## Completed Milestones

### ✅ Milestone 0.5: End-to-end smoke test
- Basic API working
- `cargo run --example smoke` passes

### ✅ Phase 1: Foundation
- Project scaffold with Cargo.toml
- QuaminaError enum
- Quamina struct with new()
- Minimal JSON parser (no deps)
- Exact match logic

### ✅ Phase 2: All Pattern Operators
- exists (true/false)
- prefix
- suffix
- wildcard (with `\*` and `\\` escaping)
- shellstyle (simple wildcard without escaping)
- anything-but
- equals-ignore-case
- numeric (>, >=, <, <=, =)
- regex (via regex crate)
- Nested object patterns
- Array element matching (pattern matches any array element)
- Unicode escape sequences (`\uXXXX`) in JSON
- Array cross-element correctness (via ArrayTrail tracking)

## Architectural Comparison: Rust vs Go

The Rust implementation takes a simpler, more direct approach while the Go version has a sophisticated automaton-based architecture.

### Key Architectural Gaps

| Aspect | Go | Rust |
|--------|----|----- |
| Matching engine | NFA/DFA automaton-based | Simple HashMap lookups |
| Pattern storage | Decorated automatons | HashMap<X, Vec<Pattern>> |
| Dependencies | Multiple internal packages | Minimal (just regex crate) |

### Go Features Not in Rust

1. **Custom Flattener** - Go allows processing non-JSON formats; Rust is JSON-only
2. **Copy API for Concurrency** - Go has sophisticated thread-safe snapshots with pruner-based pattern management

### Rust-Only Feature

**Suffix matching** (`{"suffix": "str"}`) - Rust supports this but Go doesn't.

### Pattern Type Parity

Both support: exact, numeric, prefix, wildcard, shellstyle, anything-but, equals-ignore-case, numeric comparisons (>, <, etc.), regex, exists, and nested objects.

### Bottom Line

Rust is now a correct implementation with full pattern operator parity to Go. The main architectural difference is that Rust uses a simpler HashMap-based approach with backtracking, while Go uses an automaton-based approach for better performance with many patterns.

## Future Work

### ✅ Phase 3: Thread Safety
- Clone for creating snapshots
- Send + Sync for thread safety

### Phase 4: Optimization (in progress)
- ✅ Performance benchmarks (criterion)
- ✅ Array cross-element correctness (via ArrayTrail, independent of automaton)
- ✅ Single-field pattern fast path (skip backtracking for common case)
- ✅ Push/pop optimization for trail tracking (avoid allocations)
- ✅ Field-path indexing with adaptive heuristic (70-82% improvement for diverse patterns)
- 🔄 Automaton-based matching (like Go version) - foundation complete, integration pending
  - ✅ SmallTable (byte-indexed transition table with ceilings/steps)
  - ✅ FaState (automaton state with table and field transitions)
  - ✅ FA builders: make_string_fa, make_prefix_fa, make_shellstyle_fa, make_wildcard_fa
  - ✅ DFA/NFA traversal with epsilon closure and spinout handling
  - ✅ merge_fas for combining multiple automata
  - ✅ AutomatonValueMatcher for single-field value matching
  - ✅ Multi-field CoreMatcher with add_pattern and matches_for_fields
    - MutableFieldMatcher with transitions, matches, exists_true/false maps
    - MutableValueMatcher with singleton optimization and automaton
    - Pattern addition builds graph of FieldMatcher -> ValueMatcher -> FieldMatcher
    - Matching traverses graph recursively with array trail conflict checking
    - Tests passing: single-field, multi-field AND, OR within field, exists patterns
  - ⏳ Integration with main Quamina struct (replace HashMap-based matching)
- Memory optimization

### Phase 5: Future Enhancements (not yet started)
- Custom flatteners
