# quamina-rs

A Rust port of [quamina](https://github.com/timbray/quamina) - a fast pattern-matching library for filtering JSON events.

## Overview

quamina-rs provides the same core functionality as the Go version:
- Add patterns (JSON templates) to a matcher instance
- Match events (JSON data) against stored patterns
- Return identifiers of all matching patterns

**Design principles:**
- Zero dependencies beyond `std` (like the Go version)
- Idiomatic Rust with strong typing and ownership semantics
- Similar API surface adapted to Rust conventions

## Current Status

✅ **All core pattern operators implemented** (23 tests passing)

| Feature | Status |
|---------|--------|
| Exact match | ✅ |
| Multiple values (OR) | ✅ |
| Multiple fields (AND) | ✅ |
| Exists | ✅ |
| Prefix | ✅ |
| Suffix | ✅ |
| Wildcard | ✅ |
| Anything-but | ✅ |
| Equals-ignore-case | ✅ |
| Numeric comparisons | ✅ |
| Nested objects | ✅ |
| Delete patterns | ✅ |

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
    pub fn delete_patterns(&mut self, x: &X) -> Result<(), QuaminaError>;
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
| Anything-but | `[{"anything-but": [...]}]` | `{"status": [{"anything-but": ["deleted"]}]}` |
| Equals-ignore-case | `[{"equals-ignore-case": "str"}]` | `{"name": [{"equals-ignore-case": "test"}]}` |
| Numeric | `[{"numeric": ["op", val, ...]}]` | `{"price": [{"numeric": [">=", 10, "<", 100]}]}` |
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
- wildcard (*, prefix*, *suffix, *contains*)
- anything-but
- equals-ignore-case
- numeric (>, >=, <, <=, =)
- Nested object patterns

## Future Work

### Phase 3: Optimization (not yet started)
- Automaton-based matching (like Go version)
- Performance benchmarks
- Memory optimization

### Phase 4: Full Parity (not yet started)
- Regular expression support
- Copy() for thread-safe parallel matching
- Custom flatteners
