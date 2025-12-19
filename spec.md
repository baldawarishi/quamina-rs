# quamina-rs

A Rust port of [quamina](https://github.com/timbray/quamina) - a fast pattern-matching library for filtering JSON events.

## Overview

quamina-rs will provide the same core functionality as the Go version:
- Add patterns (JSON templates) to a matcher instance
- Match events (JSON data) against stored patterns
- Return identifiers of all matching patterns

**Design principles:**
- Zero dependencies beyond `std` (like the Go version)
- Idiomatic Rust with strong typing and ownership semantics
- Similar API surface adapted to Rust conventions

## User-Facing API

```rust
use quamina::Quamina;

// Create a new instance
let mut q = Quamina::new();

// Add patterns with an identifier (any type implementing the X trait)
q.add_pattern("order-pattern", r#"{"status": ["pending", "shipped"]}"#)?;
q.add_pattern("urgent", r#"{"priority": ["high"], "status": ["pending"]}"#)?;

// Match an event
let event = r#"{"status": "pending", "priority": "high", "id": 123}"#;
let matches = q.matches_for_event(event.as_bytes())?;
// matches: ["order-pattern", "urgent"]

// Delete patterns by identifier
q.delete_patterns("order-pattern")?;
```

## Core Types

```rust
/// The main pattern matcher
pub struct Quamina<X = String> {
    // internal state
}

impl<X: Clone + Eq + Hash> Quamina<X> {
    /// Create a new Quamina instance
    pub fn new() -> Self;

    /// Add a pattern with the given identifier
    pub fn add_pattern(&mut self, x: X, pattern: &str) -> Result<(), QuaminaError>;

    /// Find all patterns that match the given event
    pub fn matches_for_event(&self, event: &[u8]) -> Result<Vec<X>, QuaminaError>;

    /// Delete all patterns with the given identifier
    pub fn delete_patterns(&mut self, x: &X) -> Result<(), QuaminaError>;
}

/// Errors that can occur during pattern matching
#[derive(Debug)]
pub enum QuaminaError {
    InvalidJson(String),
    InvalidPattern(String),
    InvalidUtf8,
}
```

## Pattern Syntax (matching Go version)

Patterns are JSON objects where leaf values are arrays (OR semantics within array, AND across fields):

| Pattern Type | Syntax | Example |
|-------------|--------|---------|
| Exact match | `[value]` | `{"status": ["active"]}` |
| Multiple values | `[v1, v2]` | `{"status": ["pending", "shipped"]}` |
| Exists | `[{"exists": bool}]` | `{"field": [{"exists": true}]}` |
| Prefix | `[{"prefix": "str"}]` | `{"name": [{"prefix": "prod-"}]}` |
| Wildcard | `[{"wildcard": "pat"}]` | `{"id": [{"wildcard": "*-123"}]}` |
| Anything-but | `[{"anything-but": [...]}]` | `{"status": [{"anything-but": ["deleted"]}]}` |
| Equals-ignore-case | `[{"equals-ignore-case": "str"}]` | `{"name": [{"equals-ignore-case": "test"}]}` |

## Implementation Roadmap

### Milestone 0.5: End-to-end smoke test (first priority)

Implement the smallest end-to-end slice so the public API is real early:

```rust
use quamina::Quamina;

let mut q = Quamina::new();
q.add_pattern("test", r#"{"status": ["active"]}"#).unwrap();

let matches = q.matches_for_event(r#"{"status": "active", "id": 1}"#.as_bytes()).unwrap();
assert_eq!(matches, vec!["test"]);

let no_matches = q.matches_for_event(r#"{"status": "inactive"}"#.as_bytes()).unwrap();
assert!(no_matches.is_empty());
```

- Add `examples/smoke.rs` that runs the above and asserts expected results
- **Gate:** `cargo run --example smoke` passes

This milestone only needs exact string matching - no wildcards, prefixes, or other operators yet.

### Phase 1: Foundation (to achieve Milestone 0.5)

**Step 1: Project scaffold** (~30 lines)
- Initialize Cargo project with lib structure
- Define `QuaminaError` enum
- Empty `Quamina` struct with `new()`

**Step 2: Minimal JSON parser** (~100 lines)
- Parse JSON events into field paths and values
- Parse JSON patterns into field paths and expected values
- Only handle strings, numbers, booleans, null, objects, arrays

**Step 3: Exact match logic** (~70 lines)
- Store patterns as HashMap of field→values
- Match events by checking all pattern fields exist with matching values
- Implement `add_pattern` and `matches_for_event`

**Step 4: Smoke test example** (~20 lines)
- Create `examples/smoke.rs` with the test case above
- Verify it passes

### Phase 2: Core Matching
*Details to be filled in after Phase 1 completion*

### Phase 3: Pattern Operators
*Details to be filled in after Phase 2 completion*

### Phase 4: Optimization & Parity
*Details to be filled in after Phase 3 completion*

---

## Next Action

Implement **Step 1: Project scaffold** - create the basic Cargo project structure with error types and empty Quamina struct.
