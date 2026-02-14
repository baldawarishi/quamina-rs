# Rust Idioms & Code Quality Spec

This is the living guide for writing idiomatic, maintainable Rust in quamina-rs.
It covers API design, error handling, testing, performance, module organization,
and duplicate-code reduction. Work is done incrementally across sessions.

## Session Continuation Prompt

```
Continue the Rust idioms cleanup tracked in docs/RUST_IDIOMS_SPEC.md

1. Pick the next item from "Pending Work" and apply the relevant guidelines.
2. Verify: cargo check && cargo test && cargo clippy && cargo bench --bench matching -- --quick
3. Update the "Progress Tracker" section.
```

---

## 1. Guiding Principles

1. **Minimum necessary visibility** - Default to private. Escalate only when needed.
   See [Effective Rust Item 22](https://effective-rust.com/visibility.html).
2. **Preserve quamina-go semantics** - This crate is a port. Keep behavioral
   compatibility with the Go implementation; depart only for clear Rust wins.
3. **No performance/correctness regressions** - Every change is validated by
   `cargo test`, `cargo clippy`, and `cargo bench`.
4. **Incremental changes** - One file or module at a time. Commit after each
   verifiable step.
5. **Simplicity over cleverness** - Prefer straightforward code. Three similar
   lines are better than a premature abstraction.
6. **Let the type system work** - Encode invariants in types where practical
   (newtype wrappers, enums, builder pattern).

---

## 2. Visibility & Module Organization

### Visibility Decision Tree

```text
Is it part of the public API (used by downstream crates)?
  YES -> pub
  NO  -> Is it used by other modules within this crate?
           YES -> pub(crate)
           NO  -> Is it used only by the parent module?
                    YES -> pub(super)
                    NO  -> private (no pub keyword)

Is it only used in tests?
  YES -> Move into a #[cfg(test)] block or make it private.
```

### Module Layout Guidelines

| Principle | Rationale |
|-----------|-----------|
| One concept per module | Keeps files focused and navigable. |
| Re-export public API from `lib.rs` | Downstream users write `use quamina::Quamina`, not deep paths. |
| `#[doc(hidden)] pub mod` only for benchmarks | Avoids leaking internals while letting criterion access them. |
| Test modules live in the file they test, or as `mod tests_*` siblings | Keeps test context close to implementation. |

### Current Module Map

```text
src/
  lib.rs               # Public API: Quamina, QuaminaBuilder, QuaminaError, PatternLimits
  automaton/            # NFA/DFA arena, field matching, thread-safe wrapper
    mod.rs
    arena.rs            # ArenaSmallTable, StateArena, AutomatonValueMatcher
    mutable_matcher.rs  # CoreMatcher (mutable, single-threaded builder)
    thread_safe.rs      # ThreadSafeCoreMatcher (ArcSwap-based concurrent reader)
    small_table.rs      # BYTE_CEILING, SmallTable constants
    sparse_set.rs       # SparseSet for NFA state dedup
    tests.rs            # Automaton-level unit tests
  json.rs               # Pattern parsing, Matcher enum, MultiConditionPattern
  flatten_json.rs       # Streaming JSON flattener (FlattenJsonState, Field)
  flattener.rs          # Flattener trait, JsonFlattener, OwnedField
  regexp/               # I-Regexp (RFC 9485) parser + NFA builder
    mod.rs
    nfa.rs
    parser.rs
  numbits.rs            # Base-128 float encoding for numeric comparison
  segments_tree.rs      # Field-path pruning tree
  case_folding.rs       # Unicode case-fold table (equals-ignore-case)
  unicode_categories.rs # Unicode general categories (regex \p{} support)
  regexp_samples.rs     # Test-only regexp sample data (#[cfg(test)])
  kani_proofs.rs        # Formal verification proofs (#[cfg(kani)])
  tests_core.rs         # Core matching tests (2191 lines)
  tests_operators.rs    # Operator tests (3089 lines)
  tests_stress.rs       # Stress/concurrency/unicode tests (1327 lines)
```

---

## 3. Error Handling

### Current Approach (keep)

`QuaminaError` is a hand-written enum with a manual `Display` impl and an
`impl std::error::Error`. This is fine for a library with a small, stable error
surface.

### Guidelines

| Do | Don't |
|----|-------|
| Return `Result<T, QuaminaError>` from all public APIs that can fail. | Use `unwrap()`/`expect()` in library code (only in tests and infallible paths). |
| Use `?` for propagation inside the crate. | Add `anyhow` or `thiserror` — the error type is simple enough to hand-write. |
| Keep error messages lowercase, no trailing period (Rust convention). | Expose internal details (arena addresses, indices) in user-facing messages. |
| Use `QuaminaError::PatternTooComplex` for all limit violations. | Panic on user input (reserve `panic!`/`assert!` for logic bugs and `#[cfg(test)]`). |

### Error Message Format

```text
"invalid pattern: {reason}"
"pattern too complex: depth {actual} exceeds limit {max}"
"pattern too complex: {actual} fields exceeds limit {max}"
```

---

## 4. Testing Best Practices

### Test Organization

Tests are split into three `#[cfg(test)]` sibling modules in `lib.rs`:

- `tests_core.rs` — Basic matching, exists, nesting, arrays, delete/rebuild,
  builder, clone, error handling, pattern limits.
- `tests_operators.rs` — Prefix, suffix, wildcard, shellstyle, anything-but,
  equals-ignore-case, numeric, regex, CIDR, lookaround.
- `tests_stress.rs` — Fuzz, concurrency, unicode, scaling guards.

Plus `automaton/tests.rs` for low-level automaton unit tests.

### Reducing Test Duplication

The test files contain significant repetitive boilerplate. Apply these patterns:

#### A. Use a `q!` helper macro for common setup

Many tests repeat `Quamina::new()` + `add_pattern` + `matches_for_event`. A
macro can reduce noise:

```rust
// In a shared test helper at the top of each test module, or a test_helpers module:
macro_rules! q {
    ($($name:expr => $pattern:expr),+ $(,)?) => {{
        let mut q = Quamina::new();
        $( q.add_pattern($name, $pattern).unwrap(); )+
        q
    }};
}
```

Usage:
```rust
#[test]
fn test_exact_match() {
    let q = q!("p1" => r#"{"status": ["active"]}"#);
    assert_eq!(q.matches_for_event(br#"{"status": "active"}"#).unwrap(), vec!["p1"]);
}
```

#### B. Use `assert_matches!` / `assert_no_match!` helper macros

The pattern `q.matches_for_event(event.as_bytes()).unwrap()` + assert is
repeated hundreds of times. Extract it:

```rust
macro_rules! assert_matches {
    ($q:expr, $event:expr, $expected:expr) => {
        let matches = $q.matches_for_event($event.as_bytes()).unwrap();
        assert_eq!(matches, $expected, "Event: {}", $event);
    };
    ($q:expr, $event:expr, $expected:expr, $msg:expr) => {
        let matches = $q.matches_for_event($event.as_bytes()).unwrap();
        assert_eq!(matches, $expected, "{}: event={}", $msg, $event);
    };
}

macro_rules! assert_no_match {
    ($q:expr, $event:expr) => {
        let matches = $q.matches_for_event($event.as_bytes()).unwrap();
        assert!(matches.is_empty(), "Expected no match for: {}", $event);
    };
}
```

#### C. Use table-driven tests for repeated patterns

Many tests check the same operation across multiple inputs. Consolidate into
loops:

```rust
// BEFORE (repeated blocks):
let m1 = q.matches_for_event(r#"{"x": "a"}"#.as_bytes()).unwrap();
assert_eq!(m1, vec!["p1"]);
let m2 = q.matches_for_event(r#"{"x": "b"}"#.as_bytes()).unwrap();
assert_eq!(m2, vec!["p1"]);

// AFTER (table-driven):
for event in [r#"{"x": "a"}"#, r#"{"x": "b"}"#] {
    assert_matches!(q, event, vec!["p1"]);
}
```

#### D. Consolidate Miri-friendly test variants

Several tests have both a full version (`#[cfg_attr(miri, ignore)]`) and a
Miri-friendly variant. Where the logic is identical, use a shared helper
parameterized by iteration count:

```rust
fn rebuild_threshold_impl(iterations: usize, patterns: usize) {
    // ... shared logic ...
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_should_rebuild_threshold() { rebuild_threshold_impl(500, 5); }

#[test]
#[cfg(miri)]
fn test_should_rebuild_threshold_miri() { rebuild_threshold_impl(400, 3); }
```

#### E. Reduce verbosity in automaton/tests.rs

The `EventField` construction is very verbose. A helper function would help:

```rust
fn field(path: &str, value: &str) -> EventField {
    EventField { path: path.to_string(), value: value.to_string(), array_trail: vec![], is_number: false }
}
```

#### F. Test Naming Convention

| Convention | Example |
|-----------|---------|
| `test_{feature}_{scenario}` | `test_prefix_match` |
| `test_{feature}_{scenario}_miri_friendly` | `test_rebuild_threshold_miri_friendly` |
| Avoid redundant `test_test_` prefixes | |

### Test Quality Rules

- Every test must assert something meaningful (no empty test bodies).
- Use `#[cfg_attr(miri, ignore)]` with a `// MIRI SKIP RATIONALE:` comment.
- Prefer `assert_eq!` over `assert!(x == y)` for better failure messages.
- Stress/fuzz tests use `SeedableRng` for reproducibility.

---

## 5. Performance Idioms

### Allocation

| Do | Don't |
|----|-------|
| Reuse buffers via thread-locals (`TL_FLATTENER`, `TL_NFA_BUFS`). | Allocate new `Vec`s on every `matches_for_event` call. |
| Use `SmallVec` for small, bounded collections (epsilon targets). | Use `SmallVec` everywhere — only where profiling shows benefit. |
| Use `FxHashSet`/`FxHashMap` for non-cryptographic internal maps. | Use default `HashMap` for hot paths (slower hashing). |
| Pre-size `Vec::with_capacity` when the size is known. | Call `vec![]` then repeatedly `push` when the count is known. |
| Use `Cow<'_, [u8]>` when data is usually borrowed but occasionally owned. | Clone defensively "just in case". |

### Iteration

| Do | Don't |
|----|-------|
| Use iterator chains (`.iter().filter().map().collect()`). | Write manual index loops unless index access is needed. |
| Use `sort_unstable_by` for non-stable sorts (faster). | Use `sort_by` when stability doesn't matter. |
| Use `memchr` for single-byte searches. | Use `bytes().position(|b| b == target)`. |

### Concurrency

| Do | Don't |
|----|-------|
| Use `ArcSwap` for lock-free read-mostly data. | Use `RwLock` for the hot read path. |
| Use `parking_lot::Mutex` when contention is rare. | Use `std::sync::Mutex` (no poisoning, slower). |
| Use thread-local buffers for per-call scratch space. | Share mutable buffers across threads with locks. |
| Drop locks (`drop(guard)`) before doing further work. | Hold locks across I/O or heavy computation. |

---

## 6. API Design (Rust API Guidelines)

Reference: [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

### Naming

| Convention | Example |
|-----------|---------|
| Types are `UpperCamelCase`. | `QuaminaBuilder`, `PatternLimits` |
| Functions/methods are `snake_case`. | `matches_for_event`, `add_pattern` |
| Builder methods are `with_*` for configuration. | `with_media_type`, `with_auto_rebuild` |
| Getter methods omit `get_` prefix. | `pattern_count()`, not `get_pattern_count()` |
| Predicate methods start with `is_`/`has_`/`should_`. | `is_empty()`, `should_rebuild()` |
| Conversion methods: `as_*` (cheap), `to_*` (expensive), `into_*` (consuming). | |

### Type Design

| Guideline | Applied in quamina-rs |
|-----------|----------------------|
| Implement `Default` for types with sensible defaults. | `Quamina::default()`, `QuaminaBuilder::default()` |
| Implement `Clone` where semantically meaningful. | `Quamina::clone()` creates an independent snapshot. |
| Implement `Debug` for all public types. | `QuaminaError`, `PatternLimits`, `PrunerStats` |
| Implement `Display` for error types. | `QuaminaError::fmt()` |
| Implement `std::error::Error` for error types. | `impl std::error::Error for QuaminaError` |
| Use generics with trait bounds, not `dyn Trait` in hot paths. | `Quamina<X: Clone + Eq + Hash + Send + Sync>` |
| Use builder pattern for complex construction. | `QuaminaBuilder` |
| Validate inputs early; panic only on logic bugs. | `assert!(depth > 0)` in builder; `Result` for patterns. |

### Documentation

| Convention | Example |
|-----------|---------|
| All public items have `///` doc comments. | `/// Add a pattern with the given identifier` |
| Include `# Example` sections with runnable doctests. | `QuaminaBuilder::new()` has examples. |
| Document `# Errors` and `# Panics` sections where relevant. | `with_media_type` documents both. |
| Module-level docs use `//!`. | `//! quamina-rs: Fast pattern-matching library` |
| Don't document private items unless the logic is non-obvious. | |

---

## 7. Code Duplication Inventory

### Source Code (src/)

The source code is generally well-factored. Key observations:

| Area | Status | Action |
|------|--------|--------|
| `matches_for_event` / `matches_for_event_custom_flattener` share filtering logic | **Done** | Extracted `filter_deleted_matches()` helper. |
| `clone()` / `rebuild()` share pattern-replay logic | **Done** | Extracted `replay_patterns_into()` helper. |

### Test Code (7,216 lines across 4 files)

Test duplication is the main area for improvement:

| Pattern | Occurrences | Reduction Strategy |
|---------|-------------|-------------------|
| `Quamina::new()` + `add_pattern` + unwrap | ~80 | `q!` macro |
| `matches_for_event(x.as_bytes()).unwrap()` + assert | ~150 | `assert_matches!` / `assert_no_match!` macros |
| `EventField { path: ..., value: ..., array_trail: vec![], is_number: false }` | ~20 | `field()` helper in `automaton/tests.rs` |
| Full + Miri-friendly duplicate tests | ~6 pairs | Shared parameterized helper |
| Repeated `QuaminaBuilder::<&str>::new()...build().unwrap()` | ~15 | `builder!` macro or helper function |
| Each `anything-but` validation test creates its own `Quamina::new()` | ~5 | Consolidate into single test with table |

**Estimated line reduction: ~800-1200 lines (11-17%)** across test files, with
no loss of coverage.

### Automaton Tests Verbosity

`automaton/tests.rs` (609 lines) has highly repetitive `CoreMatcher` and
`ThreadSafeCoreMatcher` tests that mirror each other. These could be
consolidated with a generic test helper:

```rust
fn test_matcher_exact_impl<M: MatcherLike>(matcher: M) { ... }

#[test]
fn test_core_matcher_exact() { test_matcher_exact_impl(CoreMatcher::new()); }
#[test]
fn test_thread_safe_matcher_exact() { test_matcher_exact_impl(ThreadSafeCoreMatcher::new()); }
```

---

## 8. Clippy & Lints Configuration

### Configuration (Cargo.toml)

```toml
[lints.rust]
unexpected_cfgs = { level = "warn", check-cfg = ['cfg(kani)'] }
unsafe_code = "warn"

[lints.clippy]
perf = { level = "warn", priority = -1 }
undocumented_unsafe_blocks = "warn"
transmute_ptr_to_ptr = "warn"
# Nursery lints (selectively enabled)
use_self = "warn"
derive_partial_eq_without_eq = "warn"
redundant_clone = "warn"
unused_peekable = "warn"
or_fun_call = "warn"
branches_sharing_code = "warn"
equatable_if_let = "warn"
```

### Nursery Lint Evaluation

| Lint | Status | Rationale |
|------|--------|-----------|
| `use_self` | **Enabled** | 52 fixes, purely mechanical, improves readability |
| `derive_partial_eq_without_eq` | **Enabled** | 1 fix, correct |
| `redundant_clone` | **Enabled** | Performance improvement |
| `unused_peekable` | **Enabled** | Catches dead code |
| `or_fun_call` | **Enabled** | Avoids unnecessary allocation in `ok_or` |
| `branches_sharing_code` | **Enabled** | Deduplicates shared code at end of if/else |
| `equatable_if_let` | **Enabled** | Cleaner pattern matching |
| `option_if_let_else` | Skipped | Too many false positives where `map_or` hurts readability |
| `missing_const_for_fn` | Skipped | 41 warnings, `const fn` restrictions evolve too quickly |
| `significant_drop_tightening` | Skipped | False positives on intentional lock scopes |
| `non_send_fields_in_send_ty` | Skipped | False positives on safe wrappers |
| `redundant_pub_crate` | Skipped | `pub(crate)` inside private modules is harmless |

### Formatting

- `rustfmt` with default settings (no `.rustfmt.toml` overrides needed).
- Line width: 100 (default).
- Import groups: `std` > external crates > `crate::` > `super::`.

---

## 9. Patterns from quamina-go to Preserve

These patterns were inherited from the Go implementation and should be retained:

1. **Numbits encoding** — Base-128 variable-width encoding for comparing floats
   as byte strings. The `to_q_number` function must preserve total ordering.
2. **Segments tree pruning** — Field-path trie that lets the flattener skip
   JSON subtrees not referenced by any pattern.
3. **Case folding table** — Unicode case-insensitive matching via a static fold
   table (not locale-dependent `to_lowercase`).
4. **I-Regexp subset** — RFC 9485 compliance with `~` as escape character.
   Supports lookahead/lookbehind extensions.
5. **NFA-based matching** — Arena-allocated NFA states with epsilon closures
   precomputed at build time. ArcSwap for lock-free reads.
6. **Array cross-element prevention** — Array trail tracking ensures patterns
   match within a single array element, not across elements.
7. **Soft-delete + rebuild** — Patterns are soft-deleted (filtered from results),
   then permanently purged on `rebuild()`. Pruner stats drive auto-rebuild.

---

## 10. Anti-Patterns to Avoid

### General Rust

1. **Excessive `.clone()`** — Prefer borrowing. Clone only when ownership transfer
   is needed or the data is `Copy`.
2. **`unwrap()` in library code** — Use `?` propagation. Reserve `unwrap()` for
   provably-infallible paths with a `// SAFETY:` or `// invariant:` comment.
3. **`pub` to silence dead-code warnings** — Fix the root cause. `pub(crate)` is
   almost always sufficient for internal code.
4. **`#[allow(unused)]` at crate level** — Be specific: `#[allow(dead_code)]` on
   individual items with a comment explaining why.
5. **Stringly-typed errors** — Use `QuaminaError` variants, not `String`.
6. **Manual index loops** — Use iterators unless index arithmetic is the point.
7. **Blocking I/O in hot paths** — The matcher is pure computation; keep it that way.

### quamina-rs Specific

8. **Breaking the public API** — Don't reduce visibility of items that
   downstream crates depend on without a semver bump.
9. **Adding `#[doc(hidden)] pub mod` without justification** — Only for benchmark
   access. Prefer `pub(crate)` otherwise.
10. **Duplicating test logic across full/Miri variants** — Extract shared helpers;
    parameterize by iteration count.
11. **Creating a new `Quamina::new()` per assertion** — Group related assertions
    in a single test with a shared instance.
12. **Verbose struct construction in tests** — Use helper functions/macros for
    `EventField`, `OwnedField`, `MockFlattener`.

---

## 11. Rust 2024 Edition Opportunities

The crate currently uses `edition = "2021"`. When upgrading to 2024:

| Feature | Impact |
|---------|--------|
| `let ... else` (stable since 1.65, idiomatic in 2024) | Simplify early-return patterns |
| RPITIT (return-position `impl Trait` in traits) | Could simplify `Flattener` trait |
| `async fn` in traits (stable 1.75) | Not needed (synchronous crate) |
| `gen` blocks (nightly) | Not yet applicable |
| Lifetime elision improvements | Minor cleanup |

No rush to upgrade — wait until all dependencies support edition 2024.

---

## 12. Verification Checklist

Run after every change:

```bash
cargo check                                    # Type checking
cargo test                                     # All tests pass
cargo clippy -- -D warnings                    # No clippy warnings
cargo bench --bench matching -- --quick        # No perf regression (>10%)
cargo doc --no-deps                            # Docs build without warnings
```

For Miri validation (occasional):
```bash
cargo +nightly miri test                       # Memory safety under Miri
```

---

## 13. Progress Tracker

### Completed (Visibility Audit)

| File | Date | Changes |
|------|------|---------|
| `src/numbits.rs` | 2026-01-31 | Made `MAX_BYTES_IN_ENCODING` private; removed unused type aliases; moved test-only helpers to `#[cfg(test)]`; changed public functions to `pub(crate)` |
| `src/case_folding.rs` | 2026-01-31 | Made `CASE_FOLDING_PAIRS` private |
| `src/regexp/mod.rs` | 2026-01-31 | Removed internal functions from public re-exports |
| `src/regexp/nfa.rs` | 2026-01-31 | Changed `make_dot_fa` to `pub(crate)` |
| `src/regexp/parser.rs` | 2026-01-31 | Changed `invert_rune_range`, `simplify_rune_range` to `pub(crate)` |
| `src/segments_tree.rs` | 2026-01-31 | Made `SEGMENT_SEPARATOR` `pub(crate)` |
| `src/flatten_json.rs` | 2026-01-31 | Removed unused `len()` and `is_empty()` methods |

### Completed (Test Dedup)

| Item | Date | Changes |
|------|------|---------|
| Add `q!`, `assert_matches!`, `assert_no_match!` test macros | 2026-02-10 | Created `src/test_helpers.rs` with 6 macros + `exercise_wildcard()` helper; ~1000 lines saved across 4 test files |
| Add `field()` helper to `automaton/tests.rs` | 2026-02-10 | Replaced ~20 verbose `EventField` constructions |
| Consolidate Miri full/friendly test pairs | 2026-02-10 | Extracted `verify_ordering_random(count)` in numbits.rs, `verify_bulk_add_correctness(count)` in tests_stress.rs |
| Table-drive `test_invalid_json_events` | 2026-02-10 | Converted 13 cases to `&[(&[u8], &str)]` table |

### Completed (Source Dedup)

| Item | Date | Changes |
|------|------|---------|
| Extract `replay_patterns_into` helper | 2026-02-10 | Shared by `clone()` and `rebuild()` in lib.rs |
| Extract `filter_deleted_matches` helper | 2026-02-10 | Shared by `matches_for_event` and `matches_for_event_custom_flattener` in lib.rs |

### Completed (Visibility Audit — Phase 2)

| Item | Date | Changes |
|------|------|---------|
| `src/lib.rs` module visibility | 2026-02-10 | Added `#[doc(hidden)]` to `automaton`, `numbits`, `regexp` modules |
| `src/automaton/mod.rs` | 2026-02-10 | Added `#[doc(hidden)]` to `arena` submodule; changed `sparse_set` to `pub(crate)` |

### Completed (Clippy Nursery Lints)

| Item | Date | Changes |
|------|------|---------|
| Evaluate and enable nursery lints | 2026-02-11 | Enabled 7 nursery lints in Cargo.toml; fixed 52 `use_self`, 1 `unused_peekable`, 1 `or_fun_call`, 1 `branches_sharing_code`, 3 `option_if_let_else` (easy cases); skipped 5 noisy/false-positive lints |

### Pending Work

| Item | Category | Priority | Notes |
|------|----------|----------|-------|
| Upgrade to edition 2024 | Toolchain | Low | Wait for dependency readiness |

---

## 14. How to Use This Spec

1. Pick an item from "Pending Work" (prefer High priority).
2. Read the relevant files and apply the guidelines from the relevant section.
3. Verify with the checklist in section 12.
4. Move the item to "Completed" with a date and summary.
5. Commit with a message like: `refactor(tests): add q! macro to reduce test boilerplate`
