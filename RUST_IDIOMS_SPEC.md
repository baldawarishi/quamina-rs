# Rust Idioms & Cleanup Spec

This document tracks idiomatic Rust patterns to apply across the codebase. Work is done incrementally across sessions, file-by-file or module-by-module.

## Session Continuation Prompt

Copy this into a new Claude Code session to continue the work:

```
Continue the Rust idioms cleanup tracked in RUST_IDIOMS_SPEC.md

Pick the next file from "Pending Review" section and:
1. Audit visibility (pub -> pub(crate) -> private as appropriate)
2. Move test-only code to #[cfg(test)] blocks
3. Remove unused type aliases/constants
4. Verify: cargo check, cargo test, cargo clippy, cargo bench --bench matching -- --quick

After changes, update the spec:
- Move file to "Completed" with summary of changes
- Add any new files discovered to "Pending Review"

Start with src/segments_tree.rs or src/flattener.rs (smaller files first).
```

## Guiding Principles

1. **Minimum necessary visibility** - [Effective Rust Item 22](https://effective-rust.com/visibility.html)
2. **Preserve quamina-go patterns where appropriate** - This codebase is a port, keep compatible idioms
3. **No performance/correctness regressions** - Always run tests and benchmarks
4. **Incremental changes** - One file/module at a time, verify after each

## Checklist Per File/Module

### Visibility Audit
- [ ] `pub` items: Are they used outside the crate? If not, use `pub(crate)`
- [ ] `pub(crate)` items: Are they used outside the module? If not, use `pub(super)` or private
- [ ] Public type aliases: Are they actually used by external callers?
- [ ] Public constants: Are they implementation details or part of the API?
- [ ] Test-only utilities: Should be in `#[cfg(test)]` blocks, not public

### Visibility Decision Tree
```
Is it used by external crates (public API)?
  YES -> pub
  NO  -> Is it used by other modules in this crate?
           YES -> pub(crate)
           NO  -> Is it used by parent module only?
                    YES -> pub(super)
                    NO  -> private (no pub)

Is it only used in tests?
  YES -> Move to #[cfg(test)] block or make it private
```

### Code Quality
- [ ] No `#[allow(dead_code)]` without explanation comment
- [ ] Unused imports removed
- [ ] Clippy passes with no warnings

### Verification
- [ ] `cargo check` passes
- [ ] `cargo test` passes (all tests)
- [ ] `cargo clippy` passes
- [ ] `cargo bench` shows no significant regression (>10%)

## Progress Tracker

### Completed
| File | Date | Changes |
|------|------|---------|
| `src/numbits.rs` | 2026-01-31 | Made `MAX_BYTES_IN_ENCODING` private; removed unused `Numbits`/`QNumber` type aliases; moved `q_num_from_bytes`/`q_num_to_string` to test module; changed `numbits_from_f64`/`to_q_number` to `pub(crate)` |
| `src/case_folding.rs` | 2026-01-31 | Made `CASE_FOLDING_PAIRS` private (only used by `case_fold_char`) |
| `src/regexp/mod.rs` | 2026-01-31 | Removed `make_dot_fa`, `invert_rune_range`, `simplify_rune_range` from public re-exports |
| `src/regexp/nfa.rs` | 2026-01-31 | Changed `make_dot_fa` to `pub(crate)` |
| `src/regexp/parser.rs` | 2026-01-31 | Changed `invert_rune_range`, `simplify_rune_range` to `pub(crate)` |

### Pending Review
| File | Notes |
|------|-------|
| `src/regexp_samples.rs` | Test data file - already `#[cfg(test)]`, looks clean |
| `src/unicode_categories.rs` | All categories used via match statement - OK as-is |
| `src/segments_tree.rs` | Public module, needs visibility audit |
| `src/flatten_json.rs` | `#[doc(hidden)]` - review if should stay public |
| `src/json.rs` | `#[doc(hidden)]` - review if should stay public |
| `src/flattener.rs` | Has public re-exports in lib.rs - review what's actually needed |
| `src/automaton/*.rs` | Large module, audit incrementally |
| `src/lib.rs` | Main entry point - verify public API is intentional |

## Patterns from quamina-go to Preserve

These patterns were inherited from the Go implementation and should be retained:

1. **Numbits encoding** - The base-128 variable-width encoding for floats
2. **Segments tree pruning** - Field path tracking for JSON flattening optimization
3. **Case folding table** - Unicode case-insensitive matching approach
4. **I-Regexp subset** - RFC 9485 compliance with `~` as escape character
5. **NFA-based matching** - The overall automaton architecture

## Anti-Patterns to Avoid

1. Don't add `pub` just to silence dead_code warnings - fix the actual issue
2. Don't use `#[allow(unused)]` crate-wide - be specific
3. Don't expose internal implementation types in public API
4. Don't break semver by changing existing public API visibility (library users depend on it)

## How to Use This Spec

1. Pick a file from "Pending Review"
2. Read the file, apply the checklist
3. Make changes, verify with tests/clippy/benchmarks
4. Move entry to "Completed" with summary of changes
5. Commit with message referencing this spec
