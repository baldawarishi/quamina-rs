# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**148 tests passing.** All core operators implemented.

| Operator | Path | Operator | Path |
|----------|------|----------|------|
| Exact, Prefix, Suffix | automaton/fallback | Wildcard, Shellstyle | automaton |
| Anything-but, Exists | automaton | Equals-ignore-case | automaton (Unicode) |
| Numeric (exact/compare) | automaton/fallback | Regex | fallback (`regex` crate) |

## Performance

| Benchmark | Go (ns) | Rust (ns) | Winner |
|-----------|---------|-----------|--------|
| status_context_fields | 382 | 500 | Go 1.31x |
| status_middle_nested | 6,400 | 4,700 | **Rust 1.36x** |
| status_last_field | 6,600 | 5,000 | **Rust 1.32x** |
| citylots | 3,400 | 4,100 | Go 1.21x |

## Completed

Tasks 1-10, 13, 15-16, 19-20, 22: Q-numbers, segments_tree, streaming flattener, allocations (NfaBuffers, Cow), Unicode case folding, parking_lot::Mutex, automaton split, lib.rs split (wildcard.rs), unsafe from_utf8_unchecked, SmallVec for Field path/array_trail, direct Field matching (removed EventFieldRef indirection).

## Next Steps

### Performance Optimization

| # | Task | Impact | Notes |
|---|------|--------|-------|
| 21 | Reuse `Vec<Field>` across calls | ~50ns | Store in `FlattenJsonState`, return `&[Field]`. Requires lifetime refactor |
| 23 | Remove `transition_map` lookup | ~20ns | Store `FrozenFieldMatcher` directly in transitions instead of pointer map |
| 24 | Profile citylots gap | ~700ns | Use flamegraph to identify GeoJSON-specific bottlenecks |

### Feature Parity

| # | Task | Notes |
|---|------|-------|
| 14 | Add profiling | flamegraph, pprof comparison |
| 17 | Pruner rebuilding | Go: auto-rebuilds when filtered/emitted > 0.2. Rust: simple HashSet filter. See `pruner.go` |
| 18 | Custom regex NFA | Go: custom NFA in automaton. Rust: `regex` crate fallback. See `regexp_nfa.go` |

## Parity

| Feature | Go | Rust | Notes |
|---------|:--:|:----:|-------|
| Automaton core | ✅ | ✅ | SmallTable, NFA/DFA |
| Segment flattener | ✅ | ✅ | Early termination |
| Unicode case folding | ✅ | ✅ | case_folding.rs |
| Custom regex NFA | ✅ | ❌ | Go: `regexp_nfa.go`, `regexp_parse.go`. Rust: `regex` crate fallback |
| Pruner rebuilding | ✅ | ❌ | Go: `pruner.go` auto-rebuilds. Rust: HashSet deletion (no rebuild) |

### Performance Gap Analysis

**Where Go wins:**
- `status_context_fields` (1.38x): Early termination with 2 fields. Gap: flatten 50ns + matching 94ns.
- `citylots` (1.32x): Complex GeoJSON with 4 patterns.

**Where Rust wins:**
- `status_middle_nested` (1.28x): Deep nested matching.
- `status_last_field` (1.25x): Late field in large JSON.

**Root causes identified:**
- Flatten: Rust allocates new `Vec<Field>` per call; Go reuses slice with `fields[:0]`
- Matching: Rust has `transition_map` lookup; `EventFieldRef` indirection eliminated
