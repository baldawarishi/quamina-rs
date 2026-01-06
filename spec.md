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
| status_context_fields | 382 | 487 | Go 1.27x |
| status_middle_nested | 6,400 | 4,800 | **Rust 1.33x** |
| status_last_field | 6,600 | 5,100 | **Rust 1.29x** |
| citylots | 3,400 | 3,700 | Go 1.09x |

## Completed

Tasks 1-10, 13, 15-16, 19-22: Q-numbers, segments_tree, streaming flattener, allocations (NfaBuffers, Cow), Unicode case folding, parking_lot::Mutex, automaton split, lib.rs split (wildcard.rs), unsafe from_utf8_unchecked, SmallVec for Field path/array_trail, direct Field matching (removed EventFieldRef indirection), Vec<Field> reuse across calls.

## Next Steps

### Performance Optimization

| # | Task | Impact | Notes |
|---|------|--------|-------|
| 23 | Remove `transition_map` lookup | ~20ns | Store `FrozenFieldMatcher` directly in transitions instead of pointer map |
| 24 | Profile citylots gap | ~300ns | Use flamegraph to identify GeoJSON-specific bottlenecks |

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
| Segment flattener | ✅ | ✅ | Early termination, Vec reuse |
| Unicode case folding | ✅ | ✅ | case_folding.rs |
| Custom regex NFA | ✅ | ❌ | Go: `regexp_nfa.go`, `regexp_parse.go`. Rust: `regex` crate fallback |
| Pruner rebuilding | ✅ | ❌ | Go: `pruner.go` auto-rebuilds. Rust: HashSet deletion (no rebuild) |

### Performance Gap Analysis

**Where Go wins:**
- `status_context_fields` (1.27x): Early termination with 2 fields. Gap narrowed from 1.31x.
- `citylots` (1.09x): Complex GeoJSON. Gap narrowed significantly from 1.21x (700ns to 300ns).

**Where Rust wins:**
- `status_middle_nested` (1.33x): Deep nested matching.
- `status_last_field` (1.29x): Late field in large JSON.

**Task 21 Results (Vec<Field> reuse):**
- `citylots`: -10% (4,100ns → 3,700ns)
- `exact_match`: -11%
- `nested_match`: -12%
- `100_patterns_no_match`: -15%
- `100_diverse_patterns_no_match`: -30%

**Remaining root causes:**
- Matching: Rust has `transition_map` lookup overhead
