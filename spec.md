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
| status_context_fields | 404 | 524 | Go 1.30x |
| status_middle_nested | 6,745 | 4,885 | **Rust 1.38x** |
| status_last_field | 7,217 | 5,037 | **Rust 1.43x** |
| citylots | 3,812 | 5,029 | Go 1.32x |

## Completed

Tasks 1-10, 13, 15-16: Q-numbers, segments_tree, streaming flattener, allocations (NfaBuffers, Cow), Unicode case folding, EventFieldRef, parking_lot::Mutex, automaton split, lib.rs split (wildcard.rs), unsafe from_utf8_unchecked (~5% improvement on status_context_fields).

## Next Steps

| # | Task | Notes |
|---|------|-------|
| 14 | Add profiling | flamegraph, pprof comparison |
| 17 | Pruner rebuilding | Go: auto-rebuilds when filtered/emitted > 0.2. Rust: simple HashSet filter. See `pruner.go` |
| 18 | Custom regex NFA | Go: custom NFA in automaton. Rust: `regex` crate fallback. See `regexp_nfa.go` |
| 19 | Investigate citylots gap | Go 3.8μs vs Rust 5μs (1.32x). Profile JSON flattening for GeoJSON |
| 20 | Investigate context_fields gap | Go 404ns vs Rust 524ns (1.30x). Profile early termination path |

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
- `status_context_fields` (1.30x): Early termination with small JSON. Rust may have overhead in field processing.
- `citylots` (1.32x): Complex GeoJSON. Rust flattener may allocate more or have slower traversal.

**Where Rust wins:**
- `status_middle_nested` (1.38x): Deep nested matching.
- `status_last_field` (1.43x): Late field in large JSON.

**Investigation needed:** Profile both gaps with flamegraph to identify specific bottlenecks.
