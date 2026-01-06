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

## Completed (Tasks 1-22)

Core: Q-numbers, segments_tree, streaming flattener, NfaBuffers/Cow allocations, Unicode case folding, parking_lot::Mutex, automaton split, wildcard.rs extraction.

Optimizations: unsafe from_utf8_unchecked, SmallVec for Field path/array_trail, direct Field matching (no EventFieldRef), Vec<Field> reuse (returns `&mut [Field]`).

## Next Steps

| # | Task | Notes |
|---|------|-------|
| 23 | Remove `transition_map` lookup | Store FrozenFieldMatcher directly (~20ns) |
| 24 | Profile citylots gap | Flamegraph for remaining ~300ns gap |
| 17 | Pruner rebuilding | Go auto-rebuilds at 0.2 ratio. See `pruner.go` |
| 18 | Custom regex NFA | Go: `regexp_nfa.go`. Rust uses `regex` crate |

## Parity

| Feature | Go | Rust | Notes |
|---------|:--:|:----:|-------|
| Automaton core | ✅ | ✅ | SmallTable, NFA/DFA |
| Segment flattener | ✅ | ✅ | Early termination, Vec reuse |
| Unicode case folding | ✅ | ✅ | case_folding.rs |
| Custom regex NFA | ✅ | ❌ | Rust uses `regex` crate fallback |
| Pruner rebuilding | ✅ | ❌ | Rust: HashSet deletion only |
