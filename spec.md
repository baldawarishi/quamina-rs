# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**142 tests passing.** All core operators implemented.

| Operator | Path | Operator | Path |
|----------|------|----------|------|
| Exact, Prefix, Suffix | automaton/fallback | Wildcard, Shellstyle | automaton |
| Anything-but, Exists | automaton | Equals-ignore-case | automaton (Unicode) |
| Numeric (exact/compare) | automaton/fallback | Regex | fallback (`regex` crate) |

## Performance

| Benchmark | Go (ns) | Rust (ns) | Winner |
|-----------|---------|-----------|--------|
| status_context_fields | 404 | 554 | Go 1.37x |
| status_middle_nested | 6,745 | 4,972 | **Rust 1.36x** |
| status_last_field | 7,217 | 5,213 | **Rust 1.38x** |
| citylots | 3,812 | 4,937 | Go 1.29x |

## Completed

Tasks 1-10, 15: Q-numbers, segments_tree, streaming flattener, allocations (NfaBuffers, Cow), Unicode case folding, EventFieldRef, parking_lot::Mutex, automaton split (2,581→6 files, largest 815 lines).

## Next Steps

| # | Task | Notes |
|---|------|-------|
| 13 | Unsafe `from_utf8_unchecked` | Optional, ~5-10ns/field |
| 14 | Add profiling | flamegraph, pprof comparison |
| 16 | Split lib.rs | 2,890 lines → ~3 files |

## Parity

| Feature | Go | Rust | Notes |
|---------|:--:|:----:|-------|
| Automaton core | ✅ | ✅ | SmallTable, NFA/DFA |
| Segment flattener | ✅ | ✅ | Early termination |
| Unicode case folding | ✅ | ✅ | case_folding.rs |
| Custom regex NFA | ✅ | ❌ | Using `regex` crate |
| Pruner rebuilding | ✅ | ❌ | HashSet deletion |
