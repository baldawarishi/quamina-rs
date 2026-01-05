# quamina-rs

A Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

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

## Completed (1-10)

1. ✅ Benchmark baseline
2. ✅ Port numbits.go (Q-numbers)
3. ✅ Integrate Q-numbers into automaton
4. ✅ JSON parsing optimization (segments_tree + streaming flattener)
5. ✅ CityLots benchmark
6. ✅ Reduce allocations (reusable flattener, NfaBuffers, Cow)
7. ✅ Unicode case folding (2876 pairs from Go)
8. ✅ Evaluate regex approach → Keep `regex` crate
9. ✅ Zero-copy field matching (EventFieldRef)
10. ✅ Switch to `parking_lot::Mutex` (566→554ns, ~2% improvement)

## Investigated (not beneficial)

- **Combined MatchState Mutex (tasks 11-12)**: Tested combining flattener + nfa_bufs into single Mutex. Caused 12% regression in automaton-heavy benchmarks due to longer critical section. Two quick locks beat one long lock.

## Next Steps

### Performance (close early-field gap)

| # | Task | Impact |
|---|------|--------|
| 13 | Unsafe `from_utf8_unchecked` (optional) | ~5-10ns/field |

### Code Health

| # | Task | Current | Target |
|---|------|---------|--------|
| 14 | Add profiling (flamegraph, pprof comparison) | - | - |
| 15 | Split automaton.rs | 2,581 lines | ~5 files (match Go) |
| 16 | Split lib.rs | 2,890 lines | ~3 files (match Go) |

**Note:** automaton.rs converted to module directory, tests extracted (1,016 lines).
Further splitting possible: small_table, nfa, fa_builders, mutable_matcher, thread_safe.

**Go file sizes for reference:** largest is 869 lines (flatten_json.go)

## Parity

| Feature | Go | Rust | Notes |
|---------|:--:|:----:|-------|
| Automaton core | ✅ | ✅ | SmallTable, NFA/DFA |
| Segment flattener | ✅ | ✅ | Early termination |
| Unicode case folding | ✅ | ✅ | case_folding.rs |
| Custom regex NFA | ✅ | ❌ | Using `regex` crate |
| Pruner rebuilding | ✅ | ❌ | HashSet deletion |
