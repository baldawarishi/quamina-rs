# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**164 tests passing.** All core operators implemented. Performance parity achieved.

| Benchmark | Go (ns) | Rust (ns) | Status |
|-----------|---------|-----------|--------|
| status_context_fields | 442 | 458 | Parity (4% gap) |
| status_middle_nested | 7,700 | 4,650 | **Rust 1.66x faster** |
| status_last_field | 8,100 | 4,964 | **Rust 1.63x faster** |
| citylots | 3,570 | 3,446 | **Rust 3% faster** |

## Parity Gaps

### Functional (non-blocking)
| Gap | Notes |
|-----|-------|
| Config options | Go has WithMediaType, WithFlattener, WithPatternDeletion; Rust uses simple new() |
| Custom Flattener | Go allows pluggable flatteners; Rust hardcodes JSON |

### Test Coverage Gaps
| Category | Priority | Notes |
|----------|----------|-------|
| Large-scale stress tests | HIGH | Go tests 27 patterns × 150K events; Rust lacks |
| Race condition tests | HIGH | Go uses -race flag; Rust needs equivalent |
| Fuzzing | HIGH | Go has TestFuzzValueMatcher; Rust has none |
| Pruner edge cases | MEDIUM | Go has 8+ tests; Rust has 4 |
| Concurrent update stress | MEDIUM | Test pattern add during active matching |

### Rust-only features (not in Go)
- `{"numeric": ["<", 100]}` - numeric range operators (uses fallback)
- `{"suffix": ".jpg"}` - dedicated suffix operator (automaton-integrated)
- `has_matches()`, `count_matches()` - optimized boolean/count queries
- `pattern_count()`, `is_empty()`, `clear()` - inventory management
- `pruner_stats()`, `set_auto_rebuild()` - explicit rebuild control
- Better number parsing: Rust accepts `1e0`, Go rejects (Go bug)

## Architecture

```
src/
├── lib.rs              # Public API (Quamina struct)
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── segments_tree.rs    # Field path tracking for skip optimization
├── numbits.rs          # Q-number encoding for numeric comparisons
├── regexp.rs           # I-Regexp parser and NFA builder
├── automaton/
│   ├── small_table.rs  # SmallTable (byte transition table), FaState, NfaBuffers
│   ├── fa_builders.rs  # make_string_fa, make_prefix_fa, merge_fas
│   ├── nfa.rs          # traverse_dfa, traverse_nfa
│   ├── thread_safe.rs  # FrozenFieldMatcher/ValueMatcher (immutable, for matching)
│   ├── mutable_matcher.rs  # MutableFieldMatcher/ValueMatcher (for building)
│   └── wildcard.rs     # Shellstyle/wildcard patterns
└── fallback/           # Suffix, numeric ranges (non-automaton path)
```

**Matching flow:**
1. `flatten()` JSON -> sorted `Vec<Field>` (path as Arc, value, array_trail)
2. `matches_for_fields_direct()` traverses automaton from root
3. For each field, `transition_on()` matches value via DFA/NFA traversal

## Optimizations Applied (Tasks 1-27)

| Optimization | Impact | Notes |
|-------------|--------|-------|
| Arc<[u8]> for paths | citylots -20% | O(1) cloning from SegmentsTree |
| FxHashMap transitions | ~5% | Faster than std HashMap for ptr keys |
| NfaBuffers reuse | ~8% | Vec reuse in traverse_dfa/traverse_nfa |
| Direct Field matching | ~3% | Removed EventFieldRef indirection |
| SmallVec array_trail | minor | Inline storage for shallow nesting |
| #[inline] hot paths | varies | dstep, step, traverse_*, transition_on |

**Key learnings:**
- Task 23: Vec-based indexing regressed 5-8% (Arc deref cost). FxHashMap with Arc::as_ptr() is faster.
- Task 27: Path cloning was citylots bottleneck. Go uses slice refs; now we use Arc<[u8]>.

## Future Work

**Performance (diminishing returns):**
- SIMD for SmallTable.step() ceiling search
- Pool allocations for transition_on result vectors

**Test coverage:**
- Add large-scale stress tests (1000+ patterns)
- Add property-based fuzzing (proptest/quickcheck)
- Add concurrent update stress tests

## Go Reference

| Feature | Go File | Key Functions |
|---------|---------|---------------|
| Matching | `core_matcher.go` | matchesForFields, tryToMatch |
| Values | `value_matcher.go` | transitionOn, makeStringFA |
| Flatten | `flatten_json.go` | segmentsTree, Flatten |
| Pruner | `pruner.go` | rebuildRatio=0.2, deletePatterns |
| Regex | `regexp_nfa.go` | Custom NFA (I-Regexp subset) |
| Bench | `benchmarks_test.go` | Status patterns, citylots |

## Commands

```bash
cargo test                    # 164 tests
cargo bench status            # status_* benchmarks
cargo bench citylots          # citylots benchmark
cargo clippy -- -D warnings   # CI runs this
```

## Session Notes

When continuing work:
1. Read this spec for context
2. For Go behavior, read Go source directly (don't trust past interpretations)
3. Run benchmarks before/after changes: `cargo bench <name>`
4. Push often and check CI (`gh run list`)
