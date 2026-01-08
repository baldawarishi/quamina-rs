# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**170 tests passing.** All core operators implemented. Rust outperforms Go on all benchmarks.

| Benchmark | Go (ns) | Rust (ns) | Status |
|-----------|---------|-----------|--------|
| status_context_fields | 442 | 408 | **Rust 8% faster** |
| status_middle_nested | 7,700 | 4,866 | **Rust 1.58x faster** |
| status_last_field | 8,100 | 5,139 | **Rust 1.58x faster** |
| citylots | 3,570 | 2,105 | **Rust 1.70x faster** |
| numeric_range_single | - | 145 | Rust-only (automaton) |
| numeric_range_two_sided | - | 144 | Rust-only (automaton) |
| numeric_range_10_patterns | - | 176 | Rust-only (automaton) |

## Recent: Automaton-Based Numeric Ranges (Completed)

Successfully moved `{"numeric": ["<", 100]}` from fallback to automaton-based matching.

**Implementation:**
- `make_numeric_less_fa()` - FA for `<` and `<=` operators
- `make_numeric_greater_fa()` - FA for `>` and `>=` operators
- `make_numeric_range_fa()` - Combined FA for two-sided ranges (e.g., `>= 0, < 100`)
- Q-numbers preserve lexicographic ordering, enabling byte-by-byte comparison

## Parity Gaps

### Functional (non-blocking)
| Gap | Notes |
|-----|-------|
| Config options | Go has WithMediaType, WithFlattener, WithPatternDeletion; Rust uses simple new() |
| Custom Flattener | Go allows pluggable flatteners; Rust hardcodes JSON |

### Test Coverage Gaps
| Category | Priority | Notes |
|----------|----------|-------|
| ~~Large-scale stress tests~~ | ~~HIGH~~ | ✓ Ported: 10K string/number fuzz, citylots2 213K events |
| Race condition tests | HIGH | Go uses -race flag; Rust needs equivalent |
| ~~Fuzzing~~ | ~~HIGH~~ | ✓ Ported: test_stress_fuzz_strings, test_stress_fuzz_numbers |
| Pruner edge cases | MEDIUM | Go has 8+ tests; Rust has 4 |
| Concurrent update stress | MEDIUM | Test pattern add during active matching |

### Known Issues
| Issue | Notes |
|-------|-------|
| merge_fas spinout bug | Multiple shellstyle patterns on same field give incorrect results due to improper spinout merging. Go's `mergeFAStates` handles this; Rust workaround: run patterns individually. |

### Rust-only features (not in Go)
- `{"numeric": ["<", 100]}` - numeric range operators (automaton-integrated)
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

### Completed: Automaton-Based Numeric Ranges

~~**Goal:** Move `{"numeric": ["<", 100]}` from fallback to automaton.~~

**Status: COMPLETED.** Numeric ranges now use automaton-based matching with ~142ns latency.

**Implementation details:**
- Single-sided ranges (`< 100`, `>= 50`): Direct FA construction via `make_numeric_less_fa`/`make_numeric_greater_fa`
- Two-sided ranges (`>= 0, < 100`): Combined FA via `make_numeric_range_fa` (not intersection-based)
- Q-numbers preserve lexicographic ordering, enabling byte-by-byte comparison
- FA handles variable-length Q-numbers (1-10 bytes) correctly

---

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
cargo test                    # 167 tests
cargo bench status            # status_* benchmarks
cargo bench citylots          # citylots benchmark
cargo bench numeric_range     # numeric range benchmarks
cargo clippy -- -D warnings   # CI runs this
```

## Session Notes

When continuing work:
1. Read this spec for context
2. For Go behavior, read Go source directly (don't trust past interpretations)
3. Run benchmarks before/after changes: `cargo bench <name>`
4. Push often and check CI (`gh run list`)
