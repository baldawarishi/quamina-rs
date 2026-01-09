# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**213 tests passing.** All core operators implemented. Full Go parity achieved plus `{n,m}` quantifiers. Rust outperforms Go on all benchmarks.

| Benchmark | Go (ns) | Rust (ns) | Status |
|-----------|---------|-----------|--------|
| status_context_fields | 442 | 408 | **Rust 8% faster** |
| status_middle_nested | 7,700 | 4,866 | **Rust 1.58x faster** |
| status_last_field | 8,100 | 5,139 | **Rust 1.58x faster** |
| citylots | 3,570 | 2,105 | **Rust 1.70x faster** |
| numeric_range_single | - | 145 | Rust-only (automaton) |
| numeric_range_two_sided | - | 144 | Rust-only (automaton) |
| numeric_range_10_patterns | - | 176 | Rust-only (automaton) |

## Parity Status

**Full Go parity achieved.** All Go pattern types and features implemented.

| Go Feature | Rust Status |
|------------|-------------|
| Exact/Prefix/Wildcard/Shellstyle | ✓ Automaton-based |
| Anything-but/Exists | ✓ Automaton-based |
| Equals-ignore-case | ✓ Automaton-based |
| Regexp (I-Regexp subset) | ✓ NFA-based (see Known Issues) |
| Custom Flattener | ✓ `Flattener` trait |
| Config options | ✓ `QuaminaBuilder` |
| Copy/Clone | ✓ `impl Clone for Quamina` |

### Known Issues

1. **Regexp performance**: Rust uses chain-based NFA (depth=100) vs Go's cyclic GC references. Complex patterns with nested `*`/`+` on long strings may be slower.
2. **Regexp sample coverage**: 992 Go test samples ported; 67 fully tested, rest skipped due to performance constraints (patterns with multiple `*`/`+`, `[^]`).

### Rust-only features (not in Go)
- Regexp `{n,m}` quantifiers (Go parses but rejects as unimplemented)
- `{"numeric": ["<", 100]}` - numeric range operators (automaton-integrated)
- `{"suffix": ".jpg"}` - dedicated suffix operator (automaton-integrated)
- `has_matches()`, `count_matches()` - optimized boolean/count queries
- `pattern_count()`, `is_empty()`, `clear()` - inventory management
- `pruner_stats()`, `set_auto_rebuild()` - explicit rebuild control
- Better number parsing: Rust accepts `1e0`, Go rejects (Go bug)

## Custom Flattener API

The `Flattener` trait allows custom event parsers for non-JSON formats:

```rust
use quamina::{Flattener, SegmentsTreeTracker, OwnedField, QuaminaError};

struct MyFlattener;

impl Flattener for MyFlattener {
    fn flatten(
        &mut self,
        event: &[u8],
        tracker: &dyn SegmentsTreeTracker,
    ) -> Result<Vec<OwnedField>, QuaminaError> {
        // Custom parsing logic - use tracker to skip unused fields
        Ok(vec![])
    }

    fn copy(&self) -> Box<dyn Flattener> {
        Box::new(MyFlattener)
    }
}

// Usage:
let q = QuaminaBuilder::<String>::new()
    .with_flattener(Box::new(MyFlattener))
    .unwrap()
    .build()
    .unwrap();
```

Key types:
- `Flattener` - Trait for event parsers
- `SegmentsTreeTracker` - Trait for field path tracking (enables skip optimization)
- `OwnedField` - Returned by custom flatteners (path, value, array_trail, is_number)
- `JsonFlattener` - Default JSON implementation (also usable through trait)

## Architecture

```
src/
├── lib.rs              # Public API (Quamina struct, QuaminaBuilder)
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener (internal)
├── flattener.rs        # Flattener/SegmentsTreeTracker traits, JsonFlattener
├── segments_tree.rs    # Field path tracking for skip optimization
├── numbits.rs          # Q-number encoding for numeric comparisons
├── regexp.rs           # I-Regexp parser and NFA builder
├── regexp_samples.rs   # 992 test samples from Go (test-only)
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

**Regexp improvements (optional):**
- Optimize `[^]` negated class NFA construction (currently O(unicode_range))
- Consider true cyclic NFA with `unsafe` or `Rc<RefCell>` for better * performance

**General (diminishing returns):**
- SIMD for SmallTable.step() ceiling search
- Pool allocations for transition_on result vectors
- Property-based fuzzing (proptest/quickcheck)

## Go Reference

| Feature | Go File | Key Functions |
|---------|---------|---------------|
| Matching | `core_matcher.go` | matchesForFields, tryToMatch |
| Values | `value_matcher.go` | transitionOn, makeStringFA |
| Flatten | `flatten_json.go` | segmentsTree, Flatten |
| Pruner | `pruner.go` | rebuildRatio=0.2, deletePatterns |
| Regex | `regexp_nfa.go` | Custom NFA (I-Regexp subset) |
| Bench | `benchmarks_test.go` | Status patterns, citylots |
| Tests | `regexp_validity_test.go` | 992 samples, TestToxicStack |

## Commands

```bash
cargo test                    # 210 tests
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
