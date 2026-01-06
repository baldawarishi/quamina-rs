# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**164 tests passing.** All core operators implemented.

| Operator | Path | Operator | Path |
|----------|------|----------|------|
| Exact, Prefix, Suffix | automaton/fallback | Wildcard, Shellstyle | automaton |
| Anything-but, Exists | automaton | Equals-ignore-case | automaton (Unicode) |
| Numeric (exact/compare) | automaton/fallback | Regex | automaton (I-Regexp subset) |

## Performance

| Benchmark | Go (ns) | Rust (ns) | Winner |
|-----------|---------|-----------|--------|
| status_context_fields | 382 | 492 | Go 1.29x |
| status_middle_nested | 6,400 | 4,558 | **Rust 1.40x** |
| status_last_field | 6,600 | 4,826 | **Rust 1.37x** |
| citylots | 3,400 | 3,624 | Go 1.07x |

Run: `cargo bench status` or `cargo bench citylots`

## Architecture

```
src/
├── lib.rs              # Public API (Quamina struct)
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener (segments_tree)
├── numbits.rs          # Q-number encoding for numeric comparisons
├── regexp.rs           # I-Regexp parser and NFA builder
├── automaton/
│   ├── mod.rs          # Module exports
│   ├── small_table.rs  # SmallTable, FaState, FieldMatcher, NfaBuffers
│   ├── fa_builders.rs  # make_string_fa, make_prefix_fa, merge_fas, etc.
│   ├── nfa.rs          # traverse_dfa, traverse_nfa
│   ├── thread_safe.rs  # ThreadSafeCoreMatcher, FrozenFieldMatcher/ValueMatcher
│   ├── mutable_matcher.rs  # MutableFieldMatcher/ValueMatcher (building)
│   └── wildcard.rs     # Shellstyle/wildcard pattern handling
└── fallback/           # Suffix, numeric ranges (non-automaton)
```

**Key types:**
- `SmallTable`: Compact byte-indexed transition table (ceilings/steps representation)
- `FaState`: Automaton state with table + field_transitions
- `FieldMatcher`: Low-level automaton node (path -> ValueMatcher transitions)
- `FrozenFieldMatcher<X>`: Immutable matcher with pattern IDs, used during matching
- `MutableFieldMatcher<X>`: RefCell-based builder, converted to Frozen after add_pattern

**Matching flow:**
1. `flatten()` JSON -> sorted `Vec<Field>` (path, value, array_trail, is_number)
2. `matches_for_fields_direct()` traverses automaton from root
3. For each field, `transition_on()` matches value via DFA/NFA traversal
4. `FrozenValueMatcher.transition_map` (FxHashMap) maps FieldMatcher ptrs -> FrozenFieldMatcher

## Completed (Tasks 1-26)

**Core:** Q-numbers, segments_tree flattener, NfaBuffers reuse, Unicode case folding, parking_lot::Mutex, automaton module split, custom regex NFA, pruner rebuilding.

**Optimizations:**
- `unsafe from_utf8_unchecked` in flattener (validated JSON)
- `SmallVec` for Field path/array_trail
- Direct Field matching (removed EventFieldRef indirection)
- Vec<Field> reuse across flatten calls (returns `&mut [Field]`)
- FxHashMap for transition lookups (faster than std HashMap for usize keys)
- Vec reuse in traverse_dfa/traverse_nfa via NfaBuffers.transitions
- `#[inline]` on hot functions (dstep, step, traverse_dfa, traverse_nfa, transition_on)

**Task 18 (regex NFA):** Ported I-Regexp subset parser and NFA builder from Go. Supports `.`, `[...]`, `|`, `(...)`, `?`. Escape char is `~` (not `\`). Unsupported features (`*`, `+`, `{n,m}`, `~p{...}`) fall back to `regex` crate.

**Task 23 learnings:** Attempted Vec-based indexing (store index in FieldMatcher, lookup by index). **Regressed 5-8%** because accessing `arc_fm.index` requires Arc dereference, while `Arc::as_ptr()` for HashMap keys doesn't dereference. FxHashMap was better solution.

**Task 25 (pruner rebuilding):** Added `PrunerStats` to track emitted/filtered patterns. `rebuild()` creates new automaton with only live patterns. `maybe_rebuild()` auto-triggers when filtered/emitted > 0.2 and activity > 1000. Matches Go's `pruner.go` behavior.

**Task 26 (traverse optimization):** Modified traverse_dfa/traverse_nfa to accept mutable Vec reference instead of returning new Vec. Added transitions buffer to NfaBuffers for reuse. Improved status_middle_nested by 8%, status_last_field by 3.4%. Citylots gap narrowed to 1.07x (was 1.10x).

## Next Steps

| # | Task | Notes |
|---|------|-------|
| 27 | Optimize citylots gap | Profile JSON flattening vs matching; citylots dominated by many small values |

**Potential optimizations:**
- Pool allocations for result vectors in transition_on
- SIMD for SmallTable.step() ceiling search
- Generic FaState<T> to eliminate HashMap indirection for frozen matchers

## Parity

| Feature | Go | Rust | Notes |
|---------|:--:|:----:|-------|
| Automaton core | ✅ | ✅ | SmallTable, NFA/DFA |
| Segment flattener | ✅ | ✅ | Early termination, Vec reuse |
| Unicode case folding | ✅ | ✅ | case_folding.rs |
| Custom regex NFA | ✅ | ✅ | I-Regexp subset (`.`, `[...]`, `|`, `(...)`, `?`) |
| Pruner rebuilding | ✅ | ✅ | Auto-rebuild at 0.2 filtered/emitted ratio |

## Go Reference Files

| Feature | Go File | Notes |
|---------|---------|-------|
| Matching core | `core_matcher.go` | matchesForFields, tryToMatch |
| Value matching | `value_matcher.go` | transitionOn, makeStringFA |
| Flattening | `flatten_json.go` | segmentsTree, Flatten |
| Pruner | `pruner.go` | rebuildRatio=0.2, deletePatterns |
| Regex NFA | `regexp_nfa.go` | Custom NFA vs Rust's `regex` crate |
| Benchmarks | `benchmarks_test.go` | Status patterns, citylots |

## Commands

```bash
cargo test                    # Run all tests
cargo bench status            # Run status_* benchmarks
cargo bench citylots          # Run citylots benchmark
cargo bench                   # Run all benchmarks
```
