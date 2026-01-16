# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**258 tests passing.** Full Go parity + Rust-only features. Rust 1.5-2x faster on all benchmarks. Synced with Go commit c443b44 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| status_context_fields | 398 | 362 | 1.1x |
| status_middle_nested | 7,437 | 4,912 | 1.5x |
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 405 | 1.8x |

## Pattern Types

**Go parity (all automaton-based):**
- `"value"` - exact match
- `{"prefix": "foo"}` - prefix match
- `{"suffix": ".jpg"}` - suffix match (Rust-only dedicated operator)
- `{"wildcard": "a*b"}` - wildcard with escape support
- `{"shellstyle": "a*b"}` - simple wildcard (no escapes)
- `{"exists": true/false}` - field presence
- `{"anything-but": ["a", "b"]}` - exclusion list
- `{"equals-ignore-case": "FOO"}` - case-insensitive
- `{"regexp": "[a-z]+"}` - I-Regexp subset via NFA

**Rust-only features:**
- `{"anything-but": 404}` / `{"anything-but": [400, 404]}` - numeric exclusion (Go #328)
- `{"numeric": [">=", 0, "<", 100]}` - range operators
- `{"cidr": "10.0.0.0/24"}` - IPv4/IPv6 CIDR matching (Go #187)
- `{"regexp": "a{2,5}"}` - quantifier support
- `has_matches()`, `count_matches()`, `pattern_count()`, `clear()`

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum, CidrPattern
├── flatten_json.rs     # Streaming JSON flattener
├── flattener.rs        # Flattener trait for custom parsers
├── segments_tree.rs    # Field path tracking (skip optimization)
├── numbits.rs          # Q-number encoding for numeric comparisons
├── regexp.rs           # I-Regexp parser and arena NFA builder
├── automaton/
│   ├── small_table.rs  # SmallTable (byte transitions), FaState
│   ├── fa_builders.rs  # make_string_fa, make_prefix_fa, merge_fas
│   ├── nfa.rs          # traverse_dfa, traverse_nfa
│   ├── arena.rs        # StateArena for cyclic NFA (regexp)
│   ├── trie.rs         # ValueTrie for O(n) bulk string construction
│   ├── thread_safe.rs  # FrozenFieldMatcher (immutable, for matching)
│   └── mutable_matcher.rs  # MutableFieldMatcher (for building)
└── wildcard.rs         # Shellstyle/wildcard matching
```

**Matching flow:**
1. `flatten()` JSON → sorted `Vec<Field>` (path as Arc, value, array_trail)
2. `matches_for_fields_direct()` traverses automaton from root
3. `transition_on()` matches via DFA (simple) or NFA (wildcard/regexp)

**Key design decisions:**
- Arc<[u8]> for paths (O(1) cloning, 20% citylots improvement)
- FxHashMap with Arc::as_ptr() keys (5% faster than std HashMap)
- Arena-based NFA for regexp (2.5x faster than chain NFA)
- Buffer reuse in NFA traversal (55% shellstyle improvement)

## Custom Flattener

```rust
use quamina::{Flattener, SegmentsTreeTracker, OwnedField, QuaminaError};

impl Flattener for MyFlattener {
    fn flatten(&mut self, event: &[u8], tracker: &dyn SegmentsTreeTracker)
        -> Result<Vec<OwnedField>, QuaminaError> {
        // Custom parsing - use tracker.is_prefix_used() to skip unused fields
        Ok(vec![])
    }
    fn copy(&self) -> Box<dyn Flattener> { Box::new(MyFlattener) }
}

let q = QuaminaBuilder::<String>::new()
    .with_flattener(Box::new(MyFlattener)).unwrap()
    .build().unwrap();
```

## Go Reference

| Feature | Go File | Key Functions |
|---------|---------|---------------|
| Matching | `core_matcher.go` | matchesForFields, tryToMatch |
| Values | `value_matcher.go` | transitionOn, makeStringFA |
| Flatten | `flatten_json.go` | segmentsTree, Flatten |
| Pruner | `pruner.go` | rebuildRatio=0.2, deletePatterns |
| Regex | `regexp_nfa.go` | Custom NFA (I-Regexp subset) |

## Commands

```bash
cargo test                    # 258 tests
cargo bench status            # status_* benchmarks
cargo bench citylots          # citylots benchmark
cargo bench shellstyle        # shellstyle benchmark
cargo clippy -- -D warnings   # CI runs this
```

## Session Notes

**When continuing work:**
1. Read this spec for context
2. For Go behavior, read Go source directly (don't trust past interpretations)
3. Run benchmarks before/after changes: `cargo bench <name>`
4. Push often and check CI (`gh run list`)

**Known issues:**
- Regexp: 992 Go samples ported, 67 fully tested. Others use features outside I-Regexp (lookahead, backrefs)

## Completed: Bulk Pattern Add Optimization (Go #363)

**Problem solved**: Adding many patterns with many values was O(n²) due to repeated `merge_fas` calls.

**Solution**: Trie-based bulk construction for patterns with multiple exact string values.
- Arena-based trie: all nodes in contiguous Vec, referenced by index (fewer heap allocs)
- SmallVec for children (most nodes have 1-4 children)
- Binary search for sorted children (consistent hashing, no sort needed)
- FxHashMap for state deduplication cache
- Iterative hash generation (avoids recursion overhead and SmallVec clones)
- Single-pass conversion from trie to SmallTable

**Performance improvements (vs naive O(n²)):**

| Benchmark | Naive | Hierarchical | Trie v1 | Trie v2 (Arena) | Packed Merge | Total Speedup |
|-----------|-------|--------------|---------|-----------------|--------------|---------------|
| bulk_100x10 | 16ms | 11ms | 2.2ms | 1.8ms | **1.1ms** | 15x |
| bulk_1000x10 | ~5s | 182ms | 75ms | 70ms | **62ms** | ~81x |
| bulk_100x100 | 181ms | 119ms | 6.6ms | 3.9ms | **3.1ms** | 58x |
| bulk_100x10_multifield | 2.5s | 36ms | 6.2ms | 5.1ms | **4.3ms** | 581x |

**Key files:**
- `src/automaton/trie.rs`: Arena-based ValueTrie with hash deduplication
- `src/automaton/mutable_matcher.rs`: `add_string_transitions_bulk()` uses trie
- `src/automaton/fa_builders.rs`: `merge_tables_packed()` for efficient table merging
- `benches/matching.rs`: Bulk benchmark suite

**Approaches tried:**
1. ❌ Hash-consed states alone: Overhead outweighed benefits
2. ⚡ Hierarchical merge: O(n log n), good but not optimal
3. ✅ Trie-based construction: O(n), optimal for string building
4. ✅ Arena allocation + SmallVec: 17-41% faster than Box-per-node
5. ✅ Packed merge: Avoids 256-element array allocation, 12-40% faster

**Future optimization opportunities (for follow-up sessions):**
- Parallel trie building with rayon for very large value sets
- Extended trie support for prefix/monocase patterns (as in Go fork)
- Incremental trie updates for addPattern() use case

**Benchmarks to verify:**
```bash
cargo bench bulk_100x10   # ~1.1ms
cargo bench bulk_1000x10  # ~62ms
cargo bench bulk_100x100  # ~3.1ms
```

## Future Work

**Range quantifier tests added (Jan 2026):**
- Error parsing tests (`a{2,`, `a{5,2}`, etc.)
- Equivalence tests (`{0,1}`=`?`, `{1,}`=`+`, `{0,}`=`*`)
- Edge cases: `{0,0}` (epsilon), `.{2,4}`, `(ab){2,3}`, `a{5,10}`

**Pending tasks for next session:**
1. **Expand regexp sample testing** - 992 samples ported, ~67 fully tested. Update `test_regexp_validity` to enable `*`, `+`, `[^...]` patterns (now implemented). Use arena NFA for + and * patterns.
2. **Pattern retrieval API (Go #73)** - Add `get_patterns()` and `list_pattern_ids()` methods to retrieve registered patterns.
3. **Parallel trie building** - Use rayon for very large value sets.
4. **Unicode property matchers** - `~p{Lu}`, `~P{Ll}` not yet in Go or Rust.
