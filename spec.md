# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**248 tests passing.** Full Go parity + Rust-only features. Rust 1.5-2x faster on all benchmarks. Synced with Go commit c443b44 (Jan 2026).

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
cargo test                    # 248 tests
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
- `ValueTrie` builds all strings into a trie with shared prefixes: O(n) construction
- `add_string_transitions_bulk()` uses trie instead of hierarchical merge
- Hash-based deduplication of identical end states
- Single-pass conversion from trie to SmallTable

**Performance improvements (vs naive O(n²)):**

| Benchmark | Naive | Hierarchical | Trie | Speedup |
|-----------|-------|--------------|------|---------|
| bulk_100x10 | 16ms | 11ms | **2.2ms** | 7x |
| bulk_1000x10 | ~5s | 182ms | **75ms** | ~66x |
| bulk_100x100 | 181ms | 119ms | **6.6ms** | 27x |
| bulk_100x10_multifield | 2.5s | 36ms | **6.2ms** | 400x |

**Key files:**
- `src/automaton/trie.rs`: ValueTrie with hash-based deduplication
- `src/automaton/mutable_matcher.rs`: `add_string_transitions_bulk()` uses trie
- `benches/matching.rs`: Bulk benchmark suite

**Approaches tried:**
1. ❌ Hash-consed states alone: Overhead outweighed benefits
2. ⚡ Hierarchical merge: O(n log n), good but not optimal
3. ✅ Trie-based construction: O(n), optimal for string building

**Future optimization opportunities (for follow-up sessions):**
- Parallel trie building with rayon for very large value sets
- Extended trie support for prefix/monocase patterns (as in Go fork)
- Incremental trie updates for addPattern() use case

**Benchmarks to verify:**
```bash
cargo bench bulk_100x10   # ~2.2ms
cargo bench bulk_1000x10  # ~75ms
cargo bench bulk_100x100  # ~6.6ms
```
