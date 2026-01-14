# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**242 tests passing.** Full Go parity + Rust-only features. Rust 1.5-2x faster on all benchmarks. Synced with Go commit c443b44 (Jan 2026).

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
cargo test                    # 242 tests
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

## Active Work: Bulk Pattern Add Optimization (Go #363)

**Problem**: Adding many patterns with many values is O(n²) due to repeated `merge_fas` calls.
Current: 1000 patterns × 1000 values takes ~12s in Go. Goal: 2-3s.

**Benchmark to create first:**
```rust
// benches/bulk_add.rs - establish baseline before any changes
fn bench_bulk_add(c: &mut Criterion) {
    c.bench_function("bulk_1000x100", |b| {
        b.iter(|| {
            let mut q = Quamina::<usize>::new();
            for i in 0..1000 {
                let pattern = format!(r#"{{"field": ["{}"]}}"#,
                    (0..100).map(|j| format!("value_{}_{}", i, j)).collect::<Vec<_>>().join("\", \""));
                q.add_pattern(i, &pattern).unwrap();
            }
        })
    });
}
```

**Approaches (try in order, revert to main if stuck):**

### Approach 1: Hash-Consed States (Most Ambitious)
Global content-addressable cache for `FaState`. States with identical content share memory.

```rust
// Key idea: hash FaState by content, not pointer
type StateHash = u64;
struct GlobalStateCache {
    cache: DashMap<StateHash, Arc<FaState>>,  // or RwLock<HashMap>
}

fn hash_state(state: &FaState) -> StateHash {
    // DJB2 or FxHash over: transitions, epsilons, field_transitions
}

fn get_or_create_state(cache: &GlobalStateCache, state: FaState) -> Arc<FaState> {
    let hash = hash_state(&state);
    cache.entry(hash).or_insert_with(|| Arc::new(state)).clone()
}
```

Changes needed:
- Add `StateHash` computation to `FaState`
- Global cache in `MutableValueMatcher` or thread-local
- Modify all `make_*_fa` functions to use cache
- Modify `merge_fa_states` to check cache before creating

**Success criteria**: 3x+ speedup on bulk_1000x100, no test regressions.
**Failure modes**: Hash collisions cause incorrect matching, complexity explosion.

### Approach 2: Parallel Build + Hierarchical Merge
Build FAs in parallel with rayon, merge in tree pattern (log N rounds).

```rust
fn add_patterns_bulk(&mut self, patterns: Vec<(X, String)>) {
    // 1. Parse all patterns in parallel
    let fas: Vec<SmallTable> = patterns.par_iter()
        .map(|(_, p)| build_fa_for_pattern(p))
        .collect();

    // 2. Hierarchical merge (log N rounds)
    let merged = fas.into_iter()
        .reduce(|a, b| merge_fas(&a, &b))
        .unwrap();

    // 3. Final merge with existing
    self.start_table = merge_fas(&self.start_table, &merged);
}
```

Changes needed:
- Add `rayon` dependency
- New `add_patterns` API on `Quamina`
- Parallel FA construction
- Tree-reduce merge

**Success criteria**: 2x+ speedup on bulk_1000x100 with 4+ cores.
**Failure modes**: Merge still dominates, minimal speedup on single core.

### Approach 3: Trie-Based Bulk Construction (Go Fork Approach)
Build trie first, convert to DFA in one shot. Proven 5x speedup in Go.

```rust
struct TrieNode {
    children: HashMap<u8, Box<TrieNode>>,
    is_end: bool,
    is_wildcard: bool,
    patterns: HashSet<X>,
    field_transitions: HashMap<String, Box<TrieNode>>,
    hash: u64,  // cached, computed bottom-up
}

fn build_trie(patterns: &[(X, Vec<u8>)]) -> TrieNode { ... }
fn trie_to_small_table(node: &TrieNode, cache: &mut HashMap<u64, Arc<FaState>>) -> SmallTable { ... }
```

Reference: https://github.com/DigitalPath-Inc/quamina/blob/main/trie.go

Changes needed:
- New `trie.rs` module
- Trie construction from patterns
- Hash calculation (DJB2, sorted children)
- Trie-to-SmallTable conversion with dedup cache

**Success criteria**: 5x+ speedup matching Go fork results.
**Failure modes**: Complexity in handling all pattern types (wildcard, prefix, etc.).

### Approach 4: Incremental Minimal DFA (Daciuk's Algorithm)
Add strings one-by-one, minimizing on-the-fly. Requires sorted input.

Reference: Daciuk et al., "Incremental Construction of Minimal Acyclic Finite State Automata" (2000)

**Success criteria**: Minimal memory, competitive speed.
**Failure modes**: Hard to adapt for non-string patterns (wildcards, etc.).

**Research links:**
- [Hierarchical DFA Merging](https://ieeexplore.ieee.org/document/9064254/)
- [DAWG basics](https://jbp.dev/blog/dawg-basics.html)
- [Hash consing for automata](http://gallium.inria.fr/blog/fixin-your-automata/)
- [Daciuk's algorithm](http://www.jandaciuk.pl/adfa.html)
