# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**369 tests passing.** Rust 1.5-2x faster than Go.

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 454 | 1.6x |
| status_middle_nested | 7,437 | 5,400 | 1.4x |

**Matchers:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`, `{"cidr"}`, `{"numeric"}`

---

## Go Sync Status

Last synced: Go commit `e3d13cd` (Jan 15 2026).
Go HEAD as of Feb 6 2026: commit `12c9d3c`.

### Porting Candidates

| Go Change | Rust Status | Priority | Notes |
|-----------|-------------|----------|-------|
| Remove `forField` from public API | `for_field` still exposed in `regexp/nfa.rs` | **Medium** | Small change. Go always wraps with quote matching now. |
| Epsilon closure precomputation | On-demand via `FxHashSet` | **Low** | Different tradeoff. Benchmark before porting. |
| `sync.Pool` for `nfaBuffers` | Owned buffers with `.clear()` | **Low** | Rust ownership model handles this differently. |
| Result buffer pooling (`matchesInto`) | Allocates per call | **Low** | Would require API change. |
| `transmap.all()` preallocation | No direct equivalent | **Low** | Minor allocation win. |

### Already Covered (No Action Needed)

| Go Change | Why No Port Needed |
|-----------|--------------------|
| Full I-Regexp support (quantifiers, classes, groups, Unicode props) | Rust already has all of this **plus** lookaround assertions |
| Flattener loop optimization (`readNumber`, `readStringValue`) | Rust already uses index-based loops with zero-copy |
| `faNext` struct removal (issue #404) | Rust never had the wrapper; uses `Arc<FaState>`/`StateId` directly |
| `startState` caching in nfaBuffers | Rust accesses via frozen matcher reference, already cached |
| ArrayTrail batch allocation | Rust uses `SmallVec<[ArrayPos; 4]>` (inline, no heap for common case) |
| SkinnyRuneTree sparse optimization | Rust handles ranges at UTF-8 byte level; already efficient |
| DFA transitions buffer pooling | `NfaBuffers` already clears and reuses buffers |
| Transmap buffer pooling | `SparseSet` with O(1) clear is better than Go's map approach |

### Key Differences in Approach

**Epsilon closure:** Go precomputes at build time, stores `epsilonClosure []*faState` per state, uses generation counter to avoid visited-set allocation. Rust computes on-demand per traversal step using `FxHashSet<StatePtr>`. Rust has ~6% overhead on simple patterns (HashSet setup > scanning 1-2 pointers) but is faster on complex patterns with many epsilon transitions.

**Buffer reuse:** Go uses `sync.Pool` for thread-safe buffer sharing. Rust uses owned `NfaBuffers` with `.clear()` on each call. Rust's `SparseSet` gives O(1) clear vs Go's `clear(map)`. Both approaches are valid for their respective concurrency models.

**Regexp:** Rust is ahead with lookaround assertions (`(?=...)`, `(?!...)`, `(?<=...)`, `(?<!...)`). Go doesn't have these yet.

---

## Optimization Process

**Before implementing any optimization:**

1. **Profile** - Run `cargo bench` and `cargo flamegraph` to find actual hot-spots
2. **Measure baseline** - Record exact numbers for the workload you're targeting
3. **Research** - Find reputable implementations from popular OSS libraries, research papers, and other reputable sources
4. **Prototype** - Implement minimal version, measure impact
5. **Evaluate tradeoff** - Is the complexity worth the perf/memory gain?
6. **Verify coverage** - No loss in tests, miri, fuzz, or kani
7. **Ship or reject** - If it works, commit. If not, add to "Rejected" table below

**Rules:**
- No feature toggles - code is either in or out
- When in doubt, mirror Go behavior for algorithmic and performance parity
- Read actual source/tests, not notes
- Push often, check CI
- Keep spec.md under 300 lines

---

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── flattener.rs        # Flattener trait (pluggable parsers)
├── segments_tree.rs    # Fast field-skipping optimization
├── regexp/
│   ├── parser.rs       # I-Regexp parser + lookaround
│   └── nfa.rs          # NFA building (arena-based)
└── automaton/
    ├── small_table.rs  # SmallTable (byte transitions), NfaBuffers
    ├── nfa.rs          # traverse_dfa, traverse_nfa, acceleration
    ├── arena.rs        # StateArena for cyclic NFA (5.5k lines)
    ├── mutable_matcher.rs # Pattern building (single-threaded)
    ├── thread_safe.rs  # ThreadSafeCoreMatcher (ArcSwap, lock-free reads)
    ├── trie.rs         # Trie structure for patterns
    └── sparse_set.rs   # O(1)-clear set for state tracking
```

**Key files for optimization:**
- `src/automaton/nfa.rs` - traversal hot path, acceleration
- `src/automaton/arena.rs` - arena NFA traversal, cyclic patterns
- `src/regexp/nfa.rs` - regexp NFA building, `for_field` parameter

---

## Completed Optimizations

### Epsilon Closure O(n squared) to O(n)
Fixed in `automaton/nfa.rs` using `FxHashSet<StatePtr>`. 10-18% NFA speedup.

### State Acceleration (Wildcards)
memchr SIMD skip for `traverse_nfa` with single spinout state.

| String | Before | After | Speedup |
|--------|--------|-------|---------|
| 10k chars | 263 us | 4.2 us | **62x** |
| 1k chars | 26.8 us | 676 ns | **40x** |

### ASCII Fast Path for Negated Patterns
memchr acceleration for `[^x]+`, `[^/]+`, `[^"]+`. Detects at parse time.

| String | Before | After | Speedup |
|--------|--------|-------|---------|
| 1000 chars | 34.4 us | 801 ns | **43x** |
| 100 chars | 4.06 us | 444 ns | **9x** |

### Memory Layout (SmallVec + SparseSet)
Inlined small collections (4-8 elements) to avoid heap. SparseSet for O(1) membership + clear.

### Fast DFA Traversal Path
Direct dispatch when no epsilon/spinout states. Avoids NFA overhead for pure-DFA patterns.

---

## Rejected Optimizations

| Optimization | Why Rejected |
|--------------|--------------|
| Byte equivalence classes | 6% regression for simple patterns |
| Batch ArrayPos allocation | SmallVec<[ArrayPos; 4]> already inline |
| SkinnyRuneTree (sparse) | Rust's range-based building already efficient |
| Arena NFA accel (Unicode) | UTF-8 validation = too many exit bytes |
| Literal prefiltering | Only helps pure literals non-matching case. ~10ns overhead for matching. |
| memchr for JSON string scan | 10-15% regression. Setup cost > benefit for short strings (< 20 bytes). |

---

## Behavioral Differences from Go

1. `{"anything-but": "foo"}` - Rust accepts single string, Go requires array
2. Flattener stops parsing once all pattern fields found
3. Rust exposes `for_field` param in regexp NFA building; Go removed it (always true)
4. Rust has lookaround assertions in regexp; Go does not

---

## Commands

```bash
cargo test                    # 369 tests
cargo bench                   # benchmarks
cargo clippy -- -D warnings   # lint
cargo +nightly miri test      # memory safety
cargo fuzz run <target>       # fuzzing
gh run list                   # CI status

# Flamegraph
CARGO_PROFILE_BENCH_DEBUG=true cargo flamegraph --root --bench matching -- --bench
open flamegraph.svg
```

---

## References

- `../quamina` - Go implementation (read source directly)
- [Hyperscan paper](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf)
- [regex-automata](https://docs.rs/regex-automata/latest/regex_automata/)
- [ripgrep internals](https://blog.burntsushi.net/ripgrep/)

Append more as you find them here.
