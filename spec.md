# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**362 tests passing.** Rust 1.5-2x faster than Go. Synced with Go commit e3d13cd (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 454 | 1.6x |
| status_middle_nested | 7,437 | 5,400 | 1.4x |

**Matchers:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`, `{"cidr"}`, `{"numeric"}`

---

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── regexp/
│   ├── parser.rs       # I-Regexp parser + lookaround
│   └── nfa.rs          # NFA building
└── automaton/
    ├── small_table.rs  # SmallTable (byte transitions)
    ├── fa_builders.rs  # FA construction
    ├── nfa.rs          # traverse_dfa, traverse_nfa
    ├── arena.rs        # StateArena for cyclic NFA
    └── mutable_matcher.rs # Pattern building
```

---

## Completed Optimizations

### Epsilon Closure O(n²)→O(n) ✅

Fixed in `automaton/nfa.rs` using `FxHashSet<StatePtr>`. 10-18% NFA speedup.

### State Acceleration (Wildcards) ✅

Implemented memchr SIMD skip for `traverse_nfa` with single spinout state.

**Results (suffix pattern `*X`):**
| String | Without | With | Speedup |
|--------|---------|------|---------|
| 10k | 263 µs | 4.2 µs | **62x** |
| 1k | 26.8 µs | 676 ns | **40x** |

### ASCII Fast Path for Negated Patterns ✅

Implemented memchr acceleration for ASCII-only negated character classes (`[^x]+`, `[^/]+`, `[^"]+`). Detects patterns at parse time and uses negated bytes directly as exit bytes.

**Results (pattern `a[^x]+x`):**
| String | Without | With | Speedup |
|--------|---------|------|---------|
| 1000 chars | 34.4 µs | 801 ns | **43x** |
| 100 chars | 4.06 µs | 444 ns | **9x** |
| 10 chars | 718 ns | 397 ns | **1.8x** |

---

## Optimization Roadmap

### Phase 2: Literal Prefiltering 🎯 NEXT

**Problem:** Full automaton runs on every field value.

**Solution:** Extract literal substrings from patterns, use fast multi-string search to find candidates, only run NFA around match positions.

From [Hyperscan paper](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf):
> "Complex regular expressions contain literal strings. It's cheaper to match literals than do full regex."

**Example:** Pattern `[a-z]+@example\.com`
1. Extract literal: `@example.com`
2. Use memchr to find `@` positions in input
3. Only run `[a-z]+` NFA backwards from `@`
4. Verify `example.com` suffix forwards

```rust
pub enum Prefilter {
    None,
    Memchr(u8),                    // Single byte literal
    Memchr2(u8, u8),               // Two alternatives
    Memmem(Vec<u8>),               // Literal string
    AhoCorasick(AhoCorasick),      // Multiple literals
}
```

**Impact:** 2-10x speedup for patterns with literals.

**Testing:** Fuzz literal extraction, verify prefilter never misses matches (superset property).

**Validation:** Benchmark with/without prefilter, measure false positive rate, memory overhead of Aho-Corasick.

**Reference:** [regex-filtered crate](https://docs.rs/regex-filtered/latest/regex_filtered/), [Hyperscan Rose subsystem](https://deepwiki.com/intel/hyperscan/1.1-architecture-overview)

---

### Phase 3: Teddy Multi-String Matching

**Problem:** Aho-Corasick is slower than SIMD-accelerated alternatives for small literal sets.

**Solution:** Use [Teddy algorithm](https://dl.acm.org/doi/10.1145/3472456.3473512) (already in `aho-corasick` crate) for fast multi-pattern literal search.

> "Teddy matches up to 64 characters with only 16 SIMD operations."

Used by ripgrep, Hyperscan. Available via `aho-corasick` crate's packed searchers.

**When:** After literal prefiltering is implemented, as the prefilter backend.

**Validation:** Benchmark Teddy vs NFA baseline, compare memory usage.

---

### Phase 4: Pattern Decomposition

**Problem:** Long patterns run entire NFA even when literals could short-circuit.

**Solution:** Break patterns at literal boundaries, chain specialized engines.

**Example:** `[a-z]+@[a-z]+\.com`
- Chunk 1: `[a-z]+` (NFA)
- Literal: `@` (memchr)
- Chunk 2: `[a-z]+` (NFA)
- Literal: `.com` (memmem)

From [Hyperscan](https://branchfree.org/2019/02/28/paper-hyperscan-a-fast-multi-pattern-regex-matcher-for-modern-cpus/):
> "Discovery of literal factors and decomposition into smaller engines separated by literals."

**Impact:** Avoids quadratic behavior, enables per-chunk optimization.

**Validation:** Benchmark long patterns before/after, verify no correctness regression, fuzz chunk boundaries.

---

### Phase 5: Lazy/Hybrid DFA

**Problem:** NFA simulation visits multiple states per byte.

**Solution:** Build DFA states lazily during search, cache for reuse.

From [regex-automata](https://docs.rs/regex-automata/latest/regex_automata/hybrid/index.html):
> "Builds DFA lazily during search. Only builds states actually visited."

**When:** After prefiltering, for hot-path patterns that would benefit from DFA speed.

**Validation:** Miri for cache safety, benchmark cache hit rates, memory ceiling enforcement, fuzz eviction paths.

---

### Phase 6: Bit-Parallel NFA (Research)

**Problem:** NFA state tracking uses pointer-chasing.

**Solution:** Represent states as bit vectors, use SIMD for parallel transitions.

From [Hyperscan](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf):
> "512 NFA states limit for SIMD Glushkov implementation - enough for most regexes."

**Challenge:** Unicode character classes create large NFAs. May only apply to ASCII patterns.

**Reference:** [Navarro's bit-parallel paper](https://users.dcc.uchile.cl/~gnavarro/ps/algor04.2.pdf)

---

## Evaluated & Rejected

| Optimization | Why Rejected |
|--------------|--------------|
| Byte equivalence classes | 6% regression for simple patterns |
| Batch ArrayPos allocation | SmallVec<[ArrayPos; 4]> already inline |
| SkinnyRuneTree (sparse) | Rust's range-based building already efficient |
| Arena NFA accel (Unicode) | UTF-8 validation = too many exit bytes |

---

## Behavioral Differences from Go

1. `{"anything-but": "foo"}` - Rust accepts single string, Go requires array
2. Flattener stops parsing once all pattern fields found

---

## Commands

```bash
cargo test                    # 362 tests
cargo bench                   # benchmarks
cargo clippy -- -D warnings   # lint
gh run list                   # CI status
```

---

## Key Files

**Optimization targets:**
- `src/automaton/nfa.rs` - traversal, acceleration
- `src/automaton/arena.rs` - cyclic NFA
- `src/regexp/nfa.rs` - regexp NFA building

**References:**
- `../quamina` - Go implementation
- [Hyperscan paper](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf)
- [regex-automata](https://docs.rs/regex-automata/latest/regex_automata/)
