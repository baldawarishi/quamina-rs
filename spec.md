# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**368 tests passing.** Rust 1.5-2x faster than Go. Synced with Go commit e3d13cd (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 454 | 1.6x |
| status_middle_nested | 7,437 | 5,400 | 1.4x |

**Matchers:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`, `{"cidr"}`, `{"numeric"}`

---

## Optimization Process

**Before implementing any optimization:**

1. **Profile** - Run `cargo bench` and `cargo flamegraph` to find actual hot-spots
2. **Measure baseline** - Record exact numbers for the workload you're targeting
3. **Research** - Find reputable implementations from populare OSS libraries, research papers, and other reputable sources.
4. **Prototype** - Implement minimal version, measure impact
5. **Evaluate tradeoff** - Is the complexity worth the perf/memory gain?
6. **Verify coverage** - No loss in tests, miri, fuzz, or kani
7. **Ship or reject** - If it works, commit. If not, add to "Rejected" table below

**Rules:**
- No feature toggles - code is either in or out
- When in doubt, mirror Go behavior for algorithmic and performance parity
- Read actual source/tests, not notes
- Push often, check CI
- Keep Spec.md under 300 lines

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

**Key files for optimization:**
- `src/automaton/nfa.rs` - traversal, acceleration
- `src/automaton/arena.rs` - cyclic NFA
- `src/regexp/nfa.rs` - regexp NFA building

---

## Completed Optimizations

### Epsilon Closure O(n²)→O(n)

Fixed in `automaton/nfa.rs` using `FxHashSet<StatePtr>`. 10-18% NFA speedup.

### State Acceleration (Wildcards)

Implemented memchr SIMD skip for `traverse_nfa` with single spinout state.

| String | Before | After | Speedup |
|--------|--------|-------|---------|
| 10k chars | 263 µs | 4.2 µs | **62x** |
| 1k chars | 26.8 µs | 676 ns | **40x** |

### ASCII Fast Path for Negated Patterns

memchr acceleration for `[^x]+`, `[^/]+`, `[^"]+`. Detects at parse time.

| String | Before | After | Speedup |
|--------|--------|-------|---------|
| 1000 chars | 34.4 µs | 801 ns | **43x** |
| 100 chars | 4.06 µs | 444 ns | **9x** |

---

## Rejected Optimizations

| Optimization | Why Rejected |
|--------------|--------------|
| Byte equivalence classes | 6% regression for simple patterns |
| Batch ArrayPos allocation | SmallVec<[ArrayPos; 4]> already inline |
| SkinnyRuneTree (sparse) | Rust's range-based building already efficient |
| Arena NFA accel (Unicode) | UTF-8 validation = too many exit bytes |
| Literal prefiltering (basic) | Only helps narrow case (pure literals, non-matching). Most patterns use +/* which routes to arena NFA. Added ~10ns overhead for matching inputs. |
| memchr for JSON string scan | 10-15% regression. memchr setup cost > benefit for short strings (typical JSON values < 20 bytes). Byte-by-byte loop is faster. |

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
cargo +nightly miri test      # memory safety
cargo fuzz run <target>       # fuzzing
gh run list                   # CI status

# Flamegraph (requires terminal for sudo password)
CARGO_PROFILE_BENCH_DEBUG=true cargo flamegraph --root --bench matching -- --bench
open flamegraph.svg           # view in browser
```

---

## References

- `../quamina` - Go implementation (read source directly)
- [Hyperscan paper](https://www.usenix.org/system/files/nsdi19-wang-xiang.pdf)
- [regex-automata](https://docs.rs/regex-automata/latest/regex_automata/)
- [ripgrep internals](https://blog.burntsushi.net/ripgrep/)

append more as you find them here. 