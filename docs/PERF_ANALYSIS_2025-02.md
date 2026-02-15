# Performance Analysis: Benchmark Gaps vs Go/Java (Feb 2025)

Three benchmarks where quamina-rs was slower than reference implementations.
All benchmarks use identical patterns/events. Measured on Apple M3 Max.

## Status

- [x] **Finding 1**: Vec capacity bug — FIXED, merged into `perf/fix-vec-capacity-nfa`
- [ ] **Finding 2**: Suffix algorithm — TODO (reverse-and-DFA-trie)
- [ ] **Finding 3**: Result Vec allocation in `transition_on` — TODO
- [ ] **Finding 4**: Spinout epsilon closure — UNVALIDATED (needs profiling)

## Before/After (Vec capacity fix only)

| Benchmark | Before | After | Change | vs Go/Java |
|---|---|---|---|---|
| Regexp `[a-z]+` | 324 ns | **204 ns** | **−37%** | 0.80x Go (faster) |
| Shellstyle 26 `A*`-`Z*` | 429 ns | **254 ns** | **−41%** | 0.89x Go (faster) |
| Suffix 100 `.ext0`-`.ext99` | 17,446 ns | **15,182 ns** | **−13%** | 42x Java (still slow) |
| Wildcard 26 (bonus) | ~414 ns | **232 ns** | **−44%** | — |
| exact_single | 83 ns | 83 ns | unchanged | — |
| exact_100 | 149 ns | 149 ns | unchanged | — |
| prefix_100 | 165 ns | 165 ns | unchanged | — |

## Methodology

- Samply CPU profiling at 10kHz with presymbolication (symbolicated via sidecar files)
- Criterion benchmarks for baselines
- Micro-benchmarks isolating specific operations (take vs swap Vec pattern)
- Source code comparison across Rust, Go, Java codebases

## Finding 1: Vec Capacity Bug — FIXED

**Location**: `src/automaton/arena.rs`, `traverse_arena_nfa`

`std::mem::take(&mut bufs.current_states)` replaced the Vec with `Vec::new()` (capacity 0).
This empty Vec got swapped into `next_states`. On every subsequent iteration, pushes to
`next_states` triggered `grow_one` — a heap allocation every single iteration of the main loop.

### Profile evidence (% of total benchmark time in `grow_one`, pre-fix)

| Benchmark | grow from `traverse_arena_nfa` | grow from `try_to_match_direct` | Total |
|---|---|---|---|
| Shellstyle | 24.4% (~105 ns) | 3.7% (~16 ns) | 28.1% |
| Regexp | 22.5% (~73 ns) | 5.5% (~18 ns) | 28.0% |
| Suffix | 15.7% (~2,739 ns) | 0.1% | 15.8% |

### Micro-benchmark validation (isolated take vs swap, constant 2-state workload)

| Iterations | `take` pattern | `swap` pattern | Speedup |
|---|---|---|---|
| 8 (regexp-like) | 134 ns | 33 ns | 4.1x |
| 12 (shellstyle-like) | 197 ns | 36 ns | 5.4x |
| 19 (suffix-like) | 291 ns | 42 ns | 7.0x |

### Fix applied

Replaced `take` with split-borrow destructuring (`let ArenaNfaBuffers { ref mut current_states, .. } = *bufs`)
+ `clear()` + `swap()`. This preserves Vec capacity across iterations — zero heap allocations
in the main loop when state count stays within initial capacity (16).

## Finding 2: Suffix Algorithm Mismatch — TODO

**Profile**: `traverse_arena_nfa` = 76.8% self time, 93.6% total time (post-fix: ~85%+ estimated).

### Algorithm comparison

| | Java (event-ruler) | Rust (quamina-rs) |
|---|---|---|
| Strategy | Reverse suffix, insert into DFA trie | Convert to shellstyle `*.ext50"`, use NFA |
| Match work | Walk value right-to-left through DFA, exit after ~7 bytes | Walk all 19 bytes through NFA, 2-3 states/byte |
| Complexity | O(suffix_len), DFA O(1) lookup | O(value_len * NFA_states), per-step overhead |

### Key code locations

- Java: `ByteMachine.java:611-636` — reverse-iterates value bytes through DFA, breaks early
- Rust: `mutable_matcher.rs:426-432` — `format!("*{}\"", s)` then `add_shellstyle_transition`

### Proposed fix

Implement reverse-and-prefix-trie: reverse suffix bytes, insert into a separate DFA arena,
match by iterating value bytes right-to-left through `traverse_arena_dfa` (already exists at
`arena.rs:557`). Expected result: ~150-300 ns (vs current 15,182 ns).

## Finding 3: Result Vec Allocation in `try_to_match_direct` — TODO

**Profile evidence**: 3.7-5.5% of total time in `grow_one` called from `try_to_match_direct`.

`FrozenValueMatcher::transition_on` (`thread_safe.rs:156`) returns a fresh `Vec<Arc<FrozenFieldMatcher>>`
per call. Go reuses `bufs.transitionsBuf` across calls.

### Proposed fix

Accept a `&mut Vec` or `&mut SmallVec` parameter instead of returning a new Vec. Reuse across calls.

## Unvalidated Hypotheses (not yet measurable with current tooling)

These were identified via source code comparison but could NOT be isolated or confirmed
with samply sampling profiler. They are subsumed within the ~26% self-time of
`traverse_arena_nfa` and would require either:
- Instruction-level profiling (e.g., `perf record -e cycles:pp` on Linux, or Instruments on macOS with sudo)
- Manual instrumentation with counters/timers in the hot loop
- Differential benchmarking (implement the fix, measure before/after)

### Spinout epsilon closure size (shellstyle/regexp)

Code reading suggests Rust's spinout state has `epsilon_closure = [self, continuation]` (2 entries)
while Go's spinner has `closure = [self]` (1 entry) because Go encodes the wildcard as byte-level
self-loop transitions via `makeByteDotFA`. This would double per-byte inner-loop iterations.
- Rust spinout: `arena.rs:2086-2101` — uses `spinout` field + epsilon to continuation
- Go spinner: `shell_style.go:47-82` — `makeByteDotFA(spinner, pp)` maps all bytes to self
- **Impact**: Unknown without isolating from other NFA loop work
- **Fix**: Restructure spinout to use Go-style byte-transition self-loop; remove `spinout` field

### Arc atomic refcounting in NFA traversal

Every field transition collection does `Arc::clone()` (atomic fetch-add) at `arena.rs:509`.
Go uses raw `*fieldMatcher` pointers with zero refcounting overhead.
- **Impact**: Estimated 15-20 ns per benchmark from code reading; not isolatable with sampling
- **Fix**: Store raw pointers during traversal, only Arc::clone when returning results

### RefCell borrow overhead in MutableValueMatcher

`mutable_matcher.rs:861-920` performs 6 separate `RefCell::borrow()` calls per `transition_on`.
Go does a single `atomic.Pointer.Load()` then direct field access.
- **Impact**: Estimated 5-8 ns; not profiled
- **Fix**: Only relevant for mutable matcher path (not the frozen/thread-safe hot path)

### FxHashSet for transition dedup

`ArenaNfaBuffers::clear` accounts for 1.5-1.7% of total time. The FxHashSet is used to
deduplicate field transitions via pointer address. For the common case of 0-1 transitions,
a SmallVec with linear scan would be cheaper.
- **Impact**: ~1.5-1.7% (measured via samply, minor)
- **Fix**: Replace FxHashSet with SmallVec + linear scan for small N
