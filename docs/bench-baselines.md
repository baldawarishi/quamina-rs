# Benchmark Baselines

Recorded on M3 Max, commit 78cac82.

## matching bench (cargo bench --bench matching)

| Benchmark | Time (ns) |
|-----------|-----------|
| exact_match | 127 |
| nested_match | 177 |
| regex_match | 100 |
| has_matches_early_exit | 208 |
| flatten_context_fields | ~156 |
| shellstyle_26_patterns | 513 |

## 10k benchmarks (cargo bench --bench matching -- "10k_")

| Benchmark | Time (ns) |
|-----------|-----------|
| 10k_patterns_1_match | 179 |
| 10k_patterns_no_match | 73 |
| 10k_diverse_patterns_1_match | 152 |
| 10k_mixed_exact_match | 172 |
| 10k_mixed_prefix_match | 171 |
| 10k_mixed_numeric_match | 179 |

## Optimization Log

### Flatten Epsilon Targets (Go PR #486) - one-level

**Change:** During arena NFA merges, inline epsilon targets of epsilon-only
splice states one level deep, reducing nesting from repeated merges.

**Approach tested:**
1. Full recursive flatten: -6% on small patterns, but +5-9% REGRESSION on 10k
   patterns due to huge SmallVec spill and poor cache locality.
2. One-level flatten (shipped): -3% to -7% across ALL benchmarks. No regressions.

**Clean A/B results (criterion, same session):**

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| exact_match | 127 ns | 121 ns | **-3.5%** |
| nested_match | 177 ns | 171 ns | **-5.6%** |
| shellstyle_26 | 513 ns | 489 ns | **-6.6%** |
| 10k_1_match | 179 ns | 167 ns | **-6.1%** |
| 10k_no_match | 73 ns | 71 ns | **-3.5%** |
| 10k_exact | 172 ns | 163 ns | **-5.3%** |
| 10k_prefix | 171 ns | 161 ns | **-4.9%** |
| 10k_numeric | 179 ns | 175 ns | **-3.5%** |

### String/Number Type Distinction Fix (forField removal)

**Change:** Keep JSON quotes on string values as implicit type tags. Automaton
now processes 2 extra bytes (surrounding quotes) per string value. Regexp NFAs
gain 2 states (leading/trailing quote transitions). CIDR builders gain 2 states.

**Expected cost:** +2 bytes per value comparison. This is the unavoidable cost
of fixing the correctness bug where string `"123"` matched number `123`.

**Results (same machine, single-run spot check):**

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| exact_match | 121 ns | 126 ns | +4.1% |
| nested_match | 171 ns | 172 ns | +0.6% |
| regex_match | 100 ns | 119 ns | +19% |
| shellstyle_26 | 489 ns | 537 ns | +9.8% |
| 10k_1_match | 167 ns | 177 ns | +6.0% |
| 10k_no_match | 71 ns | 77 ns | +8.5% |
| 10k_exact | 163 ns | 167 ns | +2.5% |
| 10k_prefix | 161 ns | 172 ns | +6.8% |
| 10k_numeric | 175 ns | 180 ns | +2.9% |

Note: These are single-run numbers, not A/B criterion comparisons. The regex
and shellstyle regressions reflect 2 extra NFA state transitions per match.
Numbers and booleans are unaffected (no quotes added).

### Precomputed Epsilon Closures (Go PR #482)

**Change:** Precompute epsilon closures at build time (after merge or FA
construction) and store them on each `ArenaFaState`. During NFA traversal,
read the precomputed closure directly instead of running a DFS with SparseSet
on every state for every byte.

Go PR #482 introduced this as "Precomputed epsilon closures" — the initial
assessment focused on the generation counter (vs Rust's SparseSet) and
missed the larger architectural difference: Go precomputes closures at build
time while Rust was computing them dynamically at match time.

**Implementation:** Added `epsilon_closure: SmallVec<[StateId; 4]>` to
`ArenaFaState`. Called `precompute_epsilon_closures()` after `merge_arena_nfas`,
after `clone_arena_subset`, and when first setting `main_arena`. Fallback to
dynamic `fill_epsilon_closure()` for arenas without precomputation.

**Results (same machine, single-run):**

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| shellstyle_26 | 549 ns | 512 ns | **-6.7%** |
| shellstyle_multi | 97.2 µs | 84.5 µs | **-13.1%** |
| regexp_plus_short | 402 ns | 374 ns | **-7.0%** |
| regexp_plus_long | 3.510 µs | 3.073 µs | **-12.4%** |
| regexp_star_long | 3.574 µs | 3.063 µs | **-14.3%** |
| regexp_dot_star | 782 ns | 693 ns | **-11.4%** |
| arena_nfa_100chars | 3.141 µs | 2.688 µs | **-14.4%** |
| arena_nfa_5chars | 217 ns | 196 ns | **-9.7%** |
| accel_suffix_10k | 257 µs | 233 µs | **-9.3%** |
| accel_suffix_1k | 26.1 µs | 23.4 µs | **-10.3%** |
| accel_suffix_short | 423 ns | 399 ns | **-5.7%** |
| regexp_negated_short | 138 ns | 141 ns | +2.2% (noise) |
| regexp_negated_long | 187 ns | 187 ns | 0% |
| regexp_negated_1k | 682 ns | 686 ns | +0.6% (noise) |
| exact_match (DFA) | 124 ns | 121 ns | -2.4% (noise) |
| nested_match (DFA) | 178 ns | 179 ns | +0.6% (noise) |
| 10k_1_match (DFA) | 183 ns | 181 ns | -1.1% (noise) |

NFA-heavy benchmarks: **-6% to -14%** improvement. DFA benchmarks: no
regression. regexp_negated benchmarks flat because memchr acceleration
dominates (epsilon closure is only computed at transition boundaries, not
during the memchr-skipped middle section).
