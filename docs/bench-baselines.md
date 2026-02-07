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
