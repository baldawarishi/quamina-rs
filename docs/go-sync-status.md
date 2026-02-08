# Go Sync Status

Last synced: Go commit `e3d13cd` (Jan 2026)

## Feature Parity (as of Feb 2026)

Rust is **ahead** of Go on features:

| Feature | Go | Rust | Notes |
|---------|:--:|:----:|-------|
| Exact match | Yes | Yes | |
| Prefix | Yes | Yes | |
| Suffix | No | Yes | Rust-only |
| Wildcard | Yes | Yes | |
| Shellstyle | Yes | Yes | |
| Exists | Yes | Yes | |
| Anything-but (strings) | Yes | Yes | |
| Anything-but (numbers) | No | Yes | Rust-only |
| Equals-ignore-case | Yes | Yes | |
| Numeric range (`>=`,`<`,etc) | No | Yes | Rust-only |
| CIDR IP matching | No | Yes | Rust-only |
| Regexp (I-Regexp RFC 9485) | Yes | Yes | |
| Regexp lookarounds | No | Yes | Rust-only |
| Pattern deletion/rebuild | Yes | Yes | |
| SegmentsTree field skip | Yes | Yes | |
| Custom flattener | Yes | Yes | |

## Go Commits Since Last Sync

Checked: Feb 2026. ~20 non-merge commits since `e3d13cd`, all optimizations (no new features).

### 1. Epsilon Closure Refactoring (PR #482)

**Go change:** Precomputed epsilon closures at build time. Each state stores its closure (`state.epsilonClosure`) so match-time traversal just iterates a slice instead of running DFS. Generation counter on `smallTable.lastVisitedGen` optimizes the build-time computation.

**Rust status:** PORTED. Added `epsilon_closure: SmallVec<[StateId; 4]>` to `ArenaFaState`. Closures precomputed after merges and FA construction via `precompute_epsilon_closures()`. Match-time `traverse_arena_nfa` reads precomputed closures directly, eliminating per-byte DFS + SparseSet overhead. Generation counter not needed (SparseSet with O(1) clear used for build-time computation).

### 2. Flatten Epsilon Targets (PR #486)

**Go change:** When merging two FAs with epsilon transitions, recursively flatten splice chains so epsilons point directly to real states instead of nested splice-to-splice chains. Prevents O(depth^2) nesting from repeated merges.

**Rust status:** PORT CANDIDATE. The arena merge code creates 2-epsilon splice states without flattening. On repeated merges this can accumulate depth. Straightforward to add.

**Impact:** -10% memory on deeply-merged patterns, +1-3% match-time perf (fewer epsilon states to traverse).

### 3. Cache startState in nfaBuffers (PR #490)

**Go change:** Cache `*faState` and `[]*faState` closure slice in `nfaBuffers` to avoid one heap allocation per `traverseNFA` call.

**Rust status:** SKIP. Marginal benefit due to Arc semantics. Arena code uses `StateId` (u32, no allocation). Non-arena creates one `Arc<FaState>` per traversal which is cheap.

### 4. SkinnyRuneTree Memory Optimization (PR #483)

**Go change:** Replaced 256-entry arrays with parallel `byteVals`/`entries` arrays for Unicode property FA construction. Added caching via `cachedFaShells` map.

**Rust status:** PORTED (FA caching only). Rust uses range-based tree building (`add_arena_rune_pair_tree_entry`) rather than Go's per-codepoint iteration, so the skinny tree structure optimization is unnecessary. However, the FA shell caching is ported: a thread-local `CachedShell` cache stores pre-built FA shells for Unicode property categories (`~p{L}`, `~p{Nd}`, etc.) and XML name char escapes (`~i`, `~I`, `~c`, `~C`). On repeated use, shells are instantiated by cloning and remapping state IDs instead of rebuilding from scratch.

### 5. forField Removal + String/Number Type Distinction Fix

**Go change:** Removed always-true `forField` boolean parameter from `makeRegexpNFA` and related functions. Go keeps JSON quotes on string values so the automaton naturally distinguishes strings from numbers.

**Rust status:** PORTED. Rust had `for_field` parameter (always `false`) because `value_bytes()` stripped quotes. This caused a correctness bug: string `"123"` matched number `123` since their bytes were identical after stripping. Fix: stopped stripping quotes in `value_bytes()`, wrapped string pattern values in quotes in `value_to_string()`, updated all matchers (prefix/shellstyle/wildcard/etc.) and CIDR builders to handle quotes, then removed `for_field` parameter (hardcoded `true`).

## Porting Tracker

Investigate each Go optimization empirically - one per session. Measure before/after.

| # | Optimization | Initial Assessment | Empirical Result | Session |
|---|---|---|---|---|
| 1 | Flatten Epsilon Targets (#486) | PORT CANDIDATE | PORTED (one-level), -3% to -7% | Feb 2026 |
| 2 | Epsilon Closure Refactoring (#482) | SKIP (arena already has SparseSet) | PORTED: precomputed closures at build time, -6% to -14% on NFA benchmarks | Feb 2026 |
| 3 | Cache startState (#490) | SKIP (marginal in Rust) | SKIP confirmed: StateId=u32 (no alloc), closure computed in-place in reusable Vec | Feb 2026 |
| 4 | SkinnyRuneTree (#483) | DEFER (no Unicode prop automata) | PORTED (FA caching only; skinny tree structure not needed — Rust uses range-based tree building) | Feb 2026 |
| 5 | forField Removal | N/A (different design) | PORTED: removed `for_field` param, fixed string/number type distinction bug | Feb 2026 |

---

## Behavioral Differences from Go

1. `{"anything-but": "foo"}` - Rust accepts single string, Go requires array
2. Flattener stops parsing once all pattern fields found (optimization)
