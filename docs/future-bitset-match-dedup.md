# Future: Bitset-based match deduplication

**Status:** Explored — net neutral for current benchmarks
**Expected gain:** 5-15ns on `status_context_fields` (predicted); ~0ns measured
**Complexity:** ~100 lines in `thread_safe.rs`, moderate risk

---

## Problem

During matching, `FrozenFieldMatcher.matches: Vec<X>` stores pattern identifiers
(typically `String`). Each match site does `matches.add(m.clone())` — cloning a
String (heap alloc + memcpy) and comparing against previously seen matches. For the
common case of few matches this is fast (Step 5 switched to Vec linear dedup), but
the String clones at each match site remain.

## Idea

Assign each unique `X` a dense `u32` index at `add_pattern` time. Store indices
inside the matcher instead of `X` values. Dedup during matching becomes a bitset
operation — zero allocation, O(1) insert, no string comparison, no string cloning.

```rust
// In ThreadSafeCoreMatcher:
id_to_index: FxHashMap<X, u32>,
index_to_id: Vec<X>,

// In FrozenFieldMatcher:
matches: Vec<u32>,  // was Vec<X>

// In FrozenMatchSet (replaces current Vec-based dedup):
bits: u64,  // for ≤64 patterns; tier to Vec<u64> for more
```

## What would change

1. **`ThreadSafeCoreMatcher`** — Add bidirectional `X ↔ u32` mapping. Assign IDs
   in `add_pattern`. (~15 lines)

2. **`FrozenFieldMatcher.matches`** — `Vec<X>` → `Vec<u32>`. The `X` generic stays
   on the type (needed by transitions/exists maps) but matches become cheap indices.
   (~5 lines)

3. **`FrozenMatchSet`** — Replace with bitset. `u64` for ≤64 patterns, `Vec<u64>`
   for more. (~20 lines)

4. **`try_to_match*` methods** (~6 methods, ~18 call sites) — Change
   `matches.add(m.clone())` to `matches.set(idx)`. Eliminates all String clones
   during matching. (~18 lines)

5. **`matches_for_fields*` return paths** (~3 methods) — Translate `u32` indices
   back to `X` values using `index_to_id`. (~10 lines)

6. **Freeze code** — `freeze_field_matcher_impl` clones `Vec<u32>` instead of
   `Vec<X>`. (~2 lines)

7. **`MutableFieldMatcher`** in `mutable_matcher.rs` — `matches: RefCell<Vec<X>>`
   → `RefCell<Vec<u32>>`, `add_match` takes `u32`. Cascades to ~15 call sites in
   the mutable matcher's own matching methods. (~20 lines)

**Total:** ~90 lines across `thread_safe.rs` and `mutable_matcher.rs`.

## Tradeoffs

**Wins:**
- Zero-allocation dedup (bitset on stack for ≤64 patterns)
- No string comparison during matching
- No `m.clone()` at 18 match-site call sites — this is the main gain
- O(1) insert and membership test

**Costs:**
- Adds lookup indirection at the return boundary (`index_to_id[idx].clone()`)
- Adds complexity to `add_pattern` (ID assignment)
- `id_to_index` map grows monotonically (deleted patterns keep their index)
- For >64 patterns, need heap-backed `Vec<u64>` (still much smaller than before)
- Moderate risk: touches the hottest code path and the most correctness-critical
  matching logic (try_to_match recursion, exists-false, array-trail conflicts)

## Exploration results

A full implementation was built and benchmarked (see branch
`claude/explore-bitset-dedup-yoHkY`). The changes only touch the frozen
(thread-safe) matching path — `MutableFieldMatcher` and `CoreMatcher` are
unchanged. Key design choices:

- `FrozenRoot` wrapper bundles the root `FrozenFieldMatcher` and `index_to_id`
  into a single `ArcSwap` load (eliminates double-ArcSwap overhead)
- `try_to_match*` signatures changed from `&Arc<FrozenFieldMatcher<X>>` to
  `&FrozenFieldMatcher<X>` to remove pointer indirection
- `PhantomData<X>` added to `FrozenFieldMatcher` since `X` is no longer used
  non-recursively after `matches: Vec<X>` → `Vec<u32>`

### Benchmark results (noisy VM, back-to-back A/B runs)

| Benchmark (X=String) | Main | Bitset | Delta |
|---|---|---|---|
| status_context_fields | 472ns | 479ns | +7ns (~0) |
| match_only_context | 243-249ns | 252-260ns | +6-11ns |
| 100_patterns | 218ns | 220ns | ~0ns |
| 100_patterns_no_match | 141ns | 130ns | **-11ns** |

| Benchmark (X=usize) | Main | Bitset | Delta |
|---|---|---|---|
| 10k_patterns_1_match | 229-242ns | 308-351ns | **+70-80ns** |
| 10k_patterns_no_match | 127ns | 143ns | +16ns |

### Why it's neutral (not the expected win)

The predicted gain assumed eliminating clone overhead at match sites. In reality:

1. **Total clone count is unchanged for single-match.** The bitset moves 1
   `X::clone()` from the match site to the return boundary — it doesn't
   eliminate it. The net gain is zero for the common 1-match case.

2. **Bitset overhead offsets dedup savings.** The `FrozenRoot` wrapper, bitset
   iteration (`trailing_zeros` loop), and `index_to_id` lookup add ~5-10ns of
   overhead that roughly cancels any savings from avoiding `Vec::contains`.

3. **Cheap-to-clone X types regress.** When `X = usize`, `clone()` is a
   register copy. The bitset + index translation is strictly slower. The
   `10k_patterns` benchmark (usize) regresses ~70ns.

4. **Benefit requires high dedup ratio.** The bitset wins when N match-site
   visits produce D << N unique matches (eliminating N-D clones). Current
   benchmarks are single-match or low-match — the dedup ratio is ~1.

### Worth revisiting if

- A benchmark reveals **multi-match dedup-heavy** workloads (e.g., 50+ patterns
  matching the same event with many duplicate visits)
- Rust gains specialization, allowing the bitset path only for expensive-clone
  types
- The dominant X type becomes something with truly expensive clone (e.g.,
  `String` with long pattern IDs)
