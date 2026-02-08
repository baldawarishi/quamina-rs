# Future: Bitset-based match deduplication

**Status:** Proposed
**Expected gain:** 5-15ns on `status_context_fields`
**Complexity:** ~90 lines across 2 files, moderate risk

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

## Why not now

Steps 2-5 already closed the gap: 420ns → 272ns (−35%), now 29% faster than Go's
382ns. The remaining gain from bitset dedup (~5-15ns) doesn't justify the risk of
changing the core data flow. Worth revisiting if:
- A new benchmark reveals match-heavy workloads where clone overhead matters
- The codebase needs a larger refactor that touches these types anyway
- Someone wants to push below 260ns
