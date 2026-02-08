# Plan: Close the "early field match" performance gap

**Goal:** Make Rust's `status_context_fields` benchmark faster than Go's (~382ns).
Currently Rust is ~398ns — a 16ns / 4% gap. See `docs/profile-early-field-gap.md` for
the full profiling analysis.

**Branch:** `perf/close-early-field-gap` (create from `main`)
**Approach:** Incremental commits, push often, run CI + benchmarks after each step.

---

## Session prompt

```
Close the early-field-match performance gap between Go and Rust quamina.

Rust: ~/workspaces/quamina-rs
Go: ~/workspaces/quamina

Read the plan at docs/plan-close-early-field-gap.md — it tracks progress and has
all context. Read Rust source directly; don't trust other docs as they may be stale.
For Go behavior, read Go source directly.

Approach: work on branch perf/close-early-field-gap. Push & commit often, check CI.
Use todos and sub-agents to manage context. Algorithmic parity and performance matter.
Update the plan doc with progress after each step.
```

---

## Step 1: Branch setup and baseline

- [ ] Create branch `perf/close-early-field-gap` from `main`
- [ ] Commit the profiling scaffolding from the current session (benchmarks, accessors,
  profile example, Cargo.toml debug symbols, profile doc)
- [ ] Run baseline: `cargo bench --bench matching -- status_context_fields`
- [ ] Record baseline numbers below

**Baseline (2026-02-08):**
```
status_context_fields          = 420 ns
status_context_fields_no_mutex = 385 ns
flatten_direct_context_fields  = 155 ns
flatten_sort_context_fields    = 161 ns
match_only_context_fields      = 232 ns
```

---

## Step 2: Eliminate `from_utf8` in SegmentsTree (~10-15ns expected)

**What:** The 4 lookup methods in `src/segments_tree.rs` call `std::str::from_utf8(segment)`
to convert `&[u8]` -> `&str` for `FxHashMap<String>` lookup. The flattener guarantees
field names are valid UTF-8 (rejects non-UTF-8 in `read_member_name`). Profile shows
24/234 samples (10%) in `from_utf8`.

**Change:** Replace `std::str::from_utf8(segment)` with
`unsafe { std::str::from_utf8_unchecked(segment) }` at 4 call sites (lines 104, 114,
122, 131). Same pattern already used in `flatten_json.rs:57` (`Field::path_str()`).

**Safety argument:** JSON spec (RFC 8259) requires field names to be valid UTF-8. The
flattener's `read_member_name` validates this. `SegmentsTree::add()` takes `&str`, so
all map keys are guaranteed valid. The only question is the lookup input, which always
comes from the JSON parser.

**Files:** `src/segments_tree.rs`

- [x] Make the 4 changes
- [x] `cargo test` — all 442 tests pass
- [x] `cargo bench --bench matching -- status_context_fields` — recorded improvement
- [x] Commit & push (b7af159)

**Result:** `status_context_fields` = 387 ns (delta: −33 ns / −8.5%)
**No-mutex:** `status_context_fields_no_mutex` = 379 ns (delta: −6 ns / −1.6%)

---

## Step 3: Switch FrozenFieldMatcher maps to FxHashMap (~5-8ns expected)

**What:** `FrozenFieldMatcher` has 3 maps using `std::collections::HashMap` (SipHash):
`transitions`, `exists_true`, `exists_false` (`src/automaton/thread_safe.rs:68,72,74`).
`FrozenMatchSet.seen` uses `std::collections::HashSet` (SipHash) (line 784).
Profile shows 26/234 samples (11%) in SipHash operations.

These maps are built at pattern-add time and read-only during matching — no HashDoS risk.
The `SegmentsTree` already uses `FxHashMap` for the same reason.

**Change:**
1. In `FrozenFieldMatcher`: `HashMap` -> `FxHashMap` for `transitions`, `exists_true`,
   `exists_false`
2. In `FrozenMatchSet`: `HashSet` -> `FxHashSet` for `seen`
3. Update all construction sites (`HashMap::new()` -> `FxHashMap::default()`, etc.)
4. The `HashMap` at line 827 (`pattern_map` in `AutomatonValueMatcher`) is also read-only
   during matching — consider switching too, but verify it doesn't affect correctness
5. Leave the `HashSet` at line 894 (`seen_ids` in `collect_exists_false_matches`) — it's
   a cold path

**Files:** `src/automaton/thread_safe.rs`

- [x] Make the changes
- [x] `cargo test` — all 442 tests pass
- [x] `cargo bench --bench matching -- status_context_fields` — recorded improvement
- [x] Commit & push (e0c89dc)

**Result:** `status_context_fields` = 329 ns (delta: −58 ns / −15% from Step 2)
**No-mutex:** `status_context_fields_no_mutex` = 322 ns (delta: −57 ns / −15% from Step 2)
**Cumulative:** 420 → 329 ns (−91 ns / −22%, now 14% faster than Go's 382 ns)

---

## Step 4: Thread-local flattener and NFA buffers (~14-16ns expected)

**What:** `Quamina::matches_for_event` acquires two `parking_lot::Mutex` locks per call
(`src/lib.rs:445,453`). Benchmark shows 18ns overhead (398ns with mutex vs 380ns without).
Go avoids this because `Quamina` is not thread-safe — it stores flattener/bufs directly.

**Change:** Replace `Mutex<FlattenJsonState>` and `Mutex<NfaBuffers>` with `thread_local!`
storage inside `matches_for_event`. Keep Mutex fields on the struct for `Copy()` (which
creates fresh instances anyway).

**Design:**
```rust
thread_local! {
    static FLATTENER: RefCell<FlattenJsonState> = RefCell::new(FlattenJsonState::new());
    static NFA_BUFS: RefCell<NfaBuffers> = RefCell::new(NfaBuffers::new());
}
```

**Key considerations:**
- The flattener's `flatten()` returns `&mut [Field<'a>]` borrowing the flattener.
  The `RefCell::borrow_mut()` scope must encompass flatten + sort + match (same as
  current Mutex scope). Use `FLATTENER.with(|cell| { ... })` wrapping the whole body.
- `Copy()` already creates fresh `FlattenJsonState::new()` and `NfaBuffers::new()` —
  no change needed there.
- Keep the `Mutex` fields for backward compat but mark them `#[allow(dead_code)]` or
  remove if nothing else uses them. Actually, check if `flatten_only()` or other
  benchmark helpers use them — if so, switch those too.
- The custom flattener path (`custom_flattener: Option<Mutex<...>>`) stays as-is since
  it's already a separate code path.

**Tricky bit:** If `matches_for_event` is called recursively (shouldn't happen), `RefCell`
panics vs Mutex deadlock. Arguably better. If this is a concern, use `try_borrow_mut()`
and fall back to allocating fresh state.

**Files:** `src/lib.rs`

- [x] Implement thread_local approach
- [x] `cargo test` — all 442 tests pass (including concurrent tests)
- [x] `cargo bench --bench matching -- status_context_fields` — recorded improvement
- [x] Run the full benchmark suite to check for regressions — no regressions
- [x] Commit & push (08490a4)

**Result:** `status_context_fields` = 328 ns (mutex overhead eliminated)
**No-mutex:** `status_context_fields_no_mutex` = 329 ns (now equal — confirms Mutex is gone)
**Note:** Delta vs Step 3 is within noise (~329→328ns). The real win is closing the
gap between the mutex and no-mutex variants, confirming Mutex overhead is eliminated.

---

## Step 5: Reuse FrozenMatchSet across calls (~2-3ns expected, optional)

**What:** `matches_for_fields_direct` creates `FrozenMatchSet::new()` per call
(`src/automaton/thread_safe.rs:698`). Go reuses its `matchSet` via `clear()`.

**Change:** Move `FrozenMatchSet` fields (the HashSet + Vec, now FxHashSet after Step 3)
into `NfaBuffers`. Clear them per call. This avoids making `NfaBuffers` generic over `X`
by storing `FxHashSet<X>` + `Vec<X>` behind a type-erased wrapper, OR by simply making
the match-set a separate thread-local / passed-in buffer.

**Decision point:** If Steps 2-4 already close the gap sufficiently, skip this.
The complexity of making `NfaBuffers` generic may not be worth 2-3ns.

- [ ] Decide: implement or skip
- [ ] If implementing: make changes, test, bench, commit & push

**Result:** `status_context_fields` = ___ ns (delta: ___ ns) OR skipped

---

## Step 6: Final benchmark and comparison

- [ ] Run full decomposed benchmarks:
  ```
  cargo bench --bench matching -- 'flatten_direct_context|flatten_sort_context|status_context_fields|match_only_context'
  ```
- [ ] Run Go comparison:
  ```
  cd ~/workspaces/quamina && go test -bench='Benchmark_JsonFlattner_Evaluate_ContextFields|Benchmark_JsonFlattener_ContextFields' -benchtime=5s -benchmem
  ```
- [ ] Fill in comparison table:

| Metric | Rust (before) | Rust (after) | Go | Winner |
|--------|--------------|-------------|-----|--------|
| Full path | 398 ns | ___ ns | 382 ns | |
| Flatten only | 151 ns | ___ ns | 140 ns | |
| Match only | 227 ns | ___ ns | ~242 ns | |

- [ ] Run full test suite: `cargo test`
- [ ] Run full benchmark suite to check for regressions: `cargo bench --bench matching`
- [ ] Update `docs/profile-early-field-gap.md` with final numbers

---

## Step 7: Cleanup

Decide what to keep vs remove from the profiling scaffolding:

**Keep permanently:**
- [ ] Decomposed benchmarks in `benches/matching.rs` (flatten_sort, no_mutex, match_only)
  — useful for ongoing perf regression detection
- [ ] `docs/profile-early-field-gap.md` — reference for future profiling
- [ ] This plan doc (as historical record, mark completed)

**Decide:**
- [ ] `Quamina::automaton()` and `segments_tree()` accessors in `src/lib.rs` — needed by
  benchmarks. Keep as `#[doc(hidden)]` or remove if benchmarks are restructured.
- [ ] `examples/profile_status.rs` — useful for future flamegraph sessions. Keep if small.
- [ ] `Cargo.toml` `[profile.bench]` / `[profile.release]` `debug = 1` — useful for
  profiling but adds ~5% to binary size. Keep or gate behind a feature.
- [ ] `profile_status.json`, `sample_output.txt` — ephemeral profiling artifacts. Delete.

**Remove:**
- [ ] Any `#[allow(dead_code)]` annotations added during refactoring
- [ ] Any TODO comments added during implementation

Final actions:
- [ ] `cargo test && cargo clippy`
- [ ] Commit cleanup
- [ ] Open PR to merge `perf/close-early-field-gap` into `main`
- [ ] Mark this plan as COMPLETED

---

## Progress log

Update this section as each step is completed.

| Step | Status | Date | Notes |
|------|--------|------|-------|
| 1. Branch + baseline | Done | 2026-02-08 | 420ns baseline (machine slightly higher than original 398ns measurement) |
| 2. from_utf8_unchecked | Done | 2026-02-08 | 420→387ns (−33ns/−8.5%), commit b7af159 |
| 3. FxHashMap | Done | 2026-02-08 | 387→329ns (−58ns/−15%), commit e0c89dc. Cumulative: 420→329ns (−22%) |
| 4. thread_local | Done | 2026-02-08 | Mutex overhead eliminated, ~328ns ≈ no_mutex ~329ns, commit 08490a4 |
| 5. Reuse match-set | Not started | | |
| 6. Final benchmark | Not started | | |
| 7. Cleanup | Not started | | |
