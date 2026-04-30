# Lazy flatten architecture: Plan & Progress Tracker

> Created: 2026-04-29
> Meta-plan: `~/.claude/plans/create-a-plane-to-squishy-pearl.md` (design intent — does not update)
> This doc: live tracker. Update after every step (what changed, bench delta, commit SHA, ≤10 lines).
> Companion: `docs/simd-skip-block-plan.md` (style reference), `docs/simd-skip-string-continuation.md`.

---

## Problem statement

Two structural inefficiencies remain after Phase 2 of the SIMD branch (`perf/simd-flatten-json`,
through `f81b435`):

1. **Re-scan on entry into walked objects.** `read_member_name`, `read_string_value`,
   `skip_block` each kick off a fresh SIMD scan from `self.index`. A walked object with
   N string members re-scans bytes ~N times when one structural pass would suffice.
2. **Eager escape materialization.** `read_string_value` (`src/flatten_json.rs:833`) allocates
   `FieldValue::Owned(Vec<u8>)` on `\`. The matcher only inspects value bytes inside
   `FrozenValueMatcher::transition_on` (`src/automaton/thread_safe.rs:182-186`). Values whose
   path is in `segments_tree` but doesn't transition through any active state still pay.

Both phases are contained — no matcher API rewrite required.

### Out of scope

- Full simdjson stage-1/stage-2 (gives up skip_block efficiency on subtree-skipping patterns).
- Matcher-side On-Demand pull-by-path — `try_to_match_direct` (`src/automaton/thread_safe.rs:996-998`)
  is a combinatorial walk over the full sorted field slice with no per-pattern early exit.
- Adaptive event-size tiering (small/medium/large).
- `MemberName::EscapedRaw` — names hit the hashmap immediately; lazy is no win.

---

## Phase 1 — Per-object structural pre-scan

**Goal.** Entering a walked object body, do **one** SIMD pass that records every member-name
boundary and the matching `}`. Subsequent reads inside the object consume offsets instead
of re-scanning.

### Hook & reuse points

| Element | Location |
|---|---|
| Entry hook | `src/flatten_json.rs:256` (`read_object`) and recursive call at `:408` |
| Existing kernel to extend | `flatten_json_simd::scan_block` (call sites `:631`, `:649`) |
| State pool | `FlattenJsonState` (`src/flatten_json.rs:117-189`); reset at `:146-150` |
| Padded-tail fallback model | `skip_block` zero-padded buffer (`:638-655`) |
| Member-name scanner today | `scan_delim` at `src/flatten_json.rs:718` |
| String-value scanner today | `scan_delim` at `src/flatten_json.rs:839` |
| Tree lookup (per-member) | `:300-305` → `src/segments_tree.rs:205` |

### Steps

| # | Action | Kill criteria |
|---|--------|---------------|
| 0 | Capture baseline: `cargo run --release --example profile_status`, `…profile_array_heavy`, `cargo bench --bench matching -- flatten citylots status_`. Record numbers below. Commit `examples/profile_array_heavy.rs` (currently untracked). | n/a |
| 1 | Extend `flatten_json_simd` with `scan_object_index(data, start) -> (Option<usize>, ScanIndex, carry…)` — same `run_scan` core, plus `&mut Vec<u32>` capturing structural offsets (quotes outside strings, opens/closes ≥ level 1, matching close). Reuse `Backend`, `find_escaped`, `prefix_xor`. Gated off (no callers yet). | `profile_array_heavy` regresses >5%. |
| 2 | Add pooled `obj_index_buf: Vec<u32>` to `FlattenJsonState`. Extend `reset()` (`:146-150`) capacity-preserving. Nested objects: stack discipline (push len-marker on recurse, restore on return). | `dhat` allocation count on `bench_citylots_core` increases. |
| 3 | At `read_object` top (`:256`), gate on `remaining_bytes >= 256` (matches `BLOCK_SIMD_THRESHOLD` reasoning). Above gate: `scan_object_index`. Below gate: existing per-member SIMD path. | `flatten_status_*` regresses >2%. |
| 4 | Indexed read variants: `read_member_name_indexed(end_pos)`, `read_string_value_indexed(end_pos)`, `skip_block_indexed(close_pos)`. Existing entry points stay for sub-threshold + outer-object paths. Escape paths fall back as today. | `cargo +nightly fuzz run fuzz_flatten_json -- -max_total_time=200` finds a crash. |
| 5 | **User bench gate.** `cargo bench --bench matching -- flatten citylots status_`. Record deltas. Stop and consult before tuning. | citylots regresses >2%, OR `flatten_context_fields` regresses, OR any `status_` >3%. |

### Failure modes pre-identified

- **Backslash carry across pre-scan ↔ indexed-read boundary** — reuse `scan_string`'s `init_odd_bs`
  protocol; thread the `u64` through.
- **Sub-chunk tails (object body <64B)** — Step 3 size gate handles via per-member fallback.
- **Truncation** — `scan_object_index` returns `None` for matching `}` → propagate as existing
  `truncated block` error.
- **Escape-heavy strings** — pre-scan emits offset of `\`; indexed read sees this and falls
  through to scalar `*_with_escapes` exactly as today.

### Expected win (honest)

5–15% on `flatten_only` over walk-heavy events with multi-member short-string objects
(status.json, citylots `properties`). Possibly flat or slight regression on tiny events
from the gate-check; possibly worse on skip-heavy inputs if gate is wrong. Track
`flatten_status_*` as the small-event canary.

### Commit shape (target 3–5 commits)

1. `perf(flatten_json_simd): scan_object_index kernel`
2. `perf(flatten_json): pool obj_index_buf in FlattenJsonState`
3. `perf(flatten_json): per-object pre-scan in read_object`
4. `perf(flatten_json): indexed read_*/skip_* variants`
5. `docs(flatten): record Phase 1 results`

### Progress

| # | Status | Commit | Bench delta | Notes |
|---|--------|--------|-------------|-------|
| 0. Baseline + profile_array_heavy commit | ✅ Done | `8fb46cf` | — | Apple Silicon M-series; tree at `f81b435`. |
| 1. `scan_object_index` kernel | ✅ Done | `1b89de2` | array_heavy 2571→2533 ns (-1.5%, within noise) | Kernel + 4 backends + dispatchers + 7 unit tests. No callers yet. |
| 2. Pool `obj_index_buf` | ✅ Done | `f588b72` | — | Field + reset; `#[allow(dead_code)]` until Step 3 wires it. |
| 3. Pre-scan in `read_object` (gated) | ⬜ **Resume here** | — | — | — |
| 4. Indexed read/skip variants | ⬜ Todo | — | — | — |
| 5. Bench gate + tuning | ⬜ Todo | — | — | — |

#### Baseline (Step 0, 2026-04-29, branch `perf/simd-flatten-json` at `f81b435`)

`profile_status` (status.json, single pattern):

| metric | ns/op |
|---|---|
| flatten_only | 1838 |
| match_only | 198 |
| full_pipeline | 2101 |

Flatten = 87% of pipeline.

`profile_array_heavy` (event 904 B, 100-element string array, current pinned
`STRING_SIMD_THRESHOLD=BLOCK_SIMD_THRESHOLD=0`):

- string-gate sweep at threshold 0: **2571 ns/call** (best in sweep: 2524 @ 32, -1.8%)
- block-gate sweep at threshold 0: **2586 ns/call** (best in sweep: 2556 @ 64, -1.1%)

Sweep is flat ±1.5% across the threshold range — current pin is fine; this is the
walk-heavy-array canary for Phase 1.

`cargo bench --bench matching -- 'flatten|citylots|status_'`:

| bench | time |
|---|---|
| flatten_context_fields | 84.23 ns |
| flatten_direct_context_fields | 83.78 ns |
| flatten_sort_context_fields | 92.23 ns |
| flatten_middle_nested | 1.719 µs |
| flatten_last_field | 1.035 µs |
| status_context_fields | 204.55 ns |
| status_middle_nested | 1.779 µs |
| status_last_field | 1.078 µs |
| status_all_three_patterns | 2.090 µs |
| citylots | 1.680 µs |
| citylots_core | 280.30 ns |

Criterion `change` vs prior runs is informational only — Phase 1 deltas are computed
against these absolute numbers.

#### Step 1 — `scan_object_index` kernel

Added `run_scan_object_index<B: Backend>` to `flatten_json_simd.rs` plus per-backend
`scan_object_index` wrappers (NEON, AVX2, SSE4.2, scalar), an `OnceLock`-cached
x86_64 dispatcher, and the public entry `scan_object_index(data, start, &mut Vec<u32>,
&mut depth, init_in_str, init_odd_bs)`. Reuses `find_escaped` + `prefix_xor`; uses 6
`cmp_mask` calls (`"`, `\`, `{`, `}`, `[`, `]`) with the same string-mask gating as
`run_scan`. Emits depth-1 quote/open/close offsets only — depth ≥2 internals are
skipped (consumer recurses or calls `skip_block_indexed`). Carry state mirrors
`scan_block` so a padded-tail re-scan resumes seamlessly.

Verified: 7 new unit tests cover empty body, single member, escaped quote, nested
object/array (depth-2 skip), brace inside string, and 64-byte chunk-boundary carry.
780 lib tests pass. `profile_array_heavy`: 2571 → 2533 ns/call (-1.5%, code-layout
noise; kernel has no callers yet — wired in at Step 3).

#### Step 2 — Pool `obj_index_buf`

Added `obj_index_buf: Vec<u32>` to `FlattenJsonState` (default empty), cleared in
`reset()` capacity-preserving alongside `array_trail` / `fields`. Field carries
`#[allow(dead_code)]` since Step 3 is the first caller; `cargo clippy -- -D warnings`
clean. 780 lib tests pass. Bench-neutral by construction (zero-byte pool initially;
allocation happens only when Step 3 starts pushing offsets). No re-baseline taken.

---

## Phase 2 — Lazy value materialization

**Goal.** Defer escape decoding + `FieldValue::Owned(Vec<u8>)` allocation from flatten time
to value-comparison time. Fields whose path never reaches `FrozenValueMatcher::transition_on`
skip the allocation entirely.

### Touch points

| Element | Location |
|---|---|
| `FieldValue` definition | `src/flatten_json.rs:80-109` |
| Producer (escape branch) | `src/flatten_json.rs:833` (`read_string_value`) → `read_string_with_escapes` |
| Consumer (decode trigger) | `src/automaton/thread_safe.rs:182-186` (path-lookup gate) and `:1033` (caller) |
| Thread-local pool | `src/lib.rs:46-50` (`NfaBuffers`) |

### Type change

```rust
enum FieldValue<'a> {
    Borrowed(&'a [u8]),         // unchanged: no escapes, raw slice
    Owned(Vec<u8>),             // unchanged: pre-decoded — kept for fallback
    EscapedRaw(&'a [u8]),       // NEW: borrowed slice with escapes, decode on demand
}
```

`MemberName` parallel `EscapedRaw` is **out of scope** — names hit the hashmap immediately.

### Steps

| # | Action | Kill criteria |
|---|--------|---------------|
| 0 | Capture pre-Phase-2 baseline (post-Phase-1 numbers) on the same bench set. | n/a |
| 1 | Extract escape-decoder body from `read_string_with_escapes` into `pub(crate) fn decode_json_escapes(raw: &[u8], scratch: &mut Vec<u8>) -> &[u8]`. No behavior change. | `cargo test --lib`. |
| 2 | Add `FieldValue::EscapedRaw(&'a [u8])`. Update exhaustive `match` sites (compiler-driven). Borrowed access unchanged; consumers needing decoded bytes call new helper. | Compile errors from missed arms. |
| 3 | Switch `read_string_value` (`:833`) escape path: return `EscapedRaw(raw_slice_between_quotes)` instead of `Owned`. Existing `Owned` path stays available for callers that need pre-decoded values. | `cargo test --lib` regression — audit and convert. |
| 4 | Add `decode_scratch: Vec<u8>` to `NfaBuffers` (`src/lib.rs:46-50`). Reset `clear()` per call alongside other buffers. | `dhat` allocation count increases. |
| 5 | At `FrozenValueMatcher::transition_on` call (`src/automaton/thread_safe.rs:1033`), wrap value extraction: on `EscapedRaw` call `decode_json_escapes(raw, &mut bufs.decode_scratch)` and pass scratch slice; on `Borrowed`/`Owned` pass directly. | Match correctness regression in `cargo test --lib` (escape-content patterns) or fuzz. |
| 6 | **User bench gate.** `cargo bench --bench matching -- citylots flatten_ status_`. Allocation profile: `samply` / `dhat` confirms `Owned` alloc dropped on escape-content workloads. Record deltas. | Wall-clock regression >1% on any flatten/match bench. |

### Expected win (honest)

Small. Two compounding limits: (1) most JSON strings have no escapes; (2) flatten already
filters via `segments_tree`, so most "never-consumed" paths aren't emitted. The remaining
intersection — escaped values whose path is in `segments_tree` but doesn't reach a
value-transition — is a real but small population. Plausible ≤2% on flatten/match benches;
primary value is the `EscapedRaw` variant unlocking future deeper-laziness work.

### Commit shape (target 2–3 commits)

1. `refactor(flatten_json): extract decode_json_escapes`
2. `perf(flatten_json): FieldValue::EscapedRaw + lazy decode at matcher`
3. `docs(flatten): record Phase 2 results`

### Progress

| # | Status | Commit | Bench delta | Notes |
|---|--------|--------|-------------|-------|
| 0. Pre-Phase-2 baseline | ⬜ Todo | — | — | — |
| 1. Extract `decode_json_escapes` | ⬜ Todo | — | — | — |
| 2. `FieldValue::EscapedRaw` variant | ⬜ Todo | — | — | — |
| 3. Switch escape path to `EscapedRaw` | ⬜ Todo | — | — | — |
| 4. `NfaBuffers::decode_scratch` | ⬜ Todo | — | — | — |
| 5. Wrap matcher caller for lazy decode | ⬜ Todo | — | — | — |
| 6. Bench + alloc-profile gate | ⬜ Todo | — | — | — |

#### Baseline (Step 0)

_To be filled at Phase 2 Step 0._

---

## Verification (per phase boundary)

```bash
# Bench (capture before and after each phase). Criterion takes a SINGLE regex
# filter — multi-arg space-separated lists silently fail with "unexpected
# argument".
cargo bench --bench matching -- 'flatten|citylots|status_'
cargo run --release --example profile_status
cargo run --release --example profile_array_heavy

# Correctness (after every step)
cargo test --lib
just check

# Per phase
cargo +nightly fuzz run fuzz_flatten_json -- -max_total_time=200
cargo +nightly-2026-01-26 miri test
```

### Miri considerations

- Phase 1: reuses existing dispatcher, no new intrinsic call sites — should be miri-neutral.
- Phase 2: zero SIMD touch — trivially miri-neutral.

If miri runtime grows >10% from baseline after a step, treat as a regression.

---

## To resume in a fresh session

**Current state (2026-04-29):** Phase 1 Steps 0–2 done. Pool field is in
place (`#[allow(dead_code)]`); `scan_object_index` kernel + 4 backends + 7 unit
tests landed; **no callers yet** — first wiring happens at Step 3.
780 lib tests pass, clippy + fmt clean.

1. Re-read in this order: this doc (top to bottom),
   `~/.claude/plans/create-a-plane-to-squishy-pearl.md` (design intent),
   `docs/simd-skip-block-plan.md` Phase 2 section (conventions for the per-step
   subsection style and the "stop at the bench gate" rule).
2. Confirm tree state — `git log --oneline -1` should show
   `1b89de2 perf(flatten_json_simd): scan_object_index kernel` (or a later
   commit if more steps landed). `cargo test --lib` clean.
3. **No re-baseline needed** for Step 2 (allocation-pool change; bench-neutral
   by construction). Re-baseline before Step 3 (first measurable step) by re-running
   the verification block above.
4. Pick up at the next ⬜ row.

### Step 3 entry points (resume here)

- **Caller hook:** `read_object` top at `src/flatten_json.rs:256` (and the
  recursive call at `:408`). At entry, gate on `remaining_bytes >= 256` (matches
  `BLOCK_SIMD_THRESHOLD` reasoning in `flatten_json_simd`). Above gate: call
  `scan_object_index(event, self.index, &mut state.obj_index_buf, …)`. Below
  gate: existing per-member SIMD path stays.
- **Recursion discipline:** one shared `obj_index_buf` across the whole flatten
  call. Before recursing into a nested object, snapshot `let mark =
  state.obj_index_buf.len();`; after the nested `read_object` returns,
  `state.obj_index_buf.truncate(mark);`. This keeps offsets contiguous without
  per-level allocation.
- **State-pool plumbing:** `FlattenContext` (`:193-200`) currently borrows
  `&mut Vec<Field<'static>>` and `&mut ArrayTrailVec`. Step 3 will add a
  `&'b mut Vec<u32>` borrow for `obj_index_buf`. Mind the existing borrow
  pattern in `FlattenJsonState::flatten` (`:160-188`) — extend it.
- **Kernel signature reminder:** `scan_object_index(data: &[u8], start: usize,
  out: &mut Vec<u32>, depth: &mut u32, init_in_str: bool, init_odd_bs: u64) ->
  (Option<usize> /* matching '}' */, ScanIndex, …)` per Step 1 wiring; offsets
  emitted are depth-1 quote/open/close positions. Returning `None` for the
  matching `}` means truncation → propagate as the existing `truncated block`
  error.
- **Re-baseline before measuring:** Step 3 is the first measurable change. Run
  the verification block (`cargo bench --bench matching -- 'flatten|citylots|status_'`,
  `profile_status`, `profile_array_heavy`) before any tuning; record numbers in
  the Step 3 subsection.
- **Kill criterion:** `flatten_status_*` regresses >2%. If hit, tune the size
  gate (try 128, 384, 512) before reverting.

### Conventions

- One subsection per step: what changed, bench delta, commit SHA. ≤10 lines.
- `cargo test --lib` + `just check` must pass after every step.
- Stop for user bench gate at Phase 1 Step 5 and Phase 2 Step 6.
- Failed sub-attempts stay in the doc as post-mortems (style:
  `docs/simd-narrow-first-scan-attempt.md`). Don't delete — record why it failed.

---

## Critical files (summary index)

| File | Phase 1 changes | Phase 2 changes |
|---|---|---|
| `src/flatten_json_simd.rs` | New `scan_object_index` kernel | — |
| `src/flatten_json.rs` | `obj_index_buf` pool, `read_object` pre-scan, indexed read variants | `FieldValue::EscapedRaw`, `decode_json_escapes` extraction, `read_string_value` escape path |
| `src/automaton/thread_safe.rs` | — | Wrap `transition_on` callers (`:1033`) for lazy decode |
| `src/lib.rs` | — | `NfaBuffers::decode_scratch` |
| `examples/profile_array_heavy.rs` | Tracked at `8fb46cf` — walk-heavy-array canary | — |
| `docs/lazy-flatten-architecture-plan.md` (this file) | Created `8fb46cf`, updated each step | Append step subsections |
