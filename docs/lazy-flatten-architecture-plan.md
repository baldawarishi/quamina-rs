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
| 2. Pool `obj_index_buf` | ✅ Done | `f588b72` | — | Field + reset; preserved as `#[allow(dead_code)]` after Phase 1 abandon. |
| 3. Pre-scan in `read_object` (gated) | ❌ **Abandoned** | — | +2470% on `flatten_context_fields` | See `docs/lazy-flatten-phase1-step3-attempt.md` — pre-scan is incompatible with early-exit. |
| 3b. Strategy microbench | ✅ Done | `b2b6a8e` | V1 ≥ V0 on every workload | `examples/profile_prescan_strategies.rs`; closes Phase 1. |
| 4. Indexed read/skip variants | ⏭️ Skipped | — | — | Microbench shows no headroom for any chunk-bounded design. |
| 5. Bench gate + tuning | ⏭️ Skipped | — | — | — |

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

#### Step 3 — Per-object pre-scan in `read_object` — abandoned

Tried wrapping `read_object` with stack-discipline `obj_index_buf` snapshot/truncate
and a `scan_object_index` call gated on `remaining >= 256`. Shadow-only — no
consumers — but **regressed `flatten_context_fields` 84 ns → 2.17 µs (+2470%)** on
status.json. Root cause: the existing parser hits `FlattenError::EarlyStop` for
patterns that match few fields (~hundreds of bytes parsed), but `scan_object_index`
unconditionally sweeps the entire 9.4 KB body up front. Per-object pre-scan is
**incompatible with early-exit semantics**. Reverted with `git checkout`. Full
post-mortem with table + alternatives in `docs/lazy-flatten-phase1-step3-attempt.md`.

#### Step 3 follow-up — Strategy microbench

Built `examples/profile_prescan_strategies.rs` (commit `b2b6a8e`) to quantify
the amortization headroom for any chunk-bounded redesign without modifying
production code. Compares per-member `scan_delim` (V0) against full
`scan_object_index` (V1), chunk-bounded `scan_object_index` at K∈{1,2,4,8,16,ALL}
chunks (V2 — models option (a) streaming with consumer-driven refill +
early-exit at chunk K), and per-chunk streaming with carry state (V3 —
measures dispatch overhead).

Results on aarch64 NEON (M-series), full output in commit:

| Workload | V0 full walk | V1 full pre-scan | V2 K=1 | V2 K=ALL | Verdict |
|---|---|---|---|---|---|
| W1 (30 short, 360 B) | 177 ns / 60 quotes | 182 ns | 33.7 ns | 172.8 ns | ≈ break-even |
| W2 (200 short, 2.6 KB) | 937 ns / 400 quotes | 1303 ns | 34.6 ns | 1316 ns | **V1 −39% vs V0** |
| W3 (status outer, 147 B) | 14.8 ns / 7 quotes | 39 ns | 19.4 ns | 42.5 ns | **V1 −165%** |
| status.json full body | 5.5 ns / 2 quotes | 1923 ns | 14.1 ns | 1923 ns | catastrophic |

Read on the data — **even a perfectly chunk-bounded streaming pre-scan
(option (a) with zero implementation overhead) loses to baseline on every
measured workload, including the full-walk W2**. Per-member `scan_delim`
costs ~2.3-3 ns/find; `scan_object_index` costs ~13-32 ns/chunk and pushes
to a Vec (more overhead on dense bodies). The plan's "amortize N re-scans"
premise is false — `scan_delim` already returns at first hit per chunk, so
re-scan cost is bounded by chunk size, not body size.

V3 streaming dispatch overhead is small (~5-10% per chunk vs V1's monolithic
loop) — i.e. the streaming approach doesn't pay much extra for control flow.
The kernel itself is the cost, not the dispatch.

**Conclusion:** Phase 1's "pre-scan amortization" thesis is structurally
unsupported by the data. Both option (a) "streaming chunk pre-scan" and
option (b) "multi-hit scan_delim" inherit the same per-chunk-load cost
(option (b) is a strict improvement on (a) since it drops depth+string
masking, but the upper bound is V0 itself). Phase 1 closed.

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

**Current state (2026-04-29):** Phase 1 closed. Step 0–2 landed
(`8fb46cf`, `1b89de2`, `f588b72`). Step 3 attempted shadow wire-up;
reverted after measuring +2470% regression on `flatten_context_fields`
(post-mortem: `docs/lazy-flatten-phase1-step3-attempt.md`). Step 3b
microbench (`b2b6a8e`, `examples/profile_prescan_strategies.rs`) showed
that **even a zero-overhead chunk-bounded pre-scan loses to baseline on
every measured workload** — V1 was −39% vs V0 on the W2 full-walk case
where the plan expected the biggest win. Per-member `scan_delim` is
already near-optimal (~2.3-3 ns/find) and `scan_object_index` costs
~13-32 ns/chunk which can't be amortized below baseline.

The `scan_object_index` kernel and pooled `obj_index_buf` field are
preserved in the tree (kernel useful in isolation; field marked
`#[allow(dead_code)]`).

Resume at **Phase 2 Step 0** (lazy value materialization). Phase 2 is
structurally independent of pre-scan and the plan's expected ≤2% win
remains live. 780 lib tests pass, clippy + fmt clean.

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

### Phase 2 Step 0 entry points (resume here)

Phase 1 is closed (microbench data + post-mortem rules out chunked redesigns).
Start Phase 2 fresh.

#### Read order before touching code

1. **This doc, top to bottom** (especially the Phase 1 Step 3 + Step 3b
   summaries — they explain why the kernel + pool field stay in the tree
   even though no production code calls them).
2. `docs/lazy-flatten-phase1-step3-attempt.md` — the early-exit failure mode.
3. `~/.claude/plans/create-a-plane-to-squishy-pearl.md` — Phase 2 design
   intent (this doc is the live tracker; that one is the original plan).
4. `docs/simd-skip-block-plan.md` Phase 2 section — conventions for
   per-step subsection style and the "stop at the bench gate" rule.

#### Tree state to confirm

```bash
git log --oneline -1   # should be b2b6a8e or later
cargo test --lib       # 780 passing
just check             # clean
```

If `git status` shows the unrelated `docs/simd-narrow-first-scan-attempt.md`
as untracked, that's pre-existing and unrelated to this plan.

#### Re-baseline before Step 1

Step 1 is a refactor (no behavior change), but Phase 2's `<2%` kill criterion
will need a fresh baseline. Run before touching code:

```bash
cargo bench --bench matching -- 'flatten|citylots|status_'
cargo run --release --example profile_status
cargo run --release --example profile_array_heavy
```

Record numbers in the new "Phase 2 Step 0 — baseline" subsection (style
matches the existing Phase 1 Step 0 baseline block in this doc).

#### Step 1 plan in detail

**File:** `src/flatten_json.rs`. Read `read_string_with_escapes` at `:883`
in full first — it's ~80 lines covering the escape table, `\uXXXX` decoding,
and surrogate-pair handling. Extract its body into:

```rust
pub(crate) fn decode_json_escapes(raw: &[u8], scratch: &mut Vec<u8>) -> &[u8] {
    // Caller passes the bytes between the opening and closing `"` (NOT
    // including the quotes). `scratch` is cleared at function entry; the
    // returned slice borrows from `scratch`.
}
```

Caveats from the existing code:

- The current implementation pushes the leading `"` into the output Vec
  (`val: Vec<u8> = vec![b'"'];` at `:887`) and the trailing `"` at the end
  (`:894`). The new helper takes already-stripped bytes and returns
  un-quoted content. Caller in `read_string_with_escapes` re-wraps with
  quotes for the `Owned(Vec<u8>)` path; in Phase 2 Step 5 the matcher-side
  wrapper bypasses re-wrapping.
- `read_hex_4` is a method on `FlattenContext`. The extracted helper can't
  call it. Either (a) inline the hex parsing into `decode_json_escapes` —
  16 lines — or (b) make `read_hex_4` a free function. (a) is cleaner; the
  caller already validated bounds before reaching escape territory.
- Surrogate-pair logic at `:921-937` reads ahead in `self.event` past the
  current position. In the extracted helper, `raw` is the full content
  slice between quotes — surrogate look-ahead becomes look-ahead within
  `raw`. Mind the bounds.
- Member-name decoder `read_member_name_with_escapes` (`:764`) has the
  same body shape. **Out of scope for Phase 2** — names hit the hashmap
  immediately, so lazy decode is no win. Don't extract it as part of
  Step 1; doing so adds a usage site without a Phase 2 consumer.

**Verification at Step 1:** `cargo test --lib` clean. The two-step plan
is "extract, then switch caller" — Step 1 only extracts, so test coverage
of `read_string_with_escapes` is the regression net.

#### Steps 2-5 already specified in the table above

No new design needed — the existing Phase 2 Steps table at line 207 is the
working spec. Pick up at Step 2 once Step 1 lands.

### Files in scope for Phase 2

| File | Phase 2 changes |
|---|---|
| `src/flatten_json.rs` | Extract `decode_json_escapes`; add `FieldValue::EscapedRaw`; switch `read_string_value` escape branch |
| `src/automaton/thread_safe.rs` | Wrap `transition_on` callers (`:1033`) for lazy decode |
| `src/lib.rs` | Add `decode_scratch: Vec<u8>` to `NfaBuffers` (`:46-50`) |

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
