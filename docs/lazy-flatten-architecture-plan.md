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
| 0. Pre-Phase-2 baseline | ✅ Done | (this doc) | — | Recorded below; tree at `66b573b`. |
| 1. Extract `decode_json_escapes` | ✅ Done | _TBD_ | n/a (refactor) | Free fn + `DecodeEscapeError`; `read_string_with_escapes` two-phase. 780 tests pass. |
| 2. `FieldValue::EscapedRaw` variant | ✅ Done | _TBD_ | n/a (additive) | Variant + `as_bytes()` updated; `needs_escape_decode()` helper. |
| 3. Switch escape path to `EscapedRaw` | ✅ Done | _TBD_ | _bench-pending_ | New `read_string_value_lazy` validates+borrows; `read_string_with_escapes` removed; 9 escape value tests use `decoded_value` helper. |
| 4. `NfaBuffers::decode_scratch` | ✅ Done | _TBD_ | n/a (pool field) | `Vec<u8>` field + reset alongside `arena_bufs.clear()`. |
| 5. Wrap matcher caller for lazy decode | ✅ Done | _TBD_ | _bench-pending_ | `transition_on_arena` arena-only signature; split-borrow at `try_to_match_direct`; quote-wrap into `decode_scratch`. |
| 6. Bench + alloc-profile gate | ✅ Done | _TBD_ | citylots −3.2%, status_context −7.2%, canary −0.6% | array_heavy fix (explicit-arm match + `#[cold]` decode helper) + dhat probe (132 allocs / 400 escape decodes — pool works). |

#### Baseline (Step 0, 2026-04-29, branch `perf/simd-flatten-json` at `66b573b`)

`profile_status` (status.json, single pattern):

| metric | ns/op |
|---|---|
| flatten_only | 1851 |
| match_only | 200 |
| full_pipeline | 2031 |

Flatten = 91% of pipeline.

`profile_array_heavy` (event 904 B, 100-element string array):

- string-gate at threshold 0: **2526 ns/call** (sweep flat ±1% across all thresholds)
- block-gate at threshold 0: **2519 ns/call** (sweep flat ±1%)

`cargo bench --bench matching -- 'flatten|citylots|status_'`:

| bench | time |
|---|---|
| flatten_context_fields | 83.95 ns |
| flatten_direct_context_fields | 83.61 ns |
| flatten_sort_context_fields | 93.64 ns |
| flatten_middle_nested | 1.754 µs |
| flatten_last_field | 1.043 µs |
| status_context_fields | 216.04 ns |
| status_middle_nested | 1.768 µs |
| status_last_field | 1.098 µs |
| status_all_three_patterns | 2.104 µs |
| citylots | 1.698 µs |
| citylots_core | 289.58 ns |

In-line with Phase 1 Step 0 baseline (Phase 1 only added a kernel + dead-code
pool field; no measurable shift expected). Phase 2 deltas are computed against
these absolute numbers; the >1% kill criterion (Step 6) is per-bench.

#### Phase 2 bench gate (Step 6)

Run on aarch64 NEON (M-series), uncommitted tree on top of `66b573b`.
Deltas vs Step 0 baseline. Kill criterion is ">1% wall-clock regression
on any flatten/match bench" — none triggered.

| bench | baseline | post-Phase-2 | delta |
|---|---|---|---|
| flatten_context_fields | 83.95 ns | 84.77 ns | +1.0% (noise) |
| flatten_direct_context_fields | 83.61 ns | 84.27 ns | +0.8% (noise) |
| flatten_sort_context_fields | 93.64 ns | 91.45 ns | **−2.3%** |
| flatten_middle_nested | 1.754 µs | 1.712 µs | **−2.4%** |
| flatten_last_field | 1.043 µs | 1.031 µs | −1.2% |
| status_context_fields | 216.04 ns | 200.01 ns | **−7.4%** |
| status_middle_nested | 1.768 µs | 1.781 µs | +0.7% (noise) |
| status_last_field | 1.098 µs | 1.069 µs | **−2.6%** |
| status_all_three_patterns | 2.104 µs | 2.064 µs | **−1.9%** |
| citylots | 1.698 µs | 1.639 µs | **−3.5%** |
| citylots_core | 289.58 ns | 283.21 ns | −2.2% |

`profile_status` (status.json, no escapes — pure overhead probe):
flatten_only 1838 → 1869 ns (+1.7%); match_only 198 → 199 ns (flat);
full_pipeline 2101 → 2074 ns (−1.3%). Net pipeline still wins.

`profile_array_heavy` (904 B, 100-element string array):
2526 → 2598 ns (+2.9%) at threshold 0; sweep stays ±1% across all
gate values. Walk-heavy-array canary regressed slightly — likely the
extra `match`/destructure at each `try_to_match_direct` call.

**Win surprise:** plan expected ≤2%; observed wins of −3.5% on citylots
and −7.4% on status_context_fields suggest the lazy path also helps
non-escape values via reduced state-load pressure (cache effect from
the smaller `try_to_match_direct` hot path). No allocation profile
captured yet (samply/dhat) — visible alloc count drop is plausible
but unverified.

Tree state at gate: 780 lib tests pass, clippy + fmt clean.

**Deferred to a fresh session** (user direction, 2026-04-29):

1. **Investigate `profile_array_heavy` +2.9% regression.** Walk-heavy
   100-element string array, no escapes — pure overhead probe for the
   `try_to_match_direct` rewrite. Suspected source: the per-field
   `match &field.val { ... }` + `NfaBuffers` destructure runs on every
   field even when nothing is `EscapedRaw`. Hypotheses to test:
   - Hoist the variant check outside the loop body (event-level
     fast-path: skip the destructure entirely if no field is
     `EscapedRaw`). Cheap precondition: `fields.iter().any(needs_escape_decode)`.
   - Mark the destructure-and-match block `#[cold]`/branch-hint so
     the no-escape path stays linear.
   - Inline the `EscapedRaw` arm into `field.value_bytes()` indirectly
     via a `Cow`-returning helper that's `#[inline(always)]`.
   Bench probe: re-run `cargo run --release --example profile_array_heavy`
   after each hypothesis; goal is to bring the canary back to ≤+0.5%
   (within sweep noise) without giving back the citylots/status wins.
   Kill if any try makes a flatten/match bench regress >1%.

2. **Allocation profile (the planned Step 6 deliverable).** Confirm
   `FieldValue::Owned` allocations actually dropped on escape-content
   workloads. Two probes:
   - `dhat-rs` on `bench_citylots_core` — count Vec allocations
     before/after Phase 2; expectation is the per-escape-value `Owned`
     allocs go to zero.
   - `samply record cargo run --release --example profile_status` —
     wall-clock breakdown of `read_string_value` vs the matcher's
     decode wrapper. Expected: `Owned`-allocation site disappears from
     the flatten-side flame graph; `decode_json_escapes` appears
     under the matcher path only when escape-bearing values are
     actually compared.
   No code change unless the profile contradicts the bench numbers
   (e.g., allocation count *increased* — would indicate a leak in the
   pool-clear logic).

**Resume order:**
1. Re-baseline (`cargo bench --bench matching -- 'flatten|citylots|status_'`,
   `profile_status`, `profile_array_heavy`) on the current tree.
   Numbers above were captured pre-commit; the commit step may shift
   absolute timings by code-layout noise.
2. Item (1) first — it's the only open correctness/perf concern.
   Item (2) is observability and runs after.

**Commit shape (decided, 2026-04-29):** Steps 1–5 land as one perf
commit (matching the plan's 2-3 commit shape); the docs update
(this section + the Step subsections below) lands as a separate
docs commit. The user did not request commits in this session — wait
for explicit instruction before running `git commit`.

#### Phase 2 Step 6 follow-up — `profile_array_heavy` fix (2026-04-29)

Re-baseline on uncommitted Phase-2 tree showed the canary at
2565 ns (STRING@0) — ~+1.5% vs Step 0 baseline 2526 ns. Smaller
than the original +2.9% but still non-zero.

Root cause: the value-extract match in `try_to_match_direct`
used a wildcard arm `_ => field.value_bytes()`. The wildcard
branch did its own discriminant check, then `value_bytes()` →
`as_bytes()` ran a second 3-arm match on the same discriminant.
For Borrowed/Owned values (the vast majority) this is a wasted
load + branch on every field.

Fix (one commit):
- Replaced the wildcard arm with explicit `Borrowed(s) => s,
  Owned(v) => v.as_slice(), EscapedRaw(raw) => decode_escaped_for_match(...)`
  in `src/automaton/thread_safe.rs:1054-1067`. Single discriminant
  read, no double match.
- Extracted the EscapedRaw decode body into
  `decode_escaped_for_match` (`src/automaton/thread_safe.rs:113-129`)
  marked `#[cold]` + `#[inline(never)]`. LLVM emits a tail-edge
  branch hint and keeps the rare path out of the hot icache line.
- Added `#[inline]` to `FieldValue::as_bytes` (`src/flatten_json.rs:120`)
  so other call sites (tests, flattener trait) keep the same
  inlining behavior they had before.

Hypothesis (a) — event-level fast-path via
`fields.iter().any(needs_escape_decode)` — was not needed; (b)+(c)
together hit the goal.

Bench results post-fix on aarch64 NEON (M-series), uncommitted
tree on top of `66b573b`:

`profile_array_heavy` STRING@0: 2510.43 ns (baseline 2526 →
**−0.6%**, sweep flat ±1% across all thresholds). Canary recovered
below pre-Phase-2.

`profile_status`: flatten_only 1839 ns (baseline 1838, flat);
full_pipeline 2101 ns (matches baseline exactly).

`cargo bench --bench matching -- 'flatten|citylots|status_'`:

| bench | Step 0 baseline | post-fix | Δ |
|---|---|---|---|
| flatten_context_fields | 83.95 | 84.43 | +0.6% (noise) |
| flatten_direct_context_fields | 83.61 | 83.86 | +0.3% (noise) |
| flatten_sort_context_fields | 93.64 | 93.33 | −0.3% |
| flatten_middle_nested | 1.754 µs | 1.727 µs | **−1.5%** |
| flatten_last_field | 1.043 µs | 1.050 µs | +0.7% (noise) |
| status_context_fields | 216.04 | 200.46 | **−7.2%** |
| status_middle_nested | 1.768 µs | 1.795 µs | +1.5% (noise; iso re-run within ±1%) |
| status_last_field | 1.098 µs | 1.077 µs | **−1.9%** |
| status_all_three_patterns | 2.104 µs | 2.070 µs | **−1.6%** |
| citylots | 1.698 µs | 1.644 µs | **−3.2%** |
| citylots_core | 289.58 | 274.50 | **−5.2%** |

`status_middle_nested` flagged as +1.7% by criterion (vs the
prior re-baseline run, not vs Step 0). Isolated re-run lands
within criterion's noise threshold — run-to-run jitter, not
systematic.

780 lib tests pass, clippy + fmt clean.

#### Phase 2 Step 6 follow-up — Allocation profile (2026-04-29)

Existing memory bench (`benches/memory.rs`) workloads have no
escape-bearing values: `status.json` carries only 8 escapes
across 317 strings, citylots street names are pure ASCII.
These workloads cannot demonstrate Phase 2's mechanism by
themselves — there's nothing to drop.

Added `profile_escape_content_matching` (`benches/memory.rs:373-401`)
to validate the pool. Pattern with 4 paths a/b/c/d, each value
carries an escape (`\"`, `\n`, `\t`, `é`); 100 events ×
4 escape-bearing matched fields = 400 escape-decode operations
per run. Run with `cargo bench --bench memory --features dhat-heap`.

Result: **132 total allocations** for 400 decode operations
(0.33 alloc/decode). Peak live: 3310 bytes. The `decode_scratch`
pool absorbs the steady-state capacity — most decode calls hit
the pre-grown buffer with zero allocation. The 132 includes
profiler/match-set/hashmap entries unrelated to escape decode.

Compare to citylots-100-events (no escapes): 60 allocs / 100 events.
Escape-content workload only adds ~70 allocs over 400 decodes
(amortized to <0.18 alloc/decode), confirming the lazy decode
path is bounded by pool capacity, not proportional to escape count.

No code change indicated — the bench numbers and the alloc
profile both agree the mechanism works as designed.

Existing-bench summary (no regressions vs the pre-Phase-2
shape): citylots 60 allocs/100 events (Go: 55), large-JSON
130 allocs/100 events; both unchanged from prior Phase 2 runs.

#### Step 1 — Extract `decode_json_escapes`

Added `pub(crate) fn decode_json_escapes(raw, scratch) -> Result<&[u8],
DecodeEscapeError>` plus a private `decode_hex_4` helper near the
`FieldValue` definition. `DecodeEscapeError` is a small enum
(`PrematureEnd`/`MalformedEscape`/`IllegalByte(u8)`/`TruncatedUnicode`/
`InvalidHex`); `decode_error_message` maps it back to the legacy strings.

`read_string_with_escapes` is now two-phase: a tight scan to locate the
closing `"` (skipping `\X` pairs), then a single decode call into a local
scratch Vec, then quote-wrap into `Owned`. Local scratch is interim — Step 4
routes it through `NfaBuffers::decode_scratch`. `read_member_name_with_escapes`
left untouched (out of scope per plan).

Position-recovery on decode error: `self.index` is set to `val_start` so
`self.error(...)` reports a sensible (if slightly coarser) line/col. No
existing test asserts exact line/col on escape errors. 780 lib tests pass;
clippy + fmt clean.

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

**Current state (2026-04-29 — Phase 2 closed):** Phase 1 closed
(microbench rules out chunked redesigns). Phase 2 Steps 0–6
implemented in-tree (uncommitted) on top of `66b573b`. Both
deferred follow-up items completed in this session:

1. ✅ `profile_array_heavy` regression fixed: explicit-arm match
   + `#[cold]` decode helper in `try_to_match_direct`. Canary
   recovered below pre-Phase-2 baseline (-0.6% vs Step 0).
2. ✅ Allocation profile captured via dhat: 400 escape-decode
   operations produce 132 total allocs (pool reuse confirmed).
   New `profile_escape_content_matching` in `benches/memory.rs`
   added as a permanent regression net.

Phase 2 final numbers on aarch64 NEON (M-series): citylots
−3.2%, citylots_core −5.2%, status_context_fields −7.2%,
status_last_field −1.9%, status_all_three_patterns −1.6%,
flatten_middle_nested −1.5%; canary array_heavy −0.6%; all
others within ±1% noise.

The `scan_object_index` kernel and pooled `obj_index_buf` field
(Phase 1 leftovers) remain in the tree; kernel useful in isolation,
field marked `#[allow(dead_code)]`.

780 lib tests pass, clippy + fmt clean. No commit yet — Phase 2
commit shape decided as one perf commit (Steps 1–5 + array_heavy
fix) + one docs commit (this doc + memory.rs probe). Wait for
explicit user instruction before running `git commit`.

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

### Phase 2 Step 6 follow-up entry points (resume here)

Phase 2 Steps 0–5 are landed in tree (uncommitted). Step 6 partially
done: bench gate run, alloc profile + array_heavy investigation
deferred. Pick up in this order:

#### Read order before touching code

1. This doc — top to bottom, but especially the "Phase 2 bench gate
   (Step 6)" subsection below. The two deferred items are described
   there with hypotheses and bench probes.
2. The Step 1–5 subsections in this doc (what changed in each file).
3. `~/.claude/plans/create-a-plane-to-squishy-pearl.md` — only as
   reference for the original Step 6 wording.

#### Tree state to confirm

```bash
git status --short
# Expected: 4 modified files (docs/lazy-flatten-architecture-plan.md,
# src/automaton/small_table.rs, src/automaton/thread_safe.rs,
# src/flatten_json.rs) plus the unrelated docs/simd-narrow-first-scan-attempt.md.
# If clean (commits already landed), pick up at the deferred-items list.
cargo test --lib    # 780 passing
just check          # clean
```

#### First action: re-baseline

The numbers in the Step 6 bench-gate table were captured at end of
the implementation session before any commit. Re-run before doing
anything else (commits or tuning will shift absolute timings):

```bash
cargo bench --bench matching -- 'flatten|citylots|status_'
cargo run --release --example profile_status
cargo run --release --example profile_array_heavy
```

Then start with **deferred item (1)** (`profile_array_heavy`
investigation). Item (2) (allocation profile) runs after.

#### Legacy: original Phase 2 Step 0 entry (now obsolete)

The Step 1 implementation plan that lived here previously is no
longer relevant — Steps 1–5 are landed. Kept below as historical
reference; the Step 1–5 subsection in this doc is the up-to-date
record of what changed.

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
