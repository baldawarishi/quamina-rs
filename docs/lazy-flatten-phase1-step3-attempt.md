# Phase 1 Step 3: per-object pre-scan in `read_object` — abandoned (shadow-only)

## Hypothesis

Per the lazy-flatten plan, `read_object` would call `scan_object_index` once
on entry to populate `obj_index_buf` with depth-1 structural offsets for the
body. Step 3 wires the kernel call as a "shadow run" — the offsets are
written but not yet consumed. Step 4 was to introduce indexed `read_*` /
`skip_*` variants that read from the buffer instead of re-scanning.

## What was tried

Wrapped `read_object` so the outer fn snapshots `obj_index_buf.len()` on
entry and truncates back on return (stack discipline across recursion). The
inner fn calls `scan_object_index(self.event, self.index, …)` when the
remaining body is ≥ 256 bytes (matching `BLOCK_SIMD_THRESHOLD` reasoning).
`FlattenContext` gained `obj_index_buf: &'b mut Vec<u32>`; `flatten()` threads
it from `FlattenJsonState::obj_index_buf`. No consumers were touched — the
existing per-member SIMD `scan_delim` path stayed.

## Result on aarch64 (NEON, M-series)

Measured against the post-Step-2 baseline at `8fc7844`:

| Bench                          | Pre        | Post (shadow) | Δ        |
|--------------------------------|------------|---------------|----------|
| flatten_context_fields         | 84.4 ns    | 2.17 µs       | **+2470%** |
| flatten_direct_context_fields  | 83.9 ns    | 2.15 µs       | +2462%   |
| flatten_sort_context_fields    | 90.8 ns    | 2.16 µs       | +2280%   |
| status_context_fields          | 209 ns     | 2.29 µs       | +997%    |
| flatten_last_field             | 1.04 µs    | 5.14 µs       | +395%    |
| status_last_field              | 1.07 µs    | 5.18 µs       | +384%    |
| flatten_middle_nested          | 1.72 µs    | 6.28 µs       | +266%    |
| status_middle_nested           | 1.76 µs    | 6.34 µs       | +260%    |
| status_all_three_patterns      | 2.12 µs    | 6.70 µs       | +216%    |
| citylots                       | 1.67 µs    | 1.88 µs       | +12.4%   |
| citylots_core                  | 277 ns     | 279 ns        | flat     |

`flatten_status_*` regression vastly exceeds the planned 2% kill criterion.

## Why it failed

The plan framed Step 3 as "shadow only — same work, just record offsets we
discard." That framing is **wrong for early-exit patterns**.

`status.json` is 9.4 KB. The pattern in `flatten_context_fields` only needs
two fields (`context.user_id`, `context.friends_count`), both near the front
of the document. The existing parser hits `FlattenError::EarlyStop` once both
are found — total time ~84 ns, parsing maybe a few hundred bytes.

`scan_object_index` does **not** know which fields the consumer wants. It
runs to the matching outer `}` of the top-level object, sweeping all 9.4 KB
unconditionally. ~150 64-byte chunks × ~13 ns/chunk ≈ 2 µs. That ~2 µs
matches the 84 ns → 2.17 µs delta exactly.

The trade only pays off when the consumer would have walked most of the body
anyway. For early-exit patterns — which are the *common* case for matching
workloads — pre-scan converts a sub-µs walk into a multi-µs full sweep,
regardless of whether Step 4's indexed consumers ever land. The cost is
**inherent in Step 3's commitment to the body-wide scan**, not in any
specific consumer choice.

`citylots_core` was flat because the citylots core event is small enough
that the gate (`remaining >= 256`) never engages, so the kernel never runs.
`citylots` regressed +12% — its pattern walks much of the document, so the
pre-scan cost is partially amortized but still net-negative against the
existing per-member SIMD path.

## Implications for the broader plan

Per-object pre-scan as designed is **incompatible with early-exit
semantics**. Any future attempt at this strategy needs one of:

1. **Streaming pre-scan.** Scan one 64-byte chunk at a time, advancing the
   pre-scan pointer as consumers consume. Lets the parser early-exit before
   sweeping unread bytes. Adds significant control-flow complexity and
   couples the kernel to consumer demand.
2. **Tree-aware gating.** Skip pre-scan when `tree.fields_count() +
   tree.nodes_count()` is small relative to the body size — i.e. when the
   parser is likely to early-exit. Heuristic, brittle to pattern shapes.
3. **Pre-scan only inside walked sub-objects whose tree is fully realized.**
   The outermost `read_object` should never pre-scan since early-exit is
   common at the top level. Even nested walked sub-objects can early-exit
   though.
4. **Drop Phase 1 entirely.** The per-member SIMD scan (`scan_delim` at
   `:728`, `:849`) is already fast (~50 ns/call, 4-vec movemask). Re-scan
   amortization across object members is a smaller win than Phase 2's lazy
   value materialization, which is structurally cheaper.

`scan_object_index` (committed at `1b89de2`) is preserved — the kernel is
correct and tested in isolation. The pooled `obj_index_buf` (committed at
`f588b72`) is preserved as `#[allow(dead_code)]` for any future approach that
wants a depth-1 offset buffer.

## What got reverted

The Step 3 change to `src/flatten_json.rs` (FlattenContext field +
read_object wrapper + pre-scan call) was reverted with `git checkout`. No
commit lands.

## Recommendation

Skip to **Phase 2 (lazy value materialization)**. The Phase 2 design is
independent of pre-scan: it defers escape decoding from flatten time to the
matcher's value-comparison time, with no body-wide commitment. Expected win
is small (≤2%) but the structural cost is also small.

If Phase 1 is to be revisited, prototype option (1) — streaming pre-scan —
in a throwaway branch first, with a microbenchmark that measures the
chunk-at-a-time amortization on a synthetic walk-heavy input that doesn't
trigger early-exit (e.g. a pattern that requires the *last* field of a long
object).
