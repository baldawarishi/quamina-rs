# SIMD `skip_string_value` — Continuation Plan

> Created: 2026-04-20
> Target file: `src/flatten_json.rs`
> Companion doc: `docs/simd-skip-block-plan.md`

---

## Current State

`skip_string_value` now uses a three-tier structure:

1. **Size-gated entry** (`if self.event.len() - self.index < 64`): small events skip SIMD setup and go straight to scalar helper.
2. **SIMD main loop** (`simd_block::scan_string`): 64-byte chunks via `Backend::cmp_mask` trait, `find_escaped`, first-unescaped-quote bit.
3. **Scalar tail** (`skip_string_scalar`, `#[cold] #[inline(never)]`): plain `iter().position()` byte-loop, handles SIMD carry state (`odd_bs`).

**Companion `skip_block`** (unchanged): SIMD main loop + padded-chunk 64-byte-buffer tail. All platforms (NEON/AVX2/SSE4.2/scalar) via unified `Backend` trait. No `memchr` in this file.

**Status**: 773 tests pass. Last run showed `nested_match` regression fixed (−14.7%), but minor regressions (+2–11%) on 20+ unrelated benchmarks suggesting the code-layout fix via `#[cold]`/size-gate is the right direction — needs benchmark validation.

---

## Problem Chain (for context)

1. Replaced aarch64-only NEON `skip_block` with multi-backend trait (NEON/AVX2/SSE4.2/scalar). ✅
2. Simplified `skip_block` by replacing the inline string-finish + `memchr3` tail with a padded-chunk second scan. ✅
3. Removed `memchr2` from `skip_string_value`; first replaced with `iter().position()`, then with SIMD `scan_string`. ✅
4. Padded-chunk tail in `scan_string` caused regressions on small events (82-byte `nested_match` paid 64-byte memcpy per call). Fixed with scalar tail. ✅
5. Scalar tail inlined into hot path caused code-layout regressions on unrelated benchmarks. Fixed with size-gate + `#[cold]` helper. ⏳ **needs bench verification**

---

## Immediate Next Step (fresh session)

### 1. Run benchmarks to validate the `#[cold]` + size-gate change

```bash
cargo bench --bench matching 2>&1 | grep -E "ns/iter|µs|ms" | head -60
```

**What to look for:**
- `nested_match` stays ≤ ~115 ns (avoid regression)
- `100_patterns`, `numeric_range_single`, `regexp_complex`, `shellstyle_26_patterns` — should recover the previous layout-related regressions
- `flatten_last_field`, `status_last_field` — should retain the 8-9% gains from SIMD `skip_string_value`
- `status_middle_nested`, `flatten_middle_nested` — should retain the ~3-5% gains

**Baseline to beat** (from previous run with scalar-inline version):
| Bench | Target |
|-------|--------|
| `nested_match` | ≤ 115 ns |
| `100_patterns` | ≤ 112 ns (was 109 before scalar-inline regressed it) |
| `numeric_range_single` | ≤ 90 ns |
| `flatten_last_field` | ≤ 1.20 µs |
| `status_middle_nested` | ≤ 1.99 µs |
| `shellstyle_multi_match` | ≤ 6.95 µs |

---

## If the `#[cold]` fix doesn't fully recover — fallback options

### Option A: Input pre-padding (option #5 from the original list)

At the top of `FlattenJsonState::flatten()`, copy `event` into a reusable padded buffer:

```rust
struct FlattenJsonState<'a> {
    event_buf: Vec<u8>,      // persistent, +64 bytes padding
    event: &'a [u8],         // current window (points into event_buf)
    event_len: usize,        // original event length
    index: usize,
    // ...
}

fn flatten(&mut self, event: &[u8], tree: &SegmentsTree) -> ... {
    self.event_buf.clear();
    self.event_buf.reserve(event.len() + 64);
    self.event_buf.extend_from_slice(event);
    self.event_buf.resize(event.len() + 64, 0);  // zero-pad
    // then operate on self.event_buf, using event.len() for bounds
}
```

Then `scan_string` / `scan_block` can *always* read 64-byte chunks without any tail handling — eliminates the scalar path entirely. One memcpy per event (O(event_len)) in exchange for cleaner inner loops.

**Trade-off**: memcpy cost for tiny events (~1-2 ns for 82-byte `nested_match`). Might wash out the SIMD gains on small events. Worth trying only if the size-gate fix is insufficient.

### Option B: Overread-from-source (option #2)

If we can guarantee 64 bytes readable past `self.event.len()` (true if caller provides padded input, or if `event` comes from a `Vec<u8>` with spare capacity), do unaligned loads directly. Same code as #5 but no copy. Requires API contract change.

### Option C: SWAR tail (option #4)

Replace the scalar tail with a u64-at-a-time SWAR scan using `has_zero_byte` patterns. Faster than byte-loop for medium tails (8–63 bytes), no SIMD register setup cost. More code complexity.

---

## Plan Doc Updates Needed

When the current change is validated, update `docs/simd-skip-block-plan.md`:

- Step 9 (x86_64 benchmark): still todo
- Add new section on `skip_string_value` architecture
- Note the size-gate threshold choice (64 bytes)

---

## Reference Snippets

### Current `skip_string_value` (at `src/flatten_json.rs:629`)

```rust
#[inline]
fn skip_string_value(&mut self) -> Result<(), FlattenError> {
    self.step()?;
    if self.event.len() - self.index < 64 {
        return self.skip_string_scalar(0);
    }
    let (found, scanned_to, odd_bs) =
        simd_block::scan_string(self.event, self.index, 0);
    if let Some(pos) = found {
        self.index = pos;
        return Ok(());
    }
    self.index = scanned_to;
    self.skip_string_scalar(odd_bs)
}

#[cold]
#[inline(never)]
fn skip_string_scalar(&mut self, init_odd_bs: u64) -> Result<(), FlattenError> {
    if init_odd_bs != 0 && self.index < self.event.len() {
        self.index += 1;
    }
    loop {
        let slice = &self.event[self.index..];
        let offset = slice.iter().position(|&b| b == b'"' || b == b'\\')
            .ok_or_else(|| FlattenError::Error(self.error("truncated string")))?;
        self.index += offset;
        if self.event[self.index] == b'"' {
            return Ok(());
        }
        self.index += 1;
        if self.index < self.event.len() {
            self.index += 1;
        }
    }
}
```

### `scan_string` (at `src/flatten_json.rs` in `mod simd_block`)

Pure-u64 `find_escaped` + `cmp_mask(b'"')` + `cmp_mask(b'\\')` per 64-byte chunk. Returns `(Option<pos>, scanned_to, prev_odd_bs)`.

### `Backend` trait

`unsafe trait Backend { load(data, offset) -> Self; cmp_mask(&self, target) -> u64 }`. Implementations: `NeonChunk`, `Avx2Chunk`, `Sse42Chunk`, `ScalarChunk`. Runtime dispatch via `is_x86_feature_detected!` on x86_64; direct NEON on aarch64.

---

## Commit Strategy

Single logical commit for this work, after bench validation:

```
perf: SIMD skip_string_value + size-gate scalar tail for small events

- Add scan_string to simd_block (NEON/AVX2/SSE4.2/scalar)
- Size-gate entry: events with <64 bytes remaining skip SIMD setup
- Scalar tail extracted to #[cold] helper to preserve hot-path layout
- Removes last memchr usage from flatten_json.rs
```
