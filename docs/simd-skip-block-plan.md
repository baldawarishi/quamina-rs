# SIMD skip_block: Architecture Plan & Progress Tracker

> Last updated: 2026-04-20  
> Reference repo: `/Users/rishibaldawa/workspaces/simd-json`  
> Target file: `src/flatten_json.rs` → `mod simd_block`

---

## Problem Statement

`skip_block` and `skip_string_value` dominate JSON flattening time when matching patterns
that require skipping large/deeply nested JSON objects. Initial profiling showed these two
functions account for ~96-97% of pipeline time on skip-heavy payloads.

Previous attempts:
- `memchr3` scalar: ~5.4 µs (not an improvement over baseline)
- Raw NEON intrinsics with rollback: ~5.54 µs (correctness hack caused 2× work)
- Raw NEON carry-state (current): ~2.2 µs (`status_middle_nested` after NFA→DFA landed)

The carry-state NEON implementation is correct and working, but it is:
1. **aarch64-only** (`#[cfg(target_arch = "aarch64")]`) — no x86 path
2. **Uses raw `std::arch::aarch64::*` intrinsics** — hard to read, no portability
3. **Not using SVE** (Apple Silicon doesn't expose it; server ARM SVE is 2026+ in stable Rust)

---

## Goal

Replace raw NEON intrinsics with a **multi-backend trait abstraction** modeled after
simd-json, supporting:
- ARM NEON (aarch64) — already working
- x86 SSE4.2 / AVX2 — new
- Pure-Rust fallback — for non-SIMD targets

Stay on **stable Rust** (no nightly). Do NOT add external SIMD crate dependencies;
simd-json proves raw `std::arch` intrinsics are stable and sufficient.

---

## Key Findings from simd-json

### Architecture (reference: `/Users/rishibaldawa/workspaces/simd-json/src/`)

simd-json uses a **trait-based abstraction** (`Stage1Parse` in `src/lib.rs:149-289`)
that all backends implement identically. The trait defines:
- `new(ptr: &[u8]) -> Self` — load 64 bytes into SIMD registers
- `cmp_mask_against_input(m: u8) -> u64` — compare all 64 bytes vs `m`, return bitmask
- `compute_quote_mask(quote_bits: u64) -> u64` — prefix-XOR (platform-specific: SSE uses `pclmulqdq`)
- `find_whitespace_and_structurals(...)` — shufti nibble-table technique

Backends live in `src/impls/{avx2,sse42,neon,simd128,native,portable}/stage1.rs`.

### NEON movemask technique (simd-json vs ours)

**simd-json** (`src/impls/neon/stage1.rs:21-41`):
```rust
// Uses AND + pairwise horizontal-add
let bit_mask = [0x01u8, 0x02, 0x04, ..., 0x80, 0x01, 0x02, ...];
let t = vandq_u8(p, bit_mask);   // isolate each bit's position
vpaddq_u8(t0, t1)                // horizontal pairwise-add to pack 16→8→4→2→1 lanes
```

**Our current approach** (`src/flatten_json.rs:movemask16`):
```rust
// Uses shift-right-7 + multiply by lane-weights + horizontal-add
let high = vshrq_n_u8(eq, 7);                  // isolate MSB
let weighted = vmulq_u8(high, weights);         // 1,2,4,8...128 per lane
let lo = vaddv_u8(vget_low_u8(weighted));       // sum low 8 bytes
let hi = vaddv_u8(vget_high_u8(weighted));      // sum high 8 bytes
```

Both are valid; simd-json's `vand+vpadd` chain processes 4 registers in one call
(`neon_movemask_bulk`), while ours calls `movemask16` four times separately.

### `compute_quote_mask` — SSE4.2 vs pure-Rust fallback

SSE4.2 uses `_mm_clmulepi64_si128` (carryless multiply = GF(2) multiply) for prefix-XOR
in a single instruction. ARM NEON has no equivalent; simd-json falls back to 6 XOR+shift
steps — which is exactly what we implemented as `prefix_xor`. ✓

### Runtime dispatch

simd-json uses `std::is_x86_feature_detected!("avx2")` at runtime + `AtomicPtr` caching
for zero-cost after first call. ARM does no runtime dispatch (NEON is always present).

### Stable Rust requirement
simd-json requires Rust **1.88+** stable. Their `portable` backend (std::simd) is opt-in
via `features = ["portable"]` and requires nightly — it is NOT the default.
Conclusion: raw `std::arch` intrinsics on stable is the right path.

---

## Design for quamina-rs

### Module structure (target state)

```
src/flatten_json.rs
  mod simd_block {
      trait BlockScanner { ... }          // analogous to simd-json's Stage1Parse subset

      struct AArch64Scanner { ... }       // NEON backend
      struct Avx2Scanner { ... }          // AVX2 backend (new)
      struct Sse42Scanner { ... }         // SSE4.2 backend (new)
      struct ScalarScanner { ... }        // fallback (bitwise, no SIMD)

      // Runtime dispatch (x86 only; aarch64 always uses NEON)
      fn scan_block(...) -> (Option<usize>, usize, bool, u64)
  }
```

### `BlockScanner` trait

Only the parts we need for `skip_block` (we don't need simd-json's full Stage1Parse):

```rust
trait BlockScanner {
    /// Load 64 bytes from data[i..i+64] into SIMD registers
    fn load(data: &[u8], offset: usize) -> Self;

    /// Return a 64-bit mask: bit j=1 iff data[offset+j] == target
    fn cmp_mask(&self, target: u8) -> u64;

    /// Prefix-XOR of quote bitmask → within-string mask
    fn compute_quote_mask(quote_bits: u64) -> u64;
}
```

`find_escaped`, `find_close_in_bits`, and `prefix_xor` (our current implementations)
are pure `u64` operations — they don't need to be in the trait and are shared.

---

## Implementation Plan

### Step 1 — Extract shared u64 logic ✓ (already done)
`prefix_xor`, `find_escaped`, `find_close_in_bits` are already platform-agnostic.
No changes needed.

### Step 2 — Refactor AArch64Scanner
- Keep current NEON `movemask16` + `bitmask64` logic
- Optionally adopt simd-json's `neon_movemask_bulk` (single-call for all 4 registers)
- Wrap in a `struct AArch64Scanner { v0..v3: uint8x16_t }` + implement `BlockScanner`
- Benchmark to confirm no regression

### Step 3 — Add Avx2Scanner
- Use `_mm256_set1_epi8`, `_mm256_cmpeq_epi8`, `_mm256_movemask_epi8`
- Two 256-bit registers cover 64 bytes (same as simd-json's avx2 backend)
- `compute_quote_mask`: use `_mm_clmulepi64_si128` for single-instruction prefix-XOR
- `#[cfg(target_arch = "x86_64")]` + `#[target_feature(enable = "avx2")]`

### Step 4 — Add Sse42Scanner
- Four 128-bit registers cover 64 bytes
- `_mm_cmpeq_epi8` + `_mm_movemask_epi8` 
- `compute_quote_mask`: same `pclmulqdq` trick
- `#[cfg(target_arch = "x86_64")]` + `#[target_feature(enable = "sse4.2")]`

### Step 5 — ScalarScanner fallback
- No intrinsics; `cmp_mask` implemented as a byte-by-byte loop building a u64
- `compute_quote_mask` uses the 6 XOR+shift steps (our current `prefix_xor`)
- Always safe, no feature gates

### Step 6 — Runtime dispatch
- x86_64: `if is_x86_feature_detected!("avx2")` → Avx2Scanner, else Sse42Scanner, else Scalar
- aarch64: always AArch64Scanner (NEON is baseline)
- Cache dispatch choice in an `AtomicU8` (0=uninit, 1=avx2, 2=sse42, 3=scalar) or use
  simd-json's `AtomicPtr` function-pointer approach

### Step 7 — Remove `#[cfg(target_arch = "aarch64")]` gate in `skip_block`
Once ScalarScanner covers non-SIMD targets, the SIMD path can be unconditional.

### Step 8 — Benchmarks
Run `status_middle_nested`, `flatten_middle_nested`, `citylots` on:
- aarch64 (current hardware) — should be neutral vs current
- x86_64 (CI or separate machine) — new data point

---

## Progress Tracking

| Step | Status | Notes |
|------|--------|-------|
| 1. Shared u64 logic extracted | ✅ Done | `prefix_xor`, `find_escaped`, `find_close_in_bits` |
| 2. Carry-state (no rollback) | ✅ Done | Commit in main; 773 tests pass |
| 3. Refactor to `Backend` trait + `NeonChunk` | ✅ Done | `unsafe trait Backend { load, cmp_mask }`; `NeonChunk` uses simd-json's `movemask_bulk` (AND+vpadd) |
| 4. Add `Avx2Chunk` | ✅ Done | `_mm256_loadu_si256` + `_mm256_movemask_epi8`; 2 registers cover 64 bytes |
| 5. Add `Sse42Chunk` | ✅ Done | `_mm_loadu_si128` × 4 + `_mm_movemask_epi8` |
| 6. `ScalarChunk` fallback | ✅ Done | `[u8; 64]` copy + bit loop; safe on all targets |
| 7. Runtime dispatch | ✅ Done | Per-arch `dispatch()` fn; x86_64 uses `is_x86_feature_detected!` |
| 8. Remove aarch64-only cfg gate in `skip_block` | ✅ Done | `scan_block` is now safe to call on all platforms |
| 9. Benchmark on x86_64 | ⬜ Todo | Needs CI or separate machine |

---

## Key simd-json Files to Reference

| What | File in simd-json repo |
|------|------------------------|
| Stage1Parse trait | `src/lib.rs:149-289` |
| Runtime dispatch + caching | `src/lib.rs:318-510` |
| AVX2 cmp_mask + shufti | `src/impls/avx2/stage1.rs` |
| SSE4.2 cmp_mask + pclmulqdq | `src/impls/sse42/stage1.rs` |
| NEON movemask_bulk | `src/impls/neon/stage1.rs:21-41` |
| compute_quote_mask (all backends) | `src/impls/*/stage1.rs:66-80` |
| find_odd_backslash_sequences | `src/lib.rs:215-244` |
| Scalar fallback | `src/impls/native/stage1.rs` |

---

## Open Questions

1. **Does `neon_movemask_bulk` outperform our current `movemask16 × 4`?** ✅ Resolved.  
   Yes — 21–34% improvement on flatten/status benchmarks. Old: `vshrq+vmulq+vaddv×2` per register
   (~28 instructions for 4 registers). New: `vand×4 + vpadd×3 + vgetq` (9 total). `vmulq_u8` latency
   was the bottleneck on Apple Silicon.

2. **AVX2 pclmulqdq for `compute_quote_mask` on x86 — measurable win?**  
   On aarch64 we use 6 XOR+shift steps (prefix_xor). On x86 with SSE4.2+PCLMUL,
   a single `_mm_clmulepi64_si128` replaces all 6 steps. Will test.

3. **Worth adding `find_whitespace_and_structurals` shufti?**  
   simd-json uses nibble-table lookup (shufti) to detect `{`, `}`, `[`, `]`, `"`, `,`
   in one pass. For `skip_block` we only need open/close/quote/backslash, so 4 calls to
   `cmp_mask` is already optimal. Shufti would save ~2 comparisons but add LUT setup.
   Probably not worth it for our use case.

4. **x86 test environment?**  
   Current dev machine is Apple M-series (aarch64). x86 benchmarks need CI or a separate
   Linux machine.

---

## Phase 2: extended SIMD coverage

Plan: `~/.claude/plans/okay-let-s-create-a-snappy-elephant.md`. Phase 1 shipped
the `skip_block` / `skip_string_value` SIMD kernels (NEON/AVX2/SSE4.2/scalar).
Phase 2 extends SIMD to the three remaining hot scalar loops in
`flatten_json.rs`: used-field reads (`read_string_value` / `read_member_name`),
whitespace skipping, and number parsing.

### Baseline (2026-04-21, Step 0 — apple silicon M-series)

- `examples/profile_status`: `flatten_only 2037 ns/op`, `match_only 193 ns/op`,
  `full_pipeline 2273 ns/op` (flatten = 90% of pipeline).
- `cargo test --lib`: 773 passed.
- `just check`: clean.
- Thresholds: `STRING_SIMD_THRESHOLD=64`, `BLOCK_SIMD_THRESHOLD=0` (defaults, not
  yet swept — Step 1).
- Commits: (a) SIMD infra module + lib wiring + profile examples; (b) `skip_block`
  / `skip_string_value` callers switched over. See `git log` for SHAs.
