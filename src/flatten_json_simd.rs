// SIMD intrinsics module: every function body is inside an `unsafe` boundary
// guaranteed by a `#[target_feature]` caller. Rather than repeat the same
// safety comment hundreds of times, allow the lints at the module level.
#![allow(unsafe_code)]
#![allow(clippy::undocumented_unsafe_blocks)]
#![allow(clippy::missing_safety_doc)]
#![allow(clippy::multiple_unsafe_ops_per_block)]

//! simdjson-style block/string scanner, multi-backend.
//!
//! Shared algorithm (all backends):
//! 1. Load 64 bytes; compare against `"`, `\`, `open`, `close` → four 64-bit bitmasks.
//! 2. `find_escaped`: identify chars after odd-length backslash runs (simdjson algorithm).
//! 3. `prefix_xor`: 6 XOR+shifts convert real-quote positions into a within-string mask.
//! 4. Structural chars masked by `!string_mask` drive nesting-level tracking.
//!
//! Backends: NEON (aarch64), AVX2 / SSE4.2 (x86_64), scalar (all others).
//!
//! On x86_64 the backend is resolved once at first call and cached as a
//! function pointer to avoid per-call feature detection.
//!
//! Sub-64-byte tails are handled by the kernel's caller via a zero-padded
//! buffer (zeros never match a structural byte); SIMD runs unconditionally
//! across all event sizes — short-event scalar short-circuits were
//! evaluated and not retained.
//!
//! Fast-path short-circuits in the scan loop:
//! - if `bs_bits == 0 && prev_odd_bs == 0` → `escaped = 0`, skip `find_escaped`.
//! - if `real_quotes == 0 && !prev_in_str` → `string_mask = 0`, skip `prefix_xor`.

const EVEN_BITS: u64 = 0x5555_5555_5555_5555;
const ODD_BITS: u64 = 0xAAAA_AAAA_AAAA_AAAA;

#[inline]
fn prefix_xor(mut x: u64, prev: &mut bool) -> u64 {
    x ^= x << 1;
    x ^= x << 2;
    x ^= x << 4;
    x ^= x << 8;
    x ^= x << 16;
    x ^= x << 32;
    if *prev {
        x = !x;
    }
    *prev = (x >> 63) != 0;
    x
}

#[inline]
fn find_escaped(bs: u64, prev_odd_bs: &mut u64) -> u64 {
    let start_edges = bs & !(bs << 1);
    let even_start_mask = EVEN_BITS ^ *prev_odd_bs;
    let even_starts = start_edges & even_start_mask;
    let odd_starts = start_edges & !even_start_mask;
    let even_carries = bs.wrapping_add(even_starts);
    let (odd_carries_raw, overflow) = bs.overflowing_add(odd_starts);
    let odd_carries = odd_carries_raw | *prev_odd_bs;
    *prev_odd_bs = u64::from(overflow);
    (even_carries & !bs & ODD_BITS) | (odd_carries & !bs & EVEN_BITS)
}

#[inline]
fn find_close_in_bits(opens: u64, closes: u64, level: &mut i32) -> Option<u32> {
    let mut bits = opens | closes;
    while bits != 0 {
        let pos = bits.trailing_zeros();
        if (opens >> pos) & 1 != 0 {
            *level += 1;
        } else {
            *level -= 1;
            if *level == 0 {
                return Some(pos);
            }
        }
        bits &= bits - 1;
    }
    None
}

/// Load 64 bytes and compare against a target byte, returning a 64-bit position mask.
/// # Safety
/// Implementors must ensure required ISA features are available at the call site.
unsafe trait Backend: Sized {
    unsafe fn load(data: &[u8], offset: usize) -> Self;
    unsafe fn cmp_mask(&self, target: u8) -> u64;
}

/// Shared scan loop — monomorphized per backend via `#[inline(always)]`.
#[inline(always)]
unsafe fn run_scan<B: Backend>(
    data: &[u8],
    start: usize,
    open: u8,
    close: u8,
    level: &mut i32,
    init_in_str: bool,
    init_odd_bs: u64,
) -> (Option<usize>, usize, bool, u64) {
    let mut i = start;
    let mut prev_in_str = init_in_str;
    let mut prev_odd_bs = init_odd_bs;

    while i + 64 <= data.len() {
        let chunk = unsafe { B::load(data, i) };
        let bs_bits = unsafe { chunk.cmp_mask(b'\\') };
        let quote_bits = unsafe { chunk.cmp_mask(b'"') };
        let open_bits = unsafe { chunk.cmp_mask(open) };
        let close_bits = unsafe { chunk.cmp_mask(close) };

        // Fast path: no backslashes in view and no escape carry → no escapes possible.
        let escaped = if bs_bits == 0 && prev_odd_bs == 0 {
            0
        } else {
            find_escaped(bs_bits, &mut prev_odd_bs)
        };
        let real_quotes = quote_bits & !escaped;
        // Fast path: no quotes and not currently inside a string → string_mask is 0.
        let string_mask = if real_quotes == 0 && !prev_in_str {
            0
        } else {
            prefix_xor(real_quotes, &mut prev_in_str)
        };
        let real_open = open_bits & !string_mask;
        let real_close = close_bits & !string_mask;

        if let Some(rel) = find_close_in_bits(real_open, real_close, level) {
            return (Some(i + rel as usize), i + 64, prev_in_str, prev_odd_bs);
        }

        i += 64;
    }

    (None, i, prev_in_str, prev_odd_bs)
}

/// Scan forward through a string body (past the opening `"`) for the closing `"`.
/// Returns (Some(abs_pos_of_closing_quote), scanned_to, prev_odd_bs).
#[inline(always)]
unsafe fn run_scan_string<B: Backend>(
    data: &[u8],
    start: usize,
    init_odd_bs: u64,
) -> (Option<usize>, usize, u64) {
    let mut i = start;
    let mut prev_odd_bs = init_odd_bs;

    while i + 64 <= data.len() {
        let chunk = unsafe { B::load(data, i) };
        let bs_bits = unsafe { chunk.cmp_mask(b'\\') };
        let quote_bits = unsafe { chunk.cmp_mask(b'"') };

        let escaped = if bs_bits == 0 && prev_odd_bs == 0 {
            0
        } else {
            find_escaped(bs_bits, &mut prev_odd_bs)
        };
        let real_quotes = quote_bits & !escaped;

        if real_quotes != 0 {
            return (
                Some(i + real_quotes.trailing_zeros() as usize),
                i + 64,
                prev_odd_bs,
            );
        }
        i += 64;
    }
    (None, i, prev_odd_bs)
}

/// Scan forward for the first `"` or `\`. Simpler than `run_scan_string`:
/// the caller bails to scalar on `\`, so we don't need escape masking.
///
/// Returns `(Some((abs_pos, which_byte)), scanned_to)` — which_byte is `"` or
/// `\`. On chunk-exhausted, `None` is returned and the caller continues from
/// `scanned_to` with a scalar loop.
#[inline(always)]
unsafe fn run_scan_delim<B: Backend>(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
    let mut i = start;
    while i + 64 <= data.len() {
        let chunk = unsafe { B::load(data, i) };
        let q = unsafe { chunk.cmp_mask(b'"') };
        let bs = unsafe { chunk.cmp_mask(b'\\') };
        let hit = q | bs;
        if hit != 0 {
            let rel = hit.trailing_zeros() as usize;
            let which = if (q >> rel) & 1 != 0 { b'"' } else { b'\\' };
            return (Some((i + rel, which)), i + 64);
        }
        i += 64;
    }
    (None, i)
}

// ── NEON (aarch64) ───────────────────────────────────────────────────────────
#[cfg(target_arch = "aarch64")]
mod neon {
    use super::*;
    use std::arch::aarch64::*;

    struct NeonChunk {
        v0: uint8x16_t,
        v1: uint8x16_t,
        v2: uint8x16_t,
        v3: uint8x16_t,
    }

    unsafe impl Backend for NeonChunk {
        #[target_feature(enable = "neon")]
        #[inline]
        unsafe fn load(data: &[u8], offset: usize) -> Self {
            unsafe {
                let p = data.as_ptr().add(offset);
                Self {
                    v0: vld1q_u8(p),
                    v1: vld1q_u8(p.add(16)),
                    v2: vld1q_u8(p.add(32)),
                    v3: vld1q_u8(p.add(48)),
                }
            }
        }

        #[target_feature(enable = "neon")]
        #[inline]
        unsafe fn cmp_mask(&self, target: u8) -> u64 {
            unsafe {
                let vt = vdupq_n_u8(target);
                movemask_bulk(
                    vceqq_u8(self.v0, vt),
                    vceqq_u8(self.v1, vt),
                    vceqq_u8(self.v2, vt),
                    vceqq_u8(self.v3, vt),
                )
            }
        }
    }

    /// simd-json's neon_movemask_bulk: AND with bit-position mask + 4× pairwise-add.
    /// Produces a u64 with one bit per byte across all four 16-byte chunks.
    #[target_feature(enable = "neon")]
    #[inline]
    unsafe fn movemask_bulk(c0: uint8x16_t, c1: uint8x16_t, c2: uint8x16_t, c3: uint8x16_t) -> u64 {
        unsafe {
            const BIT_MASK: [u8; 16] = [1, 2, 4, 8, 16, 32, 64, 128, 1, 2, 4, 8, 16, 32, 64, 128];
            let bm = vld1q_u8(BIT_MASK.as_ptr());
            let s = vpaddq_u8(
                vpaddq_u8(vandq_u8(c0, bm), vandq_u8(c1, bm)),
                vpaddq_u8(vandq_u8(c2, bm), vandq_u8(c3, bm)),
            );
            let s = vpaddq_u8(s, s);
            vgetq_lane_u64(vreinterpretq_u64_u8(s), 0)
        }
    }

    #[target_feature(enable = "neon")]
    pub(super) unsafe fn scan(
        data: &[u8],
        start: usize,
        open: u8,
        close: u8,
        level: &mut i32,
        init_in_str: bool,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, bool, u64) {
        unsafe { run_scan::<NeonChunk>(data, start, open, close, level, init_in_str, init_odd_bs) }
    }

    #[target_feature(enable = "neon")]
    pub(super) unsafe fn scan_string(
        data: &[u8],
        start: usize,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, u64) {
        unsafe { run_scan_string::<NeonChunk>(data, start, init_odd_bs) }
    }

    #[target_feature(enable = "neon")]
    pub(super) unsafe fn scan_delim(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
        unsafe { run_scan_delim::<NeonChunk>(data, start) }
    }
}

// ── AVX2 (x86_64) ────────────────────────────────────────────────────────────
#[cfg(target_arch = "x86_64")]
mod avx2 {
    use super::*;
    use std::arch::x86_64::*;

    struct Avx2Chunk {
        v0: __m256i,
        v1: __m256i,
    }

    unsafe impl Backend for Avx2Chunk {
        #[target_feature(enable = "avx2")]
        #[inline]
        unsafe fn load(data: &[u8], offset: usize) -> Self {
            unsafe {
                let p = data.as_ptr().add(offset);
                Self {
                    v0: _mm256_loadu_si256(p as *const __m256i),
                    v1: _mm256_loadu_si256(p.add(32) as *const __m256i),
                }
            }
        }

        #[target_feature(enable = "avx2")]
        #[inline]
        unsafe fn cmp_mask(&self, target: u8) -> u64 {
            // `_mm256_{set1,cmpeq,movemask}_epi8` are safe-to-call within an
            // AVX2 target_feature scope on modern stdlib; no inner block needed.
            let vt = _mm256_set1_epi8(target as i8);
            let r0 = _mm256_movemask_epi8(_mm256_cmpeq_epi8(self.v0, vt)) as u32;
            let r1 = _mm256_movemask_epi8(_mm256_cmpeq_epi8(self.v1, vt)) as u32;
            (r0 as u64) | ((r1 as u64) << 32)
        }
    }

    #[target_feature(enable = "avx2")]
    pub(super) unsafe fn scan(
        data: &[u8],
        start: usize,
        open: u8,
        close: u8,
        level: &mut i32,
        init_in_str: bool,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, bool, u64) {
        unsafe { run_scan::<Avx2Chunk>(data, start, open, close, level, init_in_str, init_odd_bs) }
    }

    #[target_feature(enable = "avx2")]
    pub(super) unsafe fn scan_string(
        data: &[u8],
        start: usize,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, u64) {
        unsafe { run_scan_string::<Avx2Chunk>(data, start, init_odd_bs) }
    }

    #[target_feature(enable = "avx2")]
    pub(super) unsafe fn scan_delim(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
        unsafe { run_scan_delim::<Avx2Chunk>(data, start) }
    }
}

// ── SSE4.2 (x86_64) ──────────────────────────────────────────────────────────
#[cfg(target_arch = "x86_64")]
mod sse42 {
    use super::*;
    use std::arch::x86_64::*;

    struct Sse42Chunk {
        v0: __m128i,
        v1: __m128i,
        v2: __m128i,
        v3: __m128i,
    }

    unsafe impl Backend for Sse42Chunk {
        #[target_feature(enable = "sse4.2")]
        #[inline]
        unsafe fn load(data: &[u8], offset: usize) -> Self {
            unsafe {
                let p = data.as_ptr().add(offset);
                Self {
                    v0: _mm_loadu_si128(p as *const __m128i),
                    v1: _mm_loadu_si128(p.add(16) as *const __m128i),
                    v2: _mm_loadu_si128(p.add(32) as *const __m128i),
                    v3: _mm_loadu_si128(p.add(48) as *const __m128i),
                }
            }
        }

        #[target_feature(enable = "sse4.2")]
        #[inline]
        unsafe fn cmp_mask(&self, target: u8) -> u64 {
            // `_mm_{set1,cmpeq,movemask}_epi8` are safe-to-call within an
            // SSE4.2 target_feature scope on modern stdlib; no inner block needed.
            let vt = _mm_set1_epi8(target as i8);
            let r0 = _mm_movemask_epi8(_mm_cmpeq_epi8(self.v0, vt)) as u16;
            let r1 = _mm_movemask_epi8(_mm_cmpeq_epi8(self.v1, vt)) as u16;
            let r2 = _mm_movemask_epi8(_mm_cmpeq_epi8(self.v2, vt)) as u16;
            let r3 = _mm_movemask_epi8(_mm_cmpeq_epi8(self.v3, vt)) as u16;
            (r0 as u64) | ((r1 as u64) << 16) | ((r2 as u64) << 32) | ((r3 as u64) << 48)
        }
    }

    #[target_feature(enable = "sse4.2")]
    pub(super) unsafe fn scan(
        data: &[u8],
        start: usize,
        open: u8,
        close: u8,
        level: &mut i32,
        init_in_str: bool,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, bool, u64) {
        unsafe { run_scan::<Sse42Chunk>(data, start, open, close, level, init_in_str, init_odd_bs) }
    }

    #[target_feature(enable = "sse4.2")]
    pub(super) unsafe fn scan_string(
        data: &[u8],
        start: usize,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, u64) {
        unsafe { run_scan_string::<Sse42Chunk>(data, start, init_odd_bs) }
    }

    #[target_feature(enable = "sse4.2")]
    pub(super) unsafe fn scan_delim(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
        unsafe { run_scan_delim::<Sse42Chunk>(data, start) }
    }
}

// ── Scalar fallback ──────────────────────────────────────────────────────────
// Runtime fallback on x86_64 when no SIMD ISA is detected, the only path on
// non-aarch64 non-x86 targets, and the parity-test reference under
// `cfg(test)` on aarch64 (where NEON is the runtime path).
#[cfg(any(test, not(target_arch = "aarch64")))]
mod scalar {
    use super::*;

    struct ScalarChunk([u8; 64]);

    unsafe impl Backend for ScalarChunk {
        #[inline]
        unsafe fn load(data: &[u8], offset: usize) -> Self {
            let mut buf = [0u8; 64];
            buf.copy_from_slice(&data[offset..offset + 64]);
            Self(buf)
        }

        #[inline]
        unsafe fn cmp_mask(&self, target: u8) -> u64 {
            let mut mask = 0u64;
            for (i, &b) in self.0.iter().enumerate() {
                if b == target {
                    mask |= 1u64 << i;
                }
            }
            mask
        }
    }

    pub(super) fn scan(
        data: &[u8],
        start: usize,
        open: u8,
        close: u8,
        level: &mut i32,
        init_in_str: bool,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, bool, u64) {
        unsafe {
            run_scan::<ScalarChunk>(data, start, open, close, level, init_in_str, init_odd_bs)
        }
    }

    pub(super) fn scan_string(
        data: &[u8],
        start: usize,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, u64) {
        unsafe { run_scan_string::<ScalarChunk>(data, start, init_odd_bs) }
    }

    pub(super) fn scan_delim(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
        unsafe { run_scan_delim::<ScalarChunk>(data, start) }
    }
}

// ── Platform dispatcher ───────────────────────────────────────────────────────

#[cfg(target_arch = "x86_64")]
type ScanFn = fn(&[u8], usize, u8, u8, &mut i32, bool, u64) -> (Option<usize>, usize, bool, u64);
#[cfg(target_arch = "x86_64")]
type ScanStringFn = fn(&[u8], usize, u64) -> (Option<usize>, usize, u64);
#[cfg(target_arch = "x86_64")]
type ScanDelimFn = fn(&[u8], usize) -> (Option<(usize, u8)>, usize);

// x86_64: resolve the backend once and cache the function pointer.
#[cfg(target_arch = "x86_64")]
mod x86_dispatch {
    use super::*;
    use std::sync::OnceLock;

    fn avx2_scan(
        data: &[u8],
        start: usize,
        open: u8,
        close: u8,
        level: &mut i32,
        init_in_str: bool,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, bool, u64) {
        unsafe { avx2::scan(data, start, open, close, level, init_in_str, init_odd_bs) }
    }
    fn sse42_scan(
        data: &[u8],
        start: usize,
        open: u8,
        close: u8,
        level: &mut i32,
        init_in_str: bool,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, bool, u64) {
        unsafe { sse42::scan(data, start, open, close, level, init_in_str, init_odd_bs) }
    }
    fn avx2_scan_string(
        data: &[u8],
        start: usize,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, u64) {
        unsafe { avx2::scan_string(data, start, init_odd_bs) }
    }
    fn sse42_scan_string(
        data: &[u8],
        start: usize,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, u64) {
        unsafe { sse42::scan_string(data, start, init_odd_bs) }
    }
    fn avx2_scan_delim(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
        unsafe { avx2::scan_delim(data, start) }
    }
    fn sse42_scan_delim(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
        unsafe { sse42::scan_delim(data, start) }
    }

    static SCAN: OnceLock<ScanFn> = OnceLock::new();
    static SCAN_STR: OnceLock<ScanStringFn> = OnceLock::new();
    static SCAN_DELIM: OnceLock<ScanDelimFn> = OnceLock::new();

    fn resolve_scan() -> ScanFn {
        if std::is_x86_feature_detected!("avx2") {
            avx2_scan
        } else if std::is_x86_feature_detected!("sse4.2") {
            sse42_scan
        } else {
            scalar::scan
        }
    }
    fn resolve_scan_string() -> ScanStringFn {
        if std::is_x86_feature_detected!("avx2") {
            avx2_scan_string
        } else if std::is_x86_feature_detected!("sse4.2") {
            sse42_scan_string
        } else {
            scalar::scan_string
        }
    }
    fn resolve_scan_delim() -> ScanDelimFn {
        if std::is_x86_feature_detected!("avx2") {
            avx2_scan_delim
        } else if std::is_x86_feature_detected!("sse4.2") {
            sse42_scan_delim
        } else {
            scalar::scan_delim
        }
    }

    #[inline]
    pub(super) fn scan(
        data: &[u8],
        start: usize,
        open: u8,
        close: u8,
        level: &mut i32,
        init_in_str: bool,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, bool, u64) {
        let f = *SCAN.get_or_init(resolve_scan);
        f(data, start, open, close, level, init_in_str, init_odd_bs)
    }

    #[inline]
    pub(super) fn scan_string(
        data: &[u8],
        start: usize,
        init_odd_bs: u64,
    ) -> (Option<usize>, usize, u64) {
        let f = *SCAN_STR.get_or_init(resolve_scan_string);
        f(data, start, init_odd_bs)
    }

    #[inline]
    pub(super) fn scan_delim(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
        let f = *SCAN_DELIM.get_or_init(resolve_scan_delim);
        f(data, start)
    }
}

// Top-level dispatchers route to the active backend. Each arm forwards to a
// single backend module — the per-call CPU-feature probe (and its OnceLock
// caching) lives one level deeper inside `x86_dispatch`.
cfg_if::cfg_if! {
    if #[cfg(target_arch = "aarch64")] {
        #[inline]
        fn scan_block_dispatch(
            data: &[u8], start: usize, open: u8, close: u8,
            level: &mut i32, init_in_str: bool, init_odd_bs: u64,
        ) -> (Option<usize>, usize, bool, u64) {
            unsafe { neon::scan(data, start, open, close, level, init_in_str, init_odd_bs) }
        }
        #[inline]
        fn scan_string_dispatch(
            data: &[u8], start: usize, init_odd_bs: u64,
        ) -> (Option<usize>, usize, u64) {
            unsafe { neon::scan_string(data, start, init_odd_bs) }
        }
        #[inline]
        fn scan_delim_dispatch(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
            unsafe { neon::scan_delim(data, start) }
        }
    } else if #[cfg(target_arch = "x86_64")] {
        #[inline]
        fn scan_block_dispatch(
            data: &[u8], start: usize, open: u8, close: u8,
            level: &mut i32, init_in_str: bool, init_odd_bs: u64,
        ) -> (Option<usize>, usize, bool, u64) {
            x86_dispatch::scan(data, start, open, close, level, init_in_str, init_odd_bs)
        }
        #[inline]
        fn scan_string_dispatch(
            data: &[u8], start: usize, init_odd_bs: u64,
        ) -> (Option<usize>, usize, u64) {
            x86_dispatch::scan_string(data, start, init_odd_bs)
        }
        #[inline]
        fn scan_delim_dispatch(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
            x86_dispatch::scan_delim(data, start)
        }
    } else {
        #[inline]
        fn scan_block_dispatch(
            data: &[u8], start: usize, open: u8, close: u8,
            level: &mut i32, init_in_str: bool, init_odd_bs: u64,
        ) -> (Option<usize>, usize, bool, u64) {
            scalar::scan(data, start, open, close, level, init_in_str, init_odd_bs)
        }
        #[inline]
        fn scan_string_dispatch(
            data: &[u8], start: usize, init_odd_bs: u64,
        ) -> (Option<usize>, usize, u64) {
            scalar::scan_string(data, start, init_odd_bs)
        }
        #[inline]
        fn scan_delim_dispatch(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
            scalar::scan_delim(data, start)
        }
    }
}

pub fn scan_block(
    data: &[u8],
    start: usize,
    open: u8,
    close: u8,
    level: &mut i32,
    init_in_str: bool,
    init_odd_bs: u64,
) -> (Option<usize>, usize, bool, u64) {
    scan_block_dispatch(data, start, open, close, level, init_in_str, init_odd_bs)
}

pub fn scan_string(data: &[u8], start: usize, init_odd_bs: u64) -> (Option<usize>, usize, u64) {
    scan_string_dispatch(data, start, init_odd_bs)
}

/// Find the first `"` or `\` at or after `start` in `data`.
/// Returns `(Some((abs_pos, which_byte)), scanned_to)` or `(None, scanned_to)`
/// if the remainder is <64 bytes with no hit. `which_byte` is `"` or `\`.
pub fn scan_delim(data: &[u8], start: usize) -> (Option<(usize, u8)>, usize) {
    scan_delim_dispatch(data, start)
}

// ── Cross-backend parity tests ───────────────────────────────────────────────
//
// Every SIMD backend must produce bit-identical output to the scalar
// reference on the same input. The corpus is intentionally compact (each
// case ≤ 192 bytes, ≤ 8 cases) so the suite stays inside Miri's CI budget.
//
// Miri behaviour: `is_x86_feature_detected!` always returns false under
// Miri, so the AVX2/SSE4.2 branches are skipped automatically — Miri only
// exercises the scalar path against itself, validating UB-freedom of the
// shared `run_scan*` kernels. The aarch64 NEON backend is gated with
// `cfg(target_arch = "aarch64")` and Miri runs on x86_64 in CI, so NEON is
// also elided there at compile time. NEON parity is exercised on dev
// machines (the user's mac) and on any future aarch64 CI runner.
#[cfg(test)]
mod parity_tests {
    use super::*;

    /// Build the parity corpus. Each case targets a distinct code path in
    /// `run_scan*`: empty / sub-chunk / chunk-aligned / multi-chunk with
    /// string masking / odd-length backslash carry / even-length runs.
    fn corpus() -> Vec<(&'static str, Vec<u8>)> {
        let mut v: Vec<(&'static str, Vec<u8>)> = vec![
            ("empty", vec![]),
            ("short_no_struct", b"hello world".to_vec()),
            ("short_quotes", b"\"hi\"".to_vec()),
            ("short_escape", b"\"a\\\"b\"".to_vec()),
        ];

        // Exactly one chunk (64B) with both an open and close brace.
        let mut one = vec![b'.'; 64];
        one[10] = b'{';
        one[50] = b'}';
        v.push(("one_chunk_braces", one));

        // Two chunks: a `{` lives inside a string in chunk 0 (must be
        // masked out), the real `}` lives in chunk 1.
        let mut two = vec![b'.'; 128];
        two[10] = b'"';
        two[40] = b'{';
        two[70] = b'"';
        two[90] = b'}';
        v.push(("two_chunks_string_mask", two));

        // Odd-length backslash run straddling the 64-byte boundary: the
        // following `"` must be treated as escaped, NOT as a real quote.
        let mut odd = vec![b'a'; 192];
        odd[0] = b'"';
        for byte in odd.iter_mut().take(67).skip(60) {
            *byte = b'\\';
        }
        odd[67] = b'"'; // escaped — must be ignored
        odd[100] = b'"'; // real close
        v.push(("odd_bs_carry", odd));

        // Even-length backslash run: carry must NOT propagate; following
        // `"` is real.
        let mut even = vec![b'a'; 128];
        even[0] = b'"';
        for byte in even.iter_mut().take(64).skip(60) {
            *byte = b'\\';
        }
        even[64] = b'"'; // real (preceded by `\\\\`)
        v.push(("even_bs_no_carry", even));

        // Truncated string with no closing `"`: `scan_string` and
        // `scan_delim` must report `None` rather than overrunning.
        let mut trunc = vec![b'a'; 80];
        trunc[0] = b'"';
        v.push(("truncated_string", trunc));

        // Unclosed brace, padded to 192 bytes of filler: `scan_block`'s
        // `find_close_in_bits` must return `None` after exhausting input.
        let mut unclosed = vec![b'.'; 192];
        unclosed[0] = b'{';
        v.push(("unclosed_brace_192", unclosed));

        // Backslash at the last byte of chunk 0 (byte 63): the next `"`
        // at byte 64 is escaped via the cross-chunk `prev_odd_bs` carry,
        // not via in-chunk masking.
        let mut bs_boundary = vec![b'a'; 128];
        bs_boundary[0] = b'"';
        bs_boundary[63] = b'\\';
        bs_boundary[64] = b'"'; // escaped via carry, must be ignored
        bs_boundary[100] = b'"'; // real close
        v.push(("bs_at_chunk_boundary", bs_boundary));

        v
    }

    /// Initial-state matrix for `scan_block` / `scan_string`. Covers fresh
    /// state plus both carry flags so the `find_escaped` / `prefix_xor`
    /// fast-path guards (`bs_bits == 0 && prev_odd_bs == 0`,
    /// `real_quotes == 0 && !prev_in_str`) are exercised.
    const SCAN_INIT_STATES: &[(bool, u64, &str)] = &[
        (false, 0, "fresh"),
        (true, 0, "in_str"),
        (false, 1, "odd_bs"),
        (true, 1, "in_str+odd_bs"),
    ];

    #[test]
    fn scan_block_parity() {
        for (case, data) in corpus() {
            for &(in_str, odd_bs, st) in SCAN_INIT_STATES {
                let mut lvl_ref = 1i32;
                let baseline = scalar::scan(&data, 0, b'{', b'}', &mut lvl_ref, in_str, odd_bs);
                let scenario = format!("{case}/{st}");

                #[cfg(target_arch = "aarch64")]
                {
                    let mut lvl = 1i32;
                    let got = unsafe { neon::scan(&data, 0, b'{', b'}', &mut lvl, in_str, odd_bs) };
                    assert_eq!(got, baseline, "neon vs scalar @ {scenario}");
                    assert_eq!(lvl, lvl_ref, "neon level @ {scenario}");
                }

                #[cfg(target_arch = "x86_64")]
                {
                    if std::is_x86_feature_detected!("avx2") {
                        let mut lvl = 1i32;
                        let got =
                            unsafe { avx2::scan(&data, 0, b'{', b'}', &mut lvl, in_str, odd_bs) };
                        assert_eq!(got, baseline, "avx2 vs scalar @ {scenario}");
                        assert_eq!(lvl, lvl_ref, "avx2 level @ {scenario}");
                    }
                    if std::is_x86_feature_detected!("sse4.2") {
                        let mut lvl = 1i32;
                        let got =
                            unsafe { sse42::scan(&data, 0, b'{', b'}', &mut lvl, in_str, odd_bs) };
                        assert_eq!(got, baseline, "sse42 vs scalar @ {scenario}");
                        assert_eq!(lvl, lvl_ref, "sse42 level @ {scenario}");
                    }
                }
            }
        }
    }

    #[test]
    fn scan_string_parity() {
        for (case, data) in corpus() {
            for &(_, odd_bs, st) in SCAN_INIT_STATES {
                let baseline = scalar::scan_string(&data, 0, odd_bs);
                let scenario = format!("{case}/{st}");

                #[cfg(target_arch = "aarch64")]
                {
                    let got = unsafe { neon::scan_string(&data, 0, odd_bs) };
                    assert_eq!(got, baseline, "neon vs scalar @ {scenario}");
                }

                #[cfg(target_arch = "x86_64")]
                {
                    if std::is_x86_feature_detected!("avx2") {
                        let got = unsafe { avx2::scan_string(&data, 0, odd_bs) };
                        assert_eq!(got, baseline, "avx2 vs scalar @ {scenario}");
                    }
                    if std::is_x86_feature_detected!("sse4.2") {
                        let got = unsafe { sse42::scan_string(&data, 0, odd_bs) };
                        assert_eq!(got, baseline, "sse42 vs scalar @ {scenario}");
                    }
                }
            }
        }
    }

    #[test]
    fn scan_delim_parity() {
        for (case, data) in corpus() {
            let baseline = scalar::scan_delim(&data, 0);

            #[cfg(target_arch = "aarch64")]
            {
                let got = unsafe { neon::scan_delim(&data, 0) };
                assert_eq!(got, baseline, "neon vs scalar @ {case}");
            }

            #[cfg(target_arch = "x86_64")]
            {
                if std::is_x86_feature_detected!("avx2") {
                    let got = unsafe { avx2::scan_delim(&data, 0) };
                    assert_eq!(got, baseline, "avx2 vs scalar @ {case}");
                }
                if std::is_x86_feature_detected!("sse4.2") {
                    let got = unsafe { sse42::scan_delim(&data, 0) };
                    assert_eq!(got, baseline, "sse42 vs scalar @ {case}");
                }
            }
        }
    }

    /// Mid-stream start: every backend must respect `start > 0` identically
    /// (matters for `read_string_value` resuming after a `\` bail-out).
    #[test]
    fn nonzero_start_parity() {
        let mut data = vec![b'.'; 192];
        data[5] = b'"';
        data[80] = b'"';
        data[150] = b'"';

        for &start in &[1usize, 32, 64, 65, 128] {
            let baseline = scalar::scan_delim(&data, start);

            #[cfg(target_arch = "aarch64")]
            {
                let got = unsafe { neon::scan_delim(&data, start) };
                assert_eq!(got, baseline, "neon vs scalar @ start={start}");
            }

            #[cfg(target_arch = "x86_64")]
            {
                if std::is_x86_feature_detected!("avx2") {
                    let got = unsafe { avx2::scan_delim(&data, start) };
                    assert_eq!(got, baseline, "avx2 vs scalar @ start={start}");
                }
                if std::is_x86_feature_detected!("sse4.2") {
                    let got = unsafe { sse42::scan_delim(&data, start) };
                    assert_eq!(got, baseline, "sse42 vs scalar @ start={start}");
                }
            }
        }
    }
}
