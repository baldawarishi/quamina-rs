//! Microbench for the lazy-flatten Phase 1 strategy comparison. The deliverable
//! doc abandoned the body-wide pre-scan after Step 3 regressed `flatten_context_fields`
//! by +2470%. Before designing more, this prototype quantifies:
//!
//! - **V0 baseline.** Per-member `scan_delim` calls (current production path).
//! - **V1 full pre-scan.** Single `scan_object_index` over the whole body
//!   (what Step 3 attempted — the upper-bound win for full-walk consumers and
//!   the upper-bound regression for early-exit consumers).
//! - **V2 chunked pre-scan.** `scan_object_index` over the first K 64-byte
//!   chunks. Models option (a) streaming refill + early-exit at chunk K. The
//!   K=ALL case equals V1; smaller K models partial walks.
//! - **V3 per-chunk streaming overhead.** Repeated `scan_object_index` calls
//!   on individual 64-byte windows summed. Measures the cost penalty of
//!   chunk-level dispatch vs the inlined `while i + 64 <= data.len()` loop in
//!   V1 — i.e. how much the streaming approach pays for control flow.
//!
//! Three workloads:
//!
//! - **W1.** 30 short string members (`"k00":"v00",...`). ~600 B.
//! - **W2.** 200 short string members. ~4 KB.
//! - **W3.** Status-outer-object shape (5 mixed-type members). ~150 B.
//!
//! Run: `cargo run --release --example profile_prescan_strategies`.
//!
//! Reads on the data:
//! - V1 vs V0 = amortization headroom (negative = pre-scan never wins; positive
//!   = ceiling on Phase-1 win for a full walk).
//! - V2[K] vs V0[K members] = where streaming pre-scan crosses over with
//!   baseline as the parser walks more of the body.
//! - V3 vs V1 = per-chunk dispatch tax of a streaming impl.

use quamina::flatten_json_simd::{scan_delim, scan_object_index};
use std::hint::black_box;
use std::time::Instant;

fn build_walk_heavy(n: usize) -> Vec<u8> {
    let members: Vec<String> = (0..n).map(|i| format!(r#""k{i:02}":"v{i:02}""#)).collect();
    let body = members.join(",") + "}";
    body.into_bytes()
}

/// Members with values that span chunk boundaries — the workload most likely
/// to benefit from pre-scan, since `scan_delim` would do multiple chunk loads
/// per find while pre-scan amortizes them.
fn build_long_strings(n: usize) -> Vec<u8> {
    // Each value is ~80 bytes → ~100 byte member → spans 2 chunks.
    let filler: String = "x".repeat(80);
    let members: Vec<String> = (0..n)
        .map(|i| format!(r#""key{i:02}":"{filler}{i:02}""#))
        .collect();
    let body = members.join(",") + "}";
    body.into_bytes()
}

/// Status.json's outer shape: `id`, `text`, `created_at`, `user` (sub-object
/// here just opened+closed for kernel symmetry), `followers_count`. Mixed
/// types so per-member scans aren't all string-string.
fn build_status_outer() -> Vec<u8> {
    let body = r#""id":1234567890,"text":"hello world hello world","created_at":"Mon Apr 29 18:00:00 +0000 2026","user":{"id":42,"name":"jdoe"},"followers_count":42}"#;
    body.as_bytes().to_vec()
}

/// All depth-1 quote positions in the body — used to drive V0's per-member
/// scan emulation. Skips quotes inside nested objects (depth ≥ 2).
fn quote_positions_depth1(body: &[u8]) -> Vec<usize> {
    let mut out = Vec::new();
    let mut depth: i32 = 1;
    let mut in_str = false;
    let mut prev_bs = false;
    for (i, &b) in body.iter().enumerate() {
        if depth == 0 {
            break; // hit closing }
        }
        if in_str {
            if prev_bs {
                prev_bs = false;
            } else if b == b'\\' {
                prev_bs = true;
            } else if b == b'"' {
                if depth == 1 {
                    out.push(i);
                }
                in_str = false;
            }
            continue;
        }
        match b {
            b'"' => {
                if depth == 1 {
                    out.push(i);
                }
                in_str = true;
            }
            b'{' | b'[' => depth += 1,
            b'}' | b']' => depth -= 1,
            _ => {}
        }
    }
    out
}

/// V0 baseline: emulate the parser's per-member scan pattern. Each call to
/// `scan_delim` looks for the next `"` or `\` from `start`; we emulate the
/// production path's "scan to closing quote" by calling scan_delim N times
/// where N = number of depth-1 quotes that *close* a string (= every other
/// depth-1 quote, starting from the second). Returning `start` to one byte
/// after the *opening* quote and scanning forward mimics
/// `read_member_name` / `read_string_value`.
///
/// Cost = N × scan_delim, where N is roughly `quote_positions.len() / 2`.
#[inline(never)]
fn bench_v0_baseline(body: &[u8], quotes: &[usize], member_limit: usize) -> u64 {
    // For each pair (open, close), simulate the call from open+1 → close.
    // member_limit caps how many closing-scans we do (= early-exit at member M).
    let mut acc: u64 = 0;
    let pairs = quotes.len() / 2;
    let n = pairs.min(member_limit);
    for i in 0..n {
        let open = quotes[i * 2];
        let scan_from = open + 1;
        let (found, _scanned_to) = scan_delim(body, scan_from);
        if let Some((pos, _)) = found {
            acc = acc.wrapping_add(pos as u64);
        }
    }
    acc
}

/// V1: single `scan_object_index` over the whole body. Discards offsets via
/// truncation (so allocator stays cold on the hot path).
#[inline(never)]
fn bench_v1_full_prescan(body: &[u8], scratch: &mut Vec<u32>) -> u64 {
    scratch.clear();
    let mut depth: i32 = 1;
    // Note: kernel processes 64-byte chunks via `while i + 64 <= data.len()`.
    // Bodies smaller than 64 bytes process zero chunks and return immediately —
    // that's the same gating production code would apply.
    let (close, _scanned_to, _in_str, _odd_bs) =
        scan_object_index(body, 0, scratch, &mut depth, false, 0);
    close.unwrap_or(0) as u64
}

/// V2: `scan_object_index` over the first `chunks` × 64 bytes only.
/// Truncates the input slice; emulates "consumer early-exits after K chunks".
#[inline(never)]
fn bench_v2_chunked(body: &[u8], scratch: &mut Vec<u32>, chunks: usize) -> u64 {
    scratch.clear();
    let limit = (chunks * 64).min(body.len());
    let truncated = &body[..limit];
    let mut depth: i32 = 1;
    let (close, _scanned_to, _in_str, _odd_bs) =
        scan_object_index(truncated, 0, scratch, &mut depth, false, 0);
    close.unwrap_or(scratch.len() as usize) as u64
}

/// V3: simulate streaming by calling `scan_object_index` one 64-byte chunk
/// at a time, with carry state passed through. Measures per-chunk dispatch
/// overhead vs V1's monolithic call.
#[inline(never)]
fn bench_v3_streaming(body: &[u8], scratch: &mut Vec<u32>, chunks: usize) -> u64 {
    scratch.clear();
    let mut depth: i32 = 1;
    let mut in_str = false;
    let mut odd_bs: u64 = 0;
    let mut acc: u64 = 0;
    for c in 0..chunks {
        let lo = c * 64;
        let hi = (lo + 64).min(body.len());
        if hi <= lo + 63 {
            break; // less than a full chunk → kernel processes nothing
        }
        let window = &body[..hi]; // kernel walks `start..i+64<=len` from `start = lo`
        let (close, _scanned_to, ns, nb) =
            scan_object_index(window, lo, scratch, &mut depth, in_str, odd_bs);
        in_str = ns;
        odd_bs = nb;
        if let Some(p) = close {
            acc = acc.wrapping_add(p as u64);
            break;
        }
    }
    acc
}

fn measure<F: FnMut() -> u64>(iterations: usize, mut f: F) -> f64 {
    // Warmup
    for _ in 0..(iterations / 10).max(1) {
        black_box(f());
    }
    let start = Instant::now();
    let mut acc: u64 = 0;
    for _ in 0..iterations {
        acc = acc.wrapping_add(f());
    }
    let elapsed_ns = start.elapsed().as_nanos() as f64;
    black_box(acc);
    elapsed_ns / iterations as f64
}

fn report_workload(name: &str, body: &[u8]) {
    let quotes = quote_positions_depth1(body);
    let pairs = quotes.len() / 2;
    let chunks_total = body.len() / 64;
    println!("\n=== {name} ===");
    println!(
        "  body: {} B, depth-1 quote pairs (≈ string members): {}, chunks (64B): {}",
        body.len(),
        pairs,
        chunks_total
    );

    let iterations = 200_000;

    // V0: full walk
    let v0_full_ns = measure(iterations, || bench_v0_baseline(body, &quotes, pairs));
    println!(
        "  V0 baseline (full walk, {} members): {:>8.1} ns/op  ({:.2} ns/member)",
        pairs,
        v0_full_ns,
        if pairs == 0 {
            0.0
        } else {
            v0_full_ns / pairs as f64
        }
    );

    // V0: 1, 2, 5, 10 members (early-exit cases)
    for &m in &[1usize, 2, 5, 10] {
        if m > pairs {
            continue;
        }
        let ns = measure(iterations, || bench_v0_baseline(body, &quotes, m));
        println!("    V0 early-exit at {m:>3} members: {ns:>8.1} ns/op");
    }

    // V1: full pre-scan
    let mut scratch: Vec<u32> = Vec::with_capacity(2048);
    let v1_ns = measure(iterations, || bench_v1_full_prescan(body, &mut scratch));
    let scratch_len_after = scratch.len();
    println!(
        "  V1 full pre-scan (whole body):       {v1_ns:>8.1} ns/op  ({scratch_len_after} offsets emitted)",
    );

    // V2: chunked pre-scan
    println!("  V2 chunked pre-scan (early-exit at K chunks):");
    for &k in &[1usize, 2, 4, 8, 16, usize::MAX] {
        let real_k = if k == usize::MAX { chunks_total + 1 } else { k };
        if real_k > 1 && (real_k - 1) * 64 > body.len() && k != usize::MAX {
            continue;
        }
        let label = if k == usize::MAX {
            "ALL"
        } else {
            &k.to_string()
        };
        let ns = measure(iterations, || bench_v2_chunked(body, &mut scratch, real_k));
        println!("    K={label:>3}: {ns:>8.1} ns/op");
    }

    // V3: streaming per-chunk
    println!("  V3 streaming (per-chunk scan_object_index) at K chunks:");
    for &k in &[1usize, 2, 4, 8, 16, usize::MAX] {
        let real_k = if k == usize::MAX { chunks_total + 1 } else { k };
        if real_k > 1 && (real_k - 1) * 64 > body.len() && k != usize::MAX {
            continue;
        }
        let label = if k == usize::MAX {
            "ALL"
        } else {
            &k.to_string()
        };
        let ns = measure(iterations, || {
            bench_v3_streaming(body, &mut scratch, real_k)
        });
        println!("    K={label:>3}: {ns:>8.1} ns/op");
    }

    // Summary cross-overs
    println!("  --- amortization summary ---");
    let v0_per_member = if pairs == 0 {
        0.0
    } else {
        v0_full_ns / pairs as f64
    };
    let break_even_members = if v0_per_member > 0.0 {
        (v1_ns / v0_per_member).ceil() as usize
    } else {
        0
    };
    println!(
        "  V1 break-even vs V0: ~{} members reached (V1 cost / V0 per-member cost)",
        break_even_members
    );
    println!(
        "  V1 - V0(full): {:+.1} ns ({:+.1}% of V0)",
        v1_ns - v0_full_ns,
        if v0_full_ns > 0.0 {
            (v1_ns / v0_full_ns - 1.0) * 100.0
        } else {
            0.0
        }
    );
}

fn main() {
    println!(
        "profile_prescan_strategies — Phase 1 alternative-strategy microbench\n\
         arch: {}, cpu_features: {}",
        std::env::consts::ARCH,
        cpu_features_brief(),
    );

    let w1 = build_walk_heavy(30);
    let w2 = build_walk_heavy(200);
    let w3 = build_status_outer();
    let w4 = build_long_strings(30); // values span chunks

    report_workload("W1: walk-heavy small (30 short string members)", &w1);
    report_workload("W2: walk-heavy large (200 short string members)", &w2);
    report_workload("W3: status-outer (5 mixed-type members)", &w3);
    report_workload("W4: walk-heavy long-string (30 members, ~80B values)", &w4);

    println!("\n=== Real status.json (whole event after the leading '{{') ===");
    if let Ok(bytes) = std::fs::read("testdata/status.json") {
        // Skip leading whitespace to first `{`
        let start = bytes
            .iter()
            .position(|&b| b == b'{')
            .map(|p| p + 1)
            .unwrap_or(0);
        report_workload("status.json (outer object body)", &bytes[start..]);
    } else {
        println!("  (testdata/status.json not found, skipping)");
    }
}

fn cpu_features_brief() -> String {
    #[cfg(target_arch = "aarch64")]
    {
        if std::arch::is_aarch64_feature_detected!("neon") {
            return "neon".into();
        }
    }
    #[cfg(target_arch = "x86_64")]
    {
        let mut feats = vec![];
        if std::arch::is_x86_feature_detected!("avx2") {
            feats.push("avx2");
        }
        if std::arch::is_x86_feature_detected!("sse4.2") {
            feats.push("sse4.2");
        }
        if !feats.is_empty() {
            return feats.join(",");
        }
    }
    "scalar".into()
}
