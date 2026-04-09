//! Visualizes where (and whether) lazy DFA provides benefit over NFA.
//!
//! All conclusions are derived from measured data — nothing is hardcoded.
//! N values are pushed high enough to actually trigger tier transitions.
//!
//! Run with: cargo run --release --example profile_lazy_sweet_spot

use std::hint::black_box;
use std::sync::Arc;
use std::time::{Duration, Instant};

use quamina::automaton::FieldMatcher;
use quamina::automaton::arena::{
    ArenaNfaBuffers, LazyDfa, StateArena, StateId, make_cidr_arena_fa, make_shellstyle_arena_fa,
    merge_arena_nfas, traverse_arena_dfa, traverse_arena_nfa, traverse_lazy_dfa,
};
use quamina::json::CidrPattern;
use quamina::regexp::{make_regexp_nfa_arena, parse_regexp};

// ============================================================================
// Constants + timing (mirrored from thread_safe.rs)
// ============================================================================

const EAGER_MULTIPLIER: usize = 8;
const EAGER_CAP: usize = 10_000;
const LAZY_CAP: usize = 100_000;

fn eager_budget(nfa_states: usize) -> usize {
    (nfa_states * EAGER_MULTIPLIER).min(EAGER_CAP)
}

const WARMUP: u64 = 100;
const ROUNDS: usize = 5;
const MIN_ROUND: Duration = Duration::from_millis(50);

fn bench<F: FnMut()>(mut f: F) -> u64 {
    for _ in 0..WARMUP {
        f();
    }
    let mut iters = 1u64;
    loop {
        let t = Instant::now();
        for _ in 0..iters {
            f();
        }
        if t.elapsed() >= MIN_ROUND {
            break;
        }
        iters = (iters * 4).min(10_000_000);
    }
    let mut samples = [0u64; ROUNDS];
    for s in &mut samples {
        let t = Instant::now();
        for _ in 0..iters {
            f();
        }
        *s = t.elapsed().as_nanos() as u64 / iters;
    }
    *samples.iter().min().unwrap()
}

fn encode_value(s: &str) -> Vec<u8> {
    let mut v = Vec::with_capacity(s.len() + 2);
    v.push(b'"');
    v.extend_from_slice(s.as_bytes());
    v.push(b'"');
    v
}

// ============================================================================
// Build helpers
// ============================================================================

/// Maps 0..17575 → "aaa".."zzz" (base-26 3-char keyword).
/// 26^3 = 17_576 unique patterns — enough to overflow the eager DFA budget
/// (trie DFA ≈ N states; eager cap = 10_000, so transition at N ≈ 9_300).
fn nth_keyword(n: usize) -> String {
    let c0 = n / 676;
    let c1 = (n / 26) % 26;
    let c2 = n % 26;
    format!(
        "{}{}{}",
        (b'a' + c0 as u8) as char,
        (b'a' + c1 as u8) as char,
        (b'a' + c2 as u8) as char,
    )
}

fn keyword_regexp_nfa(n: usize) -> (StateArena, StateId) {
    let p = nth_keyword(n);
    let root = parse_regexp(&p).expect("valid keyword regexp");
    let (mut a, s, _) = make_regexp_nfa_arena(root);
    a.precompute_epsilon_closures();
    (a, s)
}

/// Determine tier label by trying nfa_to_dfa at both budgets.
fn tier_label(arena: &StateArena, start: StateId) -> &'static str {
    let nfa_states = arena.len();
    let eb = eager_budget(nfa_states);
    if arena.nfa_to_dfa(start, eb).is_some() {
        return "eager";
    }
    if arena.nfa_to_dfa(start, LAZY_CAP).is_some() {
        return "lazy";
    }
    "nfa"
}

// ============================================================================
// Section 1: N merged 3-char keyword patterns
//
// 3-char a-z patterns form a trie DFA of size ≈ N+700. The DFA overflows
// EAGER_CAP around N ≈ 9 300. This section sweeps all the way to N = 12 000
// so we actually see the tier transition.
//
// Query is always "aaa" (pattern #0 — always present, always a match).
// NFA must check all N simultaneously-active states after reading the shared
// first byte(s). Lazy DFA only caches the one matching path (~4 states).
// ============================================================================

fn section1_keyword_scaling() {
    println!("=== Section 1: N merged 3-char keyword patterns (\"aaa\"..\"zzz\") ===");
    println!("  Query: always \"aaa\" (pattern #0, one active path out of N).");
    println!("  Trie DFA ≈ N+700 states → eager fails near N ≈ 9 300.");
    println!();
    println!(
        "  {:>6}  {:>10}  {:>12}  {:>12}  {:>12}  {:>7}  {}",
        "N", "NFA_states", "NFA_ns", "EagerDFA_ns", "LazyWarm_ns", "cached", "tier"
    );
    println!("  {}", "-".repeat(80));

    // Sample points chosen to bracket the tier transitions.
    let n_values: &[usize] = &[
        1, 10, 100, 500, 1_000, 2_000, 4_000, 6_000, 8_000, 9_000, 9_200, 9_400, 10_000, 11_000,
        12_000,
    ];

    let query = encode_value("aaa");
    let mut merged = StateArena::new();
    let mut merged_start = StateId::NONE;
    let mut next_idx = 0;
    let max_n = *n_values.last().unwrap();

    for i in 0..max_n {
        let (a, s) = keyword_regexp_nfa(i);
        let (new, ns) = merge_arena_nfas(&merged, merged_start, &a, s);
        merged = new;
        merged_start = ns;

        let n = i + 1;
        if next_idx < n_values.len() && n == n_values[next_idx] {
            next_idx += 1;

            let nfa_states = merged.len();
            let eb = eager_budget(nfa_states);

            // NFA
            let mut frozen = merged.clone();
            frozen.flatten_tables();
            let mut bufs = ArenaNfaBuffers::new();
            let nfa_ns = bench(|| {
                traverse_arena_nfa(
                    black_box(&frozen),
                    black_box(merged_start),
                    black_box(&query),
                    &mut bufs,
                );
            });

            // Eager DFA (if it fits)
            let eager_str = match merged.nfa_to_dfa(merged_start, eb) {
                Some((mut dfa, ds)) => {
                    dfa.flatten_tables();
                    let mut t = Vec::new();
                    let ns = bench(|| {
                        t.clear();
                        traverse_arena_dfa(
                            black_box(&dfa),
                            black_box(ds),
                            black_box(&query),
                            &mut t,
                        );
                    });
                    format!("{} ns", ns)
                }
                None => "N/A".to_string(),
            };

            // Lazy DFA — warm
            let mut lazy = LazyDfa::new(merged.clone(), merged_start, LAZY_CAP);
            let mut t = Vec::new();
            for _ in 0..WARMUP {
                t.clear();
                traverse_lazy_dfa(&mut lazy, &query, &mut t);
            }
            let cached = lazy.cached_count();
            let lazy_ns = bench(|| {
                t.clear();
                traverse_lazy_dfa(black_box(&mut lazy), black_box(&query), &mut t);
            });

            let tier = tier_label(&merged, merged_start);

            println!(
                "  {:>6}  {:>10}  {:>12}  {:>12}  {:>12}  {:>7}  {}",
                n,
                nfa_states,
                format!("{} ns", nfa_ns),
                eager_str,
                format!("{} ns", lazy_ns),
                cached,
                tier,
            );
        }
    }
    println!();
}

// ============================================================================
// Section 2: N merged CIDR rules (same /16 prefix, different /24 subnets)
//
// Rules: 10.0.0.0/24, 10.0.1.0/24, ..., 10.0.(N-1).0/24
// All share the "10.0." prefix so DFA cannot disambiguate early.
// Each /24 NFA is individually large due to range-checking epsilon chains.
//
// Query: 10.0.0.1 (matches rule 0 only — one path out of N).
// ============================================================================

fn section2_cidr_same_prefix() {
    println!("=== Section 2: N merged CIDR /24 rules within 10.0.x.0/24 ===");
    println!("  All rules share \"10.0.\" prefix — DFA cannot disambiguate until byte 5.");
    println!("  Query: always 10.0.0.1 (matches rule 0 only).");
    println!();
    println!(
        "  {:>5}  {:>10}  {:>12}  {:>12}  {:>12}  {:>7}  {}",
        "N", "NFA_states", "NFA_ns", "EagerDFA_ns", "LazyWarm_ns", "cached", "tier"
    );
    println!("  {}", "-".repeat(80));

    let n_values: &[usize] = &[1, 2, 3, 5, 8, 10, 15, 20, 30, 50, 75, 100, 150, 200, 255];
    let query = encode_value("10.0.0.1");

    let mut merged = StateArena::new();
    let mut merged_start = StateId::NONE;
    let mut next_idx = 0;
    let max_n = *n_values.last().unwrap();

    for i in 0..max_n {
        let cidr_str = format!("10.0.{}.0/24", i);
        let cidr = CidrPattern::parse(&cidr_str).expect("valid CIDR");
        let nf = Arc::new(FieldMatcher::new());
        let (a, s) = make_cidr_arena_fa(&cidr, nf);
        let (new, ns) = merge_arena_nfas(&merged, merged_start, &a, s);
        merged = new;
        merged_start = ns;

        let n = i + 1;
        if next_idx < n_values.len() && n == n_values[next_idx] {
            next_idx += 1;

            let nfa_states = merged.len();
            let eb = eager_budget(nfa_states);

            // NFA
            let mut frozen = merged.clone();
            frozen.flatten_tables();
            let mut bufs = ArenaNfaBuffers::new();
            let nfa_ns = bench(|| {
                traverse_arena_nfa(
                    black_box(&frozen),
                    black_box(merged_start),
                    black_box(&query),
                    &mut bufs,
                );
            });

            // Eager DFA
            let eager_str = match merged.nfa_to_dfa(merged_start, eb) {
                Some((mut dfa, ds)) => {
                    dfa.flatten_tables();
                    let mut t = Vec::new();
                    let ns = bench(|| {
                        t.clear();
                        traverse_arena_dfa(
                            black_box(&dfa),
                            black_box(ds),
                            black_box(&query),
                            &mut t,
                        );
                    });
                    format!("{} ns", ns)
                }
                None => "N/A".to_string(),
            };

            // Lazy DFA — warm
            let mut lazy = LazyDfa::new(merged.clone(), merged_start, LAZY_CAP);
            let mut t = Vec::new();
            for _ in 0..WARMUP {
                t.clear();
                traverse_lazy_dfa(&mut lazy, &query, &mut t);
            }
            let cached = lazy.cached_count();
            let lazy_ns = bench(|| {
                t.clear();
                traverse_lazy_dfa(black_box(&mut lazy), black_box(&query), &mut t);
            });

            let tier = tier_label(&merged, merged_start);

            println!(
                "  {:>5}  {:>10}  {:>12}  {:>12}  {:>12}  {:>7}  {}",
                n,
                nfa_states,
                format!("{} ns", nfa_ns),
                eager_str,
                format!("{} ns", lazy_ns),
                cached,
                tier,
            );
        }
    }
    println!();
}

// ============================================================================
// Section 3: N merged shellstyle patterns
//
// Shellstyle "*kw_NNN" creates epsilon cycles → DFA conversion never terminates.
// This shows the "nfa only" regime: neither eager nor lazy can convert.
// Even a single shellstyle pattern may push into lazy/nfa quickly when merged.
// ============================================================================

fn section3_shellstyle_scaling() {
    println!("=== Section 3: N merged shellstyle patterns (*kw_NNN) ===");
    println!("  Shellstyle \"*\" creates epsilon cycles — DFA never converges.");
    println!("  Shows the regime where neither eager nor lazy DFA can help.");
    println!();
    println!(
        "  {:>4}  {:>10}  {:>12}  {:>12}  {:>12}  {:>7}  {}",
        "N", "NFA_states", "NFA_ns", "EagerDFA_ns", "LazyWarm_ns", "cached", "tier"
    );
    println!("  {}", "-".repeat(80));

    let n_values: &[usize] = &[1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20];
    let query = encode_value("kw_0001");

    let mut merged = StateArena::new();
    let mut merged_start = StateId::NONE;
    let mut next_idx = 0;
    let max_n = *n_values.last().unwrap();

    for i in 0..max_n {
        let pattern = format!("*kw_{:04}", i + 1);
        let nf = Arc::new(FieldMatcher::new());
        let (a, s) = make_shellstyle_arena_fa(pattern.as_bytes(), nf);
        let (new, ns) = merge_arena_nfas(&merged, merged_start, &a, s);
        merged = new;
        merged_start = ns;

        let n = i + 1;
        if next_idx < n_values.len() && n == n_values[next_idx] {
            next_idx += 1;

            let nfa_states = merged.len();
            let eb = eager_budget(nfa_states);

            // NFA
            let mut frozen = merged.clone();
            frozen.flatten_tables();
            let mut bufs = ArenaNfaBuffers::new();
            let nfa_ns = bench(|| {
                traverse_arena_nfa(
                    black_box(&frozen),
                    black_box(merged_start),
                    black_box(&query),
                    &mut bufs,
                );
            });

            // Eager DFA (with timeout guard: if it fails quickly, report N/A)
            let eager_str = match merged.nfa_to_dfa(merged_start, eb) {
                Some((mut dfa, ds)) => {
                    dfa.flatten_tables();
                    let mut t = Vec::new();
                    let ns = bench(|| {
                        t.clear();
                        traverse_arena_dfa(
                            black_box(&dfa),
                            black_box(ds),
                            black_box(&query),
                            &mut t,
                        );
                    });
                    format!("{} ns", ns)
                }
                None => "N/A".to_string(),
            };

            // Lazy DFA
            let lazy_budget_val = (nfa_states * EAGER_MULTIPLIER * 10).min(LAZY_CAP);
            let mut lazy = LazyDfa::new(merged.clone(), merged_start, lazy_budget_val);
            let mut t = Vec::new();
            for _ in 0..WARMUP {
                t.clear();
                traverse_lazy_dfa(&mut lazy, &query, &mut t);
            }
            let cached = lazy.cached_count();
            let lazy_ns = bench(|| {
                t.clear();
                traverse_lazy_dfa(black_box(&mut lazy), black_box(&query), &mut t);
            });

            let tier = tier_label(&merged, merged_start);

            println!(
                "  {:>4}  {:>10}  {:>12}  {:>12}  {:>12}  {:>7}  {}",
                n,
                nfa_states,
                format!("{} ns", nfa_ns),
                eager_str,
                format!("{} ns", lazy_ns),
                cached,
                tier,
            );
        }
    }
    println!();
}

// ============================================================================
// Section 4: Is "N separate rules for the same field" == "one big alternation"?
//
// Compares merging N individual regexp NFAs vs. building one alternation regexp.
// Tests whether NFA state count and match time are equivalent.
// ============================================================================

fn section4_alternation_vs_merge() {
    println!("=== Section 4: N separate rules vs. one big alternation regexp ===");
    println!("  Comparing {{\"f\":\"aaa\"}},{{\"f\":\"aab\"}},...,{{\"f\":\"<Nth>\"}}");
    println!("  vs. {{\"f\":\"aaa|aab|...|<Nth>\"}} as a single pattern.");
    println!();
    println!(
        "  {:>6}  {:>18}  {:>18}  {:>14}  {:>14}",
        "N", "merge_NFA_states", "altern_NFA_states", "merge_NFA_ns", "altern_NFA_ns"
    );
    println!("  {}", "-".repeat(80));

    let n_values: &[usize] = &[5, 10, 25, 50, 100, 200, 500];
    let query = encode_value("aaa");

    for &n in n_values {
        // Approach A: N merged individual NFAs
        let mut merged = StateArena::new();
        let mut merged_start = StateId::NONE;
        for i in 0..n {
            let (a, s) = keyword_regexp_nfa(i);
            let (new, ns) = merge_arena_nfas(&merged, merged_start, &a, s);
            merged = new;
            merged_start = ns;
        }
        let merged_states = merged.len();
        let mut frozen_m = merged.clone();
        frozen_m.flatten_tables();
        let mut bufs = ArenaNfaBuffers::new();
        let merge_ns = bench(|| {
            traverse_arena_nfa(
                black_box(&frozen_m),
                black_box(merged_start),
                black_box(&query),
                &mut bufs,
            );
        });

        // Approach B: single alternation regexp
        let altern_pattern: String = (0..n).map(nth_keyword).collect::<Vec<_>>().join("|");
        let root = parse_regexp(&altern_pattern).expect("valid alternation");
        let (mut altern_nfa, altern_start, _) = make_regexp_nfa_arena(root);
        altern_nfa.precompute_epsilon_closures();
        let altern_states = altern_nfa.len();
        altern_nfa.flatten_tables();
        let altern_ns = bench(|| {
            traverse_arena_nfa(
                black_box(&altern_nfa),
                black_box(altern_start),
                black_box(&query),
                &mut bufs,
            );
        });

        println!(
            "  {:>6}  {:>18}  {:>18}  {:>14}  {:>14}",
            n,
            merged_states,
            altern_states,
            format!("{} ns", merge_ns),
            format!("{} ns", altern_ns),
        );
    }
    println!();
}

fn main() {
    section1_keyword_scaling();
    section2_cidr_same_prefix();
    section3_shellstyle_scaling();
    section4_alternation_vs_merge();
}
