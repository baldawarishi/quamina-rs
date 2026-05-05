//! Performance-oriented budget tuning for the NFA→DFA two-tier strategy.
//!
//! Measures build time, memory, and match time per tier across all pattern types
//! (regexp, shellstyle, wildcard, CIDR) so the budget constants can be tuned
//! for real-world performance:
//!
//!   EAGER_DFA_BUDGET_MULTIPLIER = 8,  EAGER_DFA_BUDGET_CAP = 10_000
//!
//! Run with: cargo run --release --example profile_budget_tuning

use std::hint::black_box;
use std::sync::Arc;
use std::time::{Duration, Instant};

use quamina::automaton::FieldMatcher;
use quamina::automaton::arena::{
    NfaBuffers, StateArena, StateId, make_cidr_arena_fa, make_shellstyle_arena_fa,
    make_wildcard_arena_fa, merge_arena_nfas, traverse_arena_dfa, traverse_arena_nfa,
};
use quamina::json::CidrPattern;
use quamina::regexp::{make_regexp_nfa_arena, parse_regexp};

// ============================================================================
// Budget constants (mirrored from src/automaton/thread_safe.rs)
// Keep in sync with EAGER_DFA_BUDGET_MULTIPLIER and EAGER_DFA_BUDGET_CAP there.
// ============================================================================

const EAGER_MULTIPLIER: usize = 8;
const EAGER_CAP: usize = 10_000;

fn eager_budget(nfa_states: usize) -> usize {
    (nfa_states * EAGER_MULTIPLIER).min(EAGER_CAP)
}

// ============================================================================
// Timing
// ============================================================================

/// Number of warmup passes before any timed measurement.
const WARMUP: u64 = 100;
/// Number of independent measurement rounds; we report min and max.
const ROUNDS: usize = 5;
/// Each round runs for at least this long to amortize OS timer resolution.
const MIN_ROUND: Duration = Duration::from_millis(50);

/// Run `WARMUP` calls of `f`, then measure `ROUNDS` independent rounds each
/// lasting at least `MIN_ROUND`.  Returns `(min_ns, max_ns)`.
///
/// `min` eliminates OS preemptions and cold-cache effects.
/// `max` exposes jitter: if max/min > 1.1 the measurement environment is noisy.
fn bench<F: FnMut()>(mut f: F) -> (u64, u64) {
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
        // ns/iter, computed in u128 to avoid u128→u64 truncation, then
        // narrowed back via try_from once we know the value is small.
        let ns_per_iter = t.elapsed().as_nanos() / u128::from(iters);
        *s = u64::try_from(ns_per_iter).expect("per-iter ns easily fits in u64");
    }
    let min = *samples.iter().min().unwrap();
    let max = *samples.iter().max().unwrap();
    (min, max)
}

/// Format a `(min, max)` pair.  Flag with `!` when jitter exceeds 10%.
fn fmt_ns(min: u64, max: u64) -> String {
    // Integer percent: (max - min) * 100 / min. `min` is non-zero in
    // practice; guard with `.max(1)` so divide-by-zero never panics.
    let jitter_pct = max.saturating_sub(min).saturating_mul(100) / min.max(1);
    if jitter_pct > 10 {
        format!("{min}ns!({jitter_pct}%)")
    } else {
        format!("{min}ns")
    }
}

// ============================================================================
// Pattern kinds and test cases
// ============================================================================

#[derive(Copy, Clone)]
enum PatternKind {
    Regexp(&'static str),
    Shellstyle(&'static str),
    Wildcard(&'static str),
    Cidr(&'static str),
}

struct PatternCase {
    label: &'static str,
    kind: PatternKind,
    values: &'static [&'static str],
}

impl PatternCase {
    const fn kind_name(&self) -> &'static str {
        match self.kind {
            PatternKind::Regexp(_) => "regexp",
            PatternKind::Shellstyle(_) => "shell",
            PatternKind::Wildcard(_) => "wildcard",
            PatternKind::Cidr(_) => "cidr",
        }
    }
}

fn all_patterns() -> Vec<PatternCase> {
    vec![
        // --- Group A: CommonRegex —— real-world structural patterns ---
        PatternCase {
            label: "email",
            kind: PatternKind::Regexp("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+~.[a-zA-Z]{2,4}"),
            values: &["user@example.com", "test@test.org", "admin@co.uk"],
        },
        PatternCase {
            label: "ipv4_regex",
            kind: PatternKind::Regexp("[0-9]{1,3}~.[0-9]{1,3}~.[0-9]{1,3}~.[0-9]{1,3}"),
            values: &["192.168.0.1", "10.0.0.1", "255.255.255.0"],
        },
        PatternCase {
            label: "hex_color",
            kind: PatternKind::Regexp("#[0-9a-fA-F]{6}"),
            values: &["#ff0000", "#00ff00", "#0000ff"],
        },
        PatternCase {
            label: "iso_date",
            kind: PatternKind::Regexp("[0-9]{4}-[0-9]{2}-[0-9]{2}"),
            values: &["2024-01-15", "2023-12-31", "2026-04-08"],
        },
        // --- Group B: rebar benchmarks —— string matching classics ---
        PatternCase {
            label: "alternation",
            kind: PatternKind::Regexp("Sher[a-z]+|Hol[a-z]+"),
            values: &["Sherlock", "Holmes", "Holmesford"],
        },
        PatternCase {
            label: "suffix_ing",
            kind: PatternKind::Regexp("[a-zA-Z]+ing"),
            values: &["running", "testing", "walking"],
        },
        PatternCase {
            label: "negated_fixed",
            kind: PatternKind::Regexp("[a-q][^u-z]{13}x"),
            values: &["aabcdefghijklmx", "babcdefghijklmx"],
        },
        // --- Group C: OWASP ReDoS patterns —— NFA state explosion ---
        PatternCase {
            label: "owasp_aplus",
            kind: PatternKind::Regexp("(a+)+"),
            values: &["aaa", "aaaaaaa", "aaaaaaaaaaa"],
        },
        PatternCase {
            label: "owasp_alt",
            kind: PatternKind::Regexp("(a|aa)+"),
            values: &["aaa", "aaaa", "aaaaa"],
        },
        PatternCase {
            label: "owasp_cc",
            kind: PatternKind::Regexp("([a-z]+)+"),
            values: &["abc", "abcdef", "abcdefghi"],
        },
        // --- Group D: Shellstyle patterns ---
        PatternCase {
            label: "shell_error",
            kind: PatternKind::Shellstyle("*error*"),
            values: &["fatal error", "error occurred", "no errors found"],
        },
        PatternCase {
            label: "shell_ext",
            kind: PatternKind::Shellstyle("*.log"),
            values: &["app.log", "error.log", "system.log"],
        },
        PatternCase {
            label: "shell_abc",
            kind: PatternKind::Shellstyle("*a*b*c*"),
            values: &["abc", "xaybzc", "aXbYcZ"],
        },
        // --- Group E: Wildcard patterns ---
        PatternCase {
            label: "wild_prefix",
            kind: PatternKind::Wildcard("error*"),
            values: &["error", "error occurred", "errors found"],
        },
        PatternCase {
            label: "wild_suffix",
            kind: PatternKind::Wildcard("*critical"),
            values: &["critical", "system critical", "high critical"],
        },
        // --- Group F: CIDR patterns —— epsilon-transition-based matching ---
        PatternCase {
            label: "cidr_192_16",
            kind: PatternKind::Cidr("192.168.0.0/16"),
            values: &["192.168.0.1", "192.168.1.1", "192.168.255.254"],
        },
        PatternCase {
            label: "cidr_10_8",
            kind: PatternKind::Cidr("10.0.0.0/8"),
            values: &["10.0.0.1", "10.1.2.3", "10.255.255.255"],
        },
    ]
}

// ============================================================================
// Build helpers
// ============================================================================

/// Build arena with closures + flat tables — ready for NFA traversal.
fn build_kind_frozen(kind: PatternKind) -> (StateArena, StateId) {
    let (mut arena, start) = build_kind(kind);
    arena.flatten_tables();
    (arena, start)
}

/// Build arena with closures only (no flat tables) — ready for nfa_to_dfa.
///
/// Shellstyle/wildcard/CIDR factory functions call `precompute_epsilon_closures`
/// internally; regexp requires an explicit call.
fn build_kind(kind: PatternKind) -> (StateArena, StateId) {
    match kind {
        PatternKind::Regexp(p) => {
            let root = parse_regexp(p).expect("valid regexp");
            let (mut arena, start, _) = make_regexp_nfa_arena(root);
            arena.precompute_epsilon_closures();
            (arena, start)
        }
        PatternKind::Shellstyle(p) => {
            let nf = Arc::new(FieldMatcher::new());
            make_shellstyle_arena_fa(p.as_bytes(), nf)
        }
        PatternKind::Wildcard(p) => {
            let nf = Arc::new(FieldMatcher::new());
            make_wildcard_arena_fa(p.as_bytes(), nf)
        }
        PatternKind::Cidr(p) => {
            let cidr = CidrPattern::parse(p).expect("valid CIDR");
            let nf = Arc::new(FieldMatcher::new());
            make_cidr_arena_fa(&cidr, nf)
        }
    }
}

/// Encode a string value with surrounding quotes, as the flattener provides.
fn encode_value(s: &str) -> Vec<u8> {
    let mut v = Vec::with_capacity(s.len() + 2);
    v.push(b'"');
    v.extend_from_slice(s.as_bytes());
    v.push(b'"');
    v
}

// ============================================================================
// Section 1: Per-pattern tier comparison (build time, memory, match time)
// ============================================================================

fn section1_tier_comparison() {
    println!("=== Section 1: Per-pattern performance profile (build + memory + match) ===");
    println!("  warm_ns  = steady-state: pre-warmed {WARMUP} passes, then min of {ROUNDS} rounds");
    println!("  !        = >10% jitter between rounds (noisy measurement)");
    println!();

    for case in &all_patterns() {
        let encoded: Vec<Vec<u8>> = case.values.iter().map(|v| encode_value(v)).collect();
        let (frozen_nfa, nfa_start) = build_kind_frozen(case.kind);
        let nfa_states = frozen_nfa.len();
        let nfa_mem_kb = frozen_nfa.estimated_byte_size() / 1024;
        let eb = eager_budget(nfa_states);

        println!(
            "Pattern [{:<8}] {:<25}  NFA: {} states  eager_budget: {}",
            case.kind_name(),
            case.label,
            nfa_states,
            eb,
        );

        // NFA build + match
        let (nfa_build_min, nfa_build_max) = bench(|| {
            let _ = black_box(build_kind_frozen(black_box(case.kind)));
        });
        let mut nfa_bufs = NfaBuffers::new();
        let (nfa_match_min, nfa_match_max) = bench(|| {
            for ev in &encoded {
                traverse_arena_nfa(
                    black_box(&frozen_nfa),
                    black_box(nfa_start),
                    black_box(ev),
                    &mut nfa_bufs,
                );
            }
        });
        println!(
            "  NFA      build: {:>12}  match(warm): {:>14}  mem: {:>3} KB",
            fmt_ns(nfa_build_min, nfa_build_max),
            fmt_ns(nfa_match_min, nfa_match_max),
            nfa_mem_kb,
        );

        // Eager DFA
        let (nfa_for_dfa, start_for_dfa) = build_kind(case.kind);
        if let Some((mut dfa_arena, dfa_start)) = nfa_for_dfa.nfa_to_dfa(start_for_dfa, eb) {
            dfa_arena.flatten_tables();
            let dfa_mem_kb = dfa_arena.estimated_byte_size() / 1024;
            let (dfa_build_min, dfa_build_max) = bench(|| {
                let (nfa2, s2) = build_kind(black_box(case.kind));
                let _ = black_box(nfa2.nfa_to_dfa(black_box(s2), eb));
            });
            let mut dfa_t = Vec::new();
            let (dfa_match_min, dfa_match_max) = bench(|| {
                for ev in &encoded {
                    dfa_t.clear();
                    traverse_arena_dfa(
                        black_box(&dfa_arena),
                        black_box(dfa_start),
                        black_box(ev),
                        &mut dfa_t,
                    );
                }
            });
            // Speedup as integer tenths (e.g. "5.4x") computed without f64.
            // Guard against divide-by-zero when DFA timing rounds to 0ns.
            let speedup_tenths = nfa_match_min.saturating_mul(10) / dfa_match_min.max(1);
            println!(
                "  EagerDFA build: {:>12}  match(warm): {:>14}  mem: {:>3} KB  ({}.{}x faster than NFA)",
                fmt_ns(dfa_build_min, dfa_build_max),
                fmt_ns(dfa_match_min, dfa_match_max),
                dfa_mem_kb,
                speedup_tenths / 10,
                speedup_tenths % 10,
            );
        } else {
            println!("  EagerDFA not convertible at budget {eb}");
        }

        println!();
    }
}

// ============================================================================
// Section 2: Budget multiplier sensitivity sweep
//
// For each pattern, try budget multipliers [1×, 2×, 4×, 8×, 16×] and record
// whether the DFA conversion succeeds, how many DFA states are produced, and
// the match time. This informs the choice of EAGER_MULTIPLIER.
// ============================================================================

fn section2_budget_sensitivity() {
    println!("=== Section 2: Budget multiplier sensitivity (DFA state count vs match time) ===");
    println!("  Columns: NFA_n, then for each multiplier [1×,2×,4×,8×,16×]: ok/-- and match ns");
    println!();

    let multipliers: &[usize] = &[1, 2, 4, 8, 16];

    println!(
        "{:<18}  {:<8}  {:>6}    {}",
        "label",
        "kind",
        "NFA_n",
        multipliers
            .iter()
            .map(|m| format!("{:>14}", format!("{}×", m)))
            .collect::<Vec<_>>()
            .join("  "),
    );
    println!("{}", "-".repeat(120));

    for case in &all_patterns() {
        let encoded: Vec<Vec<u8>> = case.values.iter().map(|v| encode_value(v)).collect();
        let (nfa_for_dfa, start_for_dfa) = build_kind(case.kind);
        let nfa_states = nfa_for_dfa.len();

        let mut cols: Vec<String> = Vec::new();
        for &mult in multipliers {
            let budget = (nfa_states * mult).min(EAGER_CAP);
            if let Some((mut dfa, dfa_start)) = nfa_for_dfa.nfa_to_dfa(start_for_dfa, budget) {
                let dfa_states = dfa.len();
                dfa.flatten_tables();
                let mut dfa_t = Vec::new();
                let (ns, _) = bench(|| {
                    for ev in &encoded {
                        dfa_t.clear();
                        traverse_arena_dfa(
                            black_box(&dfa),
                            black_box(dfa_start),
                            black_box(ev),
                            &mut dfa_t,
                        );
                    }
                });
                cols.push(format!("ok({dfa_states:4}st) {ns:4}ns"));
            } else {
                cols.push(format!("{:>14}", "--"));
            }
        }

        println!(
            "{:<18}  {:<8}  {:>6}    {}",
            case.label,
            case.kind_name(),
            nfa_states,
            cols.join("  "),
        );
    }

    println!();
}

// ============================================================================
// Section 3: Multi-pattern (merged) build time + match time cost curve
// ============================================================================

fn section3_merged_performance() {
    println!("=== Section 3: Multi-pattern build + match time as patterns are added ===");
    println!("  Patterns ordered simple→complex to sweep through all two tiers.");
    println!();

    let mut all_arenas: Vec<(StateArena, StateId, String)> = Vec::new();

    // Group 1: simple regexp — each adds a few NFA states, DFA stays tiny (expect: eager)
    for (label, pat) in [
        ("re:[0-9]{4}", "[0-9]{4}"),
        ("re:[a-z]+", "[a-z]+"),
        ("re:[A-Z][a-z]+", "[A-Z][a-z]+"),
        ("re:abc|def|ghi", "abc|def|ghi"),
    ] {
        let root = parse_regexp(pat).expect("valid regexp");
        let (mut arena, start, _) = make_regexp_nfa_arena(root);
        arena.precompute_epsilon_closures();
        all_arenas.push((arena, start, label.to_string()));
    }

    // Group 2: anchored wildcards — no epsilon cycles, DFA stays small (expect: eager)
    for (label, pat) in [
        ("wc:abc*", "abc*"),
        ("wc:*xyz", "*xyz"),
        ("wc:log*", "log*"),
        ("wc:*error", "*error"),
    ] {
        let nf = Arc::new(FieldMatcher::new());
        let (arena, start) = make_wildcard_arena_fa(pat.as_bytes(), nf);
        all_arenas.push((arena, start, label.to_string()));
    }

    // Group 3: 2-wildcard shellstyle — product DFA can overflow eager budget (expect: nfa)
    for (label, pat) in [
        ("sh:*ab*", "*ab*"),
        ("sh:*cd*", "*cd*"),
        ("sh:*ef*", "*ef*"),
        ("sh:*gh*", "*gh*"),
        ("sh:*ij*", "*ij*"),
        ("sh:*kl*", "*kl*"),
    ] {
        let nf = Arc::new(FieldMatcher::new());
        let (arena, start) = make_shellstyle_arena_fa(pat.as_bytes(), nf);
        all_arenas.push((arena, start, label.to_string()));
    }

    // Group 4: 3-wildcard shellstyle — large DFA, too large for eager DFA (expect: nfa)
    for (label, pat) in [
        ("sh:*a*b*c*", "*a*b*c*"),
        ("sh:*x*y*z*", "*x*y*z*"),
        ("sh:*e*f*g*", "*e*f*g*"),
        ("sh:*m*n*o*", "*m*n*o*"),
    ] {
        let nf = Arc::new(FieldMatcher::new());
        let (arena, start) = make_shellstyle_arena_fa(pat.as_bytes(), nf);
        all_arenas.push((arena, start, label.to_string()));
    }

    // Group 5: complex regexp (expect: nfa)
    for (label, pat) in [("re:(a+)+", "(a+)+"), ("re:([a-z]+)+", "([a-z]+)+")] {
        let root = parse_regexp(pat).expect("valid regexp");
        let (mut arena, start, _) = make_regexp_nfa_arena(root);
        arena.precompute_epsilon_closures();
        all_arenas.push((arena, start, label.to_string()));
    }

    let match_values: &[&str] = &[
        "1234", "hello", "Hello", "abc", "abcdef", "xyzabc", "abcghijk", "aaaa",
    ];
    let encoded: Vec<Vec<u8>> = match_values.iter().map(|v| encode_value(v)).collect();

    println!(
        "{:>5}  {:<20}  {:>10}  {:>8}  {:>12}  tier",
        "#", "added", "NFA_states", "mem_KB", "NFA_ns"
    );
    println!("{}", "-".repeat(65));

    let mut merged_arena = StateArena::new();
    let mut merged_start = StateId::NONE;
    let mut nfa_bufs = NfaBuffers::new();

    for (i, (arena, start, label)) in all_arenas.iter().enumerate() {
        let (new_arena, new_start) = merge_arena_nfas(&merged_arena, merged_start, arena, *start);
        merged_arena = new_arena;
        merged_start = new_start;

        let mut frozen = merged_arena.clone();
        frozen.flatten_tables();

        let nfa_states = frozen.len();
        let mem_kb = frozen.estimated_byte_size() / 1024;
        let eb = eager_budget(nfa_states);

        let (nfa_min, nfa_max) = bench(|| {
            for ev in &encoded {
                traverse_arena_nfa(
                    black_box(&frozen),
                    black_box(merged_start),
                    black_box(ev),
                    &mut nfa_bufs,
                );
            }
        });

        let tier = if frozen.nfa_to_dfa(merged_start, eb).is_some() {
            "eager"
        } else {
            "nfa"
        };

        println!(
            "{:>5}  {:<20}  {:>10}  {:>8}  {:>12}  {}",
            i + 1,
            label,
            nfa_states,
            mem_kb,
            fmt_ns(nfa_min, nfa_max),
            tier,
        );
    }

    println!();
}

// ============================================================================
// Section 4: Memory vs match-time tradeoff summary (all pattern types)
// ============================================================================

fn section4_tradeoff_summary() {
    println!("=== Section 4: Memory vs match-time tradeoff summary (all pattern types) ===");
    println!();

    println!(
        "{:<18}  {:<8}  {:>6}  {:>9}  {:>9}  {:>9}  {:>9}",
        "label", "kind", "NFA_n", "NFA_KB", "NFA_ns", "DFA_KB", "DFA_ns",
    );
    println!("{}", "-".repeat(80));

    for case in &all_patterns() {
        let encoded: Vec<Vec<u8>> = case.values.iter().map(|v| encode_value(v)).collect();

        let (frozen_nfa, nfa_start) = build_kind_frozen(case.kind);
        let nfa_states = frozen_nfa.len();
        let nfa_mem_kb = frozen_nfa.estimated_byte_size() / 1024;
        let mut nfa_bufs = NfaBuffers::new();
        let (nfa_ns, _) = bench(|| {
            for ev in &encoded {
                traverse_arena_nfa(
                    black_box(&frozen_nfa),
                    black_box(nfa_start),
                    black_box(ev),
                    &mut nfa_bufs,
                );
            }
        });

        let eb = eager_budget(nfa_states);
        let (nfa_for_dfa, start_for_dfa) = build_kind(case.kind);
        let (dfa_mem_str, dfa_ns_str) =
            if let Some((mut dfa, dfa_start)) = nfa_for_dfa.nfa_to_dfa(start_for_dfa, eb) {
                dfa.flatten_tables();
                let mem = dfa.estimated_byte_size() / 1024;
                let mut dfa_t = Vec::new();
                let (ns, _) = bench(|| {
                    for ev in &encoded {
                        dfa_t.clear();
                        traverse_arena_dfa(
                            black_box(&dfa),
                            black_box(dfa_start),
                            black_box(ev),
                            &mut dfa_t,
                        );
                    }
                });
                (format!("{mem}"), format!("{ns} ns"))
            } else {
                ("N/A".to_string(), "N/A".to_string())
            };

        println!(
            "{:<18}  {:<8}  {:>6}  {:>9}  {:>9}  {:>9}  {:>9}",
            case.label,
            case.kind_name(),
            nfa_states,
            format!("{} KB", nfa_mem_kb),
            format!("{} ns", nfa_ns),
            dfa_mem_str,
            dfa_ns_str,
        );
    }

    println!();
    println!("Notes:");
    println!("  DFA = N/A when eager budget exceeded (EAGER_CAP={EAGER_CAP})");
    println!();
}

// ============================================================================
// Main
// ============================================================================

fn main() {
    section1_tier_comparison();
    section2_budget_sensitivity();
    section3_merged_performance();
    section4_tradeoff_summary();
}
