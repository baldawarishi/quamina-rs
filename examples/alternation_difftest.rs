//! Differential correctness check for the alternation branch-merge builder.
//!
//! Two phases, both comparing quamina's regexp match against the `regex` crate
//! as an oracle (anchored to a full match, matching quamina's quote-wrapped
//! value semantics):
//!   1. a curated battery of alternation edge cases, and
//!   2. a seeded fuzzer that generates random regexps over the syntax subset
//!      both engines agree on (literals, char classes, `.`, concatenation,
//!      alternation, groups, and `* + ? {n,m}` quantifiers).
//!
//! Exits non-zero on any mismatch. Run with:
//!   cargo run --release --example alternation_difftest

use quamina::Quamina;
use rand::{RngExt, SeedableRng, rngs::StdRng};
use regex::Regex;

const CURATED: &[&str] = &[
    "(a|b|c)",
    "(a|b|c)(d|e|f)(g|h|i)",
    "(foo|bar|baz)",
    "(a|ab|abc)",
    "(ab|cd|ef)+",
    "(a|b|)(c|d|)",
    "(a|b|)(c|d|)(e|f|)",
    "((a|b)|(c|d))((e|f)|(g|h))",
    "(a|bc)*",
    "(ab|a)(b|bc)",
    "x(a|b|c)*y",
    "(cat|car|cart|ca)",
    "(a|)+b",
    "(a|)*b",
    "((x|y)z|w)+",
    "(abc|ab|a)(c|cd)",
    "(|a)(|b)(|c)",
    "(a|b|c|d|e|f|g|h){2,3}",
    "((ab|cd)|(ef|gh))*",
    "(a(b|c)d|e(f|g)h)+",
];

fn json_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out
}

fn enumerate(alphabet: &[char], max_len: usize) -> Vec<String> {
    let mut all = vec![String::new()];
    let mut frontier = vec![String::new()];
    for _ in 0..max_len {
        let mut next = Vec::new();
        for s in &frontier {
            for &c in alphabet {
                let mut t = s.clone();
                t.push(c);
                next.push(t);
            }
        }
        all.extend(next.iter().cloned());
        frontier = next;
    }
    all
}

/// Check one pattern against the oracle over all `strings`. Returns the mismatch
/// count and prints the first few.
fn check(pat: &str, strings: &[String], shown: &mut usize) -> usize {
    let oracle = match Regex::new(&format!("^(?:{pat})$")) {
        Ok(r) => r,
        Err(_) => return 0, // not a valid oracle regex; skip
    };
    let mut q = Quamina::new();
    let pattern_json = format!(r#"{{"x": [{{"regexp": "{}"}}]}}"#, json_escape(pat));
    if q.add_pattern("p".to_string(), &pattern_json).is_err() {
        return 0; // quamina rejected the pattern; nothing to compare
    }

    let mut mismatches = 0usize;
    for s in strings {
        let event = format!(r#"{{"x":"{}"}}"#, json_escape(s));
        let q_match = !q.matches_for_event(event.as_bytes()).unwrap().is_empty();
        let o_match = oracle.is_match(s);
        if q_match != o_match {
            mismatches += 1;
            if *shown < 40 {
                *shown += 1;
                println!("MISMATCH pat={pat:?} value={s:?} quamina={q_match} oracle={o_match}");
            }
        }
    }
    mismatches
}

/// Generate a random regexp over {a,b,c} using a depth-bounded grammar shared by
/// both engines.
///
/// The grammar deliberately exercises the `.` atom and nests unbounded
/// quantifiers (`*`/`+`) inside one another, e.g. `(?:a*)+` — the shapes most
/// prone to over-matching in the NFA builder, where a quantifier's "match zero
/// copies" skip can leak across an inner loop's back-edge. The unit tests in
/// `tests_operators` cover the same shapes directly.
fn gen_regexp(rng: &mut StdRng, depth: u32) -> String {
    // At depth 0 only produce atoms to keep patterns bounded.
    let choice = if depth == 0 {
        rng.random_range(0usize..5)
    } else {
        rng.random_range(0usize..9)
    };
    match choice {
        0 => {
            // literal
            let c = ['a', 'b', 'c'][rng.random_range(0usize..3)];
            c.to_string()
        }
        1 | 2 => {
            // char class
            let opts = ["[ab]", "[bc]", "[abc]", "[ac]"];
            opts[rng.random_range(0usize..opts.len())].to_string()
        }
        3 => {
            // empty (only meaningful inside alternation/groups)
            String::new()
        }
        4 => {
            // dot (any character)
            ".".to_string()
        }
        5 => {
            // concatenation of 2-3 sub-expressions
            let n = 2 + rng.random_range(0usize..2);
            (0..n).map(|_| gen_regexp(rng, depth - 1)).collect()
        }
        6 => {
            // alternation of 2-4 branches (may include empties)
            let n = 2 + rng.random_range(0usize..3);
            let parts: Vec<String> = (0..n).map(|_| gen_regexp(rng, depth - 1)).collect();
            let joined = parts.join("|");
            format!("({joined})")
        }
        7 => {
            // quantified group; the body may itself contain quantifiers.
            let inner = gen_regexp(rng, depth - 1);
            let quantifiers = ["*", "+", "?", "{1,2}", "{0,2}", "{2,3}"];
            let q = quantifiers[rng.random_range(0usize..quantifiers.len())];
            format!("(?:{inner}){q}")
        }
        _ => {
            // group
            let inner = gen_regexp(rng, depth - 1);
            format!("({inner})")
        }
    }
}

fn main() {
    let alphabet: Vec<char> = "abcdefghixyzw".chars().collect();
    let curated_strings = enumerate(&alphabet, 4);
    println!(
        "phase 1 (curated): {} patterns x {} strings",
        CURATED.len(),
        curated_strings.len()
    );
    let mut shown = 0usize;
    let mut total = 0usize;
    for pat in CURATED {
        total += check(pat, &curated_strings, &mut shown);
    }

    // Phase 2: random fuzz over {a,b,c} with shorter strings (denser coverage
    // per pattern) but many patterns.
    let fuzz_alphabet: Vec<char> = "abc".chars().collect();
    let fuzz_strings = enumerate(&fuzz_alphabet, 5);
    let n_patterns = 20000;
    println!(
        "phase 2 (fuzz): {} patterns x {} strings",
        n_patterns,
        fuzz_strings.len()
    );
    let mut fuzz_checked = 0usize;
    for seed in [
        0x9e3779b97f4a7c15u64,
        0x0123_4567_89ab_cdef,
        0xdead_beef_cafe_f00d,
    ] {
        let mut rng = StdRng::seed_from_u64(seed);
        for _ in 0..n_patterns {
            let pat = gen_regexp(&mut rng, 4);
            if pat.is_empty() {
                continue;
            }
            fuzz_checked += 1;
            total += check(&pat, &fuzz_strings, &mut shown);
        }
    }

    println!("fuzz patterns actually checked: {fuzz_checked}");
    println!("total mismatches: {total}");
    if total != 0 {
        std::process::exit(1);
    }
    println!("OK: quamina agrees with oracle on all cases");
}
