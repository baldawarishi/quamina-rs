//! Benchmarks probing the NFA-vs-DFA tradeoff space for wildcard matching.
//!
//! Shellstyle wildcards compile to automata whose state space depends on the
//! pattern alphabet (a `*` can match any Unicode codepoint), while the bytes
//! actually traversed depend only on the input alphabet. These benchmarks
//! measure both ends of that spectrum: tiny-DFA patterns where eager DFA
//! conversion is trivially profitable, and wide patterns over narrow input
//! where a demand-driven (lazy) DFA only needs states for the bytes it
//! actually sees. Run with: cargo bench --bench regex_nfa_dfa

use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use quamina::Quamina;
use rand::rngs::StdRng;
use rand::{RngExt, SeedableRng};

/// Number of pre-built events each benchmark cycles through, so per-iteration
/// cost reflects a mix of inputs rather than a single hot event.
const POOL_SIZE: usize = 64;

/// Smaller pool for the narrow-input benchmarks, whose events are longer.
const NARROW_POOL_SIZE: usize = 32;

/// Adds a shellstyle pattern on the "val" field.
fn add_shellstyle(q: &mut Quamina<String>, id: String, shellstyle: &str) {
    let pattern = format!(r#"{{"val": [{{"shellstyle": "{shellstyle}"}}]}}"#);
    q.add_pattern(id, &pattern).unwrap();
}

/// Wraps a string value into a one-field JSON event.
fn make_event(value: &str) -> Vec<u8> {
    format!(r#"{{"val": "{value}"}}"#).into_bytes()
}

/// Appends `min + rand(0..extra)` characters drawn from `alphabet`.
fn push_filler(buf: &mut String, rng: &mut StdRng, alphabet: &[char], min: usize, extra: usize) {
    let n = min + rng.random_range(0..extra);
    for _ in 0..n {
        buf.push(alphabet[rng.random_range(0..alphabet.len())]);
    }
}

/// Verifies every event in the pool matches, then benchmarks matching one
/// event per iteration, cycling through the pool. Each iteration must match:
/// these benchmarks measure traversal cost, not match selectivity.
fn run_match_loop(c: &mut Criterion, name: &str, q: &Quamina<String>, events: &[Vec<u8>]) {
    for event in events {
        let matches = q.matches_for_event(event).unwrap();
        assert!(
            !matches.is_empty(),
            "no match for event: {}",
            String::from_utf8_lossy(event)
        );
    }

    c.bench_function(name, |b| {
        let mut i = 0usize;
        b.iter(|| {
            let matches = q
                .matches_for_event(black_box(&events[i % events.len()]))
                .unwrap();
            assert!(!matches.is_empty());
            i += 1;
        });
    });
}

/// Exercises patterns like "a*b" where the full DFA is tiny — just a handful
/// of states. An eager NFA-to-DFA conversion trivially handles these and
/// produces the fastest possible matcher; this benchmark shows what simple
/// wildcards gain from DFA treatment, whether eager or lazy.
fn bench_shellstyle_simple_wildcard(c: &mut Criterion) {
    // Simple prefix*suffix patterns — the DFA for each is ~3 states.
    let simple_patterns = ["a*b", "foo*bar", "x*y*z", "he*lo"];
    let lowercase: Vec<char> = ('a'..='z').collect();

    for shellstyle in simple_patterns {
        let mut q = Quamina::new();
        add_shellstyle(&mut q, shellstyle.to_string(), shellstyle);

        // Build events that match — random lowercase filler between the
        // fixed parts of the pattern (e.g. "a<random>b" for "a*b").
        let mut rng = StdRng::seed_from_u64(42);
        let events: Vec<Vec<u8>> = (0..POOL_SIZE)
            .map(|_| {
                let parts: Vec<&str> = shellstyle.split('*').collect();
                let mut buf = String::new();
                for (j, part) in parts.iter().enumerate() {
                    buf.push_str(part);
                    if j < parts.len() - 1 {
                        push_filler(&mut buf, &mut rng, &lowercase, 3, 15);
                    }
                }
                make_event(&buf)
            })
            .collect();

        run_match_loop(
            c,
            &format!("shellstyle_simple_wildcard/{shellstyle}"),
            &q,
            &events,
        );
    }
}

/// Creates shellstyle patterns whose wildcards can match almost any Unicode
/// codepoint, then benchmarks against input drawn from a tiny slice of the
/// alphabet. An eager DFA must construct states covering the full Unicode
/// byte space implied by "*"; a demand-driven approach only needs to
/// materialize states for the bytes actually encountered, making its
/// effective state space proportional to the input alphabet rather than the
/// pattern alphabet.
fn bench_shellstyle_narrow_input(c: &mut Criterion) {
    // Anchors are drawn from diverse Unicode blocks so the wildcard
    // transitions must accommodate the full UTF-8 encoding range. But the
    // text *between* the anchors in the input events only uses a narrow set.
    let anchor_sets: [(&str, &[&str]); 3] = [
        ("ascii_anchors", &["X", "Y", "Z", "W", "Q"]),
        ("cjk_anchors", &["東", "京", "北", "海", "山"]),
        ("mixed_script_anchors", &["A", "Ω", "东", "🎯", "Й"]),
    ];

    // The narrow input alphabets — the characters that fill in between anchors.
    let input_alphabets: [(&str, Vec<char>); 3] = [
        ("digits_only", "0123456789".chars().collect()),
        (
            "lowercase_ascii",
            "abcdefghijklmnopqrstuvwxyz".chars().collect(),
        ),
        ("narrow_cjk", "一二三四五六七八九十".chars().collect()),
    ];

    for (anchors_name, anchors) in anchor_sets {
        for (alphabet_name, alphabet) in &input_alphabets {
            for pattern_count in [8usize, 32, 128] {
                let name = format!(
                    "shellstyle_narrow_input/anchors={anchors_name}/input={alphabet_name}/patterns={pattern_count}"
                );

                let mut q = Quamina::new();
                let mut rng = StdRng::seed_from_u64(99);

                // Build patterns like *<anchor1>*<anchor2>* — each wildcard
                // can match any Unicode, but input will only contain chars
                // from the narrow alphabet.
                let mut pairs: Vec<(&str, &str)> = Vec::with_capacity(pattern_count);
                for i in 0..pattern_count {
                    let a1 = anchors[rng.random_range(0..anchors.len())];
                    let a2 = anchors[rng.random_range(0..anchors.len())];
                    pairs.push((a1, a2));
                    add_shellstyle(&mut q, format!("p{i}"), &format!("*{a1}*{a2}*"));
                }

                // Build events whose values contain anchor characters from an
                // actual pattern (so they're guaranteed to match), surrounded
                // by padding drawn exclusively from the narrow alphabet.
                let events: Vec<Vec<u8>> = (0..NARROW_POOL_SIZE)
                    .map(|_| {
                        let mut buf = String::new();
                        push_filler(&mut buf, &mut rng, alphabet, 5, 10);
                        let (a1, a2) = pairs[rng.random_range(0..pairs.len())];
                        buf.push_str(a1);
                        push_filler(&mut buf, &mut rng, alphabet, 5, 10);
                        buf.push_str(a2);
                        push_filler(&mut buf, &mut rng, alphabet, 5, 10);
                        make_event(&buf)
                    })
                    .collect();

                run_match_loop(c, &name, &q, &events);
            }
        }
    }
}

/// Focuses on scaling behavior as pattern count grows, with maximally broad
/// patterns (every "*" accepts all of Unicode) but input restricted to ASCII
/// digits. This isolates a demand-driven DFA's advantage: the cache only
/// needs entries for ~10 distinct byte values regardless of how many Unicode
/// codepoints the pattern theoretically permits.
fn bench_shellstyle_wide_patterns_scaling(c: &mut Criterion) {
    let digits: Vec<char> = "0123456789".chars().collect();

    // Anchors from multiple scripts force the automaton to have transitions
    // spanning the full UTF-8 byte range.
    let all_anchors = [
        "A", "B", "C", "D", "E", // Latin
        "Α", "Β", "Γ", "Δ", "Ε", // Greek
        "東", "京", "北", "上", "大", // CJK
        "🎯", "🚀", "🌟", "❤", "🎉", // Emoji
        "Д", "Ж", "З", "И", "К", // Cyrillic
    ];

    for pattern_count in [8usize, 16, 32, 64, 128, 256, 512] {
        let mut q = Quamina::new();
        let mut rng = StdRng::seed_from_u64(77);

        let mut pairs: Vec<(&str, &str)> = Vec::with_capacity(pattern_count);
        for i in 0..pattern_count {
            let a1 = all_anchors[rng.random_range(0..all_anchors.len())];
            let a2 = all_anchors[rng.random_range(0..all_anchors.len())];
            pairs.push((a1, a2));
            add_shellstyle(&mut q, format!("p{i}"), &format!("*{a1}*{a2}*"));
        }

        // Events use only ASCII digits as filler — the narrowest possible
        // byte alphabet (10 distinct values, all single-byte) — with two
        // anchors from an actual pattern embedded in the digit soup.
        let events: Vec<Vec<u8>> = (0..POOL_SIZE)
            .map(|_| {
                let mut buf = String::new();
                push_filler(&mut buf, &mut rng, &digits, 3, 5);
                let (a1, a2) = pairs[rng.random_range(0..pairs.len())];
                buf.push_str(a1);
                push_filler(&mut buf, &mut rng, &digits, 3, 5);
                buf.push_str(a2);
                push_filler(&mut buf, &mut rng, &digits, 3, 5);
                make_event(&buf)
            })
            .collect();

        run_match_loop(
            c,
            &format!("shellstyle_wide_patterns_scaling/patterns={pattern_count}"),
            &q,
            &events,
        );
    }
}

/// Adds multiple simple patterns to show that even a modest collection of
/// small-DFA patterns benefits from DFA conversion. Each pattern is
/// independent (different prefix/suffix), so the merged DFA stays small.
fn bench_shellstyle_simple_wildcard_scaling(c: &mut Criterion) {
    let prefixes = b"abcdefghijklmnopqrstuvwxyz";
    let suffixes = b"zyxwvutsrqponmlkjihgfedcba";
    let lowercase: Vec<char> = ('a'..='z').collect();

    for pattern_count in [1usize, 4, 8, 16, 26] {
        let mut q = Quamina::new();

        for i in 0..pattern_count {
            let shellstyle = format!("{}*{}", prefixes[i] as char, suffixes[i] as char);
            add_shellstyle(&mut q, format!("p{i}"), &shellstyle);
        }

        // Build events that match — each targets a random pattern.
        let mut rng = StdRng::seed_from_u64(42);
        let events: Vec<Vec<u8>> = (0..POOL_SIZE)
            .map(|_| {
                let idx = rng.random_range(0..pattern_count);
                let mut buf = String::new();
                buf.push(prefixes[idx] as char);
                push_filler(&mut buf, &mut rng, &lowercase, 5, 20);
                buf.push(suffixes[idx] as char);
                make_event(&buf)
            })
            .collect();

        run_match_loop(
            c,
            &format!("shellstyle_simple_wildcard_scaling/patterns={pattern_count}"),
            &q,
            &events,
        );
    }
}

/// Exercises NFA traversal on input containing ZWJ (Zero Width Joiner) emoji
/// sequences mixed with Japanese text. This is a demanding case for
/// byte-level automaton traversal because:
///
///  1. ZWJ emoji sequences encode a single visible glyph as many codepoints
///     joined by U+200D (ZWJ), producing 15-25+ bytes per "character".
///  2. The ZWJ byte sequence (0xE2 0x80 0x8D) shares its leading byte 0xE2
///     with hundreds of other BMP codepoints (U+2000-U+2FFF), so the NFA
///     cannot tell if 0xE2 begins a ZWJ or some unrelated character without
///     reading the second and third bytes.
///  3. Variation selectors (U+FE0F = 0xEF 0xB8 0x8F) add further multi-byte
///     sequences that interleave with the emoji and Japanese text.
///  4. The input mixes several dense leading-byte ranges (0xE2 for ZWJ,
///     0xE3 for hiragana/katakana, 0xE4+ for CJK, 0xEF for variation
///     selectors), so the wildcard's self-loop must track many active
///     multi-byte paths simultaneously.
fn bench_shellstyle_zwj_emoji(c: &mut Criterion) {
    // ZWJ emoji sequences — each is a single glyph but many bytes.
    let zwj_emoji = [
        "👨\u{200D}👩\u{200D}👧\u{200D}👦", // family
        "👩\u{200D}🚀",                     // woman astronaut
        "🏳\u{FE0F}\u{200D}🌈",              // rainbow flag
        "👨\u{200D}💻",                     // man technologist
        "🧑\u{200D}🎤",                     // singer
        "👩\u{200D}🔬",                     // woman scientist
        "🐻\u{200D}❄\u{FE0F}",              // polar bear
        "👁\u{FE0F}\u{200D}🗨\u{FE0F}",       // eye in speech bubble
    ];

    // Japanese text using leading UTF-8 bytes near the ZWJ range:
    // hiragana/katakana (0xE3), CJK (0xE4-0xE9). Combined with ZWJ (0xE2)
    // and variation selectors (0xEF), the wildcard's self-loop must handle
    // dense multi-byte traffic across several leading byte ranges.
    let japanese_filler = [
        "東京都渋谷区",
        "新宿駅前通り",
        "こんにちは",
        "カタカナテスト",
        "令和七年",
        "人工知能研究所",
        "品川駅南口",
        "秋葉原電気街",
    ];

    // Patterns use ZWJ emoji as anchors with wildcards between them. The "*"
    // must handle both Japanese multi-byte text and ZWJ byte sequences,
    // forcing the NFA to branch heavily on shared leading bytes.
    for pattern_count in [4usize, 8, 16, 32, 64] {
        let mut q = Quamina::new();
        let mut rng = StdRng::seed_from_u64(2025);

        let mut pattern_emojis: Vec<(&str, &str)> = Vec::with_capacity(pattern_count);
        for i in 0..pattern_count {
            let e1 = zwj_emoji[rng.random_range(0..zwj_emoji.len())];
            let e2 = zwj_emoji[rng.random_range(0..zwj_emoji.len())];
            pattern_emojis.push((e1, e2));
            add_shellstyle(&mut q, format!("p{i}"), &format!("*{e1}*{e2}*"));
        }

        // Events: Japanese filler interspersed with ZWJ emoji anchors. Each
        // event uses an emoji pair drawn from the pattern set so it's
        // guaranteed to match at least one pattern — the benchmark measures
        // NFA traversal cost, not match selectivity. The NFA still sees a
        // dense stream of 0xE2, 0xE3, 0xE4, 0xEF bytes and must disambiguate
        // at every step.
        let events: Vec<Vec<u8>> = (0..POOL_SIZE)
            .map(|_| {
                let (e1, e2) = pattern_emojis[rng.random_range(0..pattern_emojis.len())];
                let mut buf = String::new();
                buf.push_str(japanese_filler[rng.random_range(0..japanese_filler.len())]);
                buf.push_str(e1);
                buf.push_str(japanese_filler[rng.random_range(0..japanese_filler.len())]);
                buf.push_str(e2);
                buf.push_str(japanese_filler[rng.random_range(0..japanese_filler.len())]);
                make_event(&buf)
            })
            .collect();

        run_match_loop(
            c,
            &format!("shellstyle_zwj_emoji/patterns={pattern_count}"),
            &q,
            &events,
        );
    }
}

criterion_group!(
    regex_nfa_dfa,
    bench_shellstyle_simple_wildcard,
    bench_shellstyle_narrow_input,
    bench_shellstyle_wide_patterns_scaling,
    bench_shellstyle_simple_wildcard_scaling,
    bench_shellstyle_zwj_emoji,
);
criterion_main!(regex_nfa_dfa);
