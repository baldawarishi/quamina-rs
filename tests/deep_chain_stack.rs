//! Deep patterns must build and match on a small stack. A `prefix` pattern
//! allocates one arena state per byte, so every walk over that chain is as deep
//! as the pattern is long. Nested pattern JSON and nested regexp groups are
//! walked once per level, so they are checked far deeper than a recursive walk
//! would survive.
//!
//! A stack overflow aborts the process rather than raising a catchable panic,
//! and would take the test binary down with it. Each reproducer therefore runs
//! in a child process: the parent re-executes this test binary with `GUARD` set
//! and asserts on the child's exit status alone.

use std::process::Command;

use quamina::json::transform_lookaround_pattern;
use quamina::regexp::{expand_word_boundaries, parse_regexp, regexp_has_plus_star};
use quamina::{PatternLimits, Quamina, QuaminaBuilder, QuaminaError};

/// Set in the child process to select the reproducer instead of the spawner.
const GUARD: &str = "QUAMINA_DEEP_CHAIN_CHILD";

/// One arena state per byte, so this is also the chain depth.
const CHAIN_LEN: usize = 5_000;

/// The chain builders hold out further than the clone walk, so their cases need
/// a longer chain before the stack runs out.
const LONG_CHAIN_LEN: usize = 20_000;

/// Tokio's default worker stack, the smallest stack a caller is likely to reach
/// this code on.
const STACK_BYTES: usize = 2 * 1024 * 1024;

#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
fn deep_prefix_chain_survives_small_stack() {
    if in_child() {
        on_small_stack(|| {
            let value = "x".repeat(CHAIN_LEN);
            let mut q = Quamina::new();
            q.add_pattern("chain", &format!(r#"{{"f": [{{"prefix": "{value}"}}]}}"#))
                .expect("a chain this long is well inside the arena byte budget");

            let event = format!(r#"{{"f": "{value}tail"}}"#);
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matching against the frozen chain");
            assert_eq!(matches, vec!["chain"]);

            // Each pattern type reaches a different walk over the chain. The
            // exact-match and case-folded builders lay one down; an alternation
            // folds two of them together.
            let long = "x".repeat(LONG_CHAIN_LEN);
            match_deep_pattern(&format!(r#"{{"f": ["{long}"]}}"#), &long);
            match_deep_pattern(
                &format!(r#"{{"f": [{{"equals-ignore-case": "{long}"}}]}}"#),
                &long,
            );
            match_deep_pattern(
                &format!(r#"{{"f": [{{"regexp": "{long}|{long}z"}}]}}"#),
                &long,
            );
        });
        return;
    }

    respawn("deep_prefix_chain_survives_small_stack");
}

/// A second pattern on a field that already holds a chain merges the two state
/// by state. That walk descends through both automata at once, so it goes
/// deeper than either chain builder.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
fn merging_two_deep_chains_survives_small_stack() {
    if in_child() {
        on_small_stack(|| {
            // Both patterns of each pair accept the value, so the merged
            // automaton must keep both chains rather than one.
            let value = "x".repeat(CHAIN_LEN);
            match_two_deep_patterns(
                &format!(r#"{{"f": [{{"prefix": "{value}"}}]}}"#),
                &format!(r#"{{"f": [{{"prefix": "{value}y"}}]}}"#),
                &format!("{value}ytail"),
                &["first", "second"],
            );
            match_two_deep_patterns(
                &format!(r#"{{"f": [{{"shellstyle": "{value}*"}}]}}"#),
                &format!(r#"{{"f": [{{"shellstyle": "{value}*z"}}]}}"#),
                &format!("{value}mz"),
                &["first", "second"],
            );
        });
        return;
    }

    respawn("merging_two_deep_chains_survives_small_stack");
}

/// A lone exact-match pattern is held aside as a singleton instead of built
/// into the automaton. The next pattern on that field folds the two together
/// through a different merge from the one above: it builds a fresh automaton
/// out of both sides rather than appending to the live one.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
fn merging_a_deep_chain_into_a_singleton_survives_small_stack() {
    if in_child() {
        on_small_stack(|| {
            let value = "x".repeat(CHAIN_LEN);
            let mut q = Quamina::new();
            q.add_pattern("singleton", r#"{"f": ["abc"]}"#)
                .expect("one exact match is held as a singleton");
            q.add_pattern("chain", &format!(r#"{{"f": [{{"prefix": "{value}"}}]}}"#))
                .expect("folding a chain into the singleton stays inside the byte budget");

            let event = format!(r#"{{"f": "{value}tail"}}"#);
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matching against the folded automaton");
            assert_eq!(matches, vec!["chain"]);

            let matches = q
                .matches_for_event(br#"{"f": "abc"}"#)
                .expect("matching against the folded automaton");
            assert_eq!(matches, vec!["singleton"], "the singleton must survive");
        });
        return;
    }

    respawn("merging_a_deep_chain_into_a_singleton_survives_small_stack");
}

/// `anything-but` compiles its excluded values into a trie one level per byte,
/// so a single long excluded value is as deep as the value is long.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
fn deep_anything_but_survives_small_stack() {
    if in_child() {
        on_small_stack(|| {
            let excluded = "x".repeat(CHAIN_LEN);
            let mut q = Quamina::new();
            q.add_pattern(
                "chain",
                &format!(r#"{{"f": [{{"anything-but": ["{excluded}"]}}]}}"#),
            )
            .expect("a trie this deep is well inside the arena byte budget");

            let event = format!(r#"{{"f": "{excluded}"}}"#);
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matching against the frozen trie");
            assert!(matches.is_empty(), "the excluded value must not match");

            let other = format!(r#"{{"f": "{}"}}"#, "y".repeat(CHAIN_LEN));
            let matches = q
                .matches_for_event(other.as_bytes())
                .expect("matching against the frozen trie");
            assert_eq!(matches, vec!["chain"]);
        });
        return;
    }

    respawn("deep_anything_but_survives_small_stack");
}

/// Pattern JSON nested to the default `max_pattern_depth`, with the operator
/// syntax a leaf can add below that, and far deeper once the limit is raised.
/// Parsing, adding, cloning and dropping each visit every level.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
fn deepest_pattern_json_survives_small_stack() {
    if in_child() {
        on_small_stack(|| {
            let depth = PatternLimits::default().max_pattern_depth;
            let deeper = 200_000;

            let mut q = Quamina::new();
            q.add_pattern("deep", &nested_numeric_pattern(depth))
                .expect("nesting at max_pattern_depth is accepted");
            let err = q
                .add_pattern("deeper", &nested_numeric_pattern(deeper))
                .expect_err("nesting past max_pattern_depth is rejected");
            assert!(matches!(err, QuaminaError::PatternTooComplex(_)), "{err}");

            let mut q = QuaminaBuilder::new()
                .with_max_pattern_depth(deeper)
                .build()
                .expect("a raised depth limit is valid");
            q.add_pattern("deeper", &nested_numeric_pattern(deeper))
                .expect("nesting at a raised max_pattern_depth is accepted");
            drop(q.clone());
            drop(q.segments_tree().clone());

            // Arrays nested inside an operator are parsed before the operator
            // rejects them.
            let pattern = format!(
                r#"{{"f": [{{"numeric": {}{}}}]}}"#,
                "[".repeat(deeper),
                "]".repeat(deeper)
            );
            let err = q
                .add_pattern("arrays", &pattern)
                .expect_err("nested arrays are not a numeric comparison");
            assert!(matches!(err, QuaminaError::InvalidPattern(_)), "{err}");
        });
        return;
    }

    respawn("deepest_pattern_json_survives_small_stack");
}

/// An event nested as deep as the deepest pattern the default limit accepts.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
#[ignore = "the flattener's read_object still recurses once per nested object"]
fn deepest_pattern_json_matches_on_small_stack() {
    if in_child() {
        on_small_stack(|| {
            let depth = PatternLimits::default().max_pattern_depth;
            let mut q = Quamina::new();
            q.add_pattern("deep", &nested_numeric_pattern(depth))
                .expect("nesting at max_pattern_depth is accepted");

            let event = format!("{}2{}", r#"{"f": "#.repeat(depth), "}".repeat(depth));
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matching the deeply nested event");
            assert_eq!(matches, vec!["deep"]);
        });
        return;
    }

    respawn("deepest_pattern_json_matches_on_small_stack");
}

/// Quantified regexp groups nested past where a recursive NFA builder runs
/// out of stack. Every level's loop or skip reaches every level inside it, so
/// the automaton grows with the square of the depth and needs a larger arena
/// budget than the default.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
fn deep_quantified_regexp_groups_survive_small_stack() {
    if in_child() {
        on_small_stack(|| {
            let depth = 3_000;
            let mut q = QuaminaBuilder::new()
                .with_arena_byte_budget(256 * 1024 * 1024)
                .build()
                .expect("a larger arena budget is valid");
            for (id, close) in [("plus", ")+"), ("optional", ")?")] {
                let pattern = format!(
                    r#"{{"f": [{{"regexp": "{}a{}"}}]}}"#,
                    "(".repeat(depth),
                    close.repeat(depth)
                );
                q.add_pattern(id, &pattern)
                    .expect("quantified groups nest without a depth limit");
            }

            let mut matches = q
                .matches_for_event(br#"{"f": "a"}"#)
                .expect("matching against the nested regexps");
            matches.sort_unstable();
            assert_eq!(matches, vec!["optional", "plus"]);
            let matches = q
                .matches_for_event(br#"{"f": "aaa"}"#)
                .expect("matching against the nested regexps");
            assert_eq!(matches, vec!["plus"]);
        });
        return;
    }

    respawn("deep_quantified_regexp_groups_survive_small_stack");
}

/// Regexp groups nested deeper than Go Quamina reaches on its 1 GB maximum
/// goroutine stack (about 196,000 levels).
const REGEXP_GROUP_DEPTH: usize = 200_000;

/// Parsing, validating, rewriting, copying, dropping and building an NFA for a
/// regexp all walk its groups level by level, so each is checked here at a
/// depth far past where a recursive walk runs out of stack.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
fn deeply_nested_regexp_groups_survive_small_stack() {
    if in_child() {
        on_small_stack(|| {
            let nested = |open: &str, leaf: &str, close: &str| {
                format!(
                    "{}{leaf}{}",
                    open.repeat(REGEXP_GROUP_DEPTH),
                    close.repeat(REGEXP_GROUP_DEPTH)
                )
            };

            let tree = parse_regexp(&nested("(", "a", ")+")).expect("nesting depth is unlimited");
            assert!(regexp_has_plus_star(&tree));
            let copy = tree.clone();
            drop(tree);
            drop(copy);

            // Each failure drops the half-built tree it was holding.
            assert!(
                parse_regexp(&nested("(", "a", "")).is_err(),
                "unclosed groups"
            );
            assert!(
                parse_regexp(&format!("(?={})", nested("(", "(?=a)", ")"))).is_err(),
                "a lookahead nested in a lookahead"
            );
            assert!(
                parse_regexp(&format!("(?<={})", nested("(", "a+", ")"))).is_err(),
                "a variable-length lookbehind"
            );

            // A lookbehind is measured and copied out into a condition, and a
            // word boundary copies the atoms around it into new branches.
            let lookbehind = parse_regexp(&format!("(?<={})b", nested("(", "a", ")")))
                .expect("a fixed-length lookbehind");
            transform_lookaround_pattern(&lookbehind).expect("one lookbehind and a primary");
            let boundary = parse_regexp(&format!("a~b-{}", nested("(", "a", ")")))
                .expect("a word boundary between a word and a non-word char");
            expand_word_boundaries(&boundary).expect("the boundary resolves");

            // Groups and alternations of any depth build and match.
            let mut q = Quamina::new();
            for (id, open) in [("groups", "("), ("alternation", "(x|")] {
                let pattern = format!(r#"{{"f": [{{"regexp": "{}"}}]}}"#, nested(open, "a", ")"));
                q.add_pattern(id, &pattern)
                    .expect("groups nest without a depth limit");
            }
            let mut matches = q
                .matches_for_event(br#"{"f": "a"}"#)
                .expect("matching against the nested regexps");
            matches.sort_unstable();
            assert_eq!(matches, vec!["alternation", "groups"]);
            let matches = q
                .matches_for_event(br#"{"f": "x"}"#)
                .expect("matching against the nested regexps");
            assert_eq!(matches, vec!["alternation"]);
        });
        return;
    }

    respawn("deeply_nested_regexp_groups_survive_small_stack");
}

/// Arrays nested in an event field a pattern uses are flattened however
/// shallow the pattern is, and Go Quamina has no depth limit for them.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
#[ignore = "the flattener's read_array still recurses once per nested array"]
fn deeply_nested_event_arrays_survive_small_stack() {
    if in_child() {
        on_small_stack(|| {
            let depth = 100_000;
            let mut q = Quamina::new();
            q.add_pattern("deep", r#"{"a": ["x"]}"#)
                .expect("a flat pattern");

            let event = format!(r#"{{"a": {}"x"{}}}"#, "[".repeat(depth), "]".repeat(depth));
            let matches = q
                .matches_for_event(event.as_bytes())
                .expect("matching the deeply nested event");
            assert_eq!(matches, vec!["deep"]);
        });
        return;
    }

    respawn("deeply_nested_event_arrays_survive_small_stack");
}

/// A pattern with `depth` nested field objects and a numeric operator below
/// the last.
fn nested_numeric_pattern(depth: usize) -> String {
    format!(
        r#"{}[{{"numeric": [">", 1]}}]{}"#,
        r#"{"f": "#.repeat(depth),
        "}".repeat(depth)
    )
}

/// True when this process is the spawned child that runs the reproducer.
fn in_child() -> bool {
    std::env::var_os(GUARD).is_some()
}

/// Re-execute this test binary for `test_name` alone, with the guard set, and
/// require the child to exit cleanly.
fn respawn(test_name: &str) {
    let exe = std::env::current_exe().expect("path to this test binary");
    let status = Command::new(exe)
        .args([test_name, "--exact", "--include-ignored", "--nocapture"])
        .env(GUARD, "1")
        .status()
        .expect("spawn the reproducer in a child process");

    assert!(
        status.success(),
        "child process running {test_name} on a {STACK_BYTES}-byte stack exited with {status}"
    );
}

/// Run `body` on a thread with a small fixed stack.
fn on_small_stack(body: impl FnOnce() + Send + 'static) {
    std::thread::Builder::new()
        .stack_size(STACK_BYTES)
        .spawn(body)
        .expect("spawn the fixed-stack worker thread")
        .join()
        .expect("worker thread panicked");
}

/// Add one deep pattern and match the value it was built from.
fn match_deep_pattern(pattern: &str, value: &str) {
    let mut q = Quamina::new();
    q.add_pattern("chain", pattern)
        .expect("a chain this long is well inside the arena byte budget");

    let event = format!(r#"{{"f": "{value}"}}"#);
    let matches = q
        .matches_for_event(event.as_bytes())
        .expect("matching against the frozen chain");
    assert_eq!(matches, vec!["chain"], "pattern: {pattern:.40}…");
}

/// Add two deep patterns on the same field, so the second merges into the
/// automaton the first built, and match a value against the merged result.
fn match_two_deep_patterns(first: &str, second: &str, value: &str, expected: &[&str]) {
    let mut q = Quamina::new();
    q.add_pattern("first", first)
        .expect("a chain this long is well inside the arena byte budget");
    q.add_pattern("second", second)
        .expect("merging a second chain stays inside the arena byte budget");

    let event = format!(r#"{{"f": "{value}"}}"#);
    let mut matches = q
        .matches_for_event(event.as_bytes())
        .expect("matching against the frozen merge");
    matches.sort_unstable();
    assert_eq!(matches, expected, "patterns: {first:.40}…");
}
