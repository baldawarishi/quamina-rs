//! Long single-chain patterns must survive being built and frozen on a small
//! stack. A `prefix` pattern allocates one state per byte, and the arena walks
//! that build, fold and clone that chain visit it one state at a time, so their
//! depth tracks the pattern length rather than anything the memory budget can
//! see.
//!
//! Overflowing a stack aborts the process instead of raising a catchable panic,
//! which would take the whole test binary down with it. The reproducers
//! therefore run in a child process — the parent re-executes this same test
//! binary with `GUARD` set and asserts only on the child's exit status.

use std::process::Command;

use quamina::Quamina;

/// Set in the child process to select the reproducer instead of the spawner.
const GUARD: &str = "QUAMINA_DEEP_CHAIN_CHILD";

/// One arena state per byte, so this is also the chain depth.
const CHAIN_LEN: usize = 5_000;

/// The walks that lay a chain down held out further than the one that cloned
/// it, so their cases need a longer chain before the stack runs out.
const LONG_CHAIN_LEN: usize = 20_000;

/// Tokio's default worker stack, i.e. the smallest stack a caller is likely to
/// reach this code on.
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

            // Every pattern type that compiles to a chain reaches a different
            // walk over it: the exact-match and case-folded builders lay the
            // chain down, and an alternation folds two of them together.
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

/// A second pattern on a field the matcher already holds a chain for merges the
/// two chains state by state, which is a walk of its own and deeper than either
/// chain builder — the merge descends through both automata at once.
#[test]
#[cfg_attr(miri, ignore = "spawns a child process")]
fn merging_two_deep_chains_survives_small_stack() {
    if in_child() {
        on_small_stack(|| {
            // Both patterns of each pair accept the value, so the merged
            // automaton has to keep hold of both chains rather than one.
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

/// True when this process is the spawned child that runs the reproducer.
fn in_child() -> bool {
    std::env::var_os(GUARD).is_some()
}

/// Re-execute this test binary for `test_name` alone, with the guard set, and
/// require the child to exit cleanly.
fn respawn(test_name: &str) {
    let exe = std::env::current_exe().expect("path to this test binary");
    let status = Command::new(exe)
        .args([test_name, "--exact", "--nocapture"])
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
