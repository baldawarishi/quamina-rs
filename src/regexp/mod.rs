//! Regexp parsing and NFA building for I-Regexp (RFC 9485 subset).
//!
//! This module implements a custom regexp engine that integrates with quamina's automaton.
//! It supports a subset of I-Regexp:
//! - `.` matches any character
//! - `[...]` character classes with ranges
//! - `[^...]` negated character classes
//! - `|` alternation
//! - `(...)` grouping
//! - `?` optional quantifier
//! - `+` one-or-more quantifier
//! - `*` zero-or-more quantifier
//!
//! The escape character is `~` (not `\`) to avoid JSON escaping issues.

mod nfa;
mod parser;

// Re-export public API
pub use nfa::{make_dot_fa, make_regexp_nfa, make_regexp_nfa_arena, regexp_has_plus_star};
pub use parser::{
    invert_rune_range, parse_regexp, simplify_rune_range, QuantifiedAtom, RegexpBranch,
    RegexpError, RegexpRoot, RunePair, RuneRange, REGEXP_QUANTIFIER_MAX, RUNE_MAX,
};

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;

    #[test]
    fn test_parse_simple() {
        let root = parse_regexp("abc").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 3);
    }

    #[test]
    fn test_parse_alternation() {
        let root = parse_regexp("a|b").unwrap();
        assert_eq!(root.len(), 2);
    }

    #[test]
    fn test_parse_char_class() {
        let root = parse_regexp("[abc]").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        // a, b, c are consecutive so they get merged into one range [a-c]
        assert_eq!(root[0][0].runes.len(), 1);
        assert_eq!(root[0][0].runes[0].lo, 'a');
        assert_eq!(root[0][0].runes[0].hi, 'c');
    }

    #[test]
    fn test_parse_char_range() {
        let root = parse_regexp("[a-z]").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0][0].runes.len(), 1);
        assert_eq!(root[0][0].runes[0].lo, 'a');
        assert_eq!(root[0][0].runes[0].hi, 'z');
    }

    #[test]
    fn test_parse_dot() {
        let root = parse_regexp("a.b").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 3);
        assert!(root[0][1].is_dot);
    }

    #[test]
    fn test_parse_optional() {
        let root = parse_regexp("ab?c").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 3);
        assert_eq!(root[0][1].quant_min, 0);
        assert_eq!(root[0][1].quant_max, 1);
    }

    #[test]
    fn test_parse_group() {
        let root = parse_regexp("(a|b)c").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 2);
        assert!(root[0][0].subtree.is_some());
        let subtree = root[0][0].subtree.as_ref().unwrap();
        assert_eq!(subtree.len(), 2); // a | b
    }

    #[test]
    fn test_simplify_rune_range() {
        let rr = vec![RunePair { lo: 'a', hi: 'c' }, RunePair { lo: 'b', hi: 'd' }];
        let simplified = simplify_rune_range(rr);
        assert_eq!(simplified.len(), 1);
        assert_eq!(simplified[0].lo, 'a');
        assert_eq!(simplified[0].hi, 'd');
    }

    #[test]
    fn test_parse_invalid_unclosed_bracket() {
        // Invalid pattern with unclosed bracket should fail
        let result = parse_regexp("[invalid");
        assert!(result.is_err(), "Unclosed bracket should fail parsing");
    }

    #[test]
    fn test_parse_plus() {
        let root = parse_regexp("[a-z]+").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert!(
            root[0][0].is_plus(),
            "Should be recognized as plus quantifier"
        );
    }

    #[test]
    fn test_parse_star() {
        let root = parse_regexp("[a-z]*").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert!(
            root[0][0].is_star(),
            "Should be recognized as star quantifier"
        );
    }

    #[test]
    fn test_parse_negated_class() {
        let root = parse_regexp("[^abc]").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        // The range should be inverted (everything except a, b, c)
        // a=97, b=98, c=99 -> inverted should start at 0 and have gaps
        assert!(
            root[0][0].runes.len() > 1,
            "Negated class should produce multiple ranges"
        );
    }

    #[test]
    fn test_parse_empty() {
        // Empty pattern should succeed
        let result = parse_regexp("");
        assert!(result.is_ok(), "Empty pattern should parse successfully");
        let root = result.unwrap();
        // Empty pattern has zero branches
        assert_eq!(root.len(), 0);
    }

    #[test]
    fn test_nfa_empty_pattern() {
        use crate::automaton::{traverse_dfa, VALUE_TERMINATOR};

        // Test that empty regexp NFA matches ONLY empty string
        let root = parse_regexp("").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);

        // Test with empty value (just VALUE_TERMINATOR)
        let empty_value = vec![VALUE_TERMINATOR];
        let mut matches = Vec::new();
        traverse_dfa(&table, &empty_value, &mut matches);
        assert!(
            !matches.is_empty(),
            "Empty regexp should match empty string"
        );
        assert!(
            std::sync::Arc::ptr_eq(&matches[0], &field_matcher),
            "Should transition to field_matcher"
        );

        // Test with non-empty value - should NOT match
        let non_empty_value = vec![b'h', b'i', VALUE_TERMINATOR];
        let mut matches2 = Vec::new();
        traverse_dfa(&table, &non_empty_value, &mut matches2);
        assert!(
            matches2.is_empty(),
            "Empty regexp should NOT match non-empty string"
        );
    }

    #[test]
    fn test_nfa_simple_singleton() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // First verify basic non-quantified matching works
        let root = parse_regexp("[abc]").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        let value_a = vec![b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_a, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc] should match 'a'"
        );
    }

    #[test]
    fn test_nfa_plus_quantifier() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test that [abc]+ matches one or more of a, b, c
        let root = parse_regexp("[abc]+").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should match "a"
        let value_a = vec![b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_a, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]+ should match 'a'"
        );

        // Should match "abc"
        let value_abc = vec![b'a', b'b', b'c', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_abc, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]+ should match 'abc'"
        );

        // Should NOT match empty string
        let empty = vec![VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &empty, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]+ should NOT match empty string"
        );

        // Should NOT match "x"
        let value_x = vec![b'x', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_x, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]+ should NOT match 'x'"
        );
    }

    #[test]
    fn test_nfa_star_quantifier() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test that [abc]* matches zero or more of a, b, c
        let root = parse_regexp("[abc]*").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should match empty string (zero times)
        let empty = vec![VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &empty, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]* should match empty string"
        );

        // Should match "a"
        let value_a = vec![b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_a, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]* should match 'a'"
        );

        // Should match "abc"
        let value_abc = vec![b'a', b'b', b'c', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_abc, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]* should match 'abc'"
        );
    }

    #[test]
    fn test_parse_range_quantifier() {
        // Test {n} - exactly n times
        let root = parse_regexp("a{3}").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert_eq!(root[0][0].quant_min, 3);
        assert_eq!(root[0][0].quant_max, 3); // {n} means exactly n times

        // Test {n,m} - between n and m times
        let root = parse_regexp("a{2,5}").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert_eq!(root[0][0].quant_min, 2);
        assert_eq!(root[0][0].quant_max, 5);

        // Test {n,} - at least n times
        let root = parse_regexp("a{2,}").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert_eq!(root[0][0].quant_min, 2);
        assert_eq!(root[0][0].quant_max, REGEXP_QUANTIFIER_MAX);
    }

    #[test]
    fn test_nfa_range_exact() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test a{3} - exactly 3 'a's (I-Regexp semantics: {n} means exactly n)
        let root = parse_regexp("a{3}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should NOT match "aa"
        let value_aa = vec![b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_aa, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{3}} should NOT match 'aa'"
        );

        // Should match "aaa"
        let value_aaa = vec![b'a', b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_aaa, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{3}} should match 'aaa'"
        );

        // Should NOT match "aaaa" ({n} means exactly n)
        let value_aaaa = vec![b'a', b'a', b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_aaaa, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{3}} should NOT match 'aaaa'"
        );
    }

    #[test]
    fn test_nfa_range_bounded() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test a{2,4} - between 2 and 4 'a's
        let root = parse_regexp("a{2,4}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should NOT match "a"
        let value_a = vec![b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_a, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{2,4}} should NOT match 'a'"
        );

        // Should match "aa"
        let value_aa = vec![b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_aa, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{2,4}} should match 'aa'"
        );

        // Should match "aaa"
        let value_aaa = vec![b'a', b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_aaa, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{2,4}} should match 'aaa'"
        );

        // Should match "aaaa"
        let value_aaaa = vec![b'a', b'a', b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_aaaa, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{2,4}} should match 'aaaa'"
        );

        // Should NOT match "aaaaa"
        let value_5a = vec![b'a', b'a', b'a', b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_5a, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{2,4}} should NOT match 'aaaaa'"
        );
    }

    #[test]
    fn test_nfa_range_with_class() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test [abc]{2,3}
        let root = parse_regexp("[abc]{2,3}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should NOT match "a"
        let value_a = vec![b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_a, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]{{2,3}} should NOT match 'a'"
        );

        // Should match "ab"
        let value_ab = vec![b'a', b'b', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_ab, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]{{2,3}} should match 'ab'"
        );

        // Should match "abc"
        let value_abc = vec![b'a', b'b', b'c', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_abc, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]{{2,3}} should match 'abc'"
        );

        // Should NOT match "abcd" (4 chars)
        let value_abcd = vec![b'a', b'b', b'c', b'd', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_abcd, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern [abc]{{2,3}} should NOT match 'abcd'"
        );
    }

    #[test]
    fn test_nfa_range_zero_min() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test a{0,2} - between 0 and 2 'a's
        let root = parse_regexp("a{0,2}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should match empty string
        let empty = vec![VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &empty, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{0,2}} should match empty string"
        );

        // Should match "a"
        let value_a = vec![b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_a, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{0,2}} should match 'a'"
        );

        // Should match "aa"
        let value_aa = vec![b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_aa, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{0,2}} should match 'aa'"
        );

        // Should NOT match "aaa"
        let value_aaa = vec![b'a', b'a', b'a', VALUE_TERMINATOR];
        bufs.clear();
        traverse_nfa(&table, &value_aaa, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern a{{0,2}} should NOT match 'aaa'"
        );
    }

    #[test]
    fn test_invert_rune_range() {
        // Port of Go's TestInvertRuneRange
        // Note: Ranges spanning surrogates (U+D800-U+DFFF) are split into
        // pre-surrogate and post-surrogate parts since Rust chars can't be surrogates.
        let test_cases = vec![
            // {input, expected}
            (
                vec![RunePair { lo: 'b', hi: 'b' }],
                vec![
                    RunePair { lo: '\0', hi: 'a' },
                    RunePair {
                        lo: 'c',
                        hi: '\u{D7FF}',
                    },
                    RunePair {
                        lo: '\u{E000}',
                        hi: RUNE_MAX,
                    },
                ],
            ),
            (
                vec![RunePair { lo: 'l', hi: 'n' }],
                vec![
                    RunePair { lo: '\0', hi: 'k' },
                    RunePair {
                        lo: 'o',
                        hi: '\u{D7FF}',
                    },
                    RunePair {
                        lo: '\u{E000}',
                        hi: RUNE_MAX,
                    },
                ],
            ),
            (
                vec![RunePair { lo: 'b', hi: 'n' }, RunePair { lo: 'p', hi: 'q' }],
                vec![
                    RunePair { lo: '\0', hi: 'a' },
                    RunePair { lo: 'o', hi: 'o' },
                    RunePair {
                        lo: 'r',
                        hi: '\u{D7FF}',
                    },
                    RunePair {
                        lo: '\u{E000}',
                        hi: RUNE_MAX,
                    },
                ],
            ),
            (
                vec![
                    RunePair { lo: '\0', hi: 'x' },
                    RunePair {
                        lo: 'z',
                        hi: RUNE_MAX,
                    },
                ],
                vec![RunePair { lo: 'y', hi: 'y' }],
            ),
            (
                vec![
                    RunePair { lo: 'd', hi: 'd' },
                    RunePair { lo: 'b', hi: 'b' },
                    RunePair { lo: 'c', hi: 'c' },
                ],
                vec![
                    RunePair { lo: '\0', hi: 'a' },
                    RunePair {
                        lo: 'e',
                        hi: '\u{D7FF}',
                    },
                    RunePair {
                        lo: '\u{E000}',
                        hi: RUNE_MAX,
                    },
                ],
            ),
        ];

        for (i, (input, expected)) in test_cases.into_iter().enumerate() {
            let result = invert_rune_range(input);
            assert_eq!(
                result.len(),
                expected.len(),
                "Test case {}: wrong number of ranges. Got {:?}, expected {:?}",
                i,
                result,
                expected
            );
            for (j, (got, want)) in result.iter().zip(expected.iter()).enumerate() {
                assert_eq!(
                    got.lo, want.lo,
                    "Test case {} range {}: wrong lo. Got {:?}, expected {:?}",
                    i, j, got.lo, want.lo
                );
                assert_eq!(
                    got.hi, want.hi,
                    "Test case {} range {}: wrong hi. Got {:?}, expected {:?}",
                    i, j, got.hi, want.hi
                );
            }
        }
    }

    #[test]
    fn test_toxic_stack_arena() {
        use crate::automaton::arena::{
            traverse_arena_nfa, ArenaNfaBuffers, ARENA_VALUE_TERMINATOR,
        };

        // Port of Go's TestToxicStack using arena-based NFA
        // Pattern: (([~.~~~?~*~+~{~}~[~]~(~)~|]?)*)+"
        // This tests that quantified groups work correctly with cyclic arena NFA
        let re = "(([~.~~~?~*~+~{~}~[~]~(~)~|]?)*)+";
        let root = parse_regexp(re).expect("Should parse toxic stack pattern");

        // Verify pattern has + or * (should use arena)
        assert!(regexp_has_plus_star(&root), "Toxic pattern should have +/*");

        let (arena, start, field_matcher) = make_regexp_nfa_arena(root, true);

        // Test string: ".~?*+{}[]()|.~?*+{}[]()|.~?*+{}[]()|"
        let test_str = ".~?*+{}[]()|.~?*+{}[]()|.~?*+{}[]()|";
        let mut value: Vec<u8> = Vec::new();
        value.push(b'"');
        value.extend_from_slice(test_str.as_bytes());
        value.push(b'"');
        value.push(ARENA_VALUE_TERMINATOR);

        let mut bufs = ArenaNfaBuffers::new();
        traverse_arena_nfa(&arena, start, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Toxic stack pattern should match test string via arena NFA"
        );
    }

    #[test]
    fn test_negated_class_nfa() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test [^abc] - matches any character except a, b, c
        // This test uses the optimized range-based NFA construction that builds
        // SmallTables directly from UTF-8 byte ranges without per-character enumeration.
        let root = parse_regexp("[^abc]").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should NOT match "a", "b", "c"
        for ch in b"abc" {
            let value = vec![*ch, VALUE_TERMINATOR];
            bufs.clear();
            traverse_nfa(&table, &value, &mut bufs);
            assert!(
                !bufs
                    .transitions
                    .iter()
                    .any(|m| Arc::ptr_eq(m, &field_matcher)),
                "Pattern [^abc] should NOT match '{}'",
                *ch as char
            );
        }

        // Should match "x", "y", "z"
        for ch in b"xyz" {
            let value = vec![*ch, VALUE_TERMINATOR];
            bufs.clear();
            traverse_nfa(&table, &value, &mut bufs);
            assert!(
                bufs.transitions
                    .iter()
                    .any(|m| Arc::ptr_eq(m, &field_matcher)),
                "Pattern [^abc] should match '{}'",
                *ch as char
            );
        }
    }

    #[test]
    fn test_star_matches_empty() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Patterns with * should match empty string
        // Note: Excluding patterns with negated classes (e.g., [^?]) due to slow
        // O(unicode_range) NFA construction
        let star_patterns = vec!["[a-z]*", "[0-9]*", ".*", "([abc]*)"];

        for pattern in star_patterns {
            let root = parse_regexp(pattern).unwrap();
            let (table, field_matcher) = make_regexp_nfa(root, false);
            let mut bufs = NfaBuffers::new();

            let empty = vec![VALUE_TERMINATOR];
            bufs.clear();
            traverse_nfa(&table, &empty, &mut bufs);
            assert!(
                bufs.transitions
                    .iter()
                    .any(|m| Arc::ptr_eq(m, &field_matcher)),
                "Pattern {} should match empty string",
                pattern
            );
        }
    }

    #[test]
    fn test_arena_nfa_email_pattern() {
        use crate::automaton::arena::{
            traverse_arena_nfa, ArenaNfaBuffers, ARENA_VALUE_TERMINATOR,
        };

        // Test the pattern from the failing test
        let pattern = "[a-z]+@example~.com";
        let root = parse_regexp(pattern).unwrap();

        // Verify it has plus quantifier
        assert!(
            regexp_has_plus_star(&root),
            "Pattern should be detected as having + quantifier"
        );

        // Build arena NFA
        let (arena, start, field_matcher) = make_regexp_nfa_arena(root, false);

        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

        // Test: "alice@example.com" should match
        let mut value = b"alice@example.com".to_vec();
        value.push(ARENA_VALUE_TERMINATOR);
        traverse_arena_nfa(&arena, start, &value, &mut bufs);

        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern {} should match 'alice@example.com'",
            pattern
        );
    }

    #[test]
    fn test_arena_nfa_plus_simple() {
        use crate::automaton::arena::{
            traverse_arena_nfa, ArenaNfaBuffers, ARENA_VALUE_TERMINATOR,
        };

        // Test simple [a-z]+ pattern with arena
        let pattern = "[a-z]+";
        let root = parse_regexp(pattern).unwrap();
        let (arena, start, field_matcher) = make_regexp_nfa_arena(root, false);

        let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

        // Test: "abc" should match
        let mut value = b"abc".to_vec();
        value.push(ARENA_VALUE_TERMINATOR);
        traverse_arena_nfa(&arena, start, &value, &mut bufs);

        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "Pattern {} should match 'abc'",
            pattern
        );
    }

    #[test]
    fn test_negated_category_star_edge_cases() {
        use crate::automaton::arena::{traverse_arena_nfa, ArenaNfaBuffers};

        // Helper to test if a pattern matches a string
        fn matches(pattern: &str, input: &str) -> bool {
            let root = parse_regexp(pattern).expect(&format!("Failed to parse: {}", pattern));
            let (arena, start, field_matcher) = make_regexp_nfa_arena(root, false);
            let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

            // Note: traverse_arena_nfa auto-appends VALUE_TERMINATOR, so don't add it to input
            traverse_arena_nfa(&arena, start, input.as_bytes(), &mut bufs);

            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher))
        }

        // Sample 211: ~P{C}* should match '₠' (U+20A0, category Sc - not in C)
        // First test simpler cases
        assert!(matches(".*", "a"), ".* should match 'a'");
        assert!(matches(".*", "₠"), ".* should match '₠'");
        assert!(matches(".*", ""), ".* should match empty");

        // Test negated category without star first
        // ~P{C} means NOT in category C (Other)
        // ₠ (U+20A0) is Sc (Currency Symbol), not C, so should match
        assert!(matches("~P{C}", "₠"), "~P{{C}} should match '₠'");

        // Now test with star
        assert!(
            matches("~P{C}*", ""),
            "~P{{C}}* should match empty (zero chars)"
        );
        assert!(
            matches("~P{C}*", "₠"),
            "~P{{C}}* should match '₠' (single non-C char)"
        );

        // Test Sample 147: ~p{Lo}* should match "א𪘀"
        // Both characters are Lo (Other Letter):
        // א (U+05D0) Hebrew Alef, 𪘀 (U+2A600) CJK Extension B
        assert!(matches("~p{Lo}", "א"), "~p{{Lo}} should match Hebrew Alef");
        assert!(
            matches("~p{Lo}", "𪘀"),
            "~p{{Lo}} should match CJK Extension B char"
        );
        assert!(
            matches("~p{Lo}*", "א"),
            "~p{{Lo}}* should match Hebrew Alef"
        );
        assert!(
            matches("~p{Lo}*", "𪘀"),
            "~p{{Lo}}* should match CJK Extension B char"
        );
        assert!(
            matches("~p{Lo}*", "א𪘀"),
            "~p{{Lo}}* should match Hebrew + CJK"
        );

        // Test with VALUE_TERMINATOR appended (matching test_regexp_validity behavior)
        use crate::automaton::arena::ARENA_VALUE_TERMINATOR;
        fn matches_with_vt(pattern: &str, input: &str) -> bool {
            let root = parse_regexp(pattern).expect(&format!("Failed to parse: {}", pattern));
            let (arena, start, field_matcher) = make_regexp_nfa_arena(root, false);
            let mut bufs = ArenaNfaBuffers::with_capacity(arena.len());

            // Add VALUE_TERMINATOR to input (like test_regexp_validity does)
            let mut value: Vec<u8> = input.as_bytes().to_vec();
            value.push(ARENA_VALUE_TERMINATOR);
            traverse_arena_nfa(&arena, start, &value, &mut bufs);

            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher))
        }

        // These should also pass with VALUE_TERMINATOR appended
        assert!(
            matches_with_vt("~p{Lo}*", "א𪘀"),
            "~p{{Lo}}* should match Hebrew + CJK (with VT)"
        );
    }

    // ============= Range Quantifier Edge Case Tests =============

    #[test]
    fn test_range_quantifier_parse_errors() {
        // Error cases from Go's TestRegexpErrors
        let error_cases = vec![
            ("a{9999999999998,9999999999999}", "overflow in lo"),
            ("a{2x-3}", "invalid char after digits"),
            ("a{2,", "incomplete - no closing brace"),
            ("a{2,r}", "invalid char after comma"),
            ("a{2,4x", "invalid after complete range"),
            ("a{2,9999999999999}", "overflow in hi"),
            ("a{5,2}", "min > max"),
            ("a{,3}", "missing lo"),
            ("a{}", "empty braces"),
        ];

        for (pattern, desc) in error_cases {
            let result = parse_regexp(pattern);
            assert!(
                result.is_err(),
                "Pattern '{}' should fail: {}",
                pattern,
                desc
            );
        }
    }

    #[test]
    fn test_range_quantifier_equivalence_question() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // a{0,1} should be equivalent to a?
        let root_range = parse_regexp("a{0,1}").unwrap();
        let root_qm = parse_regexp("a?").unwrap();

        let (table_range, fm_range) = make_regexp_nfa(root_range, false);
        let (table_qm, fm_qm) = make_regexp_nfa(root_qm, false);

        let mut bufs = NfaBuffers::new();
        let test_cases = vec![
            (vec![VALUE_TERMINATOR], true, "empty"),
            (vec![b'a', VALUE_TERMINATOR], true, "a"),
            (vec![b'a', b'a', VALUE_TERMINATOR], false, "aa"),
            (vec![b'b', VALUE_TERMINATOR], false, "b"),
        ];

        for (value, should_match, desc) in test_cases {
            bufs.clear();
            traverse_nfa(&table_range, &value, &mut bufs);
            let range_matched = bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm_range));

            bufs.clear();
            traverse_nfa(&table_qm, &value, &mut bufs);
            let qm_matched = bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm_qm));

            assert_eq!(
                range_matched, qm_matched,
                "a{{0,1}} and a? should agree on '{}': range={}, qm={}",
                desc, range_matched, qm_matched
            );
            assert_eq!(
                range_matched,
                should_match,
                "Pattern should {} match '{}'",
                if should_match { "" } else { "NOT" },
                desc
            );
        }
    }

    #[test]
    fn test_range_quantifier_equivalence_plus() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // a{1,} should be equivalent to a+ (but capped at REGEXP_QUANTIFIER_MAX)
        let root_range = parse_regexp("a{1,}").unwrap();
        let root_plus = parse_regexp("a+").unwrap();

        let (table_range, fm_range) = make_regexp_nfa(root_range, false);
        let (table_plus, fm_plus) = make_regexp_nfa(root_plus, false);

        let mut bufs = NfaBuffers::new();
        let test_cases = vec![
            (vec![VALUE_TERMINATOR], false, "empty"),
            (vec![b'a', VALUE_TERMINATOR], true, "a"),
            (vec![b'a', b'a', VALUE_TERMINATOR], true, "aa"),
            (vec![b'a', b'a', b'a', VALUE_TERMINATOR], true, "aaa"),
            (vec![b'b', VALUE_TERMINATOR], false, "b"),
        ];

        for (value, should_match, desc) in test_cases {
            bufs.clear();
            traverse_nfa(&table_range, &value, &mut bufs);
            let range_matched = bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm_range));

            bufs.clear();
            traverse_nfa(&table_plus, &value, &mut bufs);
            let plus_matched = bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm_plus));

            assert_eq!(
                range_matched, plus_matched,
                "a{{1,}} and a+ should agree on '{}': range={}, plus={}",
                desc, range_matched, plus_matched
            );
            assert_eq!(
                range_matched,
                should_match,
                "Pattern should {} match '{}'",
                if should_match { "" } else { "NOT" },
                desc
            );
        }
    }

    #[test]
    fn test_range_quantifier_equivalence_star() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // a{0,} should be equivalent to a* (but capped at REGEXP_QUANTIFIER_MAX)
        let root_range = parse_regexp("a{0,}").unwrap();
        let root_star = parse_regexp("a*").unwrap();

        let (table_range, fm_range) = make_regexp_nfa(root_range, false);
        let (table_star, fm_star) = make_regexp_nfa(root_star, false);

        let mut bufs = NfaBuffers::new();
        let test_cases = vec![
            (vec![VALUE_TERMINATOR], true, "empty"),
            (vec![b'a', VALUE_TERMINATOR], true, "a"),
            (vec![b'a', b'a', VALUE_TERMINATOR], true, "aa"),
            (vec![b'b', VALUE_TERMINATOR], false, "b"),
        ];

        for (value, should_match, desc) in test_cases {
            bufs.clear();
            traverse_nfa(&table_range, &value, &mut bufs);
            let range_matched = bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm_range));

            bufs.clear();
            traverse_nfa(&table_star, &value, &mut bufs);
            let star_matched = bufs.transitions.iter().any(|m| Arc::ptr_eq(m, &fm_star));

            assert_eq!(
                range_matched, star_matched,
                "a{{0,}} and a* should agree on '{}': range={}, star={}",
                desc, range_matched, star_matched
            );
            assert_eq!(
                range_matched,
                should_match,
                "Pattern should {} match '{}'",
                if should_match { "" } else { "NOT" },
                desc
            );
        }
    }

    #[test]
    fn test_range_quantifier_exact_one() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // a{1} means exactly 1 'a' (I-Regexp semantics: {n} means exactly n)
        let root = parse_regexp("a{1}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        let test_cases = vec![
            (vec![VALUE_TERMINATOR], false, "empty"),
            (vec![b'a', VALUE_TERMINATOR], true, "a"),
            (vec![b'a', b'a', VALUE_TERMINATOR], false, "aa"), // {1} means exactly 1
        ];

        for (value, should_match, desc) in test_cases {
            bufs.clear();
            traverse_nfa(&table, &value, &mut bufs);
            let matched = bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher));
            assert_eq!(
                matched,
                should_match,
                "a{{1}} should {} match '{}'",
                if should_match { "" } else { "NOT" },
                desc
            );
        }
    }

    #[test]
    fn test_range_quantifier_exact_zero() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // a{0,0} should only match empty string
        let root = parse_regexp("a{0,0}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        let test_cases = vec![
            (vec![VALUE_TERMINATOR], true, "empty"),
            (vec![b'a', VALUE_TERMINATOR], false, "a"),
            (vec![b'a', b'a', VALUE_TERMINATOR], false, "aa"),
        ];

        for (value, should_match, desc) in test_cases {
            bufs.clear();
            traverse_nfa(&table, &value, &mut bufs);
            let matched = bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher));
            assert_eq!(
                matched,
                should_match,
                "a{{0,0}} should {} match '{}'",
                if should_match { "" } else { "NOT" },
                desc
            );
        }
    }

    #[test]
    fn test_range_quantifier_with_dot() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // .{2,4} - any 2-4 characters
        let root = parse_regexp(".{2,4}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        let test_cases = vec![
            (vec![VALUE_TERMINATOR], false, "empty"),
            (vec![b'x', VALUE_TERMINATOR], false, "x"),
            (vec![b'x', b'y', VALUE_TERMINATOR], true, "xy"),
            (vec![b'a', b'b', b'c', VALUE_TERMINATOR], true, "abc"),
            (vec![b'a', b'b', b'c', b'd', VALUE_TERMINATOR], true, "abcd"),
            (
                vec![b'a', b'b', b'c', b'd', b'e', VALUE_TERMINATOR],
                false,
                "abcde",
            ),
        ];

        for (value, should_match, desc) in test_cases {
            bufs.clear();
            traverse_nfa(&table, &value, &mut bufs);
            let matched = bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher));
            assert_eq!(
                matched,
                should_match,
                ".{{2,4}} should {} match '{}'",
                if should_match { "" } else { "NOT" },
                desc
            );
        }
    }

    #[test]
    fn test_range_quantifier_with_group() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // (ab){2,3} - "ab" repeated 2-3 times
        let root = parse_regexp("(ab){2,3}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        let test_cases = vec![
            (vec![VALUE_TERMINATOR], false, "empty"),
            (vec![b'a', b'b', VALUE_TERMINATOR], false, "ab"),
            (vec![b'a', b'b', b'a', b'b', VALUE_TERMINATOR], true, "abab"),
            (
                vec![b'a', b'b', b'a', b'b', b'a', b'b', VALUE_TERMINATOR],
                true,
                "ababab",
            ),
            (
                vec![
                    b'a',
                    b'b',
                    b'a',
                    b'b',
                    b'a',
                    b'b',
                    b'a',
                    b'b',
                    VALUE_TERMINATOR,
                ],
                false,
                "abababab",
            ),
        ];

        for (value, should_match, desc) in test_cases {
            bufs.clear();
            traverse_nfa(&table, &value, &mut bufs);
            let matched = bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher));
            assert_eq!(
                matched,
                should_match,
                "(ab){{2,3}} should {} match '{}'",
                if should_match { "" } else { "NOT" },
                desc
            );
        }
    }

    #[test]
    fn test_range_quantifier_larger_values() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // a{5,10} - between 5 and 10 'a's
        let root = parse_regexp("a{5,10}").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Test boundary cases
        let test_cases: Vec<(usize, bool)> = vec![
            (4, false),  // too few
            (5, true),   // exact min
            (7, true),   // middle
            (10, true),  // exact max
            (11, false), // too many
        ];

        for (count, should_match) in test_cases {
            let mut value: Vec<u8> = vec![b'a'; count];
            value.push(VALUE_TERMINATOR);

            bufs.clear();
            traverse_nfa(&table, &value, &mut bufs);
            let matched = bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher));
            assert_eq!(
                matched,
                should_match,
                "a{{5,10}} should {} match {} 'a's",
                if should_match { "" } else { "NOT" },
                count
            );
        }
    }

    #[test]
    fn test_multi_char_escapes_parse() {
        // Test ~d parses correctly (digits)
        let root = parse_regexp("~d").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert_eq!(root[0][0].runes.len(), 1);
        assert_eq!(root[0][0].runes[0].lo, '0');
        assert_eq!(root[0][0].runes[0].hi, '9');

        // Test ~w parses correctly (word chars: a-z, A-Z, 0-9, _)
        let root = parse_regexp("~w").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert_eq!(root[0][0].runes.len(), 4); // 4 ranges

        // Test ~s parses correctly (whitespace)
        let root = parse_regexp("~s").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert_eq!(root[0][0].runes.len(), 4); // space, tab, newline, carriage return

        // Test ~D parses correctly (non-digits - inverted)
        let root = parse_regexp("~D").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        // Inverted range should have 2 parts: [0, '0'-1] and ['9'+1, MAX]
        assert!(root[0][0].runes.len() >= 2);

        // Test ~W and ~S parse without error
        assert!(parse_regexp("~W").is_ok());
        assert!(parse_regexp("~S").is_ok());
    }

    #[test]
    fn test_multi_char_escapes_nfa() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test ~d matches digits
        let root = parse_regexp("~d").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should match "5"
        let value = vec![b'5', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~d should match '5'"
        );

        // Should NOT match "a"
        bufs.clear();
        let value = vec![b'a', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~d should NOT match 'a'"
        );

        // Test ~w matches word chars
        let root = parse_regexp("~w").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);

        // Should match "a"
        bufs.clear();
        let value = vec![b'a', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~w should match 'a'"
        );

        // Should match "_"
        bufs.clear();
        let value = vec![b'_', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~w should match '_'"
        );

        // Should NOT match "-"
        bufs.clear();
        let value = vec![b'-', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~w should NOT match '-'"
        );

        // Test ~s matches whitespace
        let root = parse_regexp("~s").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);

        // Should match " "
        bufs.clear();
        let value = vec![b' ', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~s should match ' '"
        );

        // Should match "\t"
        bufs.clear();
        let value = vec![b'\t', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~s should match '\\t'"
        );

        // Should NOT match "x"
        bufs.clear();
        let value = vec![b'x', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            !bufs
                .transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~s should NOT match 'x'"
        );
    }

    #[test]
    fn test_multi_char_escapes_in_class() {
        // Test [~d] in character class
        let root = parse_regexp("[~d]").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        assert_eq!(root[0][0].runes.len(), 1);
        assert_eq!(root[0][0].runes[0].lo, '0');
        assert_eq!(root[0][0].runes[0].hi, '9');

        // Test [~da-z] combines digit with range
        let root = parse_regexp("[~da-z]").unwrap();
        assert_eq!(root.len(), 1);
        assert_eq!(root[0].len(), 1);
        // Should have digits [0-9] and [a-z]
        assert!(root[0][0].runes.len() >= 2);
    }

    #[test]
    fn test_multi_char_escape_with_quantifier() {
        use crate::automaton::{traverse_nfa, NfaBuffers, VALUE_TERMINATOR};

        // Test ~d+ matches one or more digits
        let root = parse_regexp("~d+").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);
        let mut bufs = NfaBuffers::new();

        // Should match "123"
        let value = vec![b'1', b'2', b'3', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "~d+ should match '123'"
        );

        // Test ~s{0,3} matches up to 3 whitespace
        let root = parse_regexp("a~s{0,3}b").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);

        // Should match "ab" (0 spaces)
        bufs.clear();
        let value = vec![b'a', b'b', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "a~s{{0,3}}b should match 'ab'"
        );

        // Should match "a  b" (2 spaces)
        bufs.clear();
        let value = vec![b'a', b' ', b' ', b'b', VALUE_TERMINATOR];
        traverse_nfa(&table, &value, &mut bufs);
        assert!(
            bufs.transitions
                .iter()
                .any(|m| Arc::ptr_eq(m, &field_matcher)),
            "a~s{{0,3}}b should match 'a  b'"
        );
    }
}
