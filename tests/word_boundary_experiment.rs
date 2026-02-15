/// Word boundary (`~b`) and non-word boundary (`~B`) tests.
///
/// `~b` matches at a position where adjacent characters change word-class:
///   - word char [a-zA-Z0-9_] → non-word char, or vice versa
///   - At string boundaries, the implicit `"` acts as a non-word character
///
/// `~B` matches where adjacent characters are in the SAME word-class.
///
/// Implementation: character-class intersection at the AST level.
/// `~b`/`~B` between atoms A and B constrains A's last char and B's first char
/// to be in compatible word-classes, producing alternation branches.
use quamina::Quamina;

fn q_matches(pattern_json: &str, event_json: &str) -> bool {
    let mut q = Quamina::new();
    q.add_pattern("test", pattern_json).unwrap();
    let matches = q.matches_for_event(event_json.as_bytes()).unwrap();
    matches.contains(&"test")
}

fn q_pattern_err(pattern_json: &str) -> bool {
    let mut q = Quamina::new();
    q.add_pattern("test", pattern_json).is_err()
}

// =========================================================================
// Parser: ~b/~B recognition and expansion
// =========================================================================

#[test]
fn test_expansion_start_boundary() {
    use quamina::regexp::{expand_word_boundaries, has_word_boundary, parse_regexp};
    let tree = parse_regexp("~bhello").unwrap();
    assert!(has_word_boundary(&tree));
    let expanded = expand_word_boundaries(&tree).unwrap();
    // ~b at start: first char ('h' = word char) must be word → trivially satisfied
    assert_eq!(expanded.len(), 1);
    assert_eq!(expanded[0].len(), 5);
}

#[test]
fn test_expansion_impossible_boundary() {
    use quamina::regexp::{expand_word_boundaries, parse_regexp};
    // ab~bcd: 'b' and 'c' are both word chars → ~b impossible → empty tree
    let tree = parse_regexp("ab~bcd").unwrap();
    let expanded = expand_word_boundaries(&tree).unwrap();
    assert!(
        expanded.is_empty(),
        "impossible boundary should produce empty tree"
    );
}

#[test]
fn test_expansion_middle_boundary() {
    use quamina::regexp::{expand_word_boundaries, parse_regexp};
    // ab~b cd: 'b' is word, ' ' is non-word → boundary valid
    let tree = parse_regexp("ab~b cd").unwrap();
    let expanded = expand_word_boundaries(&tree).unwrap();
    assert_eq!(expanded.len(), 1); // Only word→nonword branch survives
}

#[test]
fn test_expansion_dot_star() {
    use quamina::regexp::{expand_word_boundaries, parse_regexp};
    // .*~bcat: dot-star before boundary, 'c' (word) after
    let tree = parse_regexp(".*~bcat").unwrap();
    let expanded = expand_word_boundaries(&tree).unwrap();
    // Two branches:
    // 1. .*{0,MAX-1} ~W c a t (last of .* is non-word, c is word)
    // 2. c a t (when .* matches 0 chars, boundary at value start, c must be word)
    assert_eq!(expanded.len(), 2);
}

// =========================================================================
// Basic ~b at start of value
// =========================================================================

#[test]
fn test_wb_start_word_char() {
    // "hello" starts with 'h' (word char) → ~b at start matches
    assert!(q_matches(
        r#"{"name": [{"regexp": "~bhello"}]}"#,
        r#"{"name": "hello"}"#
    ));
}

#[test]
fn test_wb_start_non_word_char() {
    // " hello" starts with ' ' (non-word) → ~bhello doesn't match
    // (because ~b constrains first char to word, but 'h' is the second char here)
    assert!(!q_matches(
        r#"{"name": [{"regexp": "~bhello"}]}"#,
        r#"{"name": " hello"}"#
    ));
}

// =========================================================================
// Basic ~b at end of value
// =========================================================================

#[test]
fn test_wb_end_word_char() {
    // "hello" ends with 'o' (word char) → ~b at end matches
    assert!(q_matches(
        r#"{"name": [{"regexp": "hello~b"}]}"#,
        r#"{"name": "hello"}"#
    ));
}

#[test]
fn test_wb_end_non_word_char() {
    // "hello " ends with ' ' (non-word) → ~b at end doesn't match
    assert!(!q_matches(
        r#"{"name": [{"regexp": "hello~b"}]}"#,
        r#"{"name": "hello "}"#
    ));
}

// =========================================================================
// ~b in the middle (between specific characters)
// =========================================================================

#[test]
fn test_wb_middle_word_to_nonword() {
    // "hello world": 'o'→' ' = word→nonword = boundary
    assert!(q_matches(
        r#"{"name": [{"regexp": "hello~b world"}]}"#,
        r#"{"name": "hello world"}"#
    ));
}

#[test]
fn test_wb_middle_nonword_to_word() {
    // "hello world": ' '→'w' = nonword→word = boundary
    assert!(q_matches(
        r#"{"name": [{"regexp": "hello ~bworld"}]}"#,
        r#"{"name": "hello world"}"#
    ));
}

#[test]
fn test_wb_middle_word_to_word_no_match() {
    // 'o' and 'w' are both word chars → ~b impossible → pattern never matches
    assert!(!q_matches(
        r#"{"name": [{"regexp": "hello~bworld"}]}"#,
        r#"{"name": "helloworld"}"#
    ));
}

// =========================================================================
// ~B (non-word boundary)
// =========================================================================

#[test]
fn test_nwb_word_to_word() {
    // "helloworld": 'o'→'w' = word→word = same class = ~B matches
    assert!(q_matches(
        r#"{"name": [{"regexp": "hello~Bworld"}]}"#,
        r#"{"name": "helloworld"}"#
    ));
}

#[test]
fn test_nwb_word_to_nonword_no_match() {
    // 'o' is word, ' ' is non-word → different classes → ~B impossible → never matches
    assert!(!q_matches(
        r#"{"name": [{"regexp": "hello~B world"}]}"#,
        r#"{"name": "hello world"}"#
    ));
}

#[test]
fn test_nwb_start_nonword() {
    // ~B at start: `"` is non-word, so first char must also be non-word
    assert!(q_matches(
        r#"{"name": [{"regexp": "~B hello"}]}"#,
        r#"{"name": " hello"}"#
    ));
}

#[test]
fn test_nwb_start_word_no_match() {
    // ~B at start: `"` is non-word, 'h' is word → different → ~B never matches
    assert!(!q_matches(
        r#"{"name": [{"regexp": "~Bhello"}]}"#,
        r#"{"name": "hello"}"#
    ));
}

// =========================================================================
// ~b with .* (whole-word matching)
// =========================================================================

#[test]
fn test_wb_whole_word_match() {
    // "the cat sat" contains "cat" as a whole word
    assert!(q_matches(
        r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#,
        r#"{"name": "the cat sat"}"#
    ));
}

#[test]
fn test_wb_whole_word_no_match() {
    // "concatenate" contains "cat" but NOT as a whole word
    assert!(!q_matches(
        r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#,
        r#"{"name": "concatenate"}"#
    ));
}

#[test]
fn test_wb_whole_word_at_start() {
    // "cat is here" — "cat" at the start of the value
    assert!(q_matches(
        r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#,
        r#"{"name": "cat is here"}"#
    ));
}

#[test]
fn test_wb_whole_word_at_end() {
    // "the cat" — "cat" at the end of the value
    assert!(q_matches(
        r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#,
        r#"{"name": "the cat"}"#
    ));
}

#[test]
fn test_wb_whole_word_only() {
    // "cat" — the entire value is the word
    assert!(q_matches(
        r#"{"name": [{"regexp": ".*~bcat~b.*"}]}"#,
        r#"{"name": "cat"}"#
    ));
}

// =========================================================================
// ~b with underscore (word char)
// =========================================================================

#[test]
fn test_wb_underscore_is_word_char() {
    // '_' is a word char, so no boundary between 'a' and '_' → never matches
    assert!(!q_matches(
        r#"{"name": [{"regexp": "a~b_"}]}"#,
        r#"{"name": "a_"}"#
    ));
}

#[test]
fn test_nwb_underscore_is_word_char() {
    // '_' and 'a' are both word chars → ~B matches
    assert!(q_matches(
        r#"{"name": [{"regexp": "a~B_"}]}"#,
        r#"{"name": "a_"}"#
    ));
}

// =========================================================================
// ~b with digits
// =========================================================================

#[test]
fn test_wb_digit_to_space() {
    // '3'→' ' = word→nonword = boundary
    assert!(q_matches(
        r#"{"name": [{"regexp": "abc3~b end"}]}"#,
        r#"{"name": "abc3 end"}"#
    ));
}

// =========================================================================
// ~b with character classes
// =========================================================================

#[test]
fn test_wb_with_char_class() {
    // [0-9]~b should match: digit followed by non-word boundary
    assert!(q_matches(
        r#"{"name": [{"regexp": "[0-9]~b "}]}"#,
        r#"{"name": "5 "}"#
    ));
}

#[test]
fn test_wb_with_dot() {
    // .~b. — any char, boundary, any char
    // "a " matches: 'a' (word) boundary ' ' (non-word) ✓
    assert!(q_matches(
        r#"{"name": [{"regexp": ".~b."}]}"#,
        r#"{"name": "a "}"#
    ));
    // "ab" doesn't match: 'a' (word) 'b' (word) → no boundary
    assert!(!q_matches(
        r#"{"name": [{"regexp": ".~b."}]}"#,
        r#"{"name": "ab"}"#
    ));
}
