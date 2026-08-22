//! Normalizes tree-sitter parse trees of Rust functions into sequences of
//! single-byte token-kind symbols.
//!
//! See JOURNAL.md ("Symbol representation: single-byte ASCII, asserted")
//! for why every symbol is exactly one ASCII byte: it makes byte offset,
//! Unicode-scalar offset, and token offset the same number, which both the
//! exact k-gram index and the FST Levenshtein automaton depend on silently.

use rustc_hash::FxHashMap;
use tree_sitter::Node;

/// Highest symbol code we'll hand out. 0 is reserved (never emitted, so it
/// can double as an "absent" sentinel if a caller wants one); 127 is left
/// as headroom above whatever the corpus actually uses. Both bounds keep
/// every symbol a single-byte, single-codepoint ASCII value.
pub const MAX_SYMBOL: u8 = 126;

/// Interns token-kind names (tree-sitter node kinds, or our sentinel names
/// like `IDENT`) as single ASCII bytes, in order of first appearance. The
/// mapping is deterministic given a deterministic corpus walk order, which
/// is what makes results reproducible across runs.
#[derive(Debug, Default)]
pub struct Vocab {
    to_code: FxHashMap<String, u8>,
    to_name: Vec<String>, // index 0 is an unused placeholder
}

impl Vocab {
    pub fn new() -> Self {
        Self {
            to_code: FxHashMap::default(),
            to_name: vec![String::new()],
        }
    }

    /// Interns `name`, returning its byte. Panics if the vocabulary would
    /// grow past [`MAX_SYMBOL`] distinct symbols — see the module doc.
    pub fn intern(&mut self, name: &str) -> u8 {
        if let Some(&code) = self.to_code.get(name) {
            return code;
        }
        let next = self.to_name.len();
        assert!(
            next <= MAX_SYMBOL as usize,
            "token-kind vocabulary exceeded {MAX_SYMBOL} distinct symbols \
             (single-byte-ASCII budget); either the corpus contains a Rust \
             construct the collapse rules in tokenize.rs don't handle, or \
             the reserved range needs to grow"
        );
        #[allow(
            clippy::cast_possible_truncation,
            reason = "next <= MAX_SYMBOL (126) was just asserted"
        )]
        let code = next as u8;
        debug_assert!(
            code != 0 && code.is_ascii(),
            "symbol byte must be nonzero ASCII"
        );
        self.to_code.insert(name.to_string(), code);
        self.to_name.push(name.to_string());
        code
    }

    pub fn name_of(&self, code: u8) -> &str {
        &self.to_name[code as usize]
    }

    /// Number of distinct symbols interned so far.
    pub fn len(&self) -> usize {
        self.to_name.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// A normalized function fragment: its token-kind symbol sequence, plus the
/// `[start, end)` symbol-index span of each direct statement in its body
/// (used by the mutation harness; empty for fragments whose body we didn't
/// walk, which shouldn't happen for anything extracted from a real
/// `function_item`).
#[derive(Debug, Clone)]
pub struct NormalizedFragment {
    pub symbols: Vec<u8>,
    pub stmt_ranges: Vec<(usize, usize)>,
}

/// Maps a tree-sitter node kind that should collapse to a sentinel onto
/// that sentinel's name. Recursion stops at any node this returns `Some`
/// for — we want exactly one symbol for the whole literal/comment/lifetime,
/// not one per internal token (quotes, escape sequences, doc markers...).
fn collapse_sentinel(kind: &str) -> Option<&'static str> {
    match kind {
        "identifier" | "type_identifier" | "field_identifier" | "shorthand_field_identifier" => {
            Some("IDENT")
        }
        "integer_literal" => Some("LIT_INT"),
        "float_literal" => Some("LIT_FLOAT"),
        "string_literal" | "raw_string_literal" => Some("LIT_STR"),
        "char_literal" => Some("LIT_CHAR"),
        "boolean_literal" => Some("LIT_BOOL"),
        "negative_literal" => Some("LIT_NEG"),
        "line_comment" | "block_comment" => Some("COMMENT"),
        "lifetime" => Some("LIFETIME"),
        _ => None,
    }
}

/// True for a `block`'s direct child that should count as one statement
/// slot for the mutation harness: any named child except the comment
/// "extra" nodes tree-sitter injects inline. Anonymous children (the `{`
/// and `}` tokens) are excluded by `is_named()` already.
fn is_statement_child(node: Node) -> bool {
    node.is_named() && !matches!(node.kind(), "line_comment" | "block_comment")
}

/// Normalizes a `function_item` node into a symbol sequence, recording the
/// direct statement ranges of its body (`body` field) along the way.
pub fn normalize_function(func: Node, vocab: &mut Vocab) -> NormalizedFragment {
    let mut out = Vec::new();
    let mut stmt_ranges = Vec::new();
    let block_id = func.child_by_field_name("body").map(|b| b.id());
    emit(func, block_id, vocab, &mut out, &mut stmt_ranges);
    NormalizedFragment {
        symbols: out,
        stmt_ranges,
    }
}

fn emit(
    node: Node,
    block_id: Option<usize>,
    vocab: &mut Vocab,
    out: &mut Vec<u8>,
    stmt_ranges: &mut Vec<(usize, usize)>,
) {
    if let Some(sentinel) = collapse_sentinel(node.kind()) {
        out.push(vocab.intern(sentinel));
        return;
    }

    if Some(node.id()) == block_id {
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            let start = out.len();
            emit(child, block_id, vocab, out, stmt_ranges);
            let end = out.len();
            if end > start && is_statement_child(child) {
                stmt_ranges.push((start, end));
            }
        }
        return;
    }

    if node.child_count() == 0 {
        out.push(vocab.intern(node.kind()));
        return;
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        emit(child, block_id, vocab, out, stmt_ranges);
    }
}

/// Asserts the single-byte-ASCII invariant the whole crate depends on: see
/// the module doc. Called on every fragment right after normalization, and
/// again defensively wherever a k-gram window is sliced, so a violation
/// fails loudly at the point of manufacture rather than as a distant,
/// confusing Levenshtein-distance discrepancy.
pub fn assert_ascii_symbols(symbols: &[u8]) {
    for (i, &b) in symbols.iter().enumerate() {
        assert!(
            b != 0 && b.is_ascii(),
            "symbol at index {i} is not a nonzero ASCII byte ({b}); the \
             byte-offset == char-offset == token-offset invariant this \
             crate relies on for Levenshtein k-grams is broken"
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;

    fn first_function_item<'a>(node: Node<'a>) -> Option<Node<'a>> {
        if node.kind() == "function_item" {
            return Some(node);
        }
        let mut cursor = node.walk();
        for child in node.children(&mut cursor) {
            if let Some(found) = first_function_item(child) {
                return Some(found);
            }
        }
        None
    }

    fn normalize_source(source: &str) -> (NormalizedFragment, Vocab) {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_rust::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(source, None).unwrap();
        let func = first_function_item(tree.root_node())
            .expect("test source must contain a function_item");
        let mut vocab = Vocab::new();
        let normalized = normalize_function(func, &mut vocab);
        (normalized, vocab)
    }

    #[test]
    fn identifiers_and_literals_collapse_keywords_survive() {
        let src = "fn add(left: i32, right: i32) -> i32 { left + right }";
        let (normalized, vocab) = normalize_source(src);
        let names: Vec<&str> = normalized
            .symbols
            .iter()
            .map(|&b| vocab.name_of(b))
            .collect();
        assert_eq!(
            names,
            vec![
                "fn",
                "IDENT",
                "(",
                "IDENT",
                ":",
                "primitive_type",
                ",",
                "IDENT",
                ":",
                "primitive_type",
                ")",
                "->",
                "primitive_type",
                "{",
                "IDENT",
                "+",
                "IDENT",
                "}",
            ]
        );
    }

    #[test]
    fn comments_collapse_to_one_sentinel_and_do_not_split_statements() {
        let src = "fn f() { // hello\n let x = 1; }";
        let (normalized, vocab) = normalize_source(src);
        let names: Vec<&str> = normalized
            .symbols
            .iter()
            .map(|&b| vocab.name_of(b))
            .collect();
        assert!(names.contains(&"COMMENT"));
        // exactly one statement slot (the let), the comment isn't one
        assert_eq!(normalized.stmt_ranges.len(), 1);
    }

    #[test]
    fn lifetimes_and_string_literals_collapse_to_one_symbol_each() {
        let src = r#"fn f<'a>(s: &'a str) -> &'a str { "hello world" ; s }"#;
        let (normalized, vocab) = normalize_source(src);
        let names: Vec<&str> = normalized
            .symbols
            .iter()
            .map(|&b| vocab.name_of(b))
            .collect();
        assert!(names.contains(&"LIFETIME"));
        assert!(names.contains(&"LIT_STR"));
        // the whole string literal, regardless of its length, is one symbol
        assert_eq!(names.iter().filter(|&&n| n == "LIT_STR").count(), 1);
    }

    #[test]
    fn statement_ranges_slice_out_exactly_the_statement_tokens() {
        let src = "fn f() { let a = 1; let b = 2; a + b }";
        let (normalized, vocab) = normalize_source(src);
        assert_eq!(normalized.stmt_ranges.len(), 3); // two lets + tail expression
        for &(s, e) in &normalized.stmt_ranges {
            let text: Vec<&str> = normalized.symbols[s..e]
                .iter()
                .map(|&b| vocab.name_of(b))
                .collect();
            assert!(!text.is_empty());
        }
        // first statement is exactly `let IDENT = LIT_INT ;`
        let (s, e) = normalized.stmt_ranges[0];
        let text: Vec<&str> = normalized.symbols[s..e]
            .iter()
            .map(|&b| vocab.name_of(b))
            .collect();
        assert_eq!(text, vec!["let", "IDENT", "=", "LIT_INT", ";"]);
    }

    #[test]
    fn vocab_intern_is_idempotent_and_stays_ascii() {
        let mut vocab = Vocab::new();
        let a = vocab.intern("IDENT");
        let b = vocab.intern("IDENT");
        let c = vocab.intern("LIT_INT");
        assert_eq!(a, b);
        assert_ne!(a, c);
        assert!(a.is_ascii() && a != 0);
        assert!(c.is_ascii() && c != 0);
    }

    #[test]
    #[should_panic(expected = "exceeded")]
    fn vocab_panics_past_the_reserved_symbol_budget() {
        let mut vocab = Vocab::new();
        for i in 0..(MAX_SYMBOL as usize + 5) {
            vocab.intern(&format!("kind_{i}"));
        }
    }

    #[test]
    #[should_panic(expected = "not a nonzero ASCII byte")]
    fn assert_ascii_symbols_catches_a_zero_byte() {
        assert_ascii_symbols(&[1, 2, 0, 3]);
    }

    #[test]
    #[should_panic(expected = "not a nonzero ASCII byte")]
    fn assert_ascii_symbols_catches_a_non_ascii_byte() {
        assert_ascii_symbols(&[1, 2, 200, 3]);
    }
}
