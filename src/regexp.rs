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

use std::sync::Arc;

use crate::automaton::{
    arena::{ArenaSmallTable, StateArena, StateId, ARENA_VALUE_TERMINATOR},
    merge_fas, FaState, FieldMatcher, SmallTable, BYTE_CEILING, VALUE_TERMINATOR,
};

/// A pair of runes representing an inclusive range [lo, hi].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RunePair {
    pub lo: char,
    pub hi: char,
}

/// A collection of rune pairs representing a character class.
pub type RuneRange = Vec<RunePair>;

/// Maximum quantifier value (Go uses 100).
const REGEXP_QUANTIFIER_MAX: i32 = 100;

/// The escape character (~ instead of \ to avoid JSON escaping).
const ESCAPE: char = '~';

/// A quantified atom in the regexp tree.
#[derive(Debug, Clone)]
pub struct QuantifiedAtom {
    /// True if this is a `.` (matches any character)
    pub is_dot: bool,
    /// Rune ranges for character matching
    pub runes: RuneRange,
    /// Minimum number of times to match
    pub quant_min: i32,
    /// Maximum number of times to match
    pub quant_max: i32,
    /// Subtree for parenthesized groups
    pub subtree: Option<RegexpRoot>,
}

impl Default for QuantifiedAtom {
    fn default() -> Self {
        Self {
            is_dot: false,
            runes: Vec::new(),
            quant_min: 1,
            quant_max: 1,
            subtree: None,
        }
    }
}

impl QuantifiedAtom {
    /// Returns true if this atom matches exactly once (no quantifier).
    #[inline]
    fn is_singleton(&self) -> bool {
        self.quant_min == 1 && self.quant_max == 1
    }

    /// Returns true if this atom is optional (?).
    #[inline]
    fn is_qm(&self) -> bool {
        self.quant_min == 0 && self.quant_max == 1
    }

    /// Returns true if this atom uses + (one or more).
    #[inline]
    fn is_plus(&self) -> bool {
        self.quant_min == 1 && self.quant_max == REGEXP_QUANTIFIER_MAX
    }

    /// Returns true if this atom uses * (zero or more).
    #[inline]
    fn is_star(&self) -> bool {
        self.quant_min == 0 && self.quant_max == REGEXP_QUANTIFIER_MAX
    }
}

/// A branch in the regexp (sequence of atoms).
pub type RegexpBranch = Vec<QuantifiedAtom>;

/// The root of a parsed regexp (alternatives separated by |).
pub type RegexpRoot = Vec<RegexpBranch>;

/// Features found during parsing (for validation).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegexpFeature {
    Dot,
    Star,
    Plus,
    QuestionMark,
    Range,
    ParenGroup,
    Property,
    Class,
    NegatedClass,
    OrBar,
}

/// Features that are implemented in the NFA builder.
const IMPLEMENTED_FEATURES: &[RegexpFeature] = &[
    RegexpFeature::Dot,
    RegexpFeature::Class,
    RegexpFeature::NegatedClass,
    RegexpFeature::OrBar,
    RegexpFeature::ParenGroup,
    RegexpFeature::QuestionMark,
    RegexpFeature::Plus,
    RegexpFeature::Star,
    RegexpFeature::Range,
];

/// Parser state for regexp parsing.
struct RegexpParse {
    bytes: Vec<u8>,
    index: usize,
    last_index: usize,
    nesting: Vec<RegexpRoot>,
    found_features: Vec<RegexpFeature>,
    tree: RegexpRoot,
}

/// Error type for regexp parsing.
#[derive(Debug, Clone)]
pub struct RegexpError {
    pub message: String,
    pub offset: usize,
}

impl std::fmt::Display for RegexpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at offset {}", self.message, self.offset)
    }
}

impl std::error::Error for RegexpError {}

impl RegexpParse {
    fn new(re: &str) -> Self {
        Self {
            bytes: re.as_bytes().to_vec(),
            index: 0,
            last_index: 0,
            nesting: Vec::new(),
            found_features: Vec::new(),
            tree: Vec::new(),
        }
    }

    fn nest(&mut self) {
        self.nesting.push(std::mem::take(&mut self.tree));
    }

    fn unnest(&mut self) -> RegexpRoot {
        let subtree = std::mem::take(&mut self.tree);
        self.tree = self.nesting.pop().unwrap_or_default();
        subtree
    }

    fn is_nested(&self) -> bool {
        !self.nesting.is_empty()
    }

    fn next_rune(&mut self) -> Result<char, RegexpError> {
        if self.index >= self.bytes.len() {
            return Err(RegexpError {
                message: "end of string".into(),
                offset: self.index,
            });
        }
        self.last_index = self.index;

        // Decode UTF-8
        let remaining = &self.bytes[self.index..];
        let s = std::str::from_utf8(remaining).map_err(|_| RegexpError {
            message: "UTF-8 encoding error".into(),
            offset: self.index,
        })?;

        let c = s.chars().next().ok_or(RegexpError {
            message: "empty string".into(),
            offset: self.index,
        })?;

        self.index += c.len_utf8();
        Ok(c)
    }

    fn require(&mut self, wanted: char) -> Result<(), RegexpError> {
        let got = self.next_rune()?;
        if got != wanted {
            return Err(RegexpError {
                message: format!("expected '{}', got '{}'", wanted, got),
                offset: self.last_index,
            });
        }
        Ok(())
    }

    fn bypass_optional(&mut self, c: char) -> Result<bool, RegexpError> {
        let next = self.next_rune()?;
        if next != c {
            self.backup1(next);
        }
        Ok(next == c)
    }

    fn backup1(&mut self, one_rune: char) {
        self.index -= one_rune.len_utf8();
    }

    fn is_empty(&self) -> bool {
        self.index >= self.bytes.len()
    }

    fn record_feature(&mut self, feature: RegexpFeature) {
        if !self.found_features.contains(&feature) {
            self.found_features.push(feature);
        }
    }

    fn found_unimplemented(&self) -> Vec<RegexpFeature> {
        self.found_features
            .iter()
            .filter(|f| !IMPLEMENTED_FEATURES.contains(f))
            .copied()
            .collect()
    }
}

/// Parse a regexp string into a tree structure.
pub fn parse_regexp(re: &str) -> Result<RegexpRoot, RegexpError> {
    let mut parse = RegexpParse::new(re);
    read_branches(&mut parse)?;

    // Check for unclosed parentheses
    if parse.is_nested() {
        return Err(RegexpError {
            message: "unclosed '('".into(),
            offset: parse.index,
        });
    }

    let unimplemented = parse.found_unimplemented();
    if !unimplemented.is_empty() {
        return Err(RegexpError {
            message: format!("unimplemented features: {:?}", unimplemented),
            offset: 0,
        });
    }

    Ok(parse.tree)
}

/// Read branches separated by |
fn read_branches(parse: &mut RegexpParse) -> Result<(), RegexpError> {
    while !parse.is_empty() {
        let branch = read_branch(parse)?;
        parse.tree.push(branch);

        if parse.is_empty() {
            return Ok(());
        }

        let b = parse.next_rune()?;
        if b == '|' {
            parse.record_feature(RegexpFeature::OrBar);
            continue;
        } else if b == ')' {
            parse.backup1(b);
            return Ok(());
        }
        // Shouldn't reach here
    }
    Ok(())
}

/// Read a single branch (sequence of pieces).
fn read_branch(parse: &mut RegexpParse) -> Result<RegexpBranch, RegexpError> {
    let mut branch = Vec::new();

    loop {
        match read_piece(parse) {
            Ok(piece) => branch.push(piece),
            Err(e) if e.message == "stuck" => break,
            Err(e) if e.message == "end of string" => break,
            Err(e) => return Err(e),
        }
    }

    Ok(branch)
}

/// Read a piece (atom with optional quantifier).
fn read_piece(parse: &mut RegexpParse) -> Result<QuantifiedAtom, RegexpError> {
    let mut qa = read_atom(parse)?;
    read_quantifier(parse, &mut qa)?;
    Ok(qa)
}

/// Check if a character is a "normal" character (not special).
fn is_normal_char(c: char) -> bool {
    let code = c as u32;

    if code <= 0x27 || c == ',' || c == '-' || (0x2F..=0x3E).contains(&code) {
        return true;
    }
    if (0x40..=0x5A).contains(&code) {
        return true;
    }
    // allow backslash
    if code == 0x5c {
        return true;
    }
    if (0x5E..=0x7A).contains(&code) {
        return true;
    }
    // exclude ~
    if (0x7F..=0xD7FF).contains(&code) {
        return true;
    }
    if (0xE000..=0x10FFFF).contains(&code) {
        return true;
    }
    false
}

/// Check for single-char escape sequences.
fn check_single_char_escape(c: char) -> Option<char> {
    let code = c as u32;

    // ( ) * +
    if (0x28..=0x2B).contains(&code) {
        return Some(c);
    }
    // - . ? [ \ ] ^
    if c == '-' || c == '.' || c == '?' || (0x5B..=0x5E).contains(&code) {
        return Some(c);
    }
    // Special escapes
    if c == 'n' {
        return Some('\n');
    }
    if c == 'r' {
        return Some('\r');
    }
    if c == 't' {
        return Some('\t');
    }
    // { | }
    if (0x7B..=0x7D).contains(&code) {
        return Some(c);
    }
    // Escape itself
    if c == ESCAPE {
        return Some(ESCAPE);
    }
    None
}

/// Check for multi-char escape sequences that expand to character classes.
/// Returns Some(RuneRange) for recognized escapes, None otherwise.
fn check_multi_char_escape(c: char) -> Option<RuneRange> {
    match c {
        // ~d = digit [0-9]
        'd' => Some(vec![RunePair { lo: '0', hi: '9' }]),
        // ~D = non-digit (everything except 0-9)
        'D' => Some(invert_rune_range(vec![RunePair { lo: '0', hi: '9' }])),
        // ~w = word char [a-zA-Z0-9_]
        'w' => Some(vec![
            RunePair { lo: 'a', hi: 'z' },
            RunePair { lo: 'A', hi: 'Z' },
            RunePair { lo: '0', hi: '9' },
            RunePair { lo: '_', hi: '_' },
        ]),
        // ~W = non-word char
        'W' => Some(invert_rune_range(vec![
            RunePair { lo: 'a', hi: 'z' },
            RunePair { lo: 'A', hi: 'Z' },
            RunePair { lo: '0', hi: '9' },
            RunePair { lo: '_', hi: '_' },
        ])),
        // ~s = whitespace [ \t\n\r]
        's' => Some(vec![
            RunePair { lo: ' ', hi: ' ' },
            RunePair { lo: '\t', hi: '\t' },
            RunePair { lo: '\n', hi: '\n' },
            RunePair { lo: '\r', hi: '\r' },
        ]),
        // ~S = non-whitespace
        'S' => Some(invert_rune_range(vec![
            RunePair { lo: ' ', hi: ' ' },
            RunePair { lo: '\t', hi: '\t' },
            RunePair { lo: '\n', hi: '\n' },
            RunePair { lo: '\r', hi: '\r' },
        ])),
        _ => None,
    }
}

/// Read an atom.
fn read_atom(parse: &mut RegexpParse) -> Result<QuantifiedAtom, RegexpError> {
    let b = parse.next_rune()?;

    match b {
        c if is_normal_char(c) => Ok(QuantifiedAtom {
            runes: vec![RunePair { lo: c, hi: c }],
            quant_min: 1,
            quant_max: 1,
            ..Default::default()
        }),
        '.' => {
            parse.record_feature(RegexpFeature::Dot);
            Ok(QuantifiedAtom {
                is_dot: true,
                quant_min: 1,
                quant_max: 1,
                ..Default::default()
            })
        }
        '(' => {
            parse.nest();
            parse.record_feature(RegexpFeature::ParenGroup);
            read_branches(parse)?;
            parse.require(')')?;
            let subtree = parse.unnest();
            Ok(QuantifiedAtom {
                subtree: Some(subtree),
                quant_min: 1,
                quant_max: 1,
                ..Default::default()
            })
        }
        ')' => {
            if parse.is_nested() {
                parse.backup1(b);
                Err(RegexpError {
                    message: "stuck".into(),
                    offset: parse.last_index,
                })
            } else {
                Err(RegexpError {
                    message: "unbalanced ')'".into(),
                    offset: parse.last_index,
                })
            }
        }
        '[' => {
            parse.record_feature(RegexpFeature::Class);
            let rr = read_char_class_expr(parse)?;
            Ok(QuantifiedAtom {
                runes: rr,
                quant_min: 1,
                quant_max: 1,
                ..Default::default()
            })
        }
        ']' => Err(RegexpError {
            message: "invalid ']'".into(),
            offset: parse.last_index,
        }),
        c if c == ESCAPE => {
            let next = parse.next_rune().map_err(|_| RegexpError {
                message: format!("'{}' at end of regular expression", ESCAPE),
                offset: parse.last_index,
            })?;

            if let Some(escaped) = check_single_char_escape(next) {
                return Ok(QuantifiedAtom {
                    runes: vec![RunePair {
                        lo: escaped,
                        hi: escaped,
                    }],
                    quant_min: 1,
                    quant_max: 1,
                    ..Default::default()
                });
            }

            // Check for multi-char escapes (~d, ~w, ~s, ~D, ~W, ~S)
            if let Some(runes) = check_multi_char_escape(next) {
                return Ok(QuantifiedAtom {
                    runes,
                    quant_min: 1,
                    quant_max: 1,
                    ..Default::default()
                });
            }

            if next == 'p' || next == 'P' {
                parse.record_feature(RegexpFeature::Property);
                read_category(parse)?;
                return Ok(QuantifiedAtom::default());
            }

            Err(RegexpError {
                message: format!("invalid character '{}' after '{}'", next, ESCAPE),
                offset: parse.last_index,
            })
        }
        '?' | '+' | '*' | '{' => Err(RegexpError {
            message: format!("invalid character '{}' (quantifier without atom)", b),
            offset: parse.last_index,
        }),
        '|' => {
            parse.backup1(b);
            Err(RegexpError {
                message: "stuck".into(),
                offset: parse.last_index,
            })
        }
        _ => {
            parse.backup1(b);
            Err(RegexpError {
                message: "stuck".into(),
                offset: parse.last_index,
            })
        }
    }
}

/// Read a character class expression [...]
fn read_char_class_expr(parse: &mut RegexpParse) -> Result<RuneRange, RegexpError> {
    // Check for unclosed bracket (EOF immediately after '[')
    if parse.is_empty() {
        return Err(RegexpError {
            message: "unclosed character class".into(),
            offset: parse.index,
        });
    }

    // Check for negation
    let is_negated = parse.bypass_optional('^')?;
    if is_negated {
        parse.record_feature(RegexpFeature::NegatedClass);
    }

    let mut rr = read_cce1s(parse)?;

    // Check for trailing -
    if let Ok(true) = parse.bypass_optional('-') {
        rr.push(RunePair { lo: '-', hi: '-' });
    }

    parse.require(']')?;

    // Apply negation if needed
    if is_negated {
        rr = invert_rune_range(rr);
    }

    Ok(rr)
}

/// Read CCE1 elements
fn read_cce1s(parse: &mut RegexpParse) -> Result<RuneRange, RegexpError> {
    let mut rr = Vec::new();
    let mut first = true;

    loop {
        let cce1 = read_cce1(parse, first)?;
        rr.extend(cce1);
        first = false;

        match parse.next_rune() {
            Ok(r) => {
                parse.backup1(r);
                if r == '-' || r == ']' {
                    return Ok(simplify_rune_range(rr));
                }
            }
            Err(_) => {
                // EOF inside character class - unclosed bracket
                return Err(RegexpError {
                    message: "unclosed character class".into(),
                    offset: parse.index,
                });
            }
        }
    }
}

/// Check if a character is valid in a character class
fn is_cc_char(r: char) -> bool {
    let code = r as u32;
    if code <= 0x2c || (0x2e..=0x5A).contains(&code) {
        return true;
    }
    if (0x5e..=0xd7ff).contains(&code) {
        return true;
    }
    if (0xe000..=0x10ffff).contains(&code) {
        return true;
    }
    if r == '\\' {
        return true;
    }
    false
}

/// Read a single CCE1 element
fn read_cce1(parse: &mut RegexpParse, first: bool) -> Result<RuneRange, RegexpError> {
    let r = parse.next_rune().map_err(|_| RegexpError {
        message: "unclosed character class".into(),
        offset: parse.index,
    })?;

    let lo = if first && r == '-' {
        return Ok(vec![RunePair { lo: '-', hi: '-' }]);
    } else if r == ESCAPE {
        let next = parse.next_rune().map_err(|_| RegexpError {
            message: "unclosed character class".into(),
            offset: parse.index,
        })?;
        if next == 'p' || next == 'P' {
            parse.record_feature(RegexpFeature::Property);
            read_category(parse)?;
            return Ok(Vec::new());
        }
        // Check for multi-char escapes (can't participate in ranges)
        if let Some(runes) = check_multi_char_escape(next) {
            return Ok(runes);
        }
        check_single_char_escape(next).ok_or_else(|| RegexpError {
            message: format!(
                "invalid character '{}' after {} in character class",
                next, ESCAPE
            ),
            offset: parse.last_index,
        })?
    } else {
        if !is_cc_char(r) {
            return Err(RegexpError {
                message: format!("invalid character '{}' in character class", r),
                offset: parse.last_index,
            });
        }
        r
    };

    // Check for range
    let next = parse.next_rune().map_err(|_| RegexpError {
        message: "unclosed character class".into(),
        offset: parse.index,
    })?;
    if next != '-' {
        parse.backup1(next);
        return Ok(vec![RunePair { lo, hi: lo }]);
    }

    // Looking at a range?
    let range_end = parse.next_rune().map_err(|_| RegexpError {
        message: "unclosed character class".into(),
        offset: parse.index,
    })?;

    // Might be end of class -]
    if range_end == ']' {
        parse.backup1(range_end);
        return Ok(vec![RunePair { lo, hi: lo }, RunePair { lo: '-', hi: '-' }]);
    }

    let hi = if range_end == ESCAPE {
        let escaped = parse.next_rune().map_err(|_| RegexpError {
            message: "unclosed character class".into(),
            offset: parse.index,
        })?;
        check_single_char_escape(escaped).ok_or_else(|| RegexpError {
            message: format!("invalid char '{}' after - in character class", escaped),
            offset: parse.last_index,
        })?
    } else {
        if !is_cc_char(range_end) {
            return Err(RegexpError {
                message: format!("invalid char '{}' in range", range_end),
                offset: parse.last_index,
            });
        }
        range_end
    };

    if lo > hi {
        return Err(RegexpError {
            message: format!("invalid range {}-{}", lo, hi),
            offset: parse.last_index,
        });
    }

    Ok(vec![RunePair { lo, hi }])
}

/// Simplify and merge overlapping rune ranges
fn simplify_rune_range(mut rranges: RuneRange) -> RuneRange {
    if rranges.is_empty() {
        return rranges;
    }

    rranges.sort_by_key(|rp| rp.lo);

    let mut out = Vec::new();
    let mut current = rranges[0];

    for next in rranges.iter().skip(1).copied() {
        if next.lo as u32 > current.hi as u32 + 1 {
            out.push(current);
            current = next;
            continue;
        }
        if next.hi <= current.hi {
            continue;
        }
        current.hi = next.hi;
    }
    out.push(current);
    out
}

/// Maximum Unicode code point value.
const RUNE_MAX: char = '\u{10FFFF}';

/// Invert a rune range (for negated character classes).
/// Returns a range that matches everything NOT in the input range.
fn invert_rune_range(mut rr: RuneRange) -> RuneRange {
    rr.sort_by_key(|rp| rp.lo);

    let mut inverted = Vec::new();
    let mut point: u32 = 0;

    for pair in &rr {
        let lo = pair.lo as u32;
        if lo > point {
            if let (Some(start), Some(end)) = (char::from_u32(point), char::from_u32(lo - 1)) {
                inverted.push(RunePair { lo: start, hi: end });
            }
        }
        point = pair.hi as u32 + 1;
    }

    if point < RUNE_MAX as u32 {
        if let Some(start) = char::from_u32(point) {
            inverted.push(RunePair {
                lo: start,
                hi: RUNE_MAX,
            });
        }
    }

    inverted
}

/// Read a Unicode category ~p{...} or ~P{...}
fn read_category(parse: &mut RegexpParse) -> Result<(), RegexpError> {
    parse.require('{')?;

    let cat_initial = parse.next_rune()?;
    let valid_initials = ['L', 'M', 'N', 'P', 'Z', 'S', 'C'];
    if !valid_initials.contains(&cat_initial) {
        return Err(RegexpError {
            message: format!("unknown category {}", cat_initial),
            offset: parse.last_index,
        });
    }

    let cat_detail = parse.next_rune()?;
    if cat_detail == '}' {
        return Ok(());
    }

    // Validate detail letter based on initial
    let valid_details = match cat_initial {
        'L' => "ultmo",
        'M' => "nce",
        'N' => "dlo",
        'P' => "cdseifo",
        'Z' => "slp",
        'S' => "mcko",
        'C' => "cfon",
        _ => "",
    };

    if !valid_details.contains(cat_detail) {
        return Err(RegexpError {
            message: format!(
                "unknown category {}p{{{}{}",
                ESCAPE, cat_initial, cat_detail
            ),
            offset: parse.last_index,
        });
    }

    parse.require('}')?;
    Ok(())
}

/// Read a quantifier (?, *, +, {m,n})
fn read_quantifier(parse: &mut RegexpParse, qa: &mut QuantifiedAtom) -> Result<(), RegexpError> {
    let b = match parse.next_rune() {
        Ok(c) => c,
        Err(_) => {
            qa.quant_min = 1;
            qa.quant_max = 1;
            return Ok(());
        }
    };

    match b {
        '*' => {
            parse.record_feature(RegexpFeature::Star);
            qa.quant_min = 0;
            qa.quant_max = REGEXP_QUANTIFIER_MAX;
        }
        '+' => {
            parse.record_feature(RegexpFeature::Plus);
            qa.quant_min = 1;
            qa.quant_max = REGEXP_QUANTIFIER_MAX;
        }
        '?' => {
            parse.record_feature(RegexpFeature::QuestionMark);
            qa.quant_min = 0;
            qa.quant_max = 1;
        }
        '{' => {
            parse.record_feature(RegexpFeature::Range);
            read_range_quantifier(parse, qa)?;
        }
        _ => {
            qa.quant_min = 1;
            qa.quant_max = 1;
            parse.backup1(b);
        }
    }

    Ok(())
}

/// Read a range quantifier {m,n}
fn read_range_quantifier(
    parse: &mut RegexpParse,
    qa: &mut QuantifiedAtom,
) -> Result<(), RegexpError> {
    // Helper to convert EOF to a more specific error
    let next_or_eof = |p: &mut RegexpParse| -> Result<char, RegexpError> {
        p.next_rune().map_err(|e| {
            if e.message == "end of string" {
                RegexpError {
                    message: "unexpected end of string in quantifier".into(),
                    offset: e.offset,
                }
            } else {
                e
            }
        })
    };

    let mut lo_digits = String::new();

    loop {
        let b = next_or_eof(parse)?;
        if b.is_ascii_digit() {
            lo_digits.push(b);
        } else {
            if lo_digits.is_empty() {
                return Err(RegexpError {
                    message: "invalid range quantifier, expecting digits".into(),
                    offset: parse.last_index,
                });
            }

            let lo: i32 = lo_digits.parse().map_err(|_| RegexpError {
                message: "invalid number in quantifier".into(),
                offset: parse.last_index,
            })?;
            qa.quant_min = lo;
            // Default to exact match; will be updated if comma is present
            qa.quant_max = lo;

            match b {
                // {n} means exactly n times
                '}' => return Ok(()),
                // {n,} or {n,m} - will parse upper bound below
                ',' => {
                    // Set to unbounded initially for {n,} case
                    qa.quant_max = REGEXP_QUANTIFIER_MAX;
                    break;
                }
                _ => {
                    return Err(RegexpError {
                        message: format!("unexpected character '{}' in quantifier", b),
                        offset: parse.last_index,
                    });
                }
            }
        }
    }

    // After comma
    let b = next_or_eof(parse)?;
    if b == '}' {
        return Ok(());
    }

    if !b.is_ascii_digit() {
        return Err(RegexpError {
            message: format!(
                "invalid character '{}' in quantifier range, wanted a digit",
                b
            ),
            offset: parse.last_index,
        });
    }

    let mut hi_digits = String::from(b);
    loop {
        let b = next_or_eof(parse)?;
        if b.is_ascii_digit() {
            hi_digits.push(b);
        } else if b == '}' {
            let hi: i32 = hi_digits.parse().map_err(|_| RegexpError {
                message: "invalid number in quantifier".into(),
                offset: parse.last_index,
            })?;
            if hi < qa.quant_min {
                return Err(RegexpError {
                    message: "invalid range quantifier, top must be greater than bottom".into(),
                    offset: parse.last_index,
                });
            }
            qa.quant_max = hi;
            return Ok(());
        } else {
            return Err(RegexpError {
                message: format!("invalid character '{}', expected '}}'", b),
                offset: parse.last_index,
            });
        }
    }
}

// ============================================================================
// NFA Building
// ============================================================================
/// Convert a rune to UTF-8 bytes.
fn rune_to_utf8(r: char) -> Vec<u8> {
    let mut buf = [0u8; 4];
    let s = r.encode_utf8(&mut buf);
    s.as_bytes().to_vec()
}

/// Build a regexp NFA from a parsed tree.
///
/// # Arguments
/// * `root` - The parsed regexp tree
/// * `for_field` - If true, add " matching at start/end for field values
pub fn make_regexp_nfa(root: RegexpRoot, for_field: bool) -> (SmallTable, Arc<FieldMatcher>) {
    let next_field = Arc::new(FieldMatcher::new());

    // Handle empty regexp specially - it matches any string
    if root.is_empty() {
        let table = make_empty_regexp_fa(&next_field);
        return (table, next_field);
    }

    let next_step = make_nfa_trailer(next_field.clone());

    let mut next_step_state = next_step;
    if for_field {
        let table = SmallTable::with_mappings(None, b"\"", &[next_step_state]);
        next_step_state = Arc::new(FaState::with_table(table));
    }

    let table = make_nfa_from_branches(&root, &next_step_state, for_field);
    (table, next_field)
}

/// Generate the last steps in the NFA (field-matched state + valueTerminator).
fn make_nfa_trailer(next_field: Arc<FieldMatcher>) -> Arc<FaState> {
    let match_state = Arc::new(FaState {
        table: SmallTable::new(),
        field_transitions: vec![next_field],
    });
    let table = SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[match_state]);
    Arc::new(FaState::with_table(table))
}

/// Build NFA from branches (alternatives).
fn make_nfa_from_branches(
    root: &RegexpRoot,
    next_step: &Arc<FaState>,
    for_field: bool,
) -> SmallTable {
    let mut fa = SmallTable::new();
    for branch in root {
        let next_branch = if branch.is_empty() {
            // Empty branch - just go directly to next_step (VALUE_TERMINATOR transition)
            // This matches empty string at this position
            SmallTable::with_mappings(None, &[VALUE_TERMINATOR], std::slice::from_ref(next_step))
        } else {
            make_one_regexp_branch_fa(branch, next_step, for_field)
        };
        fa = merge_fas(&fa, &next_branch);
    }
    fa
}

/// Build an FA for empty regexp that matches only empty string.
fn make_empty_regexp_fa(next_field: &Arc<FieldMatcher>) -> SmallTable {
    // Empty regexp matches only empty string.
    // Create a match state and transition only on VALUE_TERMINATOR (end of value).
    let match_state = Arc::new(FaState {
        table: SmallTable::new(),
        field_transitions: vec![next_field.clone()],
    });

    // Only match on VALUE_TERMINATOR -> matches empty string only
    SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[match_state])
}

/// Build the FA for a single quantified atom.
/// Returns the SmallTable for matching this atom (pointing to next_step on match).
fn make_atom_fa(qa: &QuantifiedAtom, next_step: &Arc<FaState>) -> SmallTable {
    if qa.is_dot {
        make_dot_fa(next_step)
    } else if let Some(ref subtree) = qa.subtree {
        make_nfa_from_branches(subtree, next_step, false)
    } else {
        make_rune_range_nfa(&qa.runes, next_step)
    }
}

/// Create the cyclic NFA structure for + and * quantifiers.
///
/// This uses `std::sync::OnceLock` to break the chicken-and-egg problem
/// of creating mutually-referencing Arc structures.
///
/// Structure for [abc]+:
/// - loop_state.table: on 'a'/'b'/'c' -> loopback
/// - loopback.epsilons: [exit_state, loop_state]
///
/// Structure for [abc]*:
/// - Same as above, plus loop_state.table.epsilons includes exit_state
fn create_plus_star_loop(
    qa: &QuantifiedAtom,
    exit_state: &Arc<FaState>,
    is_star: bool,
) -> Arc<FaState> {
    // Create a chain of states to support up to REGEXP_QUANTIFIER_MAX iterations.
    // Each level: loop_table -> loopback -> (exit OR next level)
    //
    // For +: Must match at least once, so no epsilon to exit from entry
    // For *: Can match zero times, so entry has epsilon to exit
    //
    // Structure per level:
    // - loop_table: matches atom, transitions to loopback
    // - loopback: epsilon to exit_state AND epsilon to previous level (or exit for last)

    let depth = REGEXP_QUANTIFIER_MAX as usize;
    let mut next_level: Option<Arc<FaState>> = None;

    // Build from inside out (deepest level first)
    for i in 0..depth {
        // Loopback epsilons: always include exit, and include next_level if it exists
        let mut loopback_epsilons = vec![exit_state.clone()];
        if let Some(ref nl) = next_level {
            loopback_epsilons.push(nl.clone());
        }

        let loopback = Arc::new(FaState::with_table(SmallTable {
            ceilings: Vec::new(),
            steps: Vec::new(),
            epsilons: loopback_epsilons,
            spinout: None,
        }));

        let mut loop_table = make_atom_fa(qa, &loopback);

        // For *, add epsilon to exit (can skip this level entirely)
        if is_star && i == depth - 1 {
            // Only the outermost (returned) level needs the skip epsilon
            loop_table.epsilons.push(exit_state.clone());
        }

        next_level = Some(Arc::new(FaState::with_table(loop_table)));
    }

    next_level.unwrap()
}

/// Build NFA for one branch (sequence of atoms).
/// Implements Thompson construction for quantifiers.
fn make_one_regexp_branch_fa(
    branch: &RegexpBranch,
    next_step: &Arc<FaState>,
    for_field: bool,
) -> SmallTable {
    let mut current_next = next_step.clone();
    let mut table = SmallTable::new();

    // Process atoms back to front, like Go.
    // At the start of each iteration, current_next is "where to go after matching this atom".
    for qa in branch.iter().rev() {
        // The state we want to reach after this atom (before any quantifier modifications)
        let original_next = current_next.clone();

        if qa.is_plus() || qa.is_star() {
            // Thompson construction for + (one or more) and * (zero or more).
            // Uses a helper function that creates a chain of states to simulate
            // the loop (since Rust's Arc doesn't allow true cycles without interior mutability).
            let exit_state = original_next.clone();
            let is_star = qa.is_star();
            let final_loop_state = create_plus_star_loop(qa, &exit_state, is_star);
            table = final_loop_state.table.clone();
            current_next = final_loop_state;
        } else if qa.is_qm() {
            // Thompson construction for ? (optional):
            // Build FA with epsilon to skip
            table = make_atom_fa(qa, &current_next);
            table.epsilons.push(original_next);
            current_next = Arc::new(FaState::with_table(table.clone()));
        } else if qa.is_singleton() {
            // No quantifier - simple FA
            table = make_atom_fa(qa, &current_next);
            current_next = Arc::new(FaState::with_table(table.clone()));
        } else {
            // General {n,m} quantifier - Thompson construction
            // Build (m-n) optional copies first (back to front), then n required copies
            let n = qa.quant_min as usize;
            let m = qa.quant_max as usize;

            // Special case: {0,0} means match zero times - pure epsilon transition
            if n == 0 && m == 0 {
                // No state changes needed - just epsilon to current_next
                table = SmallTable::new();
                table.epsilons.push(current_next.clone());
                current_next = Arc::new(FaState::with_table(table.clone()));
                continue;
            }

            // First, build the optional part (m-n copies, each with epsilon skip)
            // Working back to front, so we build these first
            for _ in n..m {
                table = make_atom_fa(qa, &current_next);
                // Add epsilon to skip this optional match
                table.epsilons.push(current_next.clone());
                current_next = Arc::new(FaState::with_table(table.clone()));
            }

            // Then, build the required part (n copies, no epsilon skip)
            for _ in 0..n {
                table = make_atom_fa(qa, &current_next);
                current_next = Arc::new(FaState::with_table(table.clone()));
            }
        }
    }

    if for_field {
        let first_state = Arc::new(FaState::with_table(table.clone()));
        table = SmallTable::with_mappings(None, b"\"", &[first_state]);
    }

    table
}

/// Rune tree entry for building NFA from rune ranges.
struct RuneTreeEntry {
    next: Option<Arc<FaState>>,
    child: Option<RuneTreeNode>,
}

type RuneTreeNode = Vec<Option<RuneTreeEntry>>;

fn new_rune_tree_node() -> RuneTreeNode {
    (0..BYTE_CEILING).map(|_| None).collect()
}

/// Build NFA from rune tree.
fn nfa_from_rune_tree(root: &RuneTreeNode) -> SmallTable {
    table_from_rune_tree_node(root)
}

fn table_from_rune_tree_node(node: &RuneTreeNode) -> SmallTable {
    let mut unpacked: [Option<Arc<FaState>>; BYTE_CEILING] = std::array::from_fn(|_| None);

    for (b, entry_opt) in node.iter().enumerate() {
        if let Some(entry) = entry_opt {
            if let Some(ref next) = entry.next {
                unpacked[b] = Some(next.clone());
            } else if let Some(ref child) = entry.child {
                let table = table_from_rune_tree_node(child);
                unpacked[b] = Some(Arc::new(FaState::with_table(table)));
            }
        }
    }

    let mut st = SmallTable::new();
    st.pack(&unpacked);
    st
}

/// Build NFA for a rune range.
///
/// This optimized version adds entire RunePairs at once instead of iterating
/// through individual code points. This dramatically improves performance for
/// negated character classes like `[^abc]` which cover ~1.1M Unicode code points.
fn make_rune_range_nfa(rr: &RuneRange, next: &Arc<FaState>) -> SmallTable {
    let mut root = new_rune_tree_node();

    for pair in rr {
        add_rune_pair_tree_entry(&mut root, pair.lo, pair.hi, next);
    }

    nfa_from_rune_tree(&root)
}

// UTF-8 encoding boundaries
const UTF8_1BYTE_MAX: u32 = 0x7F;
const UTF8_2BYTE_MAX: u32 = 0x7FF;
const UTF8_3BYTE_MAX: u32 = 0xFFFF;
const SURROGATE_START: u32 = 0xD800;
const SURROGATE_END: u32 = 0xDFFF;

/// Add a range of runes [lo, hi] to the tree without iterating through each code point.
/// This is the key optimization for negated character classes.
fn add_rune_pair_tree_entry(root: &mut RuneTreeNode, lo: char, hi: char, dest: &Arc<FaState>) {
    let lo_u32 = lo as u32;
    let hi_u32 = hi as u32;

    // Split by UTF-8 encoding boundaries and handle each segment
    let boundaries = [UTF8_1BYTE_MAX, UTF8_2BYTE_MAX, UTF8_3BYTE_MAX, u32::MAX];

    let mut current = lo_u32;
    for &boundary in &boundaries {
        if current > hi_u32 {
            break;
        }

        // Skip boundaries that are below current position
        if boundary < current {
            continue;
        }

        let segment_end = hi_u32.min(boundary);

        // Skip surrogate range for 3-byte sequences
        if current <= SURROGATE_END && segment_end >= SURROGATE_START {
            // Handle pre-surrogate part
            if current < SURROGATE_START {
                let pre_end = (SURROGATE_START - 1).min(segment_end);
                if let (Some(start), Some(end)) = (char::from_u32(current), char::from_u32(pre_end))
                {
                    add_utf8_range_to_tree(root, start, end, dest);
                }
            }
            // Handle post-surrogate part
            if segment_end > SURROGATE_END {
                let post_start = (SURROGATE_END + 1).max(current);
                if let (Some(start), Some(end)) =
                    (char::from_u32(post_start), char::from_u32(segment_end))
                {
                    add_utf8_range_to_tree(root, start, end, dest);
                }
            }
        } else if let (Some(start), Some(end)) =
            (char::from_u32(current), char::from_u32(segment_end))
        {
            add_utf8_range_to_tree(root, start, end, dest);
        }

        current = segment_end + 1;
    }
}

/// Add a range of characters with the same UTF-8 encoding length to the tree.
/// This assumes lo and hi are both valid (non-surrogate) characters with the same encoding length.
fn add_utf8_range_to_tree(root: &mut RuneTreeNode, lo: char, hi: char, dest: &Arc<FaState>) {
    let lo_bytes = rune_to_utf8(lo);
    let hi_bytes = rune_to_utf8(hi);

    debug_assert_eq!(
        lo_bytes.len(),
        hi_bytes.len(),
        "lo and hi must have same UTF-8 length"
    );

    add_byte_range_recursive(root, &lo_bytes, &hi_bytes, 0, dest);
}

/// Recursively add a range of UTF-8 byte sequences to the tree.
///
/// This handles the case where lo_bytes[0..=idx] equals hi_bytes[0..=idx] (common prefix),
/// and the case where they differ (need to split into three parts: lo-to-max, middle, min-to-hi).
fn add_byte_range_recursive(
    node: &mut RuneTreeNode,
    lo_bytes: &[u8],
    hi_bytes: &[u8],
    idx: usize,
    dest: &Arc<FaState>,
) {
    if idx >= lo_bytes.len() {
        return;
    }

    let lo_byte = lo_bytes[idx];
    let hi_byte = hi_bytes[idx];
    let is_last = idx == lo_bytes.len() - 1;

    if lo_byte == hi_byte {
        // Same first byte - recurse with remaining bytes
        ensure_tree_entry(node, lo_byte);
        let entry = node[lo_byte as usize].as_mut().unwrap();

        if is_last {
            entry.next = Some(dest.clone());
        } else {
            if entry.child.is_none() {
                entry.child = Some(new_rune_tree_node());
            }
            add_byte_range_recursive(
                entry.child.as_mut().unwrap(),
                lo_bytes,
                hi_bytes,
                idx + 1,
                dest,
            );
        }
    } else {
        // Different bytes - split into three parts:
        // 1. lo_byte with lo's remaining bytes to max continuation bytes
        // 2. Middle bytes (lo_byte+1 to hi_byte-1) with full continuation range
        // 3. hi_byte with min continuation bytes to hi's remaining bytes

        // Part 1: lo_byte with remaining lo_bytes but max out continuations
        add_lo_range_to_tree(node, lo_bytes, idx, dest);

        // Part 2: Middle range with full continuation bytes
        if hi_byte > lo_byte + 1 {
            add_middle_range_to_tree(
                node,
                lo_byte + 1,
                hi_byte - 1,
                lo_bytes.len() - idx - 1,
                dest,
            );
        }

        // Part 3: hi_byte with min continuation bytes to hi_bytes
        add_hi_range_to_tree(node, hi_bytes, idx, dest);
    }
}

/// Add the lower bound part: lo_bytes[idx] with remaining bytes going up to max continuation.
fn add_lo_range_to_tree(node: &mut RuneTreeNode, lo_bytes: &[u8], idx: usize, dest: &Arc<FaState>) {
    let lo_byte = lo_bytes[idx];
    let is_last = idx == lo_bytes.len() - 1;

    ensure_tree_entry(node, lo_byte);
    let entry = node[lo_byte as usize].as_mut().unwrap();

    if is_last {
        entry.next = Some(dest.clone());
    } else {
        if entry.child.is_none() {
            entry.child = Some(new_rune_tree_node());
        }
        let child = entry.child.as_mut().unwrap();
        let next_byte = lo_bytes[idx + 1];

        // lo_bytes[idx+1] with remaining bytes to max
        add_lo_range_to_tree(child, lo_bytes, idx + 1, dest);

        // Bytes from lo_bytes[idx+1]+1 to 0xBF get full continuation range
        if next_byte < 0xBF {
            add_middle_range_to_tree(child, next_byte + 1, 0xBF, lo_bytes.len() - idx - 2, dest);
        }
    }
}

/// Add the upper bound part: hi_bytes[idx] with min continuation bytes to hi_bytes.
fn add_hi_range_to_tree(node: &mut RuneTreeNode, hi_bytes: &[u8], idx: usize, dest: &Arc<FaState>) {
    let hi_byte = hi_bytes[idx];
    let is_last = idx == hi_bytes.len() - 1;

    ensure_tree_entry(node, hi_byte);
    let entry = node[hi_byte as usize].as_mut().unwrap();

    if is_last {
        entry.next = Some(dest.clone());
    } else {
        if entry.child.is_none() {
            entry.child = Some(new_rune_tree_node());
        }
        let child = entry.child.as_mut().unwrap();
        let next_byte = hi_bytes[idx + 1];

        // Bytes from 0x80 to hi_bytes[idx+1]-1 get full continuation range
        if next_byte > 0x80 {
            add_middle_range_to_tree(child, 0x80, next_byte - 1, hi_bytes.len() - idx - 2, dest);
        }

        // hi_bytes[idx+1] with remaining bytes
        add_hi_range_to_tree(child, hi_bytes, idx + 1, dest);
    }
}

/// Add a range of bytes [lo, hi] that all have `depth` continuation bytes after them.
fn add_middle_range_to_tree(
    node: &mut RuneTreeNode,
    lo: u8,
    hi: u8,
    depth: usize,
    dest: &Arc<FaState>,
) {
    if depth == 0 {
        // These bytes go directly to dest
        for byte in lo..=hi {
            ensure_tree_entry(node, byte);
            node[byte as usize].as_mut().unwrap().next = Some(dest.clone());
        }
    } else {
        // These bytes need continuation byte subtrees
        for byte in lo..=hi {
            ensure_tree_entry(node, byte);
            let entry = node[byte as usize].as_mut().unwrap();
            if entry.child.is_none() {
                entry.child = Some(new_rune_tree_node());
            }
            // Full continuation range 0x80-0xBF
            add_middle_range_to_tree(entry.child.as_mut().unwrap(), 0x80, 0xBF, depth - 1, dest);
        }
    }
}

/// Ensure a tree entry exists at the given byte position.
fn ensure_tree_entry(node: &mut RuneTreeNode, byte: u8) {
    let idx = byte as usize;
    if node[idx].is_none() {
        node[idx] = Some(RuneTreeEntry {
            next: None,
            child: None,
        });
    }
}

/// Build a dot FA that matches any valid UTF-8 character.
///
/// This is more complex than just accepting all bytes because we need to
/// validate UTF-8 encoding and reject surrogates.
pub fn make_dot_fa(dest: &Arc<FaState>) -> SmallTable {
    // Tables for continuation bytes
    let s_last = SmallTable {
        ceilings: vec![0x80, 0xc0, BYTE_CEILING as u8],
        steps: vec![None, Some(dest.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_last = Arc::new(FaState::with_table(s_last));

    let s_last_inter = SmallTable {
        ceilings: vec![0x80, 0xc0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_last_inter = Arc::new(FaState::with_table(s_last_inter));

    let s_first_inter = SmallTable {
        ceilings: vec![0x80, 0xc0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last_inter.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_first_inter = Arc::new(FaState::with_table(s_first_inter));

    // Special handling for E0 (3-byte sequences starting 0xE0)
    let s_e0 = SmallTable {
        ceilings: vec![0xa0, 0xc0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_e0 = Arc::new(FaState::with_table(s_e0));

    // Special handling for ED (surrogates - reject 0xED 0xA0-0xBF)
    let s_ed = SmallTable {
        ceilings: vec![0x80, 0xA0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_ed = Arc::new(FaState::with_table(s_ed));

    // Special handling for F0 (4-byte sequences starting 0xF0)
    let s_f0 = SmallTable {
        ceilings: vec![0x90, 0xC0, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last_inter.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_f0 = Arc::new(FaState::with_table(s_f0));

    // Special handling for F4 (max Unicode)
    let s_f4 = SmallTable {
        ceilings: vec![0x80, 0x90, BYTE_CEILING as u8],
        steps: vec![None, Some(target_last_inter.clone()), None],
        epsilons: Vec::new(),
        spinout: None,
    };
    let target_f4 = Arc::new(FaState::with_table(s_f4));

    // Main dot table
    SmallTable {
        ceilings: vec![
            0x80,               // 0: ASCII (single byte)
            0xC2,               // 1: invalid continuation or overlong
            0xE0,               // 2: 2-byte sequences
            0xE1,               // 3: E0 special case
            0xED,               // 4: 3-byte sequences E1-EC
            0xEE,               // 5: ED special case (surrogates)
            0xF0,               // 6: 3-byte sequences EE-EF
            0xF1,               // 7: F0 special case
            0xF4,               // 8: 4-byte sequences F1-F3
            0xF5,               // 9: F4 special case
            BYTE_CEILING as u8, // 10: invalid
        ],
        steps: vec![
            Some(dest.clone()),               // 0: ASCII
            None,                             // 1: invalid
            Some(target_last.clone()),        // 2: 2-byte
            Some(target_e0.clone()),          // 3: E0
            Some(target_last_inter.clone()),  // 4: E1-EC
            Some(target_ed.clone()),          // 5: ED
            Some(target_last_inter.clone()),  // 6: EE-EF
            Some(target_f0.clone()),          // 7: F0
            Some(target_first_inter.clone()), // 8: F1-F3
            Some(target_f4.clone()),          // 9: F4
            None,                             // 10: invalid
        ],
        epsilons: Vec::new(),
        spinout: None,
    }
}

/// Check if a regexp tree has any `+` or `*` quantifiers that would benefit from arena-based NFA.
pub fn regexp_has_plus_star(root: &RegexpRoot) -> bool {
    for branch in root {
        for qa in branch {
            if qa.is_plus() || qa.is_star() {
                return true;
            }
            // Recursively check subtrees (parenthesized groups)
            if let Some(ref subtree) = qa.subtree {
                if regexp_has_plus_star(subtree) {
                    return true;
                }
            }
        }
    }
    false
}

/// Build an arena-based regexp NFA from a parsed tree.
///
/// This is more efficient than the chain-based approach for patterns with `*` and `+`
/// quantifiers because it uses true cyclic structures (4 states) instead of chained
/// states (100+ states).
///
/// # Arguments
/// * `root` - The parsed regexp tree
/// * `for_field` - If true, add " matching at start/end for field values
///
/// # Returns
/// A tuple of (StateArena, start_state_id, FieldMatcher)
pub fn make_regexp_nfa_arena(
    root: RegexpRoot,
    for_field: bool,
) -> (StateArena, StateId, Arc<FieldMatcher>) {
    let next_field = Arc::new(FieldMatcher::new());

    // Handle empty regexp specially - it matches any string
    if root.is_empty() {
        let mut arena = StateArena::with_capacity(2);

        // Create match state
        let match_state = arena.alloc();
        arena[match_state]
            .field_transitions
            .push(next_field.clone());

        // Create start state that transitions to match on VALUE_TERMINATOR
        let start = arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            &[ARENA_VALUE_TERMINATOR],
            &[match_state],
        ));

        return (arena, start, next_field);
    }

    // Build the arena NFA
    let mut arena = StateArena::with_capacity(16);

    // Create match state (reached at end of value)
    let match_state = arena.alloc();
    arena[match_state]
        .field_transitions
        .push(next_field.clone());

    // Create VALUE_TERMINATOR transition state
    let vt_state = arena.alloc_with_table(ArenaSmallTable::with_mappings(
        StateId::NONE,
        &[ARENA_VALUE_TERMINATOR],
        &[match_state],
    ));

    // If for_field, add trailing quote handling
    let next_step = if for_field {
        arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            b"\"",
            &[vt_state],
        ))
    } else {
        vt_state
    };

    // Build the NFA from branches
    let start = make_arena_nfa_from_branches(&root, &mut arena, next_step, for_field);

    (arena, start, next_field)
}

/// Build arena NFA from branches (alternatives).
fn make_arena_nfa_from_branches(
    root: &RegexpRoot,
    arena: &mut StateArena,
    next_step: StateId,
    for_field: bool,
) -> StateId {
    if root.is_empty() {
        return next_step;
    }

    if root.len() == 1 {
        // Single branch - no alternation needed
        return make_one_arena_branch_fa(&root[0], arena, next_step, for_field);
    }

    // Multiple branches - create a start state with epsilons to each branch
    let mut branch_starts = Vec::with_capacity(root.len());
    for branch in root {
        if branch.is_empty() {
            // Empty branch means we can skip directly to next_step
            branch_starts.push(next_step);
        } else {
            let branch_start = make_one_arena_branch_fa(branch, arena, next_step, false);
            branch_starts.push(branch_start);
        }
    }

    // Create a start state that has epsilons to all branch starts
    let start = arena.alloc();
    arena[start].table.epsilons = branch_starts;

    if for_field {
        // Wrap with leading quote
        arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            b"\"",
            &[start],
        ))
    } else {
        start
    }
}

/// Build arena NFA for one branch (sequence of atoms).
fn make_one_arena_branch_fa(
    branch: &RegexpBranch,
    arena: &mut StateArena,
    next_step: StateId,
    for_field: bool,
) -> StateId {
    let mut current_next = next_step;

    // Process atoms back to front
    for qa in branch.iter().rev() {
        let original_next = current_next;

        if qa.is_plus() || qa.is_star() {
            // Arena-based cyclic NFA for + and *
            current_next = create_arena_plus_star_loop(qa, arena, original_next, qa.is_star());
        } else if qa.is_qm() {
            // Optional: build atom FA with epsilon to skip
            let atom_state = make_arena_atom_fa(qa, arena, current_next);
            arena[atom_state].table.epsilons.push(original_next);
            current_next = atom_state;
        } else if qa.is_singleton() {
            // No quantifier - simple FA
            current_next = make_arena_atom_fa(qa, arena, current_next);
        } else {
            // General {n,m} quantifier
            let n = qa.quant_min as usize;
            let m = qa.quant_max as usize;

            // Special case: {0,0} means match zero times - pure epsilon transition
            if n == 0 && m == 0 {
                let epsilon_state = arena.alloc();
                arena[epsilon_state].table.epsilons.push(current_next);
                current_next = epsilon_state;
                continue;
            }

            // First, build the optional part (m-n copies, each with epsilon skip)
            for _ in n..m {
                let atom_state = make_arena_atom_fa(qa, arena, current_next);
                arena[atom_state].table.epsilons.push(current_next);
                current_next = atom_state;
            }

            // Then, build the required part (n copies, no epsilon skip)
            for _ in 0..n {
                current_next = make_arena_atom_fa(qa, arena, current_next);
            }
        }
    }

    if for_field {
        // Wrap with leading quote
        arena.alloc_with_table(ArenaSmallTable::with_mappings(
            StateId::NONE,
            b"\"",
            &[current_next],
        ))
    } else {
        current_next
    }
}

/// Create a cyclic arena NFA structure for + and * quantifiers.
///
/// Structure for [abc]+:
/// - start --(abc)--> loopback --epsilon--> start (cycle!)
///   --epsilon--> exit
///
/// Structure for [abc]*:
/// - Same as above, plus start has epsilon to exit (can match zero times)
fn create_arena_plus_star_loop(
    qa: &QuantifiedAtom,
    arena: &mut StateArena,
    exit_state: StateId,
    is_star: bool,
) -> StateId {
    // Loopback state - will have epsilons to exit and back to start
    let loopback = arena.alloc();

    // Start state - matches the atom, transitions to loopback
    let start = make_arena_atom_fa_to(qa, arena, loopback);

    // Set up loopback's epsilons: to exit AND back to start (CYCLE!)
    arena[loopback].table.epsilons = vec![exit_state, start];

    // For *, add epsilon from start to exit (can skip entirely)
    if is_star {
        arena[start].table.epsilons.push(exit_state);
    }

    start
}

/// Build arena FA for a single quantified atom.
fn make_arena_atom_fa(qa: &QuantifiedAtom, arena: &mut StateArena, next_step: StateId) -> StateId {
    make_arena_atom_fa_to(qa, arena, next_step)
}

/// Build arena FA for a single atom, transitioning to a specific target state.
fn make_arena_atom_fa_to(qa: &QuantifiedAtom, arena: &mut StateArena, next: StateId) -> StateId {
    if qa.is_dot {
        make_arena_dot_fa(arena, next)
    } else if let Some(ref subtree) = qa.subtree {
        make_arena_nfa_from_branches(subtree, arena, next, false)
    } else {
        make_arena_rune_range_fa(&qa.runes, arena, next)
    }
}

/// Build arena FA for a dot (any character).
fn make_arena_dot_fa(arena: &mut StateArena, dest: StateId) -> StateId {
    // For simplicity, use the same structure as SmallTable's dot FA
    // but with arena states. This matches any valid UTF-8 character.

    // Build continuation byte states (for multi-byte UTF-8)
    let s_last = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(dest);
        table.pack(&unpacked);
        table
    });

    let s_last_inter = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let s_first_inter = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xC0].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    // Special states for specific lead bytes
    let target_e0 = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0xA0..0xC0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let target_ed = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0xA0].fill(s_last);
        table.pack(&unpacked);
        table
    });

    let target_f0 = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x90..0xC0].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    let target_f4 = arena.alloc_with_table({
        let mut table = ArenaSmallTable::new();
        let mut unpacked = [StateId::NONE; BYTE_CEILING];
        unpacked[0x80..0x90].fill(s_last_inter);
        table.pack(&unpacked);
        table
    });

    // Main state with all lead byte transitions
    arena.alloc_with_table({
        let mut unpacked = [StateId::NONE; BYTE_CEILING];

        // ASCII (0x00-0x7F) -> dest directly
        unpacked[..0x80].fill(dest);

        // 2-byte sequences (0xC2-0xDF)
        unpacked[0xC2..0xE0].fill(s_last);

        // E0
        unpacked[0xE0] = target_e0;

        // E1-EC
        unpacked[0xE1..0xED].fill(s_last_inter);

        // ED
        unpacked[0xED] = target_ed;

        // EE-EF
        unpacked[0xEE..0xF0].fill(s_last_inter);

        // F0
        unpacked[0xF0] = target_f0;

        // F1-F3
        unpacked[0xF1..0xF4].fill(s_first_inter);

        // F4
        unpacked[0xF4] = target_f4;

        let mut table = ArenaSmallTable::new();
        table.pack(&unpacked);
        table
    })
}

/// Arena version of the rune tree entry
struct ArenaRuneTreeEntry {
    next: Option<StateId>,
    child: Option<ArenaRuneTreeNode>,
}

type ArenaRuneTreeNode = Vec<Option<ArenaRuneTreeEntry>>;

fn new_arena_rune_tree_node() -> ArenaRuneTreeNode {
    (0..BYTE_CEILING).map(|_| None).collect()
}

fn arena_nfa_from_rune_tree(arena: &mut StateArena, root: &ArenaRuneTreeNode) -> StateId {
    arena_table_from_rune_tree_node(arena, root)
}

fn arena_table_from_rune_tree_node(arena: &mut StateArena, node: &ArenaRuneTreeNode) -> StateId {
    let mut unpacked: [StateId; BYTE_CEILING] = [StateId::NONE; BYTE_CEILING];

    for (b, entry_opt) in node.iter().enumerate() {
        if let Some(entry) = entry_opt {
            if let Some(next) = entry.next {
                unpacked[b] = next;
            } else if let Some(ref child) = entry.child {
                let child_state = arena_table_from_rune_tree_node(arena, child);
                unpacked[b] = child_state;
            }
        }
    }

    let mut table = ArenaSmallTable::new();
    table.pack(&unpacked);
    arena.alloc_with_table(table)
}

/// Build arena NFA for a rune range.
///
/// This optimized version adds entire RunePairs at once instead of iterating
/// through individual code points. This dramatically improves performance for
/// negated character classes like `[^abc]` which cover ~1.1M Unicode code points.
fn make_arena_rune_range_fa(rr: &RuneRange, arena: &mut StateArena, next: StateId) -> StateId {
    let mut root = new_arena_rune_tree_node();

    for pair in rr {
        add_arena_rune_pair_tree_entry(&mut root, pair.lo, pair.hi, next);
    }

    arena_nfa_from_rune_tree(arena, &root)
}

/// Add a range of runes [lo, hi] to the arena tree without iterating through each code point.
fn add_arena_rune_pair_tree_entry(root: &mut ArenaRuneTreeNode, lo: char, hi: char, dest: StateId) {
    let lo_u32 = lo as u32;
    let hi_u32 = hi as u32;

    let boundaries = [UTF8_1BYTE_MAX, UTF8_2BYTE_MAX, UTF8_3BYTE_MAX, u32::MAX];

    let mut current = lo_u32;
    for &boundary in &boundaries {
        if current > hi_u32 {
            break;
        }

        // Skip boundaries that are below current position
        if boundary < current {
            continue;
        }

        let segment_end = hi_u32.min(boundary);

        if current <= SURROGATE_END && segment_end >= SURROGATE_START {
            if current < SURROGATE_START {
                let pre_end = (SURROGATE_START - 1).min(segment_end);
                if let (Some(start), Some(end)) = (char::from_u32(current), char::from_u32(pre_end))
                {
                    add_arena_utf8_range_to_tree(root, start, end, dest);
                }
            }
            if segment_end > SURROGATE_END {
                let post_start = (SURROGATE_END + 1).max(current);
                if let (Some(start), Some(end)) =
                    (char::from_u32(post_start), char::from_u32(segment_end))
                {
                    add_arena_utf8_range_to_tree(root, start, end, dest);
                }
            }
        } else if let (Some(start), Some(end)) =
            (char::from_u32(current), char::from_u32(segment_end))
        {
            add_arena_utf8_range_to_tree(root, start, end, dest);
        }

        current = segment_end + 1;
    }
}

fn add_arena_utf8_range_to_tree(root: &mut ArenaRuneTreeNode, lo: char, hi: char, dest: StateId) {
    let lo_bytes = rune_to_utf8(lo);
    let hi_bytes = rune_to_utf8(hi);

    debug_assert_eq!(lo_bytes.len(), hi_bytes.len());

    add_arena_byte_range_recursive(root, &lo_bytes, &hi_bytes, 0, dest);
}

fn add_arena_byte_range_recursive(
    node: &mut ArenaRuneTreeNode,
    lo_bytes: &[u8],
    hi_bytes: &[u8],
    idx: usize,
    dest: StateId,
) {
    if idx >= lo_bytes.len() {
        return;
    }

    let lo_byte = lo_bytes[idx];
    let hi_byte = hi_bytes[idx];
    let is_last = idx == lo_bytes.len() - 1;

    if lo_byte == hi_byte {
        ensure_arena_tree_entry(node, lo_byte);
        let entry = node[lo_byte as usize].as_mut().unwrap();

        if is_last {
            entry.next = Some(dest);
        } else {
            if entry.child.is_none() {
                entry.child = Some(new_arena_rune_tree_node());
            }
            add_arena_byte_range_recursive(
                entry.child.as_mut().unwrap(),
                lo_bytes,
                hi_bytes,
                idx + 1,
                dest,
            );
        }
    } else {
        add_arena_lo_range_to_tree(node, lo_bytes, idx, dest);

        if hi_byte > lo_byte + 1 {
            add_arena_middle_range_to_tree(
                node,
                lo_byte + 1,
                hi_byte - 1,
                lo_bytes.len() - idx - 1,
                dest,
            );
        }

        add_arena_hi_range_to_tree(node, hi_bytes, idx, dest);
    }
}

fn add_arena_lo_range_to_tree(
    node: &mut ArenaRuneTreeNode,
    lo_bytes: &[u8],
    idx: usize,
    dest: StateId,
) {
    let lo_byte = lo_bytes[idx];
    let is_last = idx == lo_bytes.len() - 1;

    ensure_arena_tree_entry(node, lo_byte);
    let entry = node[lo_byte as usize].as_mut().unwrap();

    if is_last {
        entry.next = Some(dest);
    } else {
        if entry.child.is_none() {
            entry.child = Some(new_arena_rune_tree_node());
        }
        let child = entry.child.as_mut().unwrap();
        let next_byte = lo_bytes[idx + 1];

        add_arena_lo_range_to_tree(child, lo_bytes, idx + 1, dest);

        if next_byte < 0xBF {
            add_arena_middle_range_to_tree(
                child,
                next_byte + 1,
                0xBF,
                lo_bytes.len() - idx - 2,
                dest,
            );
        }
    }
}

fn add_arena_hi_range_to_tree(
    node: &mut ArenaRuneTreeNode,
    hi_bytes: &[u8],
    idx: usize,
    dest: StateId,
) {
    let hi_byte = hi_bytes[idx];
    let is_last = idx == hi_bytes.len() - 1;

    ensure_arena_tree_entry(node, hi_byte);
    let entry = node[hi_byte as usize].as_mut().unwrap();

    if is_last {
        entry.next = Some(dest);
    } else {
        if entry.child.is_none() {
            entry.child = Some(new_arena_rune_tree_node());
        }
        let child = entry.child.as_mut().unwrap();
        let next_byte = hi_bytes[idx + 1];

        if next_byte > 0x80 {
            add_arena_middle_range_to_tree(
                child,
                0x80,
                next_byte - 1,
                hi_bytes.len() - idx - 2,
                dest,
            );
        }

        add_arena_hi_range_to_tree(child, hi_bytes, idx + 1, dest);
    }
}

fn add_arena_middle_range_to_tree(
    node: &mut ArenaRuneTreeNode,
    lo: u8,
    hi: u8,
    depth: usize,
    dest: StateId,
) {
    if depth == 0 {
        for byte in lo..=hi {
            ensure_arena_tree_entry(node, byte);
            node[byte as usize].as_mut().unwrap().next = Some(dest);
        }
    } else {
        for byte in lo..=hi {
            ensure_arena_tree_entry(node, byte);
            let entry = node[byte as usize].as_mut().unwrap();
            if entry.child.is_none() {
                entry.child = Some(new_arena_rune_tree_node());
            }
            add_arena_middle_range_to_tree(
                entry.child.as_mut().unwrap(),
                0x80,
                0xBF,
                depth - 1,
                dest,
            );
        }
    }
}

fn ensure_arena_tree_entry(node: &mut ArenaRuneTreeNode, byte: u8) {
    let idx = byte as usize;
    if node[idx].is_none() {
        node[idx] = Some(ArenaRuneTreeEntry {
            next: None,
            child: None,
        });
    }
}

#[cfg(test)]
mod tests {
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
        let test_cases = vec![
            // {input, expected}
            (
                vec![RunePair { lo: 'b', hi: 'b' }],
                vec![
                    RunePair { lo: '\0', hi: 'a' },
                    RunePair {
                        lo: 'c',
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
