//! Regexp parsing and NFA building for I-Regexp (RFC 9485 subset).
//!
//! This module implements a custom regexp engine that integrates with quamina's automaton.
//! It supports a subset of I-Regexp:
//! - `.` matches any character
//! - `[...]` character classes with ranges
//! - `|` alternation
//! - `(...)` grouping
//! - `?` optional quantifier
//!
//! The escape character is `~` (not `\`) to avoid JSON escaping issues.

use std::sync::Arc;

use crate::automaton::{FaState, FieldMatcher, SmallTable, BYTE_CEILING, VALUE_TERMINATOR, merge_fas};

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
    RegexpFeature::OrBar,
    RegexpFeature::ParenGroup,
    RegexpFeature::QuestionMark,
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

    if code <= 0x27 || c == ',' || c == '-' || (code >= 0x2F && code <= 0x3E) {
        return true;
    }
    if code >= 0x40 && code <= 0x5A {
        return true;
    }
    // allow backslash
    if code == 0x5c {
        return true;
    }
    if code >= 0x5E && code <= 0x7A {
        return true;
    }
    // exclude ~
    if code >= 0x7F && code <= 0xD7FF {
        return true;
    }
    if code >= 0xE000 && code <= 0x10FFFF {
        return true;
    }
    false
}

/// Check for single-char escape sequences.
fn check_single_char_escape(c: char) -> Option<char> {
    let code = c as u32;

    // ( ) * +
    if code >= 0x28 && code <= 0x2B {
        return Some(c);
    }
    // - . ? [ \ ] ^
    if c == '-' || c == '.' || c == '?' || (code >= 0x5B && code <= 0x5E) {
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
    if code >= 0x7B && code <= 0x7D {
        return Some(c);
    }
    // Escape itself
    if c == ESCAPE {
        return Some(ESCAPE);
    }
    None
}

/// Read an atom.
fn read_atom(parse: &mut RegexpParse) -> Result<QuantifiedAtom, RegexpError> {
    let b = parse.next_rune()?;

    match b {
        c if is_normal_char(c) => {
            Ok(QuantifiedAtom {
                runes: vec![RunePair { lo: c, hi: c }],
                quant_min: 1,
                quant_max: 1,
                ..Default::default()
            })
        }
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
        ']' => {
            Err(RegexpError {
                message: "invalid ']'".into(),
                offset: parse.last_index,
            })
        }
        c if c == ESCAPE => {
            let next = parse.next_rune().map_err(|_| RegexpError {
                message: format!("'{}' at end of regular expression", ESCAPE),
                offset: parse.last_index,
            })?;

            if let Some(escaped) = check_single_char_escape(next) {
                return Ok(QuantifiedAtom {
                    runes: vec![RunePair { lo: escaped, hi: escaped }],
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
        '?' | '+' | '*' | '{' => {
            Err(RegexpError {
                message: format!("invalid character '{}' (quantifier without atom)", b),
                offset: parse.last_index,
            })
        }
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
    // Check for negation
    let bypassed = parse.bypass_optional('^')?;
    if bypassed {
        parse.record_feature(RegexpFeature::NegatedClass);
    }

    let mut rr = read_cce1s(parse)?;

    // Check for trailing -
    if let Ok(true) = parse.bypass_optional('-') {
        rr.push(RunePair { lo: '-', hi: '-' });
    }

    parse.require(']')?;
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
    if code <= 0x2c || (code >= 0x2e && code <= 0x5A) {
        return true;
    }
    if code >= 0x5e && code <= 0xd7ff {
        return true;
    }
    if code >= 0xe000 && code <= 0x10ffff {
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
        check_single_char_escape(next).ok_or_else(|| RegexpError {
            message: format!("invalid character '{}' after {} in character class", next, ESCAPE),
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

    for i in 1..rranges.len() {
        let next = rranges[i];
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
            message: format!("unknown category {}p{{{}{}", ESCAPE, cat_initial, cat_detail),
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
fn read_range_quantifier(parse: &mut RegexpParse, qa: &mut QuantifiedAtom) -> Result<(), RegexpError> {
    let mut lo_digits = String::new();

    loop {
        let b = parse.next_rune()?;
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
            qa.quant_max = REGEXP_QUANTIFIER_MAX;

            match b {
                '}' => return Ok(()),
                ',' => break,
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
    let b = parse.next_rune()?;
    if b == '}' {
        return Ok(());
    }

    if !b.is_ascii_digit() {
        return Err(RegexpError {
            message: format!("invalid character '{}' in quantifier range, wanted a digit", b),
            offset: parse.last_index,
        });
    }

    let mut hi_digits = String::from(b);
    loop {
        let b = parse.next_rune()?;
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

/// Iterator over a RuneRange to yield each rune.
struct RuneRangeIterator {
    pairs: RuneRange,
    which_pair: usize,
    in_pair: char,
}

impl RuneRangeIterator {
    fn new(mut rr: RuneRange) -> Option<Self> {
        if rr.is_empty() {
            return None;
        }
        rr.sort_by_key(|rp| rp.lo);
        let first = rr[0].lo;
        Some(Self {
            pairs: rr,
            which_pair: 0,
            in_pair: first,
        })
    }

    fn next_rune(&mut self) -> Option<char> {
        if self.in_pair <= self.pairs[self.which_pair].hi {
            let r = self.in_pair;
            self.in_pair = char::from_u32(self.in_pair as u32 + 1).unwrap_or('\u{FFFF}');
            return Some(r);
        }

        self.which_pair += 1;
        if self.which_pair >= self.pairs.len() {
            return None;
        }

        let r = self.pairs[self.which_pair].lo;
        self.in_pair = char::from_u32(r as u32 + 1).unwrap_or('\u{FFFF}');
        Some(r)
    }
}

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
        let table = SmallTable::with_mappings(None, &[b'"'], &[next_step_state]);
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
            SmallTable::with_mappings(None, &[VALUE_TERMINATOR], &[next_step.clone()])
        } else {
            make_one_regexp_branch_fa(branch, next_step, for_field)
        };
        fa = merge_fas(&fa, &next_branch);
    }
    fa
}

/// Build an FA for empty regexp that matches any input.
/// Creates a match state with default transition to itself (like empty prefix).
fn make_empty_regexp_fa(next_field: &Arc<FieldMatcher>) -> SmallTable {
    // For empty regexp, we want to match any string.
    // Create a match state that has field_transitions and a default transition to itself.
    // This is identical to how prefix_fa handles empty prefix.
    let match_state = Arc::new(FaState {
        table: SmallTable::new(),
        field_transitions: vec![next_field.clone()],
    });

    // Return a table with default transition to match_state
    // On any byte (including VALUE_TERMINATOR), we match
    SmallTable::with_mappings(Some(match_state), &[], &[])
}

/// Build NFA for one branch (sequence of atoms).
fn make_one_regexp_branch_fa(branch: &RegexpBranch, next_step: &Arc<FaState>, for_field: bool) -> SmallTable {
    let mut current_next = next_step.clone();
    let mut table = SmallTable::new();

    // Process atoms back to front
    for qa in branch.iter().rev() {
        if qa.is_dot {
            table = make_dot_fa(&current_next);
            current_next = Arc::new(FaState::with_table(table.clone()));
        } else if let Some(ref subtree) = qa.subtree {
            table = make_nfa_from_branches(subtree, &current_next, false);
            current_next = Arc::new(FaState::with_table(table.clone()));
        } else {
            // Match a rune range
            table = make_rune_range_nfa(&qa.runes, &current_next);

            if qa.quant_max == REGEXP_QUANTIFIER_MAX {
                // + and * not supported yet
                panic!("+ and * in regexp not supported");
            }
            if qa.quant_max > 1 {
                panic!("{{lo,hi}} quantifiers not supported");
            }

            current_next = Arc::new(FaState::with_table(table.clone()));
        }

        // Handle ? (optional)
        if qa.quant_min == 0 {
            // Add epsilon to skip this atom - rebuild with epsilons
            let mut new_table = table.clone();
            new_table.epsilons.push(next_step.clone());
            current_next = Arc::new(FaState::with_table(new_table));
        }
    }

    if for_field {
        let first_state = Arc::new(FaState::with_table(table.clone()));
        table = SmallTable::with_mappings(None, &[b'"'], &[first_state]);
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

/// Add a rune to the tree.
fn add_rune_tree_entry(root: &mut RuneTreeNode, r: char, dest: &Arc<FaState>) {
    let bytes = rune_to_utf8(r);
    add_rune_tree_entry_recursive(root, &bytes, 0, dest);
}

fn add_rune_tree_entry_recursive(
    node: &mut RuneTreeNode,
    bytes: &[u8],
    index: usize,
    dest: &Arc<FaState>,
) {
    if index >= bytes.len() {
        return;
    }

    let idx = bytes[index] as usize;
    if idx >= BYTE_CEILING {
        return; // Invalid byte
    }

    // Ensure entry exists
    if node[idx].is_none() {
        node[idx] = Some(RuneTreeEntry {
            next: None,
            child: None,
        });
    }

    let entry = node[idx].as_mut().unwrap();

    if index == bytes.len() - 1 {
        // Last byte - set destination
        entry.next = Some(dest.clone());
    } else {
        // More bytes to go - recurse into child
        if entry.child.is_none() {
            entry.child = Some(new_rune_tree_node());
        }
        add_rune_tree_entry_recursive(entry.child.as_mut().unwrap(), bytes, index + 1, dest);
    }
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
fn make_rune_range_nfa(rr: &RuneRange, next: &Arc<FaState>) -> SmallTable {
    let mut root = new_rune_tree_node();

    if let Some(mut iter) = RuneRangeIterator::new(rr.clone()) {
        while let Some(r) = iter.next_rune() {
            add_rune_tree_entry(&mut root, r, next);
        }
    }

    nfa_from_rune_tree(&root)
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
            0x80,              // 0: ASCII (single byte)
            0xC2,              // 1: invalid continuation or overlong
            0xE0,              // 2: 2-byte sequences
            0xE1,              // 3: E0 special case
            0xED,              // 4: 3-byte sequences E1-EC
            0xEE,              // 5: ED special case (surrogates)
            0xF0,              // 6: 3-byte sequences EE-EF
            0xF1,              // 7: F0 special case
            0xF4,              // 8: 4-byte sequences F1-F3
            0xF5,              // 9: F4 special case
            BYTE_CEILING as u8, // 10: invalid
        ],
        steps: vec![
            Some(dest.clone()),       // 0: ASCII
            None,                      // 1: invalid
            Some(target_last.clone()), // 2: 2-byte
            Some(target_e0.clone()),   // 3: E0
            Some(target_last_inter.clone()), // 4: E1-EC
            Some(target_ed.clone()),   // 5: ED
            Some(target_last_inter.clone()), // 6: EE-EF
            Some(target_f0.clone()),   // 7: F0
            Some(target_first_inter.clone()), // 8: F1-F3
            Some(target_f4.clone()),   // 9: F4
            None,                      // 10: invalid
        ],
        epsilons: Vec::new(),
        spinout: None,
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
        let rr = vec![
            RunePair { lo: 'a', hi: 'c' },
            RunePair { lo: 'b', hi: 'd' },
        ];
        let simplified = simplify_rune_range(rr);
        assert_eq!(simplified.len(), 1);
        assert_eq!(simplified[0].lo, 'a');
        assert_eq!(simplified[0].hi, 'd');
    }

    #[test]
    fn test_rune_range_iterator() {
        let rr = vec![
            RunePair { lo: 'a', hi: 'c' },
            RunePair { lo: 'f', hi: 'f' },
        ];
        let mut iter = RuneRangeIterator::new(rr).unwrap();
        assert_eq!(iter.next_rune(), Some('a'));
        assert_eq!(iter.next_rune(), Some('b'));
        assert_eq!(iter.next_rune(), Some('c'));
        assert_eq!(iter.next_rune(), Some('f'));
        assert_eq!(iter.next_rune(), None);
    }

    #[test]
    fn test_parse_invalid_unclosed_bracket() {
        // Invalid pattern with unclosed bracket should fail
        let result = parse_regexp("[invalid");
        assert!(result.is_err(), "Unclosed bracket should fail parsing");
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

        // Test that empty regexp NFA matches empty and non-empty strings
        let root = parse_regexp("").unwrap();
        let (table, field_matcher) = make_regexp_nfa(root, false);

        // Test with empty value (just VALUE_TERMINATOR)
        let empty_value = vec![VALUE_TERMINATOR];
        let mut matches = Vec::new();
        traverse_dfa(&table, &empty_value, &mut matches);
        assert!(!matches.is_empty(), "Empty regexp should match empty string");
        assert!(std::sync::Arc::ptr_eq(&matches[0], &field_matcher), "Should transition to field_matcher");

        // Test with non-empty value
        let non_empty_value = vec![b'h', b'i', VALUE_TERMINATOR];
        let mut matches2 = Vec::new();
        traverse_dfa(&table, &non_empty_value, &mut matches2);
        assert!(!matches2.is_empty(), "Empty regexp should match non-empty string");
    }
}
