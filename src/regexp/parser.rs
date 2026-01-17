//! Regexp parsing for I-Regexp (RFC 9485 subset).
//!
//! This module parses regexp strings into a tree structure for NFA construction.
//! Supports:
//! - `.` matches any character
//! - `[...]` character classes with ranges
//! - `[^...]` negated character classes
//! - `|` alternation
//! - `(...)` grouping
//! - `?` optional quantifier
//! - `+` one-or-more quantifier
//! - `*` zero-or-more quantifier
//! - `{n,m}` range quantifiers
//!
//! The escape character is `~` (not `\`) to avoid JSON escaping issues.

use crate::unicode_categories::{get_block_ranges, get_category_ranges};

/// A pair of runes representing an inclusive range [lo, hi].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RunePair {
    pub lo: char,
    pub hi: char,
}

/// A collection of rune pairs representing a character class.
pub type RuneRange = Vec<RunePair>;

/// Maximum quantifier value (Go uses 100).
pub const REGEXP_QUANTIFIER_MAX: i32 = 100;

/// The escape character (~ instead of \ to avoid JSON escaping).
const ESCAPE: char = '~';

/// Maximum Unicode code point value.
pub const RUNE_MAX: char = '\u{10FFFF}';

/// Surrogate range boundaries (these are invalid Unicode code points for chars)
const SURROGATE_START_CP: u32 = 0xD800;
const SURROGATE_END_CP: u32 = 0xDFFF;

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
    pub fn is_singleton(&self) -> bool {
        self.quant_min == 1 && self.quant_max == 1
    }

    /// Returns true if this atom is optional (?).
    #[inline]
    pub fn is_qm(&self) -> bool {
        self.quant_min == 0 && self.quant_max == 1
    }

    /// Returns true if this atom uses + (one or more).
    #[inline]
    pub fn is_plus(&self) -> bool {
        self.quant_min == 1 && self.quant_max == REGEXP_QUANTIFIER_MAX
    }

    /// Returns true if this atom uses * (zero or more).
    #[inline]
    pub fn is_star(&self) -> bool {
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
    RegexpFeature::Property,
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
                let mut runes = read_category(parse)?;
                // ~P{...} means NOT in the category (inverted)
                if next == 'P' {
                    runes = invert_rune_range(runes);
                }
                return Ok(QuantifiedAtom {
                    runes,
                    quant_min: 1,
                    quant_max: 1,
                    ..Default::default()
                });
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
            let mut runes = read_category(parse)?;
            // ~P{...} means NOT in the category (inverted)
            if next == 'P' {
                runes = invert_rune_range(runes);
            }
            return Ok(runes);
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
pub fn simplify_rune_range(mut rranges: RuneRange) -> RuneRange {
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

/// Add a gap range to inverted, handling surrogate boundary
fn add_gap_range(inverted: &mut Vec<RunePair>, start: u32, end: u32) {
    // Skip empty or invalid ranges
    if start > end {
        return;
    }

    // If the range spans the surrogate area, split it
    if start < SURROGATE_START_CP && end >= SURROGATE_START_CP {
        // Part before surrogates
        if let (Some(lo), Some(hi)) = (
            char::from_u32(start),
            char::from_u32(SURROGATE_START_CP - 1),
        ) {
            inverted.push(RunePair { lo, hi });
        }
        // Part after surrogates (if any)
        if end > SURROGATE_END_CP {
            if let (Some(lo), Some(hi)) =
                (char::from_u32(SURROGATE_END_CP + 1), char::from_u32(end))
            {
                inverted.push(RunePair { lo, hi });
            }
        }
    } else if (SURROGATE_START_CP..=SURROGATE_END_CP).contains(&start) {
        // Starts in surrogate range, only add part after
        if end > SURROGATE_END_CP {
            if let (Some(lo), Some(hi)) =
                (char::from_u32(SURROGATE_END_CP + 1), char::from_u32(end))
            {
                inverted.push(RunePair { lo, hi });
            }
        }
    } else {
        // Normal range (not touching surrogates)
        if let (Some(lo), Some(hi)) = (char::from_u32(start), char::from_u32(end)) {
            inverted.push(RunePair { lo, hi });
        }
    }
}

/// Invert a rune range (for negated character classes).
/// Returns a range that matches everything NOT in the input range.
pub fn invert_rune_range(mut rr: RuneRange) -> RuneRange {
    rr.sort_by_key(|rp| rp.lo);

    // Merge overlapping/adjacent ranges after sorting
    let mut merged: Vec<RunePair> = Vec::new();
    for pair in rr {
        if let Some(last) = merged.last_mut() {
            // Check if this pair overlaps or is adjacent to the last merged range
            if pair.lo as u32 <= last.hi as u32 + 1 {
                // Extend the last range if this one goes further
                if pair.hi > last.hi {
                    last.hi = pair.hi;
                }
                continue;
            }
        }
        merged.push(pair);
    }

    let mut inverted = Vec::new();
    let mut point: u32 = 0;

    for pair in &merged {
        let lo = pair.lo as u32;
        if lo > point {
            add_gap_range(&mut inverted, point, lo - 1);
        }
        point = pair.hi as u32 + 1;
    }

    if point <= RUNE_MAX as u32 {
        add_gap_range(&mut inverted, point, RUNE_MAX as u32);
    }

    inverted
}

/// Read a Unicode category ~p{...} or ~P{...} and return the character ranges.
/// Handles both general categories (Lu, Ll, Nd, etc.) and Unicode blocks (IsBasicLatin, etc.).
fn read_category(parse: &mut RegexpParse) -> Result<RuneRange, RegexpError> {
    parse.require('{')?;

    // Collect all characters until '}'
    let mut name = String::new();
    loop {
        let c = parse.next_rune()?;
        if c == '}' {
            break;
        }
        name.push(c);
    }

    if name.is_empty() {
        return Err(RegexpError {
            message: "empty category name".into(),
            offset: parse.last_index,
        });
    }

    // Check for Unicode block (starts with "Is")
    if name.starts_with("Is") {
        if let Some(ranges) = get_block_ranges(&name) {
            return Ok(ranges);
        }
        return Err(RegexpError {
            message: format!("unknown Unicode block ~p{{{}}}", name),
            offset: parse.last_index,
        });
    }

    // Parse as general category
    let mut chars = name.chars();
    let initial = match chars.next() {
        Some(c) => c,
        None => {
            return Err(RegexpError {
                message: "empty category name".into(),
                offset: parse.last_index,
            })
        }
    };

    let valid_initials = ['L', 'M', 'N', 'P', 'Z', 'S', 'C'];
    if !valid_initials.contains(&initial) {
        return Err(RegexpError {
            message: format!("unknown category {}", initial),
            offset: parse.last_index,
        });
    }

    let detail = chars.next();

    // Validate detail letter based on initial
    if let Some(d) = detail {
        let valid_details = match initial {
            'L' => "ultmo",
            'M' => "nce",
            'N' => "dlo",
            'P' => "cdseifo",
            'Z' => "slp",
            'S' => "mcko",
            'C' => "cfon",
            _ => "",
        };

        if !valid_details.contains(d) {
            return Err(RegexpError {
                message: format!("unknown category {}p{{{}{}", ESCAPE, initial, d),
                offset: parse.last_index,
            });
        }

        // Check for extra characters
        if chars.next().is_some() {
            return Err(RegexpError {
                message: format!("invalid category name ~p{{{}}}", name),
                offset: parse.last_index,
            });
        }
    }

    // Look up the category ranges
    if let Some(ranges) = get_category_ranges(initial, detail) {
        Ok(ranges)
    } else {
        Err(RegexpError {
            message: format!("unknown category ~p{{{}}}", name),
            offset: parse.last_index,
        })
    }
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
