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
    /// Cache key for large Unicode categories (e.g., "L", "Lu", "-L" for negated)
    /// Used to cache pre-built FA shells for performance.
    pub cache_key: Option<String>,
    /// Lookaround assertion type (if this atom is a lookaround group)
    pub lookaround: Option<LookaroundType>,
    /// For ASCII-only negated patterns like `[^x]`, stores the negated bytes (1-3).
    /// Used for memchr acceleration - since JSON input is valid UTF-8, we don't
    /// need UTF-8 validation during matching, so these are the only exit bytes.
    pub ascii_negated_bytes: Option<Vec<u8>>,
}

impl Default for QuantifiedAtom {
    fn default() -> Self {
        Self {
            is_dot: false,
            runes: Vec::new(),
            quant_min: 1,
            quant_max: 1,
            subtree: None,
            cache_key: None,
            lookaround: None,
            ascii_negated_bytes: None,
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

/// Type of lookaround assertion
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LookaroundType {
    /// `(?=...)` - positive lookahead: match if followed by pattern
    PositiveLookahead,
    /// `(?!...)` - negative lookahead: match if NOT followed by pattern
    NegativeLookahead,
    /// `(?<=...)` - positive lookbehind: match if preceded by pattern
    PositiveLookbehind,
    /// `(?<!...)` - negative lookbehind: match if NOT preceded by pattern
    NegativeLookbehind,
}

impl LookaroundType {
    /// Returns true if this is a negative lookaround ((?!...) or (?<!...))
    pub fn is_negative(&self) -> bool {
        matches!(
            self,
            LookaroundType::NegativeLookahead | LookaroundType::NegativeLookbehind
        )
    }

    /// Returns true if this is a lookbehind ((?<=...) or (?<!...))
    pub fn is_lookbehind(&self) -> bool {
        matches!(
            self,
            LookaroundType::PositiveLookbehind | LookaroundType::NegativeLookbehind
        )
    }
}

/// Features found during parsing (for validation).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegexpFeature {
    Dot,
    Star,
    Plus,
    QuestionMark,
    Range,
    ParenGroup,
    NonCapturingGroup,
    LazyQuantifier,
    Property,
    Class,
    NegatedClass,
    OrBar,
    /// Lookaround assertion (?=, ?!, ?<=, ?<!)
    Lookaround,
}

/// Features that are implemented in the NFA builder.
const IMPLEMENTED_FEATURES: &[RegexpFeature] = &[
    RegexpFeature::Dot,
    RegexpFeature::Class,
    RegexpFeature::NegatedClass,
    RegexpFeature::OrBar,
    RegexpFeature::ParenGroup,
    RegexpFeature::NonCapturingGroup,
    RegexpFeature::LazyQuantifier,
    RegexpFeature::QuestionMark,
    RegexpFeature::Plus,
    RegexpFeature::Star,
    RegexpFeature::Range,
    RegexpFeature::Property,
    RegexpFeature::Lookaround,
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

// ============================================================================
// Lookaround Validation
// ============================================================================

/// Check if a tree contains any nested lookaround (lookaround inside lookaround).
/// This is not supported and should be rejected.
fn has_nested_lookaround(tree: &RegexpRoot) -> bool {
    for branch in tree {
        for atom in branch {
            if atom.lookaround.is_some() {
                // This atom IS a lookaround. Check if its subtree contains another lookaround.
                if let Some(subtree) = &atom.subtree {
                    if tree_has_lookaround(subtree) {
                        return true;
                    }
                }
            }
        }
    }
    false
}

/// Recursively check if a tree contains any lookaround at any depth.
fn tree_has_lookaround(tree: &RegexpRoot) -> bool {
    for branch in tree {
        for atom in branch {
            if atom.lookaround.is_some() {
                return true;
            }
            if let Some(subtree) = &atom.subtree {
                if tree_has_lookaround(subtree) {
                    return true;
                }
            }
        }
    }
    false
}

/// Validate lookaround constructs in a parsed tree.
/// Returns Ok(()) if valid, Err with message if invalid.
fn validate_lookarounds(tree: &RegexpRoot) -> Result<(), String> {
    // Check for nested lookarounds
    if has_nested_lookaround(tree) {
        return Err("nested lookaround not supported: `(?=...(?=...)...)`".into());
    }

    // Check for variable-length lookbehind
    for branch in tree {
        for atom in branch {
            if let Some(la_type) = &atom.lookaround {
                if la_type.is_lookbehind() {
                    if let Some(subtree) = &atom.subtree {
                        if has_variable_length_pattern(subtree) {
                            return Err(
                                "variable-length lookbehind not yet supported: `(?<=a+)`; \
                                 use fixed-length like `(?<=aaa)`"
                                    .into(),
                            );
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

/// Check if a pattern can match strings of different lengths.
/// Used to validate lookbehind patterns which require fixed length.
fn has_variable_length_pattern(tree: &RegexpRoot) -> bool {
    // Multiple branches (alternation) can have different lengths
    if tree.len() > 1 {
        // Check if branches have same fixed length
        let first_len = branch_fixed_length(&tree[0]);
        for branch in tree.iter().skip(1) {
            let branch_len = branch_fixed_length(branch);
            if first_len != branch_len {
                return true; // Different lengths in alternation
            }
        }
        // If we get here, all branches have same length (or all are variable)
        return first_len.is_none();
    }

    if tree.is_empty() {
        return false;
    }

    // Single branch - check for quantifiers that cause variable length
    branch_fixed_length(&tree[0]).is_none()
}

/// Calculate the fixed length of a branch, or None if variable length.
fn branch_fixed_length(branch: &RegexpBranch) -> Option<usize> {
    let mut total = 0usize;
    for atom in branch {
        // Variable length quantifiers
        if atom.quant_min != atom.quant_max {
            return None;
        }
        // Star or plus (variable length)
        if atom.is_star() || atom.is_plus() {
            return None;
        }

        let atom_len = if atom.is_dot {
            // Dot matches one character, but UTF-8 encoding varies in length
            // For simplicity, we allow single-character matchers
            1
        } else if !atom.runes.is_empty() {
            // Character class - could have varying UTF-8 lengths
            // But from a character count perspective, it's always 1 char
            1
        } else if let Some(subtree) = &atom.subtree {
            // Group - recurse (but it's the match count, not UTF-8 bytes)
            if subtree.len() == 1 {
                branch_fixed_length(&subtree[0])?
            } else {
                // Alternation - check if all have same length
                let first_len = branch_fixed_length(&subtree[0])?;
                for b in subtree.iter().skip(1) {
                    if branch_fixed_length(b) != Some(first_len) {
                        return None;
                    }
                }
                first_len
            }
        } else {
            0 // Empty atom
        };

        // Multiply by quantifier
        total += atom_len * (atom.quant_min as usize);
    }
    Some(total)
}

/// Collect all lookaround atoms from a tree with their positions.
/// Used for pattern transformation.
pub fn collect_lookarounds(tree: &RegexpRoot) -> Vec<(usize, usize, LookaroundType, RegexpRoot)> {
    let mut result = Vec::new();
    for (branch_idx, branch) in tree.iter().enumerate() {
        for (atom_idx, atom) in branch.iter().enumerate() {
            if let Some(la_type) = atom.lookaround {
                if let Some(subtree) = &atom.subtree {
                    result.push((branch_idx, atom_idx, la_type, subtree.clone()));
                }
            }
        }
    }
    result
}

/// Check if a tree contains any lookaround at the top level.
pub fn has_top_level_lookaround(tree: &RegexpRoot) -> bool {
    for branch in tree {
        for atom in branch {
            if atom.lookaround.is_some() {
                return true;
            }
        }
    }
    false
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

    let tree = std::mem::take(&mut parse.tree);

    // Validate lookaround constructs (nested, variable-length lookbehind)
    if parse.found_features.contains(&RegexpFeature::Lookaround) {
        if let Err(msg) = validate_lookarounds(&tree) {
            return Err(RegexpError {
                message: msg,
                offset: 0,
            });
        }
    }

    let unimplemented = parse.found_unimplemented();
    if !unimplemented.is_empty() {
        return Err(RegexpError {
            message: format!("unimplemented features: {:?}", unimplemented),
            offset: 0,
        });
    }

    Ok(tree)
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

/// XML NameStartChar - characters that can start an XML name (XSD `\i`).
/// From W3C XML spec: https://www.w3.org/TR/xml/#NT-NameStartChar
fn xml_name_start_char() -> RuneRange {
    vec![
        RunePair { lo: ':', hi: ':' }, // 0x3A
        RunePair { lo: 'A', hi: 'Z' }, // 0x41-0x5A
        RunePair { lo: '_', hi: '_' }, // 0x5F
        RunePair { lo: 'a', hi: 'z' }, // 0x61-0x7A
        RunePair {
            lo: '\u{C0}',
            hi: '\u{D6}',
        },
        RunePair {
            lo: '\u{D8}',
            hi: '\u{F6}',
        },
        RunePair {
            lo: '\u{F8}',
            hi: '\u{2FF}',
        },
        RunePair {
            lo: '\u{370}',
            hi: '\u{37D}',
        },
        RunePair {
            lo: '\u{37F}',
            hi: '\u{1FFF}',
        },
        RunePair {
            lo: '\u{200C}',
            hi: '\u{200D}',
        },
        RunePair {
            lo: '\u{2070}',
            hi: '\u{218F}',
        },
        RunePair {
            lo: '\u{2C00}',
            hi: '\u{2FEF}',
        },
        RunePair {
            lo: '\u{3001}',
            hi: '\u{D7FF}',
        },
        RunePair {
            lo: '\u{F900}',
            hi: '\u{FDCF}',
        },
        RunePair {
            lo: '\u{FDF0}',
            hi: '\u{FFFD}',
        },
        RunePair {
            lo: '\u{10000}',
            hi: '\u{EFFFF}',
        },
    ]
}

/// XML NameChar - characters that can appear in an XML name (XSD `\c`).
/// Includes all NameStartChar plus additional characters.
fn xml_name_char() -> RuneRange {
    let mut rr = xml_name_start_char();
    rr.extend([
        RunePair { lo: '-', hi: '-' }, // 0x2D
        RunePair { lo: '.', hi: '.' }, // 0x2E
        RunePair { lo: '0', hi: '9' }, // 0x30-0x39
        RunePair {
            lo: '\u{B7}',
            hi: '\u{B7}',
        },
        RunePair {
            lo: '\u{300}',
            hi: '\u{36F}',
        },
        RunePair {
            lo: '\u{203F}',
            hi: '\u{2040}',
        },
    ]);
    rr
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
        // ~i = XML NameStartChar (initial name char)
        'i' => Some(xml_name_start_char()),
        // ~I = NOT XML NameStartChar
        'I' => Some(invert_rune_range(xml_name_start_char())),
        // ~c = XML NameChar (name char)
        'c' => Some(xml_name_char()),
        // ~C = NOT XML NameChar
        'C' => Some(invert_rune_range(xml_name_char())),
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
            // Check for group extensions (?:...), (?=...), (?!...), (?<=...), (?<!...)
            let mut lookaround_type: Option<LookaroundType> = None;
            match parse.next_rune() {
                Ok('?') => {
                    match parse.next_rune() {
                        Ok(':') => {
                            // Non-capturing group (?:...)
                            parse.record_feature(RegexpFeature::NonCapturingGroup);
                        }
                        Ok('=') => {
                            // Positive lookahead (?=...)
                            parse.record_feature(RegexpFeature::Lookaround);
                            lookaround_type = Some(LookaroundType::PositiveLookahead);
                        }
                        Ok('!') => {
                            // Negative lookahead (?!...)
                            parse.record_feature(RegexpFeature::Lookaround);
                            lookaround_type = Some(LookaroundType::NegativeLookahead);
                        }
                        Ok('<') => {
                            // Lookbehind - need to check next char
                            match parse.next_rune() {
                                Ok('=') => {
                                    // Positive lookbehind (?<=...)
                                    parse.record_feature(RegexpFeature::Lookaround);
                                    lookaround_type = Some(LookaroundType::PositiveLookbehind);
                                }
                                Ok('!') => {
                                    // Negative lookbehind (?<!...)
                                    parse.record_feature(RegexpFeature::Lookaround);
                                    lookaround_type = Some(LookaroundType::NegativeLookbehind);
                                }
                                Ok(c) => {
                                    // Could be named group (?<name>...) - not supported
                                    return Err(RegexpError {
                                        message: format!(
                                            "named capturing groups (?<{}...) not supported",
                                            c
                                        ),
                                        offset: parse.last_index,
                                    });
                                }
                                Err(_) => {
                                    return Err(RegexpError {
                                        message: "unexpected end after (?<".into(),
                                        offset: parse.last_index,
                                    });
                                }
                            }
                        }
                        Ok(c) => {
                            // Some other (? extension we don't support
                            return Err(RegexpError {
                                message: format!("unsupported group extension (?{}...)", c),
                                offset: parse.last_index,
                            });
                        }
                        Err(_) => {
                            return Err(RegexpError {
                                message: "unexpected end after (?".into(),
                                offset: parse.last_index,
                            });
                        }
                    }
                }
                Ok(c) => {
                    // Regular capturing group - backup the character we peeked
                    parse.record_feature(RegexpFeature::ParenGroup);
                    parse.backup1(c);
                }
                Err(_) => {
                    // Empty group or EOF - will fail on require(')') later
                    parse.record_feature(RegexpFeature::ParenGroup);
                }
            }
            read_branches(parse)?;
            parse.require(')')?;
            let subtree = parse.unnest();
            Ok(QuantifiedAtom {
                subtree: Some(subtree),
                quant_min: 1,
                quant_max: 1,
                lookaround: lookaround_type,
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
            let (rr, ascii_negated_bytes) = read_char_class_expr(parse)?;
            Ok(QuantifiedAtom {
                runes: rr,
                quant_min: 1,
                quant_max: 1,
                ascii_negated_bytes,
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
                let (mut runes, cache_key) = read_category(parse)?;
                // ~P{...} means NOT in the category (inverted)
                // For negated categories, prefix cache key with "-"
                let cache_key = if next == 'P' {
                    runes = invert_rune_range(runes);
                    cache_key.map(|k| format!("-{}", k))
                } else {
                    cache_key
                };
                return Ok(QuantifiedAtom {
                    runes,
                    quant_min: 1,
                    quant_max: 1,
                    cache_key,
                    ..Default::default()
                });
            }

            // Backreferences (~1 through ~9) are not supported
            if let Some(digit) = next.to_digit(10) {
                if (1..=9).contains(&digit) {
                    return Err(RegexpError {
                        message: format!("backreferences (~{}) are not supported", digit),
                        offset: parse.last_index,
                    });
                }
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
/// Returns (RuneRange, Option<Vec<u8>>) where the second element contains
/// ASCII negated bytes for patterns like [^x] that can use memchr acceleration.
fn read_char_class_expr(
    parse: &mut RegexpParse,
) -> Result<(RuneRange, Option<Vec<u8>>), RegexpError> {
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

    // Detect ASCII-only negated patterns for memchr acceleration
    // For patterns like [^x], [^/], [^"], we can accelerate by searching for exit bytes
    let ascii_negated_bytes = if is_negated {
        detect_ascii_negated_bytes(&rr)
    } else {
        None
    };

    // Apply negation if needed
    if is_negated {
        rr = invert_rune_range(rr);
    }

    Ok((rr, ascii_negated_bytes))
}

/// Detect if a rune range (BEFORE inversion) represents an ASCII-only character set
/// with 1-3 characters. Used for memchr acceleration of negated patterns.
///
/// For patterns like `[^x]`, the pre-inversion runes are just `[{x, x}]`.
/// We return Some(vec![b'x']) which can be used for memchr acceleration.
fn detect_ascii_negated_bytes(rr: &RuneRange) -> Option<Vec<u8>> {
    let mut bytes = Vec::new();

    for pair in rr {
        // Check if both lo and hi are ASCII
        if pair.lo as u32 >= 128 || pair.hi as u32 >= 128 {
            return None; // Non-ASCII character in negated set
        }

        // Expand the range and collect bytes
        for c in (pair.lo as u8)..=(pair.hi as u8) {
            bytes.push(c);
            if bytes.len() > 3 {
                return None; // Too many exit bytes for memchr acceleration
            }
        }
    }

    if bytes.is_empty() || bytes.len() > 3 {
        return None;
    }

    Some(bytes)
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
            // Inside character class, we don't use the cache key
            let (mut runes, _cache_key) = read_category(parse)?;
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

/// Read a Unicode category ~p{...} or ~P{...} and return the character ranges
/// along with a cache key for large categories.
/// Handles both general categories (Lu, Ll, Nd, etc.) and Unicode blocks (IsBasicLatin, etc.).
/// Returns (ranges, cache_key) where cache_key is Some for general categories.
fn read_category(parse: &mut RegexpParse) -> Result<(RuneRange, Option<String>), RegexpError> {
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
    // Unicode blocks are not cached (smaller than categories)
    if name.starts_with("Is") {
        if let Some(ranges) = get_block_ranges(&name) {
            return Ok((ranges, None));
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

    // Build the cache key (e.g., "L", "Lu", "Nd")
    let cache_key = match detail {
        Some(d) => format!("{}{}", initial, d),
        None => initial.to_string(),
    };

    // Look up the category ranges
    if let Some(ranges) = get_category_ranges(initial, detail) {
        Ok((ranges, Some(cache_key)))
    } else {
        Err(RegexpError {
            message: format!("unknown category ~p{{{}}}", name),
            offset: parse.last_index,
        })
    }
}

/// Read a quantifier (?, *, +, {m,n}) with optional lazy modifier (?)
fn read_quantifier(parse: &mut RegexpParse, qa: &mut QuantifiedAtom) -> Result<(), RegexpError> {
    let b = match parse.next_rune() {
        Ok(c) => c,
        Err(_) => {
            qa.quant_min = 1;
            qa.quant_max = 1;
            return Ok(());
        }
    };

    let mut is_quantifier = true;
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
            is_quantifier = false;
        }
    }

    // Check for lazy quantifier modifier (e.g., *?, +?, {n,m}?)
    // Since we're doing pattern matching (not extraction), lazy vs greedy
    // makes no semantic difference - we just consume the optional '?'
    if is_quantifier {
        match parse.next_rune() {
            Ok('?') => {
                parse.record_feature(RegexpFeature::LazyQuantifier);
                // Lazy quantifier - consumed but has no effect on matching
            }
            Ok(c) => {
                // We consumed a char that wasn't '?', backup
                parse.backup1(c);
            }
            Err(_) => {
                // End of string - nothing to do
            }
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
// Literal Extraction for Prefiltering
// ============================================================================

/// Information about extractable literals from a regexp pattern.
/// Used for prefiltering to quickly reject non-matching inputs.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LiteralInfo {
    /// Literal bytes that must appear at the start of the value.
    /// Empty if pattern doesn't have a literal prefix.
    pub prefix: Vec<u8>,
    /// Literal bytes that must appear at the end of the value.
    /// Empty if pattern doesn't have a literal suffix.
    pub suffix: Vec<u8>,
    /// Literal bytes that must appear somewhere in the value (for patterns
    /// with a single branch that contains literals). Used with memmem.
    /// Empty if no required literal could be extracted.
    pub required: Vec<u8>,
    /// The longest contiguous literal sequence found anywhere in the pattern.
    /// This is the best candidate for memmem-based prefiltering.
    pub longest_literal: Vec<u8>,
}

impl LiteralInfo {
    /// Returns true if this info has any extractable literals.
    #[inline]
    pub fn has_literals(&self) -> bool {
        !self.prefix.is_empty()
            || !self.suffix.is_empty()
            || !self.required.is_empty()
            || !self.longest_literal.is_empty()
    }

    /// Returns true if the value could possibly match based on literals.
    /// This is a quick rejection test - returning true doesn't mean it will match.
    #[inline]
    pub fn prefilter_matches(&self, value: &[u8]) -> bool {
        // Check prefix
        if !self.prefix.is_empty() && !value.starts_with(&self.prefix) {
            return false;
        }
        // Check suffix (excluding terminator byte)
        if !self.suffix.is_empty() {
            // Value has a terminator at the end, so check before it
            let val_len = value.len();
            let suf_len = self.suffix.len();
            if val_len < suf_len + 1 {
                return false;
            }
            let suffix_start = val_len - suf_len - 1; // -1 for terminator
            if &value[suffix_start..val_len - 1] != self.suffix.as_slice() {
                return false;
            }
        }
        // Check required literal (substring search)
        if !self.required.is_empty() && !contains_slice(value, &self.required) {
            return false;
        }
        // Check longest literal
        if !self.longest_literal.is_empty() && !contains_slice(value, &self.longest_literal) {
            return false;
        }
        true
    }
}

/// Simple substring search (can be replaced with memchr::memmem for performance).
#[inline]
fn contains_slice(haystack: &[u8], needle: &[u8]) -> bool {
    if needle.is_empty() {
        return true;
    }
    if needle.len() > haystack.len() {
        return false;
    }
    haystack.windows(needle.len()).any(|w| w == needle)
}

/// Check if an atom is a single literal character (no quantifier, no special features).
fn atom_to_literal_bytes(atom: &QuantifiedAtom) -> Option<Vec<u8>> {
    // Must be a singleton (no quantifier)
    if !atom.is_singleton() {
        return None;
    }
    // Must not be a dot (matches any)
    if atom.is_dot {
        return None;
    }
    // Must not have a subtree (group)
    if atom.subtree.is_some() {
        return None;
    }
    // Must not be a lookaround
    if atom.lookaround.is_some() {
        return None;
    }
    // Must have exactly one rune pair where lo == hi (single character)
    if atom.runes.len() != 1 {
        return None;
    }
    let pair = &atom.runes[0];
    if pair.lo != pair.hi {
        return None;
    }

    // Convert the character to UTF-8 bytes
    let ch = pair.lo;
    let mut buf = [0u8; 4];
    let bytes = ch.encode_utf8(&mut buf);
    Some(bytes.as_bytes().to_vec())
}

/// Extract literal information from a parsed regexp.
/// Returns a LiteralInfo struct with any extractable literals.
pub fn extract_literals(root: &RegexpRoot) -> LiteralInfo {
    let mut info = LiteralInfo::default();

    // For alternation (multiple branches), we can only extract common prefix/suffix
    // For simplicity, only extract from single-branch patterns for now
    if root.len() != 1 {
        // TODO: For alternation, find common prefix/suffix across all branches
        return info;
    }

    let branch = &root[0];
    if branch.is_empty() {
        return info;
    }

    // Extract prefix: consecutive literal atoms from the start
    let mut prefix_bytes = Vec::new();
    for atom in branch {
        if let Some(bytes) = atom_to_literal_bytes(atom) {
            prefix_bytes.extend(bytes);
        } else {
            break;
        }
    }
    info.prefix = prefix_bytes.clone();

    // Extract suffix: consecutive literal atoms from the end
    let mut suffix_bytes = Vec::new();
    for atom in branch.iter().rev() {
        if let Some(bytes) = atom_to_literal_bytes(atom) {
            // Prepend to build suffix correctly
            let mut new_suffix = bytes;
            new_suffix.extend(suffix_bytes);
            suffix_bytes = new_suffix;
        } else {
            break;
        }
    }
    info.suffix = suffix_bytes;

    // Find the longest contiguous literal sequence anywhere in the pattern
    let mut current_literal = Vec::new();
    let mut longest = Vec::new();
    for atom in branch {
        if let Some(bytes) = atom_to_literal_bytes(atom) {
            current_literal.extend(bytes);
        } else {
            if current_literal.len() > longest.len() {
                longest = current_literal.clone();
            }
            current_literal.clear();
        }
    }
    // Check final sequence
    if current_literal.len() > longest.len() {
        longest = current_literal;
    }
    info.longest_literal = longest.clone();

    // For single-branch patterns, the longest literal is also required
    if !longest.is_empty() {
        info.required = longest;
    }

    info
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_ascii_negated_bytes_single_char() {
        // [^x] -> should detect 'x' as exit byte
        let rr = vec![RunePair { lo: 'x', hi: 'x' }];
        let result = detect_ascii_negated_bytes(&rr);
        assert_eq!(result, Some(vec![b'x']));
    }

    #[test]
    fn test_detect_ascii_negated_bytes_multiple_chars() {
        // [^abc] -> should detect a, b, c as exit bytes
        let rr = vec![
            RunePair { lo: 'a', hi: 'a' },
            RunePair { lo: 'b', hi: 'b' },
            RunePair { lo: 'c', hi: 'c' },
        ];
        let result = detect_ascii_negated_bytes(&rr);
        assert_eq!(result, Some(vec![b'a', b'b', b'c']));
    }

    #[test]
    fn test_detect_ascii_negated_bytes_range() {
        // [^a-c] -> should detect a, b, c as exit bytes
        let rr = vec![RunePair { lo: 'a', hi: 'c' }];
        let result = detect_ascii_negated_bytes(&rr);
        assert_eq!(result, Some(vec![b'a', b'b', b'c']));
    }

    #[test]
    fn test_detect_ascii_negated_bytes_too_many() {
        // [^a-z] -> too many exit bytes (26), should return None
        let rr = vec![RunePair { lo: 'a', hi: 'z' }];
        let result = detect_ascii_negated_bytes(&rr);
        assert_eq!(result, None);
    }

    #[test]
    fn test_detect_ascii_negated_bytes_non_ascii() {
        // [^ü] -> non-ASCII, should return None
        let rr = vec![RunePair { lo: 'ü', hi: 'ü' }];
        let result = detect_ascii_negated_bytes(&rr);
        assert_eq!(result, None);
    }

    #[test]
    fn test_detect_ascii_negated_bytes_mixed() {
        // [^aü] -> contains non-ASCII, should return None
        let rr = vec![RunePair { lo: 'a', hi: 'a' }, RunePair { lo: 'ü', hi: 'ü' }];
        let result = detect_ascii_negated_bytes(&rr);
        assert_eq!(result, None);
    }

    #[test]
    fn test_parse_negated_char_class_stores_ascii_bytes() {
        // Parse [^x]+ and verify ascii_negated_bytes is set
        let tree = parse_regexp("[^x]+").unwrap();
        assert_eq!(tree.len(), 1);
        assert_eq!(tree[0].len(), 1);

        let qa = &tree[0][0];
        assert_eq!(qa.ascii_negated_bytes, Some(vec![b'x']));
    }

    #[test]
    fn test_parse_non_negated_class_no_ascii_bytes() {
        // Parse [abc]+ - not negated, so no ascii_negated_bytes
        let tree = parse_regexp("[abc]+").unwrap();
        assert_eq!(tree.len(), 1);
        assert_eq!(tree[0].len(), 1);

        let qa = &tree[0][0];
        assert_eq!(qa.ascii_negated_bytes, None);
    }

    #[test]
    fn test_parse_negated_unicode_class_no_ascii_bytes() {
        // Parse [^ü]+ - negated but non-ASCII, so no ascii_negated_bytes
        let tree = parse_regexp("[^ü]+").unwrap();
        assert_eq!(tree.len(), 1);
        assert_eq!(tree[0].len(), 1);

        let qa = &tree[0][0];
        assert_eq!(qa.ascii_negated_bytes, None);
    }

    #[test]
    fn test_parse_negated_slash_class() {
        // Parse [^/]+ - common pattern for path parsing
        let tree = parse_regexp("[^/]+").unwrap();
        assert_eq!(tree.len(), 1);
        assert_eq!(tree[0].len(), 1);

        let qa = &tree[0][0];
        assert_eq!(qa.ascii_negated_bytes, Some(vec![b'/']));
    }

    #[test]
    fn test_parse_negated_quote_class() {
        // Parse [^"]+ - common pattern for quoted string parsing
        // Note: in I-Regexp, " is a normal character, no escaping needed
        let tree = parse_regexp("[^\"]+").unwrap();
        assert_eq!(tree.len(), 1);
        assert_eq!(tree[0].len(), 1);

        let qa = &tree[0][0];
        assert_eq!(qa.ascii_negated_bytes, Some(vec![b'"']));
    }

    // ========================
    // Literal Extraction Tests
    // ========================

    #[test]
    fn test_extract_literals_pure_literal() {
        // "hello" - pure literal pattern
        let tree = parse_regexp("hello").unwrap();
        let info = extract_literals(&tree);

        assert_eq!(info.prefix, b"hello");
        assert_eq!(info.suffix, b"hello");
        assert_eq!(info.longest_literal, b"hello");
        assert_eq!(info.required, b"hello");
        assert!(info.has_literals());
    }

    #[test]
    fn test_extract_literals_prefix_only() {
        // "abc.*" - literal prefix followed by wildcard
        let tree = parse_regexp("abc.*").unwrap();
        let info = extract_literals(&tree);

        assert_eq!(info.prefix, b"abc");
        assert!(info.suffix.is_empty()); // .* at end
        assert_eq!(info.longest_literal, b"abc");
    }

    #[test]
    fn test_extract_literals_suffix_only() {
        // ".*xyz" - wildcard followed by literal suffix
        let tree = parse_regexp(".*xyz").unwrap();
        let info = extract_literals(&tree);

        assert!(info.prefix.is_empty()); // .* at start
        assert_eq!(info.suffix, b"xyz");
        assert_eq!(info.longest_literal, b"xyz");
    }

    #[test]
    fn test_extract_literals_middle() {
        // ".*@example.com" - literal in middle/end
        let tree = parse_regexp(".*@example~.com").unwrap();
        let info = extract_literals(&tree);

        assert!(info.prefix.is_empty()); // .* at start
        assert_eq!(info.suffix, b"@example.com");
        assert_eq!(info.longest_literal, b"@example.com");
    }

    #[test]
    fn test_extract_literals_no_literals() {
        // ".*" - no extractable literals
        let tree = parse_regexp(".*").unwrap();
        let info = extract_literals(&tree);

        assert!(info.prefix.is_empty());
        assert!(info.suffix.is_empty());
        assert!(info.longest_literal.is_empty());
        assert!(!info.has_literals());
    }

    #[test]
    fn test_extract_literals_alternation() {
        // "a|b" - alternation, no extraction for now
        let tree = parse_regexp("a|b").unwrap();
        let info = extract_literals(&tree);

        // For now, we don't extract from alternation patterns
        assert!(!info.has_literals());
    }

    #[test]
    fn test_extract_literals_char_class() {
        // "[a-z]+" - character class, not a literal
        let tree = parse_regexp("[a-z]+").unwrap();
        let info = extract_literals(&tree);

        assert!(info.prefix.is_empty());
        assert!(info.suffix.is_empty());
        assert!(!info.has_literals());
    }

    #[test]
    fn test_extract_literals_unicode() {
        // "café" - Unicode literal
        let tree = parse_regexp("café").unwrap();
        let info = extract_literals(&tree);

        assert_eq!(info.prefix, "café".as_bytes());
        assert_eq!(info.suffix, "café".as_bytes());
        assert!(info.has_literals());
    }

    #[test]
    fn test_extract_literals_mixed_pattern() {
        // "user_[0-9]+_active" - literals on both sides of a class
        let tree = parse_regexp("user_[0-9]+_active").unwrap();
        let info = extract_literals(&tree);

        assert_eq!(info.prefix, b"user_");
        assert_eq!(info.suffix, b"_active");
        // Longest literal is the longer of prefix or suffix
        assert_eq!(info.longest_literal, b"_active");
    }

    #[test]
    fn test_prefilter_matches_prefix() {
        let info = LiteralInfo {
            prefix: b"hello".to_vec(),
            suffix: Vec::new(),
            required: Vec::new(),
            longest_literal: b"hello".to_vec(),
        };

        // With terminator byte (0xF5)
        assert!(info.prefilter_matches(b"hello world\xF5"));
        assert!(info.prefilter_matches(b"hello\xF5"));
        assert!(!info.prefilter_matches(b"world hello\xF5"));
        assert!(!info.prefilter_matches(b"hell\xF5"));
    }

    #[test]
    fn test_prefilter_matches_suffix() {
        let info = LiteralInfo {
            prefix: Vec::new(),
            suffix: b"world".to_vec(),
            required: Vec::new(),
            longest_literal: b"world".to_vec(),
        };

        // With terminator byte (0xF5)
        assert!(info.prefilter_matches(b"hello world\xF5"));
        assert!(info.prefilter_matches(b"world\xF5"));
        assert!(!info.prefilter_matches(b"world hello\xF5"));
        assert!(!info.prefilter_matches(b"worl\xF5"));
    }

    #[test]
    fn test_prefilter_matches_required() {
        let info = LiteralInfo {
            prefix: Vec::new(),
            suffix: Vec::new(),
            required: b"@example".to_vec(),
            longest_literal: b"@example".to_vec(),
        };

        // With terminator byte (0xF5)
        assert!(info.prefilter_matches(b"user@example.com\xF5"));
        assert!(info.prefilter_matches(b"@example\xF5"));
        assert!(!info.prefilter_matches(b"user@other.com\xF5"));
    }

    #[test]
    fn test_prefilter_matches_combined() {
        let info = LiteralInfo {
            prefix: b"user_".to_vec(),
            suffix: b"_active".to_vec(),
            required: Vec::new(),
            longest_literal: b"_active".to_vec(),
        };

        // With terminator byte (0xF5)
        assert!(info.prefilter_matches(b"user_123_active\xF5"));
        assert!(!info.prefilter_matches(b"admin_123_active\xF5")); // wrong prefix
        assert!(!info.prefilter_matches(b"user_123_inactive\xF5")); // wrong suffix
    }
}
