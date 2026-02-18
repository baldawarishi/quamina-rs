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
    /// Word boundary marker: Some(true) = `~b`, Some(false) = `~B`.
    pub is_word_boundary: Option<bool>,
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
            is_word_boundary: None,
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
        matches!(self, Self::NegativeLookahead | Self::NegativeLookbehind)
    }

    /// Returns true if this is a lookbehind ((?<=...) or (?<!...))
    pub fn is_lookbehind(&self) -> bool {
        matches!(self, Self::PositiveLookbehind | Self::NegativeLookbehind)
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
    /// Word boundary assertion (~b, ~B)
    WordBoundary,
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
    RegexpFeature::WordBoundary,
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
#[derive(Debug, Clone, PartialEq, Eq)]
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

        let c = s.chars().next().ok_or_else(|| RegexpError {
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

/// Check if a tree contains any word boundary atom (`~b` or `~B`).
pub fn has_word_boundary(tree: &RegexpRoot) -> bool {
    for branch in tree {
        for atom in branch {
            if atom.is_word_boundary.is_some() {
                return true;
            }
        }
    }
    false
}

/// Word char ranges: [a-zA-Z0-9_]
fn word_char_runes() -> RuneRange {
    vec![
        RunePair { lo: 'a', hi: 'z' },
        RunePair { lo: 'A', hi: 'Z' },
        RunePair { lo: '0', hi: '9' },
        RunePair { lo: '_', hi: '_' },
    ]
}

/// Non-word char ranges (inverted word chars)
fn non_word_char_runes() -> RuneRange {
    invert_rune_range(word_char_runes())
}

/// Compute the intersection of two RuneRanges.
/// Returns the set of characters that are in BOTH ranges.
fn intersect_rune_ranges(a: &RuneRange, b: &RuneRange) -> RuneRange {
    let mut result = Vec::new();
    for ap in a {
        for bp in b {
            let lo = ap.lo.max(bp.lo);
            let hi = ap.hi.min(bp.hi);
            if lo <= hi {
                result.push(RunePair { lo, hi });
            }
        }
    }
    simplify_rune_range(result)
}

/// Result of constraining an atom's character class at a word boundary.
/// For singleton atoms, a single constrained atom is returned.
/// For quantified atoms (*, +, {n,m}), we may need to split into
/// "base quantified part" + "constrained last/first char".
enum ConstrainedAtom {
    /// Single atom with class intersection applied
    Single(QuantifiedAtom),
    /// Quantified atom that needs splitting: (base_quantified, constrained_single)
    /// Used when we need to constrain only the last/first char of a quantified run.
    Split(QuantifiedAtom, QuantifiedAtom),
    /// Quantified atom with quant_min=0 (can match zero chars).
    /// First element: Split(base, constrained) for when atom matches 1+ chars.
    /// The caller must also generate a branch where this atom is absent entirely
    /// (for the zero-match case where the boundary is at the value edge).
    SplitOrAbsent(QuantifiedAtom, QuantifiedAtom),
}

/// Constrain an atom's character class at a word boundary position.
///
/// For singleton atoms, intersects the class directly.
/// For quantified atoms (*, +, {n,m} where the boundary-adjacent occurrence
/// needs constraining), splits into base + constrained single.
///
/// `is_last_char`: true if we're constraining the last char (prefix side),
///                 false if constraining the first char (suffix side).
fn constrain_atom_at_boundary(
    atom: &QuantifiedAtom,
    class: &RuneRange,
    is_last_char: bool,
) -> Option<ConstrainedAtom> {
    if atom.subtree.is_some() {
        // Group atom — can't easily intersect
        return None;
    }

    let base_runes = if atom.is_dot {
        // Dot matches any char, use full Unicode range for intersection
        vec![RunePair {
            lo: '\0',
            hi: RUNE_MAX,
        }]
    } else {
        atom.runes.clone()
    };

    let intersected = intersect_rune_ranges(&base_runes, class);
    if intersected.is_empty() {
        return None;
    }

    // Determine cache key: if the result equals ~w or ~W, cache the FA shell
    let cache_key = if intersected == simplify_rune_range(word_char_runes()) {
        Some("wb_w".to_string())
    } else if intersected == simplify_rune_range(non_word_char_runes()) {
        Some("wb_W".to_string())
    } else {
        None
    };

    if atom.is_singleton() {
        // Simple case: just intersect
        return Some(ConstrainedAtom::Single(QuantifiedAtom {
            runes: intersected,
            is_dot: false,
            quant_min: 1,
            quant_max: 1,
            cache_key,
            ..Default::default()
        }));
    }

    // Quantified atom: we need the boundary-adjacent char constrained,
    // but the rest of the quantified run unconstrained.
    let constrained_single = QuantifiedAtom {
        runes: intersected,
        is_dot: false,
        quant_min: 1,
        quant_max: 1,
        cache_key,
        ..Default::default()
    };

    // Adjust the base quantifier: reduce count by 1 since we split off one char.
    // Same logic for both is_last_char and !is_last_char.
    let _ = is_last_char; // Used for documentation; both sides reduce by 1
    let (new_min, new_max) = (
        (atom.quant_min - 1).max(0),
        if atom.quant_max == REGEXP_QUANTIFIER_MAX {
            REGEXP_QUANTIFIER_MAX
        } else {
            atom.quant_max - 1
        },
    );

    if new_max == 0 {
        // The quantified atom was {1,1} effectively → just the constrained single
        return Some(ConstrainedAtom::Single(constrained_single));
    }

    let base = QuantifiedAtom {
        is_dot: atom.is_dot,
        runes: atom.runes.clone(),
        quant_min: new_min,
        quant_max: new_max,
        cache_key: atom.cache_key.clone(),
        ascii_negated_bytes: atom.ascii_negated_bytes.clone(),
        ..Default::default()
    };

    // If the original atom can match zero times (quant_min == 0, e.g., * or ?),
    // then the atom might be absent entirely, meaning the boundary is at the
    // value edge (where `"` is non-word). Return SplitOrAbsent so the caller
    // can generate an additional branch without this atom.
    if atom.quant_min == 0 {
        Some(ConstrainedAtom::SplitOrAbsent(base, constrained_single))
    } else {
        Some(ConstrainedAtom::Split(base, constrained_single))
    }
}

/// Expand a constrained atom into atom sequences.
/// `base_first=true` gives [base, single] (prefix/last-char side),
/// `base_first=false` gives [single, base] (suffix/first-char side).
fn expand_constrained(ca: &ConstrainedAtom, base_first: bool) -> Vec<Vec<QuantifiedAtom>> {
    match ca {
        ConstrainedAtom::Single(a) => vec![vec![a.clone()]],
        ConstrainedAtom::Split(base, single) | ConstrainedAtom::SplitOrAbsent(base, single) => {
            if base_first {
                vec![vec![base.clone(), single.clone()]]
            } else {
                vec![vec![single.clone(), base.clone()]]
            }
        }
    }
}

/// Expand a `~b`/`~B` at value start (no prefix atoms before the boundary).
///
/// The `"` delimiter before the value is non-word, so:
/// - `~b`: first char must be word
/// - `~B`: first char must be non-word
fn expand_wb_at_start(suffix: &[QuantifiedAtom], is_boundary: bool, out: &mut Vec<RegexpBranch>) {
    let wc = word_char_runes();
    let nwc = non_word_char_runes();
    let required_class = if is_boundary { &wc } else { &nwc };

    let Some(constrained) = constrain_atom_at_boundary(&suffix[0], required_class, false) else {
        return;
    };

    for atoms in expand_constrained(&constrained, false) {
        let mut branch = atoms;
        branch.extend_from_slice(&suffix[1..]);
        out.push(branch);
    }

    // SplitOrAbsent: the first suffix atom matched 0 chars, so the boundary
    // falls at value start. Constrain the next real atom instead.
    if matches!(constrained, ConstrainedAtom::SplitOrAbsent(..)) && suffix.len() > 1 {
        if let Some(c2) = constrain_atom_at_boundary(&suffix[1], required_class, false) {
            for atoms in expand_constrained(&c2, false) {
                let mut branch = atoms;
                branch.extend_from_slice(&suffix[2..]);
                out.push(branch);
            }
        }
    }
}

/// Expand a `~b`/`~B` at value end (no suffix atoms after the boundary).
///
/// The `"` delimiter after the value is non-word, so:
/// - `~b`: last char must be word
/// - `~B`: last char must be non-word
fn expand_wb_at_end(prefix: &[QuantifiedAtom], is_boundary: bool, out: &mut Vec<RegexpBranch>) {
    let wc = word_char_runes();
    let nwc = non_word_char_runes();
    let required_class = if is_boundary { &wc } else { &nwc };
    let last_idx = prefix.len() - 1;

    let Some(constrained) = constrain_atom_at_boundary(&prefix[last_idx], required_class, true)
    else {
        return;
    };

    for atoms in expand_constrained(&constrained, true) {
        let mut branch = prefix[..last_idx].to_vec();
        branch.extend(atoms);
        out.push(branch);
    }

    // SplitOrAbsent: last prefix atom matched 0 chars, so boundary falls
    // at the end after the preceding atom. Constrain that one instead.
    if matches!(constrained, ConstrainedAtom::SplitOrAbsent(..)) && last_idx > 0 {
        let prev = last_idx - 1;
        if let Some(c2) = constrain_atom_at_boundary(&prefix[prev], required_class, true) {
            for atoms in expand_constrained(&c2, true) {
                let mut branch = prefix[..prev].to_vec();
                branch.extend(atoms);
                out.push(branch);
            }
        }
    }
}

/// Expand a `~b`/`~B` in the middle (between prefix and suffix atoms).
///
/// Both sides are constrained: the last char of prefix and the first char of suffix
/// must be in opposite word-classes (`~b`) or the same class (`~B`).
fn expand_wb_in_middle(
    prefix: &[QuantifiedAtom],
    suffix: &[QuantifiedAtom],
    is_boundary: bool,
    out: &mut Vec<RegexpBranch>,
) {
    let wc = word_char_runes();
    let nwc = non_word_char_runes();
    let last_idx = prefix.len() - 1;

    // ~b: (last=word, first=nonword) OR (last=nonword, first=word)
    // ~B: (last=word, first=word) OR (last=nonword, first=nonword)
    let class_pairs: [(&RuneRange, &RuneRange); 2] = if is_boundary {
        [(&wc, &nwc), (&nwc, &wc)]
    } else {
        [(&wc, &wc), (&nwc, &nwc)]
    };

    for (last_class, first_class) in &class_pairs {
        let cl = constrain_atom_at_boundary(&prefix[last_idx], last_class, true);
        let cf = constrain_atom_at_boundary(&suffix[0], first_class, false);

        let (Some(ref cl), Some(ref cf)) = (&cl, &cf) else {
            continue;
        };

        // Generate all combinations from constrained prefix × suffix
        for pe in &expand_constrained(cl, true) {
            for se in &expand_constrained(cf, false) {
                let mut branch = prefix[..last_idx].to_vec();
                branch.extend(pe.clone());
                branch.extend(se.clone());
                branch.extend_from_slice(&suffix[1..]);
                out.push(branch);
            }
        }

        // SplitOrAbsent on prefix side: prefix atom absent → boundary at value start.
        // The `"` is non-word, so constrain suffix to edge class.
        if matches!(cl, ConstrainedAtom::SplitOrAbsent(..)) {
            let edge_class = if is_boundary { &wc } else { &nwc };
            if let Some(c2) = constrain_atom_at_boundary(&suffix[0], edge_class, false) {
                for se in expand_constrained(&c2, false) {
                    let mut branch = prefix[..last_idx].to_vec();
                    branch.extend(se);
                    branch.extend_from_slice(&suffix[1..]);
                    out.push(branch);
                }
            }
        }

        // SplitOrAbsent on suffix side: suffix atom absent → boundary at value end.
        if matches!(cf, ConstrainedAtom::SplitOrAbsent(..)) {
            let edge_class = if is_boundary { &wc } else { &nwc };
            if let Some(c2) = constrain_atom_at_boundary(&prefix[last_idx], edge_class, true) {
                for pe in expand_constrained(&c2, true) {
                    let mut branch = prefix[..last_idx].to_vec();
                    branch.extend(pe);
                    branch.extend_from_slice(&suffix[1..]);
                    out.push(branch);
                }
            }
        }
    }
}

/// Expand word boundaries (`~b`/`~B`) in a regexp tree using character-class intersection.
///
/// For `A~bB`: The last char of A and first char of B must be in different word-char classes.
/// This is implemented by intersecting A's last atom with `~w`/`~W` and B's first atom
/// with the opposite class, producing two alternative branches.
///
/// Returns the expanded tree (may have more branches than the input).
pub fn expand_word_boundaries(tree: &RegexpRoot) -> Result<RegexpRoot, String> {
    let mut result_branches: Vec<RegexpBranch> = Vec::new();

    for branch in tree {
        let wb_count = branch
            .iter()
            .filter(|a| a.is_word_boundary.is_some())
            .count();
        if wb_count == 0 {
            result_branches.push(branch.clone());
            continue;
        }
        if wb_count > 4 {
            return Err("too many word boundaries in pattern (max 4)".into());
        }

        // Expand one word boundary at a time (each pass resolves the first remaining ~b/~B)
        let mut alternatives = vec![branch.clone()];

        loop {
            let mut new_alternatives = Vec::new();
            let mut found_wb = false;

            for alt in &alternatives {
                let wb_pos = alt.iter().position(|a| a.is_word_boundary.is_some());
                let Some(pos) = wb_pos else {
                    new_alternatives.push(alt.clone());
                    continue;
                };
                found_wb = true;
                let is_boundary = alt[pos].is_word_boundary.unwrap();

                let prefix = &alt[..pos];
                let suffix = &alt[pos + 1..];

                match (prefix.is_empty(), suffix.is_empty()) {
                    (true, true) => {
                        // ~b alone: between two `"` (non-word). Boundary = never, non-boundary = always.
                        if !is_boundary {
                            new_alternatives.push(Vec::new());
                        }
                    }
                    (true, false) => expand_wb_at_start(suffix, is_boundary, &mut new_alternatives),
                    (false, true) => expand_wb_at_end(prefix, is_boundary, &mut new_alternatives),
                    (false, false) => {
                        expand_wb_in_middle(prefix, suffix, is_boundary, &mut new_alternatives)
                    }
                }
            }

            alternatives = new_alternatives;
            if !found_wb {
                break;
            }
        }

        result_branches.extend(alternatives);
    }

    // An empty result means no valid alternatives exist (e.g., hello~bworld
    // where both sides are word chars). The caller (json.rs) reports this as
    // an InvalidPattern error.
    Ok(result_branches)
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
/// From W3C XML spec: <https://www.w3.org/TR/xml/#NT-NameStartChar>
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
/// Returns Some((RuneRange, Option<cache_key>)) for recognized escapes, None otherwise.
/// Large Unicode range escapes (`~i`, `~I`, `~c`, `~C`) include a cache key
/// so the FA shell can be cached and reused.
fn check_multi_char_escape(c: char) -> Option<(RuneRange, Option<String>)> {
    match c {
        // ~d = digit [0-9]
        'd' => Some((vec![RunePair { lo: '0', hi: '9' }], None)),
        // ~D = non-digit (everything except 0-9)
        'D' => Some((invert_rune_range(vec![RunePair { lo: '0', hi: '9' }]), None)),
        // ~w = word char [a-zA-Z0-9_]
        'w' => Some((word_char_runes(), None)),
        // ~W = non-word char
        'W' => Some((invert_rune_range(word_char_runes()), None)),
        // ~s = whitespace [ \t\n\r]
        's' => Some((
            vec![
                RunePair { lo: ' ', hi: ' ' },
                RunePair { lo: '\t', hi: '\t' },
                RunePair { lo: '\n', hi: '\n' },
                RunePair { lo: '\r', hi: '\r' },
            ],
            None,
        )),
        // ~S = non-whitespace
        'S' => Some((
            invert_rune_range(vec![
                RunePair { lo: ' ', hi: ' ' },
                RunePair { lo: '\t', hi: '\t' },
                RunePair { lo: '\n', hi: '\n' },
                RunePair { lo: '\r', hi: '\r' },
            ]),
            None,
        )),
        // ~i = XML NameStartChar (initial name char) — large Unicode range, cache it
        'i' => Some((xml_name_start_char(), Some("i".to_string()))),
        // ~I = NOT XML NameStartChar — large Unicode range, cache it
        'I' => Some((
            invert_rune_range(xml_name_start_char()),
            Some("-i".to_string()),
        )),
        // ~c = XML NameChar (name char) — large Unicode range, cache it
        'c' => Some((xml_name_char(), Some("c".to_string()))),
        // ~C = NOT XML NameChar — large Unicode range, cache it
        'C' => Some((invert_rune_range(xml_name_char()), Some("-c".to_string()))),
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

            // Check for multi-char escapes (~d, ~w, ~s, ~D, ~W, ~S, ~i, ~I, ~c, ~C)
            if let Some((runes, cache_key)) = check_multi_char_escape(next) {
                return Ok(QuantifiedAtom {
                    runes,
                    quant_min: 1,
                    quant_max: 1,
                    cache_key,
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

            // Word boundary: ~b (boundary) and ~B (non-boundary)
            if next == 'b' || next == 'B' {
                parse.record_feature(RegexpFeature::WordBoundary);
                let is_boundary = next == 'b';
                return Ok(QuantifiedAtom {
                    is_word_boundary: Some(is_boundary),
                    quant_min: 1,
                    quant_max: 1,
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
/// Returns `(RuneRange, Option<Vec<u8>>)` where the second element contains
/// ASCII negated bytes for patterns like [^x] that can use memchr acceleration.
fn read_char_class_expr(
    parse: &mut RegexpParse,
) -> Result<(RuneRange, Option<Vec<u8>>), RegexpError> {
    read_char_class_expr_depth(parse, 0)
}

/// Maximum nesting depth for character class subtraction (e.g., [a-[b-[c-[...]]]]).
const MAX_CLASS_SUBTRACTION_DEPTH: usize = 8;

fn read_char_class_expr_depth(
    parse: &mut RegexpParse,
    depth: usize,
) -> Result<(RuneRange, Option<Vec<u8>>), RegexpError> {
    if depth > MAX_CLASS_SUBTRACTION_DEPTH {
        return Err(RegexpError {
            message: "character class subtraction nested too deeply".into(),
            offset: parse.index,
        });
    }

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

    // Check for character class subtraction -[...] or trailing -
    if parse.bypass_optional('-') == Ok(true) {
        // Peek ahead to see if this is subtraction syntax -[
        let next = parse.next_rune().map_err(|_| RegexpError {
            message: "unclosed character class".into(),
            offset: parse.index,
        })?;
        if next == '[' {
            // Character class subtraction: base-[subtract]
            // Recursively parse the subtracted class (which may itself contain subtraction)
            let (subtract_rr, _) = read_char_class_expr_depth(parse, depth + 1)?;
            rr = subtract_rune_range(rr, subtract_rr);
        } else {
            // Not subtraction — it's a trailing literal '-'
            parse.backup1(next);
            rr.push(RunePair { lo: '-', hi: '-' });
        }
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
        // Inside character classes, cache_key is not used since ranges are combined.
        if let Some((runes, _cache_key)) = check_multi_char_escape(next) {
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

    // Character class subtraction: d-[ should NOT be a range attempt.
    // Back up both '[' and '-' so the caller can handle subtraction syntax.
    if range_end == '[' {
        parse.backup1(range_end);
        parse.backup1('-');
        return Ok(vec![RunePair { lo, hi: lo }]);
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

/// Subtract one rune range from another: `base - subtract`.
/// Returns characters that are in `base` but NOT in `subtract`.
/// Both inputs are simplified before processing.
pub(crate) fn subtract_rune_range(base: RuneRange, subtract: RuneRange) -> RuneRange {
    let base = simplify_rune_range(base);
    let subtract = simplify_rune_range(subtract);

    let mut result = Vec::new();
    let mut sub_idx = 0;

    for pair in &base {
        let mut lo = pair.lo as u32;
        let hi = pair.hi as u32;

        // Walk through subtract ranges that might overlap this base range
        while sub_idx < subtract.len() && (subtract[sub_idx].hi as u32) < lo {
            sub_idx += 1;
        }

        let mut si = sub_idx;
        while si < subtract.len() && (subtract[si].lo as u32) <= hi {
            let sub_lo = subtract[si].lo as u32;
            let sub_hi = subtract[si].hi as u32;

            // Add the gap before this subtract range (if any)
            if lo < sub_lo {
                if let (Some(r_lo), Some(r_hi)) = (char::from_u32(lo), char::from_u32(sub_lo - 1)) {
                    result.push(RunePair { lo: r_lo, hi: r_hi });
                }
            }

            // Advance past the subtracted portion
            lo = sub_hi + 1;
            si += 1;
        }

        // Add remaining portion of base range after all subtract ranges
        if lo <= hi {
            if let (Some(r_lo), Some(r_hi)) = (char::from_u32(lo), char::from_u32(hi)) {
                result.push(RunePair { lo: r_lo, hi: r_hi });
            }
        }
    }

    result
}

/// Simplify and merge overlapping rune ranges
pub(crate) fn simplify_rune_range(mut rranges: RuneRange) -> RuneRange {
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
pub(crate) fn invert_rune_range(mut rr: RuneRange) -> RuneRange {
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
    let cache_key = detail.map_or_else(|| initial.to_string(), |d| format!("{}{}", initial, d));

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

    // ========================================================================
    // Word boundary (~b/~B) expansion
    // ========================================================================

    #[test]
    fn test_wb_parse_boundary_marker() {
        let tree = parse_regexp("~bhello").unwrap();
        assert!(has_word_boundary(&tree));
        let expanded = expand_word_boundaries(&tree).unwrap();
        // ~b at start: first char ('h' = word char) must be word → trivially satisfied
        assert_eq!(expanded.len(), 1);
        assert_eq!(expanded[0].len(), 5);
    }

    #[test]
    fn test_wb_impossible_boundary_empty_tree() {
        // ab~bcd: 'b' and 'c' are both word chars → ~b impossible → empty tree
        let tree = parse_regexp("ab~bcd").unwrap();
        let expanded = expand_word_boundaries(&tree).unwrap();
        assert!(
            expanded.is_empty(),
            "impossible boundary should produce empty tree"
        );
    }

    #[test]
    fn test_wb_middle_boundary_expansion() {
        // ab~b cd: 'b' is word, ' ' is non-word → boundary valid
        let tree = parse_regexp("ab~b cd").unwrap();
        let expanded = expand_word_boundaries(&tree).unwrap();
        assert_eq!(expanded.len(), 1); // Only word→nonword branch survives
    }

    #[test]
    fn test_wb_dot_star_expansion() {
        // .*~bcat: dot-star before boundary, 'c' (word) after
        let tree = parse_regexp(".*~bcat").unwrap();
        let expanded = expand_word_boundaries(&tree).unwrap();
        // Two branches:
        // 1. .*{0,MAX-1} ~W c a t (last of .* is non-word, c is word)
        // 2. c a t (when .* matches 0 chars, boundary at value start, c must be word)
        assert_eq!(expanded.len(), 2);
    }
}
