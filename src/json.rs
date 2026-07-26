//! Minimal JSON parser for flattening events and patterns

use crate::QuaminaError;
use crate::regexp::{
    LookaroundType, RegexpBranch, RegexpRoot, collect_lookarounds, expand_word_boundaries,
    has_top_level_lookaround, has_word_boundary, parse_regexp,
};
use crate::segments_tree::SEGMENT_SEPARATOR;
use rustc_hash::FxHashMap;

/// Represents a field's position within an array in the event.
/// Array is a unique identifier for each array in the event.
/// Pos is the field's index within that array.
/// Uses i32 to match Go's int32.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ArrayPos {
    pub array: i32,
    pub pos: i32,
}

/// A flattened field from a JSON event, including array position tracking.
#[derive(Clone, Debug)]
pub struct Field {
    pub path: String,
    pub value: String,
    pub array_trail: Vec<ArrayPos>,
    /// True if the value is a JSON number (for Q-number conversion during matching)
    pub is_number: bool,
}

/// A matcher for a pattern field value
#[derive(Debug, Clone)]
pub enum Matcher {
    Exact(String),
    NumericExact(f64), // For numeric values: 35, 35.0, 3.5e1 should all match
    Exists(bool),
    Prefix(String),
    Suffix(String),
    Wildcard(String),
    Shellstyle(String), // Simpler wildcard without escape support
    AnythingBut(Vec<String>),
    AnythingButNumeric(Vec<f64>),
    EqualsIgnoreCase(String),
    Numeric(NumericComparison),
    /// Regex pattern parsed into our custom NFA
    ParsedRegexp(RegexpRoot),
    /// CIDR pattern for IP address matching
    Cidr(CidrPattern),
    /// Multi-condition pattern for lookaround support ((?=...), (?!...), (?<=...), (?<!...))
    MultiCondition(MultiConditionPattern),
}

/// Numeric comparison operators
#[derive(Debug, Clone, PartialEq)]
pub struct NumericComparison {
    pub lower: Option<(bool, f64)>, // (inclusive, value)
    pub upper: Option<(bool, f64)>, // (inclusive, value)
}

/// Parsed CIDR notation for IP matching
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CidrPattern {
    V4 { network: [u8; 4], prefix_len: u8 },
    V6 { network: [u8; 16], prefix_len: u8 },
}

impl CidrPattern {
    /// Parse a CIDR notation string (e.g., "10.0.0.0/24" or "2001:db8::/32")
    #[must_use]
    pub fn parse(s: &str) -> Option<Self> {
        let (addr_str, prefix_str) = s.split_once('/')?;
        let prefix_len: u8 = prefix_str.parse().ok()?;

        // Try IPv4 first
        if let Some(addr) = Self::parse_ipv4(addr_str) {
            if prefix_len > 32 {
                return None;
            }
            // Apply mask to get network address
            let mask = if prefix_len == 0 {
                0u32
            } else {
                !0u32 << (32 - prefix_len)
            };
            let network_bits = u32::from_be_bytes(addr) & mask;
            return Some(Self::V4 {
                network: network_bits.to_be_bytes(),
                prefix_len,
            });
        }

        // Try IPv6
        if let Some(addr) = Self::parse_ipv6(addr_str) {
            if prefix_len > 128 {
                return None;
            }
            // Apply mask to get network address
            let network = Self::apply_ipv6_mask(&addr, prefix_len);
            return Some(Self::V6 {
                network,
                prefix_len,
            });
        }

        None
    }

    /// Parse an IPv4 address string
    fn parse_ipv4(s: &str) -> Option<[u8; 4]> {
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 4 {
            return None;
        }
        let mut addr = [0u8; 4];
        for (i, part) in parts.iter().enumerate() {
            addr[i] = part.parse().ok()?;
        }
        Some(addr)
    }

    /// Parse an IPv6 address string (supports :: shorthand)
    fn parse_ipv6(s: &str) -> Option<[u8; 16]> {
        let mut addr = [0u8; 16];

        // Handle :: shorthand
        if s.contains("::") {
            let parts: Vec<&str> = s.split("::").collect();
            if parts.len() > 2 {
                return None; // Invalid: more than one ::
            }

            let left: Vec<&str> = if parts[0].is_empty() {
                vec![]
            } else {
                parts[0].split(':').collect()
            };
            let right: Vec<&str> = if parts[1].is_empty() {
                vec![]
            } else {
                parts[1].split(':').collect()
            };

            if left.len() + right.len() > 8 {
                return None;
            }

            // Fill left part
            for (i, part) in left.iter().enumerate() {
                let val = u16::from_str_radix(part, 16).ok()?;
                let [hi, lo] = val.to_be_bytes();
                addr[i * 2] = hi;
                addr[i * 2 + 1] = lo;
            }

            // Fill right part (from the end)
            let right_start = 8 - right.len();
            for (i, part) in right.iter().enumerate() {
                let val = u16::from_str_radix(part, 16).ok()?;
                let [hi, lo] = val.to_be_bytes();
                addr[(right_start + i) * 2] = hi;
                addr[(right_start + i) * 2 + 1] = lo;
            }
        } else {
            // Full address
            let parts: Vec<&str> = s.split(':').collect();
            if parts.len() != 8 {
                return None;
            }
            for (i, part) in parts.iter().enumerate() {
                let val = u16::from_str_radix(part, 16).ok()?;
                let [hi, lo] = val.to_be_bytes();
                addr[i * 2] = hi;
                addr[i * 2 + 1] = lo;
            }
        }

        Some(addr)
    }

    /// Apply a prefix mask to an IPv6 address
    fn apply_ipv6_mask(addr: &[u8; 16], prefix_len: u8) -> [u8; 16] {
        let mut result = *addr;
        let full_bytes = (prefix_len / 8) as usize;
        let remaining_bits = prefix_len % 8;

        // Zero out bytes after the prefix
        for byte in result
            .iter_mut()
            .skip(full_bytes + usize::from(remaining_bits > 0))
        {
            *byte = 0;
        }

        // Mask the partial boundary byte. A prefix with leftover bits is not
        // byte-aligned, so it is at most 127 bits and `full_bytes` is at most
        // 15 — always a valid index into the 16-byte address.
        if remaining_bits > 0 {
            let mask = !0u8 << (8 - remaining_bits);
            result[full_bytes] &= mask;
        }

        result
    }
}

// ============================================================================
// Multi-Condition Matching (Lookaround Support)
// ============================================================================

/// A condition in a multi-condition matcher for lookaround patterns.
///
/// Conditions are evaluated after the primary pattern matches. They are
/// ordered by estimated cost (cheapest first) for fast-fail optimization.
/// Inspired by regex crate's prefilter strategy: memchr > byteset > memmem.
#[derive(Debug, Clone)]
pub enum LookaroundCondition {
    /// `(?=...)` - positive lookahead: combined pattern (primary + lookahead) must match
    /// Stored as the full combined pattern for automaton construction.
    PositiveLookahead(RegexpRoot),

    /// `(?!...)` - negative lookahead: combined pattern must NOT match
    /// If primary matches but primary+suffix also matches, reject.
    NegativeLookahead(RegexpRoot),

    /// `(?<=...)` - positive lookbehind: pattern before match position must match
    /// `byte_length` is the fixed UTF-8 byte length of the lookbehind pattern.
    PositiveLookbehind {
        pattern: RegexpRoot,
        byte_length: usize,
    },

    /// `(?<!...)` - negative lookbehind: pattern before match position must NOT match
    NegativeLookbehind {
        pattern: RegexpRoot,
        byte_length: usize,
    },
}

impl LookaroundCondition {
    /// Returns true if this is a negative condition ((?!...) or (?<!...)).
    /// Negative conditions typically have higher false positive rates during
    /// candidate filtering, so they should be checked after positive conditions.
    #[must_use]
    pub const fn is_negative(&self) -> bool {
        matches!(
            self,
            Self::NegativeLookahead(_) | Self::NegativeLookbehind { .. }
        )
    }

    /// Returns true if this is a lookbehind condition.
    #[must_use]
    pub const fn is_lookbehind(&self) -> bool {
        matches!(
            self,
            Self::PositiveLookbehind { .. } | Self::NegativeLookbehind { .. }
        )
    }

    /// Estimated cost for condition ordering (lower = check first).
    /// Based on regex crate insights: prefilter speed > selectivity > complexity.
    ///
    /// Cost model:
    /// - Positive lookahead: 10 (shares prefix with primary, likely fast)
    /// - Negative lookahead: 20 (higher false positive rate)
    /// - Positive lookbehind: 30 (requires position tracking)
    /// - Negative lookbehind: 40 (position tracking + higher FP rate)
    #[must_use]
    pub const fn cost_estimate(&self) -> u32 {
        match self {
            Self::PositiveLookahead(_) => 10,
            Self::NegativeLookahead(_) => 20,
            Self::PositiveLookbehind { .. } => 30,
            Self::NegativeLookbehind { .. } => 40,
        }
    }
}

/// Multi-condition pattern for lookaround support.
///
/// Combines a primary pattern with additional conditions (lookarounds) that
/// must all be satisfied for a match. Conditions are stored in cost order
/// (cheapest first) for fast-fail optimization.
///
/// # Example patterns
/// - `foo(?=bar)` → primary="foo", conditions=[PositiveLookahead("foobar")]
/// - `foo(?!bar)` → primary="foo", conditions=[NegativeLookahead("foobar")]
/// - `(?<=foo)bar` → primary="bar", conditions=[PositiveLookbehind("foo", 3)]
/// - `(?=.*X)(?=.*Y)Z` → primary="Z", conditions=[PositiveLookahead(.*X), PositiveLookahead(.*Y)]
#[derive(Debug, Clone)]
pub struct MultiConditionPattern {
    /// Primary pattern (what we're actually matching).
    /// This is checked first; if it doesn't match, conditions are skipped.
    pub primary: RegexpRoot,

    /// Additional conditions (lookarounds) to verify after primary matches.
    /// Stored in cost order (cheapest first) for fast-fail optimization.
    /// All conditions must be satisfied for the overall match to succeed.
    pub conditions: Vec<LookaroundCondition>,

    /// True when a lookahead closes the pattern, with no primary atom after it.
    ///
    /// The asserted text counts toward the value, so in `foo(?=bar)` the primary
    /// `foo` covers only the front of `"foobar"`. Verifying the primary against
    /// such a value has to tolerate the stretch the lookahead accounts for.
    pub trailing_lookahead: bool,
}

impl MultiConditionPattern {
    /// Create a new multi-condition pattern with conditions sorted by cost.
    #[must_use]
    pub fn new(primary: RegexpRoot, mut conditions: Vec<LookaroundCondition>) -> Self {
        // Sort conditions by cost estimate (cheapest first) for fast-fail
        conditions.sort_by_key(LookaroundCondition::cost_estimate);
        Self {
            primary,
            conditions,
            trailing_lookahead: false,
        }
    }

    /// Mark the pattern as ending in a lookahead assertion.
    #[must_use]
    pub const fn with_trailing_lookahead(mut self) -> Self {
        self.trailing_lookahead = true;
        self
    }
}

// ============================================================================
// Lookaround Pattern Transformation
// ============================================================================

/// Transform a regexp tree with lookarounds into a MultiConditionPattern.
///
/// This function extracts lookaround atoms from the tree and constructs:
/// - A primary pattern (the non-lookaround parts)
/// - A list of conditions derived from each lookaround
///
/// # Transformation Rules
/// - `A(?=B)` → primary=A, conditions=[PositiveLookahead(AB)]
/// - `A(?!B)` → primary=A, conditions=[NegativeLookahead(AB)]
/// - `(?<=B)A` → primary=A, conditions=[PositiveLookbehind(B, byte_length)]
/// - `(?<!B)A` → primary=A, conditions=[NegativeLookbehind(B, byte_length)]
///
/// Returns `Ok(MultiConditionPattern)` if transformation succeeds,
/// or `Err(message)` if the pattern structure is not supported.
pub fn transform_lookaround_pattern(tree: &RegexpRoot) -> Result<MultiConditionPattern, String> {
    // Collect all lookarounds with their positions
    let lookarounds = collect_lookarounds(tree);

    if lookarounds.is_empty() {
        return Err("no lookarounds found in pattern".into());
    }

    // For now, only support patterns with a single branch
    // (no alternation at the top level with mixed lookarounds)
    if tree.len() != 1 {
        return Err("lookaround patterns with top-level alternation not yet supported".into());
    }

    let branch = &tree[0];
    let mut conditions = Vec::new();
    let mut primary_atoms: RegexpBranch = Vec::new();

    for atom in branch {
        if let Some(la_type) = atom.lookaround {
            let la_subtree = atom
                .subtree
                .as_ref()
                .ok_or("lookaround atom missing subtree")?;

            match la_type {
                LookaroundType::PositiveLookahead => {
                    // A(?=B) → condition checks that AB matches
                    // Build combined pattern: primary atoms so far + lookahead content
                    let combined = build_combined_pattern(&primary_atoms, la_subtree);
                    conditions.push(LookaroundCondition::PositiveLookahead(combined));
                }
                LookaroundType::NegativeLookahead => {
                    // A(?!B) → condition checks that AB does NOT match
                    let combined = build_combined_pattern(&primary_atoms, la_subtree);
                    conditions.push(LookaroundCondition::NegativeLookahead(combined));
                }
                LookaroundType::PositiveLookbehind => {
                    // (?<=B)A → condition checks B before A
                    // For lookbehind at position 0, it means B must precede A
                    // Byte length is computed from the lookbehind pattern
                    let byte_length = compute_lookbehind_byte_length(la_subtree)?;
                    conditions.push(LookaroundCondition::PositiveLookbehind {
                        pattern: la_subtree.clone(),
                        byte_length,
                    });
                }
                LookaroundType::NegativeLookbehind => {
                    // (?<!B)A → condition checks B does NOT precede A
                    let byte_length = compute_lookbehind_byte_length(la_subtree)?;
                    conditions.push(LookaroundCondition::NegativeLookbehind {
                        pattern: la_subtree.clone(),
                        byte_length,
                    });
                }
            }
        } else {
            // Non-lookaround atom - add to primary pattern
            primary_atoms.push(atom.clone());
        }
    }

    // If no primary atoms, the pattern is just lookarounds (e.g., (?=foo))
    // In this case, the primary matches empty string
    let primary = if primary_atoms.is_empty() {
        vec![] // Empty pattern matches empty string
    } else {
        vec![primary_atoms]
    };

    // A lookahead in final position asserts text that runs past everything the
    // primary spells out, so the primary can only be held to a prefix of the value.
    let ends_with_lookahead = branch.last().is_some_and(|atom| {
        matches!(
            atom.lookaround,
            Some(LookaroundType::PositiveLookahead | LookaroundType::NegativeLookahead)
        )
    });

    let mc = MultiConditionPattern::new(primary, conditions);
    Ok(if ends_with_lookahead {
        mc.with_trailing_lookahead()
    } else {
        mc
    })
}

/// Build a combined pattern from primary atoms and a lookahead subtree.
/// For A(?=B), this builds AB as a single pattern.
fn build_combined_pattern(primary_atoms: &RegexpBranch, lookahead: &RegexpRoot) -> RegexpRoot {
    if lookahead.is_empty() {
        // Lookahead is empty - just return primary
        return vec![primary_atoms.clone()];
    }

    // Combine each lookahead branch with the primary atoms
    let mut combined_branches = Vec::new();
    for la_branch in lookahead {
        let mut combined: RegexpBranch = primary_atoms.clone();
        combined.extend(la_branch.clone());
        combined_branches.push(combined);
    }

    combined_branches
}

/// Compute the fixed byte length of a lookbehind pattern.
/// Lookbehind patterns must have a fixed length (validated during parsing).
/// This computes the UTF-8 byte length for the pattern.
fn compute_lookbehind_byte_length(tree: &RegexpRoot) -> Result<usize, String> {
    let Some((first, rest)) = tree.split_first() else {
        return Ok(0);
    };

    // Every alternation branch must encode to the same byte length; reject any
    // that disagrees so callers can rely on a single fixed lookbehind width.
    let first_len = compute_branch_byte_length(first)?;
    for branch in rest {
        if compute_branch_byte_length(branch)? != first_len {
            return Err("variable-length lookbehind not supported".into());
        }
    }
    Ok(first_len)
}

/// Compute the byte length of a single branch.
/// Each atom contributes based on its character class and quantifier.
fn compute_branch_byte_length(branch: &RegexpBranch) -> Result<usize, String> {
    let mut total = 0usize;
    for atom in branch {
        // Must be singleton (no variable quantifiers)
        if atom.quant_min != atom.quant_max {
            return Err("variable quantifier in lookbehind not supported".into());
        }
        // Quantifier counts come out of the parser non-negative; if a stray
        // negative ever reaches here we'd rather raise a clear error than
        // wrap into an enormous usize.
        let count = usize::try_from(atom.quant_min)
            .map_err(|_| "negative quantifier count in lookbehind".to_owned())?;

        // Compute per-atom byte length
        let atom_len = if atom.is_dot {
            // Dot can match any char, but for byte length we need worst case
            // For simplicity, assume UTF-8 max (4 bytes) - this is conservative
            // A more precise implementation would track actual character ranges
            4
        } else if !atom.runes.is_empty() {
            // Character class - compute max UTF-8 length of any char in range
            let mut max_len = 1;
            for rp in &atom.runes {
                max_len = max_len.max(rp.hi.len_utf8());
            }
            max_len
        } else if let Some(subtree) = &atom.subtree {
            // Nested group - recurse
            compute_lookbehind_byte_length(subtree)?
        } else {
            0
        };

        total += atom_len * count;
    }
    Ok(total)
}

/// Parse a pattern JSON into field -> matchers map
/// e.g., {"status": ["active"]} -> {"status": [Exact("active")]}
/// e.g., {"name": [{"exists": true}]} -> {"name": [Exists(true)]}
pub fn parse_pattern(
    json: &str,
    limits: &crate::PatternLimits,
) -> Result<FxHashMap<String, Vec<Matcher>>, QuaminaError> {
    let mut parser = Parser::new(json);
    let value = parser.parse_value()?;

    let Value::Object(obj) = value else {
        return Err(QuaminaError::InvalidPattern(
            "pattern must be an object".into(),
        ));
    };

    let mut fields = FxHashMap::default();
    extract_pattern_fields(&obj, String::new(), &mut fields, 0, limits)?;
    Ok(fields)
}

fn extract_pattern_fields(
    obj: &[(String, Value)],
    prefix: String,
    fields: &mut FxHashMap<String, Vec<Matcher>>,
    depth: usize,
    limits: &crate::PatternLimits,
) -> Result<(), QuaminaError> {
    if depth >= limits.max_pattern_depth {
        return Err(QuaminaError::PatternTooComplex(format!(
            "pattern nesting depth {} exceeds maximum of {} (at path '{}')",
            depth + 1,
            limits.max_pattern_depth,
            prefix
        )));
    }
    for (key, value) in obj {
        let path = if prefix.is_empty() {
            key.clone()
        } else {
            format!("{prefix}{SEGMENT_SEPARATOR}{key}")
        };
        match value {
            Value::Array(arr) => {
                let matchers: Result<Vec<Matcher>, QuaminaError> =
                    arr.iter().map(value_to_matcher).collect();
                fields.insert(path, matchers?);
                if fields.len() > limits.max_fields_per_pattern {
                    return Err(QuaminaError::PatternTooComplex(format!(
                        "pattern has {} fields, exceeding maximum of {}",
                        fields.len(),
                        limits.max_fields_per_pattern
                    )));
                }
            }
            Value::Object(nested) => {
                extract_pattern_fields(nested, path, fields, depth + 1, limits)?;
            }
            _ => {
                return Err(QuaminaError::InvalidPattern(format!(
                    "pattern field '{path}' must be array or object"
                )));
            }
        }
    }
    Ok(())
}

fn value_to_matcher(value: &Value) -> Result<Matcher, QuaminaError> {
    match value {
        Value::Object(obj) => {
            let Some((key, val)) = obj.first() else {
                return Err(QuaminaError::InvalidPattern(
                    "matcher object cannot be empty".into(),
                ));
            };
            operator_to_matcher(key, val)
        }
        Value::Number(n) => {
            // Numeric values are stored as float so 35, 35.0, and 3.5e1 all compare equal.
            n.parse::<f64>().map_or_else(
                |_| Ok(Matcher::Exact(value_to_string(value))),
                |f| Ok(Matcher::NumericExact(f)),
            )
        }
        _ => Ok(Matcher::Exact(value_to_string(value))),
    }
}

/// Convert one `{operator: value}` pair into a `Matcher`.
fn operator_to_matcher(key: &str, val: &Value) -> Result<Matcher, QuaminaError> {
    match key {
        "exists" => {
            if let Value::Bool(b) = val {
                return Ok(Matcher::Exists(*b));
            }
            Err(QuaminaError::InvalidPattern(
                "exists value must be a boolean".into(),
            ))
        }
        "prefix" => {
            if let Value::String(s) = val {
                return Ok(Matcher::Prefix(s.clone()));
            }
            Err(QuaminaError::InvalidPattern(
                "prefix value must be a string".into(),
            ))
        }
        "suffix" => {
            if let Value::String(s) = val {
                return Ok(Matcher::Suffix(s.clone()));
            }
            Err(QuaminaError::InvalidPattern(
                "suffix value must be a string".into(),
            ))
        }
        "wildcard" => {
            if let Value::String(s) = val {
                // validate_wildcard rejects adjacent `**`, invalid escapes, and trailing `\`.
                if !validate_wildcard(s) {
                    return Err(QuaminaError::InvalidPattern(
                        "wildcard pattern has invalid escape sequence or adjacent '**'".into(),
                    ));
                }
                return Ok(Matcher::Wildcard(s.clone()));
            }
            Err(QuaminaError::InvalidPattern(
                "wildcard value must be a string".into(),
            ))
        }
        "shellstyle" => {
            if let Value::String(s) = val {
                if s.contains("**") {
                    return Err(QuaminaError::InvalidPattern(
                        "shellstyle pattern cannot contain '**'".into(),
                    ));
                }
                return Ok(Matcher::Shellstyle(s.clone()));
            }
            Err(QuaminaError::InvalidPattern(
                "shellstyle value must be a string".into(),
            ))
        }
        "anything-but" => parse_anything_but(val),
        "equals-ignore-case" => {
            if let Value::String(s) = val {
                return Ok(Matcher::EqualsIgnoreCase(s.clone()));
            }
            Err(QuaminaError::InvalidPattern(
                "equals-ignore-case value must be a string".into(),
            ))
        }
        "numeric" => {
            if let Value::Array(arr) = val {
                return parse_numeric_comparison(arr)
                    .map(Matcher::Numeric)
                    .ok_or_else(|| {
                        QuaminaError::InvalidPattern("invalid numeric comparison format".into())
                    });
            }
            Err(QuaminaError::InvalidPattern(
                "numeric value must be an array".into(),
            ))
        }
        "regexp" | "regex" => {
            if let Value::String(s) = val {
                return parse_regexp_matcher(s);
            }
            Err(QuaminaError::InvalidPattern(
                "regex value must be a string".into(),
            ))
        }
        "cidr" => {
            if let Value::String(s) = val {
                return CidrPattern::parse(s).map(Matcher::Cidr).ok_or_else(|| {
                    QuaminaError::InvalidPattern(format!("invalid CIDR notation: {s}"))
                });
            }
            Err(QuaminaError::InvalidPattern(
                "cidr value must be a string".into(),
            ))
        }
        _ => Err(QuaminaError::InvalidPattern(format!(
            "unknown operator '{key}'"
        ))),
    }
}

/// `{"anything-but": "x"}`, `{"anything-but": 1}`, or `{"anything-but": [...]}`.
fn parse_anything_but(val: &Value) -> Result<Matcher, QuaminaError> {
    match val {
        Value::String(s) => Ok(Matcher::AnythingBut(vec![s.clone()])),
        Value::Number(n) => n
            .parse::<f64>()
            .map(|f| Matcher::AnythingButNumeric(vec![f]))
            .map_err(|_| {
                QuaminaError::InvalidPattern(
                    "anything-but numeric value is not a valid number".into(),
                )
            }),
        Value::Array(arr) => parse_anything_but_array(arr),
        _ => Err(QuaminaError::InvalidPattern(
            "anything-but value must be a string, number, or array".into(),
        )),
    }
}

fn parse_anything_but_array(arr: &[Value]) -> Result<Matcher, QuaminaError> {
    if arr.is_empty() {
        return Err(QuaminaError::InvalidPattern(
            "anything-but array cannot be empty".into(),
        ));
    }
    let strings: Vec<String> = arr
        .iter()
        .filter_map(|v| match v {
            Value::String(s) => Some(s.clone()),
            _ => None,
        })
        .collect();
    let numbers: Vec<f64> = arr
        .iter()
        .filter_map(|v| match v {
            Value::Number(n) => n.parse::<f64>().ok(),
            _ => None,
        })
        .collect();
    // Arrays must be homogeneous (all strings or all numbers).
    if !strings.is_empty() && !numbers.is_empty() {
        return Err(QuaminaError::InvalidPattern(
            "anything-but array must contain only strings or only numbers".into(),
        ));
    }
    if !strings.is_empty() {
        return Ok(Matcher::AnythingBut(strings));
    }
    if !numbers.is_empty() {
        return Ok(Matcher::AnythingButNumeric(numbers));
    }
    Err(QuaminaError::InvalidPattern(
        "anything-but array must contain strings or numbers".into(),
    ))
}

/// Parse a regexp string and pick the right `Matcher` variant (raw tree or multi-condition).
fn parse_regexp_matcher(s: &str) -> Result<Matcher, QuaminaError> {
    let tree = parse_regexp(s)
        .map_err(|e| QuaminaError::InvalidPattern(format!("invalid regexp: {}", e.message)))?;

    let tree = if has_word_boundary(&tree) {
        let expanded = expand_word_boundaries(&tree).map_err(|e| {
            QuaminaError::InvalidPattern(format!("word boundary expansion failed: {e}"))
        })?;
        if expanded.is_empty() {
            // No valid alternatives — pattern can never match (e.g., `hello~bworld`
            // where both sides are word chars).
            return Err(QuaminaError::InvalidPattern(
                "word boundary ~b/~B is impossible in this pattern: adjacent characters are in the same word-class".into(),
            ));
        }
        expanded
    } else {
        tree
    };

    if has_top_level_lookaround(&tree) {
        let mc = transform_lookaround_pattern(&tree).map_err(|e| {
            QuaminaError::InvalidPattern(format!("lookaround transformation failed: {e}"))
        })?;
        return Ok(Matcher::MultiCondition(mc));
    }

    Ok(Matcher::ParsedRegexp(tree))
}

/// Parse numeric comparison like [">", 0, "<=", 100] or [">=", 5]
fn parse_numeric_comparison(arr: &[Value]) -> Option<NumericComparison> {
    let mut lower = None;
    let mut upper = None;

    let mut pairs = arr.chunks_exact(2);
    for pair in &mut pairs {
        let Value::String(op) = &pair[0] else {
            return None;
        };
        let num = match &pair[1] {
            Value::Number(n) => n.parse::<f64>().ok()?,
            _ => return None,
        };

        match op.as_str() {
            ">" => lower = Some((false, num)),
            ">=" => lower = Some((true, num)),
            "<" => upper = Some((false, num)),
            "<=" => upper = Some((true, num)),
            "=" => {
                lower = Some((true, num));
                upper = Some((true, num));
            }
            _ => return None,
        }
    }
    if !pairs.remainder().is_empty() {
        return None;
    }

    Some(NumericComparison { lower, upper })
}

fn value_to_string(value: &Value) -> String {
    match value {
        // String values are wrapped in quotes so the automaton can distinguish
        // them from boolean/null literals and numbers with identical bytes.
        // Event values from the flattener retain JSON quotes for strings.
        Value::String(s) => format!("\"{s}\""),
        Value::Number(n) => n.clone(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Object(_) | Value::Array(_) => String::new(),
    }
}

/// Validate wildcard pattern syntax:
/// - No adjacent ** (unescaped)
/// - Backslash can only be followed by * or \
/// - No trailing backslash
fn validate_wildcard(pattern: &str) -> bool {
    let mut chars = pattern.chars();
    let mut prev_was_star = false;

    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                // Must have next char and it must be * or \
                match chars.next() {
                    Some('*' | '\\') => prev_was_star = false,
                    Some(_) | None => return false, // Invalid escape or trailing backslash
                }
            }
            '*' => {
                if prev_was_star {
                    return false; // Adjacent wildcards
                }
                prev_was_star = true;
            }
            _ => prev_was_star = false,
        }
    }
    true
}

#[derive(Debug, Clone)]
enum Value {
    Object(Vec<(String, Self)>),
    Array(Vec<Self>),
    String(String),
    Number(String),
    Bool(bool),
    Null,
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    const fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_value(&mut self) -> Result<Value, QuaminaError> {
        self.skip_whitespace();
        match self.peek() {
            Some('{') => self.parse_object(),
            Some('[') => self.parse_array(),
            Some('"') => self.parse_string().map(Value::String),
            Some('t' | 'f') => self.parse_bool(),
            Some('n') => self.parse_null(),
            Some(c) if c == '-' || c.is_ascii_digit() => self.parse_number(),
            Some(c) => Err(QuaminaError::InvalidJson(format!("unexpected char: {c}"))),
            None => Err(QuaminaError::InvalidJson("unexpected end".into())),
        }
    }

    fn parse_object(&mut self) -> Result<Value, QuaminaError> {
        self.expect('{')?;
        let mut pairs = Vec::new();
        self.skip_whitespace();
        if self.peek() == Some('}') {
            self.advance();
            return Ok(Value::Object(pairs));
        }
        loop {
            self.skip_whitespace();
            let key = self.parse_string()?;
            self.skip_whitespace();
            self.expect(':')?;
            let value = self.parse_value()?;
            pairs.push((key, value));
            self.skip_whitespace();
            match self.peek() {
                Some(',') => {
                    self.advance();
                }
                Some('}') => {
                    self.advance();
                    break;
                }
                _ => return Err(QuaminaError::InvalidJson("expected , or }".into())),
            }
        }
        Ok(Value::Object(pairs))
    }

    fn parse_array(&mut self) -> Result<Value, QuaminaError> {
        self.expect('[')?;
        let mut items = Vec::new();
        self.skip_whitespace();
        if self.peek() == Some(']') {
            self.advance();
            return Ok(Value::Array(items));
        }
        loop {
            items.push(self.parse_value()?);
            self.skip_whitespace();
            match self.peek() {
                Some(',') => {
                    self.advance();
                }
                Some(']') => {
                    self.advance();
                    break;
                }
                _ => return Err(QuaminaError::InvalidJson("expected , or ]".into())),
            }
        }
        Ok(Value::Array(items))
    }

    fn parse_string(&mut self) -> Result<String, QuaminaError> {
        self.expect('"')?;
        let mut result = String::new();
        while let Some(c) = self.peek() {
            if c == '"' {
                break;
            }
            if c == '\\' {
                self.advance();
                if let Some(escaped) = self.peek() {
                    // Forward-progress invariant: every escape arm must advance
                    // past the escape character, or the outer loop re-peeks `\`
                    // forever.
                    let _prev_pos_in_escape = self.pos;
                    match escaped {
                        'n' => {
                            result.push('\n');
                            self.advance();
                        }
                        'r' => {
                            result.push('\r');
                            self.advance();
                        }
                        't' => {
                            result.push('\t');
                            self.advance();
                        }
                        'b' => {
                            result.push('\x08');
                            self.advance();
                        }
                        'f' => {
                            result.push('\x0c');
                            self.advance();
                        }
                        '\\' => {
                            result.push('\\');
                            self.advance();
                        }
                        '"' => {
                            result.push('"');
                            self.advance();
                        }
                        '/' => {
                            result.push('/');
                            self.advance();
                        }
                        'u' => {
                            self.advance(); // skip 'u'
                            let code_point = self.parse_unicode_escape()?;
                            // Check for UTF-16 surrogate pair
                            if (0xD800..=0xDBFF).contains(&code_point) {
                                // High surrogate - expect low surrogate
                                if self.peek() == Some('\\') {
                                    self.advance();
                                    if self.peek() == Some('u') {
                                        self.advance();
                                        let low = self.parse_unicode_escape()?;
                                        if (0xDC00..=0xDFFF).contains(&low) {
                                            // Decode surrogate pair
                                            let full = 0x10000
                                                + ((code_point - 0xD800) << 10)
                                                + (low - 0xDC00);
                                            if let Some(ch) = char::from_u32(full) {
                                                result.push(ch);
                                            }
                                        }
                                    }
                                }
                            } else if let Some(ch) = char::from_u32(code_point) {
                                result.push(ch);
                            }
                        }
                        other => {
                            // Per JSON spec only `" \ / b f n r t u` are valid
                            // escapes; matching Go's readTextWithEscapes, any
                            // other character is rejected.
                            return Err(QuaminaError::InvalidPattern(format!(
                                "invalid escape \\{other} in pattern string"
                            )));
                        }
                    }
                    debug_assert!(
                        self.pos > _prev_pos_in_escape,
                        "parse_string escape arm must advance past the escape char"
                    );
                }
            } else {
                result.push(c);
                self.advance();
            }
        }
        self.expect('"')?;
        Ok(result)
    }

    fn parse_unicode_escape(&mut self) -> Result<u32, QuaminaError> {
        let mut value = 0u32;
        for _ in 0..4 {
            let digit = self
                .peek()
                .and_then(|c| c.to_digit(16))
                .ok_or_else(|| QuaminaError::InvalidJson("invalid unicode escape".into()))?;
            value = value * 16 + digit;
            self.advance();
        }
        Ok(value)
    }

    fn parse_number(&mut self) -> Result<Value, QuaminaError> {
        let start = self.pos;
        if self.peek() == Some('-') {
            self.advance();
        }
        while self.peek().is_some_and(|c| c.is_ascii_digit()) {
            self.advance();
        }
        if self.peek() == Some('.') {
            self.advance();
            while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                self.advance();
            }
        }
        // Handle scientific notation (e.g., 3.5e2, 1E-10)
        if self.peek() == Some('e') || self.peek() == Some('E') {
            self.advance();
            if self.peek() == Some('+') || self.peek() == Some('-') {
                self.advance();
            }
            while self.peek().is_some_and(|c| c.is_ascii_digit()) {
                self.advance();
            }
        }
        Ok(Value::Number(self.input[start..self.pos].to_string()))
    }

    fn parse_bool(&mut self) -> Result<Value, QuaminaError> {
        if self.input[self.pos..].starts_with("true") {
            self.pos += 4;
            Ok(Value::Bool(true))
        } else if self.input[self.pos..].starts_with("false") {
            self.pos += 5;
            Ok(Value::Bool(false))
        } else {
            Err(QuaminaError::InvalidJson("expected bool".into()))
        }
    }

    fn parse_null(&mut self) -> Result<Value, QuaminaError> {
        if self.input[self.pos..].starts_with("null") {
            self.pos += 4;
            Ok(Value::Null)
        } else {
            Err(QuaminaError::InvalidJson("expected null".into()))
        }
    }

    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }
    fn advance(&mut self) {
        if let Some(c) = self.peek() {
            self.pos += c.len_utf8();
        }
    }
    fn skip_whitespace(&mut self) {
        while self.peek().is_some_and(char::is_whitespace) {
            self.advance();
        }
    }
    fn expect(&mut self, c: char) -> Result<(), QuaminaError> {
        if self.peek() == Some(c) {
            self.advance();
            Ok(())
        } else {
            Err(QuaminaError::InvalidJson(format!("expected '{c}'")))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_string_rejects_unknown_escape() {
        let mut parser = Parser::new(r#""\z""#);
        let err = parser.parse_string().unwrap_err();
        assert!(
            matches!(err, QuaminaError::InvalidPattern(ref msg) if msg.contains("\\z")),
            "expected InvalidPattern containing `\\z`, got {err:?}",
        );
    }
}
