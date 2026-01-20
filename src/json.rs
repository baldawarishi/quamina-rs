//! Minimal JSON parser for flattening events and patterns

use crate::regexp::{
    collect_lookarounds, has_top_level_lookaround, parse_regexp, LookaroundType, RegexpBranch,
    RegexpRoot,
};
use crate::segments_tree::SEGMENT_SEPARATOR;
use crate::QuaminaError;
use std::collections::HashMap;

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
#[derive(Debug, Clone, PartialEq)]
pub enum CidrPattern {
    V4 { network: [u8; 4], prefix_len: u8 },
    V6 { network: [u8; 16], prefix_len: u8 },
}

impl CidrPattern {
    /// Parse a CIDR notation string (e.g., "10.0.0.0/24" or "2001:db8::/32")
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
            return Some(CidrPattern::V4 {
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
            return Some(CidrPattern::V6 {
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
            let right: Vec<&str> = if parts.len() > 1 && !parts[1].is_empty() {
                parts[1].split(':').collect()
            } else {
                vec![]
            };

            if left.len() + right.len() > 8 {
                return None;
            }

            // Fill left part
            for (i, part) in left.iter().enumerate() {
                let val = u16::from_str_radix(part, 16).ok()?;
                addr[i * 2] = (val >> 8) as u8;
                addr[i * 2 + 1] = val as u8;
            }

            // Fill right part (from the end)
            let right_start = 8 - right.len();
            for (i, part) in right.iter().enumerate() {
                let val = u16::from_str_radix(part, 16).ok()?;
                addr[(right_start + i) * 2] = (val >> 8) as u8;
                addr[(right_start + i) * 2 + 1] = val as u8;
            }
        } else {
            // Full address
            let parts: Vec<&str> = s.split(':').collect();
            if parts.len() != 8 {
                return None;
            }
            for (i, part) in parts.iter().enumerate() {
                let val = u16::from_str_radix(part, 16).ok()?;
                addr[i * 2] = (val >> 8) as u8;
                addr[i * 2 + 1] = val as u8;
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
            .skip(full_bytes + if remaining_bits > 0 { 1 } else { 0 })
        {
            *byte = 0;
        }

        // Apply partial mask to the boundary byte
        if remaining_bits > 0 && full_bytes < 16 {
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
    pub fn is_negative(&self) -> bool {
        matches!(
            self,
            LookaroundCondition::NegativeLookahead(_)
                | LookaroundCondition::NegativeLookbehind { .. }
        )
    }

    /// Returns true if this is a lookbehind condition.
    pub fn is_lookbehind(&self) -> bool {
        matches!(
            self,
            LookaroundCondition::PositiveLookbehind { .. }
                | LookaroundCondition::NegativeLookbehind { .. }
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
    pub fn cost_estimate(&self) -> u32 {
        match self {
            LookaroundCondition::PositiveLookahead(_) => 10,
            LookaroundCondition::NegativeLookahead(_) => 20,
            LookaroundCondition::PositiveLookbehind { .. } => 30,
            LookaroundCondition::NegativeLookbehind { .. } => 40,
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
}

impl MultiConditionPattern {
    /// Create a new multi-condition pattern with conditions sorted by cost.
    pub fn new(primary: RegexpRoot, mut conditions: Vec<LookaroundCondition>) -> Self {
        // Sort conditions by cost estimate (cheapest first) for fast-fail
        conditions.sort_by_key(|c| c.cost_estimate());
        Self {
            primary,
            conditions,
        }
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

    for (i, atom) in branch.iter().enumerate() {
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

        // For lookaheads at the end, we need to track position
        // This is handled by the combined pattern approach
        let _ = i; // suppress unused warning for now
    }

    // If no primary atoms, the pattern is just lookarounds (e.g., (?=foo))
    // In this case, the primary matches empty string
    let primary = if primary_atoms.is_empty() {
        vec![] // Empty pattern matches empty string
    } else {
        vec![primary_atoms]
    };

    Ok(MultiConditionPattern::new(primary, conditions))
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
    if tree.is_empty() {
        return Ok(0);
    }

    // For single branch, compute length
    if tree.len() == 1 {
        return compute_branch_byte_length(&tree[0]);
    }

    // For alternation, all branches must have same length (already validated)
    let first_len = compute_branch_byte_length(&tree[0])?;
    for branch in tree.iter().skip(1) {
        let len = compute_branch_byte_length(branch)?;
        if len != first_len {
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
        let count = atom.quant_min as usize;

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
pub fn parse_pattern(json: &str) -> Result<HashMap<String, Vec<Matcher>>, QuaminaError> {
    let mut parser = Parser::new(json);
    let value = parser.parse_value()?;

    let Value::Object(obj) = value else {
        return Err(QuaminaError::InvalidPattern(
            "pattern must be an object".into(),
        ));
    };

    let mut fields = HashMap::new();
    extract_pattern_fields(&obj, String::new(), &mut fields)?;
    Ok(fields)
}

fn extract_pattern_fields(
    obj: &[(String, Value)],
    prefix: String,
    fields: &mut HashMap<String, Vec<Matcher>>,
) -> Result<(), QuaminaError> {
    for (key, value) in obj {
        let path = if prefix.is_empty() {
            key.clone()
        } else {
            format!("{}{}{}", prefix, SEGMENT_SEPARATOR, key)
        };
        match value {
            Value::Array(arr) => {
                let matchers: Result<Vec<Matcher>, QuaminaError> =
                    arr.iter().map(value_to_matcher).collect();
                fields.insert(path, matchers?);
            }
            Value::Object(nested) => {
                extract_pattern_fields(nested, path, fields)?;
            }
            _ => {
                return Err(QuaminaError::InvalidPattern(format!(
                    "pattern field '{}' must be array or object",
                    path
                )))
            }
        }
    }
    Ok(())
}

fn value_to_matcher(value: &Value) -> Result<Matcher, QuaminaError> {
    match value {
        Value::Object(obj) => {
            // Check for operators like {"exists": true} or {"prefix": "str"}
            if let Some((key, val)) = obj.first() {
                match key.as_str() {
                    "exists" => {
                        if let Value::Bool(b) = val {
                            return Ok(Matcher::Exists(*b));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "exists value must be a boolean".into(),
                        ));
                    }
                    "prefix" => {
                        if let Value::String(s) = val {
                            return Ok(Matcher::Prefix(s.clone()));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "prefix value must be a string".into(),
                        ));
                    }
                    "suffix" => {
                        if let Value::String(s) = val {
                            return Ok(Matcher::Suffix(s.clone()));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "suffix value must be a string".into(),
                        ));
                    }
                    "wildcard" => {
                        if let Value::String(s) = val {
                            // validate_wildcard checks for:
                            // - adjacent ** (unescaped wildcards)
                            // - invalid escape sequences (only \* and \\ are valid)
                            // - trailing backslash
                            if !validate_wildcard(s) {
                                return Err(QuaminaError::InvalidPattern(
                                    "wildcard pattern has invalid escape sequence or adjacent '**'"
                                        .into(),
                                ));
                            }
                            return Ok(Matcher::Wildcard(s.clone()));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "wildcard value must be a string".into(),
                        ));
                    }
                    "shellstyle" => {
                        if let Value::String(s) = val {
                            // shellstyle doesn't allow adjacent ** characters
                            if s.contains("**") {
                                return Err(QuaminaError::InvalidPattern(
                                    "shellstyle pattern cannot contain '**'".into(),
                                ));
                            }
                            return Ok(Matcher::Shellstyle(s.clone()));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "shellstyle value must be a string".into(),
                        ));
                    }
                    "anything-but" => {
                        // Handle single string: {"anything-but": "foo"}
                        if let Value::String(s) = val {
                            return Ok(Matcher::AnythingBut(vec![s.clone()]));
                        }
                        // Handle single number: {"anything-but": 123}
                        if let Value::Number(n) = val {
                            if let Ok(f) = n.parse::<f64>() {
                                return Ok(Matcher::AnythingButNumeric(vec![f]));
                            }
                            return Err(QuaminaError::InvalidPattern(
                                "anything-but numeric value is not a valid number".into(),
                            ));
                        }
                        // Handle array: {"anything-but": ["a", "b"]} or {"anything-but": [1, 2]}
                        if let Value::Array(arr) = val {
                            if arr.is_empty() {
                                return Err(QuaminaError::InvalidPattern(
                                    "anything-but array cannot be empty".into(),
                                ));
                            }
                            // Check if array contains strings or numbers
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
                            // Arrays must be homogeneous (all strings or all numbers)
                            if !strings.is_empty() && !numbers.is_empty() {
                                return Err(QuaminaError::InvalidPattern(
                                    "anything-but array must contain only strings or only numbers"
                                        .into(),
                                ));
                            }
                            if !strings.is_empty() {
                                return Ok(Matcher::AnythingBut(strings));
                            }
                            if !numbers.is_empty() {
                                return Ok(Matcher::AnythingButNumeric(numbers));
                            }
                            return Err(QuaminaError::InvalidPattern(
                                "anything-but array must contain strings or numbers".into(),
                            ));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "anything-but value must be a string, number, or array".into(),
                        ));
                    }
                    "equals-ignore-case" => {
                        if let Value::String(s) = val {
                            return Ok(Matcher::EqualsIgnoreCase(s.clone()));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "equals-ignore-case value must be a string".into(),
                        ));
                    }
                    "numeric" => {
                        if let Value::Array(arr) = val {
                            if let Some(cmp) = parse_numeric_comparison(arr) {
                                return Ok(Matcher::Numeric(cmp));
                            }
                            return Err(QuaminaError::InvalidPattern(
                                "invalid numeric comparison format".into(),
                            ));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "numeric value must be an array".into(),
                        ));
                    }
                    "regexp" | "regex" => {
                        if let Value::String(s) = val {
                            // Try our custom parser first (automaton-compatible)
                            match parse_regexp(s) {
                                Ok(tree) => {
                                    // Check if pattern has lookarounds - transform to multi-condition
                                    if has_top_level_lookaround(&tree) {
                                        match transform_lookaround_pattern(&tree) {
                                            Ok(mc) => return Ok(Matcher::MultiCondition(mc)),
                                            Err(e) => {
                                                return Err(QuaminaError::InvalidPattern(format!(
                                                    "lookaround transformation failed: {}",
                                                    e
                                                )))
                                            }
                                        }
                                    }
                                    return Ok(Matcher::ParsedRegexp(tree));
                                }
                                Err(e) => {
                                    return Err(QuaminaError::InvalidPattern(format!(
                                        "invalid regexp: {}",
                                        e.message
                                    )));
                                }
                            }
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "regex value must be a string".into(),
                        ));
                    }
                    "cidr" => {
                        if let Value::String(s) = val {
                            if let Some(cidr) = CidrPattern::parse(s) {
                                return Ok(Matcher::Cidr(cidr));
                            }
                            return Err(QuaminaError::InvalidPattern(format!(
                                "invalid CIDR notation: {}",
                                s
                            )));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "cidr value must be a string".into(),
                        ));
                    }
                    _ => {
                        return Err(QuaminaError::InvalidPattern(format!(
                            "unknown operator '{}'",
                            key
                        )));
                    }
                }
            }
            Err(QuaminaError::InvalidPattern(
                "matcher object cannot be empty".into(),
            ))
        }
        Value::Number(n) => {
            // For numeric values, store as float for proper comparison
            // This ensures 35, 35.0, and 3.5e1 all match each other
            if let Ok(f) = n.parse::<f64>() {
                Ok(Matcher::NumericExact(f))
            } else {
                Ok(Matcher::Exact(value_to_string(value)))
            }
        }
        _ => Ok(Matcher::Exact(value_to_string(value))),
    }
}

/// Parse numeric comparison like [">", 0, "<=", 100] or [">=", 5]
fn parse_numeric_comparison(arr: &[Value]) -> Option<NumericComparison> {
    let mut lower = None;
    let mut upper = None;

    let mut i = 0;
    while i < arr.len() {
        if let Value::String(op) = &arr[i] {
            if i + 1 >= arr.len() {
                return None;
            }
            let num = match &arr[i + 1] {
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
            i += 2;
        } else {
            return None;
        }
    }

    Some(NumericComparison { lower, upper })
}

fn value_to_string(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
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
    let mut chars = pattern.chars().peekable();
    let mut prev_was_star = false;

    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                // Must have next char and it must be * or \
                match chars.next() {
                    Some('*') | Some('\\') => prev_was_star = false,
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
    Object(Vec<(String, Value)>),
    Array(Vec<Value>),
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
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_value(&mut self) -> Result<Value, QuaminaError> {
        self.skip_whitespace();
        match self.peek() {
            Some('{') => self.parse_object(),
            Some('[') => self.parse_array(),
            Some('"') => self.parse_string().map(Value::String),
            Some('t') | Some('f') => self.parse_bool(),
            Some('n') => self.parse_null(),
            Some(c) if c == '-' || c.is_ascii_digit() => self.parse_number(),
            Some(c) => Err(QuaminaError::InvalidJson(format!("unexpected char: {}", c))),
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
                        _ => {
                            result.push(escaped);
                            self.advance();
                        }
                    }
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
        while self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
            self.advance();
        }
        if self.peek() == Some('.') {
            self.advance();
            while self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                self.advance();
            }
        }
        // Handle scientific notation (e.g., 3.5e2, 1E-10)
        if self.peek() == Some('e') || self.peek() == Some('E') {
            self.advance();
            if self.peek() == Some('+') || self.peek() == Some('-') {
                self.advance();
            }
            while self.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
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
        while self.peek().map(|c| c.is_whitespace()).unwrap_or(false) {
            self.advance();
        }
    }
    fn expect(&mut self, c: char) -> Result<(), QuaminaError> {
        if self.peek() == Some(c) {
            self.advance();
            Ok(())
        } else {
            Err(QuaminaError::InvalidJson(format!("expected '{}'", c)))
        }
    }
}
