//! quamina-rs: Fast pattern-matching library for filtering JSON events

mod json;

use json::Matcher;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

/// Errors that can occur during pattern matching
#[derive(Debug)]
pub enum QuaminaError {
    InvalidJson(String),
    InvalidPattern(String),
    InvalidUtf8,
}

impl fmt::Display for QuaminaError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            QuaminaError::InvalidJson(msg) => write!(f, "invalid JSON: {}", msg),
            QuaminaError::InvalidPattern(msg) => write!(f, "invalid pattern: {}", msg),
            QuaminaError::InvalidUtf8 => write!(f, "invalid UTF-8"),
        }
    }
}

impl std::error::Error for QuaminaError {}

/// The main pattern matcher
///
/// Quamina is Clone, allowing you to create snapshots for concurrent use:
/// ```
/// # use quamina::Quamina;
/// let mut q = Quamina::new();
/// q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
///
/// // Clone for use in another thread
/// let q_snapshot = q.clone();
/// ```
///
/// For shared concurrent access, wrap in Arc:
/// ```
/// # use quamina::Quamina;
/// use std::sync::Arc;
///
/// let q = Arc::new(Quamina::<String>::new());
/// let q_clone = Arc::clone(&q);
/// // Both can now be used for concurrent matching
/// ```
#[derive(Clone)]
pub struct Quamina<X = String> {
    patterns: HashMap<X, Vec<Pattern>>,
}

/// Internal representation of a compiled pattern
#[derive(Clone)]
struct Pattern {
    fields: HashMap<String, Vec<Matcher>>,
}

impl<X: Clone + Eq + Hash> Quamina<X> {
    /// Create a new Quamina instance
    pub fn new() -> Self {
        Quamina {
            patterns: HashMap::new(),
        }
    }

    /// Add a pattern with the given identifier
    pub fn add_pattern(&mut self, x: X, pattern_json: &str) -> Result<(), QuaminaError> {
        let fields = json::parse_pattern(pattern_json)?;
        let pattern = Pattern { fields };
        self.patterns.entry(x).or_default().push(pattern);
        Ok(())
    }

    /// Find all patterns that match the given event
    pub fn matches_for_event(&self, event: &[u8]) -> Result<Vec<X>, QuaminaError> {
        let event_fields = json::flatten_event(event)?;
        // Build multimap: field -> Vec<values> to support array element matching
        let mut event_map: HashMap<&str, Vec<&str>> = HashMap::new();
        for (k, v) in &event_fields {
            event_map.entry(k.as_str()).or_default().push(v.as_str());
        }

        let mut matches = Vec::new();
        for (id, patterns) in &self.patterns {
            for pattern in patterns {
                if self.pattern_matches(&pattern.fields, &event_map) {
                    matches.push(id.clone());
                    break; // Only add each ID once
                }
            }
        }
        Ok(matches)
    }

    fn pattern_matches(
        &self,
        pattern_fields: &HashMap<String, Vec<Matcher>>,
        event_map: &HashMap<&str, Vec<&str>>,
    ) -> bool {
        // All pattern fields must match (AND across fields)
        for (field, matchers) in pattern_fields {
            let event_values = event_map.get(field.as_str());

            // Any matcher can match (OR within field)
            // For array fields, any event value matching any matcher counts
            let field_matches = matchers.iter().any(|matcher| match matcher {
                Matcher::Exact(expected) => event_values
                    .map(|vals| vals.contains(&expected.as_str()))
                    .unwrap_or(false),
                Matcher::NumericExact(expected) => event_values
                    .map(|vals| {
                        vals.iter()
                            .any(|v| v.parse::<f64>().ok().is_some_and(|num| num == *expected))
                    })
                    .unwrap_or(false),
                Matcher::Exists(should_exist) => {
                    if *should_exist {
                        event_values.map(|v| !v.is_empty()).unwrap_or(false)
                    } else {
                        event_values.map(|v| v.is_empty()).unwrap_or(true)
                    }
                }
                Matcher::Prefix(prefix) => event_values
                    .map(|vals| vals.iter().any(|v| v.starts_with(prefix)))
                    .unwrap_or(false),
                Matcher::Suffix(suffix) => event_values
                    .map(|vals| vals.iter().any(|v| v.ends_with(suffix)))
                    .unwrap_or(false),
                Matcher::Wildcard(pattern) => event_values
                    .map(|vals| vals.iter().any(|v| wildcard_match(pattern, v)))
                    .unwrap_or(false),
                Matcher::Shellstyle(pattern) => event_values
                    .map(|vals| vals.iter().any(|v| shellstyle_match(pattern, v)))
                    .unwrap_or(false),
                Matcher::AnythingBut(excluded) => event_values
                    .map(|vals| vals.iter().any(|v| !excluded.iter().any(|e| e == *v)))
                    .unwrap_or(false),
                Matcher::EqualsIgnoreCase(expected) => event_values
                    .map(|vals| vals.iter().any(|v| v.eq_ignore_ascii_case(expected)))
                    .unwrap_or(false),
                Matcher::Numeric(cmp) => event_values
                    .map(|vals| {
                        vals.iter().any(|v| {
                            v.parse::<f64>().ok().is_some_and(|num| {
                                let lower_ok = match cmp.lower {
                                    Some((true, bound)) => num >= bound,
                                    Some((false, bound)) => num > bound,
                                    None => true,
                                };
                                let upper_ok = match cmp.upper {
                                    Some((true, bound)) => num <= bound,
                                    Some((false, bound)) => num < bound,
                                    None => true,
                                };
                                lower_ok && upper_ok
                            })
                        })
                    })
                    .unwrap_or(false),
                Matcher::Regex(re) => event_values
                    .map(|vals| vals.iter().any(|v| re.is_match(v)))
                    .unwrap_or(false),
            });

            if !field_matches {
                return false;
            }
        }
        true
    }

    /// Delete all patterns with the given identifier
    pub fn delete_patterns(&mut self, x: &X) -> Result<(), QuaminaError> {
        self.patterns.remove(x);
        Ok(())
    }

    /// Check if any pattern matches the event (returns early on first match)
    pub fn has_matches(&self, event: &[u8]) -> Result<bool, QuaminaError> {
        let event_fields = json::flatten_event(event)?;
        let mut event_map: HashMap<&str, Vec<&str>> = HashMap::new();
        for (k, v) in &event_fields {
            event_map.entry(k.as_str()).or_default().push(v.as_str());
        }

        for patterns in self.patterns.values() {
            for pattern in patterns {
                if self.pattern_matches(&pattern.fields, &event_map) {
                    return Ok(true);
                }
            }
        }
        Ok(false)
    }

    /// Count how many unique pattern IDs match the event
    pub fn count_matches(&self, event: &[u8]) -> Result<usize, QuaminaError> {
        Ok(self.matches_for_event(event)?.len())
    }

    /// Returns the number of unique pattern IDs stored
    pub fn pattern_count(&self) -> usize {
        self.patterns.len()
    }

    /// Returns true if no patterns are stored
    pub fn is_empty(&self) -> bool {
        self.patterns.is_empty()
    }

    /// Removes all patterns
    pub fn clear(&mut self) {
        self.patterns.clear();
    }
}

impl<X: Clone + Eq + Hash> Default for Quamina<X> {
    fn default() -> Self {
        Self::new()
    }
}

/// Shellstyle matching: simple wildcard where * matches any sequence (no escaping)
fn shellstyle_match(pattern: &str, text: &str) -> bool {
    // Split pattern by * to get literal segments
    let parts: Vec<&str> = pattern.split('*').collect();

    if parts.len() == 1 {
        // No wildcards, exact match
        return pattern == text;
    }

    let mut pos = 0;

    // First part must match at start (if non-empty)
    if !parts[0].is_empty() {
        if !text.starts_with(parts[0]) {
            return false;
        }
        pos = parts[0].len();
    }

    // Last part must match at end (if non-empty)
    let last = parts.last().unwrap();
    if !last.is_empty() {
        if !text.ends_with(last) {
            return false;
        }
        // Ensure middle parts don't overlap with end
        if text.len() < pos + last.len() {
            return false;
        }
    }

    // Middle parts must appear in order
    let text_to_search = if last.is_empty() {
        &text[pos..]
    } else {
        &text[pos..text.len() - last.len()]
    };

    let mut search_pos = 0;
    for part in &parts[1..parts.len() - 1] {
        if part.is_empty() {
            continue;
        }
        if let Some(found_at) = text_to_search[search_pos..].find(part) {
            search_pos += found_at + part.len();
        } else {
            return false;
        }
    }

    true
}

/// Wildcard matching supporting * as wildcard, with \* and \\ escaping
fn wildcard_match(pattern: &str, text: &str) -> bool {
    // Parse pattern into segments: either literal strings or wildcards
    let segments = parse_wildcard_pattern(pattern);

    // Match segments against text
    match_segments(&segments, text)
}

#[derive(Debug, PartialEq)]
enum WildcardSegment {
    Literal(String),
    Star,
}

/// Parse wildcard pattern handling \* and \\ escapes
fn parse_wildcard_pattern(pattern: &str) -> Vec<WildcardSegment> {
    let mut segments = Vec::new();
    let mut current_literal = String::new();
    let mut chars = pattern.chars().peekable();

    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                // Escape: next char is literal
                if let Some(&next) = chars.peek() {
                    if next == '*' || next == '\\' {
                        current_literal.push(chars.next().unwrap());
                    } else {
                        // Invalid escape - just keep the backslash
                        current_literal.push('\\');
                    }
                } else {
                    current_literal.push('\\');
                }
            }
            '*' => {
                // Unescaped star is a wildcard
                if !current_literal.is_empty() {
                    segments.push(WildcardSegment::Literal(std::mem::take(
                        &mut current_literal,
                    )));
                }
                segments.push(WildcardSegment::Star);
            }
            _ => {
                current_literal.push(c);
            }
        }
    }

    if !current_literal.is_empty() {
        segments.push(WildcardSegment::Literal(current_literal));
    }

    segments
}

/// Match parsed wildcard segments against text
fn match_segments(segments: &[WildcardSegment], text: &str) -> bool {
    if segments.is_empty() {
        return text.is_empty();
    }

    // Simple case: no wildcards
    if segments
        .iter()
        .all(|s| matches!(s, WildcardSegment::Literal(_)))
    {
        let full: String = segments
            .iter()
            .filter_map(|s| {
                if let WildcardSegment::Literal(lit) = s {
                    Some(lit.as_str())
                } else {
                    None
                }
            })
            .collect();
        return full == text;
    }

    // Use dynamic programming approach for general wildcard matching
    wildcard_dp(segments, text)
}

/// DP-based wildcard matching
fn wildcard_dp(segments: &[WildcardSegment], text: &str) -> bool {
    // Convert segments to a simpler form for DP
    let text_chars: Vec<char> = text.chars().collect();
    let n = text_chars.len();

    // Build pattern parts
    let mut parts: Vec<Option<String>> = Vec::new(); // None = *, Some = literal
    for seg in segments {
        match seg {
            WildcardSegment::Star => parts.push(None),
            WildcardSegment::Literal(s) => parts.push(Some(s.clone())),
        }
    }

    // dp[i] = can we match up to position i in text?
    // Start: only position 0 is reachable
    let mut reachable = vec![false; n + 1];
    reachable[0] = true;

    for part in &parts {
        match part {
            None => {
                // Star: can reach any position from current reachable positions onwards
                let mut new_reachable = vec![false; n + 1];
                let mut can_reach = false;
                for i in 0..=n {
                    if reachable[i] {
                        can_reach = true;
                    }
                    if can_reach {
                        new_reachable[i] = true;
                    }
                }
                reachable = new_reachable;
            }
            Some(literal) => {
                // Literal: must match exactly at reachable positions
                let mut new_reachable = vec![false; n + 1];
                let lit_chars: Vec<char> = literal.chars().collect();
                let lit_len = lit_chars.len();

                for i in 0..=n {
                    if reachable[i] && i + lit_len <= n {
                        // Check if literal matches at position i
                        if text_chars[i..i + lit_len]
                            .iter()
                            .zip(lit_chars.iter())
                            .all(|(a, b)| a == b)
                        {
                            new_reachable[i + lit_len] = true;
                        }
                    }
                }
                reachable = new_reachable;
            }
        }
    }

    reachable[n]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_match() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_no_match() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"status": "inactive"}"#.as_bytes())
            .unwrap();
        assert!(matches.is_empty());
    }

    #[test]
    fn test_numeric_match() {
        // This tests matching numeric values - currently may fail
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"count": [42]}"#).unwrap();

        let matches = q.matches_for_event(r#"{"count": 42}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match numeric value 42");
    }

    #[test]
    fn test_numeric_variant_matching() {
        // All these numeric representations of 35 should match pattern [35]
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [35]}"#).unwrap();

        // Integer form
        let m1 = q.matches_for_event(r#"{"x": 35}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["p1"], "35 should match");

        // Decimal with trailing zero
        let m2 = q.matches_for_event(r#"{"x": 35.0}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["p1"], "35.0 should match [35]");

        // Scientific notation
        let m3 = q.matches_for_event(r#"{"x": 3.5e1}"#.as_bytes()).unwrap();
        assert_eq!(m3, vec!["p1"], "3.5e1 should match [35]");
    }

    #[test]
    fn test_boolean_match() {
        // This tests matching boolean values - currently may fail
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"enabled": [true]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"enabled": true}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match boolean true");
    }

    #[test]
    fn test_null_match() {
        // This tests matching null values - currently may fail
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"value": [null]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"value": null}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match null value");
    }

    #[test]
    fn test_exists_true() {
        // Tests the exists operator - field must be present
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"exists": true}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"name": "anything", "other": 1}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match when field exists");

        let no_match = q.matches_for_event(r#"{"other": 1}"#.as_bytes()).unwrap();
        assert!(
            no_match.is_empty(),
            "Should not match when field is missing"
        );
    }

    #[test]
    fn test_exists_false() {
        // Tests the exists:false operator - field must NOT be present
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"exists": false}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"other": 1}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match when field is absent");

        let no_match = q
            .matches_for_event(r#"{"name": "value"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Should not match when field exists");
    }

    #[test]
    fn test_exists_with_empty_array() {
        // Per Go quamina: {"a": []} with exists:true does NOT match
        // but exists:false DOES match (no leaf values)
        let mut q_true = Quamina::new();
        q_true
            .add_pattern("p1", r#"{"a": [{"exists": true}]}"#)
            .unwrap();

        let mut q_false = Quamina::new();
        q_false
            .add_pattern("p2", r#"{"a": [{"exists": false}]}"#)
            .unwrap();

        // Event with empty array
        let event = r#"{"a": []}"#;

        // exists:true should NOT match (no leaf values in empty array)
        let matches_true = q_true.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches_true.is_empty(),
            "exists:true should not match empty array"
        );

        // exists:false SHOULD match (no leaf values means field effectively absent)
        let matches_false = q_false.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches_false,
            vec!["p2"],
            "exists:false should match empty array"
        );
    }

    #[test]
    fn test_prefix_match() {
        // Tests the prefix operator
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"prefix": "prod-"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"name": "prod-server-1"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match prefix");

        let no_match = q
            .matches_for_event(r#"{"name": "dev-server-1"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Should not match different prefix");
    }

    #[test]
    fn test_wildcard_suffix() {
        // Tests wildcard with * at start (suffix match)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"file": [{"wildcard": "*.txt"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"file": "document.txt"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match *.txt");

        let no_match = q
            .matches_for_event(r#"{"file": "document.pdf"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Should not match .pdf");
    }

    #[test]
    fn test_wildcard_prefix() {
        // Tests wildcard with * at end (prefix match)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"wildcard": "prod-*"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"name": "prod-server"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match prod-*");
    }

    #[test]
    fn test_wildcard_contains() {
        // Tests wildcard with * on both sides (contains)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"msg": [{"wildcard": "*error*"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"msg": "an error occurred"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match *error*");

        let no_match = q
            .matches_for_event(r#"{"msg": "all good"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_anything_but() {
        // Tests anything-but operator
        let mut q = Quamina::new();
        q.add_pattern(
            "p1",
            r#"{"status": [{"anything-but": ["deleted", "archived"]}]}"#,
        )
        .unwrap();

        let matches = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match non-excluded value");

        let no_match = q
            .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Should not match excluded value");
    }

    #[test]
    fn test_anything_but_validation() {
        // Empty anything-but array should be invalid (never matches)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": [{"anything-but": []}]}"#)
            .unwrap();

        // Empty array is invalid, pattern should never match
        let no_match = q
            .matches_for_event(r#"{"status": "anything"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "Empty anything-but should never match");

        // Non-string values in anything-but should be ignored
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"x": [{"anything-but": [1, true, null]}]}"#)
            .unwrap();

        // Pattern has no valid strings, so it's invalid
        let no_match2 = q2.matches_for_event(r#"{"x": "foo"}"#.as_bytes()).unwrap();
        assert!(
            no_match2.is_empty(),
            "Non-string anything-but should be invalid"
        );
    }

    #[test]
    fn test_equals_ignore_case() {
        // Tests case-insensitive matching
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"name": [{"equals-ignore-case": "Test"}]}"#)
            .unwrap();

        let m1 = q
            .matches_for_event(r#"{"name": "test"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["p1"], "Should match lowercase");

        let m2 = q
            .matches_for_event(r#"{"name": "TEST"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["p1"], "Should match uppercase");

        let m3 = q
            .matches_for_event(r#"{"name": "TeSt"}"#.as_bytes())
            .unwrap();
        assert_eq!(m3, vec!["p1"], "Should match mixed case");

        let no_match = q
            .matches_for_event(r#"{"name": "other"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_suffix() {
        // Tests suffix operator
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"file": [{"suffix": ".jpg"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"file": "photo.jpg"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q
            .matches_for_event(r#"{"file": "photo.png"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_nested_object_pattern() {
        // Tests matching nested object fields
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"user": {"role": ["admin"]}}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"user": {"role": "admin", "name": "alice"}}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "Should match nested field");

        let no_match = q
            .matches_for_event(r#"{"user": {"role": "guest"}}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_deeply_nested() {
        // Tests deeply nested patterns
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"a": {"b": {"c": ["value"]}}}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"a": {"b": {"c": "value"}}}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_multiple_patterns_same_id() {
        // Multiple patterns with same ID - any match counts
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p1", r#"{"status": ["pending"]}"#).unwrap();

        let m1 = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["p1"]);

        let m2 = q
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["p1"]);
    }

    #[test]
    fn test_delete_patterns() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();

        // Both match initially
        let m1 = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert!(m1.contains(&"p1"));

        // Delete p1
        q.delete_patterns(&"p1").unwrap();

        // p1 no longer matches
        let m2 = q
            .matches_for_event(r#"{"status": "active"}"#.as_bytes())
            .unwrap();
        assert!(m2.is_empty());

        // p2 still works
        let m3 = q
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(m3.contains(&"p2"));
    }

    #[test]
    fn test_or_within_field() {
        // Multiple values in array = OR
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active", "pending", "review"]}"#)
            .unwrap();

        for status in &["active", "pending", "review"] {
            let event = format!(r#"{{"status": "{}"}}"#, status);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "Should match {}", status);
        }

        let no_match = q
            .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_and_across_fields() {
        // Multiple fields = AND
        let mut q = Quamina::new();
        q.add_pattern(
            "p1",
            r#"{"type": ["order"], "status": ["pending"], "priority": ["high"]}"#,
        )
        .unwrap();

        let matches = q
            .matches_for_event(
                r#"{"type": "order", "status": "pending", "priority": "high"}"#.as_bytes(),
            )
            .unwrap();
        assert_eq!(matches, vec!["p1"]);

        // Missing one field
        let no_match = q
            .matches_for_event(r#"{"type": "order", "status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_numeric_greater_than() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"age": [{"numeric": [">", 18]}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"age": 25}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q.matches_for_event(r#"{"age": 18}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());

        let no_match2 = q.matches_for_event(r#"{"age": 15}"#.as_bytes()).unwrap();
        assert!(no_match2.is_empty());
    }

    #[test]
    fn test_numeric_range() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"score": [{"numeric": [">=", 0, "<=", 100]}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"score": 50}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let edge1 = q.matches_for_event(r#"{"score": 0}"#.as_bytes()).unwrap();
        assert_eq!(edge1, vec!["p1"]);

        let edge2 = q.matches_for_event(r#"{"score": 100}"#.as_bytes()).unwrap();
        assert_eq!(edge2, vec!["p1"]);

        let no_match = q.matches_for_event(r#"{"score": 101}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_numeric_equals() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"count": [{"numeric": ["=", 42]}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"count": 42}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q.matches_for_event(r#"{"count": 43}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_numeric_scientific_notation() {
        // JSON allows scientific notation: 3.5e2 = 350
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"value": [{"numeric": [">=", 300, "<=", 400]}]}"#)
            .unwrap();

        // All of these represent values in the 300-400 range
        let m1 = q.matches_for_event(r#"{"value": 350}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["p1"], "Integer 350 should match");

        let m2 = q
            .matches_for_event(r#"{"value": 350.0}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["p1"], "Float 350.0 should match");

        let m3 = q
            .matches_for_event(r#"{"value": 3.5e2}"#.as_bytes())
            .unwrap();
        assert_eq!(m3, vec!["p1"], "Scientific 3.5e2 should match");

        let m4 = q
            .matches_for_event(r#"{"value": 3.5E2}"#.as_bytes())
            .unwrap();
        assert_eq!(m4, vec!["p1"], "Scientific 3.5E2 should match");

        let m5 = q
            .matches_for_event(r#"{"value": 35e1}"#.as_bytes())
            .unwrap();
        assert_eq!(m5, vec!["p1"], "Scientific 35e1 should match");

        // Test negative exponents
        let mut q2 = Quamina::new();
        q2.add_pattern(
            "p2",
            r#"{"tiny": [{"numeric": [">=", 0.00001, "<=", 0.001]}]}"#,
        )
        .unwrap();

        let m6 = q2
            .matches_for_event(r#"{"tiny": 3.02e-5}"#.as_bytes())
            .unwrap();
        assert_eq!(m6, vec!["p2"], "Scientific 3.02e-5 should match");

        let m7 = q2
            .matches_for_event(r#"{"tiny": 1E-4}"#.as_bytes())
            .unwrap();
        assert_eq!(m7, vec!["p2"], "Scientific 1E-4 should match");
    }

    #[test]
    fn test_regex_match() {
        let mut q = Quamina::new();
        // Pattern code like ABC-123
        q.add_pattern("p1", r#"{"code": [{"regex": "^[A-Z]{3}-[0-9]{3}$"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"code": "ABC-123"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q
            .matches_for_event(r#"{"code": "invalid"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_regex_with_escape() {
        let mut q = Quamina::new();
        // Email pattern with escaped dot
        q.add_pattern("p1", r#"{"email": [{"regex": "^[a-z]+@example\\.com$"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"email": "alice@example.com"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"]);

        let no_match = q
            .matches_for_event(r#"{"email": "alice@exampleXcom"}"#.as_bytes())
            .unwrap();
        assert!(
            no_match.is_empty(),
            "Dot should be escaped, not match any char"
        );
    }

    #[test]
    fn test_clone_for_snapshot() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

        // Clone creates an independent snapshot
        let snapshot = q.clone();

        // Modify original
        q.add_pattern("p2", r#"{"status": ["pending"]}"#).unwrap();

        // Snapshot doesn't have p2
        let snap_matches = snapshot
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(snap_matches.is_empty());

        // Original has p2
        let orig_matches = q
            .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
            .unwrap();
        assert!(orig_matches.contains(&"p2"));
    }

    #[test]
    fn test_send_sync() {
        // Verify Quamina is Send + Sync for thread safety
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<Quamina<String>>();
    }

    #[test]
    fn test_has_matches() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();

        assert!(q.has_matches(r#"{"status": "active"}"#.as_bytes()).unwrap());
        assert!(!q
            .has_matches(r#"{"status": "inactive"}"#.as_bytes())
            .unwrap());
    }

    #[test]
    fn test_count_matches() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p2", r#"{"status": ["active"]}"#).unwrap();
        q.add_pattern("p3", r#"{"status": ["pending"]}"#).unwrap();

        assert_eq!(
            q.count_matches(r#"{"status": "active"}"#.as_bytes())
                .unwrap(),
            2
        );
        assert_eq!(
            q.count_matches(r#"{"status": "pending"}"#.as_bytes())
                .unwrap(),
            1
        );
        assert_eq!(
            q.count_matches(r#"{"status": "deleted"}"#.as_bytes())
                .unwrap(),
            0
        );
    }

    #[test]
    fn test_pattern_count_and_clear() {
        let mut q = Quamina::new();
        assert!(q.is_empty());
        assert_eq!(q.pattern_count(), 0);

        q.add_pattern("p1", r#"{"a": ["1"]}"#).unwrap();
        q.add_pattern("p2", r#"{"b": ["2"]}"#).unwrap();
        assert!(!q.is_empty());
        assert_eq!(q.pattern_count(), 2);

        q.clear();
        assert!(q.is_empty());
        assert_eq!(q.pattern_count(), 0);
    }

    #[test]
    fn test_unicode_escape_in_event() {
        // \u0048\u0065\u006c\u006c\u006f = "Hello"
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"greeting": ["Hello"]}"#).unwrap();

        // Event with unicode escapes
        let event = r#"{"greeting": "\u0048\u0065\u006c\u006c\u006f"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "Unicode escape should decode to 'Hello'"
        );
    }

    #[test]
    fn test_unicode_escape_emoji() {
        // Test UTF-16 surrogate pair for emoji 💋 (U+1F48B = D83D DC8B)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"emoji": ["💋"]}"#).unwrap();

        let event = r#"{"emoji": "\ud83d\udc8b"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "UTF-16 surrogate pair should decode to emoji"
        );
    }

    #[test]
    fn test_array_element_matching() {
        // Pattern should match if value is ANY element of the array
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"ids": [943]}"#).unwrap();

        // Event has array - should match if 943 is in the array
        let event = r#"{"ids": [116, 943, 234]}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "Should match when pattern value is in event array"
        );
    }

    #[test]
    fn test_wildcard_escape_star() {
        // \* in wildcard should match literal * character
        let mut q = Quamina::new();
        // Pattern: a\\*b matches literal "a*b" (double backslash in JSON = single backslash)
        q.add_pattern("p1", r#"{"val": [{"wildcard": "a\\*b"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"val": "a*b"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "\\* should match literal *");

        // Should NOT match "aXb" - the * is escaped, not a wildcard
        let no_match = q.matches_for_event(r#"{"val": "aXb"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty(), "Escaped * should not be wildcard");
    }

    #[test]
    fn test_wildcard_escape_backslash() {
        // \\ in wildcard should match literal \ character
        let mut q = Quamina::new();
        // Pattern: a\\\\b matches literal "a\b" (four backslash in JSON = two = one in wildcard)
        q.add_pattern("p1", r#"{"path": [{"wildcard": "a\\\\b"}]}"#)
            .unwrap();

        let matches = q
            .matches_for_event(r#"{"path": "a\\b"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches, vec!["p1"], "\\\\ should match literal \\");
    }

    #[test]
    fn test_wildcard_invalid_patterns() {
        // Invalid wildcard patterns should not match anything (silently rejected)
        let mut q = Quamina::new();

        // Adjacent ** is invalid
        q.add_pattern("p1", r#"{"x": [{"wildcard": "foo**bar"}]}"#)
            .unwrap();
        // Should not match since pattern is invalid
        let matches = q
            .matches_for_event(r#"{"x": "foobar"}"#.as_bytes())
            .unwrap();
        assert!(matches.is_empty(), "Adjacent ** should invalidate pattern");

        // Invalid escape \l (only \* and \\ are valid)
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"x": [{"wildcard": "he\\llo"}]}"#)
            .unwrap();
        let matches2 = q2
            .matches_for_event(r#"{"x": "hello"}"#.as_bytes())
            .unwrap();
        assert!(
            matches2.is_empty(),
            "Invalid escape \\l should invalidate pattern"
        );

        // Trailing backslash is invalid
        let mut q3 = Quamina::new();
        q3.add_pattern("p3", r#"{"x": [{"wildcard": "x\\"}]}"#)
            .unwrap();
        let matches3 = q3.matches_for_event(r#"{"x": "x"}"#.as_bytes()).unwrap();
        assert!(
            matches3.is_empty(),
            "Trailing backslash should invalidate pattern"
        );
    }

    #[test]
    fn test_shellstyle_suffix() {
        // shellstyle is simpler wildcard without escape support
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"a": [{"shellstyle": "*bc"}]}"#)
            .unwrap();

        // Should match "bc" and "abc"
        let matches = q.matches_for_event(r#"{"a": "bc"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let matches = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        // Should not match
        let no_match = q.matches_for_event(r#"{"a": "xyz"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_shellstyle_prefix() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"c": [{"shellstyle": "xy*"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"c": "xyzzz"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let matches = q.matches_for_event(r#"{"c": "xy"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_shellstyle_infix() {
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"b": [{"shellstyle": "d*f"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"b": "dexef"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let matches = q.matches_for_event(r#"{"b": "df"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);
    }

    #[test]
    fn test_shellstyle_multiple_wildcards() {
        // Multiple * in pattern
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"d": [{"shellstyle": "12*4*"}]}"#)
            .unwrap();

        let matches = q.matches_for_event(r#"{"d": "12345"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        let matches = q.matches_for_event(r#"{"d": "1244"}"#.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"]);

        // Should not match - missing "4"
        let no_match = q.matches_for_event(r#"{"d": "1235"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_shellstyle_contains() {
        // *foo* matches if "foo" appears anywhere
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*foo*"}]}"#)
            .unwrap();

        for text in ["xxfooyy", "fooyy", "xxfoo", "foo"] {
            let event = format!(r#"{{"x": "{}"}}"#, text);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "Should match {}", text);
        }

        let no_match = q.matches_for_event(r#"{"x": "bar"}"#.as_bytes()).unwrap();
        assert!(no_match.is_empty());
    }

    #[test]
    fn test_shellstyle_long_case() {
        // Test the "abab" suffix case from Go
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*abab"}]}"#)
            .unwrap();

        for text in ["abaabab", "ababab", "ababaabab", "abab"] {
            let event = format!(r#"{{"x": "{}"}}"#, text);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches, vec!["p1"], "Should match {}", text);
        }
    }

    #[test]
    fn test_invalid_pattern_handling() {
        // Test that invalid patterns don't cause panics and are handled gracefully
        let mut q = Quamina::new();

        // Empty pattern
        assert!(q.add_pattern("p1", "").is_err());

        // Non-object at top level
        assert!(q.add_pattern("p2", "33").is_err());
        assert!(q.add_pattern("p3", "[1,2]").is_err());

        // Malformed JSON
        assert!(q.add_pattern("p4", "{").is_err());
        assert!(q.add_pattern("p5", r#"{"foo": }"#).is_err());

        // Pattern field must be array or nested object
        assert!(q.add_pattern("p6", r#"{"foo": "string"}"#).is_err());
        assert!(q.add_pattern("p7", r#"{"foo": 123}"#).is_err());
        assert!(q.add_pattern("p8", r#"{"foo": true}"#).is_err());

        // Valid patterns should work
        assert!(q.add_pattern("valid1", r#"{"x": [1]}"#).is_ok());
        assert!(q.add_pattern("valid2", r#"{"x": ["string"]}"#).is_ok());
        assert!(q.add_pattern("valid3", r#"{"x": {"y": [1]}}"#).is_ok());
    }

    #[test]
    fn test_array_cross_element_matching() {
        // IMPORTANT: This tests cross-element array matching behavior
        // Pattern {"members": {"given": ["Mick"], "surname": ["Strummer"]}}
        // Event: members=[{given: "Joe", surname: "Strummer"}, {given: "Mick", surname: "Jones"}]
        //
        // Go quamina: would NOT match (no single element has both given=Mick AND surname=Strummer)
        // Our current impl: DOES match (flattening loses element grouping)
        //
        // This is a known limitation of our simple flattening approach.
        // Fixing this would require tracking array indices during flattening.

        let mut q = Quamina::new();
        q.add_pattern(
            "cross",
            r#"{"members": {"given": ["Mick"], "surname": ["Strummer"]}}"#,
        )
        .unwrap();

        let event = r#"{"members": [
            {"given": "Joe", "surname": "Strummer"},
            {"given": "Mick", "surname": "Jones"}
        ]}"#;

        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        // Current behavior: matches (incorrectly compared to Go)
        // This documents the limitation
        assert!(
            !matches.is_empty(),
            "Current impl matches cross-element (limitation)"
        );
    }

    #[test]
    fn test_wildcard_matches_empty_string() {
        // Based on Go quamina's wildcard tests
        // Pattern "*" should match empty string ""
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"wildcard": "*"}]}"#).unwrap();

        // Should match empty string
        let m1 = q.matches_for_event(r#"{"x": ""}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["p1"], "* should match empty string");

        // Should match any string
        let m2 = q.matches_for_event(r#"{"x": "hello"}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["p1"], "* should match any string");

        let m3 = q.matches_for_event(r#"{"x": "*"}"#.as_bytes()).unwrap();
        assert_eq!(m3, vec!["p1"], "* should match literal *");
    }

    #[test]
    fn test_multiple_overlapping_shellstyle_patterns() {
        // Based on Go quamina's TestWildCardRuler
        // Multiple shellstyle patterns can match the same event
        let mut q = Quamina::new();
        q.add_pattern("suffix_bc", r#"{"a": [{"shellstyle": "*bc"}]}"#)
            .unwrap();
        q.add_pattern("infix_ef", r#"{"b": [{"shellstyle": "d*f"}]}"#)
            .unwrap();
        q.add_pattern("infix_eff", r#"{"b": [{"shellstyle": "d*ff"}]}"#)
            .unwrap();
        q.add_pattern("prefix_xy", r#"{"c": [{"shellstyle": "xy*"}]}"#)
            .unwrap();

        // Test suffix match
        let m1 = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
        assert!(
            m1.contains(&"suffix_bc"),
            "*bc should match abc"
        );

        // Test infix match
        let m2 = q.matches_for_event(r#"{"b": "dexef"}"#.as_bytes()).unwrap();
        assert!(
            m2.contains(&"infix_ef"),
            "d*f should match dexef"
        );

        // Test both infix patterns match
        let m3 = q.matches_for_event(r#"{"b": "dexeff"}"#.as_bytes()).unwrap();
        assert_eq!(m3.len(), 2, "Both d*f and d*ff should match dexeff");
        assert!(m3.contains(&"infix_ef"));
        assert!(m3.contains(&"infix_eff"));

        // Test prefix match
        let m4 = q.matches_for_event(r#"{"c": "xyzzz"}"#.as_bytes()).unwrap();
        assert!(
            m4.contains(&"prefix_xy"),
            "xy* should match xyzzz"
        );

        // Test non-matches
        let m5 = q.matches_for_event(r#"{"a": "xyz"}"#.as_bytes()).unwrap();
        assert!(m5.is_empty(), "*bc should not match xyz");

        let m6 = q.matches_for_event(r#"{"b": "ef"}"#.as_bytes()).unwrap();
        assert!(m6.is_empty(), "d*f should not match ef (no d prefix)");
    }

    #[test]
    fn test_anything_but_prefix_relationship() {
        // Based on Go quamina's TestFootCornerCase
        // Tests that anything-but ["foo"] matches "foot" (since "foot" != "foo")
        let mut q = Quamina::new();
        q.add_pattern("not_foo", r#"{"z": [{"anything-but": ["foo"]}]}"#)
            .unwrap();

        // "foot" is not "foo", so should match
        let m1 = q.matches_for_event(r#"{"z": "foot"}"#.as_bytes()).unwrap();
        assert_eq!(
            m1,
            vec!["not_foo"],
            "anything-but ['foo'] should match 'foot'"
        );

        // "foo" should NOT match
        let m2 = q.matches_for_event(r#"{"z": "foo"}"#.as_bytes()).unwrap();
        assert!(m2.is_empty(), "anything-but ['foo'] should not match 'foo'");

        // "fo" is not "foo", so should match
        let m3 = q.matches_for_event(r#"{"z": "fo"}"#.as_bytes()).unwrap();
        assert_eq!(
            m3,
            vec!["not_foo"],
            "anything-but ['foo'] should match 'fo'"
        );
    }

    #[test]
    fn test_exists_false_ordering() {
        // Based on Go quamina's TestExistsFalseOrder
        // exists:false should properly disqualify a match regardless of where
        // it occurs lexicographically in the pattern
        let event = r#"{"aField": "a", "bField": "b", "cField": "c"}"#;

        // All these patterns should NOT match because each requires a field to be absent
        // that is actually present in the event
        let should_not_patterns = [
            // exists:false on middle field (bField)
            r#"{"aField": ["a"], "bField": [{"exists": false}], "cField": ["c"]}"#,
            // exists:false on first field (aField)
            r#"{"aField": [{"exists": false}], "bField": ["b"], "cField": ["c"]}"#,
            // exists:false on last field (cField)
            r#"{"aField": ["a"], "bField": ["b"], "cField": [{"exists": false}]}"#,
        ];

        for (i, pattern) in should_not_patterns.iter().enumerate() {
            let mut q = Quamina::new();
            q.add_pattern(format!("p{}", i), pattern).unwrap();
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.is_empty(),
                "Pattern {} should NOT match: {}",
                i,
                pattern
            );
        }

        // Also test with events that DO match these patterns (missing the required absent field)
        let events_that_match = [
            r#"{"aField": "a", "cField": "c"}"#,           // missing bField
            r#"{"bField": "b", "cField": "c"}"#,           // missing aField
            r#"{"aField": "a", "bField": "b"}"#,           // missing cField
        ];

        for (i, (pattern, event)) in should_not_patterns
            .iter()
            .zip(events_that_match.iter())
            .enumerate()
        {
            let mut q = Quamina::new();
            q.add_pattern(format!("p{}", i), pattern).unwrap();
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                !matches.is_empty(),
                "Pattern {} should match event {}: {}",
                i,
                i,
                pattern
            );
        }
    }
}
