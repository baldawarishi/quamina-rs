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
        // Empty anything-but array should return error
        let mut q = Quamina::new();
        let result = q.add_pattern("p1", r#"{"status": [{"anything-but": []}]}"#);
        assert!(
            result.is_err(),
            "Empty anything-but array should be rejected"
        );

        // Non-string values in anything-but should return error
        let mut q2 = Quamina::new();
        let result2 = q2.add_pattern("p2", r#"{"x": [{"anything-but": [1, true, null]}]}"#);
        assert!(
            result2.is_err(),
            "anything-but with only non-strings should be rejected"
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
    fn test_invalid_json_events() {
        // Based on Go quamina's TestFJErrorCases
        // Test that various malformed JSON events return errors
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"a": [1]}"#).unwrap();

        // Truncated JSON
        assert!(
            q.matches_for_event(r#"{"a"#.as_bytes()).is_err(),
            "Truncated JSON should error"
        );
        assert!(
            q.matches_for_event(r#"{"a": "#.as_bytes()).is_err(),
            "Truncated value should error"
        );
        assert!(
            q.matches_for_event(r#"{"a": ["#.as_bytes()).is_err(),
            "Truncated array should error"
        );

        // Empty input
        assert!(
            q.matches_for_event(r#""#.as_bytes()).is_err(),
            "Empty input should error"
        );

        // Non-object at top level
        assert!(
            q.matches_for_event(r#""string""#.as_bytes()).is_err(),
            "String at top level should error"
        );
        assert!(
            q.matches_for_event(r#"[1, 2]"#.as_bytes()).is_err(),
            "Array at top level should error"
        );
        assert!(
            q.matches_for_event(r#"123"#.as_bytes()).is_err(),
            "Number at top level should error"
        );

        // Malformed JSON
        assert!(
            q.matches_for_event(r#"{ "a" : }"#.as_bytes()).is_err(),
            "Missing value should error"
        );
    }

    #[test]
    fn test_json_escape_sequences() {
        // Test that JSON escape sequences in events are properly decoded
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"msg": ["line1\nline2"]}"#).unwrap();

        // Event with \n escape (literal newline)
        let event = r#"{"msg": "line1\nline2"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Should match \\n escape sequence");

        // Test tab escape
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"x": ["a\tb"]}"#).unwrap();
        let m2 = q2.matches_for_event(r#"{"x": "a\tb"}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["p2"], "Should match \\t escape sequence");

        // Test backslash escape
        let mut q3 = Quamina::new();
        q3.add_pattern("p3", r#"{"x": ["a\\b"]}"#).unwrap();
        let m3 = q3.matches_for_event(r#"{"x": "a\\b"}"#.as_bytes()).unwrap();
        assert_eq!(m3, vec!["p3"], "Should match \\\\ escape sequence");

        // Test quote escape
        let mut q4 = Quamina::new();
        q4.add_pattern("p4", r#"{"x": ["say \"hello\""]}"#).unwrap();
        let m4 = q4
            .matches_for_event(r#"{"x": "say \"hello\""}"#.as_bytes())
            .unwrap();
        assert_eq!(m4, vec!["p4"], "Should match \\\" escape sequence");
    }

    #[test]
    fn test_equals_ignore_case_multiple_patterns() {
        // Based on Go quamina's TestEqualsIgnoreCaseMatching
        // Multiple patterns with different case variations should both match
        let mut q = Quamina::new();
        q.add_pattern("r1", r#"{"a": [{"equals-ignore-case": "aBc"}]}"#)
            .unwrap();
        q.add_pattern("r2", r#"{"b": [{"equals-ignore-case": "XyZ"}]}"#)
            .unwrap();
        q.add_pattern("r3", r#"{"b": [{"equals-ignore-case": "xyZ"}]}"#)
            .unwrap();

        // r1 matches any case of "abc"
        let m1 = q.matches_for_event(r#"{"a": "abc"}"#.as_bytes()).unwrap();
        assert_eq!(m1, vec!["r1"]);

        let m2 = q.matches_for_event(r#"{"a": "AbC"}"#.as_bytes()).unwrap();
        assert_eq!(m2, vec!["r1"]);

        // r2 and r3 both match "XYZ" (they're both case variations of the same value)
        let m3 = q.matches_for_event(r#"{"b": "XYZ"}"#.as_bytes()).unwrap();
        assert_eq!(m3.len(), 2, "Both r2 and r3 should match XYZ");
        assert!(m3.contains(&"r2"));
        assert!(m3.contains(&"r3"));

        // Non-matches
        let m4 = q.matches_for_event(r#"{"b": "xyzz"}"#.as_bytes()).unwrap();
        assert!(m4.is_empty(), "xyzz should not match xyz patterns");

        let m5 = q
            .matches_for_event(r#"{"b": "ABCXYZ"}"#.as_bytes())
            .unwrap();
        assert!(m5.is_empty(), "ABCXYZ should not match xyz patterns");
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
    fn test_regex_various_patterns() {
        // Based on Go quamina's TestRegexpEnd2End
        // Test various regex patterns for correctness

        // Alternation
        let mut q1 = Quamina::new();
        q1.add_pattern("p1", r#"{"a": [{"regex": "a|b"}]}"#).unwrap();
        assert!(q1.matches_for_event(r#"{"a": "a"}"#.as_bytes()).unwrap().contains(&"p1"));
        assert!(q1.matches_for_event(r#"{"a": "b"}"#.as_bytes()).unwrap().contains(&"p1"));
        assert!(q1.matches_for_event(r#"{"a": "c"}"#.as_bytes()).unwrap().is_empty());

        // Character class
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"a": [{"regex": "[hij]"}]}"#).unwrap();
        assert!(q2.matches_for_event(r#"{"a": "h"}"#.as_bytes()).unwrap().contains(&"p2"));
        assert!(q2.matches_for_event(r#"{"a": "i"}"#.as_bytes()).unwrap().contains(&"p2"));
        assert!(q2.matches_for_event(r#"{"a": "j"}"#.as_bytes()).unwrap().contains(&"p2"));
        assert!(q2.matches_for_event(r#"{"a": "x"}"#.as_bytes()).unwrap().is_empty());

        // Character range
        let mut q3 = Quamina::new();
        q3.add_pattern("p3", r#"{"a": [{"regex": "a[e-g]x"}]}"#).unwrap();
        assert!(q3.matches_for_event(r#"{"a": "aex"}"#.as_bytes()).unwrap().contains(&"p3"));
        assert!(q3.matches_for_event(r#"{"a": "afx"}"#.as_bytes()).unwrap().contains(&"p3"));
        assert!(q3.matches_for_event(r#"{"a": "agx"}"#.as_bytes()).unwrap().contains(&"p3"));
        assert!(q3.matches_for_event(r#"{"a": "ax"}"#.as_bytes()).unwrap().is_empty());

        // Ordinal suffix pattern (like 11th, 23rd)
        let mut q4 = Quamina::new();
        q4.add_pattern("p4", r#"{"a": [{"regex": "[0-9][0-9][rtn][dh]"}]}"#).unwrap();
        assert!(q4.matches_for_event(r#"{"a": "11th"}"#.as_bytes()).unwrap().contains(&"p4"));
        assert!(q4.matches_for_event(r#"{"a": "23rd"}"#.as_bytes()).unwrap().contains(&"p4"));
        assert!(q4.matches_for_event(r#"{"a": "22nd"}"#.as_bytes()).unwrap().contains(&"p4"));
        assert!(q4.matches_for_event(r#"{"a": "first"}"#.as_bytes()).unwrap().is_empty());
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
    fn test_unicode_escape_multiple_emojis() {
        // Test multiple UTF-16 surrogate pairs in sequence
        // From Go's escaping_test.go: 😀💋😺 = \ud83d\ude00\ud83d\udc8b\ud83d\ude3a
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"emojis": ["😀💋😺"]}"#).unwrap();

        let event = r#"{"emojis": "\ud83d\ude00\ud83d\udc8b\ud83d\ude3a"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            matches,
            vec!["p1"],
            "Multiple surrogate pairs should decode correctly"
        );
    }

    #[test]
    fn test_unicode_escape_mixed_codepoints() {
        // Test mixing single-codepoint and surrogate pairs
        // From Go's escaping_test.go combinations
        // Ж = \u0416 (single), 💋 = \ud83d\udc8b (surrogate), 中 = \u4e2d (single)

        // Test: Ж💋中
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"mixed": ["Ж💋中"]}"#).unwrap();

        let event = r#"{"mixed": "\u0416\ud83d\udc8b\u4e2d"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "Mixed codepoints should decode");

        // Test: x💋y - ASCII mixed with surrogate
        let mut q2 = Quamina::new();
        q2.add_pattern("p2", r#"{"mixed": ["x💋y"]}"#).unwrap();

        let event2 = r#"{"mixed": "\u0078\ud83d\udc8b\u0079"}"#;
        let matches2 = q2.matches_for_event(event2.as_bytes()).unwrap();
        assert_eq!(matches2, vec!["p2"], "ASCII + surrogate should decode");
    }

    #[test]
    fn test_unicode_escape_standard_escapes() {
        // Test standard JSON escape sequences
        let mut q = Quamina::new();

        // Test newline
        q.add_pattern("newline", r#"{"text": ["hello\nworld"]}"#)
            .unwrap();
        let m1 = q
            .matches_for_event(r#"{"text": "hello\nworld"}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["newline"], "Newline escape should match");

        // Test tab
        q.add_pattern("tab", r#"{"text": ["hello\tworld"]}"#)
            .unwrap();
        let m2 = q
            .matches_for_event(r#"{"text": "hello\tworld"}"#.as_bytes())
            .unwrap();
        assert!(m2.contains(&"tab"), "Tab escape should match");

        // Test backslash
        q.add_pattern("backslash", r#"{"text": ["hello\\world"]}"#)
            .unwrap();
        let m3 = q
            .matches_for_event(r#"{"text": "hello\\world"}"#.as_bytes())
            .unwrap();
        assert!(m3.contains(&"backslash"), "Backslash escape should match");
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
        // Invalid wildcard patterns should return errors

        // Adjacent ** is invalid
        let mut q = Quamina::new();
        let result = q.add_pattern("p1", r#"{"x": [{"wildcard": "foo**bar"}]}"#);
        assert!(result.is_err(), "Adjacent ** should be rejected");

        // Invalid escape \l (only \* and \\ are valid)
        let mut q2 = Quamina::new();
        let result2 = q2.add_pattern("p2", r#"{"x": [{"wildcard": "he\\llo"}]}"#);
        assert!(result2.is_err(), "Invalid escape \\l should be rejected");

        // Trailing backslash is invalid
        let mut q3 = Quamina::new();
        let result3 = q3.add_pattern("p3", r#"{"x": [{"wildcard": "x\\"}]}"#);
        assert!(result3.is_err(), "Trailing backslash should be rejected");
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
    fn test_array_cross_element_comprehensive() {
        // More comprehensive test from Go's arrays_test.go TestArrayCorrectness
        // Tests the "bands" scenario with multiple patterns where only one should match
        //
        // In Go quamina: only "Wata guitar" pattern matches
        // In Rust (limitation): all three patterns incorrectly match

        let bands = r#"{
            "bands": [
                {
                    "name": "The Clash",
                    "members": [
                        {"given": "Joe", "surname": "Strummer", "role": ["guitar", "vocals"]},
                        {"given": "Mick", "surname": "Jones", "role": ["guitar", "vocals"]},
                        {"given": "Paul", "surname": "Simonon", "role": ["bass"]},
                        {"given": "Topper", "surname": "Headon", "role": ["drums"]}
                    ]
                },
                {
                    "name": "Boris",
                    "members": [
                        {"given": "Wata", "role": ["guitar", "vocals"]},
                        {"given": "Atsuo", "role": ["drums"]},
                        {"given": "Takeshi", "role": ["bass", "vocals"]}
                    ]
                }
            ]
        }"#;

        let mut q = Quamina::new();
        // Pattern 1: Mick with surname Strummer - SHOULD NOT match (cross-element)
        q.add_pattern(
            "mick_strummer",
            r#"{"bands": {"members": {"given": ["Mick"], "surname": ["Strummer"]}}}"#,
        )
        .unwrap();
        // Pattern 2: Wata with role drums - SHOULD NOT match (cross-element)
        q.add_pattern(
            "wata_drums",
            r#"{"bands": {"members": {"given": ["Wata"], "role": ["drums"]}}}"#,
        )
        .unwrap();
        // Pattern 3: Wata with role guitar - SHOULD match (same element)
        q.add_pattern(
            "wata_guitar",
            r#"{"bands": {"members": {"given": ["Wata"], "role": ["guitar"]}}}"#,
        )
        .unwrap();

        let matches = q.matches_for_event(bands.as_bytes()).unwrap();

        // Document the limitation: we get false positives
        // Go would return only ["wata_guitar"]
        // Rust returns all three due to cross-element matching bug
        assert!(
            matches.contains(&"wata_guitar"),
            "wata_guitar should always match"
        );

        // Document the false positives (this is the limitation)
        // In a correct implementation, these would NOT be in the matches
        assert!(
            matches.contains(&"mick_strummer"),
            "Limitation: mick_strummer incorrectly matches (false positive)"
        );
        assert!(
            matches.contains(&"wata_drums"),
            "Limitation: wata_drums incorrectly matches (false positive)"
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

    #[test]
    fn test_overlapping_exact_match_patterns() {
        // Based on Go quamina's TestOverlappingValues
        // Tests patterns with overlapping prefixes (foo, football, footballer)
        // to ensure exact matching with no false positives
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"a": ["foo"]}"#).unwrap();
        q.add_pattern("p2", r#"{"a": ["football"]}"#).unwrap();
        q.add_pattern("p3", r#"{"a": ["footballer"]}"#).unwrap();

        // Each event should only match its corresponding pattern
        let matches1 = q
            .matches_for_event(r#"{"x": 3, "a": "foo"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches1,
            vec!["p1"],
            "foo should only match p1, not football or footballer"
        );

        let matches2 = q
            .matches_for_event(r#"{"x": 3, "a": "football"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches2,
            vec!["p2"],
            "football should only match p2, not foo or footballer"
        );

        let matches3 = q
            .matches_for_event(r#"{"x": 3, "a": "footballer"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches3,
            vec!["p3"],
            "footballer should only match p3, not foo or football"
        );

        // Ensure no matches for unrelated values
        let no_match = q
            .matches_for_event(r#"{"a": "foot"}"#.as_bytes())
            .unwrap();
        assert!(no_match.is_empty(), "foot should not match any pattern");
    }

    #[test]
    fn test_same_pattern_id_multiple_value_types() {
        // Based on Go quamina's TestExerciseSingletonReplacement and TestMergeNfaAndNumeric
        // Same pattern ID can match via different value types (string OR number)
        let mut q = Quamina::new();
        // Add two patterns with same ID but different value types
        q.add_pattern("x", r#"{"x": ["a"]}"#).unwrap();
        q.add_pattern("x", r#"{"x": [1]}"#).unwrap();

        // Both string and number should match pattern "x"
        let matches1 = q
            .matches_for_event(r#"{"x": 1}"#.as_bytes())
            .unwrap();
        assert_eq!(matches1, vec!["x"], "number 1 should match");

        let matches2 = q
            .matches_for_event(r#"{"x": "a"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches2, vec!["x"], "string 'a' should match");

        // Test wildcard OR number for same pattern ID
        let mut q2 = Quamina::new();
        q2.add_pattern("x", r#"{"x": [{"wildcard": "x*y"}]}"#)
            .unwrap();
        q2.add_pattern("x", r#"{"x": [3]}"#).unwrap();

        let m1 = q2
            .matches_for_event(r#"{"x": 3}"#.as_bytes())
            .unwrap();
        assert_eq!(m1, vec!["x"], "number 3 should match");

        let m2 = q2
            .matches_for_event(r#"{"x": "xasdfy"}"#.as_bytes())
            .unwrap();
        assert_eq!(m2, vec!["x"], "wildcard pattern should match");
    }

    #[test]
    fn test_empty_regex_matches_empty_string() {
        // Based on Go quamina's TestEmptyRegexp
        // Empty regex pattern should match empty string value
        let mut q = Quamina::new();
        q.add_pattern("a", r#"{"a": [{"regex": ""}]}"#).unwrap();

        let matches = q
            .matches_for_event(r#"{"a": ""}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches,
            vec!["a"],
            "empty regex should match empty string"
        );

        // Empty regex should also match non-empty strings (since empty pattern matches anywhere)
        let matches2 = q
            .matches_for_event(r#"{"a": "hello"}"#.as_bytes())
            .unwrap();
        assert_eq!(
            matches2,
            vec!["a"],
            "empty regex should match any string"
        );
    }

    #[test]
    fn test_anything_but_with_exact_match() {
        // Based on Go quamina's TestAnythingButMerging
        // Tests that exact and anything-but patterns can coexist
        let mut q = Quamina::new();

        // Add exact match for "foo"
        q.add_pattern("pFoo", r#"{"z": ["foo"]}"#).unwrap();
        // Add anything-but for "foot"
        q.add_pattern("pAbFoot", r#"{"z": [{"anything-but": ["foot"]}]}"#)
            .unwrap();

        // "foo" should match BOTH patterns:
        // - pFoo: exact match
        // - pAbFoot: "foo" is not "foot"
        let matches = q
            .matches_for_event(r#"{"z": "foo"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches.len(), 2, "foo should match both patterns");

        // "foot" should match neither:
        // - pFoo: not "foot"
        // - pAbFoot: excluded
        let matches2 = q
            .matches_for_event(r#"{"z": "foot"}"#.as_bytes())
            .unwrap();
        assert!(matches2.is_empty(), "foot should match nothing");

        // "bar" should match only pAbFoot (not foo, not foot)
        let matches3 = q
            .matches_for_event(r#"{"z": "bar"}"#.as_bytes())
            .unwrap();
        assert_eq!(matches3.len(), 1, "bar should only match pAbFoot");
        assert!(matches3.contains(&"pAbFoot"));
    }

    #[test]
    fn test_anything_but_with_overlapping_exclusions() {
        // Based on Go quamina's TestAnythingButAlgo
        // Tests anything-but with overlapping prefix exclusions
        let mut q = Quamina::new();
        q.add_pattern(
            "notTTT",
            r#"{"x": [{"anything-but": ["tim", "time", "timed"]}]}"#,
        )
        .unwrap();

        // All excluded values should not match
        let excluded = ["tim", "time", "timed"];
        for val in excluded {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(matches.is_empty(), "{} should be excluded", val);
        }

        // Similar but non-excluded values should match
        let included = ["t", "ti", "timer", "timely", "timekeeper"];
        for val in included {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(matches.len(), 1, "{} should match notTTT", val);
        }
    }

    #[test]
    fn test_shellstyle_repeated_sequences() {
        // Based on Go quamina's TestLongCase
        // Tests shellstyle suffix patterns with overlapping sequences
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"x": [{"shellstyle": "*abab"}]}"#)
            .unwrap();

        // These should all match *abab
        let should_match = ["abab", "abaabab", "ababab", "ababaabab", "xxabab"];
        for val in should_match {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert_eq!(
                matches,
                vec!["p1"],
                "*abab should match '{}'",
                val
            );
        }

        // These should not match
        let should_not = ["abab_", "aba", "ab", "xaba"];
        for val in should_not {
            let event = format!(r#"{{"x": "{}"}}"#, val);
            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(matches.is_empty(), "*abab should NOT match '{}'", val);
        }
    }

    #[test]
    fn test_shellstyle_complex_wildcards() {
        // Based on Go quamina's TestMakeShellStyleFA
        // Tests shellstyle patterns with multiple wildcards in complex positions
        let test_cases = [
            // Pattern with two wildcards
            (
                r#"{"x": [{"shellstyle": "xx*yy*zz"}]}"#,
                vec!["xxabyycdzz", "xxyyzz", "xxyyzzzzz"],
                vec!["xyzyxzy yy zz", "zz yy xx"],
            ),
            // Pattern with wildcards at both ends
            (
                r#"{"x": [{"shellstyle": "*xx*yy*"}]}"#,
                vec!["xxyy", "xxyyef", "abxxyy", "abxxcdyy"],
                vec!["ayybyyzxx", "xyzzy"],
            ),
        ];

        for (pattern, should_match, should_not) in test_cases {
            let mut q = Quamina::new();
            q.add_pattern("p1", pattern).unwrap();

            for val in should_match {
                let event = format!(r#"{{"x": "{}"}}"#, val);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert_eq!(
                    matches,
                    vec!["p1"],
                    "{} should match '{}'",
                    pattern,
                    val
                );
            }

            for val in should_not {
                let event = format!(r#"{{"x": "{}"}}"#, val);
                let matches = q.matches_for_event(event.as_bytes()).unwrap();
                assert!(
                    matches.is_empty(),
                    "{} should NOT match '{}'",
                    pattern,
                    val
                );
            }
        }
    }

    #[test]
    fn test_invalid_pattern_validation() {
        // Based on Go quamina's TestPatternFromJSON
        // Tests that various invalid patterns are properly rejected
        let invalid_patterns = [
            // Value not in array (must be array or object)
            (r#"{"foo": 11}"#, "number not in array"),
            (r#"{"foo": "x"}"#, "string not in array"),
            (r#"{"foo": true}"#, "boolean not in array"),
            (r#"{"foo": null}"#, "null not in array"),
            // Invalid exists operator
            (r#"{"x": [{"exists": 23}]}"#, "exists with number"),
            (r#"{"x": [{"exists": "yes"}]}"#, "exists with string"),
            // Invalid shellstyle
            (r#"{"x": [{"shellstyle": 15}]}"#, "shellstyle with number"),
            (r#"{"x": [{"shellstyle": "a**b"}]}"#, "shellstyle with **"),
            // Invalid prefix
            (r#"{"x": [{"prefix": 23}]}"#, "prefix with number"),
            // Invalid suffix
            (r#"{"x": [{"suffix": 23}]}"#, "suffix with number"),
            // Invalid equals-ignore-case
            (
                r#"{"x": [{"equals-ignore-case": 5}]}"#,
                "equals-ignore-case with number",
            ),
            // Invalid numeric
            (r#"{"x": [{"numeric": ">=5"}]}"#, "numeric with string"),
            // Invalid regex
            (
                r#"{"x": [{"regex": "[invalid"}]}"#,
                "regex with invalid pattern",
            ),
            // Unknown operator
            (r#"{"x": [{"unknown-op": "val"}]}"#, "unknown operator"),
        ];

        for (pattern, desc) in &invalid_patterns {
            let mut q = Quamina::new();
            let result = q.add_pattern("test", pattern);
            assert!(result.is_err(), "{} should be rejected: {}", desc, pattern);
        }
    }

    #[test]
    fn test_exercise_matching_comprehensive() {
        // Based on Go quamina's TestExerciseMatching
        // Tests many different pattern types against a complex JSON event
        let event = r#"{
            "Image": {
                "Width":  800,
                "Height": 600,
                "Title":  "View from 15th Floor",
                "Thumbnail": {
                    "Url":    "https://www.example.com/image/481989943",
                    "Height": 125,
                    "Width":  100
                },
                "Animated" : false,
                "IDs": [116, 943, 234, 38793]
            }
        }"#;

        // Patterns that SHOULD match
        let should_match = [
            (r#"{"Image": {"Title": [{"exists": true}]}}"#, "exists true on Title"),
            (r#"{"Foo": [{"exists": false}]}"#, "exists false on missing Foo"),
            (r#"{"Image": {"Width": [800]}}"#, "exact number match"),
            (r#"{"Image": {"Animated": [false], "Thumbnail": {"Height": [125]}}}"#, "nested multi-field"),
            (r#"{"Image": {"Width": [800], "Title": [{"exists": true}], "Animated": [false]}}"#, "three fields"),
            (r#"{"Image": {"Width": [800], "IDs": [{"exists": true}]}}"#, "exists on array"),
            (r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "*9943"}]}}}"#, "shellstyle suffix"),
            (r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "https://www.example.com/*"}]}}}"#, "shellstyle prefix"),
            (r#"{"Image": {"Thumbnail": {"Url": [{"shellstyle": "https://www.example.com/*9943"}]}}}"#, "shellstyle infix"),
            (r#"{"Image": {"Title": [{"anything-but": ["Pikachu", "Eevee"]}]}}"#, "anything-but"),
            (r#"{"Image": {"Thumbnail": {"Url": [{"prefix": "https:"}]}}}"#, "prefix"),
            (r#"{"Image": {"Thumbnail": {"Url": ["a", {"prefix": "https:"}]}}}"#, "prefix or literal"),
            (r#"{"Image": {"Title": [{"equals-ignore-case": "VIEW FROM 15th FLOOR"}]}}"#, "equals-ignore-case"),
            (r#"{"Image": {"Title": [{"regex": "View from .... Floor"}]}}"#, "regex dots"),
            (r#"{"Image": {"Title": [{"regex": "View from [0-9][0-9][rtn][dh] Floor"}]}}"#, "regex char class"),
            (r#"{"Image": {"Title": [{"regex": "View from 15th (Floor|Storey)"}]}}"#, "regex alternation"),
        ];

        // Patterns that SHOULD NOT match
        let should_not_match = [
            (r#"{"Image": {"Animated": [{"exists": false}]}}"#, "exists false on present field"),
            (r#"{"Image": {"NotThere": [{"exists": true}]}}"#, "exists true on missing field"),
            (r#"{"Image": {"IDs": [{"exists": false}], "Animated": [false]}}"#, "exists false on array"),
            (r#"{"Image": {"Thumbnail": {"Url": [{"prefix": "http:"}]}}}"#, "wrong prefix"),
        ];

        // Test each should_match pattern individually
        for (pattern, desc) in &should_match {
            let mut q = Quamina::new();
            let result = q.add_pattern(*desc, pattern);
            assert!(result.is_ok(), "Pattern should parse: {} - {}", desc, pattern);

            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                !matches.is_empty(),
                "Pattern '{}' should match: {}",
                desc,
                pattern
            );
        }

        // Test each should_not_match pattern individually
        for (pattern, desc) in &should_not_match {
            let mut q = Quamina::new();
            let result = q.add_pattern(*desc, pattern);
            assert!(result.is_ok(), "Pattern should parse: {} - {}", desc, pattern);

            let matches = q.matches_for_event(event.as_bytes()).unwrap();
            assert!(
                matches.is_empty(),
                "Pattern '{}' should NOT match: {}",
                desc,
                pattern
            );
        }

        // Test all patterns together in one matcher
        let mut combined = Quamina::new();
        for (pattern, desc) in &should_match {
            combined.add_pattern(*desc, pattern).unwrap();
        }

        let all_matches = combined.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(
            all_matches.len(),
            should_match.len(),
            "All should_match patterns should match when combined"
        );
    }
}
