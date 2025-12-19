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
        let event_map: HashMap<&str, &str> = event_fields
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect();

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
        event_map: &HashMap<&str, &str>,
    ) -> bool {
        // All pattern fields must match (AND across fields)
        for (field, matchers) in pattern_fields {
            let event_value = event_map.get(field.as_str());

            // Any matcher can match (OR within field)
            let field_matches = matchers.iter().any(|matcher| match matcher {
                Matcher::Exact(expected) => event_value == Some(&expected.as_str()),
                Matcher::Exists(should_exist) => {
                    if *should_exist {
                        event_value.is_some()
                    } else {
                        event_value.is_none()
                    }
                }
                Matcher::Prefix(prefix) => {
                    event_value.map(|v| v.starts_with(prefix)).unwrap_or(false)
                }
                Matcher::Suffix(suffix) => {
                    event_value.map(|v| v.ends_with(suffix)).unwrap_or(false)
                }
                Matcher::Wildcard(pattern) => event_value
                    .map(|v| wildcard_match(pattern, v))
                    .unwrap_or(false),
                Matcher::AnythingBut(excluded) => event_value
                    .map(|v| !excluded.iter().any(|e| e == v))
                    .unwrap_or(false),
                Matcher::EqualsIgnoreCase(expected) => event_value
                    .map(|v| v.eq_ignore_ascii_case(expected))
                    .unwrap_or(false),
                Matcher::Numeric(cmp) => event_value
                    .and_then(|v| v.parse::<f64>().ok())
                    .map(|num| {
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
                    .unwrap_or(false),
                Matcher::Regex(re) => event_value.map(|v| re.is_match(v)).unwrap_or(false),
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
        let event_map: HashMap<&str, &str> = event_fields
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect();

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

/// Simple wildcard matching supporting * as a wildcard character
fn wildcard_match(pattern: &str, text: &str) -> bool {
    // Handle simple cases first
    if pattern == "*" {
        return true;
    }
    if !pattern.contains('*') {
        return pattern == text;
    }

    let parts: Vec<&str> = pattern.split('*').collect();

    // Pattern like "*.txt" (suffix match)
    if parts.len() == 2 && parts[0].is_empty() {
        return text.ends_with(parts[1]);
    }

    // Pattern like "prod-*" (prefix match)
    if parts.len() == 2 && parts[1].is_empty() {
        return text.starts_with(parts[0]);
    }

    // Pattern like "*error*" (contains)
    if parts.len() == 3 && parts[0].is_empty() && parts[2].is_empty() {
        return text.contains(parts[1]);
    }

    // Pattern like "pre*suf" (prefix and suffix)
    if parts.len() == 2 {
        return text.starts_with(parts[0])
            && text.ends_with(parts[1])
            && text.len() >= parts[0].len() + parts[1].len();
    }

    // General case: multiple wildcards
    let mut pos = 0;
    for (i, part) in parts.iter().enumerate() {
        if part.is_empty() {
            continue;
        }
        if i == 0 {
            // First part must be at start
            if !text.starts_with(part) {
                return false;
            }
            pos = part.len();
        } else if i == parts.len() - 1 {
            // Last part must be at end
            if !text[pos..].ends_with(part) {
                return false;
            }
        } else {
            // Middle parts can be anywhere
            if let Some(found) = text[pos..].find(part) {
                pos += found + part.len();
            } else {
                return false;
            }
        }
    }
    true
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
        assert_eq!(matches, vec!["p1"], "Unicode escape should decode to 'Hello'");
    }

    #[test]
    fn test_unicode_escape_emoji() {
        // Test UTF-16 surrogate pair for emoji 💋 (U+1F48B = D83D DC8B)
        let mut q = Quamina::new();
        q.add_pattern("p1", r#"{"emoji": ["💋"]}"#).unwrap();

        let event = r#"{"emoji": "\ud83d\udc8b"}"#;
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["p1"], "UTF-16 surrogate pair should decode to emoji");
    }
}
