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
}

impl<X: Clone + Eq + Hash> Default for Quamina<X> {
    fn default() -> Self {
        Self::new()
    }
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
}
