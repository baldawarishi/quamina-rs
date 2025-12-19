//! quamina-rs: Fast pattern-matching library for filtering JSON events

mod json;

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
    fields: HashMap<String, Vec<String>>,
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
        pattern_fields: &HashMap<String, Vec<String>>,
        event_map: &HashMap<&str, &str>,
    ) -> bool {
        // All pattern fields must match (AND across fields)
        for (field, allowed_values) in pattern_fields {
            match event_map.get(field.as_str()) {
                Some(event_value) => {
                    // Any allowed value can match (OR within field)
                    if !allowed_values.iter().any(|v| v == event_value) {
                        return false;
                    }
                }
                None => return false, // Field must exist
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
