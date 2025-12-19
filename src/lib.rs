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
    pub fn add_pattern(&mut self, _x: X, _pattern: &str) -> Result<(), QuaminaError> {
        todo!("Step 2: implement pattern parsing")
    }

    /// Find all patterns that match the given event
    pub fn matches_for_event(&self, _event: &[u8]) -> Result<Vec<X>, QuaminaError> {
        todo!("Step 3: implement matching")
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
