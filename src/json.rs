//! Minimal JSON parser for flattening events and patterns

use crate::QuaminaError;
use std::collections::HashMap;

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
    EqualsIgnoreCase(String),
    Numeric(NumericComparison),
    Regex(regex::Regex),
}

/// Numeric comparison operators
#[derive(Debug, Clone, PartialEq)]
pub struct NumericComparison {
    pub lower: Option<(bool, f64)>, // (inclusive, value)
    pub upper: Option<(bool, f64)>, // (inclusive, value)
}

/// Flatten a JSON event into path/value pairs
/// e.g., {"a": {"b": 1}} -> [("a.b", "1")]
pub fn flatten_event(json: &[u8]) -> Result<Vec<(String, String)>, QuaminaError> {
    let s = std::str::from_utf8(json).map_err(|_| QuaminaError::InvalidUtf8)?;
    let mut parser = Parser::new(s);
    let value = parser.parse_value()?;

    // Event must be a JSON object at top level
    if !matches!(value, Value::Object(_)) {
        return Err(QuaminaError::InvalidJson(
            "event must be a JSON object".into(),
        ));
    }

    let mut result = Vec::new();
    flatten_value(&value, String::new(), &mut result);
    Ok(result)
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
            format!("{}.{}", prefix, key)
        };
        match value {
            Value::Array(arr) => {
                let matchers: Vec<Matcher> = arr.iter().map(value_to_matcher).collect();
                fields.insert(path, matchers);
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

fn value_to_matcher(value: &Value) -> Matcher {
    match value {
        Value::Object(obj) => {
            // Check for operators like {"exists": true} or {"prefix": "str"}
            if let Some((key, val)) = obj.first() {
                match key.as_str() {
                    "exists" => {
                        if let Value::Bool(b) = val {
                            return Matcher::Exists(*b);
                        }
                    }
                    "prefix" => {
                        if let Value::String(s) = val {
                            return Matcher::Prefix(s.clone());
                        }
                    }
                    "suffix" => {
                        if let Value::String(s) = val {
                            return Matcher::Suffix(s.clone());
                        }
                    }
                    "wildcard" => {
                        if let Value::String(s) = val {
                            if validate_wildcard(s) {
                                return Matcher::Wildcard(s.clone());
                            }
                        }
                    }
                    "shellstyle" => {
                        if let Value::String(s) = val {
                            // shellstyle doesn't allow adjacent ** characters
                            if !s.contains("**") {
                                return Matcher::Shellstyle(s.clone());
                            }
                        }
                    }
                    "anything-but" => {
                        if let Value::Array(arr) = val {
                            // Reject empty array (matches Go behavior)
                            if arr.is_empty() {
                                // Return a matcher that never matches
                                return Matcher::Exact(String::new());
                            }
                            // Only accept strings in anything-but array
                            let excluded: Vec<String> = arr
                                .iter()
                                .filter_map(|v| match v {
                                    Value::String(s) => Some(s.clone()),
                                    _ => None,
                                })
                                .collect();
                            // If no valid strings, treat as invalid
                            if excluded.is_empty() {
                                return Matcher::Exact(String::new());
                            }
                            return Matcher::AnythingBut(excluded);
                        }
                    }
                    "equals-ignore-case" => {
                        if let Value::String(s) = val {
                            return Matcher::EqualsIgnoreCase(s.clone());
                        }
                    }
                    "numeric" => {
                        if let Value::Array(arr) = val {
                            if let Some(cmp) = parse_numeric_comparison(arr) {
                                return Matcher::Numeric(cmp);
                            }
                        }
                    }
                    "regex" => {
                        if let Value::String(s) = val {
                            if let Ok(re) = regex::Regex::new(s) {
                                return Matcher::Regex(re);
                            }
                        }
                    }
                    _ => {}
                }
            }
            Matcher::Exact(String::new()) // fallback
        }
        Value::Number(n) => {
            // For numeric values, store as float for proper comparison
            // This ensures 35, 35.0, and 3.5e1 all match each other
            if let Ok(f) = n.parse::<f64>() {
                Matcher::NumericExact(f)
            } else {
                Matcher::Exact(value_to_string(value))
            }
        }
        _ => Matcher::Exact(value_to_string(value)),
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

fn flatten_value(value: &Value, path: String, result: &mut Vec<(String, String)>) {
    match value {
        Value::Object(obj) => {
            for (key, val) in obj {
                let new_path = if path.is_empty() {
                    key.clone()
                } else {
                    format!("{}.{}", path, key)
                };
                flatten_value(val, new_path, result);
            }
        }
        Value::Array(arr) => {
            // For array elements, use the same path (no index)
            // This allows pattern {"ids": [943]} to match event {"ids": [116, 943, 234]}
            for val in arr {
                flatten_value(val, path.clone(), result);
            }
        }
        _ => result.push((path, value_to_string(value))),
    }
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
