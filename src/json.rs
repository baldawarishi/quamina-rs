//! Minimal JSON parser for flattening events and patterns

use crate::regexp::{parse_regexp, RegexpRoot};
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
    EqualsIgnoreCase(String),
    Numeric(NumericComparison),
    /// Regex pattern parsed into our custom NFA (automaton-compatible)
    ParsedRegexp(RegexpRoot),
    /// Regex pattern using the regex crate (fallback for unsupported features)
    Regex(regex::Regex),
}

/// Numeric comparison operators
#[derive(Debug, Clone, PartialEq)]
pub struct NumericComparison {
    pub lower: Option<(bool, f64)>, // (inclusive, value)
    pub upper: Option<(bool, f64)>, // (inclusive, value)
}

impl Matcher {
    /// Check if this matcher is supported by the automaton-based matching engine.
    ///
    /// The automaton supports: Exact, Prefix, Shellstyle, Wildcard, AnythingBut, Exists,
    /// and EqualsIgnoreCase (with full Unicode case folding).
    ///
    /// Not fully supported (need runtime checking or have limitations):
    /// - NumericExact: automaton does string matching but event values may have different representations
    /// - Suffix, Numeric comparisons, Regex: not implemented in automaton
    pub fn is_automaton_compatible(&self) -> bool {
        match self {
            Matcher::Exact(_) => true,
            Matcher::Exists(_) => true,
            Matcher::Prefix(_) => true,
            Matcher::Shellstyle(_) => true,
            Matcher::Wildcard(_) => true,
            Matcher::AnythingBut(_) => true,
            // EqualsIgnoreCase: automaton supports full Unicode case folding
            Matcher::EqualsIgnoreCase(_) => true,
            // ParsedRegexp: uses our custom NFA integrated into automaton
            Matcher::ParsedRegexp(_) => true,
            // NumericExact: automaton matches on string representation, but event values
            // may have different representations (35 vs 35.0 vs 3.5e1)
            Matcher::NumericExact(_) => false,
            // Not supported by automaton
            Matcher::Suffix(_) => false,
            Matcher::Numeric(_) => false,
            Matcher::Regex(_) => false,
        }
    }
}

/// Context for tracking array positions during flattening
#[allow(dead_code)]
struct FlattenContext {
    array_count: i32,
    array_trail: Vec<ArrayPos>,
}

#[allow(dead_code)]
impl FlattenContext {
    fn new() -> Self {
        Self {
            array_count: 0,
            array_trail: Vec::new(),
        }
    }

    fn push_array(&mut self) -> i32 {
        let array_id = self.array_count;
        self.array_count += 1;
        self.array_trail.push(ArrayPos {
            array: array_id,
            pos: 0,
        });
        array_id
    }

    fn pop_array(&mut self) {
        self.array_trail.pop();
    }

    fn step_array(&mut self) {
        if let Some(last) = self.array_trail.last_mut() {
            last.pos += 1;
        }
    }

    fn current_trail(&self) -> Vec<ArrayPos> {
        self.array_trail.clone()
    }
}

/// Flatten a JSON event into fields with array position tracking.
/// e.g., {"a": {"b": 1}} -> [Field { path: "a.b", value: "1", array_trail: [] }]
#[allow(dead_code)]
pub fn flatten_event(json: &[u8]) -> Result<Vec<Field>, QuaminaError> {
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
    let mut ctx = FlattenContext::new();
    flatten_value_with_trail(&value, String::new(), &mut result, &mut ctx);
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
                        if let Value::Array(arr) = val {
                            // Reject empty array (matches Go behavior)
                            if arr.is_empty() {
                                return Err(QuaminaError::InvalidPattern(
                                    "anything-but array cannot be empty".into(),
                                ));
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
                                return Err(QuaminaError::InvalidPattern(
                                    "anything-but array must contain strings".into(),
                                ));
                            }
                            return Ok(Matcher::AnythingBut(excluded));
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "anything-but value must be an array".into(),
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
                                Ok(tree) => return Ok(Matcher::ParsedRegexp(tree)),
                                Err(_) => {
                                    // Fall back to regex crate for unsupported features
                                    match regex::Regex::new(s) {
                                        Ok(re) => return Ok(Matcher::Regex(re)),
                                        Err(e) => {
                                            return Err(QuaminaError::InvalidPattern(format!(
                                                "invalid regex: {}",
                                                e
                                            )))
                                        }
                                    }
                                }
                            }
                        }
                        return Err(QuaminaError::InvalidPattern(
                            "regex value must be a string".into(),
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

#[allow(dead_code)]
fn flatten_value_with_trail(
    value: &Value,
    path: String,
    result: &mut Vec<Field>,
    ctx: &mut FlattenContext,
) {
    match value {
        Value::Object(obj) => {
            // Snapshot the trail for this object's fields
            let trail_snapshot = ctx.current_trail();
            for (key, val) in obj {
                let new_path = if path.is_empty() {
                    key.clone()
                } else {
                    format!("{}.{}", path, key)
                };
                // For non-array children, pass context through
                // The snapshot is used when we add leaf fields
                flatten_value_with_trail_inner(val, new_path, result, ctx, &trail_snapshot);
            }
        }
        Value::Array(arr) => {
            // Enter the array: push new array position
            ctx.push_array();
            for (i, val) in arr.iter().enumerate() {
                if i > 0 {
                    ctx.step_array();
                }
                flatten_value_with_trail(val, path.clone(), result, ctx);
            }
            ctx.pop_array();
        }
        _ => {
            result.push(Field {
                path,
                value: value_to_string(value),
                array_trail: ctx.current_trail(),
                is_number: matches!(value, Value::Number(_)),
            });
        }
    }
}

#[allow(dead_code)]
fn flatten_value_with_trail_inner(
    value: &Value,
    path: String,
    result: &mut Vec<Field>,
    ctx: &mut FlattenContext,
    object_trail: &[ArrayPos],
) {
    match value {
        Value::Object(obj) => {
            for (key, val) in obj {
                let new_path = if path.is_empty() {
                    key.clone()
                } else {
                    format!("{}.{}", path, key)
                };
                flatten_value_with_trail_inner(val, new_path, result, ctx, object_trail);
            }
        }
        Value::Array(arr) => {
            // Enter the array: push new array position
            ctx.push_array();
            for (i, val) in arr.iter().enumerate() {
                if i > 0 {
                    ctx.step_array();
                }
                flatten_value_with_trail(val, path.clone(), result, ctx);
            }
            ctx.pop_array();
        }
        _ => {
            // For fields directly in objects, use the object's trail snapshot
            result.push(Field {
                path,
                value: value_to_string(value),
                array_trail: object_trail.to_vec(),
                is_number: matches!(value, Value::Number(_)),
            });
        }
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
