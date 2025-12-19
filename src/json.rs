//! Minimal JSON parser for flattening events and patterns

use crate::QuaminaError;
use std::collections::HashMap;

/// A matcher for a pattern field value
#[derive(Debug, Clone, PartialEq)]
pub enum Matcher {
    Exact(String),
    Exists(bool),
    Prefix(String),
    Suffix(String),
    Wildcard(String),
    AnythingBut(Vec<String>),
    EqualsIgnoreCase(String),
}

/// Flatten a JSON event into path/value pairs
/// e.g., {"a": {"b": 1}} -> [("a.b", "1")]
pub fn flatten_event(json: &[u8]) -> Result<Vec<(String, String)>, QuaminaError> {
    let s = std::str::from_utf8(json).map_err(|_| QuaminaError::InvalidUtf8)?;
    let mut parser = Parser::new(s);
    let value = parser.parse_value()?;

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
                            return Matcher::Wildcard(s.clone());
                        }
                    }
                    "anything-but" => {
                        if let Value::Array(arr) = val {
                            let excluded: Vec<String> = arr.iter().map(value_to_string).collect();
                            return Matcher::AnythingBut(excluded);
                        }
                    }
                    "equals-ignore-case" => {
                        if let Value::String(s) = val {
                            return Matcher::EqualsIgnoreCase(s.clone());
                        }
                    }
                    _ => {}
                }
            }
            Matcher::Exact(String::new()) // fallback
        }
        _ => Matcher::Exact(value_to_string(value)),
    }
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
            for (i, val) in arr.iter().enumerate() {
                flatten_value(val, format!("{}[{}]", path, i), result);
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
        let start = self.pos;
        while let Some(c) = self.peek() {
            if c == '"' {
                break;
            }
            if c == '\\' {
                self.advance();
            } // skip escaped char
            self.advance();
        }
        let s = self.input[start..self.pos].to_string();
        self.expect('"')?;
        Ok(s)
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
