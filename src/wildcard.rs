//! Wildcard and shellstyle pattern matching

/// Shellstyle matching: simple wildcard where * matches any sequence (no escaping)
pub(crate) fn shellstyle_match(pattern: &str, text: &str) -> bool {
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
pub(crate) fn wildcard_match(pattern: &str, text: &str) -> bool {
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
    fn test_shellstyle_basic() {
        assert!(shellstyle_match("hello", "hello"));
        assert!(!shellstyle_match("hello", "world"));
    }

    #[test]
    fn test_shellstyle_star() {
        assert!(shellstyle_match("*", "anything"));
        assert!(shellstyle_match("*", ""));
        assert!(shellstyle_match("hello*", "hello world"));
        assert!(shellstyle_match("*world", "hello world"));
        assert!(shellstyle_match("hello*world", "hello big world"));
    }

    #[test]
    fn test_wildcard_basic() {
        assert!(wildcard_match("hello", "hello"));
        assert!(!wildcard_match("hello", "world"));
    }

    #[test]
    fn test_wildcard_star() {
        assert!(wildcard_match("*", "anything"));
        assert!(wildcard_match("*", ""));
        assert!(wildcard_match("hello*", "hello world"));
        assert!(wildcard_match("*world", "hello world"));
    }

    #[test]
    fn test_wildcard_escape() {
        assert!(wildcard_match(r"\*", "*"));
        assert!(wildcard_match(r"\\", "\\"));
        assert!(wildcard_match(r"a\*b", "a*b"));
        assert!(!wildcard_match(r"\*", "anything"));
    }

    #[test]
    fn test_parse_wildcard_pattern() {
        assert_eq!(
            parse_wildcard_pattern("hello"),
            vec![WildcardSegment::Literal("hello".to_string())]
        );
        assert_eq!(parse_wildcard_pattern("*"), vec![WildcardSegment::Star]);
        assert_eq!(
            parse_wildcard_pattern("a*b"),
            vec![
                WildcardSegment::Literal("a".to_string()),
                WildcardSegment::Star,
                WildcardSegment::Literal("b".to_string())
            ]
        );
        assert_eq!(
            parse_wildcard_pattern(r"\*"),
            vec![WildcardSegment::Literal("*".to_string())]
        );
    }
}
