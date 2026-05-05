//! Shared test helper macros and functions to reduce duplication across test modules.

/// Adapter the test macros run an event through on the way into
/// `matches_for_event`. We want tests to keep writing events as raw string
/// literals like `r#"{"x": 1}"#`, but calling `.as_bytes()` directly inside
/// a macro upsets clippy, and the byte-string literal alternative ruins the
/// readability of every test. Bouncing through this fn sidesteps the warning
/// without forcing every test to learn to spell `br#"..."#`.
#[doc(hidden)]
#[inline]
#[must_use]
pub fn event_bytes<E: AsRef<[u8]> + ?Sized>(event: &E) -> &[u8] {
    event.as_ref()
}

/// Create a `Quamina` matcher pre-loaded with one or more patterns.
///
/// ```ignore
/// let q = q!("p1" => r#"{"x": ["a"]}"#, "p2" => r#"{"y": [1]}"#);
/// ```
macro_rules! q {
    ($($name:expr => $pattern:expr),+ $(,)?) => {{
        let mut q = $crate::Quamina::new();
        $( q.add_pattern($name, $pattern).unwrap(); )+
        q
    }};
}

/// Assert that matching an event yields exactly the expected pattern IDs.
///
/// ```ignore
/// assert_matches!(q, r#"{"x": "a"}"#, vec!["p1"]);
/// assert_matches!(q, r#"{"x": "a"}"#, vec!["p1"], "custom message");
/// ```
macro_rules! assert_matches {
    ($q:expr, $event:expr, $expected:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert_eq!(matches, $expected, "Event: {}", $event);
    }};
    ($q:expr, $event:expr, $expected:expr, $msg:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert_eq!(matches, $expected, "{} (event: {})", $msg, $event);
    }};
}

/// Assert that matching an event yields no results.
///
/// ```ignore
/// assert_no_match!(q, r#"{"x": "z"}"#);
/// assert_no_match!(q, r#"{"x": "z"}"#, "should not match z");
/// ```
macro_rules! assert_no_match {
    ($q:expr, $event:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert!(matches.is_empty(), "Expected no match for: {}", $event);
    }};
    ($q:expr, $event:expr, $msg:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert!(matches.is_empty(), "{} (event: {})", $msg, $event);
    }};
}

/// Assert that matching an event returns results containing a specific pattern ID.
///
/// ```ignore
/// assert_has_match!(q, r#"{"x": "a"}"#, "p1");
/// ```
macro_rules! assert_has_match {
    ($q:expr, $event:expr, $name:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert!(
            matches.contains(&$name),
            "Expected {:?} in matches for {}, got {:?}",
            $name,
            $event,
            matches
        );
    }};
    ($q:expr, $event:expr, $name:expr, $msg:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert!(
            matches.contains(&$name),
            "{}: expected {:?} in matches for {}, got {:?}",
            $msg,
            $name,
            $event,
            matches
        );
    }};
}

/// Assert that matching an event does NOT contain a specific pattern ID.
///
/// ```ignore
/// assert_no_has_match!(q, r#"{"x": "z"}"#, "p1");
/// ```
macro_rules! assert_no_has_match {
    ($q:expr, $event:expr, $name:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert!(
            !matches.contains(&$name),
            "Expected {:?} NOT in matches for {}, got {:?}",
            $name,
            $event,
            matches
        );
    }};
}

/// Assert that matching an event returns exactly N results.
///
/// ```ignore
/// assert_match_count!(q, r#"{"x": "a"}"#, 2);
/// ```
macro_rules! assert_match_count {
    ($q:expr, $event:expr, $count:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert_eq!(
            matches.len(),
            $count,
            "Expected {} matches for {}, got {:?}",
            $count,
            $event,
            matches
        );
    }};
    ($q:expr, $event:expr, $count:expr, $msg:expr) => {{
        let matches = $q
            .matches_for_event($crate::test_helpers::event_bytes(&$event))
            .unwrap();
        assert_eq!(
            matches.len(),
            $count,
            "{}: expected {} matches for {}, got {:?}",
            $msg,
            $count,
            $event,
            matches
        );
    }};
}

/// Assert that `add_pattern` succeeds or fails, keeping test lines compact.
///
/// ```ignore
/// assert_add_ok!(q, "p1", r#"{"x": [1]}"#);
/// assert_add_err!(q, "p1", "not valid json");
/// ```
macro_rules! assert_add_ok {
    ($q:expr, $name:expr, $pattern:expr) => {
        assert!(
            $q.add_pattern($name, $pattern).is_ok(),
            "add_pattern({}, ...) should succeed",
            $name
        );
    };
}

macro_rules! assert_add_err {
    ($q:expr, $name:expr, $pattern:expr) => {
        assert!(
            $q.add_pattern($name, $pattern).is_err(),
            "add_pattern({}, ...) should fail",
            $name
        );
    };
}

/// Helper for wildcard pattern tests — tests a single pattern against match/no-match lists.
pub fn exercise_wildcard(pattern: &str, should_match: &[&str], should_not_match: &[&str]) {
    let mut q = crate::Quamina::new();
    let full_pattern = format!(r#"{{"x": [{{"wildcard": "{pattern}"}}]}}"#);
    q.add_pattern(pattern, &full_pattern)
        .unwrap_or_else(|_| panic!("Pattern should be valid: {pattern}"));

    for text in should_match {
        let event = format!(r#"{{"x": "{text}"}}"#);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            matches.contains(&pattern),
            "Pattern '{pattern}' should match '{text}', got {matches:?}"
        );
    }

    for text in should_not_match {
        let event = format!(r#"{{"x": "{text}"}}"#);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(
            !matches.contains(&pattern),
            "Pattern '{pattern}' should NOT match '{text}'"
        );
    }
}
