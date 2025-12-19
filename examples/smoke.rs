//! Comprehensive smoke test for quamina-rs

use quamina::Quamina;

fn main() {
    println!("Running quamina-rs smoke tests...\n");

    test_exact_match();
    test_or_within_field();
    test_and_across_fields();
    test_exists_operator();
    test_prefix_suffix();
    test_wildcard();
    test_anything_but();
    test_equals_ignore_case();
    test_nested_objects();
    test_numeric();
    test_regex();
    test_delete_patterns();

    println!("\n✅ All smoke tests passed!");
}

fn test_exact_match() {
    let mut q = Quamina::new();
    q.add_pattern("test", r#"{"status": ["active"]}"#).unwrap();

    let matches = q
        .matches_for_event(r#"{"status": "active", "id": 1}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["test"]);

    let no_matches = q
        .matches_for_event(r#"{"status": "inactive"}"#.as_bytes())
        .unwrap();
    assert!(no_matches.is_empty());
    println!("✓ Exact match");
}

fn test_or_within_field() {
    let mut q = Quamina::new();
    q.add_pattern(
        "multi",
        r#"{"status": ["pending", "shipped", "delivered"]}"#,
    )
    .unwrap();

    for status in &["pending", "shipped", "delivered"] {
        let event = format!(r#"{{"status": "{}"}}"#, status);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert_eq!(matches, vec!["multi"]);
    }
    println!("✓ OR within field (multiple values)");
}

fn test_and_across_fields() {
    let mut q = Quamina::new();
    q.add_pattern("urgent", r#"{"priority": ["high"], "status": ["pending"]}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"priority": "high", "status": "pending"}"#.as_bytes())
        .unwrap();
    assert_eq!(matches, vec!["urgent"]);

    // Missing field = no match
    let no_match = q
        .matches_for_event(r#"{"priority": "high"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
    println!("✓ AND across fields");
}

fn test_exists_operator() {
    let mut q = Quamina::new();
    q.add_pattern("has-name", r#"{"name": [{"exists": true}]}"#)
        .unwrap();
    q.add_pattern("no-name", r#"{"name": [{"exists": false}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"name": "alice"}"#.as_bytes())
        .unwrap();
    assert!(m1.contains(&"has-name"));

    let m2 = q
        .matches_for_event(r#"{"other": "field"}"#.as_bytes())
        .unwrap();
    assert!(m2.contains(&"no-name"));
    println!("✓ Exists operator");
}

fn test_prefix_suffix() {
    let mut q = Quamina::new();
    q.add_pattern("prod", r#"{"env": [{"prefix": "prod-"}]}"#)
        .unwrap();
    q.add_pattern("jpg", r#"{"file": [{"suffix": ".jpg"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"env": "prod-us-east"}"#.as_bytes())
        .unwrap();
    assert!(m1.contains(&"prod"));

    let m2 = q
        .matches_for_event(r#"{"file": "photo.jpg"}"#.as_bytes())
        .unwrap();
    assert!(m2.contains(&"jpg"));
    println!("✓ Prefix and suffix operators");
}

fn test_wildcard() {
    let mut q = Quamina::new();
    q.add_pattern("txt", r#"{"file": [{"wildcard": "*.txt"}]}"#)
        .unwrap();
    q.add_pattern("error", r#"{"msg": [{"wildcard": "*error*"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"file": "document.txt"}"#.as_bytes())
        .unwrap();
    assert!(m1.contains(&"txt"));

    let m2 = q
        .matches_for_event(r#"{"msg": "an error occurred"}"#.as_bytes())
        .unwrap();
    assert!(m2.contains(&"error"));
    println!("✓ Wildcard operator");
}

fn test_anything_but() {
    let mut q = Quamina::new();
    q.add_pattern(
        "active",
        r#"{"status": [{"anything-but": ["deleted", "archived"]}]}"#,
    )
    .unwrap();

    let matches = q
        .matches_for_event(r#"{"status": "active"}"#.as_bytes())
        .unwrap();
    assert!(matches.contains(&"active"));

    let no_match = q
        .matches_for_event(r#"{"status": "deleted"}"#.as_bytes())
        .unwrap();
    assert!(no_match.is_empty());
    println!("✓ Anything-but operator");
}

fn test_equals_ignore_case() {
    let mut q = Quamina::new();
    q.add_pattern("test", r#"{"name": [{"equals-ignore-case": "Alice"}]}"#)
        .unwrap();

    for name in &["alice", "ALICE", "Alice", "aLiCe"] {
        let event = format!(r#"{{"name": "{}"}}"#, name);
        let matches = q.matches_for_event(event.as_bytes()).unwrap();
        assert!(matches.contains(&"test"));
    }
    println!("✓ Equals-ignore-case operator");
}

fn test_nested_objects() {
    let mut q = Quamina::new();
    q.add_pattern("admin", r#"{"user": {"role": ["admin"]}}"#)
        .unwrap();

    let matches = q
        .matches_for_event(r#"{"user": {"role": "admin", "name": "alice"}}"#.as_bytes())
        .unwrap();
    assert!(matches.contains(&"admin"));
    println!("✓ Nested object patterns");
}

fn test_numeric() {
    let mut q = Quamina::new();
    q.add_pattern("cheap", r#"{"price": [{"numeric": ["<", 100]}]}"#)
        .unwrap();
    q.add_pattern("mid", r#"{"price": [{"numeric": [">=", 100, "<", 500]}]}"#)
        .unwrap();

    let m1 = q.matches_for_event(r#"{"price": 50}"#.as_bytes()).unwrap();
    assert!(m1.contains(&"cheap"));

    let m2 = q.matches_for_event(r#"{"price": 250}"#.as_bytes()).unwrap();
    assert!(m2.contains(&"mid"));

    let m3 = q
        .matches_for_event(r#"{"price": 1000}"#.as_bytes())
        .unwrap();
    assert!(m3.is_empty());
    println!("✓ Numeric comparison operator");
}

fn test_regex() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"code": [{"regex": "^[A-Z]{3}-[0-9]+$"}]}"#)
        .unwrap();

    let m1 = q
        .matches_for_event(r#"{"code": "ABC-123"}"#.as_bytes())
        .unwrap();
    assert!(m1.contains(&"p1"));

    let m2 = q
        .matches_for_event(r#"{"code": "invalid"}"#.as_bytes())
        .unwrap();
    assert!(m2.is_empty());
    println!("✓ Regex operator");
}

fn test_delete_patterns() {
    let mut q = Quamina::new();
    q.add_pattern("p1", r#"{"x": ["1"]}"#).unwrap();
    q.add_pattern("p2", r#"{"x": ["2"]}"#).unwrap();

    // p1 matches
    let m1 = q.matches_for_event(r#"{"x": "1"}"#.as_bytes()).unwrap();
    assert!(m1.contains(&"p1"));

    // Delete p1
    q.delete_patterns(&"p1").unwrap();

    // p1 no longer matches
    let m2 = q.matches_for_event(r#"{"x": "1"}"#.as_bytes()).unwrap();
    assert!(m2.is_empty());
    println!("✓ Delete patterns");
}
