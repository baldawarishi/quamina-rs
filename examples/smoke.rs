//! Smoke test for quamina-rs - validates Milestone 0.5

use quamina::Quamina;

fn main() {
    println!("Running quamina-rs smoke test...\n");

    let mut q = Quamina::new();

    // Add a simple pattern
    q.add_pattern("test", r#"{"status": ["active"]}"#)
        .expect("Failed to add pattern");
    println!("✓ Added pattern 'test' for status=active");

    // Test matching event
    let matches = q
        .matches_for_event(r#"{"status": "active", "id": 1}"#.as_bytes())
        .expect("Failed to match event");
    assert_eq!(matches, vec!["test"], "Should match 'test' pattern");
    println!("✓ Event with status=active matches pattern");

    // Test non-matching event
    let no_matches = q
        .matches_for_event(r#"{"status": "inactive"}"#.as_bytes())
        .expect("Failed to match event");
    assert!(no_matches.is_empty(), "Should not match any patterns");
    println!("✓ Event with status=inactive does not match");

    // Test multiple values (OR within field)
    let mut q2 = Quamina::new();
    q2.add_pattern("multi", r#"{"status": ["pending", "shipped"]}"#)
        .expect("Failed to add pattern");

    let m1 = q2
        .matches_for_event(r#"{"status": "pending"}"#.as_bytes())
        .unwrap();
    assert_eq!(m1, vec!["multi"]);
    println!("✓ status=pending matches [pending, shipped]");

    let m2 = q2
        .matches_for_event(r#"{"status": "shipped"}"#.as_bytes())
        .unwrap();
    assert_eq!(m2, vec!["multi"]);
    println!("✓ status=shipped matches [pending, shipped]");

    let m3 = q2
        .matches_for_event(r#"{"status": "delivered"}"#.as_bytes())
        .unwrap();
    assert!(m3.is_empty());
    println!("✓ status=delivered does not match [pending, shipped]");

    // Test multiple fields (AND across fields)
    let mut q3 = Quamina::new();
    q3.add_pattern("urgent", r#"{"priority": ["high"], "status": ["pending"]}"#)
        .expect("Failed to add pattern");

    let m4 = q3
        .matches_for_event(r#"{"priority": "high", "status": "pending"}"#.as_bytes())
        .unwrap();
    assert_eq!(m4, vec!["urgent"]);
    println!("✓ priority=high AND status=pending matches");

    let m5 = q3
        .matches_for_event(r#"{"priority": "high", "status": "shipped"}"#.as_bytes())
        .unwrap();
    assert!(m5.is_empty());
    println!("✓ priority=high AND status=shipped does not match");

    println!("\n✓ All smoke tests passed!");
}
