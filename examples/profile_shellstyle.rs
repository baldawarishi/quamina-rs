//! Profile shellstyle matching for flamegraph/samply analysis
//!
//! Run with: samply record cargo run --release --example profile_shellstyle
//! Or:       cargo flamegraph --example profile_shellstyle

use quamina::Quamina;

fn main() {
    let mut q = Quamina::new();

    // Add 16 letter patterns (A* through P*)
    for letter in [
        "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
    ] {
        q.add_pattern(
            letter.to_string(),
            &format!(r#"{{"STREET": [{{"shellstyle": "{}*"}}]}}"#, letter),
        )
        .unwrap();
    }

    // Add funky patterns with multiple wildcards (trigger complex NFA traversal)
    let funky_patterns = [
        ("funky1", "*E*E*E*"),
        ("funky2", "*A*B*"),
        ("funky3", "*N*P*"),
        ("funky4", "*O*O*O*"),
    ];
    for (name, shellstyle) in funky_patterns {
        q.add_pattern(
            name.to_string(),
            &format!(r#"{{"STREET": [{{"shellstyle": "{}"}}]}}"#, shellstyle),
        )
        .unwrap();
    }

    // Add CJK patterns
    let cjk_patterns = [
        ("jp1", "*東京*"),
        ("jp2", "新*"),
        ("cn1", "*北京*"),
        ("cn2", "上海*"),
        ("kr1", "*서울*"),
    ];
    for (name, shellstyle) in cjk_patterns {
        q.add_pattern(
            name.to_string(),
            &format!(r#"{{"STREET": [{{"shellstyle": "{}"}}]}}"#, shellstyle),
        )
        .unwrap();
    }

    // Add emoji patterns
    let emoji_patterns = [
        ("emoji1", "*🎉*"),
        ("emoji2", "🚀*"),
        ("emoji3", "*❤️*"),
        ("emoji4", "*🌟*🎯*"),
    ];
    for (name, shellstyle) in emoji_patterns {
        q.add_pattern(
            name.to_string(),
            &format!(r#"{{"STREET": [{{"shellstyle": "{}"}}]}}"#, shellstyle),
        )
        .unwrap();
    }

    // Events that will match and require NFA traversal
    let events: Vec<Vec<u8>> = vec![
        r#"{"STREET": "ASHBURY"}"#.into(),
        r#"{"STREET": "BELVEDERE"}"#.into(),
        r#"{"STREET": "CRANLEIGH"}"#.into(),
        r#"{"STREET": "DEER PARK"}"#.into(),
        r#"{"STREET": "EMBARCADERO"}"#.into(),
        r#"{"STREET": "FULTON"}"#.into(),
        r#"{"STREET": "GEARY"}"#.into(),
        r#"{"STREET": "HAIGHT"}"#.into(),
        r#"{"STREET": "IRVING"}"#.into(),
        r#"{"STREET": "JUDAH"}"#.into(),
        r#"{"STREET": "KEARNY"}"#.into(),
        r#"{"STREET": "LOMBARD"}"#.into(),
        r#"{"STREET": "MARKET"}"#.into(),
        r#"{"STREET": "NORIEGA"}"#.into(),
        r#"{"STREET": "OCTAVIA"}"#.into(),
        r#"{"STREET": "POLK"}"#.into(),
        r#"{"STREET": "EMBARCADERO STREET"}"#.into(),
        r#"{"STREET": "ALABAMA"}"#.into(),
        r#"{"STREET": "NAPOLEON"}"#.into(),
        r#"{"STREET": "COLORADO"}"#.into(),
        r#"{"STREET": "東京タワー通り"}"#.into(),
        r#"{"STREET": "新宿駅前"}"#.into(),
        r#"{"STREET": "北京路"}"#.into(),
        r#"{"STREET": "上海南京路"}"#.into(),
        r#"{"STREET": "서울대로"}"#.into(),
        r#"{"STREET": "Party Street 🎉"}"#.into(),
        r#"{"STREET": "🚀 Rocket Road"}"#.into(),
        r#"{"STREET": "Love ❤️ Lane"}"#.into(),
        r#"{"STREET": "Star 🌟 Plaza 🎯"}"#.into(),
        r#"{"STREET": "Tokyo 東京 Street"}"#.into(),
        r#"{"STREET": "Happy 😊 Avenue"}"#.into(),
    ];

    // Run many iterations for good profiling data
    let iterations = 100_000;
    let start = std::time::Instant::now();

    for _ in 0..iterations {
        for event in &events {
            let _ = std::hint::black_box(q.matches_for_event(std::hint::black_box(event)));
        }
    }

    let elapsed = start.elapsed();
    let total_ops = iterations * events.len();
    let ns_per_op = elapsed.as_nanos() / total_ops as u128;
    eprintln!(
        "{} iterations × {} events = {} ops in {:.2?} ({} ns/op, {:.1} µs/iter)",
        iterations,
        events.len(),
        total_ops,
        elapsed,
        ns_per_op,
        elapsed.as_micros() as f64 / iterations as f64,
    );
}
