//! Profile shellstyle matching for flamegraph/samply analysis
//!
//! Run with: samply record cargo run --release --example profile_shellstyle
//! Or:       cargo flamegraph --example profile_shellstyle

use quamina::Quamina;

fn add_shellstyle_pattern(q: &mut Quamina, name: &str, shellstyle: &str) {
    q.add_pattern(
        name.to_string(),
        &format!(r#"{{"STREET": [{{"shellstyle": "{shellstyle}"}}]}}"#),
    )
    .unwrap();
}

fn load_patterns(q: &mut Quamina) {
    // 16 letter patterns (A* through P*)
    for letter in [
        "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P",
    ] {
        add_shellstyle_pattern(q, letter, &format!("{letter}*"));
    }

    // Multiple-wildcard patterns to exercise complex NFA traversal.
    for (name, shellstyle) in [
        ("funky1", "*E*E*E*"),
        ("funky2", "*A*B*"),
        ("funky3", "*N*P*"),
        ("funky4", "*O*O*O*"),
    ] {
        add_shellstyle_pattern(q, name, shellstyle);
    }

    for (name, shellstyle) in [
        ("jp1", "*東京*"),
        ("jp2", "新*"),
        ("cn1", "*北京*"),
        ("cn2", "上海*"),
        ("kr1", "*서울*"),
    ] {
        add_shellstyle_pattern(q, name, shellstyle);
    }

    for (name, shellstyle) in [
        ("emoji1", "*🎉*"),
        ("emoji2", "🚀*"),
        ("emoji3", "*❤️*"),
        ("emoji4", "*🌟*🎯*"),
    ] {
        add_shellstyle_pattern(q, name, shellstyle);
    }
}

fn sample_events() -> Vec<Vec<u8>> {
    vec![
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
    ]
}

fn main() {
    let mut q = Quamina::new();
    load_patterns(&mut q);
    let events = sample_events();

    // Run many iterations for good profiling data
    let iterations: u128 = 100_000;
    let start = std::time::Instant::now();

    for _ in 0..iterations {
        for event in &events {
            let _ = std::hint::black_box(q.matches_for_event(std::hint::black_box(event)));
        }
    }

    let elapsed = start.elapsed();
    let events_count = u128::try_from(events.len()).expect("events.len() fits in u128");
    let total_ops = iterations * events_count;
    let ns_per_op = elapsed.as_nanos() / total_ops;
    // µs/iter with one decimal of resolution, computed in integer space.
    let tenths_us_per_iter = elapsed.as_nanos() * 10 / iterations / 1_000;
    eprintln!(
        "{} iterations × {} events = {} ops in {:.2?} ({} ns/op, {}.{} µs/iter)",
        iterations,
        events.len(),
        total_ops,
        elapsed,
        ns_per_op,
        tenths_us_per_iter / 10,
        tenths_us_per_iter % 10,
    );
}
