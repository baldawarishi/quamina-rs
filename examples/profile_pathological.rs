//! Profile pathological epsilon closure matching for samply analysis
//!
//! Run with: samply record cargo run --release --example profile_pathological

use quamina::Quamina;

fn main() {
    let mut q = Quamina::new();

    let shell_patterns = [
        "*a*b*c*", "*x*y*z*", "*e*f*g*", "*m*n*o*", "*p*q*r*", "*s*t*u*", "*a*e*i*", "*b*d*f*",
        "*c*g*k*", "*d*h*l*", "*i*o*u*", "*r*s*t*",
    ];
    for (i, ss) in shell_patterns.iter().enumerate() {
        q.add_pattern(
            format!("shell{i}"),
            &format!(r#"{{"val": [{{"shellstyle": "{ss}"}}]}}"#),
        )
        .unwrap();
    }

    let regex_patterns = [
        "(([abc]?)*)+",
        "([abc]+)*d",
        "(a*)*b",
        "([xyz]?)*end",
        "(([mno]?)*)+",
        "([pqr]+)*s",
    ];
    for (i, re) in regex_patterns.iter().enumerate() {
        q.add_pattern(
            format!("re{i}"),
            &format!(r#"{{"val": [{{"regexp": "{re}"}}]}}"#),
        )
        .unwrap();
    }

    let events: Vec<Vec<u8>> = vec![
        r#"{"val": "abcxyz"}"#.into(),
        r#"{"val": "mnopqr"}"#.into(),
        r#"{"val": "aeiou"}"#.into(),
        r#"{"val": "rstuvwxyz"}"#.into(),
        r#"{"val": "abcdefghijklmno"}"#.into(),
        r#"{"val": "xyzend"}"#.into(),
        r#"{"val": "abcabcabcd"}"#.into(),
        r#"{"val": "aaaaaab"}"#.into(),
    ];

    let iterations = 500_000;
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
        "{} iterations x {} events = {} ops in {:.2?} ({} ns/op, {:.1} us/iter)",
        iterations,
        events.len(),
        total_ops,
        elapsed,
        ns_per_op,
        elapsed.as_micros() as f64 / iterations as f64,
    );
}
