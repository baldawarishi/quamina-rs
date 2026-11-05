//! Depth probe: how deep patterns and events can nest before Quamina rejects
//! them or overflows a fixed-size stack, and what each depth costs.
//!
//! ```text
//! depth_probe <shape> <depth> [--stack BYTES] [--pattern-depth N]
//! depth_probe limit <shape> [--stack BYTES] [--pattern-depth N] [--max D]
//! depth_probe sweep [shape] [--depths D,D,..] [--stack BYTES] [--pattern-depth N]
//! ```
//!
//! Shapes: `regexp_groups`, `pattern_object`, `pattern_object_add`,
//! `event_used_array`, `event_unused_object`, `event_unused_array`. Times are
//! medians in microseconds; peaks are heap bytes above what was live when the
//! phase started.
//!
//! A stack overflow aborts the whole process, so `limit` and `sweep` re-run this
//! binary as a child for every case and classify its exit status.

use std::alloc::{GlobalAlloc, Layout, System};
use std::io::Write as _;
use std::process::{Command, ExitCode};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

use quamina::QuaminaBuilder;

const SHAPES: [&str; 6] = [
    "regexp_groups",
    "pattern_object",
    "pattern_object_add",
    "event_used_array",
    "event_unused_object",
    "event_unused_array",
];

/// Tokio's default worker stack, the smallest a caller is likely to use.
const DEFAULT_STACK: usize = 2 * 1024 * 1024;

/// Set by the parent so the child reports which phase it is in on stderr; the
/// last phase printed before an abort is the one that overflowed.
const CHILD_ENV: &str = "QUAMINA_DEPTH_PROBE_CHILD";

static LIVE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);

/// Live heap at which a case aborts. Some shapes grow with the square of the
/// depth, and a probe should report that rather than take the machine down.
const HEAP_CAP: usize = 1 << 30;
const HEAP_CAP_MESSAGE: &str = "depth_probe: heap cap exceeded";

struct PeakAlloc;

#[allow(
    unsafe_code,
    reason = "GlobalAlloc is an unsafe trait; every call forwards to System"
)]
unsafe impl GlobalAlloc for PeakAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let ptr = unsafe { System.alloc(layout) };
        if !ptr.is_null() {
            grow(layout.size());
        }
        ptr
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { System.dealloc(ptr, layout) };
        LIVE.fetch_sub(layout.size(), Ordering::Relaxed);
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let new = unsafe { System.realloc(ptr, layout, new_size) };
        if !new.is_null() {
            LIVE.fetch_sub(layout.size(), Ordering::Relaxed);
            grow(new_size);
        }
        new
    }
}

#[global_allocator]
static ALLOC: PeakAlloc = PeakAlloc;

fn grow(bytes: usize) {
    let live = LIVE.fetch_add(bytes, Ordering::Relaxed) + bytes;
    PEAK.fetch_max(live, Ordering::Relaxed);
    if live > HEAP_CAP {
        // Formatting would allocate and land back here, so write raw bytes.
        let _ = std::io::stderr().write_all(HEAP_CAP_MESSAGE.as_bytes());
        std::process::abort();
    }
}

/// Start a new peak window and return the baseline it is measured from.
fn reset_peak() -> usize {
    let live = LIVE.load(Ordering::Relaxed);
    PEAK.store(live, Ordering::Relaxed);
    live
}

fn peak_since(base: usize) -> usize {
    PEAK.load(Ordering::Relaxed).saturating_sub(base)
}

struct Opts {
    stack: usize,
    pattern_depth: Option<usize>,
    max: usize,
    depths: Option<Vec<usize>>,
}

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut positional = Vec::new();
    let mut opts = Opts {
        stack: DEFAULT_STACK,
        pattern_depth: None,
        max: 1_000_000,
        depths: None,
    };
    let mut it = args.iter();
    while let Some(arg) = it.next() {
        let mut value = || it.next().map_or_else(|| usage(), String::as_str);
        match arg.as_str() {
            "--stack" => opts.stack = parse_num(value()),
            "--pattern-depth" => opts.pattern_depth = Some(parse_num(value())),
            "--max" => opts.max = parse_num(value()),
            "--depths" => opts.depths = Some(value().split(',').map(parse_num).collect()),
            flag if flag.starts_with("--") => usage(),
            _ => positional.push(arg.as_str()),
        }
    }

    match positional.as_slice() {
        ["limit", shape] => limit(shape, &opts),
        ["sweep"] => sweep(&SHAPES, &opts),
        ["sweep", shape] => sweep(&[shape], &opts),
        [shape, depth] => run_case(shape, parse_num(depth), &opts),
        _ => usage(),
    }
}

fn usage() -> ! {
    eprintln!(
        "usage: depth_probe <shape> <depth> | limit <shape> | sweep [shape]\n\
         \x20      [--stack BYTES] [--pattern-depth N] [--max D] [--depths D,D,..]\n\
         shapes: {}",
        SHAPES.join(", ")
    );
    std::process::exit(64)
}

fn parse_num(s: &str) -> usize {
    s.parse().unwrap_or_else(|_| usage())
}

/// A pattern and event nested `depth` levels deep, and whether they match.
struct Case {
    pattern: String,
    event: String,
    should_match: bool,
}

fn build_case(shape: &str, depth: usize) -> Case {
    let nest = |open: &str, leaf: &str, close: &str| {
        format!("{}{leaf}{}", open.repeat(depth), close.repeat(depth))
    };
    let shallow = r#"{"f":["x"]}"#.to_owned();
    let (pattern, event, should_match) = match shape {
        "regexp_groups" => (
            format!(r#"{{"f":[{{"regexp":"{}"}}]}}"#, nest("(", "a", ")")),
            r#"{"f":"a"}"#.to_owned(),
            true,
        ),
        "pattern_object" => (
            nest(r#"{"a":"#, r#"["x"]"#, "}"),
            nest(r#"{"a":"#, r#""x""#, "}"),
            true,
        ),
        // A shallow event keeps the flattener out of it, so a failure here
        // belongs to the add path.
        "pattern_object_add" => (
            nest(r#"{"a":"#, r#"["x"]"#, "}"),
            r#"{"a":"x"}"#.to_owned(),
            depth == 1,
        ),
        // Arrays on a path the pattern uses are walked however shallow the
        // pattern is.
        "event_used_array" => (
            r#"{"a":["x"]}"#.to_owned(),
            format!(r#"{{"a":{}}}"#, nest("[", r#""x""#, "]")),
            true,
        ),
        // The deep value comes first, so the flattener has to skip all of it
        // before it reaches `f`.
        "event_unused_object" => (
            shallow,
            format!(r#"{{"g":{},"f":"x"}}"#, nest(r#"{"g":"#, "1", "}")),
            true,
        ),
        "event_unused_array" => (
            shallow,
            format!(r#"{{"g":{},"f":"x"}}"#, nest("[", "1", "]")),
            true,
        ),
        _ => usage(),
    };
    Case {
        pattern,
        event,
        should_match,
    }
}

/// Runs per phase. Deep cases take long enough that a few give a stable median.
const fn runs_for(depth: usize) -> usize {
    if depth <= 1024 { 21 } else { 5 }
}

fn stage(name: &str) {
    if std::env::var_os(CHILD_ENV).is_some() {
        eprintln!("stage={name}");
    }
}

#[derive(Default)]
struct Report {
    add: Duration,
    matched: Duration,
    add_peak: usize,
    match_peak: usize,
    outcome: String,
}

fn run_case(shape: &str, depth: usize, opts: &Opts) -> ExitCode {
    let case = build_case(shape, depth);
    let pattern_depth = opts.pattern_depth;
    let worker = std::thread::Builder::new()
        .stack_size(opts.stack)
        .spawn(move || probe(&case, pattern_depth, runs_for(depth)))
        .expect("spawn the fixed-stack worker thread");
    let report = worker.join().unwrap_or_else(|_| Report {
        outcome: "panic".to_owned(),
        ..Report::default()
    });

    println!(
        "shape={shape} depth={depth} stack={} add_us={:.1} match_us={:.1} add_peak={} match_peak={} outcome={}",
        opts.stack,
        report.add.as_secs_f64() * 1e6,
        report.matched.as_secs_f64() * 1e6,
        report.add_peak,
        report.match_peak,
        report.outcome,
    );
    if report.outcome == "ok" {
        ExitCode::SUCCESS
    } else {
        ExitCode::from(2)
    }
}

fn probe(case: &Case, pattern_depth: Option<usize>, runs: usize) -> Report {
    let mut report = Report::default();
    let mut times = Vec::with_capacity(runs);
    let mut q = None;
    for _ in 0..runs {
        if let Some(previous) = q.take() {
            stage("drop");
            drop(previous);
        }
        let builder = QuaminaBuilder::<&str>::new();
        let mut fresh = match pattern_depth {
            Some(depth) => builder.with_max_pattern_depth(depth),
            None => builder,
        }
        .build()
        .expect("a builder without a custom flattener always builds");

        stage("add");
        let base = reset_peak();
        let start = Instant::now();
        let added = fresh.add_pattern("p", &case.pattern);
        times.push(start.elapsed());
        report.add_peak = report.add_peak.max(peak_since(base));
        if let Err(e) = added {
            report.add = median(&mut times);
            report.outcome = format!("error@add: {}", truncate(&e.to_string()));
            return report;
        }
        q = Some(fresh);
    }
    report.add = median(&mut times);
    let q = q.expect("at least one run");

    // The first match also pays for the lazy freeze, which the peak keeps.
    stage("match");
    times.clear();
    report.outcome = "ok".to_owned();
    for _ in 0..runs {
        let base = reset_peak();
        let start = Instant::now();
        let result = q.matches_for_event(case.event.as_bytes());
        times.push(start.elapsed());
        report.match_peak = report.match_peak.max(peak_since(base));
        match result {
            Ok(m) if m == ["p"][..usize::from(case.should_match)] => {}
            Ok(m) => report.outcome = format!("wrong: {m:?}"),
            Err(e) => report.outcome = format!("error@match: {}", truncate(&e.to_string())),
        }
        if report.outcome != "ok" {
            break;
        }
    }
    report.matched = median(&mut times);

    stage("drop");
    drop(q);
    report
}

fn median(times: &mut [Duration]) -> Duration {
    times.sort_unstable();
    times[times.len() / 2]
}

fn truncate(msg: &str) -> String {
    msg.replace('\n', " ").chars().take(80).collect()
}

/// One case run in a child process: its report line, if it got that far, and
/// how it ended.
struct ChildRun {
    ok: bool,
    kind: String,
    line: Option<String>,
}

fn run_child(shape: &str, depth: usize, opts: &Opts) -> ChildRun {
    let exe = std::env::current_exe().expect("path to this binary");
    let mut cmd = Command::new(exe);
    cmd.args([
        shape,
        &depth.to_string(),
        "--stack",
        &opts.stack.to_string(),
    ]);
    if let Some(d) = opts.pattern_depth {
        cmd.args(["--pattern-depth", &d.to_string()]);
    }
    let out = cmd.env(CHILD_ENV, "1").output().expect("spawn child probe");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    let line = stdout
        .lines()
        .find(|l| l.starts_with("shape="))
        .map(str::to_owned);
    let stage = stderr
        .lines()
        .rev()
        .find_map(|l| l.strip_prefix("stage="))
        .unwrap_or("start");

    let kind = if stderr.contains("overflowed its stack") {
        format!("overflow@{stage}")
    } else if stderr.contains(HEAP_CAP_MESSAGE) {
        format!("heap_cap@{stage}")
    } else if let Some((_, outcome)) = line.as_deref().and_then(|l| l.split_once(" outcome=")) {
        outcome.to_owned()
    } else {
        format!("{}@{stage}", out.status)
    };
    ChildRun {
        ok: out.status.success(),
        kind,
        line,
    }
}

/// Double the depth until a case fails, then bisect between the last depth that
/// passed and the first that did not.
fn limit(shape: &str, opts: &Opts) -> ExitCode {
    let mut ok = 0;
    let mut fail: Option<(usize, String)> = None;
    loop {
        let depth = match &fail {
            None if ok >= opts.max => break,
            None => (ok * 2).clamp(1, opts.max),
            Some((bad, _)) if bad - ok <= 1 => break,
            Some((bad, _)) => ok + (bad - ok) / 2,
        };
        let run = run_child(shape, depth, opts);
        eprintln!("  {shape} depth={depth}: {}", run.kind);
        if run.ok {
            ok = depth;
        } else {
            fail = Some((depth, run.kind));
        }
    }

    let first_fail = fail.map_or_else(
        || format!("none (max {})", opts.max),
        |(bad, kind)| format!("{bad} kind={kind}"),
    );
    println!(
        "shape={shape} stack={} deepest_ok={ok} first_fail={first_fail}",
        opts.stack
    );
    ExitCode::SUCCESS
}

/// Depths inside the limits Quamina enforces today, so every case should pass.
fn default_depths(shape: &str) -> Vec<usize> {
    match shape {
        "regexp_groups" => vec![16, 64, 128],
        "pattern_object" | "pattern_object_add" => vec![16, 64, 256],
        _ => vec![16, 1024, 100_000],
    }
}

fn sweep(shapes: &[&str], opts: &Opts) -> ExitCode {
    for shape in shapes {
        let depths = opts.depths.clone().unwrap_or_else(|| default_depths(shape));
        for depth in depths {
            let run = run_child(shape, depth, opts);
            match run.line {
                Some(line) if run.ok => println!("{line}"),
                _ => println!("shape={shape} depth={depth} outcome={}", run.kind),
            }
        }
    }
    ExitCode::SUCCESS
}
