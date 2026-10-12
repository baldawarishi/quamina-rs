# Quamina: JSON event filtering for Rust

[![CI](https://github.com/baldawarishi/quamina-rs/actions/workflows/test.yml/badge.svg)](https://github.com/baldawarishi/quamina-rs/actions/workflows/test.yml)
[![Crates.io](https://img.shields.io/crates/v/quamina.svg)](https://crates.io/crates/quamina)
[![Documentation](https://docs.rs/quamina/badge.svg)](https://docs.rs/quamina)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

Quamina filters and routes JSON events in Rust. It matches each event against many patterns in microseconds. Use it in high-throughput cloud services and event streams.

Add JSON patterns to a matcher, then pass it an event. Quamina returns the ID of every pattern that matches.

Quamina compiles the patterns into shared finite automata. Matching time changes little as you add patterns. The benchmarks below include matches that complete in tens or hundreds of nanoseconds.

Quamina runs inside your application, with no service, network request, async runtime, or code generation.

Use the [playground](https://baldawarishi.github.io/quamina-rs/) to test patterns and events in your browser.

## Contents

- [What Quamina does](#what-quamina-does)
- [Quick start](#quick-start)
- [Pattern language](#pattern-language)
- [Choosing a Rust rule engine](#choosing-a-rust-rule-engine)
- [APIs](#apis)
- [Concurrency](#concurrency)
- [Performance](#performance)
- [How it works](#how-it-works)
- [Kani formal verification](#kani-formal-verification)
- [Limitations](#limitations)
- [Credits](#credits)

## What Quamina does

- Matches one JSON event against many patterns.
- Returns all matching pattern IDs.
- Supports exact values, prefixes, suffixes, wildcards, regular expressions, numeric ranges, and IP address ranges.
- Shares matching work across patterns.
- Runs matches in parallel when threads share a `Quamina` instance.
- Limits pattern depth, field count, state count, and memory use.

## Quick start

```bash
cargo add quamina
```

```rust
use quamina::Quamina;

fn main() -> Result<(), quamina::QuaminaError> {
    let mut q = Quamina::new();

    q.add_pattern("p1", r#"{"status": ["error"]}"#)?;
    q.add_pattern("p2", r#"{"level": [1, 2, 3]}"#)?;

    let event = br#"{"status": "error", "level": 2}"#;
    let matches = q.matches_for_event(event)?;
    assert!(matches.contains(&"p1") && matches.contains(&"p2"));

    Ok(())
}
```

## Pattern language

A pattern is a JSON object. Its structure follows the event that it matches.

- Every field in the pattern must match.
- A field value is an array. At least one item in the array must match.
- Quamina ignores event fields that are not in the pattern.

For example, consider this event:

```json
{
  "source": "test.app",
  "detail": {
    "status": "error",
    "code": 500
  },
  "tags": ["urgent", "backend"]
}
```

Each of these patterns matches the event:

```json
{"source": ["test.app"]}
```

```json
{"detail": {"status": ["error", "warning"]}}
```

```json
{"tags": ["urgent"]}
```

```json
{"detail": {"code": [{"numeric": [">=", 400]}]}}
```

```json
{"source": [{"prefix": "test."}]}
```

```json
{"source": [{"suffix": ".app"}]}
```

```json
{"source": [{"wildcard": "*.app"}]}
```

```json
{"detail": {"status": [{"exists": true}]}}
```

```json
{"detail": {"status": [{"anything-but": ["ok", "pending"]}]}}
```

```json
{"detail": {"status": [{"equals-ignore-case": "ERROR"}]}}
```

```json
{"source": [{"regexp": "test~.[a-z]+"}]}
```

### Pattern types

#### Exact values

Match a value exactly:

```json
{"status": ["active"]}
{"count": [100]}
{"enabled": [true]}
{"deleted": [null]}
```

#### Prefixes and suffixes

Match the start or end of a string:

```json
{"url": [{"prefix": "https://"}]}
{"file": [{"suffix": ".json"}]}
```

#### Wildcards

Use `*` to match any sequence of characters:

```json
{"message": [{"wildcard": "*error*"}]}
{"id": [{"wildcard": "user-*-prod"}]}
```

Use `\*` to match an asterisk. Use `\\` to match a backslash.

The legacy `shellstyle` matcher does not support these escape sequences. Use `wildcard` for glob-style matching or `regexp` for regular expressions. The `shellstyle` matcher might be removed in a future release.

#### Field presence

Match based on whether a field exists:

```json
{"email": [{"exists": true}]}
{"deleted_at": [{"exists": false}]}
```

#### Excluded values

Match unless the field contains one of the listed values:

```json
{"status": [{"anything-but": ["pending", "cancelled"]}]}
{"code": [{"anything-but": [400, 404, 500]}]}
```

#### Case-insensitive values

Match a string without comparing letter case:

```json
{"level": [{"equals-ignore-case": "ERROR"}]}
```

#### Numeric ranges

Compare numbers or define a range:

```json
{"price": [{"numeric": [">", 100]}]}
{"age": [{"numeric": [">=", 18, "<", 65]}]}
```

#### IP address ranges

Match IPv4 or IPv6 addresses with Classless Inter-Domain Routing (CIDR) notation:

```json
{"ip": [{"cidr": "10.0.0.0/8"}]}
{"ip": [{"cidr": "2001:db8::/32"}]}
```

#### Regular expressions

Match strings with I-Regexp syntax from RFC 9485:

```json
{"email": [{"regexp": "[a-z]+@[a-z]+\\.[a-z]+"}]}
{"code": [{"regexp": "[A-Z]{3}-[0-9]{4}"}]}
```

Regular expressions use `~` as the escape character. Use `~d` for digits, `~p{L}` for Unicode letters, and `~b` or `~B` for word boundaries. Word boundaries can make some patterns slower.

## Choosing a Rust rule engine

These Rust projects solve related problems. The table compares their scope. It does not compare their performance.

| Project | Rules and input | Main capabilities | Use it for |
|---|---|---|---|
| **Quamina** | JSON patterns and JSON event bytes | Matches many patterns in a shared automaton; returns every matching ID | Filtering and routing event streams with many patterns |
| [`gene`](https://docs.rs/gene/latest/gene/) | YAML rules and Rust event types | Supports comparisons, regular expressions, bitwise operations, dependencies, and templates | Detecting security events and adding rule metadata |
| [`json_rules_engine`](https://docs.rs/json-rules-engine/latest/json_rules_engine/) | Rust condition trees and serializable facts | Supports logical conditions, equality, membership, string checks, and numeric ranges | Building application rules with condition trees and actions |
| [`cel-interpreter`](https://docs.rs/cel-interpreter/latest/cel_interpreter/) | Common Expression Language programs and Rust values | Supports expressions, collections, functions, time operations, and optional regular expressions | Evaluating portable business expressions and validation rules |
| [`regorus`](https://docs.rs/regorus/latest/regorus/) | Rego policies and JSON-like values | Supports policy logic, data joins, comprehensions, built-in functions, and extensions | Building authorization and policy-as-code systems |

Choose Quamina when you need to find every pattern that matches a JSON event. Choose a general expression or policy engine when rules need calculations, functions, data joins, or detailed decision results.

## APIs

### Create and configure a matcher

```rust
use quamina::{Quamina, QuaminaBuilder};

// Simple
let q = Quamina::<String>::new();

// With options
let q = QuaminaBuilder::<String>::new()
    .with_media_type("application/json")?
    .with_auto_rebuild(true)
    .build()?;

// With custom ID type
let q = Quamina::<u64>::new();

// With custom pattern complexity limits
let q = QuaminaBuilder::<String>::new()
    .with_max_pattern_depth(128)
    .with_max_fields_per_pattern(64)
    .with_arena_byte_budget(5 * 1024 * 1024)
    .with_max_states_per_pattern(512)
    .build()?;
```

### Add and remove patterns

```rust
q.add_pattern("my-rule", r#"{"x": [1]}"#)?;
q.delete_patterns(&"my-rule")?;
q.clear();
```

### Match an event

```rust
let matches = q.matches_for_event(event)?;  // Vec of matching IDs
let matched = q.has_matches(event)?;         // bool
let count   = q.count_matches(event)?;       // number of matches
```

### Handle errors

`add_pattern` returns an error when a pattern contains malformed JSON, uses invalid syntax, or exceeds a configured complexity limit. `matches_for_event` returns an error when the event is not valid JSON.

```rust
match q.add_pattern("bad", r#"{"x": "not-an-array"}"#) {
    Err(QuaminaError::InvalidPattern(msg)) => println!("{}", msg),
    Err(QuaminaError::PatternTooComplex(msg)) => println!("{}", msg),
    _ => {}
}
```

## Concurrency

Share one `Quamina` instance across threads with `Arc`. Each thread uses its own matching buffers. Threads can call `matches_for_event()` at the same time without contending for a global match lock.

`add_pattern` needs mutable access. If several threads change patterns, protect the instance with a lock:

```rust
let q = Arc::new(RwLock::new(Quamina::new()));
```

`clone()` rebuilds the automaton from its stored patterns. Cloning can take time when the matcher contains many patterns.

## Performance

Quamina compiles all patterns into one automaton. Matching time grows slowly as the pattern count increases.

### Pattern count scaling

On an M4 Max:

| Patterns | Match time |
|----------|-----------|
| 100 | ~110 ns |
| 10,000 | ~90 ns |

Matching cost stays nearly flat in this benchmark. Small differences can reflect measurement noise.

### Event benchmarks

| Benchmark | Time | Description |
|-----------|-----:|-------------|
| citylots | ~1,400 ns | 4 patterns, 206 KB of GeoJSON |
| nested field match | ~4,100 ns | 9 KB JSON, deeply nested field |
| early field exit | ~170 ns | 9 KB JSON, matching field near the top |

### Pattern type benchmarks

Measured in `BuiltForSpeed` mode:

| Benchmark | Time | Description |
|---|---:|---|
| exact_match | ~56 ns | Single exact match |
| nested_match | ~82 ns | Exact match on a nested key |
| regex_match | ~49 ns | Simple regex on an email value |
| anything_but_match | ~69 ns | `anything-but` with 3 excluded values |
| numeric_range_two_sided | ~74 ns | Two-sided range (`>= 0, < 100`) |
| 100_prefix_patterns | ~124 ns | 100 `prefix` patterns merged into one automaton |
| shellstyle_26_patterns | ~89 ns | 26 shellstyle patterns (A\*–Z\*) |
| regexp_plus_long | ~265 ns | `[a-z]+` on a 100-char value |

### What affects performance

- Each unique field path adds work for every event.
- Large JSON events take longer to parse and flatten.
- Regular expressions with Unicode categories, such as `~p{L}`, take longer to compile.
- `BuiltForSpeed` takes longer to add patterns but matches wildcard and regular expression patterns faster.

### Choose a build mode

Quamina compiles wildcard and regular expression patterns into nondeterministic finite automata (NFAs). The default `BuiltForComfort` mode keeps the NFAs. It adds patterns quickly and uses less memory. Matching gets slower as you add wildcard and regular expression patterns.

`BuiltForSpeed` tries to convert the NFAs into deterministic finite automata (DFAs). It takes longer to add patterns but reduces the effect of pattern count on matching time.

The number of DFA states can grow exponentially, up to O(2ⁿ). If conversion exceeds the limit, eligible automata use a limited DFA cache that learns from real events. Other automata continue to use their NFAs. Use `matcher_stats()` to track matcher size.

```rust
use quamina::MatcherBuildMode;

let mut q = Quamina::<String>::new();
q.set_matcher_build_mode(MatcherBuildMode::BuiltForSpeed);
assert_eq!(q.matcher_build_mode(), MatcherBuildMode::BuiltForSpeed);
```

### Running benchmarks

```bash
cargo bench --bench matching              # all benchmarks
cargo bench --bench matching -- citylots  # specific benchmark
```

## How it works

Quamina focuses on one task: match an event against many patterns and return all matching IDs.

1. Quamina compiles each field matcher into a finite automaton. It shares common paths and transitions across patterns.
2. The JSON parser skips parts of the event that no pattern uses.
3. Field values move through the automata. Quamina collects an ID when all fields in that pattern match.

Quamina uses these techniques on the matching path:

- **Q-number ranges:** Quamina converts JSON numbers into bytes that keep the same numeric order. It can then match numeric ranges with the same automata that match strings. The temporary value stays in a fixed stack buffer and does not allocate heap memory.
- **Lazy DFA cache:** `BuiltForSpeed` converts small NFAs into DFAs before matching. For some larger pattern sets, Quamina creates DFA states only when events use them. Quamina limits the cache size and keeps an NFA fallback.
- **Immutable snapshots:** Matching threads share a read-only automaton. Each thread uses its own work buffers. Pattern changes create the next snapshot outside the matching path.

## Kani formal verification

The test suite includes unit, stress, differential, fuzz, and Miri tests. Quamina also runs [Kani proof harnesses](src/kani_proofs.rs) in [continuous integration](.github/workflows/test.yml). Kani checks these bounded properties:

- The Q-number conversion preserves the order of finite `f64` values.
- The stack encoding fits in its fixed buffer.
- The Unicode case-folding lookup table stays in order.
- Byte lookups in the compact transition table select the correct state.

These proofs cover specific invariants. They do not verify the entire crate or every possible rule.

## Limitations

Quamina limits pattern complexity to control memory use and processing time:

| Limit | Default | Builder method |
|-------|---------|----------------|
| Max nesting depth | 256 | `with_max_pattern_depth` |
| Max fields per pattern | 256 | `with_max_fields_per_pattern` |
| Arena byte budget | 10 MB | `with_arena_byte_budget` |
| Max states per pattern | 1024 | `with_max_states_per_pattern` |

Quamina returns `QuaminaError::PatternTooComplex` when a pattern exceeds a limit. The default limits support typical patterns. Use the builder methods to change them for your workload.

Other limits:

- Event input must use the `application/json` media type.
- Pattern field names are case-sensitive.
- `shellstyle` patterns do not support `\*` or `\\` escapes. Use `wildcard` or `regexp` instead.

## Credits

[Tim Bray](https://www.tbray.org/) created the [original Go library](https://github.com/timbray/quamina). Its contributors developed the pattern language and matching design. The [Quamina Diary](https://www.tbray.org/ongoing/What/Technology/Quamina%20Diary/) explains the automata-based approach.

The [`.go-upstream-sync`](.go-upstream-sync) file records the last upstream Go commit. Run `just upstream` to check for changes.

## License

Apache 2.0
