# quamina-rs

[![CI](https://github.com/baldawarishi/quamina-rs/actions/workflows/test.yml/badge.svg)](https://github.com/baldawarishi/quamina-rs/actions/workflows/test.yml)
[![Crates.io](https://img.shields.io/crates/v/quamina.svg)](https://crates.io/crates/quamina)
[![Documentation](https://docs.rs/quamina/badge.svg)](https://docs.rs/quamina)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)

Rust port of [quamina](https://github.com/timbray/quamina), a pattern-matching library for filtering JSON objects.

In Quamina, you add Patterns to a quamina instance, then match Events against it. Quamina tells you which Patterns matched. It does this fast—millions of events per second, regardless of how many patterns you have.

Try it online in the **[playground](https://baldawarishi.github.io/quamina-rs/)** to test patterns against JSON events in your browser.

## Contents

- [Quick Start](#quick-start)
- [Patterns](#patterns)
- [APIs](#apis)
- [Concurrency](#concurrency)
- [Performance](#performance)
- [Limitations](#limitations)
- [Credits](#credits)

## Quick Start

```rust
use quamina::Quamina;

let mut q = Quamina::new();

q.add_pattern("p1", r#"{"status": ["error"]}"#)?;
q.add_pattern("p2", r#"{"level": [1, 2, 3]}"#)?;

let event = r#"{"status": "error", "level": 2}"#;
let matches = q.matches_for_event(event.as_bytes())?;
// matches: ["p1", "p2"]
```

## Patterns

A Pattern is a JSON object. Field values are arrays—if any element matches, it's a match. All fields mentioned must match (AND), but only one value per field needs to match (OR).

Given this event:
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

These patterns match though quamina:
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

**Exact match** — value must equal exactly:
```json
{"status": ["active"]}
{"count": [100]}
{"enabled": [true]}
{"deleted": [null]}
```

**Prefix/Suffix** — string starts or ends with:
```json
{"url": [{"prefix": "https://"}]}
{"file": [{"suffix": ".json"}]}
```

**Wildcard** — wild-card matching:
```json
{"message": [{"wildcard": "*error*"}]}
{"id": [{"wildcard": "user-*-prod"}]}
```

`*` matches any sequence. Use `\*` to match a literal asterisk, `\\` for a literal backslash.   

We also have Quamina's legacy `shellstyle` based matcher but you should avoid it. Shellstyle doesn't support `\*` or `\\` escapes. It may go away entirely in the long run. In fact, prefer to use regex whenever you are comfortable with its performance and syntax. 

**Exists** — field presence:
```json
{"email": [{"exists": true}]}
{"deleted_at": [{"exists": false}]}
```

**Anything-but** — match unless value is in list:
```json
{"status": [{"anything-but": ["pending", "cancelled"]}]}
{"code": [{"anything-but": [400, 404, 500]}]}
```

**Equals-ignore-case** — case-insensitive:
```json
{"level": [{"equals-ignore-case": "ERROR"}]}
```

**Numeric** — comparisons:
```json
{"price": [{"numeric": [">", 100]}]}
{"age": [{"numeric": [">=", 18, "<", 65]}]}
```

**CIDR** — IP address ranges:
```json
{"ip": [{"cidr": "10.0.0.0/8"}]}
{"ip": [{"cidr": "2001:db8::/32"}]}
```

**Regexp** — I-Regexp (RFC 9485):
```json
{"email": [{"regexp": "[a-z]+@[a-z]+\\.[a-z]+"}]}
{"code": [{"regexp": "[A-Z]{3}-[0-9]{4}"}]}
```

Regexp uses `~` as the escape character to stay compliant with Quamina. There's also `~d` for digits, `~p{L}` for Unicode letters, and `~b`/`~B` for word boundaries where you will pay a performance penalty depending on the patterns and events in use. 

## APIs

### Creating and configuring

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

### Adding and removing patterns

```rust
q.add_pattern("my-rule", r#"{"x": [1]}"#)?;
q.delete_patterns(&"my-rule")?;
q.clear();
```

### Matching

```rust
let matches = q.matches_for_event(event)?;  // Vec of matching IDs
let matched = q.has_matches(event)?;         // bool
let count   = q.count_matches(event)?;       // number of matches
```

### Errors

`add_pattern` returns an error if the pattern JSON is malformed, uses invalid syntax, or exceeds complexity limits. `matches_for_event` returns an error if the event isn't valid JSON.

```rust
match q.add_pattern("bad", r#"{"x": "not-an-array"}"#) {
    Err(QuaminaError::InvalidPattern(msg)) => println!("{}", msg),
    Err(QuaminaError::PatternTooComplex(msg)) => println!("{}", msg),
    _ => {}
}
```

## Concurrency

A single `Quamina` instance can be safely shared across threads via `Arc`. Matching uses thread-local buffers, so multiple threads calling `matches_for_event()` on the same `Arc<Quamina>` run in parallel without contention.

Pattern addition (`add_pattern`) requires `&mut self`. For concurrent writes, wrap in a lock:

```rust
let q = Arc::new(RwLock::new(Quamina::new()));
```

`clone()` rebuilds the automaton from stored patterns. It's not a cheap operation for instances with many patterns.

## Performance

Matching time is nearly independent of pattern count. All patterns compile into a single automaton, so 10 patterns and 10,000 patterns have similar matching speed.

### Pattern count scaling

On an M4 Max:

| Patterns | Match time |
|----------|-----------|
| 100 | ~110 ns |
| 10,000 | ~90 ns |

Matching time is sublinear in pattern count because all patterns share one automaton.

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

- **Unique fields**: More unique field paths across patterns = more work per event
- **Event size**: Larger JSON takes longer to parse and flatten
- **Pattern complexity**: Regexps with Unicode categories (e.g., `~p{L}`) are slower to compile
- **Build mode**: `BuiltForSpeed` trades slower adds for faster matching of wildcard/regexp patterns (see below)

### Comfort vs speed

Wildcard and regexp patterns compile to NFAs. By default (`BuiltForComfort`) they stay NFAs: cheap to add, but `matches_for_event` slows down roughly linearly as you add more such patterns. `BuiltForSpeed` converts them to DFAs when the matcher freezes, giving matching time only weakly related to the pattern count — at the cost of slower adds and, for some pattern combinations, explosive matcher growth (as bad as O(2ⁿ)). Watch `matcher_stats()` if you enable it.

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

## Limitations

Patterns are subject to complexity limits to prevent resource exhaustion from deeply nested or extremely wide patterns:

| Limit | Default | Builder method |
|-------|---------|----------------|
| Max nesting depth | 256 | `with_max_pattern_depth` |
| Max fields per pattern | 256 | `with_max_fields_per_pattern` |
| Arena byte budget | 10 MB | `with_arena_byte_budget` |
| Max states per pattern | 1024 | `with_max_states_per_pattern` |

Patterns exceeding these limits return `QuaminaError::PatternTooComplex`. The defaults are generous enough for any realistic use case; they're primarily a safety net against adversarial input.

Other limitations:
- JSON (media type `application/json`) is the only format enabled by default. MessagePack, CBOR, Protobuf, Avro, and transport-header/CloudEvents flattening are available behind opt-in Cargo features (`messagepack`, `cbor`, `protobuf`, `avro`, `headers`, `cloudevents`, or `all-formats` for all of them) — see `tests/contracts/README.md` for the contract each one satisfies.
- Pattern field names are case-sensitive
- `shellstyle` patterns don't support `\*` or `\\` escapes — prefer `wildcard` or `regexp` instead

## Credits

All credits should go to [Tim](https://www.tbray.org/) and other contributors in the [original Go](https://github.com/timbray/quamina) version. Tim's [Quamina Diary](https://www.tbray.org/ongoing/What/Technology/Quamina%20Diary/) also explains how automata-based matching works.

The last-synced upstream Go commit is tracked in [`.go-upstream-sync`](.go-upstream-sync). Run `just upstream` to check for new changes.


## License

Apache 2.0
