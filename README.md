# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - a fast pattern-matching library for filtering JSON events.

All credit should go to [Tim Bray](https://www.tbray.org/) who designed and maintains the original quamina. Tim also created [aws/event-ruler](https://github.com/aws/event-ruler), the Java library that powers Amazon EventBridge among a staggering amount of Amazon things. For those curious, Tim also has documented the design extensively in his [Quamina Diary](https://www.tbray.org/ongoing/What/Technology/Quamina%20Diary/) Series which I'd highly recommend for understanding the internals of Quamina.

## Warning

This port was 100% "vibecoded" in the background. I don't have a deep understanding of the source code (yet). Use at your own risk ... but if you do find issues, please do report them.

## Feature Parity

### Pattern Matchers

| Matcher | Go | Rust | Example |
|---------|:--:|:----:|---------|
| Exact string | Yes | Yes | `"value"` |
| Exact number | Yes | Yes | `35` matches `35.0`, `3.5e1` |
| Prefix | Yes | Yes | `{"prefix": "https://"}` |
| Suffix | No | Yes | `{"suffix": ".json"}` |
| Wildcard | Yes | Yes | `{"wildcard": "*error*"}` |
| Shellstyle | Yes | Yes | `{"shellstyle": "*.txt"}` |
| Exists | Yes | Yes | `{"exists": true}` |
| Anything-but (strings) | Yes | Yes | `{"anything-but": ["error", "warn"]}` |
| Anything-but (numbers) | No | Yes | `{"anything-but": [404, 500]}` |
| Equals-ignore-case | Yes | Yes | `{"equals-ignore-case": "ERROR"}` |
| Numeric comparisons | No | Yes | `{"numeric": [">=", 0, "<", 100]}` |
| CIDR | No | Yes | `{"cidr": "10.0.0.0/8"}` |
| Regexp | Yes | Yes | `{"regexp": "[a-z]+"}` |

### Regexp Features (I-Regexp / RFC 9485)

| Feature | Go | Rust | Example |
|---------|:--:|:----:|---------|
| Character classes | Yes | Yes | `[a-z]`, `[^0-9]` |
| Quantifiers `?`, `*`, `+` | Yes | Yes | `a+b*c?` |
| Range quantifiers `{n,m}` | No | Yes | `a{2,5}` |
| Groups `(...)` | Yes | Yes | `(ab)+` |
| Non-capturing groups `(?:...)` | No | Yes | `(?:ab)+` |
| Alternation | Yes | Yes | `cat\|dog` |
| Dot `.` | Yes | Yes | `a.b` |
| Escapes `~d`, `~w`, `~s` | Yes | Yes | `~d+` (digits) |
| Unicode categories `~p{L}` | Yes | Yes | `~p{Lu}` (uppercase) |
| Unicode blocks | Yes | Yes | `~p{IsBasicLatin}` |
| Lazy quantifiers `*?`, `+?` | No | Yes | `a+?` |
| XML name chars `~i`, `~c` | Yes | Yes | `~i~c*` |

**Regexp test coverage:** Rust passes 652/992 [XSD regexp test samples](https://github.com/qt4cg/xslt40-test) (by Michael Kay) vs Go's 203/992.

## Performance Parity

Benchmarks on Apple M3 Max, January 2026. Lower is better.

### Comparable Benchmarks (Rust vs Go)

| Benchmark | Go | Rust | Speedup | Description |
|-----------|---:|-----:|--------:|-------------|
| citylots | 3,103 ns | 2,025 ns | 1.5x | 4 patterns vs 206k GeoJSON features |
| status_middle_nested | 6,878 ns | 4,784 ns | 1.4x | Nested field match in 14KB JSON |
| status_context_fields | 403 ns | 329 ns | 1.2x | Early field match in 14KB JSON |

### Rust-only Benchmarks

| Benchmark | Time | Description |
|-----------|-----:|-------------|
| exact_match | 110 ns | Single exact match |
| nested_match | 148 ns | Nested field exact match |
| 100_patterns | 183 ns | 100 patterns, 1 match |
| 100_patterns_no_match | 68 ns | 100 patterns, 0 matches |
| regex_match | 159 ns | Simple regex pattern |
| shellstyle_26_patterns | 366 ns | 26 shellstyle patterns (A*-Z*) |
| 100_prefix_patterns | 194 ns | 100 prefix patterns |
| anything_but_match | 136 ns | Anything-but with 3 values |
| numeric_range_single | 136 ns | Single-sided numeric (`< 100`) |
| numeric_range_two_sided | 137 ns | Two-sided numeric (`>= 0, < 100`) |
| has_matches_early_exit | 166 ns | Boolean match check (early exit) |

### Reproducing Benchmarks

**Rust:**
```bash
cargo bench citylots
cargo bench status_middle
cargo bench --bench matching   # all benchmarks
```

**Go** (requires quamina source):
```bash
cd /path/to/quamina
go test -bench=BenchmarkCityLots -benchtime=3s
go test -bench=Benchmark_JsonFlattner_Evaluate_MiddleNestedField -benchtime=3s
go test -bench=. -benchtime=2s  # all benchmarks
```

**Cross-comparison script:**
```bash
#!/bin/bash
# Usage: ./scripts/bench-compare.sh /path/to/quamina
set -e
QUAMINA_GO="${1:?Usage: $0 /path/to/quamina}"

echo "=== Rust ==="
cargo bench --bench matching -- citylots --noplot 2>/dev/null | grep -E "time:.*\[" | head -1
cargo bench --bench matching -- status_middle_nested --noplot 2>/dev/null | grep -E "time:.*\[" | head -1

echo ""
echo "=== Go ==="
(cd "$QUAMINA_GO" && go test -bench=BenchmarkCityLots -benchtime=3s -count=1 2>/dev/null | grep ns/op)
(cd "$QUAMINA_GO" && go test -bench=Benchmark_JsonFlattner_Evaluate_MiddleNestedField -benchtime=3s -count=1 2>/dev/null | grep ns/op)
```

## License

Apache 2.0, same as the original quamina.
