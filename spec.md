# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**258 tests passing.** Rust 1.5-2x faster. Synced with Go commit c443b44 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| status_context_fields | 398 | 362 | 1.1x |
| status_middle_nested | 7,437 | 4,912 | 1.5x |
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 405 | 1.8x |

## Pattern Types

**Go parity:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`

**Rust-only:** `{"anything-but": 404}` (numeric), `{"numeric": [">=", 0]}`, `{"cidr": "10.0.0.0/24"}`, `{"regexp": "a{2,5}"}` (range quantifiers)

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── regexp.rs           # I-Regexp parser + arena NFA builder
├── automaton/
│   ├── small_table.rs  # SmallTable (byte transitions)
│   ├── nfa.rs          # traverse_dfa, traverse_nfa
│   ├── arena.rs        # StateArena for cyclic NFA (regexp *)
│   ├── trie.rs         # ValueTrie for O(n) bulk construction
│   └── mutable_matcher.rs  # Pattern building
└── wildcard.rs         # Shellstyle matching
```

**Go reference:** `core_matcher.go` (matching), `value_matcher.go` (FA building), `regexp_nfa.go` (regexp)

## Commands

```bash
cargo test                    # 258 tests
cargo bench status            # benchmarks
cargo clippy -- -D warnings   # CI check
gh run list                   # check CI
```

## Regexp Implementation

**I-Regexp subset (RFC 9485):**
- `.` any char, `[...]` classes, `|` alternation, `(...)` groups
- `?` optional, `+` one-or-more, `*` zero-or-more
- `{n}` exactly n, `{n,m}` between n and m, `{n,}` at least n
- Escape char is `~` (not `\`) to avoid JSON escaping

**Two NFA implementations:**
1. **Chain NFA** (`make_regexp_nfa`): Simple patterns, no cycles
2. **Arena NFA** (`make_regexp_nfa_arena`): Efficient for `*`/`+` with cyclic structures

**Sample testing status (992 samples from Go):**
- 77 samples fully tested (patterns without problematic features)
- Skipped features in bulk testing:
  - `.` (dot) - creates huge Unicode state machines
  - `[^...]` negated classes - large UTF-8 range tables
  - `~w`, `~d`, `~s`, `~p{}` - unimplemented character class escapes
  - `-[` character class subtraction - XSD feature, not I-Regexp

**Recent fix:** `{n}` now correctly means "exactly n" (was "at least n"). Go has same bug but skips all `{}` tests.

## Bulk Pattern Optimization

Trie-based O(n) construction for patterns with many string values. See `src/automaton/trie.rs`.

```bash
cargo bench bulk_100x10   # ~1.1ms (was 16ms naive)
cargo bench bulk_1000x10  # ~62ms (was ~5s naive)
```

## Next Tasks

### 1. Character Class Escapes (Medium)
Implement `~w` (word), `~d` (digit), `~s` (space) and negated versions.

**Files:** `src/regexp.rs`
**Approach:** Add to `check_single_char_escape()` or create multi-char escape handler. Each maps to a `RuneRange`.

```rust
// ~d = [0-9]
// ~w = [a-zA-Z0-9_]
// ~s = [ \t\n\r]
```

**Challenge:** Large Unicode ranges for `~W`, `~D`, `~S` (negated) - use optimized range construction like `[^...]`.

### 2. Enable `*`/`+` Sample Testing (Medium)
Arena NFA handles `*`/`+` efficiently but sample testing is slow.

**Options:**
- Run subset of `*`/`+` samples (first 50?)
- Add timeout per sample
- Profile and optimize arena NFA traversal

### 3. Pattern Retrieval API (Easy)
Add methods to retrieve registered patterns (Go #73).

**Files:** `src/lib.rs`
```rust
impl Quamina<X> {
    pub fn get_patterns(&self) -> Vec<&X> { ... }
    pub fn list_pattern_ids(&self) -> Vec<&X> { ... }
}
```

### 4. Unicode Property Matchers (Hard)
`~p{Lu}` (uppercase), `~P{Ll}` (not lowercase). Requires Unicode tables.

**Files:** `src/regexp.rs`
**Note:** Not implemented in Go either.

## Session Notes

**When continuing:**
1. Read this spec for context
2. For Go behavior, read Go source directly - don't trust past interpretations
3. Push often, check CI (`gh run list`)
4. Run `cargo fmt` before commit

**Test regexp changes:**
```bash
cargo test test_regexp_validity -- --nocapture
cargo test test_parse_range_quantifier
cargo test test_nfa_range
```

**Key test file locations:**
- Regexp samples: `src/regexp_samples.rs` (992 samples)
- Regexp tests: `src/regexp.rs` (bottom of file)
- Validity test: `src/lib.rs` (`test_regexp_validity`)
