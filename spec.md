# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**262 tests passing.** Rust 1.5-2x faster. Synced with Go commit c443b44 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| status_context_fields | 398 | 362 | 1.1x |
| status_middle_nested | 7,437 | 4,912 | 1.5x |
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 405 | 1.8x |

## Completeness

**Rust is a superset of Go quamina.** All Go features implemented plus Rust-only extensions.

**Go parity:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`

**Rust-only extensions:**
- `{"numeric": [">=", 0]}` - numeric comparisons (Go has TODO)
- `{"cidr": "10.0.0.0/24"}` - IP range matching (not in Go)
- `{"anything-but": 404}` - numeric anything-but
- `{"regexp": "a{2,5}"}` - range quantifiers
- `~d`/`~w`/`~s`/`~D`/`~W`/`~S` - character class escapes

**Regexp sample testing:**
- Rust: 193 samples tested (including `*`, `+`, `.`, `[^...]`)
- Go: 128 samples tested
- We test 50% more patterns than Go!

## Public API

```rust
// Core matching
q.add_pattern(id, pattern_json)?;
q.delete_patterns(id)?;
q.matches_for_event(event)?;

// Inspection
q.list_pattern_ids() -> Vec<&X>   // all active pattern IDs
q.contains_pattern(&id) -> bool   // check if pattern exists
q.pattern_count() -> usize
q.is_empty() -> bool
```

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── regexp.rs           # I-Regexp parser + NFA builder
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
cargo test                    # 262 tests
cargo bench status            # benchmarks
cargo clippy -- -D warnings   # CI check
gh run list                   # check CI
cargo fmt                     # format before commit
```

## Regexp Implementation

**I-Regexp subset (RFC 9485):**
- `.` any char, `[...]` classes, `[^...]` negated classes, `|` alternation, `(...)` groups
- `?` optional, `+` one-or-more, `*` zero-or-more
- `{n}` exactly n, `{n,m}` between n and m, `{n,}` at least n
- `~d` digits, `~w` word chars, `~s` whitespace (and negated `~D`/`~W`/`~S`)
- Escape char is `~` (not `\`) to avoid JSON escaping

**Two NFA implementations:**
1. **Chain NFA** (`make_regexp_nfa`): Simple patterns, no cycles
2. **Arena NFA** (`make_regexp_nfa_arena`): Efficient for `*`/`+` with cyclic structures

**Skipped in sample testing:** `~c`/`~i`/`~p{}` (unimplemented), character class subtraction `[a-[b]]` (XSD feature)

## Next Tasks

### 1. Unicode Property Matchers (Hard)
`~p{Lu}` (uppercase), `~P{Ll}` (not lowercase). Requires Unicode tables. Not in Go either.

### 2. Potential Improvements
- Consider implementing `~c` (XML name chars) and `~i` (XML initial chars) if needed
- Performance profiling for large pattern sets
- Documentation improvements

## Session Notes

**When continuing:**
1. Read this spec for context
2. For Go behavior, read Go source directly - don't trust past interpretations
3. Push often, check CI (`gh run list`)
4. Use todos to manage context window

**Test commands:**
```bash
cargo test test_regexp_validity -- --nocapture  # 193 samples
cargo test test_multi_char_escapes              # ~d/~w/~s tests
cargo test test_nfa_range                       # range quantifiers
cargo test test_regexp_end2end                  # end-to-end regexp
```

**Key files:**
- Regexp samples: `src/regexp_samples.rs` (992 samples from Go)
- Regexp tests: `src/regexp.rs` (bottom of file)
- Multi-char escapes: `src/regexp.rs:check_multi_char_escape()`
- Sample validity test: `src/lib.rs:test_regexp_validity()`
