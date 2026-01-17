# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**268 tests passing.** Rust 1.5-2x faster. Synced with Go commit 74475a4 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 405 | 1.8x |
| status_middle_nested | 7,437 | 4,912 | 1.5x |

## Completeness

**Rust is fully I-Regexp (RFC 9485) compliant.** Go quamina is not yet fully compliant.

**Pattern matchers (Go parity):** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`

**Rust-only pattern extensions:**
- `{"numeric": [">=", 0]}` - numeric comparisons
- `{"cidr": "10.0.0.0/24"}` - IP range matching
- `{"anything-but": 404}` - numeric anything-but

**I-Regexp features (RFC 9485) - Rust has, Go lacks:**
- `{"regexp": "a{2,5}"}` - range quantifiers `{n}`, `{n,m}`, `{n,}`

**Rust extensions beyond I-Regexp:**
- `~d`/`~w`/`~s`/`~D`/`~W`/`~S` - character class escapes (not in RFC 9485)
- `~p{IsBasicLatin}` - Unicode block matchers (not in RFC 9485)

**Regexp sample testing:** Rust tests 560 samples (Go implements 203 of 992)

## Public API

```rust
q.add_pattern(id, pattern_json)?;
q.delete_patterns(id)?;
q.matches_for_event(event)?;
q.list_pattern_ids() -> Vec<&X>
q.contains_pattern(&id) -> bool
q.pattern_count() -> usize
```

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── regexp.rs           # I-Regexp parser + NFA builder
├── unicode_categories.rs # Unicode category/block data for ~p{}
├── automaton/
│   ├── small_table.rs  # SmallTable (byte transitions)
│   ├── nfa.rs          # traverse_dfa, traverse_nfa
│   ├── arena.rs        # StateArena for cyclic NFA (regexp *)
│   └── trie.rs         # ValueTrie for O(n) bulk construction
└── wildcard.rs         # Shellstyle matching
```

## Regexp Features

**I-Regexp (RFC 9485) - fully implemented:**
- `.` any char, `[...]` classes, `[^...]` negated classes
- `|` alternation, `(...)` groups
- `?` optional, `+` one-or-more, `*` zero-or-more
- `{n}`, `{n,m}`, `{n,}` range quantifiers
- `~p{Lu}`, `~p{Ll}`, `~p{Nd}` - Unicode general categories
- Escape char is `~` (not `\`) to avoid JSON escaping

**Extensions beyond I-Regexp:**
- `~d` digits, `~w` word, `~s` whitespace (+ negated `~D`/`~W`/`~S`)
- `~p{IsBasicLatin}` - Unicode blocks

**Not in I-Regexp (not implemented):**
- `~c` / `~i` - XML name char escapes (XSD only)
- `[a-[b]]` - character class subtraction (XSD only)

## Commands

```bash
cargo test                    # 268 tests
cargo bench status            # benchmarks
cargo clippy -- -D warnings   # CI check
gh run list                   # check CI
cargo fmt && git push         # format and push
```

## Session Notes

**When continuing:**
1. Read this spec for context
2. For Go behavior, read Go source directly - don't trust past interpretations
3. Push often, check CI (`gh run list`)
4. Use todos to manage context window

**Key files:**
- Regexp parser: `src/regexp.rs`
- Unicode categories: `src/unicode_categories.rs`
- Sample validity test: `src/lib.rs:test_regexp_validity()`
- Go regexp samples: `src/regexp_samples.rs` (992 samples)

**Test commands:**
```bash
cargo test test_regexp_validity -- --nocapture  # 560 samples
cargo test test_multi_char_escapes              # ~d/~w/~s tests
cargo test test_regexp_end2end                  # end-to-end regexp
```

**Go reference files:**
- `regexp_reader.go` - parsing
- `regexp_nfa.go` - NFA building
- `regexp_samples_test.go` - 992 test samples
- `case_folding.go` - 49KB generated Unicode data (case folding only)

## Future Work

1. **XML escapes** - Implement `~c` and `~i` for XML name characters (Go doesn't have these either)
2. **Character class subtraction** - `[a-[b]]` XSD feature (low priority)
3. **More Unicode categories** - Complete coverage for all Unicode general categories
