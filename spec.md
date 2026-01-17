# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**270 tests passing.** Rust 1.5-2x faster. Synced with Go commit 74475a4 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 405 | 1.8x |
| status_middle_nested | 7,437 | 4,912 | 1.5x |

## Completeness

**Rust is fully I-Regexp (RFC 9485) compliant.** Go lacks range quantifiers `{n,m}`.

**Pattern matchers:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`

**Rust-only extensions:**
- `{"numeric": [">=", 0]}` - numeric comparisons
- `{"cidr": "10.0.0.0/24"}` - IP range matching
- `{"anything-but": 404}` - numeric anything-but
- `{"regexp": "a{2,5}"}` - range quantifiers (Go lacks)
- `~d`/`~w`/`~s` - character class escapes (not in I-Regexp)
- `~p{IsBasicLatin}` - Unicode blocks (not in I-Regexp)

**Regexp samples:** Rust 560, Go 203 (of 992 total)

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── regexp/
│   ├── mod.rs          # Re-exports and tests
│   ├── parser.rs       # I-Regexp parser, data structures
│   └── nfa.rs          # NFA building + shell caching
├── unicode_categories.rs # Unicode category/block data
├── automaton/
│   ├── small_table.rs  # SmallTable (byte transitions)
│   ├── nfa.rs          # traverse_dfa, traverse_nfa
│   ├── arena.rs        # StateArena for cyclic NFA
│   └── trie.rs         # ValueTrie for O(n) bulk construction
└── wildcard.rs         # Shellstyle matching
```

## Implementation Notes

**Shell caching (now implemented):** For Unicode categories like `~p{L}` (~1.1M code points), we cache pre-built FA shells. Second use of same category is O(copy) instead of O(rebuild).

**Go parity achieved:**
- Clean module structure (regexp/parser.rs + regexp/nfa.rs)
- Shell caching for Unicode categories
- Structured errors with offset context
- Unicode block support (`~p{IsBasicLatin}`)

## Improvement Opportunities

**Medium impact:**
1. **Optimize quantifier chains** - Current 100-state chain for `+`/`*` is memory-heavy
2. **Lazy negated categories** - Don't expand `[^abc]` eagerly

**Low priority (not in I-Regexp):**
3. XML escapes `~c`/`~i` - XSD only, +53 samples
4. Character class subtraction `[a-[b]]` - XSD only, +74 samples

## Commands

```bash
cargo test                    # 270 tests
cargo bench status            # benchmarks
cargo clippy -- -D warnings   # CI check
gh run list                   # check CI
cargo fmt && git push         # format and push
```

## Session Checklist

1. Read this spec
2. For Go behavior, read Go source directly
3. Push often, check CI (`gh run list`)
4. Use todos to manage context

**Key files:**
- `src/regexp/parser.rs` - I-Regexp parsing
- `src/regexp/nfa.rs` - NFA building + shell caching
- `src/unicode_categories.rs` - Unicode data

**Go reference:**
- `regexp_reader.go` - parsing
- `regexp_nfa.go` - NFA building
- `rune_range.go` - shell caching
