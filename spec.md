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
├── regexp.rs           # I-Regexp parser + NFA builder (3715 lines - needs split)
├── unicode_categories.rs # Unicode category/block data
├── automaton/
│   ├── small_table.rs  # SmallTable (byte transitions)
│   ├── nfa.rs          # traverse_dfa, traverse_nfa
│   ├── arena.rs        # StateArena for cyclic NFA
│   └── trie.rs         # ValueTrie for O(n) bulk construction
└── wildcard.rs         # Shellstyle matching
```

## Go vs Rust Implementation Comparison

**Go wins:**
- Shell caching for negated classes (major optimization we lack)
- Better modularity (separate parser/NFA files)
- GC enables clever caching patterns

**Rust wins:**
- Structured errors with offset context
- Unicode block support (`~p{IsBasicLatin}`)
- Type safety, compile-time guarantees

**Key Go optimization we should implement:**
```go
// Go builds FA once with placeholder, reuses for different next states
cachedFaShells["L"] = makeRuneRangeFA(letterRunes, PlaceholderState)
fa := faFromShell(cachedFaShells["L"], PlaceholderState, realNextState)
```
For `[^abc]` (~1.1M Unicode points): Go reuses cached shell instantly, Rust expands all points every time.

## Improvement Opportunities

**High impact:**
1. **Shell caching** - Port Go's `cachedFaShells` pattern for negated classes
2. **Split regexp.rs** - 3715 lines is too large; split into `regexp_parser.rs` + `regexp_nfa.rs`

**Medium impact:**
3. **Optimize quantifier chains** - Current 100-state chain for `+`/`*` is memory-heavy
4. **Lazy negated categories** - Don't expand `[^abc]` eagerly

**Low priority (not in I-Regexp):**
5. XML escapes `~c`/`~i` - XSD only, +53 samples
6. Character class subtraction `[a-[b]]` - XSD only, +74 samples

## Commands

```bash
cargo test                    # 268 tests
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
- `src/regexp.rs` - parser + NFA (needs refactoring)
- `src/unicode_categories.rs` - Unicode data
- `src/lib.rs:test_regexp_validity()` - 560 samples tested

**Go reference:**
- `regexp_reader.go` - parsing (765 lines, cleaner than ours)
- `regexp_nfa.go` - NFA building with shell caching
- `rune_range.go` - rune range utilities
- `character_properties.go` - generated Unicode data
