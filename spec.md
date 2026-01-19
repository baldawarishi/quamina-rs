# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**275 tests passing.** Rust 1.5-2x faster. Synced with Go commit 74475a4 (Jan 2026).

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
- `~i`/`~c` - XML name character escapes (XSD compatibility)
- `~p{IsBasicLatin}` - Unicode blocks (not in I-Regexp)
- `(?:...)` - non-capturing groups (XSD syntax)
- `*?`/`+?`/`{n,m}?` - lazy quantifiers (XSD syntax)

**Regexp samples:** Rust 652, Go 203 (of 992 total)

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

**Optimizations implemented:**
- **Shell caching:** For Unicode categories like `~p{L}` (~1.1M code points), we cache pre-built FA shells. Second use of same category is O(copy) instead of O(rebuild).
- **Arena-based cyclic NFA:** For `+`/`*` quantifiers, we use `StateArena` with true cycles (2-3 states) instead of Go's mutable pointer approach. Production code routes ALL regexp patterns through `make_regexp_nfa_arena`.

**Go parity achieved:**
- Clean module structure (regexp/parser.rs + regexp/nfa.rs)
- Shell caching for Unicode categories
- Structured errors with offset context
- Unicode block support (`~p{IsBasicLatin}`)
- Efficient cyclic NFA for quantifiers

## Improvement Opportunities

**Medium impact:**
1. **Lazy negated categories** - Don't expand `[^abc]` eagerly

**Low priority (not in I-Regexp):**
2. Character class subtraction `[a-[b]]` - XSD only, +74 samples

## HashMap Fallback Deprecation Plan

**Goal:** Eliminate HashMap fallback entirely by moving all patterns to automaton-based matching.

**Current HashMap fallbacks** (see `Matcher::is_automaton_compatible()` in `src/json.rs`):
1. `Matcher::Regex` - Regex with advanced features (lookaheads, lookbehinds, backreferences)
2. `Matcher::Cidr` - IP range matching
3. `Matcher::AnythingButNumeric` - Numeric exclusion lists

**Migration priority:**

### Phase 1: CIDR (High Value, Medium Complexity)
- **Why:** Common use case (IPs in logs, security events), predictable pattern
- **Approach:** Convert CIDR to automaton-compatible byte ranges
  - Parse CIDR notation into prefix + mask
  - Build SmallTable transitions for valid IP byte sequences
  - Example: `10.0.0.0/24` → transitions for `10.0.0.[0-255]`
- **Impact:** Enables automaton matching for all IP filtering patterns
- **Files:** `src/json.rs` (CIDR matcher), `src/automaton/` (add IP transition builder)

### Phase 2: AnythingButNumeric (High Value, Low Complexity)
- **Why:** Simple to implement, completes numeric matching support
- **Approach:** Use Q-number NFA with inverted ranges
  - Current `Matcher::Numeric` already uses Q-number automaton
  - Extend to support exclusion lists via inverted transitions
  - Reuse existing Q-number encoding from `src/numbits.rs`
- **Impact:** All numeric patterns (ranges + exclusions) use automaton
- **Files:** `src/automaton/mutable_matcher.rs`, `src/numbits.rs`

### Phase 3: Regex Advanced Features (Lower Priority, High Complexity)
- **Consider:** Lookaheads, lookbehinds, backreferences
- **Feasibility check:** These are typically NP-complete and may not be worth automaton implementation
- **Alternative:** Keep regex crate fallback for these edge cases, as they're outside I-Regexp scope
- **Decision:** Evaluate after Phase 1 & 2 - may choose to keep this fallback permanently

**Success criteria:**
- After Phase 1 & 2: 95%+ of real-world patterns use automaton matching
- Zero performance regression on existing benchmarks
- Maintain or improve memory usage

## Commands

```bash
cargo test                    # 275 tests
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
