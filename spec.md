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

**Remaining HashMap fallbacks** (see `Matcher::is_automaton_compatible()` in `src/json.rs`):
1. `Matcher::Cidr` - IP range matching
2. `Matcher::Regex` - Regex with advanced features (lookaheads, lookbehinds, backreferences)

**Completed:**
- ✅ `AnythingButNumeric` - Uses Q-number FA (same algorithm as string anything-but)

### Next: CIDR Automaton Support

**Why:** Common use case (IPs in logs, security events), predictable pattern.

**Approach:**
1. Parse CIDR notation into network address + prefix length (already done in `CidrPattern`)
2. Build FA that matches valid IP strings character-by-character
3. For each octet position, create transitions for valid digit sequences
4. Example: `10.0.0.0/8` matches `10.X.X.X` where X is any valid octet (0-255)

**Challenge:** IP addresses are strings like `"10.0.0.1"`, not binary. Need to match:
- Each octet as 1-3 digit string (0-255)
- Dots between octets
- Validate octet values don't exceed 255

**Key insight:** Can build a trie-like FA for valid octet values (0-255), then compose 4 of them with dot separators.

**Files to modify:**
- `src/automaton/fa_builders.rs` - Add `make_cidr_fa` function
- `src/automaton/mutable_matcher.rs` - Add `add_cidr_transition` method
- `src/json.rs` - Update `is_automaton_compatible()` to return true for CIDR

**Reference:** See `make_anything_but_numeric_fa` for similar pattern of converting semantic values to automaton.

### Future: Regex Advanced Features (Low Priority)
- Lookaheads, lookbehinds, backreferences are outside I-Regexp scope
- May keep regex crate fallback permanently for these edge cases

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
