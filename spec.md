# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Next Session: Final Verification Before Regexp Work

**Goal:** Quick sanity check before moving on to Regexp HashMap fallback elimination.

**Verification checklist:**
1. Run `cargo test` - confirm 304 tests pass
2. Run `cargo clippy -- -D warnings` - no warnings
3. Check CI status with `gh run list`
4. Quick review of any remaining Go test files not yet checked (search for `_test.go`)
5. Verify benchmarks haven't regressed: `cargo bench status`

**After verification, proceed to Regexp work:**
- Goal: Eliminate HashMap fallback for `Matcher::Regex`
- Currently regex with advanced features (lookaheads, backreferences) falls back to HashMap
- Investigate if these can be converted to automaton-based matching
- Low priority but would complete the "no HashMap fallback" goal

**Completed:** Full test coverage audit of all Go test files.

## Status

**304 tests passing.** Rust 1.5-2x faster. Synced with Go commit 74475a4 (Jan 2026).

### Test Coverage Audit (Completed)

All Go test files audited. Tests added: 301 → 304.

**Files audited:**
- ✅ `anything_but_test.go` - Added wordle words test
- ✅ `numbers_test.go` - Added numeric variant tests (0.000035e6)
- ✅ `arrays_test.go` - Already comprehensive
- ✅ `escaping_test.go` - Added all 8 JSON escape tests (\b, \f, \r, \/, \")
- ✅ `flatten_json_test.go` - Added error case tests, documented early termination
- ✅ `monocase_test.go` - Added singleton merge test

### Behavioral Differences (Documented)

1. **anything-but single string syntax**: Rust accepts `{"anything-but": "foo"}` (EventBridge/Ruler compatible), Go requires `{"anything-but": ["foo"]}`. See json.rs:344.

2. **Flattener early termination**: Rust stops parsing JSON once all needed pattern fields are found. Invalid JSON after those fields (like `{"a": 23z}` when only "a" is needed) may not be detected. Go always fully validates. This is intentional for performance. See lib.rs:1532 and flatten_json.rs:284-291.

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 405 | 1.8x |
| status_middle_nested | 7,437 | 4,912 | 1.5x |

## Completeness

**Pattern matchers:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`, `{"cidr"}`, `{"numeric"}`

**Rust-only extensions:**
- `{"numeric": [">=", 0]}` - numeric comparisons
- `{"cidr": "10.0.0.0/24"}` - IP range matching (now automaton-based)
- `{"anything-but": 404}` - numeric anything-but
- `{"regexp": "a{2,5}"}` - range quantifiers (Go lacks)
- `~d`/`~w`/`~s` - character class escapes
- `~p{IsBasicLatin}` - Unicode blocks
- `(?:...)` - non-capturing groups
- `*?`/`+?`/`{n,m}?` - lazy quantifiers

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── regexp/
│   ├── parser.rs       # I-Regexp parser
│   └── nfa.rs          # NFA building + shell caching
└── automaton/
    ├── small_table.rs  # SmallTable (byte transitions)
    ├── fa_builders.rs  # FA construction (string, prefix, cidr, shellstyle, etc.)
    ├── nfa.rs          # traverse_dfa, traverse_nfa
    ├── arena.rs        # StateArena for cyclic NFA
    └── trie.rs         # ValueTrie for O(n) bulk construction
```

## HashMap Fallback Status

**Goal:** Eliminate HashMap fallback entirely.

**Remaining:**
- `Matcher::Regex` - Advanced features (lookaheads, backreferences) - low priority

**Completed:**
- `AnythingButNumeric` - Q-number FA
- `Cidr` - IPv4 deterministic FA, IPv6 NFA with epsilons

## Implementation Notes

**Optimizations:**
- **Shell caching:** Unicode categories cached as pre-built FA shells
- **Arena-based cyclic NFA:** For `+`/`*` quantifiers, true cycles instead of chain copies
- **CIDR FA:** IPv4 uses trie-like octet matching, IPv6 uses NFA for hex groups

**CIDR Implementation:**
- IPv4: Build FA right-to-left, each octet constrained by prefix_len bits
- IPv6: 8 hex groups with ':' separators, epsilon transitions for variable-length
- Files: `fa_builders.rs` (`make_cidr_fa`), `mutable_matcher.rs` (`add_cidr_transition`)

## Commands

```bash
cargo test                    # 304 tests
cargo bench status            # benchmarks
cargo clippy -- -D warnings   # CI check
gh run list                   # check CI
cargo fmt && git push         # format and push
```

## Session Checklist

1. Read this spec (especially "Next Session" section)
2. For Go behavior, read Go source directly
3. Push often, check CI (`gh run list`)
4. Use todos to manage context

**Key files:**
- `src/automaton/fa_builders.rs` - FA construction
- `src/automaton/mutable_matcher.rs` - Pattern building
- `src/regexp/nfa.rs` - NFA building + shell caching
