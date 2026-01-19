# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Go Test Gap Analysis (Completed)

**Summary:** Analyzed 134 Go tests across 40 files. ~75 covered, ~4 priority gaps, ~40 intentionally skipped.

### Priority Gaps (Should Add)

| Go Test | Priority | Description |
|---------|----------|-------------|
| TestRegexpValidity | High | 900+ XSD regex samples from Michael Kay - comprehensive Unicode property categories (`~p{L}`, `~p{Cc}`, `~P{IsBasicLatin}`) |
| TestArrayCorrectness | High | Complex nested array matching with AND logic across members, array index tracking |
| TestRulerCl2 | Medium | Integration test with all matchers on citylots2 dataset (40K+ lines) |
| TestFJErrorCases | Medium | 43 distinct JSON error cases for malformed input handling |

### Intentionally Skipped Go Tests

**Go Internal Implementation Details (Won't Cover):**

| Test | Rationale |
|------|-----------|
| TestInvalidValueTypes | Go panic testing (language-specific) |
| TestMakeFAFragment, TestMakeByteDotFA | Internal FA construction details |
| TestAddRuneTreeEntry, TestMultiLengthRR, TestRRiterator | Go runeTreeNode data structure |
| TestIsNormalChar, TestSingleCharEscape, TestReadCCE1 | Go parser internals |
| TestRRCacheEffectiveness | Go-specific cached FA shells optimization |
| TestTriggerTooManyFilteredDenom, TestLiveRatioTrigger, TestNeverTrigger | Go pruner internals |
| TestMemIterateFerr, TestStateDelete | Go memState interface |
| TestUnpack, TestSmallTableIterator | Go SmallTable internals |
| TestStateLists | Go stateLists deduplication structure |
| TestEpsilonClosure | Go computes standalone; Rust inline during traverse_nfa |
| TestFocusedMerge | Go FA merge statistics |
| TestPP, TestNullPP | Go pretty printer (debugging) |
| TestRegexpSamplesExist | Sanity check for test data |
| TestShellStyleBuildTime | Go performance benchmark |

**Different Architecture (Shouldn't Cover):**

| Test | Rationale |
|------|-----------|
| TestAddX, TestAddXSingleThreaded | Go matchSet vs Rust Vec with dedup |
| TestCityLots | Go benchmark; Rust has `cargo bench` |
| TestBasicRegexpFeatureRead, TestAddRegexpTransition | Go feature detection API (Rust doesn't expose) |
| TestSkinnyRuneTree | Go-specific NFA construction; Rust uses arena-based |
| TestBadState, TestUnsetRebuildTrigger, TestFlattener | Go pruner/state interface (Rust uses Arc) |

### Already Covered (Examples)

| Go Test | Rust Equivalent |
|---------|-----------------|
| TestCopy | `test_arc_snapshot_isolation` |
| TestConcurrencyCore | `test_arc_concurrent_read_write` |
| TestBasic | `test_arc_pattern_lifecycle` |
| TestTriggerRebuild | `test_arc_memory_cleanup` |
| TestConcurrency | `test_concurrent_citylots_stress` |
| TestUTF16Escaping | `test_utf16_surrogate_pairs` |
| TestOneEscape | `test_json_escape_all_eight` |
| TestAnythingButMerging | `test_anything_but_*` tests |
| TestWildcardMatching | `test_wildcard_*` tests |
| TestEqualsIgnoreCaseMatching | `test_equals_ignore_case_*` tests |
| TestNfa2Dfa | `test_shellstyle_*` tests |
| TestToxicStack | `test_toxic_stack_arena` |

---

## Next Session: Add Priority Gap Tests

**Goal:** Add the 4 priority gap tests identified above.

### Steps

1. **TestRegexpValidity equivalent**: Add comprehensive Unicode property category tests using XSD regex samples
2. **TestArrayCorrectness equivalent**: Add complex nested array matching test
3. **TestRulerCl2 equivalent**: Add integration test with multiple matcher types
4. **TestFJErrorCases equivalent**: Add JSON error case coverage

---

## Future: Regexp HashMap Fallback Elimination

**Goal:** Eliminate HashMap fallback for `Matcher::Regex` with lookaheads/backreferences, or document as intentional limitation.

### Options to Consider

1. **Document as limitation**: Some regex features fundamentally can't be represented as DFA/NFA
2. **Partial conversion**: Convert what's possible to automaton, fall back only for specific subpatterns
3. **Alternative approach**: Hybrid matching where automaton handles prefix/suffix

## Status

**316 tests passing.** Rust 1.5-2x faster. Synced with Go commit 74475a4 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 405 | 1.8x |
| status_middle_nested | 7,437 | 4,912 | 1.5x |

### Test Coverage Summary

| Category | Coverage | Notes |
|----------|----------|-------|
| Pattern matchers | ~98% | Wildcard/shellstyle/escaping comprehensive |
| Regexp (992 XSD samples) | ~95% | UTF-8 validation + invalid rejection |
| Numbers/Escaping | ~95% | UTF-16 surrogates, all 8 JSON escapes |
| Core/Infrastructure | ~95% | Arc lifecycle + snapshot isolation |
| Concurrency | ~90% | Concurrent read/write + citylots stress |

### Completed Tests (Phase 1 + 2)

**Edge Case Tests:**
- `test_utf16_surrogate_pairs`: All Go emoji combinations (😀💋😺, x💋y, Ж💋中, etc.)
- `test_json_escape_all_eight`: All 8 JSON escapes (`\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t`)
- `test_unicode_member_names`: Field names with escapes and surrogate pairs
- `test_invalid_utf8_dot_rejection`: Regexp dot rejects overlong, surrogates, invalid continuation
- `test_numbits_boundary_values`: Float64 boundaries (subnormal, normal, min/max)
- `test_numbits_to_qnumber_utf8`: 10K random Q-numbers verify UTF-8 valid + ordering

**Arc Architecture Tests:**
- `test_arc_snapshot_isolation`: Clone creates independent snapshot (Go TestCopy)
- `test_arc_concurrent_read_write`: 4 threads, 2 readers + 2 writers (Go TestConcurrencyCore)
- `test_arc_pattern_lifecycle`: Add → match → delete → rebuild → re-add (Go TestBasic)
- `test_arc_field_matcher_sharing`: Same pattern on different IDs
- `test_arc_memory_cleanup`: Delete + rebuild cleans up (Go TestTriggerRebuild)
- `test_concurrent_citylots_stress`: Pattern adds during 10K event matching (Go TestConcurrency)

### Behavioral Differences (Documented)

1. **anything-but single string**: Rust accepts `{"anything-but": "foo"}` (EventBridge compatible), Go requires array. See json.rs:344.

2. **Flattener early termination**: Rust stops parsing once all pattern fields found. Invalid JSON after may not be detected. Intentional for performance. See lib.rs:1532.

## Completeness

**Pattern matchers:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`, `{"cidr"}`, `{"numeric"}`

**Rust-only extensions:**
- `{"numeric": [">=", 0]}` - numeric comparisons
- `{"cidr": "10.0.0.0/24"}` - IP range matching (automaton-based)
- `{"anything-but": 404}` - numeric anything-but
- `{"regexp": "a{2,5}"}` - range quantifiers
- `~d`/`~w`/`~s`, `~p{IsBasicLatin}`, `(?:...)`, `*?`/`+?`/`{n,m}?`

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
    ├── fa_builders.rs  # FA construction (string, prefix, cidr, shellstyle)
    ├── nfa.rs          # traverse_dfa, traverse_nfa
    ├── arena.rs        # StateArena for cyclic NFA
    └── trie.rs         # ValueTrie for O(n) bulk construction
```

### Arc-Based Design (vs Go's Pruner)

Go uses a pruner with explicit delete tracking and periodic rebuilds. Rust uses Arc:

| Go Concept | Rust Equivalent |
|------------|-----------------|
| `prunerStats.Live` | `Arc::strong_count()` on active patterns |
| `prunerStats.Deleted` | Patterns removed from HashMap, Arc refcount drops |
| `Rebuild()` | Clone matcher (snapshot), old Arcs drop when unused |
| `memState` iteration | `patterns: HashMap<X, Vec<Arc<FieldMatcher>>>` |
| `matchSet.addX()` | `Vec<X>` with dedup |

**Key invariants to test:**
- Clone creates independent snapshot (no shared mutation)
- Delete removes from HashMap; Arc drops when last ref gone
- Concurrent reads safe during writes (RwLock on matcher)
- Field matchers shared across patterns via Arc

## HashMap Fallback Status

**Goal:** Eliminate HashMap fallback entirely.

**Remaining:** `Matcher::Regex` with lookaheads/backreferences - low priority

**Completed:** `AnythingButNumeric` (Q-number FA), `Cidr` (IPv4 DFA, IPv6 NFA)

## Commands

```bash
cargo test                    # run all tests
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
- `src/lib.rs` - Public API, Arc-based pattern storage
