# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Go Test Gap Analysis (Completed)

**Summary:** Analyzed 134 Go tests across 40 files. All covered or intentionally skipped (with Rust equivalent rigor).

### Priority Gaps (Should Add)

*None remaining* - all gaps analyzed and either covered or intentionally skipped.

**Already Covered:**
- `test_regexp_validity` - 992 XSD samples (skips `[a-[b]]`, `~b/~B` unimplemented)
- `test_array_cross_element_comprehensive` - exact port of TestArrayCorrectness (bands JSON, same-element AND logic)
- `test_stress_citylots2_operators` - exact port of TestRulerCl2 (exact, prefix, anything-but, shellstyle, equals-ignore-case, regexp)
- `test_invalid_json_events` + `test_bad_event_error_handling` - covers TestFJErrorCases; Rust rejects control chars (0x00-0x1f) in field names/values, remaining cases caught by early termination (documented at lib.rs:1537)

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

**Rust Equivalent Rigor for Skipped Go Tests:**

| Go Concern | Rust Tests |
|------------|------------|
| Pruner stats/triggers | `test_pruner_stats`, `test_should_rebuild_threshold`, `test_rebuild_after_delete`, `test_rebuild_zero_filtered_denominator` |
| Concurrency | `test_arc_concurrent_read_write`, `test_concurrent_update_during_matching`, `test_concurrent_citylots_stress`, `test_send_sync` |
| Memory/lifecycle | `test_arc_pattern_lifecycle`, `test_arc_memory_cleanup`, `test_arc_snapshot_isolation`, `test_arc_field_matcher_sharing` |
| SmallTable | `test_small_table_step`, `test_small_table_with_mappings` |
| NFA/Arena | `test_arena_alloc`, `test_arena_cyclic_reference`, `test_arena_state_count_vs_chain`, `test_arena_nfa_*` |
| SegmentsTree | `test_segments_tree_tracker_impl` |

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

## Benchmark Investigation (Jan 2026) - Resolved

Investigated reported regressions via git bisect:
- `has_matches_early_exit`: 188 ns (stable, close to baseline 176 ns)
- `multi_field_and_3_fields`: 299 ns (improved from 650 ns historical baseline; "189 ns" was measurement error)

**Conclusion:** No regressions. All core benchmarks stable.

---

## Regex Fallback: Reality Check (Jan 2026)

### What the `regex` Crate Actually Supports

The Rust `regex` crate explicitly does **NOT** support:
- **Backreferences** (`\1`, `(.+)\1`) - "requires exponential time in worst case"
- **Lookaheads** (`(?=...)`, `(?!...)`) - not implemented
- **Lookbehinds** (`(?<=...)`, `(?<!...)`) - not implemented

### Current Fallback Behavior

```
Pattern: (.)~1 or (?=foo)bar
    ↓
parse_regexp() fails
    ↓
regex::Regex::new() called with same string
    ↓
Either: fails (lookahead not supported)
Or: succeeds with WRONG semantics (~1 treated as literal "~1")
```

The fallback is only useful for:
- Patterns using `\` escapes that our I-Regexp parser (`~` escapes) treats as literals
- Edge cases in syntax our parser doesn't handle

**In practice**: Most patterns that fail our parser also fail the `regex` crate.

### Strategy: Native Transformations

Instead of relying on fallback, we transform patterns to automaton-compatible forms:

| Pattern | Transformation | Status |
|---------|---------------|--------|
| `(.)~1` | `aa\|bb\|cc\|...` enumeration | ✅ Implemented |
| `([abc])~1` | `aa\|bb\|cc` enumeration | ✅ Implemented |
| `foo(?=bar)` | Two-automaton intersection | Planned |
| `foo(?!bar)` | Automaton + exclusion | Planned |

### If Full Support Needed

To support complex backreferences/lookaheads, would need:
- `fancy-regex` crate (supports both, but slower)
- Or continue extending native transformations

---

## Future: Optional Enhancements

1. **Character class subtraction** `[a-[b]]` - XSD regex feature, unimplemented
2. **Word boundaries** `~b/~B` - Not in I-Regexp spec, unimplemented
3. **Simple lookahead transformation** - Convert `A(?=B)` to automaton intersection

## Status

**321 tests passing.** Rust 1.5-2x faster. Synced with Go commit 74475a4 (Jan 2026).

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

## Matcher Implementation Status

| Matcher | Implementation | Notes |
|---------|---------------|-------|
| Exact, Prefix, Suffix | Automaton | Core matchers |
| Wildcard, Shellstyle | Automaton | NFA-based |
| EqualsIgnoreCase | Automaton | Case-folded FA |
| AnythingBut (string) | Automaton | Complement construction |
| AnythingButNumeric | Automaton | Q-number FA |
| Numeric comparisons | Automaton | Q-number range FA |
| Cidr (IPv4) | Automaton | DFA-based |
| Cidr (IPv6) | Automaton | NFA-based |
| ParsedRegexp (I-Regexp) | Automaton | Custom parser → NFA |
| Regex (lookahead/backref) | HashMap fallback | Uses `regex` crate - see research section |

## Commands

```bash
cargo test                    # run all tests
cargo bench status            # benchmarks
cargo clippy -- -D warnings   # CI check
gh run list                   # check CI
cargo fmt && git push         # format and push
```

## Next Session: Graceful Regex Integration

**Goal:** Reduce HashMap fallback usage by converting simple lookahead/backreference patterns to automaton-compatible forms.

### Completed: Single-Char Backreference Enumeration

Implemented in `src/regexp/parser.rs`. Patterns like `(.)~1` are transformed at parse time:

```
Input:  (.)~1
Output: aa|bb|cc|...|~~ (95 branches for printable ASCII)

Input:  ([abc])~1
Output: aa|bb|cc (character class branches)

Input:  x(.)~1y
Output: xaay|xbby|...x~~y (with prefix/suffix)
```

**Supported:**
- `(.)~1` - dot repeated (printable ASCII)
- `([...])~1` - character class repeated
- Prefix/suffix like `x(.)~1y`

**Not supported (fails with error):**
- Multiple backrefs: `(.)~1~1`
- Group > 1: `(.)(.)~2`
- Multi-char groups: `(abc)~1`
- Quantified groups: `(.)+~1`
- Nested backrefs: `((.)~1)`

### Implementation Order (remaining)

#### 1. Lookahead at Pattern End
Convert `prefix(?=suffix)` to two-automaton check.

```
Input:  foo(?=bar)
Logic:  Match "foo", then verify "bar" follows (without consuming)
```

**Approach:**
- Detect lookahead at end of pattern: `A(?=B)$` or `A(?=B)` where nothing follows
- Build two automata: one for A, one for AB
- Match reports A's position if AB also matches at same start

**Where to implement:**
- `src/regexp/parser.rs` - Detect simple lookahead patterns
- `src/json.rs` - New `Matcher::LookaheadPair(Automaton, Automaton)` variant
- `src/lib.rs` - Matching logic for pair

#### 2. Negative Lookahead at End
Convert `prefix(?!suffix)` to automaton + exclusion.

```
Input:  foo(?!bar)
Logic:  Match "foo" where "bar" does NOT follow
```

**Approach:**
- Build automaton for `foo`
- Build automaton for `foobar`
- Match `foo` positions that don't have `foobar` match

#### 3. Password-Style Multi-Lookahead
Convert `(?=.*A)(?=.*B).*` to multi-condition AND.

```
Input:  (?=.*[A-Z])(?=.*[0-9]).{8,}
Logic:  Contains uppercase AND contains digit AND length >= 8
```

**Approach:**
- Detect pattern of multiple lookaheads at start
- Extract conditions as separate automata
- Match requires ALL conditions satisfied

### Research References

- **POPL 2024** (lookaheads): https://dl.acm.org/doi/10.1145/3632934
  - Oracle-NFA achieves O(m×n) for lookarounds
  - Pre-compute oracle table of match positions

- **EPFL** (rust implementation): https://systemf.epfl.ch/blog/rust-regex-lookbehinds/
  - Added linear-time lookbehinds to rust-lang/regex
  - Two-pass approach: compute lookbehind table, then match

### Key Files to Modify

| File | Purpose |
|------|---------|
| `src/regexp/parser.rs` | Pattern detection + transformation |
| `src/json.rs` | New Matcher variants |
| `src/lib.rs:is_automaton_compatible()` | Route transformed patterns to automaton |
| `src/automaton/fa_builders.rs` | Build automata for transformed patterns |

---

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
