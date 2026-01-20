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

## Next Session: Multi-Condition Automaton for Lookaround

**Goal:** Native lookahead/lookbehind support via multi-condition automata. No backtracking, no regex crate fallback.

### Design Philosophy

Inspired by **numbits**: transform the problem domain into something automata can handle.

- Lookarounds are regular languages (unlike backrefs) - they CAN be done with automata
- Use multiple automata + set operations (intersection, difference)
- Match in phases: easy checks first, then verify conditions
- Reject patterns that require unbounded memory (arbitrary backrefs)

### Supported Patterns

| Pattern | Semantics | Implementation |
|---------|-----------|----------------|
| `A(?=B)` | A followed by B | Match A where AB matches |
| `A(?!B)` | A not followed by B | Match A where AB doesn't match |
| `(?<=B)A` | A preceded by B | Match A, verify B before |
| `(?<!B)A` | A not preceded by B | Match A, verify B not before |
| `(?=X)(?=Y)Z` | Multi-condition | All automata must match |
| `(?=.*X)Y` | Contains X and matches Y | Intersection |

### Rejected Patterns (Error at Parse Time)

| Pattern | Error Message |
|---------|---------------|
| `(.+)\1` | "backreference to multi-char group not supported" |
| `(.)(.)~2` | "backreference to group > 1 not supported" |
| `(?P<x>.+)(?P=x)` | "named backreference not supported" |
| Nested lookaround | "nested lookaround not supported" |

Single-char backrefs `(.)~1` remain supported via enumeration (existing).

### Data Structures

```rust
// src/json.rs - New Matcher variant
enum Matcher {
    // ... existing variants ...

    /// Multi-condition matcher: all conditions must be satisfied
    /// Conditions are evaluated in order (fast-fail on first mismatch)
    MultiCondition(MultiConditionMatcher),
}

/// A condition in a multi-condition matcher
enum Condition {
    /// Value must match this automaton
    MustMatch(SmallTable),

    /// Value must NOT match this automaton
    MustNotMatch(SmallTable),

    /// Lookbehind: N bytes before current position must match
    LookBehind { pattern: SmallTable, offset: usize },

    /// Negative lookbehind: N bytes before must NOT match
    NegLookBehind { pattern: SmallTable, offset: usize },
}

struct MultiConditionMatcher {
    /// Primary pattern (what we're actually matching)
    primary: SmallTable,

    /// Additional conditions to verify (evaluated after primary matches)
    conditions: Vec<Condition>,
}
```

### Matching Algorithm

```rust
// src/lib.rs - Matching logic
fn match_multi_condition(matcher: &MultiConditionMatcher, value: &[u8]) -> bool {
    // Phase 1: Check primary pattern (fast path)
    let primary_matches = traverse_nfa(&matcher.primary, value, bufs);
    if primary_matches.is_empty() {
        return false;
    }

    // Phase 2: Verify all conditions
    for condition in &matcher.conditions {
        match condition {
            Condition::MustMatch(automaton) => {
                if !traverse_nfa(automaton, value, bufs).is_empty() {
                    continue;
                }
                return false;
            }
            Condition::MustNotMatch(automaton) => {
                if traverse_nfa(automaton, value, bufs).is_empty() {
                    continue;
                }
                return false;
            }
            Condition::LookBehind { pattern, offset } => {
                // Check bytes before match position
                // Implementation depends on match position tracking
            }
            Condition::NegLookBehind { pattern, offset } => {
                // Verify pattern does NOT match before position
            }
        }
    }
    true
}
```

### Parser Changes

```rust
// src/regexp/parser.rs - Pattern analysis

/// Analyze pattern for lookaround constructs
fn analyze_lookaround(pattern: &str) -> Result<LookaroundAnalysis, RegexpError> {
    // Detect: (?=...), (?!...), (?<=...), (?<!...)
    // Return: primary pattern + conditions
}

/// Transform lookaround pattern to multi-condition form
fn transform_lookaround(analysis: LookaroundAnalysis) -> Result<MultiConditionMatcher, RegexpError> {
    match analysis {
        // A(?=B) -> primary=A, conditions=[MustMatch(AB)]
        // A(?!B) -> primary=A, conditions=[MustNotMatch(AB)]
        // (?<=B)A -> primary=A, conditions=[LookBehind(B, len(B))]
        // (?=X)(?=Y)Z -> primary=Z, conditions=[MustMatch(X), MustMatch(Y)]
    }
}
```

### Implementation Order

#### Phase 1: Parser Detection (~100 lines)
File: `src/regexp/parser.rs`

1. Add `LookaroundType` enum: `LookAhead`, `NegLookAhead`, `LookBehind`, `NegLookBehind`
2. Add `parse_lookaround()` to detect `(?=`, `(?!`, `(?<=`, `(?<!`
3. Track lookaround positions during parsing
4. Reject nested/unsupported patterns with clear errors
5. Add unit tests with the same rigor as quamina (../quamina)

Reference: `fancy-regex/src/parse.rs:895-901` for lookaround detection

#### Phase 2: Data Structures (~50 lines)
File: `src/json.rs`

1. Add `Condition` enum
2. Add `MultiConditionMatcher` struct
3. Add `Matcher::MultiCondition` variant
4. Update `is_automaton_compatible()` to return true for MultiCondition
5. Add unit tests with the same rigor as quamina (../quamina)

#### Phase 3: Pattern Transformation (~100 lines)
File: `src/regexp/parser.rs`

1. `transform_positive_lookahead(prefix, suffix)` -> primary + MustMatch
2. `transform_negative_lookahead(prefix, suffix)` -> primary + MustNotMatch
3. `transform_lookbehind(prefix, behind)` -> primary + LookBehind
4. `transform_multi_lookahead(conditions, pattern)` -> primary + Vec<Condition>
5. Add unit tests with the same rigor as quamina (../quamina)

Reference: `fancy-regex/src/compile.rs:413-471` for transformation logic

#### Phase 4: Matching Logic (~80 lines)
File: `src/lib.rs`

1. Add `match_multi_condition()` function
2. Integrate into `value_matches()` path
3. Ensure conditions are evaluated in order (fast-fail)
4. Add unit tests with the same rigor as quamina (../quamina)

#### Phase 5: Tests (~100 lines)
File: `src/lib.rs` (test module) with the same or higher rigor than quamina (../quamina)

```rust
#[test] fn test_positive_lookahead() // foo(?=bar) matches "foobar" at "foo"
#[test] fn test_negative_lookahead() // foo(?!bar) matches "foobaz" not "foobar"
#[test] fn test_lookbehind()         // (?<=foo)bar matches "foobar" at "bar"
#[test] fn test_neg_lookbehind()     // (?<!foo)bar matches "xxxbar" not "foobar"
#[test] fn test_multi_lookahead()    // (?=.*[A-Z])(?=.*[0-9]).+ password style
#[test] fn test_rejected_backref()   // (.+)\1 returns error
```

See quamina-rs/src/regexp_samples.rs and how its used within lib.rs as well.

### Performance Targets

Benchmark all regex against `regex` crate (in `../regex`), esp but not limited to:

| Pattern | Target | Notes |
|---------|--------|-------|
| Simple lookahead | <2x regex | Most common case |
| Multi-condition (3) | <3x regex | Linear in conditions |
| Negative lookahead | <2x regex | Single extra check |

Run: `cargo bench lookahead` (to be added)

Also continue to benchmark against quamina's qo repo as well. 

### Error Messages

Clear, actionable errors for unsupported patterns:

```
"backreference `\\1` to multi-character group not supported; only single-char groups like `(.)\1` work"
"nested lookaround not supported: `(?=...(?=...)...)`"
"variable-length lookbehind not yet supported: `(?<=a+)`; use fixed-length like `(?<=aaa)`"
```

### Already Implemented: Single-Char Backreference Enumeration

Patterns like `(.)~1` are transformed at parse time in `src/regexp/parser.rs`:

```
(.)~1      -> aa|bb|cc|...|~~ (95 branches)
([abc])~1  -> aa|bb|cc (3 branches)
x(.)~1y    -> xaay|xbby|...x~~y
```

This remains the only backref support - multi-char backrefs are rejected. We will take this out as a supported feature and then deprecated HashMap-based fallback patterns and any associated deadcode. 

### References

- **fancy-regex** (`../fancy-regex`): Parser structure, lookaround detection
- **regex** (`../regex`): Performance baseline, automaton patterns
- **event-ruler** (`../event-rule`): Spiritual successor to Quamina
- **POPL 2024**: Oracle-NFA for O(m×n) lookarounds
- **numbits** (`src/numbits.rs`): Transform-to-automaton philosophy

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
