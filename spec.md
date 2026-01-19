# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Next Session: Edge Case Tests + Arc Architecture Tests

**Goal:** Add missing edge case tests and Arc-based architecture tests before Regexp work.

### Phase 1: Edge Case Tests (Target: ~100% user-facing coverage)

| Test | Description | Priority |
|------|-------------|----------|
| `test_utf16_surrogate_pairs` | Emoji via `\ud83d\ude00` → 😀, multi-emoji, mixed | High |
| `test_json_escape_all_eight` | All 8 escapes: `\"`, `\\`, `\/`, `\b`, `\f`, `\n`, `\r`, `\t` | High |
| `test_unicode_member_names` | Field names with escapes: `{"😀": 1, "x\u0078y": 2}` | High |
| `test_invalid_utf8_dot_rejection` | Dot (.) rejects invalid UTF-8: `(0xC0, 0x80)`, `(0xED, 0xA0, 0x80)` | Medium |
| `test_numbits_boundary_values` | Float64 boundaries: zero, subnormal, normal ranges | Medium |
| `test_numbits_to_qnumber_utf8` | 10K random numbits → Q-number, verify UTF-8 valid + ordering | Low |

**Reason for each:**
- UTF-16/escapes: Go has comprehensive tests, we should match
- Invalid UTF-8: Security edge case, ensures bad input rejected
- Numbits boundaries: Numeric matching correctness at extremes
- Numbits UTF-8: Internal invariant verification

### Phase 2: Arc Architecture Tests (Match Go's internal depth)

Go has detailed tests for pruner/memState/matchSet internals. We need equivalent depth for our Arc-based design:

| Test | Description | Go Equivalent |
|------|-------------|---------------|
| `test_arc_snapshot_isolation` | Clone creates independent snapshot, mutations don't leak | `TestCopy` |
| `test_arc_concurrent_read_write` | 4 threads: 2 readers + 2 writers, no data races | `TestConcurrencyCore` |
| `test_arc_pattern_lifecycle` | Add → match → delete → rebuild → verify deleted gone | `TestBasic` |
| `test_arc_field_matcher_sharing` | Same pattern on different IDs shares fieldMatcher Arc | (internal) |
| `test_arc_memory_cleanup` | After delete+rebuild, Arc refcounts drop, memory freed | `TestTriggerRebuild` |
| `test_concurrent_citylots_stress` | Pattern adds during 10K event matching, validate counts | `TestConcurrency` |

**Why Arc tests matter:**
- Go's pruner ensures deleted patterns are cleaned up; our Arc design relies on refcounting
- Concurrent safety is implicit in Rust but explicit tests catch logic bugs
- Snapshot isolation is key API guarantee for users

### Phase 3: Regexp HashMap Fallback Elimination

After tests pass, proceed to:
- Goal: Eliminate HashMap fallback for `Matcher::Regex`
- Currently regex with lookaheads/backreferences falls back to HashMap
- Investigate automaton conversion or document as intentional limitation

## Status

**304 tests passing.** Rust 1.5-2x faster. Synced with Go commit 74475a4 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 405 | 1.8x |
| status_middle_nested | 7,437 | 4,912 | 1.5x |

### Test Coverage Summary

| Category | Coverage | Notes |
|----------|----------|-------|
| Pattern matchers | ~95% | Wildcard/shellstyle excellent |
| Regexp (992 XSD samples) | ~90% | Minor UTF-8 edge cases |
| Numbers/Escaping | ~85% | UTF-16 surrogates missing |
| Core/Infrastructure | ~85% | Arc tests pending |
| Concurrency | ~60% | Scale stress tests pending |

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
