# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Next Session: Go Test Gap Analysis

**Goal:** Identify missing tests from Go that we should add, and document tests we intentionally skip.

### Approach

Use subtasks to parallelize analysis. Read Go test source code directly - don't trust summaries.

### Steps

1. **List Go tests**: `grep -h "^func Test" ~/Development/quamina_go_rs/quamina/*_test.go`
2. **List Rust tests**: `grep -h "fn test_" src/**/*.rs`
3. **For each Go test file**, spawn subtask to:
   - Read the actual test code
   - Check if Rust has equivalent coverage
   - Categorize: Covered / Should Add / Won't Cover / Shouldn't Cover

### Categories

| Category | Criteria |
|----------|----------|
| **Covered** | Rust has equivalent test for same behavior |
| **Should Add** | User-facing behavior not tested in Rust |
| **Won't Cover** | Go internal implementation detail (e.g., specific data structure tests) |
| **Shouldn't Cover** | Different Rust architecture (e.g., Go pruner vs Rust Arc) |

### Deliverable

Update this spec with table of intentionally skipped Go tests and rationale.

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
