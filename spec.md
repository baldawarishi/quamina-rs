# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**336 tests passing.** Rust 1.5-2x faster than Go. Synced with Go commit 74475a4 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 430 | 1.7x |
| status_middle_nested | 7,437 | 5,400 | 1.4x |

**Pattern matchers:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`, `{"cidr"}`, `{"numeric"}`

**Rust-only extensions:** `{"numeric": [">=", 0]}`, `{"cidr": "10.0.0.0/24"}`, `{"anything-but": 404}`, `{"regexp": "a{2,5}"}`, `~d`/`~w`/`~s`, `~p{IsBasicLatin}`, `(?:...)`, lazy quantifiers

---

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum, MultiConditionPattern
├── flatten_json.rs     # Streaming JSON flattener
├── regexp/
│   ├── parser.rs       # I-Regexp parser + lookaround detection
│   └── nfa.rs          # NFA building + shell caching
└── automaton/
    ├── small_table.rs  # SmallTable (byte transitions)
    ├── fa_builders.rs  # FA construction (string, prefix, cidr, shellstyle)
    ├── nfa.rs          # traverse_dfa, traverse_nfa
    ├── arena.rs        # StateArena for cyclic NFA
    ├── mutable_matcher.rs # Pattern building, MultiCondition handling
    └── trie.rs         # ValueTrie for O(n) bulk construction
```

---

## Lookaround Implementation Status

### Completed

| Phase | Description | Status |
|-------|-------------|--------|
| 1 | Parser detection (`(?=`, `(?!`, `(?<=`, `(?<!`) | ✅ Done |
| 2 | Data structures (`LookaroundCondition`, `MultiConditionPattern`) | ✅ Done |
| 3 | Pattern transformation (A(?=B) → primary + conditions) | ✅ Done |
| 4 | Primary automaton matching | ✅ Done |
| 5 | Lookaround tests (parsing, transformation, primary match) | ✅ Done |
| 6 | Lookahead condition verification | ✅ Done |
| 7 | Lookbehind condition verification | ✅ Done |
| 9 | End-to-end lookaround tests | ✅ Done |

### Implementation Details

**Condition verification** implemented in `mutable_matcher.rs` and `thread_safe.rs`:
- For lookahead `A(?=B)`: combined pattern AB must match the full value
- For negative lookahead `A(?!B)`: combined pattern AB must NOT match the full value
- For lookbehind `(?<=B)A`: combined pattern BA must match the full value
- For negative lookbehind `(?<!B)A`: combined pattern BA must NOT match the full value

**Key data structures:**
- `ConditionNfa`: arena NFA + is_negative flag for condition verification
- `MultiConditionNfa`: primary arena NFA + list of condition NFAs
- `multi_condition_nfas` field in `MutableValueMatcher` and `FrozenValueMatcher`

---

## Next Session: Cleanup

### Phase 8: Remove HashMap Fallback + regex Dependency (~50 lines deleted)
Files: `src/json.rs`, `src/lib.rs`, `Cargo.toml`

**Goal:** Remove `Matcher::Regex(regex::Regex)` variant, all fallback code, and `regex` crate dependency.

1. Remove `Matcher::Regex` enum variant from `src/json.rs`
2. Remove fallback path in `parse_regexp_value()`
3. Remove `regex = "1"` from `Cargo.toml`
4. Update `is_automaton_compatible()` (can simplify or remove)
5. Single-char backrefs `(.)~1` stay (enumeration-based, no regex needed)
6. Multi-char backrefs become hard errors

**Breaking changes:**
- Patterns that relied on regex fallback will now fail at parse time
- This is acceptable - the fallback had wrong semantics for most patterns anyway
- Removes `regex` crate dependency (only used for fallback)

---

## Data Structures Reference

```rust
// src/json.rs
pub enum LookaroundCondition {
    PositiveLookahead(RegexpRoot),      // (?=...) - combined pattern must match
    NegativeLookahead(RegexpRoot),      // (?!...) - combined pattern must NOT match
    PositiveLookbehind { pattern: RegexpRoot, byte_length: usize }, // (?<=...)
    NegativeLookbehind { pattern: RegexpRoot, byte_length: usize }, // (?<!...)
}

pub struct MultiConditionPattern {
    pub primary: RegexpRoot,                    // What we match
    pub conditions: Vec<LookaroundCondition>,   // What we verify (cost-sorted)
}

// src/automaton/mutable_matcher.rs
pub struct ConditionNfa {
    pub arena: StateArena,
    pub start: StateId,
    pub is_negative: bool,  // true for (?!...) and (?<!...)
}

pub struct MultiConditionNfa {
    pub primary_arena: StateArena,
    pub primary_start: StateId,
    pub field_matcher_ptr: *const FieldMatcher,
    pub conditions: Vec<ConditionNfa>,
}

pub enum Matcher {
    // ... existing variants ...
    MultiCondition(MultiConditionPattern),
    Regex(regex::Regex),  // TO BE REMOVED in Phase 8
}
```

---

## Rejected Patterns (Error at Parse Time)

| Pattern | Error |
|---------|-------|
| `(.+)\1` | "backreference to multi-char group not supported" |
| `(.)(.)~2` | "backreference to group > 1 not supported" |
| `(?=...(?=...))` | "nested lookaround not supported" |
| `(?<=a+)b` | "variable-length lookbehind not supported" |

Single-char backrefs `(.)~1` work via enumeration (95 branches for `.`, 3 for `[abc]`).

---

## Behavioral Differences from Go

1. **anything-but single string**: Rust accepts `{"anything-but": "foo"}`, Go requires array
2. **Flattener early termination**: Rust stops parsing once all pattern fields found

---

## Commands

```bash
cargo test                    # run all tests (336)
cargo bench                   # all benchmarks
cargo clippy -- -D warnings   # lint
gh run list                   # check CI
```

---

## Session Checklist

1. Read this spec
2. For Go behavior, read Go source directly (../quamina)
3. Push often, check CI (`gh run list`)
4. Run `cargo test` after each phase

**Key files for lookaround:**
- `src/json.rs` - `MultiConditionPattern`, `transform_lookaround_pattern()`
- `src/regexp/parser.rs` - `LookaroundType`, lookaround parsing
- `src/automaton/mutable_matcher.rs` - `ConditionNfa`, `MultiConditionNfa`, `add_multi_condition_transition()`
- `src/automaton/thread_safe.rs` - Condition verification in `transition_on()`

**References:**
- `../fancy-regex` - Lookaround implementation reference
- `../regex` - Performance patterns
- `src/numbits.rs` - Transform-to-automaton philosophy
