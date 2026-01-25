# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**349 tests passing.** Rust 1.5-2x faster than Go. Synced with Go commit 74475a4 (Jan 2026).

| Benchmark | Go (ns) | Rust (ns) | Speedup |
|-----------|---------|-----------|---------|
| citylots | 3,971 | 2,117 | 1.9x |
| shellstyle_26_patterns | 731 | 454 | 1.6x |
| status_middle_nested | 7,437 | 5,400 | 1.4x |

**Matchers:** `"value"`, `{"prefix"}`, `{"suffix"}`, `{"wildcard"}`, `{"shellstyle"}`, `{"exists"}`, `{"anything-but"}`, `{"equals-ignore-case"}`, `{"regexp"}`, `{"cidr"}`, `{"numeric"}`

**Rust extensions:** `{"numeric": [">=", 0]}`, `{"anything-but": 404}`, `{"regexp": "a{2,5}"}`, `~d`/`~w`/`~s`, `~p{IsBasicLatin}`, `(?:...)`, lazy quantifiers, lookarounds

---

## Architecture

```
src/
├── lib.rs              # Public API: Quamina, QuaminaBuilder
├── json.rs             # Pattern parsing, Matcher enum
├── flatten_json.rs     # Streaming JSON flattener
├── regexp/
│   ├── parser.rs       # I-Regexp parser + lookaround
│   └── nfa.rs          # NFA building
└── automaton/
    ├── small_table.rs  # SmallTable (byte transitions)
    ├── fa_builders.rs  # FA construction
    ├── nfa.rs          # traverse_dfa, traverse_nfa
    ├── arena.rs        # StateArena for cyclic NFA
    ├── mutable_matcher.rs # Pattern building
    └── trie.rs         # ValueTrie for bulk construction
```

---

## Performance Optimization Roadmap

Based on analysis of `../regex` (regex-automata crate). See that repo for implementation reference.

### Phase 1: Quick Wins ✅ COMPLETE

| Task | File | Impact | Status |
|------|------|--------|--------|
| Fix epsilon closure O(n²)→O(n) | `automaton/nfa.rs` | 10-18% NFA speedup | ✅ Done - uses `FxHashSet<StatePtr>` |
| Add SparseSet | `automaton/sparse_set.rs` | Future use | ✅ Done - O(1) clear via generation counter |

**Tradeoff:** Shellstyle patterns regressed ~6% (small epsilon closures where HashSet overhead > Vec scan). Still 1.6x faster than Go. See `nfa.rs` comments for hybrid approach and architectural fix options.

### Phase 2: Byte Classes (Not Implemented)

**Problem:** SmallTable uses up to 256 ceiling/step entries.
**Proposed solution:** Compute byte equivalence classes at pattern compile time.

Pattern `[a-z]+` would only need 3 classes: `[0-96]`, `[97-122]`, `[123-255]`

```rust
// Proposed approach (not implemented)
pub struct ByteClasses([u8; 256]);  // maps byte → class ID
impl SmallTable {
    fn dense_step(&self, byte: u8);  // O(1) lookup via byte_classes[byte]
}
```

**Why not implemented?** Benchmarking showed adding the dense lookup check to `SmallTable::step()` caused ~6% regression for shellstyle patterns. The overhead of `if let Some(ref dense) = self.dense_steps` on every step hurt simple patterns with 1-3 transitions per state. The current ceiling scan is already O(n) where n is typically 1-5 for most patterns.

**When would it help?** Patterns with many transitions per state:
- Character classes like `[a-zA-Z0-9]`
- Merged automata with many overlapping patterns
- Unicode category patterns

**Future approaches if needed:**
1. Adaptive based on table complexity (e.g., only densify if >4 transitions)
2. Compile-time feature flag to opt-in
3. Build separate DFA with dense tables for specific hot paths
4. Use byte classes only for DFA construction, not runtime lookup

**Reference:** `regex-automata/src/util/alphabet.rs:185-230`

### Phase 3: State Acceleration (1 week)

**Problem:** Wildcard patterns like `[^,]+` check every byte.
**Solution:** Use memchr SIMD to skip to exit bytes.

```rust
pub struct AccelInfo {
    exit_bytes: [u8; 3],  // bytes that leave this state
    len: u8,              // 0 = not accelerated, 1-3 = use memchr
}

// In traverse_nfa, when state is accelerated:
if let Some(accel) = state.accel() {
    let skip = match accel.len {
        1 => memchr::memchr(accel.exit_bytes[0], &val[pos..]),
        2 => memchr::memchr2(accel.exit_bytes[0], accel.exit_bytes[1], &val[pos..]),
        3 => memchr::memchr3(...),
        _ => None,
    };
    if let Some(n) = skip { pos += n; }
}
```

**Impact:** 5-100x speedup on repetitive patterns.
**Reference:** `regex-automata/src/dfa/accel.rs:90-130`
**Dependency:** Add `memchr` crate.

### Phase 4: Prefilter Infrastructure (2 weeks)

**Problem:** Full automaton runs on every field value.
**Solution:** Fast literal prefix search before automaton.

```rust
pub enum Prefilter {
    None,
    Memchr(u8),                    // Single byte literal
    Memchr2(u8, u8),               // Two alternatives
    Memmem(Vec<u8>),               // Literal string
    AhoCorasick(aho_corasick::AhoCorasick), // Multiple literals
}

// Before automaton traversal:
if let Some(prefilter) = value_matcher.prefilter() {
    if !prefilter.might_match(val) {
        return; // Skip automaton entirely
    }
}
```

**Impact:** 2-10x overall speedup.
**Reference:** `regex-automata/src/util/prefilter/`
**Dependency:** Add `aho-corasick` crate.

---

## What quamina-rs Already Does Well

1. **Arena-based NFAs** (`arena.rs`) - regex-automata doesn't have this
2. **SegmentsTree** - Skip JSON fields not in any pattern
3. **SmallTable ceiling/steps** - Similar to regex-automata sparse transitions

---

## Rejected Patterns (Parse-Time Errors)

| Pattern | Error |
|---------|-------|
| `(.)~1` | backreferences not supported |
| `(?=(?=...))` | nested lookaround not supported |
| `(?<=a+)b` | variable-length lookbehind not supported |

---

## Behavioral Differences from Go

1. `{"anything-but": "foo"}` - Rust accepts single string, Go requires array
2. Flattener stops parsing once all pattern fields found

---

## Commands

```bash
cargo test                    # 337 tests
cargo bench                   # benchmarks
cargo clippy -- -D warnings   # lint
gh run list                   # CI status
```

---

## Session Checklist

1. Read this spec
2. For Go behavior: `../quamina`
3. For regex-automata patterns: `../regex/regex-automata/src/`
4. Run `cargo test` after each change
5. Push often, check CI

**Key optimization files:**
- `src/automaton/nfa.rs` - epsilon closure, traversal
- `src/automaton/small_table.rs` - byte transitions
- `src/automaton/arena.rs` - cyclic NFA (already good)

**Reference repos:**
- `../regex` - regex-automata performance patterns
- `../quamina` - Go reference implementation
