# quamina-rs

Rust port of [quamina](https://github.com/timbray/quamina) - fast pattern-matching for JSON events.

## Status

**347 tests passing.** Rust 1.5-2x faster than Go. Synced with Go commit e3d13cd (Jan 2026).

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

### Phase 3: State Acceleration ⚡ PARTIAL

**Problem:** Wildcard/regexp patterns check every byte even when most don't cause transitions.

**Solution:** Use memchr SIMD to skip directly to exit bytes.

#### 3a. Wildcard Acceleration ✅ COMPLETE

Implemented for `traverse_nfa` with single spinout state.

```rust
// In traverse_nfa:
if bufs.current_states.len() == 1 {
    if let Some(skip) = try_accelerate(&table, &val[i..]) {
        i += skip;
        continue;
    }
}
```

**Results (suffix pattern `*X`):**
| String | Without | With | Speedup |
|--------|---------|------|---------|
| 10k | 263 µs | 4.2 µs | **62x** |
| 1k | 26.8 µs | 676 ns | **40x** |

#### 3b. Arena NFA Acceleration ⚠️ IMPLEMENTED BUT DISABLED

**Problem:** `traverse_arena_nfa` (used for regexp `+`/`*`) has no acceleration.

**Implementation status:**
1. ✅ Added `accel: Option<AccelInfo>` to `ArenaSmallTable`
2. ✅ Added `compute_accel()` method and `compute_loop_accel()` for regexp building
3. ✅ Added `try_accelerate_arena()` to `arena.rs`
4. ⚠️ Check in `traverse_arena_nfa` is DISABLED (causes ~4% regression with no benefit)

**Why disabled:** Unicode-aware patterns have too many "exit bytes" for acceleration to work.

For a pattern like `[^x]+`, the FA must reject:
- 'x' (0x78) - the negated character
- Invalid UTF-8 lead bytes (0x80-0xC1) - these can't start valid UTF-8
- VALUE_TERMINATOR (0xF5)

This results in 68+ exit bytes, far exceeding the 3-byte limit for memchr acceleration.
Similarly, `[a-z]+` has 230+ exit bytes (all non-letter bytes including invalid UTF-8).

**When it would help:** Patterns with 1-3 specific exit bytes AND valid UTF-8 everywhere else.
Such patterns are rare with full Unicode support.

**Benchmark (unchanged):** `regexp_plus_long` = 3.6 µs, `regexp_star_long` = 3.7 µs

**Files:** `src/automaton/arena.rs`, `src/regexp/nfa.rs`

#### 3c. Multi-State Acceleration ❌ TODO (lower priority)

**Problem:** Current acceleration requires `current_states.len() == 1`.

**Approach:** When all active states have same exit bytes, can still accelerate.

```rust
// Proposed:
fn common_exit_bytes(states: &[Arc<FaState>]) -> Option<AccelInfo> {
    // Return Some if all states share same exit bytes
}
```

**Reference:** `regex-automata/src/dfa/accel.rs`
**Dependency:** `memchr` crate (2.7) ✅ added

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

## Go-Inspired Optimizations

Reviewed Go commits from Jan 2026. These are algorithmic improvements from the Go codebase.

### Phase 5: Batch ArrayPos Allocation ✅ EVALUATED - NOT NEEDED

**Problem:** In Go, each field clones `arrayTrail` slice, causing heap allocations per array element.
Go profiler identified `storeArrayElementField` as "the most expensive function in the whole matchesForJSONEvent universe."

**Go solution (commit 4cf827d):** Single growing buffer, slice into it per field.

**Why not needed in Rust:** The Rust implementation uses `SmallVec<[ArrayPos; 4]>` which stores up to 4 elements inline (32 bytes, no heap). For typical JSON (≤4 levels of array nesting), `clone()` is just a memcpy with no allocation. This is fundamentally different from Go's slice semantics where every clone allocates.

**Benchmark results (Jan 2026):**
- `array_heavy_100_elements`: 2.89 µs (100 array elements, ~29 ns/element)
- `deep_nesting_with_arrays`: 322 ns (4 array elements in nested structure)
- Both faster than equivalent Go operations

**When to reconsider:** If profiling shows array_trail cloning as a hotspot for deeply nested arrays (>4 levels). The optimization would require significant API changes to Field struct.

### Phase 6: SkinnyRuneTree ✅ EVALUATED - NOT NEEDED

**Problem:** Rune range FA building uses dense 256-entry `Vec<Option<RuneTreeEntry>>` per UTF-8 byte level.

**Go solution (commit cc81a11):** Sparse byte/entry pairs instead of dense array.

```rust
// Go approach (sparse):
struct SkinnyRuneTreeNode {
    byteVals: Vec<u8>,
    entries: Vec<SkinnyRuneTreeEntry>,
}

// Rust keeps (dense):
type RuneTreeNode = Vec<Option<RuneTreeEntry>>;  // 256 entries per node
```

**Experiment results (Jan 2026):**
| Metric | Dense | Skinny | Diff |
|--------|-------|--------|------|
| Unicode ~p{L} compile | 848 µs | 865 µs | +2% slower |
| Matching performance | same | same | — |

**Why not needed in Rust:** Rust's range-based building (`add_rune_pair_tree_entry`) adds entire character ranges to the tree without iterating through individual code points. This creates O(log(ranges)) tree entries regardless of storage format. Go iterates individual runes, creating O(n) entries where sparse storage helps.

Both implementations produce **identical SmallTables** after packing, so matching performance is identical. The memory difference is only in transient tree construction, which is dwarfed by final SmallTable allocation.

**Decision:** Dense tree only. Sparse approach not beneficial in Rust due to range-based building.

### Phase 7: Comprehensive Shellstyle Benchmark ✅ COMPLETE

**Problem:** Current `bench_shellstyle_alphabet` uses 26 letter patterns and one event. Go's `BenchmarkShellstyleMultiMatch` is much more thorough.

**Implemented in `benches/matching.rs`:**
- 16 letter patterns (A* through P*)
- 4 "funky" multi-wildcard patterns (`*E*E*E*`, `*A*B*`, `*N*P*`, `*O*O*O*`)
- 5 CJK patterns (`*東京*`, `新*`, `*北京*`, `上海*`, `*서울*`)
- 4 emoji patterns (`*🎉*`, `🚀*`, `*❤️*`, `*🌟*🎯*`)
- 31 events: ASCII streets, CJK streets, emoji streets, mixed

**Benchmark results (Jan 2026):**
- `shellstyle_multi_match`: 92.5 µs (31 events, ~3 µs/event avg)
- `shellstyle_26_patterns`: 452 ns (single ASCII event)

**Reference:** `../quamina/shellstyle_bench_test.go`

### Reviewed but Not Applicable

| Go Change | Why Not Applicable |
|-----------|-------------------|
| `sync.Pool` for nfaBuffers | Rust uses `Mutex<NfaBuffers>` per Quamina instance - already reuses |
| Lazy nfaBuffers init | Minor benefit; all fields typically used |
| Transmap preallocation | Different architecture (Vec<Arc<FieldMatcher>>) |
| Flattener loop micro-opts | Rust iterators handle differently; would need profiling |
| MatchSet reuse | Already done via FrozenMatchSet pattern |

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
cargo test                    # 347 tests
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
