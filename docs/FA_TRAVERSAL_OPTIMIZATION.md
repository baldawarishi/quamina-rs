# FA Traversal Optimization for Numeric Matching

## Overview

Investigate whether alternative FA traversal strategies can improve numeric matching performance. The Q-number optimization reduced conversion overhead; now FA traversal is the dominant cost.

## Background

From Q-number optimization profiling (2026-02-03):
- FA traversal: ~50% of CPU time
- Q-number conversion: ~20% (now optimized to near-zero)
- With Q-number optimized, FA traversal is now the primary bottleneck

### Current Implementation

**DFA Traversal** (`src/automaton/nfa.rs:18-36`):
```rust
pub fn traverse_dfa(table: &SmallTable, val: &[u8], transitions: &mut Vec<Arc<FieldMatcher>>) {
    let mut current_table = table;
    for i in 0..=val.len() {
        let byte = if i < val.len() { val[i] } else { VALUE_TERMINATOR };
        match current_table.dstep(byte) {
            Some(next) => {
                transitions.extend(next.field_transitions.iter().cloned());
                current_table = &next.table;
            }
            None => break,
        }
    }
}
```

**`dstep()` method** (`src/automaton/small_table.rs:265-272`):
```rust
pub fn dstep(&self, utf8_byte: u8) -> Option<&Arc<FaState>> {
    for (i, &ceiling) in self.ceilings.iter().enumerate() {
        if utf8_byte < ceiling {
            return self.steps[i].as_ref();
        }
    }
    None
}
```

**SmallTable structure**:
- Uses compressed representation: `ceilings: Vec<u8>` + `steps: Vec<Option<Arc<FaState>>>`
- Not a flat 256-byte lookup array
- Memory-efficient but O(n) lookup per byte

### Numeric Matching Path

For a numeric value like `50`:
1. Q-number conversion: `50` → `[128, 133, 128]` (3 bytes, stack-allocated)
2. DFA traversal: 4 iterations (3 bytes + VALUE_TERMINATOR)
3. Each iteration: Linear scan through ceilings

## Hypothesis

The linear scan in `dstep()` is suboptimal for numeric matching where:
- Q-numbers are 1-10 bytes (average 6.09 bytes from citylots)
- Each byte requires a ceiling scan
- Total scans per match: 2-11 (bytes + terminator)

## Approaches to Compare

### Approach A: Current (Baseline)
```rust
// Linear scan through ceilings
for (i, &ceiling) in self.ceilings.iter().enumerate() {
    if utf8_byte < ceiling { return self.steps[i].as_ref(); }
}
```
- ✅ Memory efficient
- ✅ Works well for small tables (1-3 entries)
- ❌ O(n) per lookup

### Approach B: Binary Search
```rust
// Binary search for first ceiling > byte
let idx = self.ceilings.partition_point(|&c| c <= utf8_byte);
self.steps.get(idx).and_then(|s| s.as_ref())
```
- ✅ O(log n) lookup
- ✅ Better for larger tables
- ❌ Overhead may exceed benefit for small n

### Approach C: Flat Lookup Table (256-byte array)
```rust
struct FastSmallTable {
    // Direct lookup: steps[byte] gives the state
    steps: [Option<Arc<FaState>>; 256],
}
```
- ✅ O(1) lookup
- ✅ Cache-friendly for repeated access
- ❌ 2KB+ per table (256 × 8 bytes minimum)
- ❌ Cloning Arc for each entry is expensive

### Approach D: Hybrid (Flat for hot paths, compressed for cold)
```rust
enum SmallTableImpl {
    Compressed { ceilings: Vec<u8>, steps: Vec<Option<Arc<FaState>>> },
    Flat([Option<Arc<FaState>>; 256]),
}
```
- ✅ O(1) for hot numeric paths
- ✅ Memory efficient for cold paths
- ❌ Branch on every lookup
- ❌ Complexity

### Approach E: Inline State Pointers (avoid Arc indirection)
```rust
// Instead of Arc<FaState>, use indices into a state arena
struct ArenaSmallTable {
    ceilings: Vec<u8>,
    steps: Vec<Option<StateId>>,  // Index instead of Arc
}
```
- ✅ No Arc reference counting overhead
- ✅ Better cache locality
- ❌ Requires arena-based state management
- Note: Already have `StateArena` for regexp patterns

## Tasks

### Phase 0: Profile and Establish Baseline

#### Task 0.1: Detailed Profiling
**Input**: Current implementation
**Output**: Flamegraph showing time distribution within FA traversal

**Steps**:
1. Run `cargo flamegraph --bench matching -- --bench number_matching`
2. Identify time spent in `dstep()` vs other traversal overhead
3. Count average ceilings.len() for numeric patterns

**Validation**:
- [ ] Flamegraph generated
- [ ] Percentage of time in dstep() documented
- [ ] Average ceilings length documented

---

#### Task 0.2: Micro-benchmark dstep() Variations

**Input**: Current dstep() implementation
**Output**: Comparison of linear vs binary search

**Steps**:
1. Add benchmark: `dstep_linear` (baseline)
2. Add benchmark: `dstep_binary_search`
3. Test with varying ceiling sizes (1, 2, 4, 8, 16 entries)
4. Test with Q-number byte distribution

**Validation**:
- [ ] ns/lookup for each approach at each size
- [ ] Crossover point where binary search wins

---

### Phase 1: Implement and Measure Winning Approach

#### Task 1.1: Integration
**Input**: Winning approach from Phase 0
**Output**: Updated dstep() implementation

**Validation**:
- [ ] All tests pass
- [ ] Numeric matching benchmarks show improvement

---

## Verification Checklist

Run after each task:
```bash
cargo test
cargo bench -- number_matching
cargo bench -- numeric_range
cargo bench -- citylots
```

## Non-Goals

- Optimizing NFA traversal (separate concern, more complex)
- Changing pattern compilation
- Breaking existing matching semantics

## Exit Criteria

| Outcome | Condition |
|---------|-----------|
| **Success** | 10%+ improvement in numeric matching benchmarks |
| **Stay** | All approaches within 5% → keep current for simplicity |
| **Pivot** | Unexpected finding → investigate and document |

---

## Phase 0 Results (2026-02-03)

### Key Finding: FA Traversal is NOT the Bottleneck for Numeric Matching

Micro-benchmarks reveal that DFA traversal and dstep are already highly optimized:

### dstep() Performance by Ceiling Count

| Ceilings | Time | Notes |
|----------|------|-------|
| 3 | 993 ps | Typical for single-byte mapping |
| 5 | 1.96 ns | Two byte mappings |
| 9 | 3.09 ns | Four byte mappings |

**Finding**: Sub-nanosecond performance for typical numeric patterns. Linear scan is optimal at these sizes.

### DFA Traversal by Q-Number Length

| Q-Num Length | Time | Use Case |
|--------------|------|----------|
| 1 byte | 4.2 ns | Zero (0.0) |
| 3 bytes | 10.5 ns | Small integers (1-999) |
| 5 bytes | 12.8 ns | Large integers |
| 10 bytes | 26 ns | High-precision floats |

**Finding**: ~2.5 ns per byte. Average Q-number (6 bytes) traverses in ~15 ns.

### Time Distribution Breakdown

| Benchmark | Full Time | Core Time | Flattening |
|-----------|-----------|-----------|------------|
| citylots | 2.19 µs | 562 ns | 1.63 µs (74%) |
| number_matching | 157 ns | ~15 ns | ~140 ns |

**Critical Insight**: JSON flattening dominates, not FA traversal.

For the citylots benchmark:
- JSON flattening: **74%** of total time
- Core matching: **26%** of total time

For number_matching:
- DFA traversal: ~15 ns (**<10%** of 157 ns total)
- JSON parsing/flattening: ~140 ns (**~90%**)

### Why "50% FA Traversal" from Prior Profiling?

The earlier profiling that showed "FA traversal ~50% of CPU time" was likely:
1. Measuring NFA traversal (wildcard/regexp patterns), not DFA
2. Using different workloads (string patterns with longer values)
3. Including merged pattern lookups (more ceilings per state)

For **numeric matching specifically**, DFA traversal is already optimal.

### Decision: **Do Not Optimize dstep()**

**Rationale**:
- Linear scan at 3 ceilings is sub-nanosecond
- Binary search would add overhead (branch misprediction, function call)
- Full DFA traversal is only ~10% of number_matching time
- JSON flattening is the real bottleneck (74-90% of time)

### Alternative Optimization Targets

Based on these findings, better optimization targets would be:

1. **JSON Flattening** (74% of citylots time)
   - SIMD-accelerated parsing
   - Lazy flattening (only flatten fields that match patterns)

2. **Field Lookup** (part of core matching)
   - Currently uses HashMap for field->value matcher
   - Could use trie or perfect hashing

3. **Mutex Overhead** (in Quamina wrapper)
   - RwLock for read-heavy workloads
   - Lock-free reads after pattern compilation

### Exit Criteria Evaluation

| Outcome | Condition | Result |
|---------|-----------|--------|
| **Stay** | All approaches within 5% → keep current for simplicity | ✅ **Selected** |

**Conclusion**: No changes to FA traversal for numeric matching. Focus optimization efforts on JSON flattening instead.

---

## Phase 1: Direct Byte Comparison for Numeric Matching

### Motivation

Benchmarks show direct byte comparison is **2.7x faster** than FA traversal:

| Method | Time | Speedup |
|--------|------|---------|
| FA traversal (single bound) | 7.26 ns | baseline |
| Direct `slice.cmp()` (single bound) | 2.71 ns | **2.7x faster** |
| Direct range (two bounds) | 6.22 ns | faster than FA for one bound |

Additionally, the current numeric FA code is:
- Complex recursive logic (~200 lines in `fa_builders.rs`)
- Creates many `Arc<FaState>` allocations during pattern building
- Hard to reason about and maintain

### Proposed: Direct Byte Comparison

Skip FA entirely for numeric matching. Use direct lexicographic comparison on Q-number bytes:

```rust
/// Numeric comparison types stored in the matcher
#[derive(Clone, Debug)]
pub enum NumericComparison {
    LessThan { bound_q: QNumberStack, inclusive: bool },
    GreaterThan { bound_q: QNumberStack, inclusive: bool },
    Range { lower_q: QNumberStack, upper_q: QNumberStack, lower_incl: bool, upper_incl: bool },
    Exact { value_q: QNumberStack },
}

impl NumericComparison {
    #[inline]
    pub fn matches(&self, value_q: &[u8]) -> bool {
        match self {
            NumericComparison::LessThan { bound_q, inclusive } => {
                match value_q.cmp(bound_q.as_slice()) {
                    Ordering::Less => true,
                    Ordering::Equal => *inclusive,
                    Ordering::Greater => false,
                }
            }
            NumericComparison::GreaterThan { bound_q, inclusive } => {
                match value_q.cmp(bound_q.as_slice()) {
                    Ordering::Greater => true,
                    Ordering::Equal => *inclusive,
                    Ordering::Less => false,
                }
            }
            NumericComparison::Range { lower_q, upper_q, lower_incl, upper_incl } => {
                let above_lower = match value_q.cmp(lower_q.as_slice()) {
                    Ordering::Greater => true,
                    Ordering::Equal => *lower_incl,
                    Ordering::Less => false,
                };
                let below_upper = match value_q.cmp(upper_q.as_slice()) {
                    Ordering::Less => true,
                    Ordering::Equal => *upper_incl,
                    Ordering::Greater => false,
                };
                above_lower && below_upper
            }
            NumericComparison::Exact { value_q: exact_q } => {
                value_q == exact_q.as_slice()
            }
        }
    }
}
```

### Benefits

1. **2.7x faster** numeric matching
2. **~200 lines deleted** from `fa_builders.rs`
3. **Simpler code** - no recursive FA building
4. **Easier to understand** - direct comparison logic
5. **No Arc allocations** during pattern building

### Tasks

#### Task 1.1: Add NumericComparison Type

**Input**: Current `src/json.rs` and `src/numbits.rs`
**Output**: New `NumericComparison` enum with `matches()` method

**Files to modify**:
- `src/numbits.rs` - Add `NumericComparison` enum

**Steps**:
1. Define `NumericComparison` enum with all comparison types
2. Implement `matches(&self, value_q: &[u8]) -> bool`
3. Add unit tests for each comparison type

**Validation**:
- [ ] Unit tests for `LessThan`, `GreaterThan`, `Range`, `Exact`
- [ ] Edge cases: inclusive vs exclusive, equal values
- [ ] `cargo miri test` passes

---

#### Task 1.2: Add NumericMatcher to ValueMatcher

**Input**: `src/automaton/small_table.rs` (ValueMatcher)
**Output**: ValueMatcher stores numeric comparisons separately from FA

**Files to modify**:
- `src/automaton/small_table.rs` - Add field to `ValueMatcher`
- `src/automaton/mutable_matcher.rs` - Add field to `MutableValueMatcher`

**Steps**:
1. Add `numeric_comparisons: Vec<NumericComparison>` to `ValueMatcher`
2. Add corresponding field to `MutableValueMatcher`
3. Add `next_field_matcher` association for each comparison

**Validation**:
- [ ] Compilation succeeds
- [ ] Existing tests still pass (no behavior change yet)

---

#### Task 1.3: Update Pattern Building

**Input**: `src/automaton/mutable_matcher.rs`
**Output**: Numeric patterns use `NumericComparison` instead of FA

**Files to modify**:
- `src/automaton/mutable_matcher.rs` - Update `add_numeric_range_transition`

**Steps**:
1. Modify `add_numeric_range_transition` to create `NumericComparison`
2. Store in `numeric_comparisons` instead of building FA
3. Keep `has_numbers` flag for Q-number conversion

**Validation**:
- [ ] Pattern building tests pass
- [ ] No FA created for numeric-only patterns

---

#### Task 1.4: Update Match-Time Traversal

**Input**: `src/automaton/mutable_matcher.rs` and `thread_safe.rs`
**Output**: Matching checks `numeric_comparisons` in addition to FA

**Files to modify**:
- `src/automaton/mutable_matcher.rs` - Update `transition_on`
- `src/automaton/thread_safe.rs` - Update frozen matcher

**Steps**:
1. After Q-number conversion, check `numeric_comparisons`
2. If any comparison matches, add corresponding field transition
3. Continue with FA traversal for non-numeric patterns

**Validation**:
- [ ] All existing numeric tests pass
- [ ] Mixed patterns (numeric + string on same field) work

---

#### Task 1.5: Remove Old Numeric FA Builders

**Input**: `src/automaton/fa_builders.rs`
**Output**: Clean codebase without unused functions

**Files to modify**:
- `src/automaton/fa_builders.rs` - Remove functions
- `src/automaton/mod.rs` - Update exports

**Functions to remove**:
- `make_numeric_less_fa`
- `make_numeric_greater_fa`
- `make_numeric_range_fa`
- `make_less_fa_step`
- `make_greater_fa_step`
- `make_range_fa_step`

**Validation**:
- [ ] No compilation errors
- [ ] No unused code warnings

---

#### Task 1.6: Comprehensive Testing

**Input**: Complete implementation
**Output**: Verified correctness with multiple testing methods

**Existing Infrastructure** (already in codebase):
- `src/kani_proofs.rs` - Kani bounded model checking proofs
- `fuzz/fuzz_targets/fuzz_match_event.rs` - Fuzzes full matching pipeline (includes numeric patterns!)
- `fuzz/fuzz_targets/fuzz_add_pattern.rs` - Fuzzes pattern parsing
- Miri testing in CI

**Testing requirements**:

1. **Unit tests** (add to `src/numbits.rs` or new `src/tests_numeric.rs`):
   ```rust
   #[test]
   fn test_numeric_comparison_less_than() { ... }
   #[test]
   fn test_numeric_comparison_greater_than() { ... }
   #[test]
   fn test_numeric_comparison_range() { ... }
   #[test]
   fn test_numeric_comparison_exact() { ... }
   #[test]
   fn test_numeric_comparison_edge_cases() { ... }  // boundaries, inclusive/exclusive
   #[test]
   fn test_numeric_comparison_special_values() { ... }  // -0.0, NaN, Inf, subnormals
   ```
   - [ ] All comparison types covered
   - [ ] Edge cases (boundaries, inclusive/exclusive)
   - [ ] Negative numbers, zero, large numbers
   - [ ] High-precision floats

2. **Integration tests** (add to existing test files):
   - [ ] Numeric patterns in isolation
   - [ ] Mixed numeric + string patterns on same field
   - [ ] Multiple numeric patterns on same field (OR semantics)
   - [ ] Numeric patterns across multiple fields (AND semantics)

3. **Miri** (memory safety):
   ```bash
   cargo miri test
   ```
   - [ ] All new code passes Miri
   - [ ] No undefined behavior detected

4. **Fuzzing** (existing target covers this!):
   ```bash
   cargo +nightly fuzz run fuzz_match_event -- -max_total_time=3600
   ```
   The existing `fuzz_match_event` target already includes:
   - `{"price": [{"numeric": ["<", 100]}]}`
   - `{"score": [{"numeric": [">=", 0, "<=", 100]}]}`

   - [ ] Run for at least 1 hour after changes
   - [ ] No crashes or panics

5. **Kani proofs** (add to `src/kani_proofs.rs`):
   ```rust
   #[cfg(kani)]
   mod numeric_comparison_proofs {
       #[kani::proof]
       fn numeric_less_than_correct() {
           let bound: f64 = kani::any();
           let value: f64 = kani::any();
           kani::assume(bound.is_finite() && value.is_finite());

           let bound_q = q_num_stack(bound);
           let value_q = q_num_stack(value);

           let cmp = NumericComparison::LessThan {
               bound_q, inclusive: false
           };

           // Direct comparison should match Q-number comparison
           kani::assert(
               cmp.matches(value_q.as_slice()) == (value < bound),
               "LessThan must match f64 semantics"
           );
       }
   }
   ```
   - [ ] Add proofs for comparison correctness
   - [ ] Run: `cargo kani`

**Validation**:
- [ ] All existing tests pass (no regressions)
- [ ] New unit tests pass
- [ ] `cargo miri test` passes
- [ ] Fuzzing runs clean for 1+ hour
- [ ] Kani proofs verify
- [ ] Benchmarks show expected improvement

---

#### Task 1.7: Benchmarks and Documentation

**Input**: Complete, tested implementation
**Output**: Performance validation and updated docs

**Steps**:
1. Run full benchmark suite
2. Compare before/after for numeric benchmarks
3. Update this document with results
4. Add inline documentation for new code

**Expected improvements**:
- `numeric_range_single`: ~2.7x faster
- `numeric_range_two_sided`: ~2x faster
- `number_matching`: measurable improvement
- Memory: fewer allocations during pattern building

---

### Verification Checklist

Run after each task:
```bash
# Basic checks
cargo test                           # All tests pass
cargo clippy                         # No warnings
cargo fmt --check                    # Formatting

# Safety checks
cargo miri test                      # Memory safety (subset of tests)

# Performance checks
cargo bench -- numeric               # Numeric benchmarks
cargo bench -- number_matching       # End-to-end numeric matching

# Before final merge
cargo +nightly fuzz run fuzz_match_event -- -max_total_time=300   # 5 min fuzz
cargo kani                           # Bounded model checking (if proofs added)
```

### Non-Goals (for Phase 1)

- Changing Q-number encoding
- Optimizing string/prefix/shellstyle matching
- Arena-based refactor (deferred to Phase 2)

---

## Phase 2: Arena-Based FA for All Patterns (Future Session)

### Motivation

Unify all pattern matching under arena-based architecture:
- Currently: `Arc<FaState>` for most patterns, `StateArena` for regexp only
- Goal: `StateArena` for everything

### Benefits

1. **No Arc reference counting** during traversal
2. **Better cache locality** - states contiguous in memory
3. **Unified architecture** - one traversal path
4. **Simpler merging** - arena indices instead of Arc cloning

### Scope

This is a larger refactor touching:
- `SmallTable` → `ArenaSmallTable` migration
- `merge_fas()` → arena-based merging
- `traverse_dfa()` → `traverse_arena_dfa()`
- `ValueMatcher` → arena-based state storage

### Prerequisites

- Phase 1 complete (numeric separated from FA)
- Good test coverage established
- Benchmark baselines captured

### Tasks (High-Level)

1. Design arena-based ValueMatcher
2. Migrate string/prefix patterns to arena
3. Migrate shellstyle patterns to arena
4. Update merge logic
5. Remove Arc-based code
6. Comprehensive testing (miri, fuzz, benchmarks)

*Detailed task breakdown to be created at start of Phase 2 session.*

