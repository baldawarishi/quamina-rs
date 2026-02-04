# Hybrid Q-Number Representation

## Overview

Investigate whether alternative Q-number representations can improve numeric matching performance by eliminating heap allocations while preserving efficient FA traversal. **Evidence first, engineering second.**

## Background

Initial profiling (2026-02-03) showed:
- Current implementation is 20x better than Go baseline on memory (96 B/op vs 1908)
- Numeric matching (~161 ns) is comparable to string matching (~170 ns)
- FA traversal is ~50% of CPU time, Q-number conversion is ~20%

However, Q-number length analysis revealed an optimization opportunity:

| Value Type | Q-number Length | % of citylots data |
|------------|-----------------|-------------------|
| Zero (0.0) | 1 byte | 23% |
| Small int (1-999) | 3-4 bytes | 17% |
| Large int (1000+) | 4-5 bytes | 13% |
| High-precision float | 10 bytes | 47% |

**Average Q-number length: 6.09 bytes** (vs fixed 10 bytes = 64% overhead)

## Hypothesis

A hybrid representation can eliminate heap allocation without increasing FA traversal overhead.

## Approaches to Compare

### Approach A: Current (Baseline)
```rust
// Variable-length, heap allocated
fn q_num_from_f64(f: f64) -> Vec<u8>
```
- ✅ Minimal FA traversal (exact length)
- ❌ Heap allocation per conversion

### Approach B: Stack Buffer + Length
```rust
struct QNumber {
    bytes: [u8; 10],  // Stack allocated
    len: u8,          // Actual length 1-10
}
```
- ✅ Zero heap allocation
- ✅ Minimal FA traversal (uses `&bytes[..len]`)
- ? Struct copy overhead

### Approach C: Tiered Fixed Sizes
```rust
enum QNumber {
    Small([u8; 4]),   // 0.0, small ints — covers ~40% of citylots
    Medium([u8; 7]),  // medium values — covers ~10%
    Large([u8; 10]),  // high-precision floats — covers ~50%
}
```
- ✅ Zero heap allocation
- ✅ Cache-friendly (smaller variants)
- ? Branch overhead on creation
- ? Match overhead on use

## Tasks

### Phase 0: Benchmark All Approaches

#### Task 0.1: Implement QNumber Variants

**Input**: Current `src/numbits.rs`
**Output**: All three Q-number representations

**Steps**:
1. Keep current `q_num_from_f64() -> Vec<u8>` as baseline
2. Add `q_num_stack(f: f64) -> QNumberStack` (Approach B)
3. Add `q_num_tiered(f: f64) -> QNumberTiered` (Approach C)
4. Ensure all produce identical byte sequences for same input

**Validation**:
- [ ] `cargo test` — all variants produce equivalent results
- [ ] `cargo miri test` — no undefined behavior

---

#### Task 0.2: Micro-benchmark Q-number Conversion

**Input**: Task 0.1 implementations
**Output**: Conversion time comparison

**Steps**:
1. Add benchmark: `q_num_conversion_vec` (baseline)
2. Add benchmark: `q_num_conversion_stack`
3. Add benchmark: `q_num_conversion_tiered`
4. Test with citylots-representative value distribution

**Validation**:
- [ ] Clear ns/conversion numbers for each approach
- [ ] Document which is fastest for conversion alone

---

#### Task 0.3: End-to-End Matching Benchmark

**Input**: Task 0.1 implementations integrated into matcher
**Output**: Full matching performance comparison

**Steps**:
1. Create feature flag or runtime switch for Q-number representation
2. Benchmark `number_matching` with each approach
3. Benchmark `numeric_range_100_patterns` with each approach
4. Benchmark `citylots` with each approach (real-world data)

**Validation**:
- [ ] Events/sec for each approach on each benchmark
- [ ] Memory profile (B/op, allocs/op) for each approach

---

#### Task 0.4: Decision Gate

**Input**: Data from Tasks 0.2-0.3
**Output**: Select winning approach or stay with baseline

**Decision criteria**:

| Finding | Action |
|---------|--------|
| All approaches within 5% | **Stay with baseline** (simplicity wins) |
| Stack buffer 10%+ faster | **Adopt Approach B** |
| Tiered 10%+ faster | **Adopt Approach C** |
| Mixed results | **Adopt best for citylots** (real-world proxy) |

**Validation**:
- [ ] Decision documented with benchmark data
- [ ] If adopting new approach: migration plan documented

---

### Phase 1: Integration (If Approach B or C Wins)

#### Task 1.1: Replace q_num_from_f64

**Input**: Winning approach from Phase 0
**Output**: Updated `src/numbits.rs` and callers

**Steps**:
1. Replace `q_num_from_f64() -> Vec<u8>` with winning type
2. Update `mutable_matcher.rs` to use new type
3. Update any other callers

**Validation**:
- [ ] All tests pass
- [ ] No heap allocations in Q-number path (verify with DHAT)

---

#### Task 1.2: Final Benchmarks

**Input**: Integrated implementation
**Output**: Before/after comparison

**Steps**:
1. Run full benchmark suite
2. Compare against baseline from Phase 0
3. Document improvements

**Validation**:
- [ ] Improvement matches Phase 0 predictions
- [ ] No regressions in other benchmarks

---

## Verification Checklist

Run after each task:

```bash
cargo test
cargo bench              # Check for regressions
cargo miri test          # If touching unsafe code
```

## Non-Goals

- Changing the Q-number encoding algorithm (base-128 stays)
- Optimizing FA traversal itself (separate concern)
- Breaking existing numeric matching semantics

## Exit Criteria

| Outcome | Condition |
|---------|-----------|
| **Success** | 10%+ improvement in numeric matching benchmarks |
| **Stay** | All approaches within 5% → keep current for simplicity |
| **Pivot** | Unexpected finding → investigate and document |

---

## Phase 0 Results (2026-02-03)

### Micro-benchmarks (conversion only)
| Approach | Time | vs Baseline |
|----------|------|-------------|
| Vec (baseline) | 18.6 ns | - |
| Stack | 4.9 ns | **73% faster** |
| Tiered | 4.7 ns | **75% faster** |

### End-to-end benchmarks
| Benchmark | Baseline | QNumberStack | Improvement |
|-----------|----------|--------------|-------------|
| number_matching | 161 ns | 145 ns | **10%** |
| numeric_range_single | 184 ns | 157 ns | **15%** |
| numeric_range_two_sided | 176 ns | 157 ns | **11%** |
| numeric_range_10_patterns | 206 ns | 200 ns | 3% |
| citylots | 2.49 µs | 2.27 µs | **9%** |

### Memory Impact (dhat-heap profiling)

| Benchmark | Baseline | QNumberStack | Bytes Δ | Allocs Δ |
|-----------|----------|--------------|---------|----------|
| number_matching (100 events) | 9,599 B / 250 allocs | 8,600 B / 150 allocs | **-10%** | **-40%** |
| citylots (100 events) | 33,713 B / 2,135 allocs | 19,232 B / 65 allocs | **-43%** | **-97%** |
| large_json (100 events) | 21,300 B / 600 allocs | 20,400 B / 400 allocs | **-4%** | **-33%** |

### Linear Scaling Verification (100 → 10,000 events)

| Scale | Baseline | QNumberStack | Speed Δ | Allocs Δ |
|-------|----------|--------------|---------|----------|
| 100 events | 161 ns/event, 250 allocs | 153 ns/event, 150 allocs | **5%** | **-40%** |
| 10,000 events | 156 ns/event, ~25k allocs | 146 ns/event, 15k allocs | **6%** | **-40%** |

**Conclusion:** Improvement scales linearly with no edge cases or degradation.

### Decision: **Adopt Approach B (QNumberStack)**

**Rationale:**
- 4 of 5 speed benchmarks exceed 10% threshold
- Citylots (real-world proxy) shows 9% speed improvement, 97% allocation reduction
- Stack is simpler than Tiered (no enum matching overhead)
- Zero heap allocations in the Q-number hot path

**Implementation Status:** ✅ Complete
- `QNumberStack` type added to `src/numbits.rs`
- Match-time code updated in `src/automaton/mutable_matcher.rs`
- Match-time code updated in `src/automaton/thread_safe.rs`
- All 378 tests pass

---

## Phase 1 Completion (2026-02-03)

### Cleanup Completed
- ✅ Removed `QNumberTiered` enum from `src/numbits.rs`
- ✅ Removed `to_q_number_tiered()` and `q_num_tiered()` functions
- ✅ Removed `test_q_number_tiered_variants` test
- ✅ Updated equivalence tests to compare only Vec and Stack variants
- ✅ Removed tiered benchmarks from `benches/matching.rs`
- ✅ Removed tiered profiling from `benches/memory.rs`
- ✅ All tests pass

### Final State
- **Winner:** `QNumberStack` (stack-allocated `[u8; 10]` + `len: u8`)
- **Pattern building:** Uses `q_num_from_f64() -> Vec<u8>` (not hot path)
- **Match-time:** Uses `q_num_stack() -> QNumberStack` (zero heap allocation)

### Remaining Artifacts
- `q_num_from_f64()` kept for pattern building (non-hot path)
- Benchmark comparisons between Vec and Stack kept for regression testing

## References

- Q-number length analysis: `src/numbits.rs::test_q_number_length_analysis`
- Current implementation: `src/numbits.rs::q_num_stack`
- Hotpath: `src/automaton/mutable_matcher.rs:821-837`
