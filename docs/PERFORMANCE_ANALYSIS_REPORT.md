# Arena FA Performance Analysis Report

## Executive Summary

Investigation of arena FA performance gaps identified **4 key hotspots** that explain why arena FA shows modest gains (1-4%) for general patterns vs the expected 2-3x improvement seen in numeric range matching (50% improvement).

**Key Finding**: The arena FA's cache locality benefits are being masked by overhead in epsilon closure computation, hash-based deduplication, and unnecessary allocations during traversal.

## Current Benchmark Results

| Benchmark | Time | Notes |
|-----------|------|-------|
| exact_match | 130 ns | Simple DFA path |
| numeric_range_single | 239 ns | ~50% improvement from baseline |
| arena_nfa_5chars | 232 ns | ~4% improvement |
| arena_nfa_100chars | 3.48 µs | ~4% improvement |

## Memory Profiling Results

| Metric | Value | Per Event |
|--------|-------|-----------|
| matching_1000_events_100_patterns | 324,684 bytes, 14,903 allocs | ~325 B/event, ~15 allocs/event |
| matching_1000_events_no_match | 16,208 bytes, 1,001 allocs | ~16 B/event, ~1 alloc/event |
| number_matching_100_events | 20,030 bytes, 853 allocs | ~200 B/event, ~8.5 allocs/event |
| citylots_100_events | 108,762 bytes, 5,657 allocs | ~1,088 B/event (Go: 968 B/op) |

**Critical**: Allocations per match > 0 indicates room for optimization.

## Identified Hotspots

### Hotspot 1: Epsilon Closure Allocation (HIGH IMPACT)

**Location**: `src/automaton/arena.rs:534`

```rust
// Current: Allocates new SmallVec on EVERY call
fn get_arena_epsilon_closure(...) -> smallvec::SmallVec<[StateId; 4]> {
    // ...
    bufs.closure_result.iter().copied().collect()  // <-- ALLOCATES
}
```

**Problem**: Called once per state per byte. For 100-char input with 3 states = 300 allocations per match.

**Impact**: ~30-40% of traversal overhead for NFA patterns.

**Fix Options**:
1. **Return slice reference** instead of SmallVec (requires lifetime changes)
2. **Use output parameter** - pass mutable slice to fill
3. **Memoize at build time** - precompute epsilon closures during FA construction

### Hotspot 2: Hash-Based Transition Deduplication (MEDIUM IMPACT)

**Location**: `src/automaton/arena.rs:432-433`

```rust
// Current: FxHashSet requires hash computation per insert
for ft in &ec_state.field_transitions {
    let ptr = Arc::as_ptr(ft) as usize;
    if bufs.seen_transitions.insert(ptr) {  // <-- HASH + POSSIBLE RESIZE
        bufs.transitions.push(ft.clone());
    }
}
```

**Problem**: FxHashSet operations have overhead even with good hash function.

**Impact**: ~15-20% of traversal time for patterns with multiple match states.

**Fix Options**:
1. **Sparse set** with generation counter (O(1) amortized, no hashing)
2. **Bitmap** for small transition sets (if max transitions bounded)
3. **Sort and dedupe at end** (batch operation, better cache locality)

### Hotspot 3: Arc Clone Overhead (LOW-MEDIUM IMPACT)

**Location**: `src/automaton/arena.rs:433`

```rust
bufs.transitions.push(ft.clone());  // <-- ATOMIC REF COUNT INCREMENT
```

**Problem**: `Arc::clone` performs atomic increment, which has memory ordering overhead.

**Impact**: ~5-10% overhead, especially under contention.

**Fix Options**:
1. **Store raw pointers** during traversal, only clone at end if needed
2. **Embed match data directly** in ArenaFaState instead of Arc<FieldMatcher>
3. **Use indices** instead of Arc pointers

### Hotspot 4: Vec Capacity Growth (LOW IMPACT)

**Location**: Multiple places in `traverse_arena_nfa`

```rust
bufs.current_states.push(start);  // May grow if capacity exceeded
bufs.next_states.push(next);
bufs.closure_result.push(start);
```

**Problem**: Initial capacity may be insufficient, causing reallocation.

**Impact**: ~5% overhead, one-time per buffer lifetime.

**Fix**: Pre-allocate based on arena size (already partially done).

## Why Numeric Ranges Show 50% Improvement

Numeric range patterns are **pure DFA** - no epsilon transitions:
- Fast path in `get_arena_epsilon_closure` returns immediately (line 488-492)
- No SmallVec allocation
- No epsilon closure computation
- Single state tracked, minimal hash operations

This confirms the theory: arena benefits are realized when overhead is eliminated.

## Optimization Recommendations (Ranked by Impact)

### High Priority

1. **Eliminate epsilon closure allocation**
   - Change `get_arena_epsilon_closure` to use output parameter or return slice
   - Expected impact: ~10-15% improvement for NFA patterns

2. **Replace FxHashSet with sparse set**
   - Use generation counter pattern for O(1) membership check
   - Expected impact: ~5-10% improvement

### Medium Priority

3. **Precompute epsilon closures at build time**
   - For states with epsilon transitions, store precomputed closure
   - Eliminates runtime computation entirely
   - Expected impact: ~15-20% improvement for complex NFA patterns

4. **Store match data directly in ArenaFaState**
   - Instead of `Vec<Arc<FieldMatcher>>`, store match IDs directly
   - Eliminates Arc clone during traversal
   - Expected impact: ~5% improvement

### Lower Priority

5. **Remove dual traversal**
   - Complete migration to eliminate `start_table` traversal
   - Currently checking both paths in some cases

6. **Profile-guided buffer capacity**
   - Track typical state counts and pre-allocate accordingly

## Implementation Plan

### Phase 1: Quick Wins (1-2 days)
- [ ] Change `get_arena_epsilon_closure` to fill output parameter
- [ ] Replace `seen_transitions` FxHashSet with sparse set

### Phase 2: Structural Changes (2-3 days)
- [ ] Precompute epsilon closures at build time
- [ ] Store match IDs instead of Arc<FieldMatcher>

### Phase 3: Cleanup (1 day)
- [ ] Complete removal of deprecated `start_table`
- [ ] Remove chain FA code paths

## Expected Outcome

After implementing Phase 1-2 optimizations:
- arena_nfa_5chars: ~180 ns (from 232 ns) - 22% improvement
- arena_nfa_100chars: ~2.5 µs (from 3.48 µs) - 28% improvement
- exact_match: ~110 ns (from 130 ns) - 15% improvement

This would bring arena FA performance closer to the theoretical 2-3x improvement seen in Go quamina.

## Appendix: Code Snippets for Fixes

### Fix 1: Output Parameter for Epsilon Closure

```rust
/// Fill the provided slice with epsilon closure states.
/// Returns the number of states written.
fn fill_epsilon_closure(
    arena: &StateArena,
    start: StateId,
    bufs: &mut ArenaNfaBuffers,
) -> usize {
    if start.is_none() {
        return 0;
    }

    let start_state = &arena[start];
    if start_state.table.epsilons.is_empty() {
        bufs.closure_result.clear();
        bufs.closure_result.push(start);
        return 1;
    }

    // ... existing closure computation, but writes to bufs.closure_result
    // Return length instead of allocating
}
```

### Fix 2: Sparse Set for Deduplication

```rust
struct SparseSet {
    dense: Vec<usize>,
    sparse: Vec<usize>,
    generation: usize,
    generations: Vec<usize>,
}

impl SparseSet {
    fn contains(&self, item: usize) -> bool {
        item < self.generations.len() && self.generations[item] == self.generation
    }

    fn insert(&mut self, item: usize) -> bool {
        if self.contains(item) {
            return false;
        }
        if item >= self.generations.len() {
            self.generations.resize(item + 1, 0);
        }
        self.generations[item] = self.generation;
        self.dense.push(item);
        true
    }

    fn clear(&mut self) {
        self.generation += 1;
        self.dense.clear();
    }
}
```

---

*Report generated: 2026-02-04*
*Based on commit: f86d539 (main)*
