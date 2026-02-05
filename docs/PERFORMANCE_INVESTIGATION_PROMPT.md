# Performance Investigation: Arena FA vs Chain FA

## Session Goal
Investigate why arena FA performance gains are lower than expected despite cache locality improvements. Use profiling to identify hotspots and optimization opportunities.

## Background

### Expected vs Actual Performance

**Theory**: Arena FA should be significantly faster due to:
- Contiguous memory (Vec) vs scattered heap allocations (Arc)
- StateId (4 bytes) vs Arc pointer (16 bytes + ref counting)
- Sequential memory access vs pointer chasing
- Expected improvement: 2-3x based on similar Go quamina results

**Actual Results** (Phase 2.4 complete):
| Benchmark | Time | Change from Baseline |
|-----------|------|---------------------|
| numeric_range_single | 236 ns | ~50% improvement (was 470 ns) |
| arena_nfa_5chars | 227 ns | ~4% improvement |
| arena_nfa_100chars | 3.41 µs | ~4% improvement |
| exact_match | 126 ns | ~1% improvement |

The numeric range benchmarks show large gains (50%), but general pattern matching shows modest gains (1-4%). **Why?**

## Investigation Tasks

### Task 1: Profile Baseline (Before Phase 1)

First, check out the commit BEFORE arena numeric FA was integrated:

```bash
# Find the commit before Phase 1 started
git log --oneline | grep -A5 "Phase 1"
# The baseline is likely around commit before 0bee98c

# Check out baseline
git stash  # if needed
git checkout <baseline-commit>

# Build release with debug symbols for profiling
cargo build --release --bench matching
RUSTFLAGS="-C force-frame-pointers=yes" cargo build --release --bench matching

# Profile with samply (on macOS)
samply record -- ./target/release/deps/matching-* --bench numeric_range_single --profile-time 10
samply record -- ./target/release/deps/matching-* --bench exact_match --profile-time 10
samply record -- ./target/release/deps/matching-* --bench arena_nfa_5chars --profile-time 10

# Save flamegraphs/profiles for comparison
# samply opens a web UI - export the data
```

### Task 2: Profile Current State (After Phase 2.4)

```bash
# Return to main
git checkout main

# Build with debug symbols
RUSTFLAGS="-C force-frame-pointers=yes" cargo build --release --bench matching

# Profile the same benchmarks
samply record -- ./target/release/deps/matching-* --bench numeric_range_single --profile-time 10
samply record -- ./target/release/deps/matching-* --bench exact_match --profile-time 10
samply record -- ./target/release/deps/matching-* --bench arena_nfa_5chars --profile-time 10
```

### Task 3: Identify Hotspots

Look for these potential bottlenecks in the profiles:

1. **`traverse_arena_nfa` function** (src/automaton/arena.rs)
   - Is time spent in epsilon closure computation?
   - Is the `seen` HashSet causing overhead?
   - Are we still doing unnecessary allocations?

2. **`transition_on` in MutableValueMatcher** (src/automaton/mutable_matcher.rs)
   - Is Q-number conversion (fast_float2::parse) expensive?
   - Is the transition_map lookup (FxHashMap) slow?
   - Is deduplication (`seen_transitions`) adding overhead?

3. **Memory allocation patterns**
   - Are we still allocating during traversal?
   - Is `ArenaNfaBuffers::clear()` efficient?
   - SmallVec vs Vec impact?

4. **Dual traversal overhead**
   - Currently we traverse BOTH main_arena AND start_table in some paths
   - This doubles work in hybrid state

### Task 4: Specific Micro-benchmarks

Create targeted micro-benchmarks to isolate components:

```rust
// Add to benches/matching.rs

// Benchmark just the arena traversal (no Q-number, no transition_map)
fn bench_arena_traversal_only(c: &mut Criterion) {
    // Build a simple arena with known structure
    // Time just traverse_arena_nfa with pre-built value
}

// Benchmark Q-number conversion
fn bench_q_number_conversion(c: &mut Criterion) {
    // Time fast_float2::parse + q_num_stack
}

// Benchmark transition_map lookup
fn bench_transition_map_lookup(c: &mut Criterion) {
    // Time FxHashMap lookup with pointer keys
}

// Benchmark epsilon closure
fn bench_epsilon_closure(c: &mut Criterion) {
    // Build arena with epsilon transitions
    // Time epsilon_closure function specifically
}
```

### Task 5: Compare Memory Access Patterns

Use `perf` (Linux) or Instruments (macOS) to check:

```bash
# Linux: Check cache misses
perf stat -e cache-references,cache-misses,L1-dcache-load-misses ./target/release/deps/matching-* --bench exact_match

# macOS: Use Instruments "Counters" template
# Look for L1/L2 cache miss rates
```

## Hypotheses to Test

### Hypothesis 1: Dual Traversal Overhead
**Theory**: We're traversing both `main_arena` AND deprecated `start_table` in FrozenValueMatcher.
**Test**: Profile shows time in both `traverse_arena_nfa` AND `traverse_dfa/nfa`.
**Fix**: Complete migration, remove `start_table` traversal.

### Hypothesis 2: Epsilon Closure Overhead
**Theory**: The epsilon closure computation (finding all states reachable via epsilon) is expensive.
**Test**: Profile shows significant time in epsilon-related code.
**Fix**: Consider memoizing epsilon closures at build time.

### Hypothesis 3: HashSet/HashMap Overhead
**Theory**: `seen` sets and transition maps add overhead.
**Test**: Profile shows time in hash operations.
**Fix**: Use sparse sets, bloom filters, or direct indexing.

### Hypothesis 4: Buffer Clear/Reuse Overhead
**Theory**: Clearing buffers between traversals is expensive.
**Test**: Profile shows time in `clear()` or `Vec::retain`.
**Fix**: Use generation counters instead of clearing.

### Hypothesis 5: Transition Map Indirection
**Theory**: Looking up Arc pointers in HashMap adds indirection.
**Test**: Profile shows time in `FxHashMap::get`.
**Fix**: Embed match data directly in ArenaFaState.

## Key Files to Examine

1. **src/automaton/arena.rs**
   - `traverse_arena_nfa` - main traversal function
   - `epsilon_closure` - epsilon transition handling
   - `ArenaSmallTable::step` - byte transition lookup

2. **src/automaton/mutable_matcher.rs**
   - `MutableValueMatcher::transition_on` - entry point for value matching
   - Q-number conversion code
   - transition_map usage

3. **src/automaton/thread_safe.rs**
   - `FrozenValueMatcher::transition_on` - thread-safe traversal
   - Dual traversal (main_arena + start_table)

## Expected Deliverables

1. **Flamegraph comparison**: Before vs After Phase 2.4
2. **Hotspot analysis**: Top 5 functions by time
3. **Optimization recommendations**: Ranked by expected impact
4. **Micro-benchmark results**: Isolated component timings

## Task 6: Memory Profiling

Memory efficiency is a core benefit of arena FA. Profile to validate.

### 6a: Allocation Count During Matching

```bash
# Use dhat via Miri to count allocations
# Create a test that runs matching in a loop
cargo +nightly miri run --bin memory_profile 2>&1 | grep "total heap usage"

# Or use dhat-rs crate directly in a benchmark
# Add to Cargo.toml: dhat = { version = "0.3", features = ["count"] }
```

### 6b: Heap Size Comparison

```rust
// Add to benches/matching.rs or create benches/memory.rs

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

struct CountingAlloc;
static ALLOCATED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for CountingAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
        System.alloc(layout)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        ALLOCATED.fetch_sub(layout.size(), Ordering::SeqCst);
        System.dealloc(ptr, layout)
    }
}

#[global_allocator]
static A: CountingAlloc = CountingAlloc;

fn bench_memory_per_pattern(c: &mut Criterion) {
    // Measure memory for 100, 1000, 10000 patterns
    // Compare chain FA vs arena FA memory usage
}

fn bench_allocations_during_match(c: &mut Criterion) {
    // Build matcher, then reset counter
    // Run 1000 matches, check ALLOCATED delta
    // Should be ZERO for pure arena path
}
```

### 6c: Using heaptrack (Linux) or Instruments (macOS)

```bash
# Linux: heaptrack
heaptrack ./target/release/deps/matching-* --bench exact_match
heaptrack_print heaptrack.matching-*.gz

# macOS: Instruments Allocations template
# Run matching benchmark, look for:
# - Total bytes allocated
# - Allocation count
# - Transient vs persistent allocations
```

### Memory Metrics to Compare

| Metric | Chain FA (Before) | Arena FA (After) | Expected |
|--------|-------------------|------------------|----------|
| Memory per 1000 patterns | ? KB | ? KB | ~66% less |
| Allocations per match | ? | 0 | Zero |
| Peak heap (10k patterns) | ? MB | ? MB | ~66% less |
| StateId size | 16 bytes (Arc) | 4 bytes | 4x smaller |

**Critical check**: If allocations-per-match > 0 for arena path, that's a bug to fix.

---

## Commands Quick Reference

```bash
# Build with symbols
RUSTFLAGS="-C force-frame-pointers=yes" cargo build --release --bench matching

# Run specific benchmark
cargo bench --bench matching -- exact_match

# Profile with samply (macOS)
samply record -- ./target/release/deps/matching-* --bench exact_match --profile-time 10

# Profile with perf (Linux)
perf record -g ./target/release/deps/matching-* --bench exact_match --profile-time 10
perf report

# Check cache performance (Linux)
perf stat -e cache-references,cache-misses ./target/release/deps/matching-* --bench exact_match

# Generate flamegraph (with cargo-flamegraph)
cargo flamegraph --bench matching -- --bench exact_match
```

## Hypothesis 6: Hidden Allocations During Traversal

**Theory**: We're accidentally allocating during `traverse_arena_nfa` (Vec growth, HashSet operations).
**Test**: Memory profiling shows allocations per match > 0.
**Fix**: Pre-allocate buffers, use sparse sets, avoid HashSet in hot path.

This is particularly important because even a single allocation per match can dominate the cache benefits.

---

## After Investigation: Continue with Step 2.5

Once hotspots are identified and addressed, continue with:
1. Migrate `AutomatonValueMatcher` in thread_safe.rs to arena FA
2. Remove `start_table` from `FrozenValueMatcher`
3. Remove chain FA builders from `fa_builders.rs`
4. Remove `small_table.rs` entirely
5. Target: ~1,300-1,400 lines removed
