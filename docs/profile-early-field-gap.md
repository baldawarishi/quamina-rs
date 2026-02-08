# Profile: Go vs Rust "early field match" gap

**Date:** 2026-02-08
**Benchmark:** `status_context_fields` — numeric exact match on 2 early fields in ~11KB JSON
**Pattern:** `{ "context": { "user_id": [9034], "friends_count": [158] } }`
**Test data:** `testdata/status.json` (identical in both repos)

## Headline Numbers (Before Optimization)

| Metric | Rust | Go | Delta |
|--------|------|----|-------|
| Full path (`status_context_fields`) | **398 ns** | **382 ns** | Rust +16ns (+4.2%) |
| Flatten only (no mutex) | 151 ns | 140 ns | Rust +11ns (+7.9%) |
| Flatten + sort (no mutex) | 157 ns | (sort included in match) | — |
| Full path no mutex | 380 ns | — | — |
| Match cost (derived: full - flatten - sort) | 223 ns | 242 ns | **Rust -19ns (-7.9%)** |
| Mutex overhead (full - no_mutex) | ~18 ns | 0 ns | Rust pays ~18ns |
| Allocs/op (hot path) | 0 (reused) | 2 (16B) | Rust wins |

## Final Numbers (After Optimization — 2026-02-08)

| Metric | Rust (before) | Rust (after) | Go (fresh) | Winner |
|--------|--------------|-------------|-----|--------|
| Full path | 420 ns | **275 ns** | 403 ns | **Rust (32% faster)** |
| Flatten+sort | 161 ns | **124 ns** | 151 ns | **Rust (18% faster)** |
| Match only | 232 ns | **157 ns** | ~252 ns | **Rust (38% faster)** |

### Optimizations applied (branch `perf/close-early-field-gap`):

| Step | Change | Delta |
|------|--------|-------|
| 2. `from_utf8_unchecked` | Skip redundant UTF-8 validation in SegmentsTree | −33 ns |
| 3. `FxHashMap` | Switch FrozenFieldMatcher maps from SipHash to FxHash | −58 ns |
| 4. `thread_local!` | Replace Mutex with thread-local flattener/NFA buffers | −1 ns |
| 5. Vec linear dedup | Replace FxHashSet with Vec linear scan in FrozenMatchSet | −56 ns |
| **Total** | | **−148 ns (−35%)** |

## Gap Decomposition

```
Flattener gap:   +11ns  (from_utf8 validation: ~10ns, minor parse diffs: ~1ns)
Mutex overhead:  +18ns  (2 parking_lot locks)
Sort cost:        +6ns  (sorting 2 fields by path)
Matching gain:   -19ns  (faster q_num, arena DFA, less GC pressure)
                 ------
Net:             +16ns
```

## Rust Decomposed Benchmarks

These benchmarks were added in `benches/matching.rs` to isolate each phase:

```
A = flatten_direct_context_fields      =  151 ns  (flatten only, no mutex)
    flatten_context_fields             =  155 ns  (flatten only, with mutex)
B = flatten_sort_context_fields        =  157 ns  (flatten + sort, no mutex)
C = status_context_fields_no_mutex     =  380 ns  (flatten + sort + match, no mutex)
D = status_context_fields              =  398 ns  (flatten + sort + match, with 2 mutexes)
E = match_only_context_fields          =  227 ns  (match only, own NfaBuffers)

Derived:
  mutex_overhead (flatten only) = 155 - 151 =    ~4 ns
  sort_cost                     = 157 - 151 =    ~6 ns
  match_cost (from C-B)         = 380 - 157 =  ~223 ns
  mutex_overhead (total)        = 398 - 380 =   ~18 ns
  match_cost (standalone E)     =              ~227 ns  (consistent with C-B)
```

## Go Benchmark Results

```
Benchmark_JsonFlattener_ContextFields-14            140.3 ns/op     0 B/op    0 allocs/op
Benchmark_JsonFlattner_Evaluate_ContextFields-14    381.8 ns/op    16 B/op    2 allocs/op
```

Go benchmark calls `q.MatchesForEvent(event)` which reuses `q.flattener` and `q.bufs`
(same pattern as Rust). The 2 allocs/16B come from the result `[]X` slice escaping
via `matchesInto(resultBuf)`.

## Rust CPU Profile (macOS `sample` tool, 234 samples)

Profiled via: `sample profile_status -wait -f sample_output.txt`
Binary: `cargo build --example profile_status --release` (with `debug = 1`)

### Top-of-stack breakdown (where CPU time is actually spent)

| Function | Samples | % | Notes |
|----------|---------|---|-------|
| `read_object` (flattener) | 44 | 19% | JSON parsing loop |
| `FrozenFieldMatcher::transition_on` | 33 | 14% | Field name dispatch (HashMap lookup + DFA traversal) |
| `from_utf8` (validation) | 24 | 10% | UTF-8 validation in SegmentsTree lookups |
| `SipHasher::write` | 20 | 9% | HashMap hashing (field matcher + match set) |
| `xzm_free` (malloc) | 11 | 5% | Deallocation |
| `read_member_name` | 10 | 4% | Field name extraction |
| `try_to_match_direct` | 7 | 3% | Match orchestration |
| `q_num_stack` | 7 | 3% | Number to QNumber conversion |
| `hash_one` (BuildHasher) | 6 | 3% | HashMap key hashing |
| `read_number` | 6 | 3% | Number parsing |
| `xzm_xzone_malloc` | 5 | 2% | Allocation |
| `platform_memset` | 5 | 2% | Memory zeroing |
| `check_exists_false_direct` | 5 | 2% | Exists-false check |

### Cumulative breakdown (234 samples in main)

- **101 samples (43%)** in `flatten` (JSON parsing + field extraction)
- **125 samples (53%)** in `matches_for_fields_direct` (automaton matching)
- **4 samples (2%)** in sort / field comparison
- **4 samples (2%)** in result Vec deallocation

### Call tree summary (matching path)

```
matches_for_event (230 samples)
  +-- flatten (101 samples, 43%)
  |     +-- read_object (68 top-level + recursive)
  |     |     +-- path_arc_for_segment -> from_utf8 (11+4 = 15)
  |     |     +-- read_member_name (8)
  |     |     +-- from_utf8 in is_segment_used/get (5+5 = 10)
  |     |     +-- read_number (5)
  |     +-- from_utf8 elsewhere (3)
  |     +-- SegmentsTree::get (5)
  +-- matches_for_fields_direct (125 samples, 53%)
  |     +-- try_to_match_direct (recursive)
  |     |     +-- transition_on (70+37 = 107 total recursive)
  |     |     |     +-- HashMap lookup (SipHash) (13+13 = 26)
  |     |     |     +-- DFA traversal (within transition_on) (~30)
  |     |     |     +-- q_num_stack (4+4 = 8)
  |     |     |     +-- vec extend / alloc (6+2 = 8)
  |     |     +-- HashSet insert (match dedup) (16)
  |     |     +-- free (5+4 = 9)
  |     +-- into_vec / cleanup (3)
  +-- sort (4 samples, 2%)
```

## Go CPU Profile (pprof, 19.51s total)

Profiled via: `go test -bench=Benchmark_JsonFlattner_Evaluate_ContextFields -benchtime=5s -cpuprofile=cpu.prof`

### Key functions (cumulative)

| Function | flat | cum | Notes |
|----------|------|-----|-------|
| `coreMatcher.matchesForFields` | 50ms | 6.74s | Sort + tryToMatch loop |
| `tryToMatch` | 80ms | 6.11s | Recursive field->value matching |
| `fieldMatcher.transitionOn` | 100ms | 5.54s | `map[string]*valueMatcher` lookup |
| `valueMatcher.transitionOn` | 140ms | 4.79s | DFA/NFA traversal |
| `Quamina.MatchesForEvent` | 20ms | 4.07s | Entry point (not in bench timing) |
| `flattenJSON.readObject` | 270ms | 3.06s | JSON parsing |
| `qNumFromBytes` | 30ms | 2.34s | Number to QNumber (heap allocates) |
| `traverseDFA` | 470ms | 2.29s | Core DFA step loop |
| `flattenJSON.Flatten` | 30ms | 1.88s | Flatten entry |
| `mapaccess2_faststr` | 220ms | 930ms | Go map lookup for field transitions |
| `slices.SortFunc` (sort) | 50ms | 340ms | Sort 2 fields |
| `flattenJSON.ch` | 220ms | 220ms | Event byte access |
| `readMemberName` | 190ms | 190ms | Field name extraction |
| `skipBlock` | 140ms | 140ms | Skip unused JSON subtrees |
| `matchesInto` | 30ms | 230ms | Collect results (the 2 allocs come from here) |

### Go memory profile

```
Benchmark_JsonFlattner_Evaluate_ContextFields-14    381.8 ns/op    16 B/op    2 allocs/op
```

The 2 allocations (16 bytes) per op come from the result `[]X` slice returned by
`matchesInto(bufs.resultBuf[:0])`. When the slice exceeds the pre-allocated `resultBuf`
capacity, Go allocates.

Large allocation totals in the memory profile (GB scale) are from other tests
running in the same process, not from this benchmark's hot path.

## Architectural Differences

### Field matcher lookup

Both use hash maps for field name -> value matcher dispatch:
- **Rust:** `HashMap<String, Arc<FrozenValueMatcher<X>>>` (SipHash-1-3) in `FrozenFieldMatcher`
- **Go:** `map[string]*valueMatcher` (aeshash, hardware-accelerated on ARM64)

The `transitions`, `exists_true`, and `exists_false` maps are all `std::collections::HashMap`
(SipHash) in Rust. Only the `transition_map` (integer-keyed) inside `FrozenValueMatcher`
uses `FxHashMap`.

### SegmentsTree

- **Rust:** `FxHashMap<String, ...>` for both `nodes` and `fields` maps. However,
  lookups require `std::str::from_utf8(segment)` conversion because keys are `String`
  but input segments are `&[u8]`.
- **Go:** Uses a `SegmentsTreeTracker` interface with `IsSegmentUsed([]byte)` etc.
  Operates on raw `[]byte` throughout with no UTF-8 validation in the hot path.

### Match deduplication

- **Rust:** `FrozenMatchSet { seen: HashSet<X>, matches: Vec<X> }` — created fresh each call
  via `FrozenMatchSet::new()`. `HashSet` uses SipHash.
- **Go:** `matchSet { set: map[X]bool }` — reused via `bufs.getMatches()` + `reset()`.
  Map preserves allocated capacity across calls via `clear()`.

### Buffer reuse strategy

- **Rust:** `parking_lot::Mutex<FlattenJsonState>` + `parking_lot::Mutex<NfaBuffers>` on `Quamina`.
  Locks acquired per `matches_for_event` call. No per-call allocation for flattener/bufs.
- **Go:** `q.flattener` and `q.bufs` stored directly on `Quamina` struct. No synchronization
  needed because `Quamina` is not thread-safe (use `Copy()` for concurrent access).
  No per-call allocation for flattener/bufs. The `matchSet` is also reused via `bufs`.

### Number conversion

- **Rust:** `q_num_stack` — converts numbers to Q-number format on the stack. Fast, no allocation.
- **Go:** `qNumFromBytes` / `toQNumber` — allocates (0.51GB total in mem profile across all tests).
  Shows as 12% cumulative CPU.

## Files Modified for This Analysis

| File | Change |
|------|--------|
| `benches/matching.rs` | Added 3 decomposed benchmarks: `flatten_sort_context_fields`, `status_context_fields_no_mutex`, `match_only_context_fields` |
| `src/lib.rs` | Added `automaton()` and `segments_tree()` accessors on `Quamina` (doc-hidden) |
| `examples/profile_status.rs` | New profiling harness (1M iterations of matches_for_event on status.json) |
| `Cargo.toml` | Added `[profile.bench]` and `[profile.release]` with `debug = 1` for flamegraph/samply |

## Profiling Artifacts

- Rust samply profile: `profile_status.json` (open with `samply load profile_status.json`)
- Rust macOS sample: `sample_output.txt`
- Go CPU profile: `/Users/rishibaldawa/workspaces/quamina/cpu.prof` (`go tool pprof -http=:8080 cpu.prof`)
- Go mem profile: `/Users/rishibaldawa/workspaces/quamina/mem.prof`

## Optimization Opportunities (Ranked by Impact)

### A. Eliminate `from_utf8` in SegmentsTree hot path (~10-15ns)

**Where:** `src/segments_tree.rs` lines 104, 114, 122, 131 — four methods call
`std::str::from_utf8(segment)` on every invocation during flattening.

**Why it's slow:** The JSON flattener passes field name segments as `&[u8]`. SegmentsTree
keys are `String`. Each lookup converts `&[u8]` -> `&str` via `from_utf8()` which validates
UTF-8 (scans every byte). For status.json with ~50 field names checked, that's ~50 validations
per flatten call.

**Profile evidence:** 24/234 samples (10%) in `from_utf8`.

**Options:**
1. Use `unsafe { from_utf8_unchecked() }` — JSON spec guarantees field names are valid UTF-8.
   The flattener already rejects non-UTF-8 bytes.
2. Change SegmentsTree keys from `String` to `Vec<u8>` / `Box<[u8]>` — eliminates conversion
   entirely but changes the public API and makes debug output less readable.

### B. Reduce or eliminate Mutex overhead (~18ns)

**Where:** `src/lib.rs` lines 445-456 — two `parking_lot::Mutex` locks per call.

**Profile evidence:** `status_context_fields` (398ns) vs `status_context_fields_no_mutex`
(380ns) = 18ns difference.

**Options:**
1. `thread_local!` for flattener + bufs — eliminates Mutex entirely for the common
   single-threaded case. No API change needed.
2. Expose a `MatchContext` struct for per-thread state — common pattern in high-perf
   Rust libraries. Requires API change.
3. `try_lock` fast path — `parking_lot` already optimizes for uncontended case, limited
   additional gain.

### C. Switch FrozenFieldMatcher maps to FxHashMap (~5-8ns)

**Where:** `src/automaton/thread_safe.rs` line 68 (`transitions`), 72 (`exists_true`),
74 (`exists_false`) — all use `std::collections::HashMap` (SipHash).

**Profile evidence:** 20/234 samples (9%) in `SipHasher::write`, plus 6/234 (3%) in
`hash_one`. Most of this is in `transition_on` field name lookup.

**Why SipHash is unnecessary:** These maps are populated at build time and are read-only
during matching. The keys are field path strings from patterns, not user-controlled input.
HashDoS protection adds ~2-3x overhead vs FxHash for short string keys.

**Also affects:** `FrozenMatchSet.seen: HashSet<X>` uses SipHash for match deduplication.

### D. Reuse FrozenMatchSet across calls (~2-3ns)

**Where:** `src/automaton/thread_safe.rs` line 698 — `FrozenMatchSet::new()` called every
`matches_for_fields_direct` invocation.

**Profile evidence:** Small but measurable — first `HashSet::insert` allocates, and the
`Vec` also allocates on first push. Go avoids this by reusing via `clear()`.

**Option:** Move `FrozenMatchSet` (or just `HashSet<X>` + `Vec<X>`) into `NfaBuffers`
and reset with `.clear()` each call, preserving allocated capacity.
