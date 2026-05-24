//! Validates that `traverse_arena_nfa` performs zero heap allocations in
//! steady state.
//!
//! Upstream Go's commit `e33139f` ("nfa: simplify smallTable.step, eliminate
//! per-traverse stepOut alloc") removed a `&stepOut{}` literal that was
//! escaping to the heap once per `traverseNFA` call. Our Rust port is
//! claimed to avoid the same allocation by returning `StateId` (a `u32`
//! newtype) directly from `dstep` rather than threading a struct through
//! the call. This test backs that claim with a real measurement: a custom
//! global allocator counts every `alloc` call during 1000 traversals after
//! warmup, and the assertion is "exactly zero".
//!
//! The harness lives in its own integration-test binary so the global
//! allocator is scoped to this file alone — it does not perturb the unit
//! test allocator.
//!
//! The counter and its enable flag are process-global, and the allocator sees
//! allocations from every thread. Both measured scenarios therefore live in a
//! single `#[test]` function and run sequentially: that keeps exactly one test
//! thread alive in this binary, so no sibling test's setup or the harness's
//! result reporting can allocate while a counting window is open. (When the
//! two scenarios were separate `#[test]`s, `cargo test` ran them in parallel
//! and their windows interleaved, miscounting one test's allocations against
//! the other.)

#![allow(unsafe_code)]

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use quamina::automaton::FieldMatcher;
use quamina::automaton::arena::{
    ARENA_VALUE_TERMINATOR, NfaBuffers, make_shellstyle_arena_fa, traverse_arena_nfa,
};

struct CountingAlloc;

static ALLOCS: AtomicUsize = AtomicUsize::new(0);
/// Counting is gated so only the measured window contributes. Any allocation
/// performed during test setup (Vec growth in NfaBuffers, Arc construction,
/// arena building) happens with the gate closed.
static COUNTING_ENABLED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for CountingAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if COUNTING_ENABLED.load(Ordering::Relaxed) != 0 {
            ALLOCS.fetch_add(1, Ordering::Relaxed);
        }
        // SAFETY: forwarding to the real system allocator with the same layout.
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        // SAFETY: forwarding to the real system allocator with the same layout.
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        if COUNTING_ENABLED.load(Ordering::Relaxed) != 0 {
            // realloc may move the buffer — count it as one allocation event.
            ALLOCS.fetch_add(1, Ordering::Relaxed);
        }
        // SAFETY: forwarding to the real system allocator with the same layout.
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static GLOBAL: CountingAlloc = CountingAlloc;

/// Builds the shellstyle `A*` FA, warms the reusable buffers, then counts the
/// allocations performed by 1000 steady-state traversals of `value`.
///
/// `expect_match` asserts the warmup behaved as intended for the chosen value,
/// catching a broken setup before it masquerades as an allocation result.
fn count_steady_state_allocs(value: &[u8], expect_match: bool) -> usize {
    // Shellstyle "A*" gives us an FA with a junction, a spinner with a 256-byte
    // self-loop, an epsilon transition, and a value-terminator endpoint — i.e.
    // every interesting code path inside `traverse_arena_nfa`.
    let next_field = Arc::new(FieldMatcher::new());
    let (mut arena, start) = make_shellstyle_arena_fa(b"A*", next_field);
    arena.precompute_epsilon_closures();
    arena.flatten_tables();

    let mut bufs = NfaBuffers::with_capacity();

    // Warmup. The first few traversals push into the internal Vecs / HashSets
    // inside `NfaBuffers`, growing them to their working capacity. Sixteen
    // iterations is overkill for this small FA but cheap insurance.
    for _ in 0..16 {
        traverse_arena_nfa(&arena, start, value, &mut bufs);
        assert_eq!(
            !bufs.transitions.is_empty(),
            expect_match,
            "warmup produced an unexpected match result — test setup is broken"
        );
    }

    // Open the counting window and measure 1000 steady-state traversals.
    ALLOCS.store(0, Ordering::Relaxed);
    COUNTING_ENABLED.store(1, Ordering::Relaxed);
    for _ in 0..1000 {
        traverse_arena_nfa(&arena, start, value, &mut bufs);
    }
    COUNTING_ENABLED.store(0, Ordering::Relaxed);
    ALLOCS.load(Ordering::Relaxed)
}

#[test]
// Miri intercepts the global allocator and runs the interpreter ~100x slower,
// so allocation counts aren't representative of release-mode native code and
// 1000 traversals would risk the CI timeout. Memory safety of the stepping
// path is covered by the unit tests in `src/automaton/arena.rs`.
#[cfg_attr(miri, ignore)]
fn traverse_arena_nfa_is_alloc_free_in_steady_state() {
    // Matching value: ends with the arena's value terminator and runs the
    // spinner to a successful endpoint.
    let mut matching: Vec<u8> = b"ALICE_IN_WONDERLAND_AAAAAAAAAAAA".to_vec();
    matching.push(ARENA_VALUE_TERMINATOR);
    let match_allocs = count_steady_state_allocs(&matching, true);
    assert_eq!(
        match_allocs, 0,
        "traverse_arena_nfa allocated {match_allocs} time(s) across 1000 steady-state matches; the inner NFA step is supposed to be allocation-free (upstream Go e33139f equivalent)"
    );

    // Non-matching value: starts with 'B', so the FA dies out on the first
    // byte. This still exercises every branch of the stepping loop — in fact
    // more of them, because the spinner has to absorb every byte rather than
    // terminating early. Same allocation guarantee.
    let mut no_match: Vec<u8> = b"BOB_NEVER_STARTS_WITH_A".to_vec();
    no_match.push(ARENA_VALUE_TERMINATOR);
    let no_match_allocs = count_steady_state_allocs(&no_match, false);
    assert_eq!(
        no_match_allocs, 0,
        "traverse_arena_nfa allocated {no_match_allocs} time(s) across 1000 no-match traversals"
    );
}
