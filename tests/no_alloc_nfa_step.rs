//! Validates that `traverse_arena_nfa` performs zero heap allocations in
//! steady state (Go upstream e33139f equivalent).
//!
//! Lives in its own integration-test binary so the custom global allocator
//! is scoped here and does not perturb the unit-test suite.
//!
//! The counter is thread-local rather than process-global because the test
//! harness keeps a background main thread alive even when only one `#[test]`
//! exists, and that thread occasionally allocates. `COUNTING_ENABLED` is a
//! cheap process-wide gate so the allocator skips the TLS read when no
//! window is open.

#![allow(unsafe_code)]

use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::Cell;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use quamina::automaton::FieldMatcher;
use quamina::automaton::arena::{
    ARENA_VALUE_TERMINATOR, NfaBuffers, make_shellstyle_arena_fa, traverse_arena_nfa,
};

struct CountingAlloc;

// `const` initializers keep the TLS slot in the binary's static TLS template,
// so accessing these from inside the allocator impl cannot trigger allocation.
thread_local! {
    static TL_COUNTING_ENABLED: Cell<bool> = const { Cell::new(false) };
    static TL_ALLOCS: Cell<usize> = const { Cell::new(0) };
}

static COUNTING_ENABLED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for CountingAlloc {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if COUNTING_ENABLED.load(Ordering::Relaxed) != 0 {
            TL_COUNTING_ENABLED.with(|f| {
                if f.get() {
                    TL_ALLOCS.with(|c| c.set(c.get() + 1));
                }
            });
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
            TL_COUNTING_ENABLED.with(|f| {
                if f.get() {
                    TL_ALLOCS.with(|c| c.set(c.get() + 1));
                }
            });
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

    // Warmup: grow NfaBuffers internals to working capacity before the window.
    for _ in 0..16 {
        traverse_arena_nfa(&arena, start, value, &mut bufs);
        assert_eq!(
            !bufs.transitions.is_empty(),
            expect_match,
            "warmup produced an unexpected match result — test setup is broken"
        );
    }

    // Prime TLS before the window so first-access overhead is not counted.
    TL_ALLOCS.with(|c| c.set(0));
    TL_COUNTING_ENABLED.with(|f| f.set(false));

    COUNTING_ENABLED.store(1, Ordering::Relaxed);
    TL_COUNTING_ENABLED.with(|f| f.set(true));
    for _ in 0..1000 {
        traverse_arena_nfa(&arena, start, value, &mut bufs);
    }
    TL_COUNTING_ENABLED.with(|f| f.set(false));
    COUNTING_ENABLED.store(0, Ordering::Relaxed);
    TL_ALLOCS.with(Cell::get)
}

#[test]
// Miri intercepts the global allocator and runs the interpreter ~100x slower,
// so allocation counts aren't representative of release-mode native code and
// 1000 traversals would risk the CI timeout. Memory safety of the stepping
// path is covered by the unit tests in `src/automaton/arena.rs`.
#[cfg_attr(miri, ignore)]
fn traverse_arena_nfa_is_alloc_free_in_steady_state() {
    let mut matching: Vec<u8> = b"ALICE_IN_WONDERLAND_AAAAAAAAAAAA".to_vec();
    matching.push(ARENA_VALUE_TERMINATOR);
    let match_allocs = count_steady_state_allocs(&matching, true);
    assert_eq!(
        match_allocs, 0,
        "traverse_arena_nfa allocated {match_allocs} time(s) across 1000 steady-state matches; the inner NFA step is supposed to be allocation-free (upstream Go e33139f equivalent)"
    );

    // Non-matching: 'B' kills the FA on the first byte, exercising the
    // no-transition path through the full value length.
    let mut no_match: Vec<u8> = b"BOB_NEVER_STARTS_WITH_A".to_vec();
    no_match.push(ARENA_VALUE_TERMINATOR);
    let no_match_allocs = count_steady_state_allocs(&no_match, false);
    assert_eq!(
        no_match_allocs, 0,
        "traverse_arena_nfa allocated {no_match_allocs} time(s) across 1000 no-match traversals"
    );
}
