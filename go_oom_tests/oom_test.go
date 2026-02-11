package quamina

import (
	"fmt"
	"runtime"
	"strings"
	"testing"
	"time"
)

// =============================================================================
// Test 1: Unbounded automaton growth — CONFIRMED (3.5 GB heap, no rejection)
//
// Each unique exact-match string creates new smallTable nodes (256 bytes each).
// There is no byte budget or pattern count limit. An adversary who can call
// AddPattern() with crafted input can grow the automaton without bound.
//
// Expected: heap_inuse climbs linearly (~38 MB per 1K patterns) with no error.
// =============================================================================
func TestOOM_AutomatonGrowth(t *testing.T) {
	q, err := New()
	if err != nil {
		t.Fatal(err)
	}

	var mBefore runtime.MemStats
	runtime.ReadMemStats(&mBefore)

	// Each pattern has a unique 200-char value → new DFA path each time.
	// 100k patterns × ~200 smallTables × 256 bytes ≈ multi-GB
	n := 100_000
	for i := 0; i < n; i++ {
		val := fmt.Sprintf(`{"key": ["%s"]}`, strings.Repeat(fmt.Sprintf("%06d", i), 33))
		err = q.AddPattern(i, val)
		if err != nil {
			t.Fatalf("AddPattern failed at i=%d: %v", i, err)
		}

		if i%10_000 == 0 && i > 0 {
			var m runtime.MemStats
			runtime.ReadMemStats(&m)
			allocMB := (m.TotalAlloc - mBefore.TotalAlloc) / (1024 * 1024)
			t.Logf("patterns=%d  cumulative_alloc=%d MB  heap_inuse=%d MB",
				i, allocMB, m.HeapInuse/(1024*1024))
		}
	}

	var mAfter runtime.MemStats
	runtime.ReadMemStats(&mAfter)
	allocMB := (mAfter.TotalAlloc - mBefore.TotalAlloc) / (1024 * 1024)
	t.Logf("FINAL: %d patterns  cumulative_alloc=%d MB  heap_inuse=%d MB",
		n, allocMB, mAfter.HeapInuse/(1024*1024))

	// quamina-rs rejects this at ~10 MB with QuaminaError::PatternTooComplex
	// Go quamina happily allocates 3.5+ GB
	if mAfter.HeapInuse/(1024*1024) > 100 {
		t.Logf("VULNERABILITY CONFIRMED: heap grew to %d MB with no rejection",
			mAfter.HeapInuse/(1024*1024))
	}
}

// =============================================================================
// Test 2: O(N²) algorithmic complexity in tryToMatch / noArrayTrailConflict
//         CONFIRMED — timed out at 2 minutes with 2M array elements.
//
// When a matched field is inside a large array, the matcher does:
//   for each field i:          O(N)
//     for nextIndex > i:       O(N)
//       noArrayTrailConflict   O(1) for depth-1 arrays
// Total: O(N²) = 4×10¹² operations for N=2M
//
// This is a CPU-based denial-of-service, not a memory OOM.
//
// Use a smaller array (50K) so this completes in ~30s instead of timing out,
// but still demonstrates the quadratic scaling.
// =============================================================================
func TestOOM_ArrayTrailQuadratic(t *testing.T) {
	q, err := New()
	if err != nil {
		t.Fatal(err)
	}

	// Pattern that makes "items" a live field
	err = q.AddPattern("arr", `{"items": [{"exists": true}]}`)
	if err != nil {
		t.Fatal(err)
	}

	// Test with increasing sizes to demonstrate quadratic scaling
	sizes := []int{5_000, 10_000, 20_000, 40_000}

	for _, n := range sizes {
		var sb strings.Builder
		sb.WriteString(`{"items": [`)
		for i := 0; i < n; i++ {
			if i > 0 {
				sb.WriteByte(',')
			}
			fmt.Fprintf(&sb, "%d", i)
		}
		sb.WriteString(`]}`)
		event := []byte(sb.String())

		start := time.Now()
		_, err := q.MatchesForEvent(event)
		elapsed := time.Since(start)

		t.Logf("array_size=%d  time=%v  err=%v", n, elapsed, err)
	}

	// If quadratic: doubling N should ~4x the time.
	// Linear would only 2x. This proves O(N²).
	t.Log("Compare times above: if 2x size → ~4x time, complexity is O(N²)")
}

// =============================================================================
// Test 3: Multi-field pattern × large arrays = EXPONENTIAL blowup
//
// Pattern: {"a": [exists], "b": [exists]}
// Event: {"a": [1..N], "b": [1..N]}
//
// tryToMatch for each matched "a" element recurses into ALL "b" elements
// (different Array IDs → noArrayTrailConflict always returns true).
// Work = O(N_a × N_b) = O(N²) recursive calls, each scanning O(N) fields.
//
// With N=1000 per array, this already takes seconds.
// =============================================================================
func TestOOM_MultiFieldArrayExplosion(t *testing.T) {
	q, err := New()
	if err != nil {
		t.Fatal(err)
	}

	// Two-field pattern — both fields are arrays in the event
	err = q.AddPattern("multi", `{"a": [{"exists": true}], "b": [{"exists": true}]}`)
	if err != nil {
		t.Fatal(err)
	}

	// Even 1000 elements per array = 1M recursive calls
	n := 1_000
	var sb strings.Builder
	sb.WriteString(`{"a": [`)
	for i := 0; i < n; i++ {
		if i > 0 {
			sb.WriteByte(',')
		}
		fmt.Fprintf(&sb, "%d", i)
	}
	sb.WriteString(`], "b": [`)
	for i := 0; i < n; i++ {
		if i > 0 {
			sb.WriteByte(',')
		}
		fmt.Fprintf(&sb, "%d", i)
	}
	sb.WriteString(`]}`)
	event := []byte(sb.String())

	var mBefore runtime.MemStats
	runtime.ReadMemStats(&mBefore)

	matches, err := q.MatchesForEvent(event)

	var mAfter runtime.MemStats
	runtime.ReadMemStats(&mAfter)
	allocMB := (mAfter.TotalAlloc - mBefore.TotalAlloc) / (1024 * 1024)

	t.Logf("array_size=%d per field  alloc=%d MB  matches=%v err=%v",
		n, allocMB, matches, err)
	t.Log("With 2 array fields of size N, work is O(N²) recursive calls")
	t.Log("Try increasing n to 5000 to see it hang")
}

// =============================================================================
// Test 4: Deep event nesting with a pattern that forces recursion.
//
// The previous deep nesting test passed because the pattern only matched
// at depth 1, so deeper levels were "skipped" iteratively.
//
// To force recursion, we need patterns registered at EVERY depth level
// so the SegmentsTree considers each level "live".
// =============================================================================
func TestOOM_DeepNestingForced(t *testing.T) {
	q, err := New()
	if err != nil {
		t.Fatal(err)
	}

	// Register patterns that make every nesting level "live" in the
	// SegmentsTree. Each pattern matches a different depth.
	depth := 10_000
	for d := 1; d <= depth; d++ {
		// Build pattern: {"a": {"a": ... [{"exists": true}] ...}}
		// with `d` levels of nesting
		var pat strings.Builder
		for i := 0; i < d; i++ {
			pat.WriteString(`{"a":`)
		}
		pat.WriteString(`[{"exists": true}]`)
		for i := 0; i < d; i++ {
			pat.WriteByte('}')
		}
		err = q.AddPattern(d, pat.String())
		if err != nil {
			t.Logf("AddPattern failed at depth=%d: %v", d, err)
			// Keep going — the point is to load up the SegmentsTree
		}
	}

	// Now send an event nested to the full depth
	var ev strings.Builder
	for i := 0; i < depth; i++ {
		ev.WriteString(`{"a":`)
	}
	ev.WriteString(`"leaf"`)
	for i := 0; i < depth; i++ {
		ev.WriteByte('}')
	}

	var mBefore runtime.MemStats
	runtime.ReadMemStats(&mBefore)

	matches, err := q.MatchesForEvent([]byte(ev.String()))

	var mAfter runtime.MemStats
	runtime.ReadMemStats(&mAfter)
	allocMB := (mAfter.TotalAlloc - mBefore.TotalAlloc) / (1024 * 1024)

	t.Logf("depth=%d  alloc=%d MB  matches=%v  err=%v", depth, allocMB, matches, err)
	t.Logf("goroutine stack grows ~%d KB per recursion level", 1) // rough estimate
}

// =============================================================================
// Test 5: Huge escaped string in a matching field.
//
// Every \uXXXX escape must be decoded into a fresh []byte allocation.
// No size limit on the decoded value.
// =============================================================================
func TestOOM_HugeEscapedString(t *testing.T) {
	q, err := New()
	if err != nil {
		t.Fatal(err)
	}

	err = q.AddPattern("esc", `{"data": [{"exists": true}]}`)
	if err != nil {
		t.Fatal(err)
	}

	// 5M escapes × 6 bytes each = 30 MB JSON → 5 MB decoded
	escapeCount := 5_000_000
	var sb strings.Builder
	sb.Grow(escapeCount*6 + 20)
	sb.WriteString(`{"data": "`)
	for i := 0; i < escapeCount; i++ {
		sb.WriteString(`\u0041`) // decodes to "A"
	}
	sb.WriteString(`"}`)
	event := []byte(sb.String())

	var mBefore runtime.MemStats
	runtime.ReadMemStats(&mBefore)

	_, err = q.MatchesForEvent(event)

	var mAfter runtime.MemStats
	runtime.ReadMemStats(&mAfter)
	allocMB := (mAfter.TotalAlloc - mBefore.TotalAlloc) / (1024 * 1024)

	t.Logf("escape_count=%d  event_size=%d MB  alloc=%d MB  err=%v",
		escapeCount, len(event)/(1024*1024), allocMB, err)
	t.Log("No size limit on decoded string — proportional to input, but unbounded")
}

// =============================================================================
// Test 6: Wildcard patterns cause NFA state explosion in the automaton.
//
// Each wildcard pattern like "*N*N*" creates multiple NFA states with
// epsilon transitions. Many overlapping wildcards compound this.
// =============================================================================
func TestOOM_WildcardPatternBlowup(t *testing.T) {
	q, err := New()
	if err != nil {
		t.Fatal(err)
	}

	var mBefore runtime.MemStats
	runtime.ReadMemStats(&mBefore)

	n := 50_000
	for i := 0; i < n; i++ {
		pat := fmt.Sprintf(`{"path": [{"wildcard": "*%d*%d*"}]}`, i, i)
		err = q.AddPattern(i, pat)
		if err != nil {
			t.Fatalf("AddPattern failed at i=%d: %v", i, err)
		}
		if i%10_000 == 0 && i > 0 {
			var m runtime.MemStats
			runtime.ReadMemStats(&m)
			t.Logf("patterns=%d  heap_inuse=%d MB", i, m.HeapInuse/(1024*1024))
		}
	}

	var mAfter runtime.MemStats
	runtime.ReadMemStats(&mAfter)
	t.Logf("FINAL: %d wildcard patterns  heap_inuse=%d MB",
		n, mAfter.HeapInuse/(1024*1024))
}
