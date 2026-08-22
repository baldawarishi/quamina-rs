# Findings

Full sweep in `results/sweep.csv` (100 rows), ground truth in
`results/ground_truth.csv` (200 rows). Reproduce both from scratch with
`cargo run --release --bin sweep` from this directory. See JOURNAL.md for
the narrative — corpus stats, the two bugs found while producing this data
(an OOM from an unbounded automaton cache, and a mutation-harness op-cost
bug that silently overshot low edit-rate targets), and the design
decisions behind the numbers below.

## Hypothesis: supported, but only once the window is wide enough to feel edits

> Claim: indexing normalized token k-grams in an FST and querying with a
> Levenshtein automaton at k=1 recovers substantially more edited clone
> pairs than an exact k-gram inverted index, at acceptable candidate-set
> inflation.

**Supported at w ∈ {16, 24, 32}, not supported (because there's no gap to
close) at w ∈ {8, 12}.** Averaged over the differentiated regime (w=16,
24, 32; 15 (w, rate) cells, N=40 queries each for exact/k=0/k=1, N=10 for
k=2 — see "What would break this" below):

| arm | mean recall | vs. exact |
|---|---|---|
| exact (baseline) | 0.815 | — |
| fst, k=0 (sanity check) | 0.815 | +0.000 |
| fst, k=1 | 0.922 | **+0.107** |
| fst, k=2 | 0.980 | **+0.165** |

At the single worst cell for the baseline — w=24, edit_rate=30% — exact
recall is **0.625**; k=1 recovers **0.900**; k=2 recovers **1.000**. At
w=32, edit_rate=30%, exact is **0.525**, k=1 is **0.650**, k=2 is
**0.900**. These are the cells where the hypothesis's claim is most
clearly true: a fuzzy k-gram index recovers roughly a third more true
clone pairs than the exact baseline at the same window size.

At w ∈ {8, 12}, both arms average **0.993** recall — the windows are
short enough that essentially every clone still contains at least one
untouched 8- or 12-token run, so the exact index already finds it and
there is no gap for fuzzy matching to close. This is the "null result" the
task said was a fine outcome, and it's exactly what shows up at small w:
**no measurable benefit, reported plainly.**

### The `k=0` sanity check passes exactly

Every one of the 25 `(w, edit_rate)` cells has fst-arm `k=0` recall
*bit-for-bit equal* to the exact arm's recall (checked programmatically
against all 25, not eyeballed — see the `k0_equivalence.rs` integration
test for the same property re-verified on synthetic data on every
`cargo test`). If it hadn't matched, per the task's own instruction, that
would have meant a bug to find before trusting anything else here; it
matched everywhere, so the rest of this report is standing on a verified
foundation rather than an assumed one.

## Candidate-set inflation: modest, and cheapest exactly where the benefit is

| regime | exact candidates/query | k=1 | k=2 |
|---|---|---|---|
| w ∈ {8, 12} (saturated, no recall benefit) | 9.07 | 15.54 (1.71x) | 23.40 (2.58x) |
| w ∈ {16, 24, 32} (differentiated, real recall benefit) | 1.04 | 1.32 (**1.27x**) | 1.79 (**1.72x**) |

This is the shape you'd want if you were going to recommend using this in
practice: the window sizes where fuzzy matching actually earns its
recall pay the *smaller* relative inflation (1.27-1.72x), while the
window sizes where it doesn't help (w=8/12) are exactly where it would
also cost the most relatively (1.71-2.58x) for zero benefit. In absolute
terms, at w=24/32 a query goes from returning about 1 candidate to about
1-2 candidates on average — a downstream verification stage would barely
notice the difference in workload.

**By this measure alone, the hypothesis's "acceptable candidate-set
inflation" clause holds cleanly.** The catch is below.

## The real cost is not candidates, it's query latency — and it's severe

| arm | mean p50 latency | mean p99 latency | vs. exact (p50) |
|---|---|---|---|
| exact | 4.6 µs | 29.4 µs | 1x |
| fst, k=0 | 1.9 ms | 9.1 ms | ~420x |
| fst, k=1 | 25.2 ms | 116.4 ms | ~5,500x |
| fst, k=2 | **769.2 ms** | **2.78 s** | **~167,000x** |

This is averaged across all 5 window sizes; it gets worse, not better, as
`w` grows — at `w=32, k=2` the p99 was 7.4 **seconds** for a single query
against a 40-fragment index (see `results/sweep.csv`). Index *build* time
and *size* are not the problem (both arms build in single-digit-to-tens
of milliseconds and produce payloads of the same order of magnitude — see
JOURNAL.md's build/payload table); the entire cost is in constructing a
fresh Levenshtein automaton per query gram, which the `fst` crate's own
source describes as a "proof of concept" whose DFA can run into the tens
of megabytes per construction (`fst-0.4.7/src/automaton/levenshtein.rs`).

An earlier version of this experiment tried to amortize that cost with a
cache keyed by `(gram, k)`, on the theory that real code repeats grams
often enough to make it worthwhile. It didn't — mutated query grams
don't repeat enough within a sweep to earn back a tens-of-MB, rarely-reused
cache entry, and the real run OOM-killed at ~14GB RSS before finishing a
single window size. The numbers above are from the *uncached* version,
which is slower on paper but was the one that actually finished, stayed
under 30MB RSS the entire run, and is what's shipped in `index_fst.rs`.

**This is the actual answer to "at acceptable candidate-set inflation":**
candidate-set inflation is acceptable; end-to-end query cost, at least
with this specific automaton implementation, is not — a k=2 query taking
the better part of a second (up to several seconds at p99) is disqualifying
for anything but a small, offline, patience-tolerant batch job, regardless
of how good its recall is.

## So: is the hypothesis supported?

**Yes, on recall, at w ≥ 16 — and no, not as a practical proposal, once
query latency is counted as a cost.** The two halves don't contradict
each other; they're answers to different questions the task asked for
together. If a future version of this experiment swapped in a cheaper
Levenshtein automaton (SymSpell-style deletion neighborhoods, a proper
production DFA builder, or bounding `k` to 1 and accepting ~25ms/query
instead of chasing `k=2`), the recall story above would very plausibly
still hold at a latency cost that's merely "expensive" (~5,500x exact)
instead of "impractical" (~167,000x exact at k=2).

**Best cost/benefit point in this data: `w=24, k=1`.** It's within the
window range where the recall gap is real (baseline drops as low as
0.625, k=1 recovers 0.900-1.000 across edit rates), candidate inflation
is 1.27x, and query latency (mean p50 ≈ 30ms) is two orders of magnitude
better than `k=2`'s. `k=2` buys real additional recall over `k=1` in
several cells (see the table at the top) but at roughly 30x more latency
for it — a bad trade unless recall is worth arbitrarily large query cost.

## What would break this result

- **Sample size, especially for `k=2`.** The standard sample is
  `N_STANDARD=40` originals (40 queries per `(w, rate)` cell for
  exact/k=0/k=1 — recall granularity is 2.5 percentage points per query).
  `k=2` runs on a nested `N_K2=10` subsample (10 queries/cell, 10-point
  granularity) purely because of the latency problem above — a full N=40
  run at k=2 and w=32 would be projected at roughly 20+ minutes for that
  one cell alone. Several of `k=2`'s "1.000 recall" cells are consistent
  with "the 10 sampled queries all happened to succeed," not necessarily
  "k=2 achieves perfect recall in general." The non-monotonic dips visible
  in a few curves (e.g. exact recall at w=12 rising from 0.925 back to
  1.000 between rate=0.20 and rate=0.30) are almost certainly this same
  N=40 sampling noise, not a real reversal — do not read them as signal.
- **This is one corpus, one language, one author's style.** All 1,512
  fragments come from quamina-rs's own `.rs` files — a single
  systems-Rust codebase with its own idioms, comment density, and
  statement-length distribution (mean ~24 tokens/statement). A codebase
  with much shorter statements, heavier macro use, or a different
  language entirely could shift where the w=16-ish "differentiation
  threshold" falls.
- **Mutations are applied to the normalized token sequence, not to
  re-parsed Rust source** (see JOURNAL.md). Real edited clones arise from
  editing actual source and would correlate in ways independent random
  token/statement edits don't capture (e.g., a rename touching every
  occurrence of an identifier at once — impossible here, since all
  identifiers already collapse to one `IDENT` sentinel before mutation
  ever runs). This makes the synthetic clones somewhat *harder* to match
  on average than typical real renames-only Type-3 clones, since a real
  rename is invisible to this normalization while our synthetic edits are
  visible token-level changes.
- **No verification stage.** Both arms report a candidate *set*, not a
  final match. Candidate counts are small (near 1 at the interesting
  window sizes), so a verification pass would be cheap here — but that's
  unverified, not measured.
- **The `fst` crate's Levenshtein automaton is explicitly a proof of
  concept** (its own doc comment says so). The severe latency numbers
  above are a property of *this implementation*, not a law of fuzzy
  k-gram matching in general — see "So: is the hypothesis supported?"
  above.

## Autonomy detours taken

None beyond the compute-budget adaptations already covered in JOURNAL.md
(nested `N_K2` subsample, removing the automaton cache). No winnowing or
per-token-kind substitution-cost variant was attempted — the latency
problem consumed the time budget that would have gone to those, and fixing
the two bugs (OOM, edit-rate overshoot) took priority over adding new
variants. If continuing: winnowing is the natural next experiment, since
it directly attacks the `k=2` latency problem (fewer, sparser query grams
means fewer automaton constructions) rather than working around it with
smaller samples the way this run had to.
