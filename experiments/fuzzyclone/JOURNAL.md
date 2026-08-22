# Journal

Append-only. Newest entries at the bottom. Times are wall-clock during the
session, not commit timestamps.

## 2026-08-22 — setup and design decisions

**Scope.** New standalone crate `experiments/fuzzyclone`, not a workspace
member of quamina, no dependency on the `quamina` crate. It only reads
quamina-rs's own `.rs` files as a text corpus at run time. Nothing in
`src/`, `Cargo.toml`, etc. at the repo root was touched.

**Parser: tree-sitter-rust, not syn.** Chose tree-sitter over syn for one
concrete reason: comments. The task's normalization scheme wants comments as
one of the collapsed token-kind sentinels, alongside identifiers, literals,
and lifetimes. `syn`/`proc_macro2` tokenize *after* rustc-style lexing, which
throws comments away before parsing even starts (they're not tokens in the
grammar) — a fragment normalized via syn would simply never contain a
COMMENT symbol. tree-sitter's concrete syntax tree keeps comments as real
"extra" nodes at the position where they occur, so a straightforward
tree-walk picks them up for free, in-order, without a hand-rolled lexer
bolted on the side. tree-sitter also tolerates partial/non-standalone input,
which isn't needed here (we parse whole files) but cost nothing.

Trade-off taken deliberately: tree-sitter-rust pulls in a C dependency
(compiled via `cc`), which is heavier than pure-Rust syn. Verified it
compiles cleanly in this sandbox before committing to it (see below).

**Symbol representation: single-byte ASCII, asserted.** Every task the
sweep does — building k-gram strings, indexing them in an FST, and
constructing a Levenshtein automaton per query gram — needs one invariant
to hold: byte offset == Unicode scalar offset == token offset. If a token
symbol were multi-byte UTF-8, a byte-sliced k-gram window would not
correspond to exactly `w` tokens, and the fst crate's Levenshtein automaton
(which counts edit distance in Unicode *characters*, not bytes — confirmed
by reading `fst-0.4.7/src/automaton/levenshtein.rs`) would silently measure
something other than token-level edit distance.

Decision: the token vocabulary interns each distinct token-kind symbol
(IDENT, LIT_INT, keyword text, punctuation text, ...) as a single ASCII
byte in the range `1..=126` (0 reserved as "unused/never emitted", 127+
left unused as headroom). This makes every one of the three offset spaces
identical by construction. `Vocab::intern` asserts `code != 0 && code.is_ascii()`
at the point the byte is minted, and panics outright if the corpus needs
more than 126 distinct symbols (it doesn't come close — see below). A
`kgram::assert_ascii_symbols` sanity check re-asserts the invariant on every
fragment's symbol buffer before it's indexed, and `tests/` has a dedicated
test for it. This is the "assert it holds" the task asked for, not just a
comment.

**Sentinel collapse rules** (implemented in `tokenize.rs`):
- `identifier`, `type_identifier`, `field_identifier`,
  `shorthand_field_identifier` → `IDENT`
- `integer_literal` → `LIT_INT`, `float_literal` → `LIT_FLOAT`,
  `string_literal`/`raw_string_literal` → `LIT_STR`, `char_literal` →
  `LIT_CHAR`, `boolean_literal` → `LIT_BOOL`, `negative_literal` → `LIT_NEG`
  (rare, only appears in match patterns like `-1`)
- `line_comment`, `block_comment` → `COMMENT` (covers doc comments too —
  `doc_comment` is a nested child field inside these, never visited
  separately since we stop recursion at the comment node)
- `lifetime` → `LIFETIME`

For all of the above, recursion into the node's children is **skipped** —
they're treated as atomic, so a string literal's quotes/escapes/content
don't leak extra symbols into the sequence. Everything else falls through
a generic rule: a leaf node's `kind()` string *is* the symbol (this is
exactly what makes keywords and punctuation "preserved" — `fn`, `if`,
`{`, `->`, `::` etc. are leaf token kinds whose kind() is the literal
text), and a non-leaf node is walked into without emitting anything for
itself (structural nodes like `block`, `binary_expression`, `call_expression`
contribute nothing directly; their leaf descendants do).

**Statement boundaries for mutation.** A function's `block` body's direct
named children (excluding the injected comment "extra" nodes) are treated
as its statement list: `let_declaration`, `expression_statement`, item
declarations, the trailing tail expression, etc. Each one's token-index
span within the fragment's symbol vector is recorded during the same
normalization walk (by node id, not by re-parsing), giving `stmt_ranges:
Vec<(usize, usize)>` alongside `symbols: Vec<u8>`.

**Mutation operates on the normalized representation, not on Rust source
text.** This is a deliberate scope cut, called out up front because it's
the thing most likely to matter if this experiment is extended: insert/
delete/substitute/swap are applied directly to the `Vec<u8>` symbol
sequence plus its statement ranges, not by editing and re-parsing actual
`.rs` source. Consequences: (1) mutated fragments are not guaranteed to be
syntactically valid Rust — irrelevant here since neither arm parses them,
both only ever see normalized token sequences; (2) "substitute token" on an
already-collapsed IDENT or LITERAL position is a byte-identical no-op
under this alphabet (renaming a variable doesn't change the normalized
sequence at all — that's the entire point of collapsing identifiers, but
it does mean substitute-token mutations are effectively drawn from the
smaller "structural" part of the alphabet to have any visible effect; see
`mutate.rs`); (3) "two independent statements" for the swap op is not
verified independent by any dataflow analysis — any two distinct statement
slots in the same fragment are eligible. If this experiment graduates
beyond a skeleton, source-level mutation (then re-normalizing through the
same tree-sitter pipeline) would close this gap and is the most important
thing to fix first.

**Corpus source.** All `.rs` files under the quamina-rs repo root except
`target/`, `experiments/` (self), and `playground/` (generated wasm glue).
That's `src/`, `benches/`, `examples/`, `tests/`, `fuzz/` — about 58.7k
lines. `function_item` nodes are collected from the whole parse tree
(top-level fns, impl methods, trait default methods, nested fns), each
normalized, and any fragment under 40 normalized tokens is discarded per
the brief.

**Verified before writing real code:** `fst = { features = ["levenshtein"] }`,
`tree-sitter` + `tree-sitter-rust` (needs a C toolchain — `cc`/`gcc`/`clang`
are present in this sandbox), and `rand 0.10`/`rand_chacha 0.10` (matching
the API quamina's own dev-dependencies already pin to) all fetch and
compile cleanly against crates.io from here. Read `rand-0.10.2`'s source
directly to confirm the post-0.9 API shape (`Rng`/`RngExt`/`SeedableRng` via
`rand::prelude::*`, `rng.random_range(..)`, `SeedableRng::seed_from_u64`)
rather than guessing and iterating against compiler errors.

**Planned index construction that avoids redundant work.** The FST and the
exact hashmap are both built once per window size `w`, over the *original*
(unmutated) corpus fragments only — `k` only affects the query-time
automaton, not the index, so we don't rebuild the FST per `k`. Within a
sweep cell, distinct Levenshtein automata are cached by query-gram string
(`HashMap<String, Rc<Levenshtein>>`) because real code is structurally
repetitive — many k-grams recur verbatim across fragments and across a
fragment's own mutation siblings — and building a fresh DFA per gram
instance would otherwise dominate wall time for no benefit. This is a
pragmatic cache to keep `cargo run --bin sweep` finishing in reasonable
time on a single unoptimized core, not a performance investigation in its
own right.

Next: write `tokenize.rs` + `corpus.rs`, then a quick manual run over a
handful of quamina source files to sanity-check symbol counts and fragment
counts before building the mutation harness on top.

## 2026-08-22 — tokenizer + corpus, first real numbers

`tokenize.rs` and `corpus.rs` written per the design above; 13 unit tests
passing (Vocab interning, ASCII-invariant assertions catching a planted
zero byte and a planted non-ASCII byte, sentinel collapse, statement-range
slicing, exact-arm and fst-arm basic k=0/k=1/k=2 sanity on tiny synthetic
fragments).

Ran the corpus builder over the real repo (`src/`, `benches/`, `examples/`,
`tests/`, `fuzz/`, excluding `target/`, `experiments/`, `playground/`):

- **1512 fragments**, **76 distinct symbols** (well under the 126-symbol
  budget — real Rust doesn't come close to needing the headroom).
- mean 148.9 normalized tokens/fragment, min 40 (the filter floor, working
  as designed), max 1579.
- mean 6.3 statement slots/fragment, zero fragments with 0 statements (so
  `InsertStatement`/`SwapStatements` always have somewhere to work).
- Spot-checked normalized output by eye
  (`benches/add_pattern_scaling.rs:39`): `fn IDENT ( IDENT : & [ IDENT ] )
  { let mutable_specifier IDENT = IDENT :: < primitive_type > :: IDENT ( )
  ; ...` — keywords (`fn`, `let`, `mutable_specifier` i.e. `mut`),
  punctuation (`(`, `::`, `<`), and `primitive_type` all preserved
  distinctly; identifiers collapsed uniformly. Matches the design intent
  exactly.

`mutate.rs` written next: 5 unit tests (swap conserves total length and
correctly relocates statement-length metadata, delete removes exactly one
statement's worth of tokens, insert grows by exactly the donor length,
substitute always changes at most the one chosen byte, and — important for
reproducibility — two `ChaCha8Rng` instances seeded identically produce
byte-identical mutated output).

`index_exact.rs` and `index_fst.rs` written with a shared `QueryResult {
candidates, grams_queried }` shape so `sweep.rs` can treat both arms
uniformly. Read the `fst` crate's `MapBuilder`/`Map`/`Levenshtein` API
directly from its vendored source rather than guessing (`map.search(&aut)
.into_stream()` yields `(&[u8], u64)`; `MapBuilder::memory()` +
`.into_map()` avoids a fallible round-trip through a `Vec<u8>` writer).

## 2026-08-22 — the Levenshtein automaton cost problem (the big one)

Before writing `sweep.rs` for real, timed a throwaway `src/bin/probe.rs`
against small slices of the real corpus to estimate whether the full
planned grid (5 `w` values × 3 `k` values × 5 edit rates × ~1512
fragments × 5 clones each) would finish in any reasonable time. It would
not have, by roughly two orders of magnitude. Concretely, with an
automaton *cache* keyed by `(gram, k)` in place (see `index_fst.rs`'s
original design — cache automata because real code repeats grams a lot):

| w  | k | measured cost |
|----|---|----------------|
| 16 | 0 | 58.9 µs/gram |
| 16 | 1 | 3.88 ms/gram |
| 16 | 2 | 44.5 ms/gram |
| 8  | 2 | 3.6 ms/gram |
| 32 | 2 | 96.3 ms/gram |

k=0 is cheap (it degenerates to something close to exact-string matching).
k=1 and especially k=2 are not, and the cost grows steeply with `w`. This
matches the `fst` crate's own doc comment on `Levenshtein`, read directly
from `fst-0.4.7/src/automaton/levenshtein.rs`: *"this implementation is a
proof of concept... it can use enormous amounts of memory (tens of MB
before a hard-coded limit)."* — construction cost, not the FST search
itself, is the bottleneck; per-DFA-state cost is `[Option<usize>; 256]`
(~2KB) times up to `DEFAULT_STATE_LIMIT = 10_000` states, so a single
automaton can legitimately run to tens of megabytes.

Ran the real `sweep` binary with that cache in place anyway, at a modest
N_STANDARD=40 / N_K2=10 sample, expecting the cache to earn its keep on
real code's structural repetition. It didn't: individual query p50
latencies reached **3.07 seconds** (w=8, k=2, edit_rate=0.02) and the
process was OOM-killed by the container's cgroup at **~14GB RSS**
(confirmed via `dmesg`: `oom-kill:constraint=CONSTRAINT_MEMCG ...
task=sweep ... anon-rss:13943052kB`). Root cause: mutated query grams
across ~200 clones turned out not to repeat anywhere near enough to
amortize the cache's cost, so it just accumulated thousands of
never-reused, tens-of-MB automata with no eviction.

**Fix: deleted the cache.** `FstIndex::query` now builds a fresh
`Levenshtein` automaton per gram per call, every time — the simplest
possible thing, which also happens to be what "skip optimization" was
already telling me to do. Re-measured the same cells *without* caching:

| w  | k | uncached cost |
|----|---|----------------|
| 8  | 1 | 148 µs/gram |
| 16 | 1 | 267 µs/gram |
| 32 | 1 | 593 µs/gram |
| 8  | 2 | 994 µs/gram |
| 16 | 2 | 1.95 ms/gram |
| 32 | 2 | 18.3 ms/gram |

10-100x faster than the "cached" run, and flat, predictable memory (the
real sweep run's RSS stayed at tens of MB throughout, checked via `ps`).
The lesson isn't "caching is bad" in general — it's that caching a
tens-of-MB, rarely-reused object by an effectively-unique key is a
memory leak with a hash-lookup costume on. This is now load-bearing for
FINDINGS.md's cost/benefit answer, not a footnote: arm B's *practical*
cost includes this trap, and a production implementation would need
either a bounded/LRU cache, a cheaper automaton construction than this
crate's proof-of-concept DFA builder, or no cache at all (what we shipped)
plus acceptance of the per-query cost measured above.

**Sample sizing, revised.** With the uncached numbers, budgeted the full
grid: k=0 negligible, k=1 across all 5 w's at N_STANDARD=40 ≈ well under a
minute, k=2 across all 5 w's at N_K2=10 (nested first-10 of the standard
40, not an independent sample — same population, directly comparable) ≈
low minutes. Kept `W_VALUES = [8, 12, 16, 24, 32]` exactly as specified
rather than trimming it — the point of removing the cache was precisely to
make the full spec-shaped grid affordable instead of needing to cut it
down further.

Launched the real `sweep` run in the background; got through w=8, w=12,
w=16, and most of w=24 (recall=1.000 for both arms through w=16 at every
edit rate; w=24 is where exact-arm recall first drops below 1.0 — see
below) before I caught a second, more important bug and killed it.

## 2026-08-22 — ground truth was lying: edit-rate overshoot bug

While the sweep was still running w=32, spot-checked the
`results/ground_truth.csv` it had already written for the completed
fragments, since the task is explicit that "every number you report must
trace back to this." First few rows for original_id=0:

```
clone_id,edit_rate_bucket,actual_edit_rate,ops_applied
0,0.02,0.232,1
1,0.05,0.174,1
2,0.10,0.174,1
3,0.20,0.623,1
```

`actual_edit_rate` is wildly higher than `edit_rate_bucket` in every row,
and `ops_applied=1` for all of them — the mutation loop was exiting after
a *single* operation, nowhere near the target. Computed it over the full
200-row ground truth table: mean actual rate for the 2% bucket was
**22.3%**, for the 5% bucket **18.9%**, 10%→25.0%, 20%→40.5%, 30%→49.0%.
Every bucket overshot by roughly 2-4x, not a rounding-level discrepancy.

Root cause, once I looked at `mutate_one`'s op-selection loop: it picks
uniformly at random among all *feasible* ops every iteration, and
`InsertStatement`/`DeleteStatement`/`SwapStatements` each cost a whole
statement's worth of tokens — this corpus averages 148.9 tokens over 6.3
statements/fragment, so ~24 tokens/statement. A 2% target on a 150-token
fragment is a budget of 3 tokens. The very first random draw has roughly
a 3-in-4 chance of picking a statement op, which alone spends ~24 tokens
against a 3-token budget — massive overshoot in one step, loop exits
immediately (`ops_applied=1` confirms this is exactly what happened).
This silently made the `edit_rate` sweep axis nearly meaningless: the "2%"
and "10%" buckets weren't actually distinguishable in real edit distance
applied.

**Fix** (`mutate.rs`, `mutate_one`): added `STATEMENT_OP_MIN_BUDGET = 10`.
Below that many tokens of *remaining* budget, only `SubstituteToken`
(cost exactly 1) is offered; statement-level ops are only eligible once
there's enough remaining budget that picking one won't blow past the
target by a whole statement. This doesn't change what the four op kinds
*are* or bias which ops appear at moderate-to-high target rates (30% of a
150-token fragment is a 45-token budget, comfortably above the threshold
the whole time) — it only stops the small buckets from being dominated by
one oversized first draw.

Added a regression test,
`mutation_is_deterministic_given_a_seed`'s neighbor
`low_target_rates_converge_close_instead_of_overshooting_wildly`: builds a
fragment with statement sizes matching the real corpus's ~24-token
average, runs 20 different seeds at the 2% and 5% targets, and asserts
the worst-case overshoot across all of them stays within
`STATEMENT_OP_MIN_BUDGET` tokens of the target (bounded by construction
now, not just "usually fine"). Passes.

Killed the in-flight sweep (its ground truth was compromised) and
relaunched from scratch with the fix in place. This cost real time, but
finding it *because* I read the raw ground-truth CSV instead of trusting
the recall numbers alone is exactly the kind of check the task's "every
number must trace back to this" line is asking for — recall numbers
looked plausible (1.000 across the board) even while the edit-rate axis
underneath them was broken, which is precisely the failure mode a
spot-check of raw data catches and a summary-only report would have
missed entirely.

Spot-checked the corrected `results/ground_truth.csv` the same way before
trusting it: mean actual edit rate per bucket came out 0.0203 / 0.0544 /
0.1229 / 0.3102 / 0.4197 against targets 0.02 / 0.05 / 0.10 / 0.20 / 0.30
— close at the low end (where the fix mattered most), with the expected
looser tracking at higher targets where statement-level ops are eligible
from the start and their coarser granularity is proportionally less
distorting. Good enough to proceed.

## 2026-08-22 — full sweep completed; results and analysis

Second run finished clean: 100 rows in `results/sweep.csv`, no crashes,
RSS stayed under 30MB the entire run (confirms the cache removal fixed
the memory problem, not just the specific cell that OOM-killed before).
Total wall time for the full `w × k × rate` grid was on the order of
15-20 minutes, dominated almost entirely by the `k=2` cells (see the
latency table in FINDINGS.md) — `k=0`/`k=1`/exact together are a rounding
error against `k=2`'s cost.

Verified the k=0-vs-exact sanity check programmatically over the real
data (not just eyeballed): wrote a one-off check comparing all 25 `(w,
edit_rate)` cells' recall between the `exact` arm and `fst_levenshtein,
k=0` — zero mismatches. Combined with `tests/k0_equivalence.rs`'s
synthetic-data version of the same property, this invariant is now
checked both on real sweep output and on every `cargo test` run.

Full analysis, numbers, and the answer to "is the hypothesis supported"
are in FINDINGS.md — short version: yes, at `w ∈ {16, 24, 32}` (mean
recall improves from 0.815 to 0.922 at k=1 and 0.980 at k=2 in that
regime), no measurable effect at `w ∈ {8, 12}` (both arms already at
~0.99 recall, nothing for fuzzy matching to recover), candidate-set
inflation is modest and — reassuringly — *smallest* exactly where the
recall benefit is real (1.27-1.72x at w≥16 vs. 1.71-2.58x at w≤12 where
it doesn't help). The part that actually disqualifies arm B from being a
practical recommendation as shipped is query latency: k=1 averages ~5,500x
slower than the exact index, k=2 averages ~167,000x slower (mean p50 of
769ms, p99 up to 7.4 seconds for one query at w=32). That's a property of
the `fst` crate's Levenshtein automaton implementation specifically (its
own doc comment calls it a proof of concept), not a refutation of fuzzy
k-gram matching as an idea — see FINDINGS.md's closing section for what
would need to change for this to be a real proposal instead of a
promising-but-impractical experiment result.

Did not attempt the optional autonomy detours (winnowed k-grams,
per-token-kind substitution costs) — the two bugs above and the latency
investigation consumed the time that would have gone to them. Winnowing
would be the natural next step, since it attacks the `k=2` latency
problem at its root (fewer query grams → fewer automaton constructions)
rather than working around it with a smaller sample the way this run had
to (`N_K2=10` instead of the standard `N_STANDARD=40`).

Remaining before calling this done: clean up, final `cargo test` /
`clippy` / `fmt` pass, commit, push.

## 2026-08-22 — reproducibility check, done

Ran the "Done means" checklist for real rather than assuming it:

- `cargo fmt -- --check`, `cargo clippy --all-targets`: clean.
- `cargo test`: 30 passed (27 lib unit tests + 3 integration tests in
  `tests/k0_equivalence.rs`), 0 failed.
- Reproducibility: deleted `results/` entirely, ran `cargo run --release
  --bin sweep` from scratch a second time, diffed both CSVs against the
  first run byte-for-byte. `results/ground_truth.csv` came back
  **bit-for-bit identical** — the seeded `ChaCha8Rng` mutation harness is
  exactly deterministic, no surprises. `results/sweep.csv` differed *only*
  in the four wall-clock timing columns (`query_p50_us`, `query_p99_us`,
  `query_mean_us`, `index_build_ms` — e.g. w=8 exact build 0.422ms first
  run vs 0.444ms second run, both essentially instant, the difference is
  scheduler noise); every other column — `recall`, `candidates_mean`,
  `candidates_p50/p99`, `grams_per_query_mean`, `index_distinct_grams`,
  `index_payload_bytes` — matched exactly across all 100 rows. That's the
  right property: the parts of this experiment that are supposed to be
  deterministic (which fragments, which mutations, which candidates come
  back, recall) are; only the parts that are inherently a wall-clock
  measurement (and would be even in a "correct" implementation) vary.

Deleted the throwaway `src/bin/probe.rs` calibration tool once its numbers
had done their job informing `N_STANDARD`/`N_K2` and the cache decision —
it's not part of the deliverable and would just be one more thing to keep
in sync.

This is the state pushed to the branch. Done.
