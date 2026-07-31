# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).
Style inspired by [ripgrep's CHANGELOG](https://github.com/BurntSushi/ripgrep/blob/master/CHANGELOG.md).

## [Unreleased]

### Added
- Memory and automaton statistics via `Quamina::matcher_stats()`, reporting states, bytes, fanouts, and max fanout (ported from Go upstream's `GetMatcherStats`) (#152)
- `Quamina::set_matcher_build_mode`/`matcher_build_mode` with the `MatcherBuildMode` enum (`BuiltForComfort`, `BuiltForSpeed`), letting you trade `add_pattern` cost against `matches_for_event` speed for wildcard and regexp patterns (ported from Go upstream's Get/Set MatcherBuildMode) (#178)
- `matcher_stats()` now measures the materialized (frozen) matcher, so it reflects the build mode — `BuiltForSpeed` reports the converted DFAs — letting you watch the size effect of the mode you chose (#178)

### Changed
- Word boundary expansion now rejects a pattern whose boundaries expand past 1024 alternatives instead of building an automaton for every one of them (#188)
- Default to `BuiltForComfort`, keeping wildcard and regexp matchers as NFAs; the NFA→DFA conversion now runs only under `BuiltForSpeed`. This favors cheaper `add_pattern` over faster `matches_for_event`, matching Go upstream's default (#178)
- Store self-only epsilon closures as an implicit zero-length sentinel, avoiding one `StateId` in the flattened closure buffer for the common case (#173)
- Precompute epsilon closures incrementally, re-closing only the states an added pattern introduces instead of the whole automaton on every add (matching Go upstream's `closureForNfa` prune) (#173)
- Reuse the epsilon-closure scratch buffers across pattern adds instead of reallocating them per add, speeding up incremental builds of NFA-heavy pattern sets (~20% faster adding many shellstyle patterns) (matching the spirit of Go upstream's matcher-owned `closureBuffers`) (#177)
- Merge each added non-exact pattern by appending to the live automaton and compacting unreachable history at freeze, instead of rebuilding the whole accumulated arena on every add (#162)
- Build regexp alternations by byte-merging branches into a deterministic entry instead of a Thompson epsilon hub (matching Go upstream's `makeNFAFromBranches`), shrinking alternation automata and speeding up their construction (#154)
- Synced Go upstream through 5c6e2df (#152)

### Breaking
- Removed the runtime memory-budget API (`get_memory_budget`/`set_memory_budget`), matching Go upstream dropping it; the build-time `QuaminaBuilder::with_arena_byte_budget` cap remains (#152)

### Fixed
- A `{0}` quantifier beside a word boundary (`xa{0}~b `) no longer panics `add_pattern`, and an atom that can match zero characters now hands the boundary to its neighbour instead of assuming the value edge, so `xa?~b ` matches `x ` (#188)
- Long single-chain patterns no longer overflow the stack and abort the process — a 5,000-character `prefix` on a 2 MB thread used to die at freeze — because the arena walks that build, fold and clone a chain now carry their own stack instead of recursing once per state (#189)
- Lookaround regexps now match against the value itself, not just their assertions: `foo(?!bar)baz` no longer matches `totally-unrelated`, `(?=.*foo).*bar.*` matches `foobar`, and `(?<=foo)bar(?=baz)baz` matches `foobarbaz` (#182, #183, #184)
- `BuiltForSpeed` now holds its freeze-time DFA to the arena byte budget, keeping the NFA when there is no room, and applies to lookaround regexps, which it used to leave as NFAs (#186, #188)
- `matcher_stats()` and `arena_stats()` now count the lazy DFA cache a `BuiltForSpeed` pattern falls back to, and the cache no longer allocates a transition table for states it declines to cache (#185, #186)
- `Quamina::delete_patterns` now drops the pattern's stored definitions, so adding the same id again no longer brings the deleted ones back (matching Go upstream's `memState.Delete`) (#180)
- `Quamina::rebuild` now rebuilds the field-segments index, reclaims the states of patterns the automaton rejected part-way through, and replays live patterns in the order they were added rather than in hash order (#178, #180, #181)
- Regexps no longer over-match when one unbounded quantifier nests inside another (e.g. `(.+c)*`, `(a*)*b`): a quantifier's "match zero copies" skip could leak across an inner loop's back-edge and let the construct exit before its body matched (#156)

## [0.6.0] — 2026-06-11

### Added
- (Beta) Ability to adjust the memory cap at runtime (ported from Go PR #516) (#101)
- NFA→DFA subset construction with a lazy DFA fallback for patterns over budget, giving 2~3x improvement in some cases (`regexp_plus_long`: 1259 ns → 501 ns, `pathological_epsilon`: 6.1 µs → 2.0 µs) (#90)
- memchr acceleration for self-loop states like `[^x]+` for more performance. (#90)
- Mutation-testing gate on PRs, plus `just mutants-local` for full sweeps (#147, #148)

### Changed
- Removed `SmallTable.default`. The smaller state struct speeds up epsilon traversal (#128)
- Tightened strict clippy lints and added `# Errors` docs to public fallible APIs (#95–#99)
- Expanded mutation test coverage across all modules (#83–#145)
- Synced Go upstream through d951751
- Declared minimum supported Rust version: 1.88

### Fixed
- Range quantifiers (`{n,m}`) now reject counts above 100 at parse time; huge counts previously built multi-gigabyte arenas or panicked (#150)

### Breaking
- Pattern parsing rejects unknown JSON escapes like `\z`, matching Go upstream (#122)

## [0.5.0] — 2026-03-23

### Changed
- Upgrade to Rust edition 2024: `let` chains, stricter `unsafe_op_in_unsafe_fn`, implicit binding modes
- Rename `gen` variable to `generation` (reserved keyword in edition 2024)
- Synced Go upstream through PR #510 (853e760)

## [0.4.4] — 2026-03-18

### Fixed
- Numeric range patterns (`>`, `<`, `>=`, `<=`) no longer produce false positives on string values. Added `Q_NUMBER_PREFIX` (0x80) tag byte to Q-number encodings for namespace separation from raw ASCII bytes (#72)

### Added
- Expanded mutation test coverage across thread_safe, mutable_matcher, json, flatten_json, regexp/nfa, and regexp/parser modules (#71)

## [0.4.3] — 2026-03-14

### Changed
- Flatten NFA traversal hot path: replace per-state SmallVec epsilon closures with contiguous arena buffer, add 256-entry DFA lookup table, flatten field transition pointers (~40% faster `pathological_epsilon`: 9.8 µs → 6.0 µs) (#60)
- Generation-counter dedup for NFA traversal, preventing exponential blowup on nested quantifiers (ported from Go 3d6886a) (#59)
- Binary search for `exists:false` field lookup (#68)
- Skip `dfa_lookup` validation under Miri (~3 min CI saving) (#62)

### Added
- `ArenaStats` diagnostics: state counts, table sizes, epsilon/closure statistics, flattened buffer usage (#65)
- Upstream syncs: Go ed38658..8f78c5e (#65, #66, #67)
- Pathological correctness test (ported from Go 336e69c) (#61)
- Heavy-pattern stress test (ported from Go 3157c6d, 7b6eb7d) (#64)
- `cargo-mutants` config and mutation test coverage across automaton, regexp, numbits, segments_tree, flattener, and core modules (#52–#58)

## [0.4.2] — 2026-02-26

### Changed
- Fuse SegmentsTree `fields`/`nodes` into single map, eliminating redundant hash lookups in flattener hot path (~19% faster flattening, 3–9% end-to-end)
- Rewrite shellstyle/wildcard FA construction to reduce NFA state transitions by ~48% (#44)
- Eliminate `Arc::clone` from NFA/DFA traversal hot path
- Fast-path single-element epsilon closures in NFA traversal (#44)
- Encode spinout loopback in transition table, removing a per-byte branch from NFA traversal (#44)
- Avoid redundant `String` clone in `MatchSet::add` for duplicate entries (#43)

### Added
- Overlapping shellstyle nesting tests ported from Go upstream (5b74bd7, 7cb59fa)
- Miri-friendly variants for long-running NFA tests
- `justfile` for common development tasks (#30)

## [0.4.1] — 2026-02-20

### Fixed
- Singleton short-circuit skipping lookaround matching (#40)

### Changed
- Replace `Vec` with custom `Transitions` enum in `transition_on()` for fewer allocations (#37)
- Only mark regexp FAs as NFA when actually nondeterministic (#33)

### Added
- Doctests and improved API docs for rebuild/pruner methods (#35)
- `deny(missing_docs)` lint and error variant documentation (#36)
- CodeQL workflow, synced upstream to dd6b17c (#34)
- Upstream sync tracking via `.go-upstream-sync` and `just upstream` / `just upstream-sync` (#31)

## [0.4.0] — 2026-02-12

### Added
- Word boundary assertions (`~b` / `~B`) for regexp patterns
- Character class subtraction syntax for regexp patterns

### Fixed
- Vec capacity bug in NFA traversal hot loop

### Changed
- Reversed DFA trie for suffix matching (110x speedup)

## [0.3.0] — 2026-02-10

### Added
- Pattern complexity limits (`max_states_per_pattern`) to prevent OOM from adversarial input
- Lifecycle fuzz target for continuous fuzzing
- Fast DFA traversal path for pure DFA patterns
- FA shell caching for Unicode property and XML name char patterns

### Changed
- Precompute epsilon closures at build time (ported from Go PR #482)
- Flatten epsilon targets during arena NFA merges
- SmallVec and SparseSet memory layout optimizations
- Replace HashSet with FxHashSet for faster hashing
- Close early-field-match performance gap (420ns to 275ns, 35% faster)

### Fixed
- Arena budget bypass in pattern limits
- String/number type distinction in value matching

## [0.2.0] — 2026-02-03

### Changed
- Arena-based NFA/DFA allocation for all pattern types
- Full arena FA migration replacing boxed allocations

## [0.1.0] — 2026-01-28

### Added
- Core matching operators: exact, prefix, suffix, wildcard, shellstyle, anything-but, equals-ignore-case, numeric ranges, CIDR, regexp
- Custom NFA/DFA engine with Thompson construction
- JSON flattener for nested object/array events
- Thread-safe API with `Clone` support for snapshots
- `has_matches` / `count_matches` convenience methods
- WASM playground
- Benchmarks with criterion
