# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).
Style inspired by [ripgrep's CHANGELOG](https://github.com/BurntSushi/ripgrep/blob/master/CHANGELOG.md).

## [0.7.0] — 2026-08-03

### Added
- `Quamina::matcher_stats()`: states, bytes, fanouts, and max fanout for the built matcher (ported from Go's `GetMatcherStats`) (#152)
- `set_matcher_build_mode` / `matcher_build_mode` on `Quamina`, with the `MatcherBuildMode` enum (`BuiltForComfort`, `BuiltForSpeed`), trading `add_pattern` cost against `matches_for_event` speed for wildcard and regexp patterns (ported from Go) (#178)

### Changed
- Default to `BuiltForComfort`, keeping wildcard and regexp matchers as NFAs; the NFA→DFA conversion now runs only under `BuiltForSpeed`, matching Go upstream's default (#178)
- `matcher_stats()` measures the frozen matcher, so it reflects the build mode you chose (#178)
- Merge each added pattern by appending to the live automaton and compacting at freeze, instead of rebuilding the whole arena on every add (#162)
- Precompute epsilon closures incrementally and reuse the scratch buffers across adds (~20% faster adding many shellstyle patterns) (#173, #177)
- Store self-only epsilon closures as an implicit sentinel, saving a `StateId` per state in the common case (#173)
- Build regexp alternations by byte-merging branches instead of a Thompson epsilon hub, shrinking alternation automata (#154)
- Merge two patterns on one field a byte range at a time instead of all 256 bytes per state, ~3x faster to add many prefix or shellstyle patterns to one field (#190)
- Word boundary expansion rejects a pattern whose boundaries expand past 1024 alternatives instead of building an automaton for each one (#190)
- Synced Go upstream through 81f5b73 (#152–#183)

### Fixed
- Lookaround regexps now match against the value itself, not just their assertions; `foo(?!bar)baz` no longer matches unrelated values, and `(?=.*foo).*bar.*` matches `foobar` (#182–#184)
- `BuiltForSpeed` now holds its freeze-time DFA to the arena byte budget, and applies to lookaround regexps instead of leaving them as NFAs (#186, #188)
- `matcher_stats()` and `arena_stats()` count the lazy DFA cache, which no longer allocates a transition table for states it declines to cache (#185, #186)
- A rejected `add_pattern` no longer leaves partial build state behind; transition registrations, consumed singletons, and arena inserts all roll back (#163, #164)
- `delete_patterns` drops the pattern's stored definitions, so adding the same id again no longer brings the deleted ones back (#180)
- `rebuild` now rebuilds the field-segments index, reclaims the states of rejected patterns, and replays patterns in the order they were added (#178, #180, #181)
- Regexps no longer over-match when one unbounded quantifier nests inside another, e.g. `(.+c)*` or `(a*)*b` (#156)
- Long single-chain patterns no longer overflow the stack; the arena walks that build, fold, clone and merge a chain carry their own stack instead of recursing per state (#190)
- A `{0}` quantifier beside a word boundary (`xa{0}~b `) no longer panics `add_pattern`, and an atom matching zero characters hands the boundary to its neighbour, so `xa?~b ` matches `x ` (#190)

### Breaking
- Removed the runtime memory-budget API (`get_memory_budget`/`set_memory_budget`), matching Go upstream dropping it; the build-time `QuaminaBuilder::with_arena_byte_budget` cap remains (#152)

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
