# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).
Style inspired by [ripgrep's CHANGELOG](https://github.com/BurntSushi/ripgrep/blob/master/CHANGELOG.md).

## [Unreleased]

### Added
- NFA→DFA subset construction at freeze time: regexp patterns with epsilon transitions are now eagerly converted to DFA when within a state budget (8x NFA states, max 10,000), yielding up to 2.5x faster matching on long regexp inputs (`regexp_plus_long`: 1259→501 ns, `regexp_star_long`: 1125→459 ns)
- Lazy DFA cache (tier 2): NFA arenas that exceed the eager DFA budget now use on-demand DFA state caching during matching, building states lazily and reusing them across traversals. Budget-limited to prevent memory explosion (10x eager budget, max 100,000 states)
- Three-tier matching strategy inspired by Go quamina issue #481: eager DFA → lazy DFA → NFA fallback
- DFA acceleration: `compute_dfa_accel` reconstructs memchr skip info on eager DFA states after subset construction; `try_compute_accel` lazily detects acceleration on lazy DFA self-loop states during traversal
- Profiling example `examples/profile_negated.rs` for negated char class acceleration analysis
- Kani proof harness verifying `nfa_to_dfa` respects the state budget
- 17 new unit tests covering eager and lazy DFA conversion, budget enforcement, field transition preservation, NFA/DFA matching equivalence, and DFA acceleration

### Fixed
- Negated character class regression: `[^x]+` patterns (17K NFA states from Unicode support) exceeded eager DFA budget, falling back to lazy DFA which lacked memchr acceleration. Added SIMD-accelerated byte skipping to both eager and lazy DFA traversal paths (`regexp_negated_1k`: 3.2 µs → 652 ns)

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
