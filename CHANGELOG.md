# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).
Style inspired by [ripgrep's CHANGELOG](https://github.com/BurntSushi/ripgrep/blob/master/CHANGELOG.md).

## [Unreleased]

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
