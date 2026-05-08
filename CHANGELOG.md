# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).
Style inspired by [ripgrep's CHANGELOG](https://github.com/BurntSushi/ripgrep/blob/master/CHANGELOG.md).

## [Unreleased]

### Added
- NFA→DFA subset construction at freeze time: regexp patterns with epsilon transitions are eagerly converted to DFA (budget: 8× NFA states, max 10k), with a lazy DFA fallback for patterns exceeding the eager budget (`regexp_plus_long`: 1259→501 ns, `regexp_star_long`: 1125→459 ns, `pathological_epsilon`: 6.08→2.00 µs)
- DFA acceleration: `compute_dfa_accel` detects self-loop states and attaches memchr skip info for SIMD byte skipping on patterns like `[^x]+`
- Profiling examples for NFA→DFA budget tuning and negated char class acceleration
- Allocation-free integration test for `traverse_arena_nfa` (counting global allocator, gated off Miri), expanded `dstep` hot-path documentation, and forbidden UTF-8 byte coverage tests (#100)
- `Quamina::get_memory_budget` and `Quamina::set_memory_budget` for inspecting and adjusting the arena byte budget at runtime; budget is shared across all matchers in a tree via `Arc<AtomicUsize>`, and current usage is computed by deduplicating the matcher DAG so shared sub-graphs are counted once. A budget of 0 disables the check (matches upstream Go's "0 = unlimited" convention). Ports upstream PR #516.

### Changed
- Tightened strict clippy lints: removed blanket allows for `module_name_repetitions`, `too_many_lines`, `similar_names`, and related structural/naming lints; refactored hot spots, kept per-item allows on hot loops and generated tables
- Added `# Errors` sections to public fallible APIs and enabled `missing_errors_doc`
- Removed unused `SmallTable::step` wrapper; `dstep` is the only stepping entry point
- `Clone`, `rebuild`, and `clear` now carry over the live memory budget instead of resetting to the builder's initial value, matching upstream `coreMatcher` semantics
- `MutableValueMatcher::new()` now defaults to a 0 budget (unlimited) instead of `usize::MAX`, aligning standalone matchers with the new "0 = unlimited" convention; behaviour is unchanged in practice
- Synced Go upstream through commit 53515a0

### Fixed
- `[^x]+` patterns (17K-state Unicode NFA) exceeded the DFA budget and regressed; SIMD acceleration via `AccelInfo::try_accelerate` restores performance (`regexp_negated_1k`: 3.2 µs → 652 ns)

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
