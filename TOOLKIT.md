# Quality Assurance Toolkit

This document tracks the implementation of quality tools for quamina-rs. 
The tools listed are probably the right ones but we haven't gone enough in depth to be certain.
Keep under 300 lines, refactoring / rephrasing / reorganizing as needed when you learn more. Update as work progresses. 

## Workflow Principles

- **Push often, check CI** - Verify changes work in clean environment
- **Use todos** - Manage context window, track progress across sessions
- **Read code directly** - Don't trust past interpretations
- **Refactor as appropriate** - Mirror external code when needed
- **Algorithmic parity matters** - Both CPU and memory; ask the human for input when tradeoffs need to be made.

Reference: [Ralph Wiggum Loop](https://linearb.io/dev-interrupted/podcast/inventing-the-ralph-wiggum-loop)

---

## Current Unsafe Code Locations

| File | Line | Usage | Risk |
|------|------|-------|------|
| `src/flatten_json.rs` | 51 | `from_utf8_unchecked` | Low |
| `src/flatten_json.rs` | 180,220 | `transmute` for lifetime | High |
| `src/automaton/thread_safe.rs` | 72-73,126-127,284 | Manual `Send/Sync` | High |
| `src/automaton/small_table.rs` | 23-24 | Manual `Send/Sync` for raw ptr | High |

---

## Tool Implementation Plan

### Phase 1: Miri Integration
**Status:** COMPLETE
**Priority:** Critical

Miri detects undefined behavior in unsafe Rust code. Gold standard for UB detection.

**Tasks:**
- [x] Add Miri to CI workflow (nightly toolchain required)
- [x] Run `cargo +nightly miri test` locally first
- [x] Fix any UB detected (especially transmute and Send/Sync impls) - None found
- [x] Add Miri job to `.github/workflows/test.yml`

**Note:** CI now runs full Miri test suite. Slow tests (negated classes, Unicode categories, threading, large pattern counts) are skipped via `#[cfg_attr(miri, ignore)]`.

---

### Phase 1b: Miri Full Coverage
**Status:** COMPLETE
**Priority:** Critical

Full Miri coverage with explicit rationale for each skipped test.

**Key Insight:** Quamina uses I-Regexp (RFC 9485). Unicode category patterns like `~p{L}` are valid I-Regexp but create automata covering ~130K codepoints, which is slow under Miri interpretation.

**Miri-Friendly Tests Added:**
| Test | Exercises | Replaces |
|------|-----------|----------|
| `test_arena_nfa_star_plus_miri_friendly` | Arena NFA cycles | `test_negated_category_star_edge_cases` |
| `test_nfa_positive_class_miri_friendly` | SmallTable NFA | `test_negated_class_nfa` |
| `test_cidr_miri_friendly` | CIDR matching | `test_cidr_ipv4_various_prefixes` |
| `test_concurrent_miri_friendly` | Thread safety | `test_arc_concurrent_read_write` |
| `test_memory_cleanup_miri_friendly` | Add/delete/rebuild | `test_arc_memory_cleanup` |

**Skipped Tests with Rationale (in code comments):**
- Pattern-size tests: Cannot be broken down - automaton size IS the issue
- File I/O test: Miri isolation blocks filesystem access
- Full concurrency/memory tests: Miri-friendly versions cover same code paths

**References:**
- [RFC 9485 I-Regexp](https://datatracker.ietf.org/doc/rfc9485/)
- [Miri GitHub](https://github.com/rust-lang/miri)

---

### Phase 2: Performance & Memory Benchmarks
**Status:** COMPLETE
**Priority:** High (before fuzzing to establish baselines)

**Tasks:**
- [x] Audit existing benchmarks in `benches/matching.rs` - Comprehensive (40+ benchmarks)
- [x] Add memory profiling with `dhat` - See `benches/memory.rs`
- [x] Add benchmark compilation check to CI
- [ ] ~Document baseline performance metrics~ - Manual, compare with Go as needed

**Existing Benchmark Coverage (`benches/matching.rs`):**
- Basic matching: exact, multi-pattern, nested, regex, no-match, early-exit
- Pattern types: shellstyle, prefix, anything-but, numeric ranges
- Regexp quantifiers: +/*, short/long strings, dot-star
- Arena NFA traversal, bulk pattern add (O(n²) testing)
- Go parity: status.json flatten/match, citylots (206k features)

**Memory Profiling (`benches/memory.rs`):**
```bash
cargo bench --bench memory --features dhat-heap        # Human-readable
cargo bench --bench memory --features dhat-heap -- --json  # JSON for diffing
```

Profiles: pattern add (simple, multivalue, regex, numeric), steady-state (1000 patterns), matching hot path, large JSON events, Go parity (citylots, number matching, shellstyle).

**Key Memory Observations:**
- Steady-state: ~2.6KB per pattern (1000 simple patterns = 2.6MB peak)
- Matching hot path: Near-zero allocations (0-4 allocs per match)
- No-match case: 0 allocations

**References:**
- [Criterion.rs Book](https://bheisler.github.io/criterion.rs/book/)
- [dhat crate](https://crates.io/crates/dhat)

---

### Phase 3: Fuzzing with cargo-fuzz
**Status:** COMPLETE
**Priority:** High

Coverage-guided fuzzing for JSON parsing and pattern matching.

**Tasks:**
- [x] Install cargo-fuzz: `cargo install cargo-fuzz`
- [x] Create fuzz targets directory: `fuzz/`
- [x] Add fuzz target for `flatten_json` module (JSON parsing)
- [x] Add fuzz target for `add_pattern` (pattern parsing)
- [x] Add fuzz target for `match_event` (full integration)
- [x] Add fuzzing to CI (30s smoke test per target)

**Fuzz Targets:**
| Target | Attack Surface | Input |
|--------|---------------|-------|
| `fuzz_flatten_json` | JSON parser | Arbitrary bytes |
| `fuzz_add_pattern` | Pattern parser | UTF-8 strings |
| `fuzz_match_event` | Full pipeline | Arbitrary bytes vs 17 pre-loaded patterns |

**Usage:**
```bash
cargo +nightly fuzz run fuzz_flatten_json           # Run indefinitely
cargo +nightly fuzz run fuzz_flatten_json -- -max_total_time=60  # 60s run
cargo +nightly fuzz list                            # List all targets
```

**References:**
- [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz)
- [Rust Fuzz Book](https://rust-fuzz.github.io/book/)

---

### Phase 4: Kani Model Checking
**Status:** COMPLETE
**Priority:** Medium

Bounded model checking for verifiable unsafe code properties.

**Tasks:**
- [x] Install Kani: `cargo install --locked kani-verifier && cargo kani setup`
- [x] Add proof harnesses in `src/kani_proofs.rs`
- [x] Add Kani job to CI (15 min timeout)

**Proofs Implemented (8 total):**
| Proof | Verifies |
|-------|----------|
| `byte_ceiling_utf8_valid` | BYTE_CEILING constant is 0xF6 |
| `smalltable_step_no_panic` | step() handles all valid bytes |
| `smalltable_dstep_no_panic` | dstep() handles all valid bytes |
| `stateptr_equality_reflexive` | StatePtr equality is reflexive |
| `json_string_byte_validity` | JSON string bytes satisfy UTF-8 invariants |
| `json_field_name_ascii_utf8_valid` | ASCII field names are valid UTF-8 |
| `stateid_none_is_none` | StateId::NONE is recognized |
| `stateid_none_index_max` | StateId::NONE.index() is u32::MAX |

**Tool Selection Rationale:**
- Send/Sync impls: Runtime property - verified by Miri threading tests
- Transmute lifetime: Encapsulation-based safety - documented, not formally verifiable
- `from_utf8_unchecked`: Bounded proof for ASCII subset, fuzzing for full coverage

**References:**
- [Kani GitHub](https://github.com/model-checking/kani)
- [Kani Book](https://model-checking.github.io/kani/)

---

### Phase 4.5: Quality Coverage Tracking
**Status:** SKIPPED
**Priority:** Medium

**Decision:** No automated tooling exists to verify unsafe code coverage across Miri/Fuzz/Kani. Miri and Kani don't support coverage instrumentation; fuzz coverage is non-deterministic. Manual code review with the coverage matrix below remains the practical approach.

**Coverage Matrix (Reference):**
| Unsafe Code | Miri | Fuzz | Kani | Notes |
|-------------|------|------|------|-------|
| `from_utf8_unchecked` | ✓ | ✓ | ✓ (ASCII) | Full path via Miri tests |
| `transmute` lifetime | ✓ | ✓ | ✗ | Encapsulation-based, documented |
| `Send/Sync` impls | ✓ | - | ✗ | Miri threading tests |
| `StatePtr` raw ptr | ✓ | - | ✓ | Equality/hash verified |

---

### Phase 5: cargo-audit
**Status:** NOT STARTED
**Priority:** High

Scan dependencies for known security vulnerabilities.

**Tasks:**
- [ ] Install: `cargo install cargo-audit`
- [ ] Run locally: `cargo audit`
- [ ] Add to CI workflow
- [ ] Set up Dependabot or RenovateBot for automatic updates

**CI Addition:**
```yaml
security:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - uses: rustsec/audit-check@v2
      with:
        token: ${{ secrets.GITHUB_TOKEN }}
```

**References:**
- [cargo-audit](https://crates.io/crates/cargo-audit)
- [RustSec Advisory DB](https://rustsec.org/)

---

### Phase 6: cargo-deny
**Status:** NOT STARTED
**Priority:** High

Comprehensive dependency linting: licenses, duplicates, advisories.

**Tasks:**
- [ ] Install: `cargo install cargo-deny`
- [ ] Initialize config: `cargo deny init`
- [ ] Configure `deny.toml` for license requirements
- [ ] Add to CI workflow

**CI Addition:**
```yaml
- name: Check dependencies
  uses: EmbarkStudios/cargo-deny-action@v2
```

**References:**
- [cargo-deny GitHub](https://github.com/EmbarkStudios/cargo-deny)
- [cargo-deny book](https://embarkstudios.github.io/cargo-deny/)

---

## Additional Clippy Lints

Add to `Cargo.toml`:
```toml
[lints.rust]
unsafe_code = "warn"

[lints.clippy]
undocumented_unsafe_blocks = "warn"
transmute_ptr_to_ptr = "warn"
```

---

## Progress Log

| Date | Phase | Action | Result |
|------|-------|--------|--------|
| 2026-01-25 | 1 | Miri integration | Complete - CI runs Miri on unsafe modules |
| 2026-01-25 | 1b | Miri full coverage | Complete - 5 Miri-friendly tests added, rationale documented in code |
| 2026-01-27 | 2 | Memory profiling | Complete - dhat benchmarks added, CI checks bench compilation |
| 2026-01-28 | 3 | Fuzzing | Complete - 3 fuzz targets, CI runs 30s smoke tests |
| 2026-01-28 | 4 | Kani proofs | Complete - 8 proofs in `src/kani_proofs.rs`, CI runs all harnesses |
| 2026-01-28 | 4.5 | Coverage tracking | Skipped - No automated tooling exists for cross-tool coverage |

---

## Quick Commands

```bash
# Miri
cargo +nightly miri test

# Benchmarks
cargo bench --bench matching           # Performance benchmarks
cargo bench --bench memory --features dhat-heap  # Memory profiling

# Fuzzing
cargo +nightly fuzz run fuzz_flatten_json

# Kani
cargo kani

# Security
cargo audit
cargo deny check
```

---

*Last updated: 2026-01-28*
*Line count target: <300 (currently ~250)*
