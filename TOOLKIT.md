# Quality Assurance Toolkit

This document tracks the implementation of quality tools for quamina-rs.
Keep under 300 lines. Update as work progresses.

## Workflow Principles

- **Push often, check CI** - Verify changes work in clean environment
- **Use todos** - Manage context window, track progress across sessions
- **Read code directly** - Don't trust past interpretations
- **Refactor as appropriate** - Mirror external code when needed
- **Algorithmic parity matters** - Both CPU and memory; ask on tradeoffs

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
**Status:** NOT STARTED
**Priority:** Critical

Miri detects undefined behavior in unsafe Rust code. Gold standard for UB detection.

**Tasks:**
- [ ] Add Miri to CI workflow (nightly toolchain required)
- [ ] Run `cargo +nightly miri test` locally first
- [ ] Fix any UB detected (especially transmute and Send/Sync impls)
- [ ] Add Miri job to `.github/workflows/test.yml`

**CI Addition:**
```yaml
miri:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - uses: dtolnay/rust-toolchain@nightly
      with:
        components: miri
    - name: Run Miri
      run: cargo +nightly miri test
```

**References:**
- [Miri GitHub](https://github.com/rust-lang/miri)
- [Miri POPL 2026 Paper](https://dl.acm.org/doi/10.1145/3776690)

---

### Phase 2: Performance & Memory Benchmarks
**Status:** NOT STARTED
**Priority:** High (before fuzzing to establish baselines)

**Tasks:**
- [ ] Audit existing benchmarks in `benches/matching.rs`
- [ ] Add memory profiling with `dhat` or `memory-stats`
- [ ] Add benchmark regression detection to CI
- [ ] Document baseline performance metrics

**Tools to consider:**
- `criterion` (already in dev-deps) - statistical benchmarking
- `dhat` - heap profiling
- `cargo-benchcmp` - compare benchmark results
- `bencher` or `codspeed` - CI benchmark tracking

**CI Addition:**
```yaml
benchmarks:
  runs-on: ubuntu-latest
  steps:
    - uses: actions/checkout@v4
    - uses: dtolnay/rust-toolchain@stable
    - name: Run benchmarks
      run: cargo bench --no-run  # At minimum, ensure they compile
```

**References:**
- [Criterion.rs Book](https://bheisler.github.io/criterion.rs/book/)
- [dhat crate](https://crates.io/crates/dhat)

---

### Phase 3: Fuzzing with rust-fuzz
**Status:** NOT STARTED
**Priority:** High

Fuzz JSON parsing boundaries - classic attack surface.

**Tasks:**
- [ ] Install cargo-fuzz: `cargo install cargo-fuzz`
- [ ] Create fuzz targets directory: `fuzz/`
- [ ] Add fuzz target for `flatten_json` module
- [ ] Add fuzz target for pattern matching
- [ ] Run initial fuzzing campaign locally
- [ ] Add fuzzing to CI (optional - can be expensive)

**Fuzz Target Structure:**
```
fuzz/
├── Cargo.toml
└── fuzz_targets/
    ├── fuzz_flatten_json.rs
    └── fuzz_pattern_match.rs
```

**References:**
- [cargo-fuzz](https://github.com/rust-fuzz/cargo-fuzz)
- [Rust Fuzz Book](https://rust-fuzz.github.io/book/)
- [afl.rs](https://github.com/rust-fuzz/afl.rs) (alternative)

---

### Phase 4: Kani Model Checking
**Status:** NOT STARTED
**Priority:** Medium

Formal verification for critical unsafe code paths.

**Tasks:**
- [ ] Install Kani: `cargo install --locked kani-verifier && cargo kani setup`
- [ ] Add proof harnesses for Send/Sync implementations
- [ ] Add proof harnesses for transmute safety
- [ ] Integrate into CI (can be slow)

**Example Proof Harness:**
```rust
#[cfg(kani)]
#[kani::proof]
fn check_state_ptr_safety() {
    // Verify StatePtr invariants
}
```

**References:**
- [Kani GitHub](https://github.com/model-checking/kani)
- [Kani Book](https://model-checking.github.io/kani/)

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
| TBD | 1 | Miri integration | - |

---

## Quick Commands

```bash
# Miri
cargo +nightly miri test

# Fuzzing
cargo +nightly fuzz run fuzz_flatten_json

# Kani
cargo kani

# Security
cargo audit
cargo deny check
```

---

*Last updated: 2026-01-25*
*Line count target: <300 (currently ~200)*
