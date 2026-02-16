# quamina-rs task runner
# Run `just` to see all available recipes.

# Default: list recipes
default:
    @just --list

# Run fast tests (skips slow suites like citylots)
[group('dev')]
test *args:
    cargo test {{args}}

# Run the full test suite including ignored/slow tests
[group('dev')]
test-all:
    cargo test -- --include-ignored

# Run a single test by name with output
[group('dev')]
test-one name:
    cargo test {{name}} -- --nocapture

# Format code
[group('dev')]
fmt:
    cargo fmt

# Run cargo check + clippy + fmt check
[group('dev')]
check:
    cargo check
    cargo clippy -- -D warnings
    cargo fmt -- --check

# Run matching benchmarks (optional filter)
[group('bench')]
bench *filter:
    cargo bench {{filter}}

# Run benchmarks in quick mode (fewer iterations)
[group('bench')]
bench-quick *filter:
    cargo bench {{filter}} -- --quick

# Save a benchmark baseline
[group('bench')]
bench-save name:
    cargo bench -- --save-baseline {{name}}

# Compare against a saved baseline
[group('bench')]
bench-compare name:
    cargo bench -- --baseline {{name}}

# Run dhat heap profiling benchmark
[group('bench')]
bench-memory:
    cargo test --features dhat-heap --release -- --ignored dhat

# Build WASM and start the playground dev server
[group('playground')]
playground: playground-build
    cd playground && python3 serve.py

# Build WASM module only
[group('playground')]
playground-build:
    cd playground && bash build.sh

# Run full validation: miri + fuzz + kani
[group('validate')]
validate duration="600":
    #!/usr/bin/env bash
    set -euo pipefail
    echo "=== MIRI ==="
    cargo +nightly miri test -- --test-threads=1
    echo "=== FUZZ ==="
    for target in fuzz_add_pattern fuzz_match_event fuzz_flatten_json fuzz_lifecycle; do
      echo "--- $target ({{duration}}s) ---"
      cargo +nightly fuzz run "$target" -- -max_total_time={{duration}}
    done
    echo "=== KANI ==="
    cargo kani --output-format terse
    echo "=== DONE ==="

# Run Miri tests only
[group('validate')]
miri:
    cargo +nightly miri test -- --test-threads=1

# Run fuzz targets (configurable duration in seconds)
[group('validate')]
fuzz duration="600":
    #!/usr/bin/env bash
    set -euo pipefail
    for target in fuzz_add_pattern fuzz_match_event fuzz_flatten_json fuzz_lifecycle; do
      echo "--- $target ({{duration}}s) ---"
      cargo +nightly fuzz run "$target" -- -max_total_time={{duration}}
    done

# Run Kani proofs only
[group('validate')]
kani:
    cargo kani --output-format terse

# Dry-run cargo publish
[group('release')]
publish-check:
    cargo publish --dry-run

# Publish to crates.io (requires confirmation)
[group('release')]
publish:
    @echo "About to publish quamina to crates.io."
    @echo "Press Enter to continue or Ctrl-C to abort..."
    @read _
    cargo publish

# Run the smoke example
[group('release')]
smoke:
    cargo run --example smoke

# Start LLM Agent in this repo (currently claude-code with dangerously-skip-permissions)
[group('agent')]
ai:
    cd {{justfile_directory()}} && claude --dangerously-skip-permissions
