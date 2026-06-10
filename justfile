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
    cargo clippy --all-targets --all-features -- -D warnings
    cargo fmt -- --check

# Run matching benchmarks (optional filter)
[group('bench')]
bench *filter:
    cargo bench {{filter}}

# Compare against a saved baseline
[group('bench')]
bench-compare name:
    cargo bench -- --baseline {{name}}

# Run dhat heap profiling benchmark
[group('bench')]
bench-memory:
    cargo bench --features dhat-heap memory

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

# Check upstream sync (fails if behind)
[group('dev')]
[no-exit-message]
upstream:
    ./scripts/check-upstream.sh

# Update .go-upstream-sync after porting
[group('dev')]
upstream-sync sha:
    echo "{{sha}}" > .go-upstream-sync
    ./scripts/check-upstream.sh

# Run mutation testing on the full codebase
[group('validate')]
mutants *args:
    cargo mutants -vV --in-place {{args}}

# Run mutation testing only on code changed vs main
[group('validate')]
mutants-diff *args:
    #!/usr/bin/env bash
    set -euo pipefail
    git diff origin/main.. > /tmp/quamina-mutants.diff
    cargo mutants -vV --in-diff /tmp/quamina-mutants.diff --in-place {{args}}

# Mutation gate for one file (or `regex` for part of it): 0 missed, 0 timeout
[group('validate')]
mutants-verify file regex='':
    ./scripts/mutants-verify.sh {{file}} {{regex}}

# caffeinate keeps the laptop awake; the watchdog kills cargo-mutants before the
# OS reboots, and a pressure kill reports INCOMPLETE, never a pass. nextest +
# lib-only come from .cargo/mutants.toml; scope with args like `--file src/...`.
# Full-tree local mutation sweep with the macOS memory-pressure watchdog
[group('validate')]
mutants-local *args:
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{justfile_directory()}}"
    OOM_FLAG="$(pwd)/target/mutants-local.oom"
    export OOM_FLAG
    rm -f "$OOM_FLAG"
    source scripts/mutants-watchdog.sh
    caffeinate=()
    command -v caffeinate >/dev/null 2>&1 && caffeinate=(caffeinate -i)
    start_watchdog
    trap stop_watchdog EXIT
    rc=0
    "${caffeinate[@]}" cargo mutants -vV --iterate --baseline skip \
        -j 4 --timeout 10 {{args}} || rc=$?
    stop_watchdog
    trap - EXIT
    if [[ -f "$OOM_FLAG" ]]; then
        echo "INCOMPLETE: run killed under memory pressure — result is NOT trustworthy" >&2
        exit 1
    fi
    exit "$rc"

# Start LLM Agent in this repo (currently claude-code with dangerously-skip-permissions)
[group('agent')]
ai:
    cd {{justfile_directory()}} && claude --dangerously-skip-permissions
