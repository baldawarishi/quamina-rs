#!/usr/bin/env bash
# Execution-grounded mutation gate for one file (or part of one file).
#
#   ./scripts/mutants-verify.sh <src/path.rs> [mutant-name-regex]
#
# Examples:
#   ./scripts/mutants-verify.sh src/regexp/parser.rs
#   ./scripts/mutants-verify.sh src/regexp/parser.rs 'read_atom|read_group'
#
# Passes (exit 0) only when the lib tests are green AND the scoped file has
# zero MISSED and zero TIMEOUT mutants. A timeout is treated as bad as a
# missed mutant: this is a high-performance library, and a loop mutation that
# hangs or runs pathologically is a real hole, not a free pass.
#
# Isolated by design: results go to target/mutants-verify/ and never touch
# mutants.out/ or any mutants.out.backup.* directory. Trustworthy by design:
# a watchdog kill is reported as INCOMPLETE, never as a false PASS.
#
# Requires: cargo-mutants, cargo-nextest. macOS for the memory watchdog
# (no-op elsewhere). Honors .cargo/mutants.toml.
#
# Env overrides:
#   MUTANTS_TIMEOUT   per-mutant test timeout, seconds (default 10)
#   MUTANTS_JOBS      parallel jobs (default 4)
#   SKIP_BASELINE     set to 1 to skip the up-front `cargo test --lib`
#
# Exit codes: 0 = PASS, 1 = FAIL (gaps or incomplete), 2 = usage/setup error.

set -euo pipefail

REPO_ROOT="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
cd "$REPO_ROOT"

FILE="${1:-}"
REGEX="${2:-}"
if [[ -z "$FILE" ]]; then
    echo "usage: $0 <src/path.rs> [mutant-name-regex]" >&2
    exit 2
fi
if [[ ! -f "$FILE" ]]; then
    echo "error: no such file: $FILE" >&2
    exit 2
fi

TIMEOUT="${MUTANTS_TIMEOUT:-10}"
JOBS="${MUTANTS_JOBS:-4}"
OUTDIR="$REPO_ROOT/target/mutants-verify"
OOM_FLAG="$OUTDIR/.oom-killed"

# Clean only our own isolated dir so the scoped run's txt files are authoritative.
rm -rf "$OUTDIR"
mkdir -p "$OUTDIR"

# --- up-front green-tree check (--baseline skip trusts the caller) ----------
if [[ "${SKIP_BASELINE:-0}" != "1" ]]; then
    echo "==> baseline: cargo test --lib"
    if ! cargo test --lib --quiet; then
        echo "FAIL: baseline lib tests are red — fix tests before the mutation gate" >&2
        exit 1
    fi
fi

# --- memory-pressure watchdog (macOS only) ---------------------------------
# Mirrors the failsafe from issue #41: kill the run before the OS reboots the
# laptop, and leave a sentinel so a kill is never mistaken for a clean PASS.
WATCHDOG_PID=""
start_watchdog() {
    [[ "$(uname -s)" == "Darwin" ]] || return 0
    (
        while pgrep -qf cargo-mutants; do
            lvl=$(sysctl -n kern.memorystatus_vm_pressure_level 2>/dev/null || echo 0)
            free=$(memory_pressure 2>/dev/null \
                | awk -F': ' '/free percentage/{gsub(/%/,"",$2);print $2}')
            if [[ "$lvl" -ge 4 ]] || { [[ -n "$free" ]] && [[ "$free" -lt 6 ]]; }; then
                echo "$(date): pressure=$lvl free=${free}% — killing cargo-mutants" >&2
                : > "$OOM_FLAG"
                pkill -9 -f cargo-mutants
                break
            fi
            sleep 15
        done
    ) &
    WATCHDOG_PID=$!
}
stop_watchdog() {
    [[ -n "$WATCHDOG_PID" ]] && kill "$WATCHDOG_PID" 2>/dev/null || true
}
trap stop_watchdog EXIT
start_watchdog

# --- scoped mutation run ---------------------------------------------------
CAFFEINATE=()
command -v caffeinate >/dev/null 2>&1 && CAFFEINATE=(caffeinate -i)

ARGS=(mutants -vV --baseline skip -j "$JOBS" --timeout "$TIMEOUT"
      --test-tool nextest --file "$FILE" --output "$OUTDIR")
[[ -n "$REGEX" ]] && ARGS+=(--re "$REGEX")

echo "==> cargo ${ARGS[*]}"
MUT_RC=0
"${CAFFEINATE[@]}" cargo "${ARGS[@]}" || MUT_RC=$?

stop_watchdog
trap - EXIT

# --- adjudicate ------------------------------------------------------------
if [[ -f "$OOM_FLAG" ]]; then
    echo "INCOMPLETE: run was killed under memory pressure — result is NOT trustworthy" >&2
    echo "RESULT: FAIL ($FILE) — incomplete (memory pressure)"
    exit 1
fi

RESULTS="$OUTDIR/mutants.out"
count() { [[ -s "$RESULTS/$1" ]] && grep -c . "$RESULTS/$1" || echo 0; }

if [[ ! -d "$RESULTS" ]]; then
    echo "INCOMPLETE: no results at $RESULTS (cargo-mutants rc=$MUT_RC)" >&2
    echo "RESULT: FAIL ($FILE) — no results"
    exit 1
fi

MISSED=$(count missed.txt)
TIMEOUT_N=$(count timeout.txt)
CAUGHT=$(count caught.txt)
UNVIABLE=$(count unviable.txt)
SCOPE="$FILE${REGEX:+ /$REGEX/}"

echo
echo "RESULT: $SCOPE — caught $CAUGHT, unviable $UNVIABLE, missed $MISSED, timeout $TIMEOUT_N"
if [[ "$MISSED" -ne 0 ]]; then
    echo "--- missed ---"; cat "$RESULTS/missed.txt"
fi
if [[ "$TIMEOUT_N" -ne 0 ]]; then
    echo "--- timeout (counts as missed for this gate) ---"; cat "$RESULTS/timeout.txt"
fi

if [[ "$MISSED" -eq 0 && "$TIMEOUT_N" -eq 0 ]]; then
    echo "GATE PASS: $SCOPE — missed 0, timeout 0"
    exit 0
fi
echo "GATE FAIL: $SCOPE — $MISSED missed, $TIMEOUT_N timeout" >&2
exit 1
