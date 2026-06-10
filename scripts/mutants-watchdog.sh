#!/usr/bin/env bash
# Shared memory-pressure watchdog for the local cargo-mutants runs (issue #41).
#
# A full-tree mutation sweep builds hundreds of mutated trees in parallel; on a
# memory-constrained laptop that can exhaust RAM and force a reboot. This kills
# cargo-mutants first, and records the kill so a run the OS cut short is never
# reported as a clean PASS.
#
# Source it, point OOM_FLAG at a sentinel path, then bracket the run:
#
#   OOM_FLAG=/path/to/.oom-killed
#   source scripts/mutants-watchdog.sh
#   start_watchdog
#   trap stop_watchdog EXIT
#   cargo mutants ...
#   stop_watchdog
#   [[ -f "$OOM_FLAG" ]] && { echo INCOMPLETE; exit 1; }
#
# macOS only: it reads the kernel memory-pressure level and free-memory
# percentage and SIGKILLs cargo-mutants once either crosses the danger line.
# Linux has neither sysctl key nor memory_pressure(1), so start_watchdog is a
# no-op there and the run proceeds without a killer (CI/Linux is unaffected).

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
