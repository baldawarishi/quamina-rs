#!/usr/bin/env bash
# Gate: fail if timbray/quamina has commits we haven't synced.
#
#   ./scripts/check-upstream.sh
#
# Requires: gh CLI, .go-upstream-sync file with last-synced Go commit SHA.
# Must be run from the main branch (skipped in CI).

set -euo pipefail

REPO_ROOT="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
LAST_SYNC="$(tr -d '[:space:]' < "$REPO_ROOT/.go-upstream-sync")"

# --- Must be on main (skip in CI where checkout is detached) ---
if [[ -z "${CI:-}" ]]; then
    CURRENT_BRANCH="$(git -C "$REPO_ROOT" rev-parse --abbrev-ref HEAD)"
    if [[ "$CURRENT_BRANCH" != "main" ]]; then
        echo "Error: must be on main branch (currently on $CURRENT_BRANCH)" >&2
        exit 2
    fi
fi

RESPONSE="$(gh api "repos/timbray/quamina/compare/${LAST_SYNC}...main")"
AHEAD_BY="$(echo "$RESPONSE" | jq -r '.ahead_by')"

if [[ "$AHEAD_BY" == "0" ]]; then
    echo "Up to date at ${LAST_SYNC:0:7}."
    exit 0
fi

echo "$AHEAD_BY new commit(s) since ${LAST_SYNC:0:7}:"
echo ""
echo "$RESPONSE" | jq -r '.commits[] | "  \(.sha[0:7])  \(.commit.author.date[0:10])  \(.commit.message | split("\n")[0])"'
echo ""
LATEST="$(echo "$RESPONSE" | jq -r '.commits[-1].sha[0:7]')"
echo "After porting:  echo \"$LATEST\" > .go-upstream-sync"
exit 1
