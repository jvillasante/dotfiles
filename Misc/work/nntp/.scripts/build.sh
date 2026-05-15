#!/usr/bin/env bash

# Build nntpcode inside a persistent podman daemon container, then refresh
# build/compile_commands.json from the JSON fragments dropped by the
# compile-commands recording wrapper (/usr/local/bin/{gcc,g++} in the image,
# which shadow the real compilers via PATH).
#
# Usage: build.sh [make-targets...]
#   Any arguments are forwarded directly to make, e.g.:
#     build.sh clean
#     build.sh -j4
#
# Session behavior:
#   First call starts the container as a background daemon (`tail -f /dev/null`).
#   Subsequent calls reuse the running container via `podman exec`.
#   The container is auto-removed when stopped (`--rm`).
#   To tear the session down: `podman stop nntp-<worktree>`.
#
# compile_commands.json behavior:
#   The container bind-mounts the project at the same absolute path it has on
#   the host, so paths in the recorded fragments are already correct on both
#   sides — no rewriting needed. The fragment dir is recreated before each
#   `make` so a build only stitches what *this* invocation compiled. The
#   stitcher (compdb-stitch, inside the image) then merges those entries
#   into the existing compile_commands.json (keyed on `file`, new wins),
#   preserving entries from previous builds for files this build didn't
#   touch. Incremental builds keep the compdb complete.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJ_DIR=$(git rev-parse --show-toplevel)
BUILD_DIR="$PROJ_DIR/build"
FRAG_DIR="$BUILD_DIR/.compdb-fragments"

# ---------------------------------------------------------------------------
# Container lifecycle (shared with ensure-container.sh)
# ---------------------------------------------------------------------------

source "$SCRIPT_DIR/ensure-container.sh"

# ---------------------------------------------------------------------------
# Build directory setup (idempotent). Recreate the fragment dir so we only
# stitch what this build compiled.
# ---------------------------------------------------------------------------

mkdir -p "$BUILD_DIR"
if [ ! -L "$BUILD_DIR/Makefile" ]; then
    ln -sf ../Makefile "$BUILD_DIR/Makefile"
fi
rm -rf "$FRAG_DIR"
mkdir -p "$FRAG_DIR"

# ---------------------------------------------------------------------------
# Run make + stitch fragments inside the container. Non-interactive `bash -c`
# does not source the container's .bashrc (it bails out early for non-
# interactive shells), so set MAKEFLAGS explicitly. NNTP_PROJ_DIR and
# COMPDB_FRAGMENT_DIR are inherited from the container's environment, set by
# ensure-container.sh at `podman run` time.
# ---------------------------------------------------------------------------

podman exec --user nntpuser "$CONTAINER" bash -c '
    : "${NNTP_PROJ_DIR:?NNTP_PROJ_DIR not set inside container}"
    cd "$NNTP_PROJ_DIR/build"
    export MAKEFLAGS="PARALLEL_SUBDIRS=1 -j$(nproc --ignore=2)"
    make "$@"
    rc=$?
    compdb-stitch || true
    exit $rc
' -- "$@"
