#!/usr/bin/env bash

# Build nntpcode inside a persistent podman daemon container.
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
#   To tear the session down: `podman stop nntpcode`

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJ_DIR=$(git rev-parse --show-toplevel)
BUILD_DIR="$PROJ_DIR/build"

# ---------------------------------------------------------------------------
# Container lifecycle (shared with ensure-container.sh)
# ---------------------------------------------------------------------------

source "$SCRIPT_DIR/ensure-container.sh"

# ---------------------------------------------------------------------------
# Build directory setup (idempotent)
# ---------------------------------------------------------------------------

mkdir -p "$BUILD_DIR"
if [ ! -L "$BUILD_DIR/Makefile" ]; then
    ln -sf ../Makefile "$BUILD_DIR/Makefile"
fi

# ---------------------------------------------------------------------------
# Run make inside the container, forwarding any extra arguments as targets
# ---------------------------------------------------------------------------

exec podman exec --user nntpuser "$CONTAINER" \
    bash -c 'cd /tmp/nntpcode/build && exec make "$@"' -- "$@"
