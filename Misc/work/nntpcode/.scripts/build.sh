#!/usr/bin/env bash

# Build nntpcode inside a persistent podman daemon container.
#
# Usage: build.sh [make-targets...]
#   Any arguments are forwarded directly to make, e.g.:
#     build.sh clean
#     build.sh -j4
#
# Session behaviour:
#   First call starts the container as a background daemon (tail -f /dev/null).
#   Subsequent calls reuse the running container via `podman exec`.
#   The container is auto-removed when stopped (--rm).
#   To tear the session down: `podman stop nntpcode`

set -euo pipefail

PROJ_DIR="$(cd "$(dirname "$0")/.." && pwd)"
CONTAINER="nntpcode"
IMAGE="localhost/nntp:latest"
BUILD_DIR="$PROJ_DIR/build"

# ---------------------------------------------------------------------------
# Container lifecycle
# ---------------------------------------------------------------------------

container_running() {
    podman inspect -f '{{.State.Running}}' "$CONTAINER" 2>/dev/null | grep -q "^true$"
}

if ! container_running; then
    # Remove any stale stopped container with the same name before starting fresh.
    # (podman rm fails silently if the container does not exist)
    podman rm "$CONTAINER" 2>/dev/null || true

    echo "[build] Starting daemon container '$CONTAINER' from image '$IMAGE'..."
    podman run \
        --user nntpuser \
        --rm \
        --detach \
        --volume "$PROJ_DIR":/tmp/nntpcode:rw,z \
        --userns=keep-id \
        --workdir /tmp/nntpcode \
        --name "$CONTAINER" \
        "$IMAGE" \
        tail -f /dev/null
else
    echo "[build] Reusing running container '$CONTAINER'."
fi

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
