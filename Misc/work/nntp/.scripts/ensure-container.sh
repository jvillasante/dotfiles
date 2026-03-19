#!/usr/bin/env bash

# Ensure the nntp podman container is running for the current worktree.
# Starts the container if it is not already running; otherwise does nothing.
#
# Meant to be called directly or sourced by other scripts (e.g. build.sh).
# After this script completes, $CONTAINER holds the running container name.

set -euo pipefail

PROJ_DIR=$(git rev-parse --show-toplevel)
WORKTREE_NAME=$(basename "$PROJ_DIR")
CONTAINER="nntp-${WORKTREE_NAME}"
IMAGE="localhost/nntp:latest"

container_running() {
    podman inspect -f '{{.State.Running}}' "$CONTAINER" 2>/dev/null | grep -q "^true$"
}

if ! container_running; then
    # Remove any stale stopped container with the same name before starting fresh.
    # (`podman rm` fails silently if the container does not exist)
    podman rm "$CONTAINER" 2>/dev/null || true

    echo "[container] Starting daemon container '$CONTAINER' from image '$IMAGE'..."
    podman run \
        --user nntpuser \
        --rm \
        --detach \
        --init \
        --volume "$PROJ_DIR":/tmp/nntpcode:rw,z \
        --userns=keep-id \
        --workdir /tmp/nntpcode \
        --name "$CONTAINER" \
        "$IMAGE" \
        tail -f /dev/null
else
    echo "[container] Reusing running container '$CONTAINER'."
fi
