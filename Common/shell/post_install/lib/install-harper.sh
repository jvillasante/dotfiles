#!/usr/bin/env bash

# install-harper.sh - Install harper from github release
#
# This file should be sourced by other scripts, not executed directly.

# Only apply options if this file hasn't been sourced before,
# and if it's being sourced (not executed directly).
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
    # Exit immediately if a command exits with a non-zero status.
    set -o errexit

    # Treat unset variables as an error.
    set -o nounset
fi

install_harper() {
    # Install
    [ ! -d "$HOME"/Workspace/Software ] && mkdir -p "$HOME"/Workspace/Software
    [ ! -d "$HOME"/Workspace/Software/harper ] && mkdir -p "$HOME"/Workspace/Software/harper

    pushd "$HOME"/Workspace/Software/harper || {
        notify-send "Can't cd into $HOME/Workspace/Software/harper" --expire-time=20
        exit 1
    }

    HARPER_VERSION=v0.70.0
    HARPER_CLI_RELEASE=harper-cli-x86_64-unknown-linux-gnu.tar.gz
    HARPER_LS_RELEASE=harper-ls-x86_64-unknown-linux-gnu.tar.gz

    curl -LJO https://github.com/Automattic/harper/releases/download/"$HARPER_VERSION"/"$HARPER_CLI_RELEASE"
    atool --extract --explain "$HARPER_CLI_RELEASE"
    sudo cp -f harper-cli /usr/local/bin/

    curl -LJO https://github.com/Automattic/harper/releases/download/"$HARPER_VERSION"/"$HARPER_LS_RELEASE"
    atool --extract --explain "$HARPER_LS_RELEASE"
    sudo cp -f harper-ls /usr/local/bin/

    popd || {
        notify-send "Can't cd to previous directory" --expire-time=20
        exit 1
    }
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
