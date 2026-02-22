#!/usr/bin/env bash

# install-stylua.sh - Install StyLua from github release
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

install_stylua() {
    # Install
    [ ! -d "$HOME"/Workspace/Software ] && mkdir -p "$HOME"/Workspace/Software
    [ ! -d "$HOME"/Workspace/Software/stylua ] && mkdir -p "$HOME"/Workspace/Software/stylua

    pushd "$HOME"/Workspace/Software/stylua || {
        echo "Can't cd into $HOME/Workspace/Software/stylua"
        exit 1
    }

    STYLUA_VERSION=v2.3.1
    STYLUA_RELEASE=stylua-linux-x86_64.zip
    curl -LJO https://github.com/JohnnyMorganz/StyLua/releases/download/"$STYLUA_VERSION"/"$STYLUA_RELEASE"
    atool --extract --explain "$STYLUA_RELEASE"
    sudo cp -f stylua /usr/local/bin/

    popd || {
        echo "Can't cd to previous directory"
        exit 1
    }
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
