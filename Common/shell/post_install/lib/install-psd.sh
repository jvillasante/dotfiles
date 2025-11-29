#!/usr/bin/env bash

# install-psd.sh - Install psd from source
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

install_psd() {
    # Set branch to use
    local PSD_BRANCH
    PSD_BRANCH=master

    # Prepare git repo
    [ ! -d "$HOME"/Workspace/Software ] && mkdir -p "$HOME"/Workspace/Software
    if [ ! -d "$HOME"/Workspace/Software/profile-sync-daemon ]; then
        git clone --depth 1 --branch "$PSD_BRANCH" git@github.com:graysky2/profile-sync-daemon.git \
            "$HOME"/Workspace/Software/profile-sync-daemon
        pushd "$HOME"/Workspace/Software/profile-sync-daemon || {
            echo "Can't cd into $HOME/Workspace/Software/profile-sync-daemon"
            exit 1
        }
    else
        echo "PSD already installed"
        exit 1
    fi

    # Setup git branch
    local BRANCH
    BRANCH=$(git branch --show-current)
    [[ "$BRANCH" != "$PSD_BRANCH" ]] && echo "Unexpected branch, expecting $PSD_BRANCH, got $BRANCH" && exit 1

    # Install
    make -j"$(nproc --ignore=2)"
    sudo make install

    popd || echo "Can't cd to previous directory" && exit 1

    # Copy brave profile
    if [ -f /usr/share/psd/contrib/brave ]; then
        sudo cp /usr/share/psd/contrib/brave /usr/share/psd/browsers
    elif [ -f "$HOME"/Workspace/Public/dotfiles/Common/psd/brave ]; then
        sudo cp "$HOME"/Workspace/Public/dotfiles/Common/psd/brave /usr/share/psd/browsers
    fi

    # PSD: Needs sudo permissions for overlay-fs - needs a logout :(
    echo 'jvillasante ALL=(ALL) NOPASSWD: /usr/bin/psd-overlay-helper' | sudo EDITOR='tee -a' visudo

    # PSD: Enable and Start the Daemon
    systemctl --user enable --now psd.service && systemctl --user start psd.service
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
