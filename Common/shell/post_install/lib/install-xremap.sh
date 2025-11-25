#!/usr/bin/env bash

# install-xremap.sh - Install xremap from github release
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

install_xremap() {
    # Install
    [ ! -d "$HOME"/Workspace/Software ] && mkdir -p "$HOME"/Workspace/Software
    [ ! -d "$HOME"/Workspace/Software/xremap ] && mkdir -p "$HOME"/Workspace/Software/xremap

    pushd "$HOME"/Workspace/Software/xremap || {
        echo "Can't cd into $HOME/Workspace/Software/xremap"
        exit 1
    }

    XREMAP_VERSION=v0.14.3
    XREMAP_RELEASE=xremap-linux-x86_64-kde.zip
    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
        XREMAP_RELEASE=xremap-linux-x86_64-gnome.zip
    fi

    curl -LJO https://github.com/k0kubun/xremap/releases/download/"$XREMAP_VERSION"/"$XREMAP_RELEASE"
    atool --extract --explain "$XREMAP_RELEASE"
    sudo cp -f xremap /usr/local/bin/

    popd || {
        echo "Can't cd to previous directory"
        exit 1
    }

    # First create a new group to which we allow access to the input stuff; add this group to your user:
    sudo gpasswd -a "$USER" input

    # Second Create new udev rule granting access:
    sudo cp -f "$HOME"/Workspace/Public/dotfiles/Common/udev/70-xremap.rules \
         /etc/udev/rules.d/70-xremap.rules

    # Enable the daemon
    [ ! -d "$HOME"/.config/systemd/user ] && mkdir -p "$HOME"/.config/systemd/user
    [ -L "$HOME/.config/systemd/user/xremap.service" ] &&
        unlink "$HOME/.config/systemd/user/xremap.service"
    ln -s "$HOME/Workspace/Public/dotfiles/Common/systemd/user/xremap.service" \
       "$HOME/.config/systemd/user"
    systemctl --user daemon-reload
    systemctl --user enable --now xremap.service && systemctl --user start xremap.service
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
