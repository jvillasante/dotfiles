#!/usr/bin/env bash

# setup-ssh-and-gpg.sh - Setup ssh/gpg and clone public repos
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

setup-secrets-and-repos() {
    read -r -p "Enter keys backup directory: " KEYS_DIR
    KEYS_DIR=${KEYS_DIR%/}
    [ ! -d "$KEYS_DIR" ] && notify-send "$KEYS_DIR is not a directory" --expire-time=20 && exit 1
    [ ! -f "$(pwd)/../scripts/+crypt" ] && notify-send "$(pwd)/../scripts/+crypt script does not exists" \
                                                       --expire-time=20 && exit 1
    [ ! -f "$KEYS_DIR/ssh.tar.gz.gpg" ] && notify-send "$KEYS_DIR/ssh.tar.gz.gpg does not exists" \
                                                       --expire-time=20 && exit 1
    [ ! -f "$KEYS_DIR/gpg.tar.gz.gpg" ] && notify-send "$KEYS_DIR/gpg.tar.gz.gpg does not exists" \
                                                       --expire-time=20 && exit 1

    echo ">> Setting up ssh keys from $KEYS_DIR/ssh.tar.gz.gpg"
    "$(pwd)/../scripts/+crypt" -d "$KEYS_DIR/ssh.tar.gz.gpg"
    [ ! -d "$KEYS_DIR"/.ssh ] && notify-send "Decryption failed, $KEYS_DIR/ssh does not exists" \
                                             --expire-time=20 && exit 1
    mkdir -p ~/.ssh && rm -rf ~/.ssh/*
    cp "$KEYS_DIR"/.ssh/id_* ~/.ssh
    cp "$KEYS_DIR"/.ssh/config ~/.ssh
    chmod 700 ~/.ssh
    chmod 644 ~/.ssh/config
    chmod 600 ~/.ssh/id_*
    chmod 644 ~/.ssh/id_*.pub
    rm -rf "$KEYS_DIR"/.ssh

    echo ">> Setting up gpg keys from $KEYS_DIR/gpg.tar.gz.gpg"
    "$(pwd)/../scripts/+crypt" -d "$KEYS_DIR/gpg.tar.gz.gpg"
    [ ! -d "$KEYS_DIR"/gpg ] && notify-send "Decryption failed, $KEYS_DIR/gpg does not exists" \
                                            --expire-time=20 && exit 1
    mkdir -p ~/.gnupg && rm -rf ~/.gnupg/*
    cp "$KEYS_DIR"/gpg/config/*.conf ~/.gnupg
    gpg --import "$KEYS_DIR"/gpg/new_keys/0xB3F739419D91C7F3-2022-09-28.pub.asc
    rm -rf "$KEYS_DIR"/gpg

    echo "Editing gpg key 0xB3F..., you should 'trust' ultimately (Option 5) and 'quit'"
    gpg --edit-key 0xB3F739419D91C7F3

    # Remove keys
    rm -rf "$KEYS_DIR"

    # With the new keys we can go ahead and download some repos
    [ ! -d "$HOME/.password-store" ] &&
        git clone git@github.com:jvillasante/pass.git "$HOME"/.password-store
    [ ! -d "$HOME"/Workspace/Public/dotfiles ] &&
        git clone git@github.com:jvillasante/dotfiles.git "$HOME"/Workspace/Public/dotfiles
    [ ! -d "$HOME"/Workspace/Public/resume ] &&
        git clone git@github.com:jvillasante/resume.git "$HOME"/Workspace/Public/resume
    [ ! -d "$HOME"/Workspace/Public/cpp_utils ] &&
        git clone git@github.com:jvillasante/cpp_utils.git "$HOME"/Workspace/Public/cpp_utils

    # Set dotfiles
    if [ -f "$HOME"/Workspace/Public/dotfiles/make.sh ]; then
        [ -f "$HOME"/.bashrc ] && mv "$HOME"/.bashrc "$HOME"/.bashrc.bak
        [ -f "$HOME"/.bash_profile ] && mv "$HOME"/.bash_profile "$HOME"/.bash_profile.bak
        [ -d "$HOME"/.config/psd ] && rm -rf "$HOME"/.config/psd
        "$HOME"/Workspace/Public/dotfiles/make.sh

        # Udev rules for ZSA Voyager
        # sudo cp -f "$HOME"/Workspace/Public/dotfiles/Common/udev/50-zsa.rules \
            #      /etc/udev/rules.d/50-zsa.rules

        # Custom DNS - Breaks work VPN
        # [ -d /etc/systemd/resolved.conf.d ] && sudo mv /etc/systemd/resolved.conf.d /etc/systemd/resolved.conf.d.bak
        # sudo cp -r "$HOME"/Workspace/Public/dotfiles/Common/systemd/resolved.conf.d /etc/systemd/
        # sudo systemctl restart systemd-resolved
        # verify DoT is working
        #   resolvectl status
    fi
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
