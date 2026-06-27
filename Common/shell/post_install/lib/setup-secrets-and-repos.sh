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

# Move an existing, non-empty dir aside to "<dir>.bak.<timestamp>" instead of
# deleting it, so re-running this on an already-configured machine can never
# clobber a live ~/.ssh or ~/.gnupg. No-op if the dir is missing or empty.
backup_dir_if_present() {
    local d="$1"
    if [ -d "$d" ] && [ -n "$(ls -A "$d" 2>/dev/null)" ]; then
        local bak
        bak="${d}.bak.$(date +%Y%m%d-%H%M%S)"
        echo ">> $d is not empty; backing it up to $bak"
        mv "$d" "$bak"
    fi
}

setup_secrets_and_repos() {
    read -r -p "Enter keys backup directory: " KEYS_DIR
    KEYS_DIR=${KEYS_DIR%/}
    [ ! -d "$KEYS_DIR" ] && echo "$KEYS_DIR is not a directory" && exit 1
    [ ! -f "$(pwd)/../scripts/my-crypt" ] && echo "$(pwd)/../scripts/my-crypt script does not exists" && exit 1
    [ ! -f "$KEYS_DIR/ssh.tar.gz.gpg" ] && echo "$KEYS_DIR/ssh.tar.gz.gpg does not exists" && exit 1
    [ ! -f "$KEYS_DIR/gpg.tar.gz.gpg" ] && echo "$KEYS_DIR/gpg.tar.gz.gpg does not exists" && exit 1

    echo ">> Setting up ssh keys from $KEYS_DIR/ssh.tar.gz.gpg"
    "$(pwd)/../scripts/my-crypt" -d "$KEYS_DIR/ssh.tar.gz.gpg"
    [ ! -d "$KEYS_DIR"/.ssh ] && echo "Decryption failed, $KEYS_DIR/ssh does not exists" && exit 1
    backup_dir_if_present ~/.ssh && mkdir -p ~/.ssh
    cp "$KEYS_DIR"/.ssh/id_* ~/.ssh
    cp "$KEYS_DIR"/.ssh/config ~/.ssh
    chmod 700 ~/.ssh
    chmod 644 ~/.ssh/config
    chmod 600 ~/.ssh/id_*
    chmod 644 ~/.ssh/id_*.pub
    rm -rf "$KEYS_DIR"/.ssh

    echo ">> Setting up gpg keys from $KEYS_DIR/gpg.tar.gz.gpg"
    "$(pwd)/../scripts/my-crypt" -d "$KEYS_DIR/gpg.tar.gz.gpg"
    [ ! -d "$KEYS_DIR"/gpg ] && echo "Decryption failed, $KEYS_DIR/gpg does not exists" && exit 1
    backup_dir_if_present ~/.gnupg && mkdir -p ~/.gnupg
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
    [ ! -d "$HOME"/Workspace/Projects/dotfiles ] &&
        git clone git@github.com:jvillasante/dotfiles.git "$HOME"/Workspace/Projects/dotfiles
    [ ! -d "$HOME"/Workspace/Projects/resume ] &&
        git clone git@github.com:jvillasante/resume.git "$HOME"/Workspace/Projects/resume
    [ ! -d "$HOME"/Workspace/Projects/cpp_utils ] &&
        git clone git@github.com:jvillasante/cpp_utils.git "$HOME"/Workspace/Projects/cpp_utils

    # Set dotfiles
    if [ -f "$HOME"/Workspace/Projects/dotfiles/make.sh ]; then
        [ -f "$HOME"/.bashrc ] && mv "$HOME"/.bashrc "$HOME"/.bashrc.bak
        [ -f "$HOME"/.bash_profile ] && mv "$HOME"/.bash_profile "$HOME"/.bash_profile.bak
        [ -d "$HOME"/.config/psd ] && rm -rf "$HOME"/.config/psd
        "$HOME"/Workspace/Projects/dotfiles/make.sh
    fi
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
