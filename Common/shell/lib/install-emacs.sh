#!/usr/bin/env bash

# install-emacs.sh - Install emacs from source
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

install-emacs() {
    # Set branch to use (default: emacs-30, override with first argument)
    # local EMACS_BRANCH="${1:-emacs-30}"
    local EMACS_BRANCH="${1:-master}"

    # Prepare git repo
    [ ! -d "$HOME"/Workspace/Software ] && mkdir -p "$HOME"/Workspace/Software
    if [ ! -d "$HOME"/Workspace/Software/emacs ]; then
        git clone --depth 1 --branch "$EMACS_BRANCH" https://git.savannah.gnu.org/git/emacs.git "$HOME"/Workspace/Software/emacs
        pushd "$HOME"/Workspace/Software/emacs || {
            echo "Can't cd into $HOME/Workspace/Software/emacs"
            exit 1
        }
    else
        pushd "$HOME"/Workspace/Software/emacs || {
            echo "Can't cd into $HOME/Workspace/Software/emacs"
            exit 1
        }

        if [ -f Makefile ]; then
            sudo make uninstall || true
            make clean || true
            make distclean || true
        fi
        git reset --hard HEAD
        git clean -dfx
        git fetch --depth 1 origin "$EMACS_BRANCH"
        git checkout -B "$EMACS_BRANCH" FETCH_HEAD
    fi

    # Setup git branch
    local BRANCH
    BRANCH=$(git branch --show-current)
    if [[ "$BRANCH" != "$EMACS_BRANCH" ]]; then
        echo "Unexpected branch, expecting $EMACS_BRANCH, got $BRANCH"
        exit 1
    fi

    # Build and install
    # Pure GTK (Wayland native): --with-pgtk
    # Lucid (X11/Xwayland): --with-x-toolkit=lucid
    ./autogen.sh
    ./configure \
        --with-x-toolkit=lucid \
        --with-native-compilation=aot \
        --with-tree-sitter \
        --with-dbus \
        --with-cairo-xcb \
        --without-compress-install \
        --disable-gc-mark-trace \
        CFLAGS="-O2 -march=native -mtune=native -pipe -fomit-frame-pointer -fno-semantic-interposition -flto=auto" \
        LDFLAGS="-flto=auto"
    make -j"$(nproc --ignore=2)"
    sudo make install

    popd

    # Update packages
    read -rp "Emacs has been installed, do you want to update packages now? (Y/N): " confirm
    if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
        local DOTFILES_DIR
        DOTFILES_DIR="$HOME"/Workspace/Public/dotfiles/

        if [[ ! -d "$DOTFILES_DIR"/Common/emacs/emacs.d ]]; then
            echo "Emacs personal configuration not found, exiting..."
            exit 1
        fi
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/eln-cache ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/eln-cache
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/elpa ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/elpa
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/tree-sitter ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/tree-sitter
        /usr/local/bin/emacs --init-directory="$DOTFILES_DIR"/Common/emacs/emacs.d

        read -rp "Emacs packages updated, do you want to start the systemd service now? (Y/N): " confirm
        if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
            systemctl --user daemon-reload
            systemctl --user enable --now emacs.service
            systemctl --user status emacs.service
        fi
    fi
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
