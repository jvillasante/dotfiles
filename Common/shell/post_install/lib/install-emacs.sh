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
    # Set branch to use
    local EMACS_BRANCH
    EMACS_BRANCH=emacs-30

    # Prepare git repo
    [ ! -d "$HOME"/Workspace/Software ] && mkdir -p "$HOME"/Workspace/Software
    if [ ! -d "$HOME"/Workspace/Software/emacs ]; then
        git clone --depth 1 --branch "$EMACS_BRANCH" git://git.savannah.gnu.org/emacs.git "$HOME"/Workspace/Software/emacs
        pushd "$HOME"/Workspace/Software/emacs || {
            echo "Can't cd into $HOME/Workspace/Software/emacs"
            exit 1
        }
    else
        pushd "$HOME"/Workspace/Software/emacs || {
            echo "Can't cd into $HOME/Workspace/Software/emacs"
            exit 1
        }

        sudo make uninstall
        make clean && make distclean
        git reset --hard HEAD
        sudo git clean -dfx
        git fetch && git pull
    fi

    # Setup git branch
    local BRANCH
    BRANCH=$(git branch --show-current)
    [[ "$BRANCH" != "$EMACS_BRANCH" ]] && echo "Unexpected branch, expecting $EMACS_BRANCH, got $BRANCH" && exit 1

    # Install
    # Pure GTK: --with-x-toolkit=no --with-pgtk
    # Lucid: --with-x-toolkit=lucid --with-cairo --with-xft
    ./autogen.sh
    ./configure \
        --prefix=/usr/local \
        --without-compress-install \
        --disable-gc-mark-trace \
        --with-x-toolkit=lucid --with-cairo --with-xft \
        --with-native-compilation=aot \
        --with-tree-sitter \
        CFLAGS="-O2 -mtune=native -march=native -pipe -fomit-frame-pointer"
    make -j"$(nproc --ignore=2)" NATIVE_FULL_AOT=1
    sudo make install

    popd

    # Update packages
    read -rp "Emacs has been installed, do you want to update packages now? (Y/N): " confirm
    if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
        local DOTFILES_DIR
        DOTFILES_DIR="$HOME"/Workspace/Public/dotfiles/

        [[ ! -d "$DOTFILES_DIR"/Common/emacs/emacs.d ]] &&
            echo "Emacs personal configuration not found, exiting..." && exit 1
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/eln-cache ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/eln-cache
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/elpa ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/elpa
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/tree-sitter ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/tree-sitter
        /usr/local/bin/emacs --init-directory="$DOTFILES_DIR"/Common/emacs/emacs.d
    fi

    read -rp "Emacs packages updated, do you want to start the systemd service now? (Y/N): " confirm
    if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
        systemctl --user daemon-reload
        sleep 1 && systemctl --user enable --now emacs.service
        sleep 1 && systemctl --user start emacs.service
        sleep 1 && systemctl --user status emacs.service
    fi
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
