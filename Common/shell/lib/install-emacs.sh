#!/usr/bin/env bash

# install-emacs.sh - Install emacs from source
#
# This file should be sourced by other scripts, not executed directly.
#
# Note: errexit/nounset are intentionally not set here to avoid leaking
# shell options to the sourcing script. Errors are handled explicitly.

install_emacs() {
    # Set branch to use (default: emacs-31, override with first argument)
    local EMACS_BRANCH="${1:-emacs-31}"
    # local EMACS_BRANCH="${1:-master}"

    # Prepare git repo
    mkdir -p "$HOME"/Workspace/Software
    if [ ! -d "$HOME"/Workspace/Software/emacs ]; then
        git clone --depth 1 --branch "$EMACS_BRANCH" https://git.savannah.gnu.org/git/emacs.git \
            "$HOME"/Workspace/Software/emacs
        pushd "$HOME"/Workspace/Software/emacs || {
            echo "Can't cd into $HOME/Workspace/Software/emacs"
            return 1
        }
    else
        pushd "$HOME"/Workspace/Software/emacs || {
            echo "Can't cd into $HOME/Workspace/Software/emacs"
            return 1
        }

        if [ -f Makefile ]; then
            sudo make uninstall || true
            make clean || true
            make distclean || true
        fi
        git clean -dfx
        git fetch --depth 1 origin "$EMACS_BRANCH"
        git checkout -B "$EMACS_BRANCH" FETCH_HEAD
    fi

    # Build and install
    # Pure GTK (Wayland native): --with-pgtk
    # Lucid (X11/Xwayland): --with-x-toolkit=lucid
    ./autogen.sh
    ./configure \
        --without-x \
        --with-pgtk \
        --with-toolkit-scroll-bars \
        --with-cairo \
        --without-xft \
        --with-harfbuzz \
        --without-libotf \
        --with-gnutls \
        --without-xdbe \
        --without-xim \
        --without-gpm \
        --disable-gc-mark-trace \
        --enable-link-time-optimization \
        --with-gsettings \
        --with-modules \
        --with-threads \
        --with-libgmp \
        --with-xml2 \
        --with-tree-sitter \
        --with-zlib \
        --with-native-compilation \
        --with-file-notification=inotify \
        --without-compress-install \
        CFLAGS="-O2 -pipe -march=native -mtune=native -fno-omit-frame-pointer -flto=auto" \
        LDFLAGS="-Wl,-O2 -Wl,--sort-common -Wl,--as-needed -Wl,-z,pack-relative-relocs -flto=auto"
    make -j"$(nproc)" -l"$(nproc --ignore=1)"
    sudo make install-strip

    popd || return 1

    # Update packages
    read -rp "Emacs has been installed, do you want to update packages now? (Y/N): " confirm
    if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
        local DOTFILES_DIR
        DOTFILES_DIR="$HOME"/Workspace/Projects/dotfiles/

        if [[ ! -d "$DOTFILES_DIR"/Common/emacs/emacs.d ]]; then
            echo "Emacs personal configuration not found, exiting..."
            return 1
        fi
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/eln-cache ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/eln-cache
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/elpa ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/elpa
        [[ -d "$DOTFILES_DIR"/Common/emacs/emacs.d/var/tree-sitter ]] &&
            rm -rf "$DOTFILES_DIR"/Common/emacs/emacs.d/var/tree-sitter
        find "$DOTFILES_DIR"/Common/emacs/emacs.d/ -name '*.eln' -delete
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
return 0 2>/dev/null
