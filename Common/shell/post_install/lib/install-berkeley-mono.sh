#!/usr/bin/env bash

# install-berkeley-mono.sh - Install the best fonts system wide
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

install_berkeley_mono_font() {
    [ ! -d "$(pwd)"/fonts ] && notify-send "$(pwd)/fonts is not a directory" --expire-time=20 && return
    [ ! -f "$(pwd)"/fonts/berkeley-mono-typeface.tar.gz.gpg ] &&
        notify-send "$(pwd)/fonts/berkeley-mono-typeface.tar.gz.gpg does not exists" --expire-time=20 && return
    [ ! -f "$(pwd)"/../scripts/+crypt ] &&
        notify-send "$(pwd)/../scripts/+crypt script does not exists" --expire-time=20 && return
    [ ! -d "$HOME"/Workspace/Software/fonts ] && mkdir -p "$HOME"/Workspace/Software/fonts
    [ -d "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface ] &&
        rm -rf "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface

    # copy and decrypt Berkeley Mono typeface
    cp "$(pwd)"/fonts/berkeley-mono-typeface.tar.gz.gpg "$HOME"/Workspace/Software/fonts
    [ ! -f "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface.tar.gz.gpg ] &&
        notify-send "$HOME/Workspace/Software/fonts/berkeley-mono-typeface.tar.gz.gpg does not exists" \
                    --expire-time=20 && return
    "$(pwd)"/../scripts/+crypt -d "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface.tar.gz.gpg
    [ ! -d "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface ] &&
        notify-send "Decryption failed, $HOME/Workspace/Software/fonts/berkeley-mono-typeface does not exists" \
                    --expire-time=20 && return

    # just in case
    mkdir -p ~/.local/share/fonts

    # remove all fonts from ~/.local/share/fonts that start with "BerkeleyMono"
    rm -rf ~/.local/share/fonts/Berkeley*

    # copy all V1 TTF fonts to ~/.local/share/fonts
    # find "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface/TX-01/ \
    #      -type f -name "*.ttf" -exec cp {} "$HOME"/.local/share/fonts/ \; -print

    # copy all V2 OTF fonts to ~/.local/share/fonts
    find "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface/TX-02/ \
         -type f -name "*.otf" -exec cp {} "$HOME"/.local/share/fonts/ \; -print

    # Build font information caches
    fc-cache -f

    # cleanup
    [ -d "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface ] &&
        rm -rf "$HOME"/Workspace/Software/fonts/berkeley-mono-typeface
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
