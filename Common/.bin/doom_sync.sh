#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Do you want to sync doom?"; then
    DOTFILES_DIR=$(find_dotfiles)
    ${DOTFILES_DIR}/make.sh
    check $?

    ~/.emacs.d/bin/doom sync
    check $?
fi
