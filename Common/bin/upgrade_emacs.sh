#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Do you want to upgrade emacs?"; then
    DOTFILES_DIR=$(find_dotfiles)
    ${DOTFILES_DIR}/make.sh
    check $?

    ~/.emacs.d/bin/doom -y upgrade -f
    check $?
fi
