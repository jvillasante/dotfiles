#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Do you want to update your dotfiles"; then
    DOTFILES_DIR=$(find_dotfiles)
    
    ${DOTFILES_DIR}/make.sh
    check $?
fi

if ask "Do you want to upgrade emacs?"; then
    ~/.emacs.d/bin/doom -y upgrade -f
    check $?
fi

if ask "Do you want to sync emacs?"; then
    ~/.emacs.d/bin/doom sync
    check $?
fi
