#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Do you want to update your dotfiles"; then
    DOTFILES_DIR=$(find_dotfiles)
    
    ${DOTFILES_DIR}/make.sh
    check $?
fi

if ask "Do you want to upgrade emacs?"; then
    ~/.emacs.d/bin/doom clean
    check $?

    ~/.emacs.d/bin/doom -y upgrade -f
    check $?

    ~/.emacs.d/bin/doom compile :core
    check $?
fi

if ask "Do you want to sync emacs?"; then
    # ~/.emacs.d/bin/doom update
    # check $?

    ~/.emacs.d/bin/doom sync
    check $?
fi
