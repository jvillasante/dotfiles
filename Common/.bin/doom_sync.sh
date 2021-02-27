#!/bin/sh

. $(dirname "$0")/common.sh

if [ ! -f ~/.emacs.d/bin/doom ]; then
    echo "doom script not present"
    exit 1
fi

if ask "Do you want to sync doom?"; then
    DOTFILES_DIR=$(find_dotfiles)
    ${DOTFILES_DIR}/make.sh
    check $?

    ~/.emacs.d/bin/doom sync
    check $?
fi
