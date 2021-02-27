#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Do you want to make your dotfiles?"; then
    DOTFILES_DIR=$(find_dotfiles)
    ${DOTFILES_DIR}/make.sh
    check $?
fi
