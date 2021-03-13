#!/bin/sh

. $(dirname "$0")/common.sh

dotfiles_sync() {
    DOTFILES_DIR=$(find_dotfiles)
    if [ ! -f ${DOTFILES_DIR}/make.sh ]; then
        echo "make script not present, exiting..."
        exit 1
    fi

    ${DOTFILES_DIR}/make.sh
    check $?
}

doom_sync() {
    if [ ! -f ${HOME}/.emacs.d/bin/doom ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    dotfiles_sync
    ${HOME}/.emacs.d/bin/doom sync
    check $?
}

doom_upgrade() {
    if [ ! -f ${HOME}/.emacs.d/bin/doom ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    dotfiles_sync
    ${HOME}/.emacs.d/bin/doom -y upgrade -f
    check $?
}

while true; do
    PS3="Choose an option: "
    options=("Dotfiles Symlink" "Doom Sync" "Doom Upgrade" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1) dotfiles_sync; break ;;
            2) doom_sync; break ;;
            3) doom_upgrade; break ;;
            4) break 2 ;;
            *) echo "Invalid option!" >&2
        esac
    done

    echo ""

    if ask "Are we done?"; then
        break
    else
        echo ""
    fi
done
