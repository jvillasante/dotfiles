#!/bin/bash

. "$(dirname "$0")/common.sh"

dotfiles_pull() {
    local DOTFILES_DIR
    DOTFILES_DIR="$(find_dotfiles)"

    git -C "${DOTFILES_DIR}" status
    check $?

    if ask "Do you want to pull?"; then
        git -C "${DOTFILES_DIR}" pull
        check $?
    fi
}

dotfiles_sync() {
    local DOTFILES_DIR
    DOTFILES_DIR="$(find_dotfiles)"
    if [ ! -f "${DOTFILES_DIR}/make.sh" ]; then
        echo "make script not present, exiting..."
        exit 1
    fi

    "${DOTFILES_DIR}/make.sh"
    check $?
}

doom_sync() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    dotfiles_sync
    "${HOME}/.emacs.d/bin/doom" sync
    check $?
}

doom_upgrade() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    dotfiles_sync
    "${HOME}/.emacs.d/bin/doom" -y upgrade -f
    check $?
}

doom_build() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    dotfiles_sync
    "${HOME}/.emacs.d/bin/doom" -y build -f
    check $?
}

doom_doctor() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    dotfiles_sync
    "${HOME}/.emacs.d/bin/doom" doctor
    check $?
}

emacs_service_restart() {
    printf "\nRestarting emacs service..."
    systemctl --user restart emacs
    check $?
}

while true; do
    PS3="Choose an option: "
    options=("Dotfiles Pull" "Dotfiles Sync" "Doom Sync" "Doom Upgrade" "Doom Build" "Doom Doctor" "Emacs Service Restart" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                dotfiles_pull
                break
                ;;
            2)
                dotfiles_sync
                break
                ;;
            3)
                doom_sync
                emacs_service_restart
                break
                ;;
            4)
                doom_upgrade
                emacs_service_restart
                break
                ;;
            5)
                doom_build
                break
                ;;
            6)
                doom_doctor
                break
                ;;
            7)
                emacs_service_restart
                break
                ;;
            8) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done

    echo ""

    if ask "Are we done?"; then
        break
    else
        echo ""
    fi
done
