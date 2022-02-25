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
    systemctl --user --no-block restart emacs
    check $?
}

while true; do
    PS3="Choose an option: "
    options=("Dotfiles Pull" "Dotfiles Sync" "Doom Sync" "Doom Upgrade" "Doom Build" "Doom Doctor" "Emacs Service Restart" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                dotfiles_pull
                hr
                break
                ;;
            2)
                dotfiles_sync
                hr
                break
                ;;
            3)
                doom_sync
                hr
                break
                ;;
            4)
                doom_upgrade
                hr
                break
                ;;
            5)
                doom_build
                hr
                break
                ;;
            6)
                doom_doctor
                hr
                break
                ;;
            7)
                emacs_service_restart
                hr
                break
                ;;
            8) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done
done
