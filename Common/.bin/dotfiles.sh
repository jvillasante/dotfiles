#!/usr/bin/env bash

. "$(dirname "$0")/common.sh"

tmuxifier_update() {
    local DOTFILES_DIR
    DOTFILES_DIR="$(find_dotfiles)"

    if [ -d "$DOTFILES_DIR/.tmuxifier" ]; then
        git -C "$DOTFILES_DIR/.tmuxifier" status
        check $?

        if ask "Do you want to pull?"; then
            git -C "${DOTFILES_DIR}/.tmuxifier" pull
            check $?
        fi
    fi
}

dotfiles_pull() {
    local DOTFILES_DIR
    DOTFILES_DIR="$(find_dotfiles)"

    if [ -d "$DOTFILES_DIR" ]; then
        git -C "${DOTFILES_DIR}" status
        check $?

        if ask "Do you want to pull?"; then
            git -C "${DOTFILES_DIR}" pull
            check $?
        fi
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

doom_help() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    "${HOME}/.emacs.d/bin/doom" -y help
    check $?
}

doom_sync() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    dotfiles_sync
    "${HOME}/.emacs.d/bin/doom" -y sync
    check $?
}

doom_upgrade() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    dotfiles_sync
    "${HOME}/.emacs.d/bin/doom" -y upgrade
    check $?
}

doom_purge() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    "${HOME}/.emacs.d/bin/doom" -y purge -g
    check $?
}

doom_build() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    "${HOME}/.emacs.d/bin/doom" -y build
    check $?
}

doom_doctor() {
    if [ ! -f "${HOME}/.emacs.d/bin/doom" ]; then
        echo "doom script not present, exiting..."
        exit 1
    fi

    "${HOME}/.emacs.d/bin/doom" doctor
    check $?
}

emacs_kill() {
    if pgrep -x emacs >/dev/null; then
        emacsclient -e "(kill-emacs)"
        check $?
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Tmuxifier Update" "Dotfiles Pull" "Dotfiles Sync" "Doom Help" "Doom Sync" "Doom Upgrade" "Doom Purge" "Doom Build" "Doom Doctor" "Kill Emacs" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                tmuxifier_update
                hr
                break
                ;;
            2)
                dotfiles_pull
                hr
                break
                ;;
            3)
                dotfiles_sync
                hr
                break
                ;;
            4)
                doom_help
                hr
                break
                ;;
            5)
                doom_sync
                hr
                break
                ;;
            6)
                doom_upgrade
                hr
                break
                ;;
            7)
                doom_purge
                hr
                break
                ;;
            8)
                doom_build
                hr
                break
                ;;
            9)
                doom_doctor
                hr
                break
                ;;
            10)
                emacs_kill
                hr
                break
                ;;
            11) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done
done
