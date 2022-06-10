#!/usr/bin/env bash

#
# This is how I manage my dotfiles
#

. "$(dirname "$0")/common.sh"
DOTFILES_DIR="$(find_dotfiles)"

function usage() {
    echo "Usage:"
    echo "    $0 help:"
    echo "        Show this help message"
    echo "    $0 tmuxifier update:"
    echo "        Update tmuxifier"
    echo "    $0 dotfiles [pull|sync]:"
    echo "        Update personal dotfiles (pull from remote or sync on local)"
    echo "    $0 doom [arbitrary doom command]:"
    echo "        Run arbitrary doom command"
    echo "    $0 emacs kill"
    echo "        Kill emacs daemon"
    echo
    echo " e.g: $0 doom -y sync"
    exit "$1"
}

function do_tmuxifier() {
    case $1 in
        update)
            if [ -d "$DOTFILES_DIR/.tmuxifier" ]; then
                git -C "$DOTFILES_DIR/.tmuxifier" status
                check $?

                if ask "Do you want to pull?"; then
                    git -C "${DOTFILES_DIR}/.tmuxifier" pull
                    check $?
                fi
            fi
            ;;
        *)
            usage 1
            ;;
    esac
}

function do_dotfiles() {
    case $1 in
        pull)
            if [ -d "$DOTFILES_DIR" ]; then
                git -C "${DOTFILES_DIR}" status
                check $?

                if ask "Do you want to pull?"; then
                    git -C "${DOTFILES_DIR}" pull
                    check $?
                fi
            fi
            ;;
        sync)
            if [ ! -f "${DOTFILES_DIR}/make.sh" ]; then
                echo "make script not present, exiting..."
                exit 1
            fi

            "${DOTFILES_DIR}/make.sh"
            check $?
            ;;
        *)
            usage 1
            ;;
    esac
}

function do_emacs() {
    case $1 in
        kill)
            if pgrep -x emacs >/dev/null; then
                emacsclient -e "(kill-emacs)"
                check $?
            fi
            ;;
        *)
            usage 1
            ;;
    esac
}

nargs=$#
cmd=${1-}
rc=0
if [ "$#" -gt 0 ]; then shift; fi
case $cmd in
    tmuxifier)
        [ "$nargs" -eq 2 ] || usage 1
        do_tmuxifier "$@"
        ;;
    dotfiles)
        [ "$nargs" -eq 2 ] || usage 1
        do_dotfiles "$@"
        ;;
    doom)
        [ "$nargs" -lt 2 ] && usage 1
        if [ ! -f "${DOTFILES_DIR}/.emacs.doom/bin/doom" ]; then
            echo "doom script not present, exiting..."
            exit 1
        else
            "${DOTFILES_DIR}/.emacs.doom/bin/doom" "$@"
        fi
        ;;
    emacs)
        [ "$nargs" -eq 2 ] || usage 1
        do_emacs "$@"
        ;;
    help | --help | -h)
        usage 0
        ;;
    *)
        usage 1
        ;;
esac
exit $rc
