#!/usr/bin/env bash

. "$(dirname "$0")/common.sh"

CURRENT_OS="$(find_os)"
if [ ! "$CURRENT_OS" = "LINUX" ]; then
    echo "$CURRENT_OS :: Not on Linux"
    exit 1
fi

function usage() {
    echo "Usage:"
    echo "    $0 help:"
    echo "        Show this help message"
    echo "    $0 update:"
    echo "        Update DNF Packages"
    echo "    $0 dnf [arbitrary DNF command]:"
    echo "        Run arbitrary DNF command"
    echo
    echo " e.g: $0 dnf check-update"
    exit "$1"
}

function fedora_update() {
    dnf -y upgrade --refresh
    check $?

    dnf -y distro-sync
    check $?
}

function fedora_cleanup() {
    dnf autoremove
    check $?
}

function update_and_cleanup() {
    if hash dnf 2>/dev/null; then
        fedora_update
        fedora_cleanup
    fi
}

nargs=$#
cmd=${1-}
rc=0
if [ "$#" -gt 0 ]; then shift; fi
case $cmd in
    update)
        [ "$nargs" -eq 1 ] || usage 1
        update_and_cleanup "$@"
        ;;
    dnf)
        [ "$nargs" -lt 2 ] && usage 1
        if hash dnf 2>/dev/null; then
            dnf "$@"
        else
            echo "dnf not installed, exiting..."
            exit 1
        fi
        ;;
    help | --help | -h)
        usage 0
        ;;
    *)
        usage 1
        ;;
esac
exit $rc
