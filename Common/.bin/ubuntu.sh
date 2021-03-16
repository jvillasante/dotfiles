#!/bin/bash

. $(dirname "$0")/common.sh

CURRENT_OS=$(find_os)
if [ ! $CURRENT_OS = "LINUX" ]; then
    echo "$CURRENT_OS :: Not on Linux"
    exit 1
fi

ubuntu_update() {
    if hash apt 2>/dev/null; then
        sudo apt update && sudo apt upgrade && sudo apt dist-upgrade
        check $?
    fi
}

ubuntu_cleanup() {
    if hash apt 2>/dev/null; then
        sudo apt autoremove && sudo apt autoclean
        check $?
    fi
}

ubuntu_all() {
    ubuntu_update
    ubuntu_cleanup
}

while true; do
    PS3="Choose an option: "
    options=("Ubuntu update" "Ubuntu cleanup" "Ubuntu all" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1) ubuntu_update; break ;;
            2) ubuntu_cleanup; break ;;
            3) ubuntu_all; break ;;
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
