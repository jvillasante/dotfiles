#!/bin/bash

. "$(dirname "$0")/common.sh"

CURRENT_OS="$(find_os)"
if [ ! "$CURRENT_OS" = "LINUX" ]; then
    echo "$CURRENT_OS :: Not on Linux"
    exit 1
fi

fedora_update() {
    if hash dnf 2>/dev/null; then
        sudo dnf check-update && sudo dnf upgrade && sudo dnf distro-sync
        check $?
    fi
}

fedora_cleanup() {
    if hash dnf 2>/dev/null; then
        sudo dnf autoremove
        check $?
    fi
}

fedora_all() {
    fedora_update
    fedora_cleanup
}

while true; do
    PS3="Choose an option: "
    options=("Fedora update" "Fedora cleanup" "Fedora all" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                fedora_update
                break
                ;;
            2)
                fedora_cleanup
                break
                ;;
            3)
                fedora_all
                break
                ;;
            4) break 2 ;;
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
