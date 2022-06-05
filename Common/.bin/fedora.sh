#!/usr/bin/env bash

. "$(dirname "$0")/common.sh"

CURRENT_OS="$(find_os)"
if [ ! "$CURRENT_OS" = "LINUX" ]; then
    echo "$CURRENT_OS :: Not on Linux"
    exit 1
fi

fedora_check_update() {
    if hash dnf 2>/dev/null; then
        sudo dnf check-update
        check $?
    fi
}

fedora_update() {
    if hash dnf 2>/dev/null; then
        sudo dnf -y upgrade --refresh
        check $?

        sudo dnf -y distro-sync
        check $?
    fi
}

fedora_cleanup() {
    if hash dnf 2>/dev/null; then
        sudo dnf autoremove
        check $?
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Fedora check-update" "Fedora update" "Fedora cleanup" "Fedora update-cleanup" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                fedora_check_update
                hr
                break
                ;;
            2)
                fedora_update
                hr
                break
                ;;
            3)
                fedora_cleanup
                hr
                break
                ;;
            4)
                fedora_update
                fedora_cleanup
                hr
                break
                ;;
            5) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done
done
