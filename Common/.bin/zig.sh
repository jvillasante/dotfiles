#!/bin/bash

. "$(dirname "$0")/common.sh"

update_zls() {
    if hash zig 2>/dev/null; then
        local ZIG_DIR="${HOME}/Workspace/Software/zig"
        if [ ! -d "$ZIG_DIR" ]; then
            mkdir "$ZIG_DIR"
        fi

        local ZLS_DIR="$ZIG_DIR/zls"
        if [ -d "$ZLS_DIR" ]; then
            rm -rf "$ZLS_DIR"
        fi

        git clone --recurse-submodules git@github.com:zigtools/zls.git "$ZLS_DIR"
        check $?

        cd "$ZLS_DIR" || exit 1
        zig build -Drelease-safe
        check $?
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Update ZLS" "Quit")
    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                update_zls
                hr
                break
                ;;
            2) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done
done
