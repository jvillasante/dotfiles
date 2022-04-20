#!/bin/sh

. "$(dirname "$0")/common.sh"

update_zig() {
    local ZIG_DIR="${HOME}/Workspace/Software/zig"
    if [ ! -d "$ZIG_DIR" ]; then
        mkdir "$ZIG_DIR"
    fi

    local ZIG_ZIG_DIR="$ZIG_DIR/zig"
    if [ -d "$ZIG_ZIG_DIR" ]; then
        rm -rf "$ZIG_ZIG_DIR"
    fi
    mkdir "$ZIG_ZIG_DIR"

    # Get lastest tarball from here: https://ziglang.org/download/
    local ZIG_TAR_LOCATION="https://ziglang.org/builds/zig-linux-x86_64-0.10.0-dev.1862+a7c05c06b.tar.xz"
    local ZIG_TAR_FILE
    ZIG_TAR_FILE="$(basename $ZIG_TAR_LOCATION)"

    curl --output-dir "$ZIG_ZIG_DIR" -sS -O "$ZIG_TAR_LOCATION"
    check $?

    tar --strip-components=1 -xvf "$ZIG_ZIG_DIR/$ZIG_TAR_FILE" --directory "$ZIG_ZIG_DIR"
    check $?

    rm -f "$ZIG_ZIG_DIR/$ZIG_TAR_FILE"
    check $?
}

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
    else
        echo "zig is not on your path, try sourcing '.zshenv' or just restart this terminal"
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Update Zig" "Update ZLS" "Quit")
    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                update_zig
                hr
                break
                ;;
            2)
                update_zls
                hr
                break
                ;;
            3) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done
done
