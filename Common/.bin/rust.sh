#!/usr/bin/env bash

. "$(dirname "$0")/common.sh"

function usage() {
    echo "Usage:"
    echo "    $0 help:"
    echo "        Show this help message"
    echo "    $0 update:"
    echo "        Update both rust and rust_analyzer"
    echo "    $0 package [show|update]:"
    echo "        Show or update global packages installed with cargo"
    echo "    $0 cargo [arbitrary cargo command]:"
    echo "        Run arbitrary cargo command"
    echo
    echo " e.g: $0 package show"
    exit "$1"
}

update_rust() {
    if hash rustup 2>/dev/null; then
        rustup self update
        check $?

        rustup update
        check $?
    fi
}

update_rust_analyzer() {
    if [ ! -d "${CARGO_HOME:-~/.cargo}/bin" ]; then
        echo ">>> cargo is not installed, exiting..."
        exit 1
    fi

    local CURRENT_OS
    CURRENT_OS="$(find_os)"
    if [ "$CURRENT_OS" = "OSX" ]; then
        curl -LsSf https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-apple-darwin.gz |
            gunzip -c - >"${CARGO_HOME:-~/.cargo}/bin/rust-analyzer"
    elif [ "$CURRENT_OS" = "LINUX" ]; then
        curl -LsSf https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz |
            gunzip -c - >"${CARGO_HOME:-~/.cargo}/bin/rust-analyzer"
    fi

    if hash "${CARGO_HOME:-~/.cargo}/bin/rust-analyzer" 2>/dev/null; then
        chmod +x "${CARGO_HOME:-~/.cargo}/bin/rust-analyzer"
    else
        echo ">>> ${CARGO_HOME:-~/.cargo}/bin/rust-analyzer does not exit, exiting..."
        exit 1
    fi
}

function do_update() {
    update_rust
    update_rust_analyzer
}

function show_global_packages() {
    if hash cargo 2>/dev/null; then
        echo "Global Packages Installed:"
        cargo install-update -al
        check $?

        echo "Run the following to update:
        $ cargo install-update -a                # Update all installed packages.
        $ cargo install-update crate1 crate2 ... # check for newer versions and update selected packages, will not install new packages."
    fi
}

function update_global_packages() {
    if hash cargo 2>/dev/null; then
        cargo install-update -a
        check $?
    fi
}

function do_package() {
    case $1 in
        show)
            show_global_packages
            ;;
        update)
            update_global_packages
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
    update)
        [ "$nargs" -eq 1 ] || usage 1
        do_update "$@"
        ;;
    package)
        [ "$nargs" -eq 2 ] || usage 1
        do_package "$@"
        ;;
    cargo)
        [ "$nargs" -lt 2 ] && usage 1
        if hash cargo 2>/dev/null; then
            cargo "$@"
        else
            echo "cargo not installed, exiting..."
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
