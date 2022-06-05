#!/usr/bin/env bash

. "$(dirname "$0")/common.sh"

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

update_nextest() {
    if [ ! -d "${CARGO_HOME:-~/.cargo}/bin" ]; then
        echo ">>> cargo is not installed, exiting..."
        exit 1
    fi

    local CURRENT_OS
    CURRENT_OS="$(find_os)"
    if [ "$CURRENT_OS" = "OSX" ]; then
        curl -LsSf https://get.nexte.st/latest/mac | tar zxf - -C "${CARGO_HOME:-~/.cargo}/bin"
    elif [ "$CURRENT_OS" = "LINUX" ]; then
        curl -LsSf https://get.nexte.st/latest/linux | tar zxf - -C "${CARGO_HOME:-~/.cargo}/bin"
    fi
}

update_all() {
    update_rust
    update_rust_analyzer
    update_nextest
}

show_global_packages() {
    if hash cargo 2>/dev/null; then
        echo "Global Packages Installed:"
        cargo install-update -al
        check $?

        echo "Run the following to update:
        $ cargo install-update -a                # Update all installed packages.
        $ cargo install-update crate1 crate2 ... # check for newer versions and update selected packages, will not install new packages."
    fi
}

update_global_packages() {
    if hash cargo 2>/dev/null; then
        cargo install-update -a
        check $?
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Update Rust" "Update Rust Analyzer" "Update Nextest" "Update Rust, Rust Analyzer and Nextest"
        "Show Global Packages Installed" "Update Global Packages Installed"
        "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                update_rust
                hr
                break
                ;;
            2)
                update_rust_analyzer
                hr
                break
                ;;
            3)
                update_nextest
                hr
                break
                ;;
            4)
                update_all
                hr
                break
                ;;
            5)
                show_global_packages
                hr
                break
                ;;
            6)
                update_global_packages
                hr
                break
                ;;
            7) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done
done
