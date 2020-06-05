#!/bin/sh

. $(dirname "$0")/common.sh

# Check if rustup is installed and perform updates
if hash rustup 2>/dev/null; then
    if ask "Do you want to update rust?"; then
        rustup self update
        check $?

        rustup update
        check $?
    else
        echo "Not updating rust."
    fi
fi

echo ""
echo "===================================================================================="
echo ""

if ask "Do you want to install/update rust-analyzer?"; then
    if [ ! -d ~/.bin/bin/ ]; then
        mkdir ~/.bin/bin/
    fi

    CURRENT_ENV=$(find_env)
    if [ $CURRENT_ENV = "HOME" ]; then
        curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-mac -o ~/.bin/bin/rust-analyzer
    elif [ $CURRENT_ENV = "WORK" ]; then
        curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.bin/bin/rust-analyzer
    fi

    if hash ~/.bin/bin/rust-analyzer 2>/dev/null; then
        chmod +x ~/.bin/bin/rust-analyzer
    fi
fi

echo ""
echo "===================================================================================="
echo ""

echo "Global Packages Installed:"
cargo install-update -al
check $?

echo "Run the following to update:
        $ cargo install-update -a                # Update all installed packages.
        $ cargo install-update crate1 crate2 ... # check for newer versions and update selected packages, will not install new packages."
