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

# Check if rust-analyzer is installed and perform updates
if hash rust-analyzer 2>/dev/null; then
    if ask "Do you want to update rust-analyzer?"; then
        CURRENT_ENV=$(find_env)
        RUST_ANALYZER_DIR="NOT_FOUND"
        if [ $CURRENT_ENV = "HOME" ]; then
            RUST_ANALYZER_DIR=~/Workspace/Software/rust/rust-analyzer 
        elif [ $CURRENT_ENV = "WORK" ]; then
            RUST_ANALYZER_DIR=~/Workspace/Software/rust/rust-analyzer
        fi

        if [ -d $RUST_ANALYZER_DIR ]; then
            (cd $RUST_ANALYZER_DIR && git pull && cargo xtask install --server)
        fi
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
