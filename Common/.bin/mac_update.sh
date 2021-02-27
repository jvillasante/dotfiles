#!/bin/sh

. $(dirname "$0")/common.sh

CURRENT_OS=$(find_os)
if [ ! $CURRENT_OS = "OSX" ]; then
    echo "$CURRENT_OS :: Not on MacOS"
    exit 1
fi

# Check developer tools
xcode-select --install
echo ""

# Check if brew is installed and perform maintainance
if hash brew 2>/dev/null; then
    if ask "Do you want to update brew?"; then
        brew update
        brew upgrade
        brew upgrade --cask --greedy
        mas upgrade
    else
        echo "Not updating brew."
    fi

    if ask "Do you want to run brew doctor?"; then
        brew doctor
        brew missing
    else
        echo "Not runing brew doctor."
    fi

    if ask "Do you want to cleanup brew?"; then
        brew cleanup -s
    else
        echo "Not cleaning up brew."
    fi
fi
