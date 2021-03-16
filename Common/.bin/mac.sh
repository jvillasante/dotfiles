#!/bin/bash

. $(dirname "$0")/common.sh

CURRENT_OS=$(find_os)
if [ ! $CURRENT_OS = "OSX" ]; then
    echo "$CURRENT_OS :: Not on MacOS"
    exit 1
fi

# Check developer tools (always!)
xcode-select --install 2>/dev/null

brew_update() {
    if hash brew 2>/dev/null; then
        brew update
        brew upgrade
        brew upgrade --cask --greedy
        mas upgrade
    fi
}

brew_doctor() {
    if hash brew 2>/dev/null; then
        echo "Running brew doctor..."
        brew doctor
        brew missing
    fi
}

brew_cleanup() {
    if hash brew 2>/dev/null; then
        echo "Running brew cleanup..."
        brew cleanup -s
    fi
}

brew_all() {
    brew_update
    brew_doctor
    brew_cleanup
}

while true; do
    PS3="Choose an option: "
    options=("Brew update" "Brew Doctor" "Brew cleanup" "Brew all" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1) brew_update; break ;;
            2) brew_doctor; break ;;
            3) brew_cleanup; break ;;
            4) brew_all; break ;;
            5) break 2 ;;
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
