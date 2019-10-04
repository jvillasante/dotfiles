#!/bin/sh

. $(dirname "$0")/common.sh

# Check if apt is installed and perform updates
if hash apt 2>/dev/null; then
    if ask "Do you want to update ubuntu?"; then
        sudo apt update && sudo apt upgrade && sudo apt dist-upgrade
        check $?
    else
        echo "Not updating ubuntu."
    fi

    if ask "Do you want to clean up ubuntu?"; then
        sudo apt autoremove && sudo apt autoclean
        check $?
    else
        echo "Not cleaning up ubuntu."
    fi
fi