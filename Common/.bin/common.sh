#!/bin/bash

#
# Find current environment based on `hostname`
# Call it like: CURRENT_ENV=$(find_env)
#
find_env() {
    local CURRENT_ENV="UNSUPPORTED"
    local HOST=$(hostname)
    case "$HOST" in
        "Julios-MacBook-Pro.local")
            {
                CURRENT_ENV="HOME"
            } ;;
        "Julios-MBP")
            {
                CURRENT_ENV="HOME"
            } ;;
        "jvillasante-Latitude-7480")
            {
                CURRENT_ENV="WORK"
            } ;;
        *)
            {
                echo ">> Unsupported ENV: '$HOST', exiting..."
                exit 1
            } ;;
    esac
    echo "$CURRENT_ENV"
}

#
# Find current OS based on `uname`
# Call it like: CURRENT_OS=$(find_os)
#
find_os() {
    local CURRENT_OS="UNSUPPORTED"
    local PLATFORM=$(uname -s)
    case "$PLATFORM" in
        "Darwin")
            {
                CURRENT_OS="OSX"
            } ;;
        "Linux")
            {
                CURRENT_OS="LINUX"
            } ;;
        *)
            {
                echo ">> Unsupported OS: '$PLATFORM', exiting..."
                exit 1
            } ;;
    esac
    echo "$CURRENT_OS"
}

#
# Find current dotfiles directory based on current env
# Call it like: DOTFILES_DIR=$(find_dotfiles)
#
find_dotfiles() {
    local DOTFILES_DIR="NOT_FOUND"
    local CURRENT_ENV=$(find_env)
    if [ $CURRENT_ENV = "HOME" ]; then
        DOTFILES_DIR=${HOME}/Workspace/Others/dotfiles     # dotfiles directory
    elif [ $CURRENT_ENV = "WORK" ]; then
        DOTFILES_DIR=${HOME}/Workspace/Others/dotfiles     # dotfiles directory
    fi
    echo "$DOTFILES_DIR"
}

#
# Check helper for return values
#
check() {
    if [ "$1" != 0 ]; then
        echo ""
        echo ">>> This is an error, do something else... We don't know what's wrong here!!!"
        echo ""
        exit $1
    fi
}

#
# Ask helper (https://djm.me/ask)
# 
ask() {
    local prompt default reply

    while true; do
        if [ "${2:-}" = "Y" ]; then
            prompt="Y/n"
            default=Y
        elif [ "${2:-}" = "N" ]; then
            prompt="y/N"
            default=N
        else
            prompt="y/n"
            default=
        fi

        # Ask the question (not using "read -p" as it uses stderr not stdout)
        echo "$1 [$prompt] "

        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read reply </dev/tty

        # Default?
        if [ -z "$reply" ]; then
            reply=$default
        fi

        # Check if the reply is valid
        case "$reply" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac
    done
}
