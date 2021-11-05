#!/bin/bash

#
# Find current shell
# Call it like: CURRENT_SHELL=$(find_current_shell)
#
find_current_shell() {
    local CURRENT_SHELL="unknown"
    if [[ -n ${ZSH_VERSION} ]]; then
        CURRENT_SHELL="zsh $ZSH_VERSION"
    elif [[ -n $BASH_VERSION ]]; then
        CURRENT_SHELL="bash $BASH_VERSION"
    fi
    echo "$CURRENT_SHELL"
}

#
# Find current host based on `hostname`
# Call it like: CURRENT_HOST=$(find_host)
#
find_host() {
    local CURRENT_HOST="UNSUPPORTED"
    local HOST="$(hostname)"
    case "$HOST" in
        "Julios-MacBook-Pro.local")
            {
                CURRENT_HOST="$HOST"
            }
            ;;
        "fedora-latitude-7480")
            {
                CURRENT_HOST="$HOST"
            }
            ;;
        "fedora-thinkpad")
            {
                CURRENT_HOST="$HOST"
            }
            ;;
        *)
            {
                echo ">> Unsupported HOST: '$HOST', exiting..."
                exit 1
            }
            ;;
    esac
    echo "$CURRENT_HOST"
}

#
# Find current OS based on `uname`
# Call it like: CURRENT_OS=$(find_os)
#
find_os() {
    local CURRENT_OS="UNSUPPORTED"
    local PLATFORM="$(uname -s)"
    case "$PLATFORM" in
        "Darwin")
            {
                CURRENT_OS="OSX"
            }
            ;;
        "Linux")
            {
                CURRENT_OS="LINUX"
            }
            ;;
        *)
            {
                echo ">> Unsupported OS: '$PLATFORM', exiting..."
                exit 1
            }
            ;;
    esac
    echo "$CURRENT_OS"
}

#
# Find current dotfiles directory based on current host
# Call it like: DOTFILES_DIR=$(find_dotfiles)
#
find_dotfiles() {
    local DOTFILES_DIR="NOT_FOUND"
    local CURRENT_HOST="$(find_host)"
    if [ "$CURRENT_HOST" = "Julios-MacBook-Pro.local" ]; then
        DOTFILES_DIR="${HOME}/Workspace/Personal/dotfiles" # dotfiles directory
    elif [ "$CURRENT_HOST" = "fedora-latitude-7480" ]; then
        DOTFILES_DIR="${HOME}/Workspace/Personal/dotfiles" # dotfiles directory
    elif [ "$CURRENT_HOST" = "fedora-thinkpad" ]; then
        DOTFILES_DIR="${HOME}/Workspace/Personal/dotfiles" # dotfiles directory
    fi
    echo "$DOTFILES_DIR"
}

#
# Check helper for return values
#
check() {
    if [ "$1" -ne 0 ]; then
        echo ""
        echo ">>> This is an error, do something else... We don't know what's wrong here!!!"
        echo ""
        exit "$1"
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
        read -r reply </dev/tty

        # Default?
        if [ -z "$reply" ]; then
            reply=$default
        fi

        # Check if the reply is valid
        case "$reply" in
            Y* | y*) return 0 ;;
            N* | n*) return 1 ;;
        esac
    done
}
