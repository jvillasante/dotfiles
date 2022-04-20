#!/bin/bash

#
# Find current shell
# Call it like: CURRENT_SHELL=$(find_current_shell)
#
find_current_shell() {
    local CURRENT_SHELL
    CURRENT_SHELL="unknown"
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
    local CURRENT_HOST HOST
    CURRENT_HOST="UNSUPPORTED"
    HOST="$(hostname)"
    case "$HOST" in
        "Julios-MacBook-Pro.local")
            {
                CURRENT_HOST="$HOST"
            }
            ;;
        "fedora-xps-9710")
            {
                CURRENT_HOST="$HOST"
            }
            ;;
        "fedora-xps-9510")
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
    local CURRENT_OS PLATFORM
    CURRENT_OS="UNSUPPORTED"
    PLATFORM="$(uname -s)"
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
    local DOTFILES_DIR CURRENT_HOST
    DOTFILES_DIR="NOT_FOUND"
    CURRENT_HOST="$(find_host)"
    if [ "$CURRENT_HOST" = "Julios-MacBook-Pro.local" ]; then
        DOTFILES_DIR="${HOME}/Workspace/Public/dotfiles" # dotfiles directory
    elif [ "$CURRENT_HOST" = "fedora-xps-9710" ]; then
        DOTFILES_DIR="${HOME}/Workspace/Public/dotfiles" # dotfiles directory
    elif [ "$CURRENT_HOST" = "fedora-xps-9510" ]; then
        DOTFILES_DIR="${HOME}/Workspace/Public/dotfiles" # dotfiles directory
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

#
# Print a horizontal line followed by a new line
#
hr() {
    local start=$'\e(0' end=$'\e(B' line='qqqqqqqqqqqqqqqq'
    local cols=${COLUMNS:-$(tput cols)}
    while ((${#line} < cols)); do line+="$line"; done
    printf '%s%s%s\n' "$start" "${line:0:cols}" "$end"

    # Print also a new line
    echo
}

#
# Get latest github tarball
#   $1: GITHUB_ORGANIZATION (e.g. github user account)
#   $2: GITHUB_REPO (e.g. repo on which we want the latest tarball)
#   $3: OUTPUT (e.g. where the tarball will be decompressed)
#
# Example: https://github.com/arkenfox/user.js
#    github_latest arkenfox user.js "${HOME}/Downloads"
#    check $?
#
github_latest_release() {
    local GITHUB_ORGANIZATION
    local GITHUB_REPO
    local OUTPUT
    local LOCATION

    GITHUB_ORGANIZATION="$1"
    GITHUB_REPO="$2"
    OUTPUT="$3"
    LOCATION=$(curl -s https://api.github.com/repos/"$GITHUB_ORGANIZATION"/"$GITHUB_REPO"/releases/latest |
        grep "tarball_url" |
        awk '{ print $2 }' |
        sed 's/,$//' |
        sed 's/"//g')

    echo "Using latest ${GITHUB_ORGANIZATION}/${GITHUB_REPO} version ${LOCATION}"
    curl -L "$LOCATION" | tar zxf - -C "${OUTPUT}"
}
