#!/usr/bin/env bash

# my_functions.sh - A library of reusable bash functions.
#
# This file should be sourced by other scripts, not executed directly.

# Only apply options if this file hasn't been sourced before,
# and if it's being sourced (not executed directly).
if [[ "${BASH_SOURCE[0]}" != "${0}" ]]; then
    # Exit immediately if a command exits with a non-zero status.
    set -o errexit

    # Treat unset variables as an error.
    set -o nounset
fi

# Logs a message to standard error
# Usage: log_error "Something went wrong"
log_error() {
    echo "[ERROR] $*" >&2
}

# Logs a message to standard out
# Usage: log_info "Something went wrong"
log_info() {
    echo "[INFO] $*" >&1
}

# A function to check if a required command exists
# Usage: require_command "curl" || log_error "curl is required"
require_command() {
    command -v "$1" >/dev/null 2>&1
}

# Find current OS based on `uname`
# Usage: CURRENT_OS=$(find_os)
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

# Find current dotfiles directory based on current host
# Usage: DOTFILES_DIR=$(find_dotfiles)
find_dotfiles() {
    local DOTFILES_DIR
    DOTFILES_DIR="NOT_FOUND"
    [ -d "$HOME"/Workspace/Public/dotfiles ] && DOTFILES_DIR="$HOME"/Workspace/Public/dotfiles
    echo "$DOTFILES_DIR"
}

# Encapsulates the mess that DE detection was, is, or will ever be...
# Without arguments, check if in a Desktop Environment at all
# Subshell is intentional so we don't have to save/restore IFS
# Case-insensitive comparison
# Usage:
#   for de in "" Ubuntu Gnome KDE Unity; do
#       if is_desktop_environment "$de"; then
#           echo "Yes, this is ${de:-a DE}"
#       else
#           echo "No, this is not ${de:-a DE}"
#       fi
#   done
is_desktop_environment() (
    local de=${1:-}
    local DEs=${XDG_CURRENT_DESKTOP:-}

    # Shortcut: If de is empty, check if empty DEs
    if [[ -z "$de" ]]; then if [[ "$DEs" ]]; then return; else return 1; fi; fi

    # Lowercase both
    de=${de,,}
                DEs=${DEs,,}

    # Check de against each DEs component
    IFS=:
           for DE in $DEs; do if [[ "$de" == "$DE" ]]; then return; fi; done

    # Not found
    return 1
)

# Check helper for return values
# Usage: check $?
check() {
    if [ "$1" -ne 0 ]; then
        log_error "This is an error, do something else... We don't know what's wrong here!!!"
        exit "$1"
    fi
}

# Ask helper (https://djm.me/ask)
# Usage: if ask "Do you want to continue?"; then ... fi
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
        read -r reply < /dev/tty

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

# Print a horizontal line followed by a new line
# Usage: hr
hr() {
    local start=$'\e(0' end=$'\e(B' line='qqqqqqqqqqqqqqqq'
    local cols=${COLUMNS:-$(tput cols)}
    while ((${#line} < cols)); do line+="$line"; done
    printf '%s%s%s\n' "$start" "${line:0:cols}" "$end"

    # Print also a new line
    echo
}

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
