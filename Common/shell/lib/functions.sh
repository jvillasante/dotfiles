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

# --- End of Script ---
# Ensure no unexpected output is produced when sourcing.
return 0
