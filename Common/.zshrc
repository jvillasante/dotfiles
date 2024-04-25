#!/usr/bin/env bash

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 022

# Do nothing if not running interactively
[[ "$-" != *i* ]] && return

# Source in global settings
# shellcheck source=/dev/null
[ -f /etc/zshrc ] && source /etc/zshrc

# Source our custom files
# shellcheck source=/dev/null
source "${HOME}/.config/shell/system/custom.sh"
