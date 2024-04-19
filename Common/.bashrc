#!/usr/bin/env bash

# Do nothing if not running interactively
[[ "$-" != *i* ]] && return

# Source in global settings
# shellcheck source=/dev/null
[ -f /etc/bashrc ] && source /etc/bashrc

# Env
# shellcheck source=/dev/null
source "${HOME}/.config/shell/system/env"

# Custom
# shellcheck source=/dev/null
source "${HOME}/.config/shell/system/custom"
