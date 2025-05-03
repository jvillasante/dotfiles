#!/usr/bin/env bash

# Do nothing if not running interactively
[[ "$-" != *i* ]] && return

# Source in global settings
# shellcheck source=/dev/null
[ -f /etc/bashrc ] && source /etc/bashrc

# Custom
# shellcheck source=/dev/null
source "${HOME}/.config/shell/system/custom"

# Env
# shellcheck source=/dev/null
source "${HOME}/.config/shell/system/env"
