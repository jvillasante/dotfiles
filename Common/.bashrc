#!/usr/bin/env bash

# Do nothing if not running interactively
[[ "$-" != *i* ]] && return

# Source in global settings
[ -f /etc/bashrc ] && source /etc/bashrc

# Custom
source "${HOME}/.config/shell/system/env"
source "${HOME}/.config/shell/system/custom"
