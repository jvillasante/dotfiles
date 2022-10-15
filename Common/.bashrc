#!/usr/bin/env bash

# Do nothing if not running interactively
[[ "$-" != *i* ]] && return

# Source in global settings
[ -f /etc/bashrc ] && source /etc/bashrc

# Custom
source "${HOME}/.config/shell/system/env"
source "${HOME}/.config/shell/system/custom"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/jvillasante/anaconda3/bin/conda' 'shell.bash' 'hook' 2>/dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/jvillasante/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/jvillasante/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/jvillasante/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
