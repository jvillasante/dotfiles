# Setup fzf
# ---------
if [[ ! "$PATH" == */home/jvillasante/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/jvillasante/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/jvillasante/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/jvillasante/.fzf/shell/key-bindings.zsh"
