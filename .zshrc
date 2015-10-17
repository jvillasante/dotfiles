# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Change umask to make directory sharing easier
umask 0022

# Exports {{{
export ZSH=$HOME/.oh-my-zsh          # Path to your oh-my-zsh installation.
export ZSH_THEME="robbyrussell"      # Set name of the theme to load.
# export EDITOR=/usr/local/bin/vim     # Default editor
export EDITOR="emacsclient -c"
export ALTERNATE_EDITOR=""
export VISUAL=$EDITOR
export LANG=en_US.UTF-8              # You may need to manually set your language environment
export SSH_KEY_PATH="~/.ssh/dsa_id"  # ssh
export TERM=xterm-256color
export PATH="$HOME/bin:$PATH"
# }}}

# oh-my-zsh {{{
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git git-extras gitignore golang)
COMPLETION_WAITING_DOTS="true"
DISABLE_UNTRACKED_FILES_DIRTY="true"
DISABLE_AUTO_TITLE="true"

# User configuration
source $ZSH/oh-my-zsh.sh
zstyle :omz:plugins:ssh-agent agent-forwarding on
# }}}

# Aliases {{{
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias ls='ls -lFh --color=auto'
alias grep='grep --color=auto'
alias vi=vim                    # vi is now vim
alias gvim='gvim 2>/dev/null'   # Discard gvim starting warnings
# alias mux='tmuxifier'
alias em="emacsclient -c -n"
alias r="source ~/.zshrc"
alias tat='tmux new-session -As $(basename "$PWD" | tr . -)' # will attach if session exists, or create a new session
alias tmuxsrc="tmux source-file ~/.tmux.conf"
alias tmuxkillall="tmux ls | cut -d : -f 1 | xargs -I {} tmux kill-session -t {}" # tmux kill all sessions
alias ct="ctags -R --exclude=.git --exclude=node_modules"
alias dotfiles="ls -a | grep '^\.' | grep --invert-match '\.DS_Store\|\.$'"

#git
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gl='git pull --prune'
alias grm="git status | grep deleted | awk '{\$1=\$2=\"\"; print \$0}' | perl -pe 's/^[ \t]*//' | sed 's/ /\\\\ /g' | xargs git rm"
# }}}

# Tmux {{{
# Makes creating a new tmux session (with a specific name) easier
function tmuxopen() {
  tmux attach -t $1
}

# Makes creating a new tmux session (with a specific name) easier
function tmuxnew() {
  tmux new -s $1
}

# Makes deleting a tmux session easier
function tmuxkill() {
  tmux kill-session -t $1
}

# percol
function exists { which $1 &> /dev/null }

# history search (C-r)
if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi

# interactive pgrep
function ppgrep() {
    if [[ $1 == "" ]]; then
        PERCOL=percol
    else
        PERCOL="percol --query $1"
    fi
    ps aux | eval $PERCOL | awk '{ print $2 }'
}

#interactive pkill
function ppkill() {
    if [[ $1 =~ "^-" ]]; then
        QUERY=""            # options only
    else
        QUERY=$1            # with a query
        [[ $# > 0 ]] && shift
    fi
    ppgrep $QUERY | xargs kill $*
}

# switch to a project
function ppproj() {
    cd $(find ~/Hacking/workspace/ -maxdepth 1 -type d | percol)
}

# change to an arbitrary subdirectory
function ppcd() {
    cd $(find . -type d | percol)
}

# fuzzy match current directory contents
function ppfuzz() {
    search_term=$1
    find . -wholename \*$search_term\* -not -path './.*/*' | percol
}

# switch between git branches
function ppcheckout() {
    git checkout $(git branch | cut -c 3- | percol)
}

# find and edit file containing a specific word
function ppvim() {
    vim $(ag -l $1 | percol)
}

# run a command from the history
function pphist() {
    $(history | cut -c8- | sort -u | percol)
}
#}}}

# tmuxifier
# export PATH="$HOME/.tmuxifier/bin:$PATH"
# export TMUXIFIER_TMUX_OPTS="-2"
# eval "$(tmuxifier init -)"
# }}}

# java stuff {{{
unset JAVA_TOOL_OPTIONS
# }}}

# node stuff
export NVM_DIR="/home/jvillasante/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# go language stuff
[[ -s "/home/jvillasante/.gvm/scripts/gvm" ]] && source "/home/jvillasante/.gvm/scripts/gvm"
export GOPATH=$HOME/Hacking/workspace/go
export PATH="$GOPATH/bin:$PATH"

# android
export PATH="$HOME/Android/Sdk/tools:$PATH"
export PATH="$HOME/Android/Sdk/platform-tools:$PATH"
