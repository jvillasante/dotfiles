# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export DISABLE_AUTO_TITLE='true'

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=$HOME/.oh-my-zsh.d

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="jv-custom"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(vi-mode git common-aliases)

# User configuration

# Change umask to make directory sharing easier
umask 0022

# Exports
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"           # $EDITOR should open in terminal
export VISUAL="emacsclient -c -a emacs"  # $VISUAL opens in GUI with non-daemon as alternate
export TERM=xterm-256color

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#

alias ls='ls --color'
alias ll='ls -AlFh --color'
alias em="emacsclient -c -n -a ''"      # opens the GUI
alias emt="emacsclient -t -a ''"        # used to be "emacs -nw"
alias semt="sudo emacsclient -t -a ''"  # used to be "sudo emacs -nw"
alias r="ranger"
alias dotfiles="ls -a | grep '^\.' | grep --invert-match '\.DS_Store\|\.$'"

#git
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gl='git pull --prune'
alias grm="git status | grep deleted | awk '{\$1=\$2=\"\"; print \$0}' | perl -pe 's/^[ \t]*//' | sed 's/ /\\\\ /g' | xargs git rm"

# gpg is not gpg2 if installed
if type gpg2 >/dev/null 2>/dev/null; then
    alias gpg='gpg2'
fi

# vim
if type nvim > /dev/null 2>&1; then
  alias vim='nvim'
fi
alias vi=vim

#
# Mac Stuff
#
if [[ "$(uname -s)" == "Darwin" ]]; then
    # use gls on mac
    alias ls="/usr/local/bin/gls --color"
    alias ll='/usr/local/bin/gls -AlFh --color'

    # Toggle hidden files on mac
    alias show_hidden_files='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
    alias hide_hidden_files='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'

    # Enable font smoothing
    defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO
fi

#
# vterm (https://github.com/akermu/emacs-libvterm)
#
vterm_printf() {
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

if [[ "$(uname -s)" == "Darwin" ]]; then
    vterm_prompt_end() {
        vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
    }
    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi

#
# Tmux
#
if type tmux >/dev/null 2>/dev/null; then
    alias tat='tmux new-session -As $(basename "$PWD" | tr . -)' # will attach if session exists, or create a new session
    alias tmuxsrc="tmux source-file ~/.tmux.conf"
    alias tmuxkillall="tmux ls | cut -d : -f 1 | xargs -I {} tmux kill-session -t {}" # tmux kill all sessions

    # Makes creating a new tmux session (with a specific name) easier
    function tmux_open() {
        tmux attach -t $1
    }

    # Makes creating a new tmux session (with a specific name) easier
    function tmux_new() {
        tmux new -s $1
    }

    # Makes deleting a tmux session easier
    function tmux_kill() {
        tmux kill-session -t $1
    }
fi

#
# fzf
#
if type fzf >/dev/null 2>/dev/null; then
    [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

    # Use ~~ as the trigger sequence instead of the default **
    # export FZF_COMPLETION_TRIGGER='~~'

    # Options to fzf command
    export FZF_DEFAULT_OPTS='--height 80% --layout=reverse --border --info=inline'
    export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

    # Use fd (https://github.com/sharkdp/fd) instead of the default find
    # command for listing path candidates.
    # - The first argument to the function ($1) is the base path to start traversal
    # - See the source code (completion.{bash,zsh}) for the details.
    _fzf_compgen_path() {
        fd --hidden --follow --exclude ".git" . "$1"
    }

    # Use fd to generate the list for directory completion
    _fzf_compgen_dir() {
        fd --type d --hidden --follow --exclude ".git" . "$1"
    }

    # interactive pgrep
    function fzf_grep() {
        if [[ $1 == "" ]]; then
            FZF=fzf
        else
            FZF="fzf --query $1"
        fi
        ps aux | eval $FZF | awk '{ print $2 }'
    }

    # interactive pkill
    function fzf_kill() {
        if [[ $1 =~ "^-" ]]; then
            QUERY=""            # options only
        else
            QUERY=$1            # with a query
            [[ $# > 0 ]] && shift
        fi
        fzf_grep $QUERY | xargs kill $*
    }

    # switch to a project
    function fzf_proj() {
        cd $(find ~/Workspace/Code/ -maxdepth 2 -type d | fzf)
    }

    # change to an arbitrary subdirectory
    function fzf_cd() {
        cd $(find . -type d | fzf)
    }

    # fuzzy match current directory contents
    function fzf_fuzz() {
        search_term=$1
        find . -wholename \*$search_term\* -not -path './.*/*' | fzf
    }

    # switch between git branches
    function fzf_checkout() {
        git checkout $(git branch | cut -c 3- | fzf)
    }

    # find and edit file containing a specific word (vim)
    function fzf_vim() {
        vim $(ag -l $1 | fzf)
    }

    # find and edit file containing a specific word (emacs)
    function fzf_emacs() {
        emt $(ag -l $1 | fzf)
    }

    # run a command from the history
    function fzf_hist() {
        $(history | cut -c8- | sort -u | fzf)
    }
fi
