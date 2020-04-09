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
plugins=(vi-mode osx git common-aliases)

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

alias ls='ls -AlFh --color'
alias em="emacsclient -c -a emacs"   # opens the GUI with alternate non-daemon
alias emt="emacsclient -t"           # used to be "emacs -nw"
alias semt="sudo emacsclient -t"     # used to be "sudo emacs -nw"
alias r="source ~/.zshrc"
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
    alias ls="/usr/local/bin/gls -AlFh --color"

    # Toggle hidden files on mac
    alias show_hidden_files='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
    alias hide_hidden_files='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'
fi

#
# iTerm
#
# if [ "$TERM_PROGRAM" = 'iTerm.app' ]; then
#     test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

#     iterm_emit() {
#         local template="\e]${1}\007"
#         shift

#         if [[ -n "$TMUX" || "$TERM" = tmux* ]]; then
#             template="\ePtmux;\e${template}\e\\"
#         fi
#         printf "$template" "$@"
#     }

#     iterm_profile() {
#         iterm_emit '1337;SetProfile=%s' "$1"

#         if [[ -n "$TMUX" || "$TERM" = tmux* ]]; then
#             if [ "$1" = 'light' ]; then
#                 tmux source-file "$HOME/.tmux_light.conf"
#             else
#                 tmux source-file "$HOME/.tmux_dark.conf"
#             fi
#         fi
#     }

#     iterm_user_var() {
#         iterm_emit '1337;SetUserVar=%s=%s' "$1" "$(echo -n "$2" | base64)"
#     }

#     iterm_badge_format() {
#         iterm_emit '1337;SetBadgeFormat=%s' "$(echo -n "$1" | base64)"
#     }

#     iterm_highlight_cursor() {
#         local bool="${1:-true}"
#         iterm_emit '1337;HighlightCursorLine=%s' "$bool"
#     }

#     iterm_annotation() {
#         if [ -z "$TMUX" ]; then
#             # Doesn't work in TMUX
#             iterm_emit '1337;AddAnnotation=%s' "${1:-annotation}"
#         fi
#     }

#     iterm_clear_scrollback() {
#         iterm_emit '1337;ClearScrollback'
#     }

#     iterm_get_attention() {
#         iterm_emit '1337;RequestAttention=true'
#     }

#     iterm_steal_focus() {
#         iterm_emit '1337;StealFocus'
#     }

#     iterm_send_cwd() {
#         local cwd="${1:-$PWD}"
#         iterm_emit '1337;CurrentDir=%s' "$cwd"
#     }

#     [[ -z $chpwd_functions ]] && chpwd_functions=()
#     chpwd_functions=($chpwd_functions iterm_send_cwd)
# else
#     ITERM_SHELL_INTEGRATION_INSTALLED=no
#     ITERM2_SHOULD_DECORATE_PROMPT=0
# fi

#
# Tmux
#
if type tmux >/dev/null 2>/dev/null; then
    alias tat='tmux new-session -As $(basename "$PWD" | tr . -)' # will attach if session exists, or create a new session
    alias tmuxsrc="tmux source-file ~/.tmux.conf"
    alias tmuxkillall="tmux ls | cut -d : -f 1 | xargs -I {} tmux kill-session -t {}" # tmux kill all sessions

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
fi

#
# percol
#
if type percol >/dev/null 2>/dev/null; then
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
        cd $(find ~/Workspace/Projects/ -maxdepth 2 -type d | percol)
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
fi
