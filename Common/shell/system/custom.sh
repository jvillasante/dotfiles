# tty
stty -ixon    # Disable flow control, i.e. ^s "freeze screen" and ^q resume
stty erase ^? # kill ^u intr ^c eof ^d stop ^s
set -o emacs

# term
export TERM=xterm-256color
export COLORTERM=truecolor

# You may need to manually set your language environment
LC_ALL=en_US.UTF-8 && export LC_ALL
LANG=en_US.UTF-8 && export LANG
LANGUAGE=en_US.UTF-8 && export LANGUAGE

# XDG_* should be set
XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}" && export XDG_CACHE_HOME
XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}" && export XDG_CONFIG_HOME
XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}" && export XDG_DATA_HOME
XDG_LIB_HOME="${XDG_LIB_HOME:-$HOME/.local/lib}" && export XDG_LIB_HOME
XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}" && export XDG_STATE_HOME

# Don't put duplicate lines or liens starting with space int he history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

# Some commands to ignore
HISTIGNORE="(ls|cd|pwd|exit|fproj)*"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable programmable completion features
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        # shellcheck source=/dev/null
        source /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        # shellcheck source=/dev/null
        source /etc/bash_completion
    fi
fi

# PS1
eval "$(starship init bash)"

# Emacs please
if type emacs > /dev/null 2> /dev/null; then
    export ALTERNATE_EDITOR=""
    export VISUAL="emacsclient -c -a ''"   # $VISUAL opens in GUI and waits
    export EDITOR="$VISUAL"

    function e        { emacsclient -c -a '' --eval "(progn (select-frame-set-input-focus (selected-frame)) (find-file \"$1\"))"; }
    function et       { emacsclient -t -a '' --eval "(progn (find-file \"$1\"))"; }
    function ediff    { emacsclient -c -a '' --eval "(progn (select-frame-set-input-focus (selected-frame)) (ediff-files \"$1\" \"$2\"))"; }
    function edired   { emacsclient -c -a '' --eval "(progn (select-frame-set-input-focus (selected-frame)) (dired \"$1\"))"; }
    function emagit   { emacsclient -c -a '' --eval "(progn (select-frame-set-input-focus (selected-frame)) (magit-status \"$1\"))"; }
    function eww      { emacsclient -c -a '' --eval "(eww-browse-url \"$*\")"; }
    function ekill    { emacsclient --eval '(save-buffers-kill-emacs)'; }
    function estatus  { systemctl --user status emacs.service; }
    function edebug   { emacs --debug-init --init-directory="$HOME"/Workspace/Public/dotfiles/Common/emacs/personal/ "$@"; }
    function eminimal { emacs --init-directory="$HOME"/Workspace/Public/dotfiles/Common/emacs/minimal/ "$@"; }
fi

# vim
type vim > /dev/null 2>&1 && alias vi=vim
if type nvim > /dev/null 2>&1; then
    alias vim=nvim
    alias nvim='NVIM_APPNAME="nvim-personal/" nvim'
    # alias nvim='NVIM_APPNAME="nvim-lazyvim/" nvim'
    # alias nvim='NVIM_APPNAME="nvim-kickstart/" nvim'
fi

# gpg
if type gpg2 > /dev/null 2> /dev/null; then
    # gpg is not gpg2 if installed
    alias gpg='gpg2'
fi
GPG_TTY="$(tty)" && export GPG_TTY
SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)" && export SSH_AUTH_SOCK

# ssh
alias ssh="TERM=xterm-256color ssh"
SSH_KEY_PATH="$HOME/.ssh/dsa_id" && export SSH_KEY_PATH

# utils
alias hex="od -Ax -tx1z -v"
alias ls="ls --group-directories-first --color=auto"
alias ll="ls -AlFh --group-directories-first --color=auto"
alias cp="cp -iv"
alias mv="mv -iv"
alias rm="rm -I" # IMHO, much better than 'rm -i'
alias tree="tree --dirsfirst -a -I 'node_modules|.git'"
alias ag="ag --hidden --ignore node_modules --ignore .git"
alias reboot='systemctl reboot'
alias poweroff='systemctl poweroff'

# eza (ls replacement)
# if type eza > /dev/null 2> /dev/null; then
#     alias ls='eza --git --group-directories-first --color=automatic'
#     alias ll='eza -agFlh --git --group-directories-first --color=automatic'
# fi

# ripgrep
if type rg > /dev/null 2> /dev/null; then
    alias grep='rg -S --color=auto'
fi

# bat
if type bat > /dev/null 2> /dev/null; then
    alias bat='bat --theme ansi'
    # alias cat='bat --theme ansi'
fi

# fd-find
if type fdfind > /dev/null 2> /dev/null; then
    alias fd='fdfind'
fi

# surf: these options are very opinionated, disabling images, javascript, etc.
if type surf > /dev/null 2>&1; then
    alias surf="surf -giKMnps"
fi

# keepassxs : not using this yet
# if flatpak info org.keepassxc.KeePassXC >/dev/null 2>/dev/null; then
#     alias keepassxc="flatpak run org.keepassxc.KeePassXC"
# fi

#
# Mac Stuff
#
if [ "$(uname -s)" = "Darwin" ]; then
    # Toggle hidden files on mac
    alias show_hidden_files='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
    alias hide_hidden_files='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'

    # Enable font smoothing
    defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO

    # Symlink workspace volume
    if [ ! -d "$HOME/Workspace" ]; then
        ln -nfs /Volumes/Workspace/ ~/Workspace
    fi
fi

#
# Linux Stuff
#
if [ "$(uname -s)" = 'Linux' ]; then
    alias open='xdg-open'

    if [ -f /etc/fedora-release ]; then
        # Known bug with gpg on Fedora
        gpg_pcscd_restart() {
            sudo systemctl restart pcscd
        }
    fi
fi

#
# Tmux
#

if type tmux > /dev/null 2> /dev/null; then
    # A shortcut function for easy tmux navigation.
    #
    # tx ls will give a list of active tmux sessions.
    # tx {session_name} behaves differently depending on whether the user is inside a tmux session.
    # if outside tmux:
    #    tx {session_name} will attach to {session_name} if it exists, else will create {session_name}
    # if inside tmux:
    #    tx {session_name} will switch to {session_name} if it exists, else will print an error message.
    tx()  {
        SESSION_NAME=$1

        if [ "$SESSION_NAME" == "ls" ]; then
            # shellcheck disable=SC2033
            tmux ls
            return
        fi

        # If we are 'in' tmux, try to switch to the target session, fail if session doesn't exit.
        if [ -n "$TMUX" ]; then
            tmux switch-client -t "$SESSION_NAME"
            return $?
        fi

        # If we are outside of tmux, attach to the session if it exists, otherwise create it and attach.
        # shellcheck disable=SC2033
        if tmux ls | grep "^${SESSION_NAME}:" -q; then
            tmux attach -t "$SESSION_NAME"
        else
            tmux new -s "$SESSION_NAME"
        fi
    }

    alias txk='tmux kill-session -t'

    # Execute tmux (alacritty supports this natively)
    # if [ -x "$(command -v tmux)" ] && [ -n "${DISPLAY}" ] && [ -z "${TMUX}" ] && [ -z "$INSIDE_EMACS" ]; then
    #     ( tmux attach || tmux new -s Default > /dev/null 2>&1 ) && exit 0
    # fi
fi

#
# Fuzzy Finder
#
# shellcheck source=/dev/null
source "${HOME}/.config/shell/system/fzf.sh"
