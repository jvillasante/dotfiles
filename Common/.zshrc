# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export DISABLE_AUTO_TITLE='true'
export DISABLE_MAGIC_FUNCTIONS='true'

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=$HOME/.oh-my-zsh.d

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
# ZSH_THEME="jv-custom"
eval "$(starship init zsh)" # Using starship theme

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

# history control
HISTCONTROL=ignoredups:ignorespace

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
if [[ "$(uname -s)" == "Darwin" ]]; then
    plugins=(git common-aliases osx)
else
    plugins=(git common-aliases)
fi

# User configuration

# Change umask to make directory sharing easier
umask 0022

# Exports
export ALTERNATE_EDITOR=""
export VISUAL="emacsclient -c -n -a ''"  # $VISUAL opens in GUI with non-daemon as alternate
export EDITOR="emacsclient -t -a ''"     # $EDITOR should open in terminal
export TERM=xterm-256color

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
alias ssh="TERM=xterm-256color ssh"
export SSH_KEY_PATH="~/.ssh/dsa_id"

# emacs
if type emacs >/dev/null 2>/dev/null; then
    alias em="emacsclient -c -n -a ''"      # opens the GUI
    alias emt="emacsclient -t -a ''"        # used to be "emacs -nw"
    alias semt="sudo emacsclient -t -a ''"  # used to be "sudo emacs -nw"

    e()      { emacsclient -t "$@" }
    ediff()  { emacsclient -t -a '' --eval "(ediff-files \"$1\" \"$2\")"; }
    edired() { emacsclient -t -a '' --eval "(progn (dired \".\"))"; }
    ekill()  { emacsclient --eval '(kill-emacs)'; }
fi

# utils
alias ls='ls --color'
alias ll='ls -AlFh --color'
alias cp="cp -iv"
alias mv="mv -iv"
alias tree="tree --dirsfirst -a -I 'node_modules|.git'"
alias ag='ag --hidden --ignore node_modules --ignore .git'

# exa
if type exa >/dev/null 2>/dev/null; then
    alias ls='exa --group-directories-first --color=auto'
    alias ll='exa -agFlh --group-directories-first --color=auto'
fi

# ripgrep
if type rg >/dev/null 2>/dev/null; then
    alias grep='rg -S --color=auto'
fi

if type bat >/dev/null 2>/dev/null; then
    alias bat='bat --theme ansi'
    # alias cat='bat --theme ansi'
fi

# gpg is not gpg2 if installed
if type gpg2 >/dev/null 2>/dev/null; then
    alias gpg='gpg2'
fi

# vim
if type nvim > /dev/null 2>&1; then
    alias vim='nvim'
fi
alias vi=vim

# surf: these options are very opinionated, disabling images, javascript, etc.
if type surf > /dev/null 2>&1; then
	alias surf="surf -giKMnps"
fi

# git
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gl='git pull --prune'
alias grm="git status | grep deleted | awk '{\$1=\$2=\"\"; print \$0}' | perl -pe 's/^[ \t]*//' | sed 's/ /\\\\ /g' | xargs git rm"

#
# Mac Stuff
#
if [[ "$(uname -s)" == "Darwin" ]]; then
    # Toggle hidden files on mac
    alias show_hidden_files='defaults write com.apple.finder AppleShowAllFiles YES; killall Finder /System/Library/CoreServices/Finder.app'
    alias hide_hidden_files='defaults write com.apple.finder AppleShowAllFiles NO; killall Finder /System/Library/CoreServices/Finder.app'

    # Enable font smoothing
    defaults write -g CGFontRenderingFontSmoothingDisabled -bool NO

    # Symlink workspace volume
    if [ ! -d "~/Workspace" ]; then
        ln -nfs /Volumes/Workspace/ ~/Workspace
    fi
fi

#
# Linux Stuff
# 
if [ $(uname -s) = 'Linux' ]; then
    alias open='xdg-open';
    alias alacritty='env WINIT_UNIX_BACKEND=x11 alacritty'

    # Known bug with gpg on Fedora
    function gpg_pcscd_restart() {
        sudo systemctl restart pcscd
    }
fi

#
# Emacs vterm (https://github.com/akermu/emacs-libvterm)
#
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
    source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
fi

#
# Fuzzy Finder (https://github.com/lotabout/skim)
#
source "${HOME}/.config/shell/skim"
