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
ZSH_THEME="robbyrussell"
# ZSH_THEME="lambda"
# ZSH_THEME="jv-custom"

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
    plugins=(vi-mode git common-aliases osx)
else
    plugins=(vi-mode git common-aliases)
fi

# User configuration

# Change umask to make directory sharing easier
umask 0022

# Exports
export ALTERNATE_EDITOR=""
export VISUAL="emacsclient -c -n -a ''"   # $VISUAL opens in GUI with non-daemon as alternate
export EDITOR="emacsclient -t -a ''"  # $EDITOR should open in terminal
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
alias em="emacsclient -c -n -a ''"      # opens the GUI
alias emt="emacsclient -t -a ''"        # used to be "emacs -nw"
alias semt="sudo emacsclient -t -a ''"  # used to be "sudo emacs -nw"

# git
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gl='git pull --prune'
alias grm="git status | grep deleted | awk '{\$1=\$2=\"\"; print \$0}' | perl -pe 's/^[ \t]*//' | sed 's/ /\\\\ /g' | xargs git rm"

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
# emacs vterm (https://github.com/akermu/emacs-libvterm)
#
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
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

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

#
# fzf
#
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
if type fzf >/dev/null 2>/dev/null; then
    # Use ~~ as the trigger sequence instead of the default **
    # export FZF_COMPLETION_TRIGGER='~~'

    # Options to fzf command
    export FZF_DEFAULT_OPTS='--height 80% --layout=reverse --border --info=inline'
    # export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'
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

    #
    # General
    # 

    # switch to a project
    function pproj() {
        cd $(find ~/Workspace/Personal/ -maxdepth 2 -type d | fzf)
    }
    function wproj() {
        cd $(find ~/Workspace/Work/ -maxdepth 2 -type d | fzf)
    }

    # change to an arbitrary subdirectory
    function fcd() {
        cd $(find . -type d | fzf)
    }

    # fuzzy match current directory contents
    function ffuzz() {
        search_term=$1
        find . -wholename \*$search_term\* -not -path './.*/*' | fzf
    }

    # run a command from the history
    function fhist() {
        $(history | cut -c8- | sort -u | fzf)
    }

    #
    # Processes
    # 

    # show process id - interactive grep
    function fgrep() {
        ps aux | eval fzf | awk '{ print $2 }'
    }

    # kill processes - list only the ones you can kill.
    function fkill() {
        local pid 
        if [ "$UID" != "0" ]; then
            pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
        else
            pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
        fi  

        if [ "x$pid" != "x" ]
        then
            echo $pid | xargs kill -${1:-9}
        fi  
    }

    #
    # Git
    # 

    # fbr - checkout git branch (including remote branches)
    function fbr() {
        local branches branch
        branches=$(git branch --all | grep -v HEAD) &&
            branch=$(echo "$branches" | fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
            git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
    }

    # fco - checkout git branch/tag
    function fco() {
        local tags branches target
        branches=$(
            git --no-pager branch --all \
                --format="%(if)%(HEAD)%(then)%(else)%(if:equals=HEAD)%(refname:strip=3)%(then)%(else)%1B[0;34;1mbranch%09%1B[m%(refname:short)%(end)%(end)" \
                | sed '/^$/d') || return
        tags=$(git --no-pager tag | awk '{print "\x1b[35;1mtag\x1b[m\t" $1}') || return
        target=$(
            (echo "$branches"; echo "$tags") |
                fzf --no-hscroll --no-multi -n 2 \
                    --ansi) || return
        git checkout $(awk '{print $2}' <<<"$target" )
    }

    # fcoc - checkout git commit
    function fcoc() {
        local commits commit
        commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
            commit=$(echo "$commits" | fzf --tac +s +m -e) &&
            git checkout $(echo "$commit" | sed "s/ .*//")
    }

    # fshow - git commit browser
    function fshow() {
        git log --graph --color=always \
            --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
            fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
                --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
    }

    #
    # Tmux
    #
    if type tmux >/dev/null 2>/dev/null; then
        # `ftm_session_new` - create new tmux session, or switch to existing one. Works from within tmux too. (@bag-man)
        # `ftm_session_new` will allow you to select your tmux session via fzf.
        # `ftm_session_new irc` will attach to the irc session (if it exists), else it will create it.
        function ftm_session_new() {
            [[ -n "$TMUX" ]] && change="switch-client" || change="attach-session"
            if [ $1 ]; then
                tmux $change -t "$1" 2>/dev/null || (tmux new-session -d -s $1 && tmux $change -t "$1"); return
            fi
            session=$(tmux list-sessions -F "#{session_name}" 2>/dev/null | fzf --exit-0) &&  tmux $change -t "$session" || echo "No sessions found."
        }

        
        # zsh; needs setopt re_match_pcre. You can, of course, adapt it to your own shell easily.
        function ftm_session_kill() {
            local sessions
            sessions="$(tmux ls|fzf --exit-0 --multi)"  || return $?
            local i
            for i in "${(f@)sessions}"
            do
                [[ $i =~ '([^:]*):.*' ]] && {
                    echo "Killing $match[1]"
                    tmux kill-session -t "$match[1]"
                }
            done
        }

        # ftm_session_switch [FUZZY PATTERN] - Select selected tmux session
        #   - Bypass fuzzy finder if there's only one match (--select-1)
        #   - Exit if there's no match (--exit-0)
        function ftm_session_switch() {
            local session
            session=$(tmux list-sessions -F "#{session_name}" | \
                fzf --query="$1" --select-1 --exit-0) &&
                tmux switch-client -t "$session"
        }

        # ftn_pane_switch - switch pane
        function ftm_pane_switch() {
            local panes current_window current_pane target target_window target_pane
            panes=$(tmux list-panes -s -F '#I:#P - #{pane_current_path} #{pane_current_command}')
            current_pane=$(tmux display-message -p '#I:#P')
            current_window=$(tmux display-message -p '#I')

            target=$(echo "$panes" | grep -v "$current_pane" | fzf +m --reverse) || return

            target_window=$(echo $target | awk 'BEGIN{FS=":|-"} {print$1}')
            target_pane=$(echo $target | awk 'BEGIN{FS=":|-"} {print$2}' | cut -c 1)

            if [[ $current_window -eq $target_window ]]; then
                tmux select-pane -t ${target_window}.${target_pane}
            else
                tmux select-pane -t ${target_window}.${target_pane} &&
                    tmux select-window -t $target_window
            fi
        }
    fi
fi
