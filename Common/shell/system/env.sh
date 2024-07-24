# Set the path to include:
#  - /usr/local/bin  for Homebrew on OSX
#  - /usr/local/sbin
#  - /usr/bin        for system executable
#  - /bin
#  - /usr/sbin
#  - /sbin
export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"

# set PATH so it includes .local bin if it exists
[ -d "$HOME/.local/bin" ] && export PATH="$HOME/.local/bin:$PATH"

# Reset the prompt for remote TRAMP shells.
if [ "${INSIDE_EMACS/*tramp*/tramp}" == "tramp" ]; then
    PS1="[\u@\h \w]$ "
fi

# emacs vterm: https://github.com/akermu/emacs-libvterm
if [ "$INSIDE_EMACS" = 'vterm' ] && [ -n "$EMACS_VTERM_PATH" ]; then
    if [ "$(readlink /proc/$$/exe)" = "$(which bash)" ]; then
        # shellcheck source=/dev/null
        [ -f "$EMACS_VTERM_PATH/etc/emacs-vterm-bash.sh" ] &&
            source "$EMACS_VTERM_PATH/etc/emacs-vterm-bash.sh"
    elif [ "$(readlink /proc/$$/exe)" = "$(which zsh)" ]; then
        # shellcheck source=/dev/null
        [ -f "$EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh" ] &&
            source "$EMACS_VTERM_PATH/etc/emacs-vterm-zsh.sh"
    elif [ "$(readlink /proc/$$/exe)" = "$(which fish)" ]; then
        # shellcheck source=/dev/null
        [ -f "$EMACS_VTERM_PATH/etc/emacs-vterm-fish.sh" ] &&
            source "$EMACS_VTERM_PATH/etc/emacs-vterm-fish.sh"
    fi

    find_file() {
        vterm_cmd find-file "$(realpath "${@:-.}")"
    }
fi

# emacs eat: https://codeberg.org/akib/emacs-eat
if [ -n "$EAT_SHELL_INTEGRATION_DIR" ]; then
    if [ -r "$EAT_SHELL_INTEGRATION_DIR/bash" ]; then
        # shellcheck source=/dev/null
        source "$EAT_SHELL_INTEGRATION_DIR/bash"
    fi

    # eat alias to open a file inside emacs
    alias eopen='_eat_msg open'
fi

# snap
[ -d "/snap/bin" ] && export PATH="$PATH:/snap/bin"

# llvm
[ -d "/usr/local/opt/llvm/bin" ] && export PATH="/usr/local/opt/llvm/bin:$PATH"

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
    export CARGO_HOME="$HOME/.cargo"
    if [ -f "$CARGO_HOME/env" ]; then
        # shellcheck source=/dev/null
        source "$CARGO_HOME/env"
    else
        export PATH="$CARGO_HOME/bin:$PATH"
    fi

    LD_LIBRARY_PATH="$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH" && export LD_LIBRARY_PATH
    RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src" && export RUST_SRC_PATH
fi

# Go
if type go > /dev/null 2> /dev/null; then
    [ ! -d "$HOME/.go/" ] && mkdir -p "$HOME/.go/"
    GOPATH="$HOME/.go" && export GOPATH
    GOBIN="$GOPATH/bin" && export GOBIN
    PATH="$GOBIN:$PATH" && export PATH
fi
