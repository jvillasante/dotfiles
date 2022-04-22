# Set the path to include:
#  - /usr/local/bin  for Homebrew on OSX
#  - /usr/local/sbin
#  - /usr/bin        for system executable
#  - /bin
#  - /usr/sbin
#  - /sbin
export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"

# set PATH so it includes .local bin if it exists
if [ -d "$HOME/.local/bin" ]; then
    export PATH="$PATH:$HOME/.local/bin"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.bin/bin" ]; then
    export PATH="$PATH:$HOME/.bin/bin"
fi

# snap
if [ -d "/snap/bin" ]; then
    export PATH="$PATH:/snap/bin"
fi

# llvm
if [ -d "/usr/local/opt/llvm/bin" ]; then
    export PATH="/usr/local/opt/llvm/bin:$PATH"
fi

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
    export CARGO_HOME="$HOME/.cargo"
    if [ -f "$CARGO_HOME/env" ]; then
        source "$CARGO_HOME/env"
    else
        export PATH="$CARGO_HOME/bin:$PATH"
    fi

    export LD_LIBRARY_PATH="$(rustc --print sysroot)/lib:$LD_LIBRARY_PATH"
    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
fi

# Go
if [ -d "$HOME/Workspace/Private/Projects/go" ]; then
    export GOPATH=$HOME/Workspace/Private/Projects/go
    export GOBIN=$GOPATH/bin
    export PATH="$GOBIN:$PATH"
fi

# Zig
if [ -d "$HOME/Workspace/Software/zig/zig" ]; then
    export PATH="$HOME/Workspace/Software/zig/zig:$PATH"
fi

# Tmuxifier
if [ -d "$HOME/.tmuxifier" ]; then
    export PATH="$HOME/.tmuxifier/bin:$PATH"
    eval "$(tmuxifier init -)"
fi

# Cling
if [ -d "$HOME/Workspace/Software/system/cling/cling/bin" ]; then
    export PATH="$HOME/Workspace/Software/system/cling/cling/bin:$PATH"
fi
