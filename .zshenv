export PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
export MANPATH="/usr/local/man:$MANPATH"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.cargo/bin" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
    export RUST_SRC_PATH="$HOME/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
fi

# QT
# export PATH="$HOME/Software/Qt/5.6/gcc_64/bin:$PATH"

# golang
if [ -d "$HOME/.gvm" ]; then
    [[ -s "/home/jvillasante/.gvm/scripts/gvm" ]] && source "/home/jvillasante/.gvm/scripts/gvm"
    export GOPATH="$HOME/Hacking/workspace/golang"
    export PATH="$PATH:$GOPATH/bin"
fi

# android
# export PATH="$HOME/Android/Sdk/tools:$PATH"
# export PATH="$HOME/Android/Sdk/platform-tools:$PATH"
