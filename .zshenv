export PATH="/usr/bin:/bin:/usr/sbin:/sbin:$PATH"
export MANPATH="/usr/local/man:$MANPATH"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

# LinuxBrew
if [ -d "$HOME/.linuxbrew/bin" ]; then
  export PATH="$HOME/.linuxbrew/bin:$PATH"
  export XDG_DATA_DIRS="/home/jvillasante/.linuxbrew/share:$XDG_DATA_DIRS"
fi

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
    export RUST_SRC_PATH="$HOME/.multirust/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"
fi

# Go
if [ -d "$HOME/.gvm" ]; then
  [[ -s "/home/jvillasante/.gvm/scripts/gvm" ]] && source "/home/jvillasante/.gvm/scripts/gvm"
  export GOPATH="$HOME/Hacking/workspace/golang"
  export PATH="$PATH:$GOPATH/bin"
fi

# QT
# export PATH="$HOME/Software/Qt/5.6/gcc_64/bin:$PATH"

# Android
# export PATH="$HOME/Android/Sdk/tools:$PATH"
# export PATH="$HOME/Android/Sdk/platform-tools:$PATH"

# Boost
export BOOST_ROOT="$HOME/Software/src/boost_1_61_0"
export BOOST_VERSION=1.61.0

# ASIO
export BOOST_ASIO_ROOT="$HOME/Software/src/asio-1.10.8"
export BOOST_ASIO_VERSION=1.10.8

# Google Test
export GTEST_HOME="$HOME/Software/src/googletest"

# CppUTest
export CPPUTEST_HOME="$HOME/Software/src/cpputest"

# cURL (libcurl)
export CURL_HOME="$HOME/Software/src/curl-7.49.1"

# JsonCpp
export JSONCPP_HOME="$HOME/Software/src/jsoncpp"

# rlog
export RLOG_HOME="$HOME/Software/src/rlog-1.4"
