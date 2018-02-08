export PATH="/usr/local/bin:/usr/local/sbin:$PATH"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
    export PATH="$HOME/.cargo/bin:$PATH"
    export RUST_SRC_PATH="$HOME/.multirust/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"
fi

# Go
if [ -d "$HOME/.gvm" ]; then
  [[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
  export GOPATH=$HOME/Hacking/workspace/golang
  export GOROOT=/usr/local/opt/go/libexec
  export PATH=$PATH:$GOPATH/bin
  export PATH=$PATH:$GOROOT/bin
fi

# QT
# export PATH="$HOME/Software/Qt/5.6/gcc_64/bin:$PATH"

# Android
# export PATH="$HOME/Android/Sdk/tools:$PATH"
# export PATH="$HOME/Android/Sdk/platform-tools:$PATH"

# Boost
export BOOST_ROOT="$HOME/Hacking/Software/boost_1_65_1"
export BOOST_VERSION=1.65.1

# ASIO
# export BOOST_ASIO_ROOT="$HOME/Software/src/asio-1.10.8"
# export BOOST_ASIO_VERSION=1.10.8

# Google Test
export GTEST_HOME="$HOME/Hacking/software/googletest"

# CppUTest
# export CPPUTEST_HOME="$HOME/Software/src/cpputest"

# cURL (libcurl)
# export CURL_HOME="$HOME/Software/src/curl-7.49.1"

# JsonCpp
# export JSONCPP_HOME="$HOME/Software/src/jsoncpp"

# rlog
# export RLOG_HOME="$HOME/Software/src/rlog-1.4"
