# Set the path to include:
#  - /usr/local/bin  for Homebrew on OSX
#  - /usr/local/sbin
#  - /usr/bin        for system executable
#  - /bin
#  - /usr/sbin
#  - /sbin
export PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    export PATH="$PATH:$HOME/bin"
fi

# llvm
if [ -d "/usr/local/opt/llvm/bin" ]; then
    export PATH="/usr/local/opt/llvm/bin:$PATH"
fi

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
    export CARGO_HOME="$HOME/.cargo"
    export PATH="$CARGO_HOME/bin:$PATH"
    export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
fi

# Go
if [ -d "$HOME/Hacking/workspace/go" ]; then
    export GOPATH=$HOME/Hacking/workspace/go
    export PATH="$GOPATH/bin:$PATH"
fi

# Boost
# export BOOST_ROOT="$HOME/Hacking/software/boost_1_68_0"
# export BOOST_VERSION=1.68.0

# Google Test
# export GTEST_HOME="$HOME/Hacking/software/googletest"

# ASIO
# export BOOST_ASIO_ROOT="$HOME/Software/src/asio-1.10.8"
# export BOOST_ASIO_VERSION=1.10.8

# Range-v3
# export RANGEV3_HOME="$HOME/Hacking/software/range-v3"

# cURL (libcurl)
# export CURL_HOME="$HOME/Hacking/software/curl-7.62.0"

# JsonCpp
# export JSONCPP_HOME="$HOME/Hacking/software/jsoncpp"

# rlog
# export RLOG_HOME="$HOME/Software/src/rlog-1.4"
