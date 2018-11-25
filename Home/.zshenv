export PATH="/usr/local/bin:/usr/local/sbin:$PATH"

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
  export PATH="$HOME/bin:$PATH"
fi

# Rust
if [ -d "$HOME/.cargo/bin" ]; then
  export CARGO_HOME="$HOME/.cargo"
  export PATH="$CARGO_HOME/bin:$PATH"
  export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
fi

# Go
if [ -d "$HOME/.gvm/bin" ]; then
  [[ -s "$HOME/.gvm/scripts/gvm" ]] && source "$HOME/.gvm/scripts/gvm"
  export GOPATH=$HOME/Hacking/workspace/golang
  export PATH="$GOPATH/bin:$PATH"
fi

# Boost
export BOOST_ROOT="$HOME/Hacking/software/boost_1_68_0"
export BOOST_VERSION=1.68.0

# Google Test
export GTEST_HOME="$HOME/Hacking/software/googletest"

# ASIO
# export BOOST_ASIO_ROOT="$HOME/Software/src/asio-1.10.8"
# export BOOST_ASIO_VERSION=1.10.8

# Range-v3
# export RANGEV3_HOME="$HOME/Hacking/Software/range-v3"

# cURL (libcurl)
export CURL_HOME="$HOME/Hacking/software/curl-7.62.0"

# JsonCpp
export JSONCPP_HOME="$HOME/Hacking/software/jsoncpp"

# rlog
# export RLOG_HOME="$HOME/Software/src/rlog-1.4"
