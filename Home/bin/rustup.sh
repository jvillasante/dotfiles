#!/bin/sh

. $(dirname "$0")/common.sh

# Check if rustup is installed and perform updates
if hash rustup 2>/dev/null; then
  if ask "Do you want to update rust?"; then
    rustup self update
    check $?

    rustup update
    check $?
  else
    echo "Not updating rust."
  fi

  if ask "Do you want to update cargo?"; then
    cargo install-update -a
    check $?
  else
    echo "Not updating cargo."
  fi
fi
