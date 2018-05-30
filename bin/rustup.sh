#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Do you want to update rust?"; then
  rustup update
else
  echo "Not updating rust."
fi

if ask "Do you want to update cargo?"; then
  cargo install-update -a
else
  echo "Not updating cargo."
fi
