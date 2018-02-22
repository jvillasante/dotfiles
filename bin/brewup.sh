#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Do you want to update brew?"; then
  brew update
  check $?
else
  echo "Not updating brew."
fi

if ask "Do you want to upgrade brew?"; then
  brew upgrade
  check $?
else
  echo "Not upgrading brew."
fi

if ask "Do you want to cleanup brew?"; then
  brew cleanup -s
  check $?
else
  echo "Not cleaning up brew."
fi
