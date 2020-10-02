#!/bin/sh

. $(dirname "$0")/common.sh

# Check if brew is installed and perform maintainance
if hash brew 2>/dev/null; then
  if ask "Do you want to update brew?"; then
    brew update
    brew upgrade
    brew upgrade --cask --greedy
  else
    echo "Not updating brew."
  fi

  if ask "Do you want to run brew doctor?"; then
    brew doctor
    brew missing
  else
    echo "Not runing brew doctor."
  fi

  if ask "Do you want to cleanup brew?"; then
    brew cleanup -s
  else
    echo "Not cleaning up brew."
  fi
fi
