#!/bin/sh

#
# Functions
#
check() {
  if [ $1 -ne 0 ]; then
    echo ""
    echo ">>> This is an error, do something else... We don't know what's wrong here!!!"
    echo ""
    exit $1
  fi
}

ask() {
  # https://djm.me/ask
  local prompt default reply

  while true; do
    if [ "${2:-}" = "Y" ]; then
      prompt="Y/n"
      default=Y
    elif [ "${2:-}" = "N" ]; then
      prompt="y/N"
      default=N
    else
      prompt="y/n"
      default=
    fi

    # Ask the question (not using "read -p" as it uses stderr not stdout)
    echo "$1 [$prompt] "

    # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
    read reply </dev/tty

    # Default?
    if [ -z "$reply" ]; then
      reply=$default
    fi

    # Check if the reply is valid
    case "$reply" in
      Y*|y*) return 0 ;;
      N*|n*) return 1 ;;
    esac

  done
}

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
