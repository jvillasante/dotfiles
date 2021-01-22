#!/bin/sh

. $(dirname "$0")/common.sh

show_usage() {
  echo "Usage:"
  echo "    ./create_rust_project.sh        -> show this message"
  echo "    ./create_rust_project.sh [path] -> create project on given [path]"
  echo ""
}

#
# Run baby, run!
#

if [ -z "$1" ]; then
  show_usage
  exit 1
fi

DOTFILES_DIR=$(find_dotfiles)

if [ ! -d $1 ]; then
  echo ">>> $1 does not exists, creating it..."
  mkdir -p $1
  check $?
  echo ""

  echo ">>> Copying project template to $1..."
  cp -a ~/.bin/rust_project_template/. $1/
  check $?

  cp ~/.editorconfig $1/
  check $?

  cp ~/.rustfmt.toml $1/
  check $?

  echo ">>> Done!"
  exit 0
else
  echo ">>> $1 already exists, exiting..."
  echo ""
  exit 1
fi
