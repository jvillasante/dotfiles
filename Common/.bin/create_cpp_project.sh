#!/bin/sh

. $(dirname "$0")/common.sh

show_usage() {
  echo "Usage:"
  echo "    ./create_cpp_project.sh        -> show this message"
  echo "    ./create_cpp_project.sh [make|cmake] [path] -> create project on given [path]"
  echo ""
}

#
# Run baby, run!
#

if [ -z "$1" ] || [ -z "$2" ] ; then
  show_usage
  exit 1
fi

DOTFILES_DIR=$(find_dotfiles)

BUILD_SYSTEM=undefined
if [ "$1" = make ]; then
  BUILD_SYSTEM=make
elif [ "$1" = cmake ]; then
  BUILD_SYSTEM=cmake
else
  show_usage
  exit 1
fi

if [ ! -d $2 ]; then
  echo ">>> $2 does not exists, creating it..."
  mkdir -p $2
  check $?
  echo ""

  echo ">>> Copying project template to $2..."
  if [ $BUILD_SYSTEM = make ]; then
    cp -a ~/.bin/cpp_project_template_make/. $2/
    check $?
  elif [ $BUILD_SYSTEM = cmake ]; then
    cp -a ~/.bin/cpp_project_template_cmake/. $2/
    check $?
  else
    show_usage
    exit 1
  fi

  cp ~/.editorconfig $2/
  check $?

  cp ~/.clang-format $2/
  check $?

  cp $DOTFILES_DIR/Common/compile_flags.txt $2/
  check $?

  cp ~/.clang-tidy $2/
  check $?

  # cp $DOTFILES_DIR/Common/.ccls $2/
  # check $?

  echo ">>> Done!"
  exit 0
else
  echo ">>> $2 already exists, exiting..."
  echo ""
  exit 1
fi
