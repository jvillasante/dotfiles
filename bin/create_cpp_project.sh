#!/bin/bash

#
# Functions
#
show_usage() {
  echo "Usage:"
  echo "    ./create_cpp_project.sh        -> show this message"
  echo "    ./create_cpp_project.sh [path] -> create project on given [path]"
  echo ""
}

check() {
  if [ $1 -ne 0 ]; then
    echo ""
    echo ">>> This is an error, do something else... We don't know what's wrong here!!!"
    echo ""
    exit $1
  fi
}

#
# Run baby, run!
#
if [ -z "$1" ]; then
  show_usage
  exit 0
fi

if [ ! -d $1 ]; then
  echo ">>> $1 does not exists, creating it..."
  mkdir -p $1
  check $?
  echo ""

  echo ">>> Copying project template to $1..."
  cp -a ~/bin/cpp_project_template/. $1/
  check $?

  echo ">>> Done!"
  exit 0
else
  echo ">>> $1 already exists, exiting..."
  echo ""
  exit 0
fi
