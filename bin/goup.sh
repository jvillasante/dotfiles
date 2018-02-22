#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Install common go packages?"; then
  go get -u -v github.com/nsf/gocode
  check $?

  go get -u -v github.com/rogpeppe/godef
  check $?

  go get -u -v golang.org/x/tools/cmd/guru
  check $?

  go get -u -v golang.org/x/tools/cmd/gorename
  check $?

  go get -u -v golang.org/x/tools/cmd/goimports
  check $?
else
  echo "Not installing common go packages."
fi
