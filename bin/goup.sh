#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Install common go packages?"; then
  # rm -rf $GOPATH/bin/*
  # rm -rf $GOPATH/pkg/*

  go get -u -v github.com/nsf/gocode
  check $?

  # go get -u -v github.com/mdempsky/gocode
  # check $?

  go get -u -v github.com/rogpeppe/godef
  check $?

  go get -u -v golang.org/x/tools/cmd/guru
  check $?

  go get -u -v golang.org/x/tools/cmd/gorename
  check $?

  go get -u -v golang.org/x/tools/cmd/goimports
  check $?

  go get -u -v golang.org/x/net/html
  check $?
else
  echo "Not installing common go packages."
fi
