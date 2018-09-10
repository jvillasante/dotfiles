#!/bin/sh

. $(dirname "$0")/common.sh

if ask "Install common go packages?"; then
  # rm -rf $GOPATH/bin/*
  # rm -rf $GOPATH/pkg/*

  # Go Layer
  go get -u -v github.com/mdempsky/gocode
  check $?

  go get -u -v github.com/rogpeppe/godef
  check $?

  go get -u -v golang.org/x/tools/cmd/guru
  check $?

  go get -u -v golang.org/x/tools/cmd/gorename
  check $?

  go get -u -v golang.org/x/tools/cmd/goimports
  check $?

  go get -u -v github.com/zmb3/gogetdoc
  check $?

  go get -u -v github.com/cweill/gotests/...
  check $?

  go get -u github.com/haya14busa/gopkgs/cmd/gopkgs
  check $?

  go get -u -v github.com/davidrjenni/reftools/cmd/fillstruct
  check $?

  go get -u github.com/josharian/impl
  check $?

  # Others
  go get -u -v golang.org/x/net/html
  check $?
else
  echo "Not installing common go packages."
fi
