#!/bin/sh

. $(dirname "$0")/common.sh

# Check if go is installed and perform updates
if hash go 2>/dev/null; then
  if ask "Install common go packages?"; then
    rm -rf $GOPATH/bin/*
    rm -rf $GOPATH/pkg/*

    # Go Layer
    go get -u -v github.com/mdempsky/gocode
    go get -u -v github.com/rogpeppe/godef
    go get -u -v golang.org/x/tools/cmd/guru
    go get -u -v golang.org/x/tools/cmd/gorename
    go get -u -v golang.org/x/tools/cmd/goimports
    go get -u -v github.com/zmb3/gogetdoc
    go get -u -v github.com/cweill/gotests/...
    go get -u -v github.com/haya14busa/gopkgs/cmd/gopkgs
    go get -u -v github.com/davidrjenni/reftools/cmd/fillstruct
    go get -u -v github.com/josharian/impl

    # Others
    # go get -u -v golang.org/x/net/html
    # check $?
  else
    echo "Not installing common go packages."
  fi
fi
