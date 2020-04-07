#!/bin/sh

. $(dirname "$0")/common.sh

# Check if go is installed and perform updates
if hash go 2>/dev/null; then
  if ask "Install common go packages?"; then
    rm -rf $GOPATH/bin/*
    rm -rf $GOPATH/pkg/*

    # Go Layer
    GO111MODULE=on go get -v golang.org/x/tools/gopls@latest
    GO111MODULE=on CGO_ENABLED=0 go get -v -trimpath -ldflags '-s -w' github.com/golangci/golangci-lint/cmd/golangci-lint
    go get -u github.com/motemen/gore/cmd/gore
    go get -u github.com/stamblerre/gocode
    go get -u golang.org/x/tools/cmd/godoc
    go get -u golang.org/x/tools/cmd/goimports
    go get -u golang.org/x/tools/cmd/gorename
    go get -u golang.org/x/tools/cmd/guru
    go get -u github.com/cweill/gotests/...
    go get -u github.com/fatih/gomodifytags

    # Others
    # go get -u -v golang.org/x/net/html
    # check $?
  else
    echo "Not installing common go packages."
  fi
fi
