#!/bin/sh

. $(dirname "$0")/common.sh

# Check if go is installed and perform updates
if hash go 2>/dev/null; then
    if ask "Install common go packages?"; then
        # rm -rf $GOPATH/bin/*
        # rm -rf $GOPATH/pkg/*

        # Go Layer (DOOM Emacs)
        GO111MODULE=on go get golang.org/x/tools/gopls@latest
        GO111MODULE=on go get -u github.com/motemen/gore/cmd/gore
        GO111MODULE=on go get -u github.com/stamblerre/gocode
        GO111MODULE=on go get -u golang.org/x/tools/cmd/godoc
        GO111MODULE=on go get -u golang.org/x/tools/cmd/goimports
        GO111MODULE=on go get -u golang.org/x/tools/cmd/gorename
        GO111MODULE=on go get -u golang.org/x/tools/cmd/guru
        GO111MODULE=on go get -u github.com/cweill/gotests/...
        GO111MODULE=on go get -u github.com/fatih/gomodifytags

        # Others
        # go get -u -v golang.org/x/net/html
        # check $?
    else
        echo "Not installing common go packages."
    fi
fi
