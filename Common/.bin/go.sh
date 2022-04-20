#!/bin/bash

. "$(dirname "$0")/common.sh"

install_common_go_packages() {
    if hash go 2>/dev/null; then
        go get golang.org/x/tools/gopls@latest
        check $?

        go get -u github.com/x-motemen/gore/cmd/gore
        check $?

        go get -u github.com/stamblerre/gocode
        check $?

        go get -u golang.org/x/tools/cmd/godoc
        check $?

        go get -u golang.org/x/tools/cmd/goimports
        check $?

        go get -u golang.org/x/tools/cmd/gorename
        check $?

        go get -u golang.org/x/tools/cmd/guru
        check $?

        go get -u github.com/cweill/gotests/...
        check $?

        go get -u github.com/fatih/gomodifytags
        check $?
    fi
}

install_other_go_packages() {
    if hash go 2>/dev/null; then
        go get -u golang.org/x/net/html
        check $?
    fi
}

cleanup_go_binaries() {
    if [ -d "${GOPATH}/bin" ]; then
        sudo rm -rf "$GOPATH/bin"
    fi

    if [ -d "${GOPATH}/pkg" ]; then
        sudo rm -rf "$GOPATH/pkg"
    fi
}

while true; do
    PS3="Choose an option: "
    options=("Install Common Go Packages Globally" "Install Other Go Packages Globally" "Cleanup Go Binaries" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                install_common_go_packages
                hr
                break
                ;;
            2)
                install_other_go_packages
                hr
                break
                ;;
            3)
                cleanup_go_binaries
                hr
                break
                ;;
            4) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done
done
