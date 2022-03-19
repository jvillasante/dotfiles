#!/bin/bash

. "$(dirname "$0")/common.sh"

github_latest() {
    local GITHUB_ORGANIZATION
    local GITHUB_REPO
    local OUTPUT
    local LOCATION

    GITHUB_ORGANIZATION="$1"
    GITHUB_REPO="$2"
    OUTPUT="$3"
    LOCATION=$(curl -s https://api.github.com/repos/"$GITHUB_ORGANIZATION"/"$GITHUB_REPO"/releases/latest |
        grep "tarball_url" |
        awk '{ print $2 }' |
        sed 's/,$//' |
        sed 's/"//g')

    echo "Using latest ${GITHUB_ORGANIZATION}/${GITHUB_REPO} version ${LOCATION}"
    curl -L "$LOCATION" | tar zxf - -C "${OUTPUT}"
}

update_usersjs() {
    # https://github.com/arkenfox/user.js
    github_latest arkenfox user.js ~/Downloads/test
    check $?

    #  /home/jvillasante/.mozilla/firefox/optd5ich.default-release
}

while true; do
    PS3="Choose an option: "
    options=("Update Users.js" "Quit")

    select opt in "${options[@]}"; do
        case $REPLY in
            1)
                update_usersjs
                hr
                break
                ;;
            2) break 2 ;;
            *) echo "Invalid option '$opt'" >&2 ;;
        esac
    done
done
