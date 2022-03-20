#!/bin/bash

. "$(dirname "$0")/common.sh"
FIREFOX_PROFILE_DIR="${HOME}/.mozilla/firefox/csl5g7gp.default-release"

#
# https://github.com/arkenfox/user.js
#
update_usersjs() {
    if pgrep -x firefox >/dev/null; then
        echo "Firefox is running. Close Firefox and try again."
        exit 1
    fi

    if [ ! -d "${FIREFOX_PROFILE_DIR}" ]; then
        echo ">>> Firefox profile (${FIREFOX_PROFILE_DIR}) does not exits, please verify with 'about:support'."
        exit 1
    fi

    # Remove previous if any
    rm -rf /tmp/arkenfox-user.js-*

    # Get latest and update
    github_latest_release arkenfox user.js /tmp
    check $?

    cp -f /tmp/arkenfox-user.js-*/prefsCleaner.sh "${FIREFOX_PROFILE_DIR}"
    check $?

    cp -f /tmp/arkenfox-user.js-*/updater.sh "${FIREFOX_PROFILE_DIR}"
    check $?

    cp -f /tmp/arkenfox-user.js-*/user.js "${FIREFOX_PROFILE_DIR}"
    check $?

    cat >"${FIREFOX_PROFILE_DIR}/user-overrides.js" <<ENDOFFILE
/* 0801: re-enable location bar using search ***/
user_pref("keyword.enabled", true);

/* 4504: re-enable RFP letterboxing [FF67+] ***/
user_pref("privacy.resistFingerprinting.letterboxing", false);

/* 2801: do not delete cookies and site data on exit ***/
user_pref("network.cookie.lifetimePolicy", 0);

/* 2811: keep history on shutdown ***/
user_pref("privacy.clearOnShutdown.history", false);
ENDOFFILE

    sh "${FIREFOX_PROFILE_DIR}"/updater.sh
    check $?

    sh "${FIREFOX_PROFILE_DIR}"/prefsCleaner.sh
    check $?
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
