#!/usr/bin/env bash

. "$(dirname "$0")/common.sh"

function usage() {
    echo "Usage:"
    echo "    $0 help:"
    echo "        Show this help message"
    echo "    $0 all:"
    echo "        Update both arkenfox and non-arkenfox profiles"
    echo "    $0 arkenfox:"
    echo "        Update arkenfox profile"
    echo "    $0 non-arkenfox:"
    echo "        Update non-arkenfox profile"
    echo "    $0 personal"
    echo "        Update personal users.js"
    echo
    echo " e.g: $0 arkenfox"
    exit "$1"
}

#
# https://github.com/arkenfox/user.js
#
function update_arkenfox() {
    if pgrep -x firefox >/dev/null; then
        echo ">>> Firefox is running. Close Firefox and try again."
        usage 1
    fi

    local PROFILE
    PROFILE=$(find "$HOME"/.mozilla/firefox/ -regextype sed -regex '.*.firefox-arkenfox$')
    if [ ! -d "$PROFILE" ]; then
        echo ">>> Found $PROFILE but it is not a directory, exiting..."
        exit 1
    fi

    echo ">>> Updating 'user.js' file for $PROFILE"

    # Remove previous if any
    rm -rf /tmp/arkenfox-user.js-*

    # Get latest and update
    github_latest_release arkenfox user.js /tmp
    check $?

    cp -f /tmp/arkenfox-user.js-*/prefsCleaner.sh "${PROFILE}"
    check $?

    cp -f /tmp/arkenfox-user.js-*/updater.sh "${PROFILE}"
    check $?

    cp -f /tmp/arkenfox-user.js-*/user.js "${PROFILE}"
    check $?

    cat >"${PROFILE}/user-overrides.js" <<ENDOFFILE
// Re-enables 'about:home' page for startup landing page and new tabs.
/* 0102 */ user_pref("browser.startup.page", 1);
/* 0103 */ user_pref("browser.startup.homepage", "about:home");
/* 0104 */ user_pref("browser.newtabpage.enabled", true);
           user_pref("browser.newtab.preload", true);

// RFP is not for me
/* 4501 */ user_pref("privacy.resistFingerprinting", false);
/* 4504 */ user_pref("privacy.resistFingerprinting.letterboxing", false);
/* 4520 */ user_pref("webgl.disabled", false);

// Re-enables URL usages as a search bar.
/* 0801 */ user_pref("keyword.enabled", true);

// Enable live search suggestions
/* 0804 */ user_pref("browser.search.suggest.enabled", true);
           user_pref("browser.urlbar.suggest.searches", true);

// Don't throw away HTTP basic authentication sessions and history on Firefox shutdown.
/* 2811 */ user_pref("privacy.clearOnShutdown.sessions", false);
           user_pref("privacy.clearOnShutdown.history", false);

// Don't delete cookies and site data on exit (better to have site exceptions here!)
/* 2815 */ user_pref("privacy.clearOnShutdown.cookies", false);
           user_pref("privacy.clearOnShutdown.offlineApps", false);

// Disables Pocket extension.
/* 9000 */ user_pref("extensions.pocket.enabled", false);

// Personal, make tabbar height smaller
user_pref("browser.uidensity", 1);

// Personal, disable show menu on ALT press
user_pref("ui.key.menuAccessKeyFocuses", false);

/* END: internal custom pref to test for syntax errors ***/
user_pref("_user.js.parrot", "SUCCESS: No no he's not dead, he's, he's restin'!");
ENDOFFILE

    sh "${PROFILE}"/updater.sh
    check $?

    sh "${PROFILE}"/prefsCleaner.sh
    check $?

    echo ""
}

function update_non_arkenfox() {
    if pgrep -x firefox >/dev/null; then
        echo ">>> Firefox is running. Close Firefox and try again."
        usage 1
    fi

    local PROFILE
    PROFILE=$(find "$HOME"/.mozilla/firefox/ -regextype sed -regex '.*.firefox-non-arkenfox$')
    if [ ! -d "$PROFILE" ]; then
        echo "Found $PROFILE but it is not a directory, exiting..."
        exit 1
    fi

    echo ">>> Updating 'user.js' file for $PROFILE"

    cat >"${PROFILE}/user.js" <<ENDOFFILE
/*********************************************************************
*
* name: user.js
* descr.: Mozilla Firefox configuration file: 'user.js'
*
**********************************************************************/

// Disables Pocket extension.
user_pref("extensions.pocket.enabled", false);

// Make tabbar height smaller
user_pref("browser.uidensity", 1);

// Disable show menu on ALT press
user_pref("ui.key.menuAccessKeyFocuses", false);

/* END: internal custom pref to test for syntax errors ***/
user_pref("_user.js.parrot", "SUCCESS: No no he's not dead, he's, he's restin'!");
ENDOFFILE

    echo "All done!"
    echo ""
}

nargs=$#
cmd=${1-}
rc=0
if [ "$#" -gt 0 ]; then shift; fi
case $cmd in
    all)
        [ "$nargs" -eq 1 ] || usage 1
        update_arkenfox "$@"
        update_non_arkenfox "$@"
        ;;
    arkenfox)
        [ "$nargs" -eq 1 ] || usage 1
        update_arkenfox "$@"
        ;;
    non-arkenfox)
        [ "$nargs" -eq 1 ] || usage 1
        update_non_arkenfox "$@"
        ;;
    help | --help | -h)
        usage 0
        ;;
    *)
        usage 1
        ;;
esac
exit $rc
