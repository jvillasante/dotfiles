#!/bin/bash

. "$(dirname "$0")/common.sh"

#
# https://github.com/arkenfox/user.js
#
update_usersjs() {
    if pgrep -x firefox >/dev/null; then
        echo "Firefox is running. Close Firefox and try again."
        exit 1
    fi

    local PROFILE
    PROFILE=$(cat "$HOME/.mozilla/firefox/profiles.ini" | sed -n -e 's/^.*Default=//p' | head -n 1)
    if [[ ! "$PROFILE" =~ ^.*\.arkenfox$ ]]; then
        echo "Invalid profile '$PROFILE', shoule be called '*******.arkenfox', exiting..."
        exit 1
    else
        PROFILE="${HOME}/.mozilla/firefox/$PROFILE"
    fi

    if [ ! -d "${PROFILE}" ]; then
        echo ">>> Firefox profile (${PROFILE}) does not exits, please verify with 'about:support'."
        exit 1
    fi

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

// Disable RFP letterboxing (Dont like those huge margins)
/* 4503 */ user_pref("privacy.resistFingerprinting.letterboxing", false);

// Re-enables URL usages as a search bar.
/* 0801 */ user_pref("keyword.enabled", true);

// Don't delete cookies and site data on exit
/* 2801 */ user_pref("network.cookie.lifetimePolicy", 0);

/* 2811 */
// Don't throw away HTTP basic authentication sessions on Firefox shutdown.
user_pref("privacy.clearOnShutdown.sessions", false);
// Don't throw away history on Firefox shutdown.
user_pref("privacy.clearOnShutdown.history", false);

// Re-enables WebGL.
/* 4520 */ user_pref("webgl.disabled", false);

/* 9000 */
// Disables Pocket extension.
user_pref("extensions.pocket.enabled", false);

// Sets Quad9's DoH resolver as TRR.
// From <https://quad9.net/doh-quad9-dns-servers/#UsingDoHwithQuad9DNSServers-Firefox>
// ... and <https://wiki.mozilla.org/Trusted_Recursive_Resolver>.
user_pref("network.trr.mode", 2);
user_pref("network.trr.custom_uri", "Quad9");
user_pref("network.trr.uri", "https://dns.quad9.net:5053/dns-query");
user_pref("network.trr.bootstrapAddress", "9.9.9.9");

/* END: internal custom pref to test for syntax errors ***/
user_pref("_user.js.parrot", "SUCCESS: No no he's not dead, he's, he's restin'!");
ENDOFFILE

    sh "${PROFILE}"/updater.sh
    check $?

    sh "${PROFILE}"/prefsCleaner.sh
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
