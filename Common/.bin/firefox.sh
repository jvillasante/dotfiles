#!/usr/bin/env bash

. "$(dirname "$0")/common.sh"

function usage() {
    echo "Usage:"
    echo "    $0 help:"
    echo "        Show this help message"
    echo "    $0 arkenfox:"
    echo "        Update arkenfox users.js"
    echo "    $0 personal"
    echo "        Update personal users.js"
    echo
    echo " e.g: $0 arkenfox"
    exit "$1"
}

#
# https://github.com/arkenfox/user.js
#
update_usersjs_arkenfox() {
    if pgrep -x firefox >/dev/null; then
        echo ">>> Firefox is running. Close Firefox and try again."
        usage 1
    fi

    local PROFILE
    PROFILE=$(cat "$HOME/.mozilla/firefox/profiles.ini" | sed -n -e 's/^.*Default=//p' | head -n 1)
    if [[ ! "$PROFILE" =~ ^.*\.[Pp]ersonal$ ]]; then
        echo ">>> Invalid profile '$PROFILE', should be called '*******.personal', exiting..."
        usage 1
    else
        PROFILE="${HOME}/.mozilla/firefox/$PROFILE"
    fi

    echo ">>> Creating 'user.js' file in $PROFILE"

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

// Disable saving passwords
/* 5003 user_pref("signon.rememberSignons", false); */

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
}

#
# https://brainfucksec.github.io/firefox-hardening-guide
#
update_usersjs_personal() {
    if pgrep -x firefox >/dev/null; then
        echo ">>> Firefox is running. Close Firefox and try again."
        usage 1
    fi

    local PROFILE
    PROFILE=$(cat "$HOME/.mozilla/firefox/profiles.ini" | sed -n -e 's/^.*Default=//p' | head -n 1)
    if [[ ! "$PROFILE" =~ ^.*\.personal$ ]]; then
        echo ">>> Invalid profile '$PROFILE', should be called '*******.personal', exiting..."
        usage 1
    else
        PROFILE="${HOME}/.mozilla/firefox/$PROFILE"
    fi

    echo "Creating 'user.js' file in $PROFILE"
    cat >"${PROFILE}/user.js" <<ENDOFFILE
/*********************************************************************
*
* name: user.js | brainfucksec
* descr.: Mozilla Firefox configuration file: 'user.js'
* date: 2022-03-21
* version: 0.12.0 - FF-98.0.x
* url: https://gist.github.com/brainfucksec/68e79da1c965aeaa4782914afd8f7fa2
* maintainer: brainf+ck
*
* info:
* Set preferences for the selected profile when Firefox start.
* Copy this file on Firefox Profile folder.  You should create a
* new profile to insert this file:
*
* $HOME/.mozilla/firefox/<profile-ID.name>
*
* See: "Back up and restore": https://support.mozilla.org/en-US/kb/back-and-restore-information-firefox-profiles
*
* For more information how to use this file see:
* https://kb.mozillazine.org/User.js_file
* https://github.com/arkenfox/user.js/wiki/1.1-Overview
*
* For "about:config" entries see:
* https://searchfox.org/mozilla-release/source/modules/libpref/init/all.js
*
* OPTION FORMAT:
*   user_pref("<entry>", <boolean> || <number> || "<string>");
*
* NOTE: Commented preferences are those disabled by default, some
* conflict with others if enabled, and some disable some basic
* features like audio/video libraries or other things you need.
* So be careful and check what you enable/disable.
*
**********************************************************************/

/*********************************************************************
 *
 * SECTIONS:
 *    - StartUp Settings
 *    - Geolocation
 *    - Language / Locale
 *    - Auto-updates / Recommendations
 *    - Telemetry
 *    - Studies
 *    - Crash Reports
 *    - Captive Portal Detection / Network Checks
 *    - Safe Browsing (SB)
 *    - Network: DNS / Proxy / IPv6
 *    - Search Bar: Suggestions / Autofill
 *    - Passwords
 *    - Disk Cache / Memory
 *    - HTTPS / SSL/TLS / OSCP / CERTS
 *    - Headers / Referers
 *    - Audio/Video (WebRTC, WebGL)
 *    - Downloads
 *    - Cookies
 *    - UI Features
 *    - Shutdown Settings
 *    - Fingerprinting
 *
 *********************************************************************/

/*********************************************************************
 * StartUp Settings
 *********************************************************************/

// disable about:config warning
user_pref("browser.aboutConfig.showWarning", false);

// disable check if Firefox is your default browser
user_pref("browser.shell.checkDefaultBrowser", false);

// set startup page
// 0=blank, 1=home, 2=last visited page, 3=resume previous session
user_pref("browser.startup.page",  1);
user_pref("browser.startup.homepage", "start.duckduckgo.com");

// if you want only a home blank page
//user_pref("browser.startup.page", 0);
//user_pref("browser.startup.homepage", "about:blank");

// disable activity stream on new windows and tab pages
user_pref("browser.newtabpage.enabled", false);
user_pref("browser.newtab.preload", false);
user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false);
user_pref("browser.newtabpage.activity-stream.telemetry", false);
user_pref("browser.newtabpage.activity-stream.feeds.snippets", false);
user_pref("browser.newtabpage.activity-stream.section.topstories", false);
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false);
user_pref("browser.newtabpage.activity-stream.showSponsored", false);
user_pref("browser.newtabpage.activity-stream.feeds.discoverystreamfeed", false);
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false);
user_pref("browser.newtabpage.activity-stream.default.sites", "");

/*********************************************************************
 * Geolocation
 *********************************************************************/

// use Mozilla geolocation service instead of Google if permission is granted
user_pref("geo.provider.network.url", "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%");

// disable using the OS's geolocation service
//user_pref("geo.provider.ms-windows-location", false); //Windows
//user_pref("geo.provider.use_corelocation", false); //macOS
user_pref("geo.provider.use_gpsd", false); //Linux

// disable region updates
user_pref("browser.region.network.url", "");
user_pref("browser.region.update.enabled", false);

/*********************************************************************
 * Language / Locale
 *********************************************************************/

// set language for displaying web pages:
user_pref("intl.accept_languages", "en-US, en");
user_pref("javascript.use_us_english_locale", true); //Hidden pref

/*********************************************************************
 * Auto-updates / Recommendations
 *********************************************************************/

// disable auto-installing Firefox updates
//user_pref("app.update.background.scheduling.enabled", false); //Windows
user_pref("app.update.auto", false); //Non-Windows

// disable addons recommendations (use Google Analytics)
user_pref("extensions.getAddons.showPane", false); //Hidden pref
user_pref("extensions.htmlaboutaddons.recommendations.enabled", false);

/*********************************************************************
 * Telemetry
 *********************************************************************/

// disable telemetry
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("toolkit.telemetry.enabled", false); //Default: false
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.server", "data:,");
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("toolkit.telemetry.newProfilePing.enabled", false);
user_pref("toolkit.telemetry.shutdownPingSender.enabled", false);
user_pref("toolkit.telemetry.updatePing.enabled", false);
user_pref("toolkit.telemetry.bhrPing.enabled", false);
user_pref("toolkit.telemetry.firstShutdownPing.enabled", false);
user_pref("toolkit.telemetry.coverage.opt-out", true); //Hidden pref
user_pref("toolkit.coverage.opt-out", true); //Hidden pref
user_pref("toolkit.coverage.endpoint.base.", "");
user_pref("browser.ping-centre.telemetry", false);
user_pref("beacon.enabled", false);

/*********************************************************************
 * Studies
 *********************************************************************/

// disable studies
user_pref("app.shield.optoutstudies.enabled", false);

// disable normandy/shield
user_pref("app.normandy.enabled", false);
user_pref("app.normandy.api_url", "");

/*********************************************************************
 * Crash Reports
 *********************************************************************/

// disable crash reports
user_pref("breakpad.reportURL", "");
user_pref("browser.tabs.crashReporting.sendReport", false);

/*********************************************************************
 * Captive Portal Detection / Network Checks
 *********************************************************************/

// disable captive portal detection
user_pref("captivedetect.canonicalURL", "")
user_pref("network.captive-portal-service.enabled", false);

// disable network connections checks
user_pref("network.connectivity-service.enabled", false);

/*********************************************************************
 * Safe Browsing (SB)
 *********************************************************************/

// disable safe browsing service
user_pref("browser.safebrowsing.malware.enabled", false);
user_pref("browser.safebrowsing.phishing.enabled", false);

// disable SB checks for downloads
user_pref("browser.safebrowsing.downloads.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.enabled", false);
user_pref("browser.safebrowsing.downloads.remote.url", "");

// disable SB checks for unwanted software
user_pref("browser.safebrowsing.downloads.remote.block_potentially_unwanted", false);
user_pref("browser.safebrowsing.downloads.remote.block_uncommon", false);

// disable bypasses the block of SB with a click for current session
user_pref("browser.safebrowsing.allowOverride", false);

/*********************************************************************
 * Network: DNS / Proxy / IPv6
 *********************************************************************/

// disable link prefetching
user_pref("network.prefetch-next", false);

// disable DNS prefetching
user_pref("network.dns.disablePrefetch", true);

// disable predictor / prefetching
user_pref("network.predictor.enabled", false);

// disable link-mouseover opening connection to linked server
user_pref("network.http.speculative-parallel-limit", 0);

// disable mousedown speculative connections on bookmarks and history
user_pref("browser.places.speculativeConnect.enabled", false);

// disable IPv6
user_pref("network.dns.disableIPv6", true);

// disable "GIO" protocols as a potential proxy bypass vectors
user_pref("network.gio.supported-protocols", ""); //Hidden pref

// remove special permissions for certain mozilla domains
user_pref("permissions.manager.defaultsUrl", "");

// use Punycode in Internationalized Domain Names to eliminate possible spoofing
user_pref("network.IDN_show_punycode", true);

/*********************************************************************
 * Search Bar: Suggestions / Autofill
 *********************************************************************/

// disable location bar domain guessing
user_pref("browser.fixup.alternate.enabled", false);

// display all parts of the url in the bar
user_pref("browser.urlbar.trimURLs", false);

// disable location bar making speculative connections
user_pref("browser.urlbar.speculativeConnect.enabled", false);

// disable form autofill
user_pref("browser.formfill.enable", false);
user_pref("extensions.formautofill.addresses.enabled", false);
user_pref("extensions.formautofill.available", "off");
user_pref("extensions.formautofill.creditCards.available", false);
user_pref("extensions.formautofill.creditCards.enabled", false);
user_pref("extensions.formautofill.heuristics.enabled", false);

// disable location bar contextual suggestions:
user_pref("browser.urlbar.suggest.quicksuggest.nonsponsored", false);
user_pref("browser.urlbar.suggest.quicksuggest.sponsored", false);

/*********************************************************************
 * Passwords
 *********************************************************************/

// disable saving passwords
user_pref("signon.rememberSignons", false);

// autofill login and passwords
user_pref("signon.autofillForms", false);

// disable formless login capture for Password Manager
user_pref("signon.formlessCapture.enabled", false);

/*
 * hardens against potential credentials phishing
 *    0 = don't allow sub-resources to open HTTP authentication credentials dialogs
 *    1 = don't allow cross-origin sub-resources to open HTTP authentication credentials dialogs
 *    2 = allow sub-resources to open HTTP authentication credentials dialogs (default)
 */
user_pref("network.auth.subresource-http-auth-allow", 1);

/*********************************************************************
 * Disk Cache / Memory
 *********************************************************************/

// disable disk cache
user_pref("browser.cache.disk.enable", false);

/*
 * disable storing extra session data:
 *    0 = everywhere
 *    1 = unencrypted sites
 *    2 = nowhere
 */
user_pref("browser.sessionstore.privacy_level", 2);

// disable resuming session from crash
user_pref("browser.sessionstore.resume_from_crash", false);

/*********************************************************************
 * HTTPS / SSL/TLS / OSCP / CERTS
 *********************************************************************/

// enable HTTPS-Only mode in all windows
user_pref("dom.security.https_only_mode", true);

// disable sending HTTP request for checking HTTPS support by the server
user_pref("dom.security.https_only_mode_send_http_background_request", false);

// display advanced information on Insecure Connection warning pages
user_pref("browser.xul.error_pages.expert_bad_cert", true);

// disable TLS1.3 0-RTT (round-trip time)
user_pref("security.tls.enable_0rtt_data", false);

// set OCSP to terminate the connection when a CA isn't validate
user_pref("security.OCSP.require", true);

// disable SHA-1 certificates
user_pref("security.pki.sha1_enforcement_level", 1);

/*
 * enable strict pinning (PKP (Public Key Pinning)):
 *    0 = disabled
 *    1 = allow user MiTM (i.e. your Antivirus)
 *    2 = strict
 */
user_pref("security.cert_pinning.enforcement_level", 2);

/*********************************************************************
 * Headers / Referers
 *********************************************************************/

/*
 * control when to send a referer:
 *    0 = always (default)
 *    1 = only if base domains match
 *    2 = only if hosts match
 */
user_pref("network.http.referer.XOriginPolicy", 2);

/*
 * control amount of information to send:
 *    0 = send full URI (default)
 *    1 = scheme+host+port+path
 *    2 = scheme+host+port
 */
user_pref("network.http.referer.XOriginTrimmingPolicy", 2);

/*********************************************************************
 * Audio/Video (WebRTC, WebGL)
 *********************************************************************/

// disable WebRTC
user_pref("media.peerconnection.enabled", false);

// force WebRTC inside the proxy
user_pref("media.peerconnection.ice.proxy_only_if_behind_proxy", true);

// force a single network interface for ICE candidates generation
user_pref("media.peerconnection.ice.default_address_only", true);

// force exclusion of private IPs from ICE candidates
user_pref("media.peerconnection.ice.no_host", true);

// disable Web Audio API
//user_pref("dom.webaudio.enabled", false);

// disable WebGL (Web Graphics Library):
user_pref("webgl.disabled", true);

/*
 * disable autoplay of HTML5 media
 *    0 = allow all
 *    1 = blocl non-muted media (default)
 *    5 = block all
 */
user_pref("media.autoplay.default", 5);

/*********************************************************************
 * Downloads
 *********************************************************************/

// always ask you where to save files:
user_pref("browser.download.useDownloadDir", false);

// disable adding downloads to system's "recent documents" list
user_pref("browser.download.manager.addToRecentDocs", false);

/*********************************************************************
 * Cookies
 *********************************************************************/

/*
 * enable ETP (Enhanced Tracking Protection)
 * ETP strict mode enables Total Cookie Protection (TCP)
 */
user_pref("browser.contentblocking.category", "strict");

// enable state partitioning of service workers
user_pref("privacy.partition.serviceWorkers", true);

/*********************************************************************
 * UI Features
 *********************************************************************/

// block popup windows
user_pref("dom.disable_open_during_load", true);

// disable Pocket extension
user_pref("extensions.pocket.enabled", false);

// disable Screenshots extension
user_pref("extensions.screenshots.disabled", true);

// disable PDFJS scripting
user_pref("pdfjs.enableScripting", false);

/*********************************************************************
 * Shutdown Settings
 *********************************************************************/

// clear history when Firefox closes
user_pref("network.cookie.lifetimePolicy", 2);
user_pref("privacy.sanitize.sanitizeOnShutdown", true);
user_pref("privacy.clearOnShutdown.cache", true);
user_pref("privacy.clearOnShutdown.cookies", true); // keep cookies
user_pref("privacy.clearOnShutdown.downloads", true);
user_pref("privacy.clearOnShutdown.formdata", true);
user_pref("privacy.clearOnShutdown.history", true); // keep history
user_pref("privacy.clearOnShutdown.offlineApps", true);
user_pref("privacy.clearOnShutdown.sessions", true);
user_pref("privacy.clearOnShutdown.sitesettings", true);
user_pref("privacy.sanitize.timeSpan", 0);

/*********************************************************************
 * Fingerprinting
 *********************************************************************/

/*
 * RFP (Resist Fingerprinting):
 *
 * can cause some website breakage: mainly canvas, use a site
 * exception via the urlbar.
 *
 * RFP also has a few side effects: mainly timezone is UTC0, and
 * websites will prefer light theme.
 * [1] https://bugzilla.mozilla.org/418986
 *
 * See: https://support.mozilla.org/en-US/kb/firefox-protection-against-fingerprinting
 */
//user_pref("privacy.resistFingerprinting", true);

// set new window size rounding max values
//user_pref("privacy.window.maxInnerWidth", 1600);
//user_pref("privacy.window.maxInnerHeight", 900);

// disable mozAddonManager Web API
//user_pref("privacy.resistFingerprinting.block_mozAddonManager", true); //Hidden pref

// disable using system colors
//user_pref("browser.display.use_system_colors", false); //Default: false (Non-Windows)
ENDOFFILE
}

nargs=$#
cmd=${1-}
rc=0
if [ "$#" -gt 0 ]; then shift; fi
case $cmd in
    arkenfox)
        [ "$nargs" -eq 1 ] || usage 1
        update_usersjs_arkenfox "$@"
        ;;
    personal)
        [ "$nargs" -eq 1 ] || usage 1
        update_usersjs_personal "$@"
        ;;
    help | --help | -h)
        usage 0
        ;;
    *)
        usage 1
        ;;
esac
exit $rc
