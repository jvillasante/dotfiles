#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
shopt -s inherit_errexit
[[ "${TRACE-0}" == "1" ]] && set -o xtrace
cd "$(dirname "$0")" || exit 1

if [[ -z "${BASH_VERSION}" ]]; then
    echo "Error: This script requires Bash to run." >&2
    exit 1
fi

if [ ! -f /etc/debian_version ]; then
    echo "Error: Debian is not running on this system, exiting..."
    exit 1
fi

if ! hash apt 2> /dev/null; then
    echo "Error: apt not installed on this system, exiting..."
    exit 1
fi

if ! hash extrepo 2> /dev/null; then
    echo "Error: extrepo not installed on this system, exiting..."
    exit 1
fi

SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_NAME

# source everything in ../lib
for f in "$(dirname "$0")"/../lib/*; do
    # shellcheck source=/dev/null
    source "$f" || exit 1
done

# source everything in ./lib
for f in "$(dirname "$0")"/lib/*; do
    # shellcheck source=/dev/null
    source "$f" || exit 1
done

readonly WS_WAYLAND="Wayland"
readonly WS_X11="X11"
readonly WM_KDE="KDE"
readonly WM_GNOME="Gnome"
WINDOW_SYSTEM="$WS_WAYLAND"
WINDOW_MANAGER="$WM_KDE"

# Debian release codename. Update on dist-upgrade (e.g., trixie -> forky).
readonly DEBIAN_RELEASE="trixie"

usage() {
    cat <<EOF
Usage:
    $SCRIPT_NAME help
        Show this help message
    $SCRIPT_NAME install [--ws Wayland|X11] [--wm KDE|Gnome]
        Install Debian System (defaults: --ws Wayland --wm KDE)

e.g: $SCRIPT_NAME install --ws Wayland --wm Gnome
EOF
    exit "$1"
}

pause() {
    read -rp "${1:-Press enter to continue...}"
}

debian_install() {
    HEIGHT=25
    WIDTH=100
    CHOICE_HEIGHT=4
    BACKTITLE="Debian Setup Util"
    TITLE="Please Make a selection"
    MENU="Please Choose one of the following options:"

    # Confirm before kicking off the installer
    read -rp "$BACKTITLE (WS=$WINDOW_SYSTEM and WM=$WINDOW_MANAGER)? (Y/N): " confirm
    if [[ ! $confirm =~ ^[yY]([eE][sS])?$ ]]; then
        echo "Aborted."
        exit 0
    fi

    # Check to see if dialog is installed, if not install it
    command -v dialog &> /dev/null || sudo apt install -y dialog

    # kick off the installer
    OPTIONS=(
        1 "Set Defaults - Set some defaults & update"
        2 "Install Firmware - Install Firmware Updates"
        3 "Install Nvidia Drivers - Install Nvidia Drivers"
        4 "Install Software - Installs a bunch of my most used software"
        5 "Install Extras - Codecs, Fonts, etc"
        6 "Enable Flatpak - Enables the Flatpak repo and installs packages"
        7 "Setup Secrets and Repos - Setup ssh and gpg from backups and get git repos"
        8 "Install Emacs - Install Emacs from source"
        9 "Install Xremap - Install Xremap"
        10 "Install PSD - Install profile-sync-daemon from source"
        11 "Quit")

    while true; do
        CHOICE=$(dialog --clear \
                        --backtitle "$BACKTITLE (WS=$WINDOW_SYSTEM and WM=$WINDOW_MANAGER)" \
                        --title "$TITLE" \
                        --nocancel \
                        --menu "$MENU" \
                        $HEIGHT $WIDTH $CHOICE_HEIGHT \
                        "${OPTIONS[@]}" 2>&1 > /dev/tty)
        clear
        case $CHOICE in
            1)
                echo "$CHOICE) Setting Defaults"
                (
                    # hostname
                    read -rp "Enter pretty hostname (defaults to 'Julio's Personal Laptop'): " HOSTNAME_PRETTY
                    [ -z "$HOSTNAME_PRETTY" ] && HOSTNAME_PRETTY="Julio's Personal Laptop"
                    sudo hostnamectl set-hostname --pretty "$HOSTNAME_PRETTY"

                    read -rp "Enter static hostname (defaults to 'debian-xps-9710'): " HOSTNAME_STATIC
                    [ -z "$HOSTNAME_STATIC" ] && HOSTNAME_STATIC="debian-xps-9710"
                    sudo hostnamectl set-hostname --static "$HOSTNAME_STATIC"

                    # Sources
                    [ -f /etc/apt/sources.list ] && \
                        sudo cp /etc/apt/sources.list /etc/apt/sources.list.bak
                    [ ! -f /etc/apt/sources.list ] && \
                        sudo touch /etc/apt/sources.list
                    cat << EOF | sudo tee /etc/apt/sources.list > /dev/null
# Sources moved to /etc/apt/sources.list.d/debian.sources (deb822 format).
# deb http://deb.debian.org/debian $DEBIAN_RELEASE main contrib non-free
# deb http://deb.debian.org/debian-security ${DEBIAN_RELEASE}-security main contrib non-free
EOF
                    [ -f /etc/apt/sources.list.d/debian.sources ] && \
                        sudo cp /etc/apt/sources.list.d/debian.sources /etc/apt/sources.list.d/debian.sources.bak
                    [ ! -f /etc/apt/sources.list.d/debian.sources ] && \
                        sudo touch /etc/apt/sources.list.d/debian.sources
                    cat << EOF | sudo tee /etc/apt/sources.list.d/debian.sources > /dev/null
# Debian $DEBIAN_RELEASE
Types: deb deb-src
URIs: https://deb.debian.org/debian
Suites: $DEBIAN_RELEASE ${DEBIAN_RELEASE}-updates
Components: main non-free-firmware contrib non-free
Enabled: yes
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg

# Security Updates
Types: deb deb-src
URIs: https://security.debian.org/debian-security
Suites: ${DEBIAN_RELEASE}-security
Components: main non-free-firmware contrib non-free
Enabled: yes
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg
EOF

                    # xdg
                    export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
                    export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
                    export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
                    export XDG_LIB_HOME="${XDG_LIB_HOME:-$HOME/.local/lib}"
                    export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
                    mkdir -p \
                          "${XDG_CACHE_HOME}" \
                          "${XDG_CONFIG_HOME}" \
                          "${XDG_DATA_HOME}" \
                          "${XDG_LIB_HOME}" \
                          "${XDG_STATE_HOME}"

                    # Folder structure
                    mkdir -p "$HOME/.local/bin"
                    mkdir -p "$HOME/Workspace"
                    mkdir -p "$HOME/Workspace/Books"
                    mkdir -p "$HOME/Workspace/Private/Projects"
                    mkdir -p "$HOME/Workspace/Public"
                    mkdir -p "$HOME/Workspace/Software"
                    mkdir -p "$HOME/Workspace/Stuff"
                    mkdir -p "$HOME/Workspace/Work"
                    mkdir -p "$HOME/Workspace/Work/Projects"
                    mkdir -p "$HOME/Workspace/Work/Software"
                    mkdir -p "$HOME/Workspace/Work/Stuff"

                    # Gnome Gsettings - dconf-editor
                    #  - View current settings - gsettings list-recursively org.gnome.desktop.interface
                    #  - Reset to default setting - gsettings reset org.gnome.desktop.interface enable-animations
                    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                        # Clock Settings
                        gsettings set org.gnome.desktop.interface clock-format '12h'
                        gsettings set org.gnome.desktop.interface clock-show-date true
                        gsettings set org.gnome.desktop.interface clock-show-seconds false
                        gsettings set org.gnome.desktop.interface clock-show-weekday false

                        # No hot corners
                        gsettings set org.gnome.desktop.interface enable-hot-corners false

                        # Emacs please
                        gsettings set org.gnome.desktop.interface gtk-key-theme 'Emacs'

                        # Show battery
                        gsettings set org.gnome.desktop.interface show-battery-percentage true

                        # Enable window buttons
                        gsettings set org.gnome.desktop.wm.preferences button-layout ':minimize,maximize,close'

                        # Set new windows centered
                        gsettings set org.gnome.mutter center-new-windows true

                        # Set list-view for Nautilius
                        gsettings set org.gnome.nautilus.preferences default-folder-viewer 'list-view'

                        # Force alt + tab to switch only on current workspace
                        gsettings set org.gnome.shell.app-switcher current-workspace-only true

                        # Allow max volume
                        gsettings set org.gnome.desktop.sound allow-volume-above-100-percent true

                        # Wayland - Set fractional scaling to 1.1 (110%), default is 1.0 (100%)
                        # if [[ "$WINDOW_SYSTEM" == "$WS_WAYLAND" ]]; then
                        #     gsettings set org.gnome.desktop.interface text-scaling-factor 1.1
                        # fi

                        # Stop Gnome Software downloading updates
                        # gsettings set org.gnome.software allow-updates false
                        # gsettings set org.gnome.software download-updates false
                        # gsettings set org.gnome.software download-updates-notify false

                        # Disable Gnome Software from Startup Apps
                        [ -f /etc/xdg/autostart/org.gnome.Software.desktop ] && \
                            sudo mv /etc/xdg/autostart/org.gnome.Software.desktop /etc/xdg/autostart/org.gnome.Software.desktop.backup
                    fi

                    # Faster Boot
                    sudo systemctl disable NetworkManager-wait-online.service

                    # Update system with the new sources
                    sudo extrepo update
                    sudo apt update -y
                    sudo apt upgrade -y
                    sudo apt full-upgrade -y
                    sudo apt autoremove -y
                    sudo apt autoclean -y

                    # Enable automatic security updates. unattended-upgrades runs
                    # daily via systemd timer; default config takes only security
                    # updates. Adjust /etc/apt/apt.conf.d/50unattended-upgrades to
                    # broaden scope if desired.
                    sudo apt install -y unattended-upgrades
                    sudo dpkg-reconfigure -f noninteractive -plow unattended-upgrades

                    # needrestart: after package upgrades (including unattended
                    # ones) flags daemons still running with old library
                    # versions. Pairs with unattended-upgrades — security
                    # patches to glibc / openssl / etc. don't take effect until
                    # the affected services restart, and needrestart tells you
                    # which need it.
                    sudo apt install -y needrestart
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Defaults has been set. Reboot may be required. Press enter to continue..."
                ;;
            2)
                echo "$CHOICE) Installing Firmware Updates"
                (
                    # Pick fwupd vs fwupd-signed based on Secure Boot state.
                    # With SB on, capsule updates require a signed shim/grub
                    # path; fwupd-signed is signed by Debian's UEFI key.
                    if mokutil --sb-state 2> /dev/null | grep -q "SecureBoot enabled"; then
                        echo "Secure Boot is enabled — installing fwupd-signed"
                        sudo apt install -y fwupd-signed
                    else
                        sudo apt install -y fwupd
                    fi

                    # Kernel firmware *packages* (loaded by the kernel at boot — not LVFS)
                    sudo apt install -y \
                         linux-headers-"$(dpkg --print-architecture)" \
                         firmware-linux \
                         firmware-misc-nonfree

                    # CPU microcode updates (security-relevant). Pull the
                    # vendor-specific package based on /proc/cpuinfo.
                    if grep -qi 'vendor.*intel' /proc/cpuinfo; then
                        sudo apt install -y intel-microcode
                    elif grep -qi 'vendor.*amd' /proc/cpuinfo; then
                        sudo apt install -y amd64-microcode
                    fi

                    # Device firmware updates from LVFS (BIOS/UEFI, NIC, NVMe,
                    # Thunderbolt, etc). UEFI/Thunderbolt updates apply at next
                    # boot via UEFI capsule — reboot needed afterwards.
                    sudo fwupdmgr refresh --force --assume-yes
                    sudo fwupdmgr update --assume-yes --no-reboot-check
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. Reboot to apply any pending UEFI/firmware updates. Press enter to continue..."
                ;;
            3)
                echo "$CHOICE) Installing Nvidia Drivers (https://wiki.debian.org/NvidiaGraphicsDrivers)"
                echo "  Note: Open kernel modules require Turing or newer GPU"
                echo "        (RTX 20-series / GTX 16-series and up)."
                (
                    # Prerequisites
                    sudo apt install -y build-essential
                    sudo apt install -y linux-headers-"$(dpkg --print-architecture)"

                    # NVIDIA driver paths on Debian Trixie:
                    # - Proprietary: nvidia-driver pulls in nvidia-kernel-dkms
                    #   sudo apt install -y nvidia-driver firmware-misc-nonfree
                    # - Open kernel modules (Turing+): install nvidia-open-kernel-dkms
                    #   first so nvidia-driver's "kernel module" dep is satisfied
                    #   without pulling proprietary.

                    sudo apt install -y nvidia-open-kernel-dkms nvidia-driver firmware-misc-nonfree

                    # Verify the kernel module built before rebooting:
                    #   dkms status
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Nvidia Drivers installed. Verify with 'dkms status', then reboot. Press enter to continue..."
                ;;
            4)
                echo "$CHOICE) Installing Software"
                (
                    # Remove Firefox ESR
                    sudo apt remove --purge --yes firefox-esr*
                    sudo apt autoremove -y

                    pkgs=(
                        # General
                        build-essential binutils coreutils openssl libssl-dev
                        tar p7zip-full zip unzip rsync rar unrar atool
                        silversearcher-ag aspell aspell-en aspell-es autojump autoconf automake
                        bat curl dnsutils dos2unix doxygen fd-find font-manager
                        btop htop nvtop isync libpcap-dev make mercurial msmtp fastfetch
                        vim neovim pandoc pass poppler-utils poppler-data ripgrep
                        scdaemon subversion telnet tree w3m wget
                        wordnet imagemagick libtool
                        shfmt editorconfig glslang-tools glslang-dev shellcheck parallel
                        tidy sqlite3 pkg-config bison flex
                        cmake ninja-build ccache meson
                        valgrind minicom mc strace
                        ffmpegthumbnailer mediainfo
                        feh mpv qrencode findutils locate
                        ncurses-term
                        gnuplot
                        libreoffice

                        # Python
                        python-is-python3 python3-pip

                        # Git
                        git
                        # git-email   # support for email based workflow
                        # difftastic  # better diffs
                        # git-delta   # better diffs
                        # lazygit     # terminal git

                        # Terminals / multiplexers
                        tmux
                        # alacritty

                        # FZF
                        fzf

                        # gpg / yubikey
                        gpg gnupg2 gnupg-pkcs11-scd pcsc-tools opensc vsmartcard-vpcd
                        yubikey-manager

                        # ncdu / borg
                        ncdu borgbackup

                        # latex base
                        texlive-base texinfo

                        # Power management (PPD provides perf/balanced/power-saver
                        # toggle integrated with KDE/GNOME — no need for TLP).
                        power-profiles-daemon

                        # Disk health monitoring. Check NVMe/SATA wear,
                        # temperature, error counts, hours powered on:
                        #   sudo smartctl -a /dev/nvme0n1
                        #   sudo smartctl -H /dev/sda          # quick health
                        #   sudo smartctl -t short /dev/sda    # run self-test
                        smartmontools

                        # "command-not-found" hint when typing unknown commands
                        # (suggests the package that provides them). Refresh
                        # the index periodically: sudo update-command-not-found
                        command-not-found

                        # apt-file: find which package provides a file (works
                        # for uninstalled packages too — unlike `dpkg -L`).
                        # Useful when building from source and missing headers:
                        #   apt-file search curl/curl.h   # → libcurl4-openssl-dev
                        #   apt-file list <pkg>           # files a package would install
                        apt-file
                    )

                    # Intel thermal daemon — prevents Tjmax throttling under
                    # sustained load on Intel laptops. Skip on AMD.
                    if grep -qi 'vendor.*intel' /proc/cpuinfo; then
                        pkgs+=(thermald)
                    fi

                    # Wayland Software
                    if [[ "$WINDOW_SYSTEM" == "$WS_WAYLAND" ]]; then
                        pkgs+=(wl-clipboard wayland-utils foot)
                    fi

                    # X11 Software
                    if [[ "$WINDOW_SYSTEM" == "$WS_X11" ]]; then
                        pkgs+=(xsel xclip xinput xdotool x11-utils)
                    fi

                    # Gnome Software
                    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                        pkgs+=(dconf-editor gnome-tweaks)
                    fi

                    # Plasma Software
                    if [[ "$WINDOW_MANAGER" == "$WM_KDE" ]]; then
                        pkgs+=(kdeconnect)
                        # kdotool yakuake latte-dock kazam
                    fi

                    sudo apt install -y "${pkgs[@]}"

                    # Populate apt-file's index database (one-time download of
                    # ~50MB; refresh periodically with `sudo apt-file update`).
                    sudo apt-file update

                    # gpg smartcard daemon
                    sudo systemctl enable --now pcscd.service

                    # starship (https://starship.rs/)
                    curl -sS https://starship.rs/install.sh | sh

                    # Browsers via extrepo (Debian's curated third-party repos).
                    # `extrepo enable <name>` writes the source to
                    # /etc/apt/sources.list.d/extrepo_<name>.sources and trusts
                    # the bundled key.
                    sudo extrepo enable mozilla
                    sudo extrepo enable brave_release
                    sudo extrepo update
                    sudo apt update
                    sudo apt install -y firefox brave-browser

                    # Fingerprint reader (XPS Plus configurations have a Goodix
                    # sensor; base XPS does not). To enable:
                    # sudo apt install -y fprintd libpam-fprintd
                    # sudo pam-auth-update --enable fprintd
                    # fprintd-enroll   # walk through enrollment per finger

                    # TODO: Install Node.js from Extrepo (sudo extrepo enable nodesource)
                    # TODO: Install LLVM from Extrepo (sudo extrepo enable llvm-toolchain-XX)

                    # non apt software
                    # TODO: Python packages (use pipx instead of pip + --break-system-packages)
                    # sudo apt install -y pipx
                    # pipx install cmake-language-server
                    # pipx install pyright
                    # TODO: Node packages
                    # sudo npm install --location=global npm@latest
                    # sudo npm install --location=global prettier
                    # sudo npm install --location=global js-beautify
                    # sudo npm install --location=global typescript-language-server typescript
                    # sudo npm install --location=global dockerfile-language-server-nodejs
                    # sudo npm install --location=global bash-language-server
                    # sudo npm install --location=global @devcontainers/cli
                    # sudo npm install --location=global @google/gemini-cli
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Software has been installed. Press enter to continue..."
                ;;
            5)
                echo "$CHOICE) Installing Extras"
                (
                    sudo apt update
                    sudo apt upgrade -y

                    # non-free codecs and other media components
                    sudo apt install -y \
                         ffmpeg \
                         libavcodec-extra \
                         ttf-mscorefonts-installer \
                         libdvdcss2 \
                         libdvd-pkg \
                         ffmpegthumbnailer \
                         ffmpegthumbs \
                         gstreamer1.0-plugins-bad \
                         gstreamer1.0-plugins-ugly \
                         gstreamer1.0-libav \
                         gstreamer1.0-tools \
                         gstreamer1.0-vaapi \
                         tumbler-plugins-extra \
                         kdegraphics-thumbnailers
                    sudo dpkg-reconfigure libdvd-pkg

                    # Hardware video acceleration (VA-API) + diagnostics.
                    # After install, run `vainfo` to verify supported decode
                    # profiles (H.264/HEVC/AV1).
                    if grep -qi 'vendor.*intel' /proc/cpuinfo; then
                        sudo apt install -y intel-media-va-driver-non-free
                    elif grep -qi 'vendor.*amd' /proc/cpuinfo; then
                        sudo apt install -y mesa-va-drivers
                    fi
                    sudo apt install -y vainfo vulkan-tools mesa-vulkan-drivers

                    # VPN tooling (CLI; NetworkManager plugins handle GUI)
                    sudo apt install -y wireguard-tools openvpn

                    # Bluetooth audio codecs beyond default SBC. Verify the
                    # active codec on a connected device with `pactl list sinks`.
                    sudo apt install -y \
                         libfreeaptx0 \
                         libldacbt-abr2 libldacbt-enc2 \
                         libfdk-aac2

                    # Firewall (ufw)
                    sudo apt install -y ufw
                    sudo ufw allow ssh
                    sudo ufw allow http
                    sudo ufw allow https
                    sudo ufw enable
                    sudo ufw status

                    # Docker (https://docs.docker.com/engine/install/debian/) — using podman instead
                    sudo apt install -y podman

                    # Note: texlive-full is ~5GB. Swap for texlive (medium) or
                    # texlive-base if disk space matters.
                    sudo apt install -y texlive-full

                    # Steam extras
                    # sudo apt install -y steam-devices

                    # Fonts
                    sudo apt install -y fonts-jetbrains-mono

                    read -rp "Install Berkeley Mono Fonts (gpg encrypted downloaded fonts)? (Y/N): " confirm
                    if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
                        install_berkeley_mono_font
                    fi

                    # Remove pre-installed Gnome games
                    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                        sudo apt purge -y iagno lightsoff four-in-a-row gnome-robots pegsolitaire gnome-2048 \
                             hitori gnome-klotski gnome-mines gnome-mahjongg gnome-sudoku quadrapassel swell-foop \
                             gnome-tetravex gnome-taquin aisleriot gnome-chess five-or-more gnome-nibbles tali
                        sudo apt autoremove -y
                    fi

                    # Set Gnome fonts
                    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                        gsettings set org.gnome.desktop.interface font-hinting 'full'
                        gsettings set org.gnome.desktop.interface font-antialiasing 'rgba'
                        gsettings set org.gnome.desktop.interface font-name 'Inter Variable 11'
                        gsettings set org.gnome.desktop.interface document-font-name 'Inter 11'
                        gsettings set org.gnome.desktop.interface monospace-font-name 'Berkeley Mono 11'
                        # gsettings set org.gnome.desktop.interface text-scaling-factor 1.25 # using fractional scaling instead!!
                    fi
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Extras installed. Press enter to continue..."
                ;;

            6)
                echo "$CHOICE) Enabling Flatpak"
                (
                    sudo apt install -y flatpak
                    flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
                    sudo flatpak update

                    if [[ "$WINDOW_MANAGER" == "$WM_KDE" ]]; then
                        sudo apt install -y plasma-discover-backend-flatpak
                    fi

                    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                        sudo apt install -y gnome-software-plugin-flatpak
                        flatpak install --user --noninteractive flathub org.gnome.TextEditor
                        flatpak install --user --noninteractive flathub org.gnome.Firmware
                        flatpak install --user --noninteractive flathub com.mattjakeman.ExtensionManager
                    fi
                    flatpak install --user --noninteractive flathub com.discordapp.Discord
                    flatpak install --user --noninteractive flathub com.dropbox.Client
                    flatpak install --user --noninteractive flathub com.slack.Slack
                    flatpak install --user --noninteractive flathub org.videolan.VLC
                    flatpak install --user --noninteractive flathub org.wireshark.Wireshark
                    flatpak install --user --noninteractive flathub com.github.tchx84.Flatseal
                    flatpak install --user --noninteractive flathub com.github.johnfactotum.Foliate
                    flatpak install --user --noninteractive flathub org.gimp.GIMP
                    flatpak install --user --noninteractive flathub com.transmissionbt.Transmission
                    flatpak install --user --noninteractive flathub org.telegram.desktop
                    # flatpak install --user --noninteractive flathub com.valvesoftware.Steam
                    # flatpak install --user --noninteractive org.freedesktop.Platform.VulkanLayer.MangoHud # this is needed for steam!
                    # flatpak install --user --noninteractive flathub io.github.kolunmi.Bazaar
                    # flatpak install --user --noninteractive flathub io.podman_desktop.PodmanDesktop
                    # flatpak install --user --noninteractive flathub org.keepassxc.KeePassXC
                    # flatpak install --user --noninteractive flathub com.borgbase.Vorta
                    # flatpak install --user --noninteractive flathub io.github.Hexchat
                    # flatpak install --user --noninteractive flathub engineer.atlas.Nyxt
                    # flatpak install --user --noninteractive flathub org.mozilla.firefox
                    # flatpak install --user --noninteractive flathub org.gnucash.GnuCash
                    # flatpak install --user --noninteractive flathub im.riot.Riot
                    # flatpak install --user --noninteractive flathub com.obsproject.Studio
                    # flatpak install --user --noninteractive flathub com.brave.Browser
                    # flatpak install --user --noninteractive flathub org.shotcut.Shotcut
                    # flatpak install --user --noninteractive flathub dev.geopjr.Tuba
                    # flatpak install --user --noninteractive flathub io.github.mimbrero.WhatsAppDesktop
                    # flatpak install --user --noninteractive flathub org.signal.Signal
                    # flatpak install --user --noninteractive flathub com.microsoft.Edge
                    # flatpak install --user --noninteractive flathub com.visualstudio.code
                    # flatpak install --user --noninteractive flathub com.github.GradienceTeam.Gradience
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Flatpak has now been enabled. Press enter to continue..."
                ;;
            7)
                echo "$CHOICE) Setting up secrets and repos"
                ( setup_secrets_and_repos ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Secrets and repos are set. Press enter to continue..."
                ;;
            8)
                echo "$CHOICE) Installing Emacs"
                if hash emacs 2> /dev/null; then
                    echo "Emacs is already installed"
                else
                    (
                        # libgccjit-XX should match the current gcc in the system (gcc --version)
                        # echo "Current gcc version $(gcc --version)"
                        # read -rp "Is gcc version 14.*? (Y/N): " confirm &&
                        #     [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]] || exit 1

                        # Install dependencies
                        sudo apt build-dep -y emacs
                        # sudo apt install -y build-essential autoconf automake libtool texinfo libgtk-3-dev libxpm-dev \
                            #      libjpeg-dev libgif-dev libtiff5-dev gnutls-bin libncurses-dev libxml2-dev libgpm-dev \
                            #      libdbus-1-dev libgtk2.0-dev libpng-dev libotf-dev libm17n-dev librsvg2-dev libmagickcore-dev \
                            #      libmagickwand-dev libglib2.0-dev libgirepository1.0-dev
                        # sudo apt install -y libgccjit0 libgccjit-14-dev # make sure is the same version as `gcc --version`
                        # sudo apt install -y libtree-sitter0.22 libtree-sitter-dev
                        # sudo apt install -y libjansson4 libjansson-dev
                        # sudo apt install -y libvterm0 libvterm-dev
                        # sudo apt install -y libwebp-dev webp
                        # sudo apt install -y libxft-dev libxft2
                        # sudo apt install -y libenchant-2-dev pkgconf # necessary for jinx
                        # sudo apt install -y libtool-bin # vterm

                        # do install emacs from source
                        install_emacs
                    ) || echo "Step $CHOICE had errors (continuing)..."
                fi
                pause "$CHOICE) Emacs has been installed. Press enter to continue..."
                ;;
            9)
                echo "$CHOICE) Installing Xremap"
                if hash xremap 2> /dev/null; then
                    echo "Xremap is already installed"
                else
                    (
                        [[ "$WINDOW_SYSTEM" == "$WS_X11" ]] && sudo apt install -y libx11-dev

                        # do install
                        install_xremap

                        # First create a new group to which we allow access to the input stuff; add this group to your user:
                        sudo gpasswd -a "$USER" input

                        # Second Create new udev rule granting access:
                        sudo cp -f "$HOME"/Workspace/Public/dotfiles/Common/udev/70-xremap.rules \
                             /etc/udev/rules.d/70-xremap.rules

                        # Enable the daemon
                        [ ! -d "$HOME"/.config/systemd/user ] && mkdir -p "$HOME"/.config/systemd/user
                        [ -L "$HOME/.config/systemd/user/xremap.service" ] &&
                            unlink "$HOME/.config/systemd/user/xremap.service"
                        ln -s "$HOME/Workspace/Public/dotfiles/Common/systemd/user/xremap.service" \
                           "$HOME/.config/systemd/user"
                        systemctl --user daemon-reload
                        systemctl --user enable --now xremap.service
                    ) || echo "Step $CHOICE had errors (continuing)..."
                fi
                pause "$CHOICE) Xremap installed. Reboot for udev rules to take effect.
                              On Gnome, install the extension at https://extensions.gnome.org/extension/5060/xremap/.
                              Press enter to continue..."
                ;;
            10)
                echo "$CHOICE) Installing PSD (profile-sync-daemon)"
                (
                    # TODO: glib2 not available on Debian

                    # Dependencies
                    # sudo apt install -y coreutils findutils glib2 kmod rsync systemd

                    # Install
                    # install_psd
                    :
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            11)
                exit 0
                ;;
        esac

    done
}

main() {
    cmd=${1-}
    if [ "$#" -gt 0 ]; then shift; fi
    case $cmd in
        install)
            while [ "$#" -gt 0 ]; do
                case $1 in
                    --ws)
                        WINDOW_SYSTEM="${2-}"
                        [ -z "$WINDOW_SYSTEM" ] && echo "Error: --ws requires a value" >&2 && usage 1
                        shift 2
                        ;;
                    --wm)
                        WINDOW_MANAGER="${2-}"
                        [ -z "$WINDOW_MANAGER" ] && echo "Error: --wm requires a value" >&2 && usage 1
                        shift 2
                        ;;
                    *)
                    echo "Error: unknown argument: $1" >&2
                    usage 1
                    ;;
                esac
            done
            case $WINDOW_SYSTEM in
                "$WS_WAYLAND" | "$WS_X11") ;;
                *)
                echo "Error: --ws must be one of: $WS_WAYLAND, $WS_X11" >&2
                usage 1
                ;;
            esac
            case $WINDOW_MANAGER in
                "$WM_KDE" | "$WM_GNOME") ;;
                *)
                echo "Error: --wm must be one of: $WM_KDE, $WM_GNOME" >&2
                usage 1
                ;;
            esac
            debian_install
            ;;
        help | --help | -h)
            usage 0
            ;;
        *)
        usage 1
        ;;
    esac
}

main "$@"
