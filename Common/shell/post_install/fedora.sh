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

if [ ! -f /etc/fedora-release ]; then
    echo "Error: Fedora is not running on this system." >&2
    exit 1
fi

if ! hash dnf 2> /dev/null; then
    echo "Error: dnf not installed on this system." >&2
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

usage() {
    cat <<EOF
Usage:
    $SCRIPT_NAME help
        Show this help message
    $SCRIPT_NAME install [--ws Wayland|X11] [--wm KDE|Gnome]
        Install Fedora System (defaults: --ws Wayland --wm KDE)

e.g: $SCRIPT_NAME install --ws Wayland --wm Gnome
EOF
    exit "$1"
}

pause() {
    read -rp "${1:-Press enter to continue...}"
}

fedora_install() {
    HEIGHT=25
    WIDTH=100
    CHOICE_HEIGHT=4
    BACKTITLE="Fedora Setup Util"
    TITLE="Please Make a selection"
    MENU="Please Choose one of the following options:"

    # Confirm before kicking off the installer
    read -rp "$BACKTITLE (WS=$WINDOW_SYSTEM and WM=$WINDOW_MANAGER)? (Y/N): " confirm
    if [[ ! $confirm =~ ^[yY]([eE][sS])?$ ]]; then
        echo "Aborted."
        exit 0
    fi

    # Check to see if Dialog is installed, if not install it
    rpm -q dialog &> /dev/null || sudo dnf install -y dialog

    OPTIONS=(
        1 "Setup Defaults - Set some defaults (hostname, folders structure, global settings, etc)"
        2 "Setup RPM Fusion - Update Kernel, RPM Fusion & Firmware (reboot needed)"
        3 "Install NVIDIA - Install NVIDIA Drivers (reboot needed)"
        4 "Install Software - Installs a bunch of my most used software"
        5 "Install Extras - Themes, Fonts and Codecs"
        6 "Enable Flatpak - Enables the Flatpak repo and installs packages"
        7 "Setup Secrets and Repos - Setup ssh and gpg from backups and get git repos"
        8 "Install Emacs - Install Emacs"
        9 "Install StyLua - Install StyLua from source"
        10 "Install App Launcher - Install App Launcher"
        11 "Install PSD - Install PSD (profile-sync-daemon)"
        12 "Install Xremap - Install Xremap from source"
        13 "Install Harper - The English grammar checker designed to be just right"
        14 "Quit")

    while true; do
        CHOICE=$(dialog --clear \
                        --backtitle "$BACKTITLE" \
                        --title "$TITLE" \
                        --nocancel \
                        --menu "$MENU" \
                        $HEIGHT $WIDTH $CHOICE_HEIGHT \
                        "${OPTIONS[@]}" \
                        2>&1 > /dev/tty)

        clear
        case $CHOICE in
            1)
                echo "$CHOICE) Setting Defaults"
                (
                    # hostname
                    read -r -p "Enter pretty hostname (defaults to 'Julio's Personal Laptop'): " HOSTNAME_PRETTY
                    [ -z "$HOSTNAME_PRETTY" ] && HOSTNAME_PRETTY="Julio's Personal Laptop"
                    sudo hostnamectl set-hostname --pretty "$HOSTNAME_PRETTY"

                    read -r -p "Enter static hostname (defaults to 'fedora-xps-9710'): " HOSTNAME_STATIC
                    [ -z "$HOSTNAME_STATIC" ] && HOSTNAME_STATIC="fedora-xps-9710"
                    sudo hostnamectl set-hostname --static "$HOSTNAME_STATIC"

                    # XDG
                    export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
                    export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
                    export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
                    export XDG_STATE_HOME="${XDG_STATE_HOME:-$HOME/.local/state}"
                    export XDG_BIN_HOME="${XDG_BIN_HOME:-$HOME/.local/bin}"
                    export XDG_LIB_HOME="${XDG_LIB_HOME:-$HOME/.local/lib}"
                    mkdir -p \
                          "${XDG_CACHE_HOME}" \
                          "${XDG_CONFIG_HOME}" \
                          "${XDG_DATA_HOME}" \
                          "${XDG_STATE_HOME}" \
                          "${XDG_BIN_HOME}" \
                          "${XDG_LIB_HOME}"

                    # Folder structure
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

                        # Wayland - Set fractional scaling
                        # if [[ "$WINDOW_SYSTEM" == "$WS_WAYLAND" ]]; then
                        #     gsettings set org.gnome.mutter experimental-features "['scale-monitor-framebuffer']"
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
                    sudo systemctl disable --now NetworkManager-wait-online.service

                    # Setup dnf configs and update
                    if ! grep -q '^max_parallel_downloads=' /etc/dnf/dnf.conf; then
                        echo 'max_parallel_downloads=10' | sudo tee -a /etc/dnf/dnf.conf
                    fi
                    sudo dnf upgrade -y --refresh
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            2)
                echo "$CHOICE) Setting up RPM Fusion and Updating Firmware"
                (
                    # update kernel
                    sudo dnf install -y gcc kernel-headers kernel-devel

                    # Enable RPM Fusion
                    sudo dnf install -y \
                         https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-"$(rpm -E %fedora)".noarch.rpm \
                         https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-"$(rpm -E %fedora)".noarch.rpm

                    # Upgrade everything
                    sudo dnf upgrade -y --refresh
                    sudo dnf group upgrade -y core

                    # Terra (https://terra.fyralabs.com/)
                    # sudo dnf install -y --nogpgcheck --repofrompath 'terra,https://repos.fyralabs.com/terra$releasever' terra-release

                    # Enable tainted repos and pull kernel firmware blobs
                    # (broadcom wifi, microcode, etc.) — these are the *-firmware
                    # *packages*, loaded by the kernel at boot. They are NOT the
                    # same as device firmware updates from LVFS (handled below
                    # via fwupdmgr — BIOS/UEFI, NIC, NVMe, Thunderbolt, etc.).
                    sudo dnf install -y rpmfusion-free-release-tainted
                    sudo dnf install -y rpmfusion-nonfree-release-tainted
                    sudo dnf install -y dnf-plugins-core
                    sudo dnf install -y \*-firmware

                    # Update device firmware from LVFS. `update` is interactive
                    # by default (prompts per update); pass --assume-yes for
                    # unattended runs. UEFI/Thunderbolt updates apply at next
                    # boot via UEFI capsule, so a reboot is needed afterwards.
                    sudo dnf install -y fwupd
                    sudo fwupdmgr refresh --force --assume-yes
                    sudo fwupdmgr update --assume-yes --no-reboot-check
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. Reboot to apply any pending UEFI/firmware updates. Press enter to continue..."
                ;;
            3)
                echo "$CHOICE) Installing NVIDIA Drivers."
                echo "  Note: requires Turing or newer GPU (RTX 20-series / GTX 16-series and up)."
                echo "  Note: either disable Secure Boot in the BIOS, or enroll a MOK with:"
                echo "        sudo kmodgenca -a && sudo mokutil --import /etc/pki/akmods/certs/public_key.der"
                echo "  Check Secure Boot state with: mokutil --sb-state"
                (
                    # Install kernel headers and dev tools
                    sudo dnf install -y kernel-devel kernel-headers gcc make \
                         libglvnd-glx libglvnd-opengl libglvnd-devel pkgconfig

                    # NVIDIA - Install Proprietary Drivers (Maxwell/Pascal/Volta or older)
                    # sudo dnf install -y akmod-nvidia xorg-x11-drv-nvidia-cuda nvidia-settings

                    # NVIDIA - Install Open Drivers (Turing or newer)
                    sudo dnf install -y akmod-nvidia-open xorg-x11-drv-nvidia-cuda nvidia-settings

                    # NVIDIA hardware video accel (VAAPI passthrough for browsers/mpv)
                    sudo dnf install -y --allowerasing --skip-unavailable nvidia-vaapi-driver

                    # Wait for the akmod kernel module to finish building so the
                    # user doesn't reboot into nouveau / a black screen.
                    # Watch progress in another terminal with: journalctl -f -u akmods
                    echo "Waiting for nvidia kernel module to build (typically 1-3 min)..."
                    timeout=600
                    while ! modinfo -F version nvidia &> /dev/null; do
                        sleep 5
                        timeout=$((timeout - 5))
                        if [ "$timeout" -le 0 ]; then
                            echo "Timed out waiting for akmod build. Check: journalctl -u akmods" >&2
                            break
                        fi
                    done
                    modinfo -F version nvidia &> /dev/null && \
                        echo "nvidia module built: $(modinfo -F version nvidia)"
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. *Important* - Reboot to load the nvidia driver. Press enter to continue..."
                ;;
            4)
                echo "$CHOICE) Installing Software"
                (
                    sudo dnf install -y dnf-plugins-core copr-cli
                    sudo dnf group install -y "development-tools" "c-development"

                    pkgs=(
                        # general
                        ripgrep fd-find util-linux-user xprop xwininfo
                        aspell aspell-en aspell-es autojump atool autoconf automake bat cmake vim
                        freetype-devel fontconfig-devel libxcb-devel libxkbcommon-devel
                        dnsutils dos2unix doxygen msmtp fastfetch
                        graphviz mercurial ninja-build nodejs npm python3 python3-pip pipx
                        multimarkdown pandoc poppler poppler-utils poppler-data
                        subversion tldr tree w3m lynx wget libtool texinfo
                        wordnet shfmt editorconfig glslang ShellCheck parallel
                        llvm clang clang-tools-extra libpcap libpcap-devel
                        pkg-config flex bison tar unar unrar unzip p7zip p7zip-plugins
                        ImageMagick ImageMagick-devel ffmpegthumbnailer mediainfo
                        sqlite sqlite-devel curl libcurl libcurl-devel
                        valgrind minicom mc strace tidy
                        libatomic libatomic-static libunwind libunwind-devel
                        gperftools gperftools-libs gperftools-devel
                        feh mpv ncurses-term ccache meson
                        zlib-devel bzip2 bzip2-devel readline-devel xz xz-devel
                        libffi-devel findutils tk-devel libyaml-devel
                        cowsay fortune-mod gnuplot telnet rlwrap nmap-ncat
                        libgcrypt libgcrypt-devel

                        # Tiny Emacs-like editor
                        mg

                        # System Monitor
                        htop smem
                        # atop below btop nvtop

                        # PDF
                        mupdf mupdf-devel mupdf-libs

                        # IO
                        libaio libaio-devel
                        liburing liburing-devel

                        # GTK
                        gtk3 gtk3-devel
                        gtk4 gtk4-devel
                        gtk-layer-shell gtk-layer-shell-devel
                        gtk4-layer-shell gtk4-layer-shell-devel

                        # Pass (password-store)
                        pass
                        # gopass

                        # Git stuff
                        git
                        # git-email   # support for email based workflow
                        # difftastic  # better diffs
                        # git-delta   # better diffs

                        # dtach: emulates the detach feature of screen
                        # dtach

                        # tmux: A terminal multiplexer
                        tmux

                        # gpg
                        gpg gnupg2 gnupg2-scdaemon gnupg-pkcs11-scd pcsc-tools opensc pcsc-lite-ccid
                        pinentry-emacs pinentry-tty

                        # yubikey
                        yubikey-manager

                        # open{ssl,ssh}
                        openssh openssl openssl-devel

                        # libssh
                        libssh libssh-devel

                        # mail
                        isync mu maildir-utils gnutls gnutls-devel

                        # ledger (https://ledger-cli.org/)
                        ledger

                        # ncdu (text-based disk usage viewer)
                        ncdu

                        # Borg Backup
                        borgbackup

                        # Command Line Fuzzy Finder
                        fzf

                        # Emacs (compiled from source instead — see option 8)
                        # emacs
                        # libvterm libvterm-devel # necessary for vterm
                        # enchant2-devel pkgconf  # necessary for jinx

                        # Neovim
                        neovim

                        # Firewall GUI to manage firewalld
                        firewall-config

                        # Needed for keychrome firmware flash
                        dfu-util

                        # Power management (PPD provides perf/balanced/power-saver
                        # toggle integrated with KDE/GNOME — no need for TLP).
                        power-profiles-daemon

                        # alacritty - A fast, cros-platform, OpenGL terminal emulator
                        # alacritty

                        # Bleachbit: Clean Your System and Free Disk Space
                        # bleachbit

                        # Go
                        # go

                        # Torrents
                        # qbittorrent

                        # grammar linter (python)
                        # proselint

                        # ebook reader/converver
                        # calibre
                    )

                    # Gnome Software
                    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                        pkgs+=(gnome-tweaks dconf-editor)
                    fi

                    # Plasma Software
                    if [[ "$WINDOW_MANAGER" == "$WM_KDE" ]]; then
                        pkgs+=(kdeconnect-kde kdotool)
                        # yakuake latte-dock
                    fi

                    # Wayland stuff
                    if [[ "$WINDOW_SYSTEM" == "$WS_WAYLAND" ]]; then
                        pkgs+=(egl-wayland egl-wayland-devel wl-clipboard)
                        pkgs+=(wayland-utils)
                        pkgs+=(ydotool wtype)

                        # foot - the fast, lightweight and minimalistic Wayland terminal emulator.
                        pkgs+=(foot)

                        # dependencies needed for https://sr.ht/~geb/dotool/
                        # pkgs+=(libxkbcommon-devel scdoc)
                    fi

                    # X11 stuff
                    if [[ "$WINDOW_SYSTEM" == "$WS_X11" ]]; then
                        pkgs+=(xclip xinput xdotool x11-utils)
                    fi

                    sudo dnf install -y --skip-unavailable "${pkgs[@]}"

                    # gpg smartcard daemon
                    sudo systemctl enable --now pcscd

                    # Starship Prompt - Using PS1 instead
                    # curl -sS https://starship.rs/install.sh | sh

                    # Mullvad VPN (Using Network Manager)
                    # sudo dnf config-manager addrepo --from-repofile=https://repository.mullvad.net/rpm/stable/mullvad.repo
                    # sudo dnf install -y mullvad-vpn

                    # Rust
                    # curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --no-modify-path

                    # terminal git via copr
                    # sudo dnf copr enable atim/lazygit -y
                    # sudo dnf install -y lazygit

                    # non dnf software
                    pipx install cmake-language-server
                    pipx install pyright
                    sudo npm install --location=global npm@latest
                    sudo npm install --location=global \
                         prettier \
                         js-beautify \
                         typescript-language-server typescript \
                         dockerfile-language-server-nodejs \
                         bash-language-server

                    # Claude
                    curl -fsSL https://claude.ai/install.sh | bash
                    sudo npm install --location=global @agentclientprotocol/claude-agent-acp

                    # vscode devcontainers
                    sudo npm install --location=global @devcontainers/cli

                    # Needed for `lsp-bridge`
                    # pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            5)
                echo "$CHOICE) Installing Extras"
                (
                    #
                    # Multimedia
                    #

                    # Install additional codecs
                    sudo dnf -y group install multimedia
                    sudo dnf -y group install sound-and-video

                    # Switch to full FFMPEG.
                    sudo dnf swap 'ffmpeg-free' 'ffmpeg' --allowerasing

                    # Installs gstreamer components. Required if you use Gnome Videos and other dependent applications.
                    sudo dnf -y upgrade @multimedia --setopt="install_weak_deps=False" \
                         --exclude=PackageKit-gstreamer-plugin

                    sudo dnf install -y --skip-unavailable \
                         mesa-dri-drivers mesa-vulkan-drivers vulkan-loader mesa-libGLU \
                         ffmpeg-libs libva libva-utils

                    # AMD / Intel
                    if grep -qi 'vendor.*intel' /proc/cpuinfo; then
                        echo "$CHOICE) Installing Hardware Accelerated Codecs for Intel Architecture"
                        sudo dnf swap libva-intel-media-driver intel-media-driver --allowerasing
                    elif grep -qi 'vendor.*amd' /proc/cpuinfo; then
                        echo "$CHOICE) Installing Hardware Accelerated Codecs for AMD Architecture"
                        sudo dnf swap mesa-va-drivers mesa-va-drivers-freeworld
                        sudo dnf swap mesa-vdpau-drivers mesa-vdpau-drivers-freeworld
                        sudo dnf swap mesa-va-drivers.i686 mesa-va-drivers-freeworld.i686
                        sudo dnf swap mesa-vdpau-drivers.i686 mesa-vdpau-drivers-freeworld.i686
                    else
                        echo "$CHOICE) Unknown Architecture. Not Installing Hardware Accelerated Codecs"
                    fi

                    # OpenH264 for Firefox (Needs to be enabled on Firefox)
                    sudo dnf install -y openh264 gstreamer1-plugin-openh264 mozilla-openh264
                    sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1

                    # Play a DVD
                    sudo dnf install -y rpmfusion-free-release-tainted
                    sudo dnf install -y libdvdcss

                    #
                    # Chromium Browser (For use with eww)
                    #

                    # sudo dnf install -y chromium

                    #
                    # Brave Browser
                    #

                    sudo dnf config-manager addrepo --from-repofile=https://brave-browser-rpm-release.s3.brave.com/brave-browser.repo
                    sudo rpm --import https://brave-browser-rpm-release.s3.brave.com/brave-core.asc
                    sudo dnf install -y brave-browser

                    #
                    # Firefox
                    #

                    # make the start page the default firefox start page
                    [ -f /usr/lib64/firefox/browser/defaults/preferences/firefox-redhat-default-prefs.js ] && \
                        sudo rm -f /usr/lib64/firefox/browser/defaults/preferences/firefox-redhat-default-prefs.js

                    #
                    # Docker (using podman)
                    #
                    #
                    # Install and Enable Docker (not working on fedora-38)
                    # sudo dnf install -y dnf-plugins-core
                    # sudo dnf config-manager addrepo --from-repofile=https://download.docker.com/linux/fedora/docker-ce.repo
                    # sudo dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
                    # sudo systemctl enable --now docker
                    #
                    # add user
                    # sudo usermod -G docker -a "$USER"
                    # sudo systemctl restart docker

                    # Note: texlive-scheme-full is ~5GB. Swap for
                    # texlive-scheme-medium / -basic if disk space matters.
                    sudo dnf install -y --skip-unavailable \
                         wireguard-tools \
                         openvpn \
                         podman podman-compose podman-docker \
                         texlive-scheme-full \
                         steam-devices \
                         pipewire-codec-aptx \
                         libreoffice-opensymbol-fonts \
                         adobe-source-code-pro-fonts \
                         jetbrains-mono-fonts-all \
                         ibm-plex-mono-fonts ibm-plex-sans-fonts ibm-plex-serif-fonts \
                         rsms-inter-fonts rsms-inter-vf-fonts

                    read -rp "Install Berkeley Mono Fonts (gpg encrypted downloaded fonts)? (Y/N): " confirm
                    if [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]]; then
                        install_berkeley_mono_font
                    fi

                    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                        gsettings set org.gnome.desktop.interface font-hinting 'full'
                        gsettings set org.gnome.desktop.interface font-antialiasing 'rgba'
                        gsettings set org.gnome.desktop.interface font-name 'Inter Variable 11'
                        gsettings set org.gnome.desktop.interface document-font-name 'Inter 11'
                        gsettings set org.gnome.desktop.interface monospace-font-name 'Berkeley Mono 11'
                        # gsettings set org.gnome.desktop.interface text-scaling-factor 1.25 # using fractional scaling instead!!
                    fi
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            6)
                echo "$CHOICE) Enabling Flatpak"
                (
                    # Just in case it is not installed
                    sudo dnf install -y flatpak

                    # Remove the limited Fedora repo
                    flatpak remote-delete fedora

                    # Add the real Flathub and update everything
                    flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
                    flatpak update --appstream

                    # Install flatpaks
                    if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
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
                    flatpak install --user --noninteractive flathub io.podman_desktop.PodmanDesktop
                    flatpak install --user --noninteractive flathub com.valvesoftware.Steam
                    flatpak install --user --noninteractive flathub org.freedesktop.Platform.VulkanLayer.MangoHud # this is needed for steam!
                    flatpak install --user --noninteractive flathub org.raspberrypi.rpi-imager # Raspberri PI imager
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
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            7)
                echo "$CHOICE) Setting up secrets and repos"
                ( setup_secrets_and_repos ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            8)
                echo "$CHOICE) Installing Emacs"
                if hash emacs 2> /dev/null; then
                    echo "Emacs is already installed"
                else
                    (
                        # Install dependencies
                        sudo dnf group install "development-tools"
                        sudo dnf -y builddep emacs

                        sudo dnf install -y --skip-unavailable \
                             Xaw3d Xaw3d-devel libpng libpng-devel zlib zlib-devel libjpeg-turbo libjpeg-devel \
                             libtiff libtiff-devel giflib giflib-devel librsvg2 librsvg2-devel libwebp libwebp-devel \
                             ImageMagick ImageMagick-devel autoconf gnutls gnutls-devel gmp-devel gtk3-devel \
                             gtk4-devel jansson-devel libgccjit libgccjit-devel libmpc-devel libvterm libvterm-devel \
                             mpfr-devel ncurses ncurses-devel texinfo systemd-devel \
                             libtree-sitter libtree-sitter-devel tree-sitter-cli gcc-c++ \
                             enchant2-devel pkgconf glib2 glib2-devel cairo cairo-devel \
                             harfbuzz harfbuzz-devel pango pango-devel dbus dbus-devel lcms2 lcms2-devel \
                             fontconfig fontconfig-devel freetype freetype-devel libotf libotf-devel \
                             m17n-lib m17n-lib-devel libxml2 libxml2-devel

                        # do install emacs from source
                        install_emacs
                    ) || echo "Step $CHOICE had errors (continuing)..."
                fi
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            9)
                echo "$CHOICE) Installing StyLua"
                if hash stylua 2> /dev/null; then
                    echo "StyLua is already installed"
                else
                    ( install_stylua ) || echo "Step $CHOICE had errors (continuing)..."
                fi
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            10)
                echo "$CHOICE) Installing App Launcher"

                #
                # Vicinae
                #
                if hash vicinae 2> /dev/null; then
                    echo "Vicinae is already installed"
                else
                    (
                        curl -fsSL https://vicinae.com/install.sh | bash
                        systemctl --user enable vicinae --now
                    ) || echo "Step $CHOICE had errors (continuing)..."
                fi
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            11)
                echo "$CHOICE) Installing PSD (profile-sync-daemon)"
                (
                    # Dependencies
                    sudo dnf install -y coreutils findutils glib2 kmod rsync systemd

                    # Install
                    install_psd

                    # Copy brave profile
                    if [ -f /usr/share/psd/contrib/brave ]; then
                        sudo cp /usr/share/psd/contrib/brave /usr/share/psd/browsers
                    elif [ -f "$HOME"/Workspace/Public/dotfiles/Common/psd/brave ]; then
                        sudo cp "$HOME"/Workspace/Public/dotfiles/Common/psd/brave /usr/share/psd/browsers
                    fi

                    # PSD: Needs sudo permissions for overlay-fs - needs a logout :(
                    echo "$USER ALL=(ALL) NOPASSWD: /usr/bin/psd-overlay-helper" | sudo EDITOR='tee -a' visudo

                    # PSD: Enable and Start the Daemon
                    systemctl --user enable --now psd.service
                ) || echo "Step $CHOICE had errors (continuing)..."
                pause "$CHOICE) Done. Press enter to continue..."
                ;;
            12)
                echo "$CHOICE) Installing Xremap"
                if hash xremap 2> /dev/null; then
                    echo "Xremap is already installed"
                else
                    (
                        [[ "$WINDOW_SYSTEM" == "$WS_X11" ]] && sudo dnf install -y libx11-devel

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
            13)
                echo "$CHOICE) Installing Harper"
                if hash harper-cli 2> /dev/null || hash harper-ls 2> /dev/null; then
                    echo "Harper is already installed"
                else
                    ( install_harper ) || echo "Step $CHOICE had errors (continuing)..."
                fi
                pause "$CHOICE) Harper installed. Press enter to continue..."
                ;;
            14)
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
            fedora_install
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
