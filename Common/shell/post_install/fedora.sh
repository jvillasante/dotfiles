#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
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
cd "$(dirname "$0")" || exit 1

# source everything in lib
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
    echo "Usage:"
    echo "    $SCRIPT_NAME help:"
    echo "        Show this help message"
    echo "    $SCRIPT_NAME install:"
    echo "        Install Fedora System"
    echo
    echo " e.g: $SCRIPT_NAME install"
    exit "$1"
}

fedora_install() {
    HEIGHT=25
    WIDTH=100
    CHOICE_HEIGHT=4
    BACKTITLE="Fedora Setup Util"
    TITLE="Please Make a selection"
    MENU="Please Choose one of the following options:"

    # Accept WS and WM for the installer to kick in
    read -rp "$BACKTITLE (WS=$WINDOW_SYSTEM and WM=$WINDOW_MANAGER)? (Y/N): " confirm
    [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]] || usage 1
    case $WINDOW_SYSTEM in
        "$WS_WAYLAND" | "$WS_X11") ;;
        *)
            echo "Error. Please select one of the supported Window Systems"
            usage 1
            ;;
    esac
    case $WINDOW_MANAGER in
        "$WM_KDE" | "$WM_GNOME") ;;
        *)
            echo "Error. Please select one of the supported Window Managers"
            usage 1
            ;;
    esac

    # Check to see if Dialog is installed, if not install it
    if [ "$(rpm -q dialog 2> /dev/null | grep -c "is not installed")" -eq 1 ]; then
        sudo dnf install -y dialog
    fi

    OPTIONS=(
        1 "Setup Defaults - Set some defaults (hostname, folders structure, global settings, etc)"
        2 "Setup RPM Fussion - Update Kernel, RPM Fussion & Firmware (reboot needed)"
        3 "Install NVIDIA - Install NVIDIA Drivers (reboot needed)"
        4 "Install Software - Installs a bunch of my most used software"
        5 "Install Extras - Themes Fonts and Codecs"
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

                # hostname
                read -r -p "Enter pretty hostname (defaults to 'Julio's Personal Laptop'): " HOSTNAME_PRETTY
                [ -z "$HOSTNAME_PRETTY" ] && HOSTNAME_PRETTY="Julio's Personal Laptop"
                hostnamectl set-hostname --pretty "$HOSTNAME_PRETTY"

                read -r -p "Enter static hostname (defaults to 'fedora-xps-9710'): " HOSTNAME_STATIC
                [ -z "$HOSTNAME_STATIC" ] && HOSTNAME_STATIC="fedora-xps-9710"
                hostnamectl set-hostname --static "$HOSTNAME_STATIC"

                # xdg
                export XDG_CACHE_HOME="${XDG_CACHE_HOME:-$HOME/.cache}"
                export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-$HOME/.config}"
                export XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
                export XDG_LIB_HOME="${XDG_DATA_HOME:-$HOME/.local/lib}"
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

                # Setup dnf configs and update
                echo 'max_parallel_downloads=10' | sudo tee -a /etc/dnf/dnf.conf
                sudo dnf upgrade -y --refresh

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            2)
                echo "$CHOICE) Setting up RPM Fusion and Updating Firmware"

                # update kernel
                sudo dnf install -y gcc kernel-headers kernel-devel

                # Enable RPM Fusion
                sudo dnf install -y \
                     https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-"$(rpm -E %fedora)".noarch.rpm \
                     https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-"$(rpm -E %fedora)".noarch.rpm

                # Upgrade everything
                sudo dnf upgrade -y --refresh
                sudo dnf group upgrade -y core
                sudo dnf -y update

                # Terra (https://terra.fyralabs.com/)
                # sudo dnf install -y --nogpgcheck --repofrompath 'terra,https://repos.fyralabs.com/terra$releasever' terra-release

                # Install some packages and firmware
                sudo dnf install -y rpmfusion-free-release-tainted
                sudo dnf install -y rpmfusion-nonfree-release-tainted
                sudo dnf install -y dnf-plugins-core
                sudo dnf install -y \*-firmware

                # Update system firmware
                # sudo fwupdmgr refresh --force
                # sudo fwupdmgr get-devices # Lists devices with available updates.
                # sudo fwupdmgr get-updates # Fetches list of available updates.
                # sudo fwupdmgr update      # Apply updates

                read -rp "$CHOICE) Done. fwupdmgr should be run manually. Press enter to continue..."
                ;;
            3)
                echo "$CHOICE) Installing NVIDIA Drivers. Disable secure boot in the bios."

                # htop to watch kernel module build after installing nvidia
                sudo dnf install -y htop

                # Check if you have Secure Boot enabled with - easier if yes!
                #     mokutil --sb-state

                # Install kernel headers and dev tools
                sudo dnf install -y kernel-devel kernel-headers gcc make dkms acpid \
                     libglvnd-glx libglvnd-opengl libglvnd-devel pkgconfig

                # NVIDIA - Install Proprietary Drivers
                # sudo dnf install -y akmod-nvidia xorg-x11-drv-nvidia-cuda

                # NVIDIA - Install Open Drivers
                sudo dnf install akmod-nvidia-open xorg-x11-drv-nvidia-cuda

                # Monitor progress
                #  sudo journalctl -f -u akmods

                # Check if the kernel module is built.
                #     modinfo -F version nvidia

                read -rp "$CHOICE) Done. Reboot after akmod build!!!. Press enter to continue..."
                ;;
            4)
                echo "$CHOICE) Installing Software"

                # Nvidia - Needed for Encrypted Drives
                # echo 'add_drivers+=" nvidia nvidia_modeset nvidia_uvm nvidia_drm "' |\
                #     sudo tee -a /etc/dracut.conf.d/nvidia.conf
                # sudo dracut --force

                # Nvidia - Enable nvidia-modeset
                # sudo grubby --update-kernel=ALL --args="nvidia-drm.modeset=1"

                # general
                sudo dnf install -y dnf-plugins-core copr-cli
                sudo dnf group install -y "development-tools" "c-development"
                sudo dnf install -y ripgrep fd-find util-linux-user xprop xwininfo
                sudo dnf install -y aspell aspell-en aspell-es autojump atool autoconf automake bat cmake vim
                sudo dnf install -y freetype-devel fontconfig-devel libxcb-devel libxkbcommon-devel
                sudo dnf install -y dnsutils dos2unix doxygen msmtp fastfetch
                sudo dnf install -y graphviz mercurial ninja-build
                sudo dnf install -y nodejs npm python3 python3-pip
                sudo dnf install -y multimarkdown pandoc poppler poppler-utils poppler-data
                sudo dnf install -y subversion tldr tree w3m lynx wget libtool texinfo
                sudo dnf install -y wordnet shfmt editorconfig glslang ShellCheck parallel
                sudo dnf install -y llvm clang clang-tools-extra libpcap libpcap-devel
                sudo dnf install -y pkg-config flex bison
                sudo dnf install -y tar unar unrar unzip p7zip p7zip-plugins
                sudo dnf install -y ImageMagick ImageMagick-devel ffmpegthumbnailer mediainfo
                sudo dnf install -y sqlite sqlite-devel
                sudo dnf install -y curl libcurl libcurl-devel
                sudo dnf install -y valgrind minicom mc strace tidy
                sudo dnf install -y libatomic libatomic-static
                sudo dnf install -y libunwind libunwind-devel
                sudo dnf install -y gperftools gperftools-libs gperftools-devel
                sudo dnf install -y ffmpegthumbnailer mediainfo
                sudo dnf install -y feh mpv
                sudo dnf install -y ncurses-term
                sudo dnf install -y ccache meson
                sudo dnf install -y zlib-devel bzip2 bzip2-devel readline-devel xz xz-devel \
                     libffi-devel findutils tk-devel libyaml-devel
                sudo dnf install -y cowsay fortune-mod gnuplot telnet rlwrap
                sudo dnf install -y libgcrypt libgcrypt-devel

                # System Monitor
                sudo dnf install -y htop
                sudo dnf install -y smem
                # sudo dnf install -y atop below btop nvtop

                # IO Stuff
                sudo dnf install -y libaio libaio-devel
                sudo dnf install -y liburing liburing-devel

                # GTK Stuff
                sudo dnf install -y gtk3 gtk3-devel \
                                    gtk4 gtk4-devel \
                                    gtk-layer-shell gtk-layer-shell-devel \
                                    gtk4-layer-shell gtk4-layer-shell-devel

                # Gnome Software
                if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                    sudo dnf -y install -y gnome-tweaks dconf-editor
                fi

                # Plasma Software
                if [[ "$WINDOW_MANAGER" == "$WM_KDE" ]]; then
                    sudo dnf install -y kdeconnect-kde
                    sudo dnf install -y kdotool
                    # sudo dnf install -y yakuake
                    # sudo dnf install -y latte-dock
                    # sudo zypper install -y kazam
                fi

                # Wayland stuff
                if [[ "$WINDOW_SYSTEM" == "$WS_WAYLAND" ]]; then
                    sudo dnf install -y egl-wayland egl-wayland-devel wl-clipboard
                    sudo dnf install -y ydotool wtype

                    # dependencies needed for https://sr.ht/~geb/dotool/
                    # sudo dnf install -y libxkbcommon-devel scdoc
                fi

                # X11 stuff
                if [[ "$WINDOW_SYSTEM" == "$WS_X11" ]]; then
                    sudo dnf install -y xclip xinput xdotool x11-utils # some X11 tools
                fi

                # Pass (password-store)
                sudo dnf install -y pass
                # sudo dnf install -y gopass

                # Git stuff
                sudo dnf install -y git           # git itself
                # sudo dnf install -y git-email   # support for email based workflow
                # sudo dnf install -y difftastic  # better diffs
                # sudo dnf install -y git-delta   # better diffs
                # terminal git
                # sudo dnf copr enable atim/lazygit -y
                # sudo dnf install -y lazygit

                # tmux: A terminal multiplexer
                sudo dnf install -y tmux

                # alacritty - A fast, cros-platform, OpenGL terminal emulator
                # sudo dnf install -y alacritty

                # foot - the fast, lightweight and minimalistic Wayland terminal emulator.
                [[ "$WINDOW_SYSTEM" == "$WS_WAYLAND" ]] && sudo dnf install -y foot

                # Starship Prompt
                curl -sS https://starship.rs/install.sh | sh

                # gpg
                sudo dnf install -y gpg gnupg2 gnupg2-scdaemon gnupg-pkcs11-scd pcsc-tools opensc pcsc-lite-ccid
                sudo dnf install -y pinentry-emacs pinentry-tty
                sudo systemctl enable --now pcscd && sudo systemctl start pcscd

                # yubikey
                sudo dnf install -y yubikey-manager

                # open{ssl,ssh}
                sudo dnf install -y openssh openssl openssl-devel

                # libssh
                sudo dnf install -y libssh libssh-devel

                # mail
                sudo dnf install -y isync mu maildir-utils gnutls gnutls-devel

                # ledger (https://ledger-cli.org/)
                sudo dnf install -y ledger

                # ncdu (text-based disk usage viewer)
                sudo dnf install -y ncdu

                # Borg Backup
                sudo dnf install -y borgbackup

                # Command Line Fuzzy Finder
                sudo dnf install -y fzf

                # Emacs (on fedora is up to date but I prefer to compile it myself)

                # sudo dnf install -y emacs
                # sudo dnf install -y libvterm libvterm-devel # necessary for vterm
                # sudo dnf install -y enchant2-devel pkgconf  # necessary for jinx

                # Neovim
                sudo dnf install -y neovim

                # Firewall GUI to manage firewalld
                sudo dnf install -y firewall-config

                # Bleachbit: Clean Your System and Free Disk Space
                # sudo dnf install -y bleachbit

                # Go
                # sudo dnf install -y go

                # Rust
                # curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --no-modify-path

                # Torrents
                # sudo dnf install -y qbittorrent

                # Mullvad VPN (Using Network Manager)
                # sudo dnf config-manager addrepo --from-repofile=https://repository.mullvad.net/rpm/stable/mullvad.repo
                # sudo dnf install -y mullvad-vpn

                # grammar linter (python)
                # sudo dnf install -y proselint

                # ebook reader/converver
                # sudo dnf install -y calibre

                # Needed for keychrome firmware flash
                sudo dnf install -y dfu-util

                # non dnf software
                pip3 install cmake-language-server
                pip3 install pyright
                sudo npm install --location=global npm@latest
                sudo npm install --location=global prettier
                sudo npm install --location=global js-beautify
                sudo npm install --location=global typescript-language-server typescript
                sudo npm install --location=global dockerfile-language-server-nodejs
                sudo npm install --location=global bash-language-server
                sudo npm install --location=global @devcontainers/cli
                # sudo npm install --location=global @google/gemini-cli

                # Needed for `lsp-bridge`
                # pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            5)
                echo "$CHOICE) Installing Extras"

                #
                # Multimedia
                #

                # Install additional codecs
                sudo dnf -y group install multimedia && \
                    sudo dnf -y group upgrade multimedia
                sudo dnf -y group install sound-and-video && \
                    sudo dnf -y group upgrade sound-and-video

                # Switch to full FFMPEG.
                sudo dnf swap 'ffmpeg-free' 'ffmpeg' --allowerasing

                # Installs gstreamer components. Required if you use Gnome Videos and other dependent applications.
                sudo dnf -y upgrade @multimedia --setopt="install_weak_deps=False" \
                     --exclude=PackageKit-gstreamer-plugin

                # Basic drivers and Vulkan support
                sudo dnf install -y mesa-dri-drivers mesa-vulkan-drivers vulkan-loader mesa-libGLU

                # Install Hardware Accelerated Codecs
                sudo dnf install -y ffmpeg-libs libva libva-utils

                # AMD / Intel
                if test "$(cat /proc/cpuinfo | grep vendor | uniq | grep -iFc "intel")"; then
                    echo "$CHOICE) Installing Hardware Accelerated Codecs for Intel Architecture"
                    sudo dnf swap libva-intel-media-driver intel-media-driver --allowerasing
                    sudo dnf install -y libva-intel-driver
                elif test "$(cat /proc/cpuinfo | grep vendor | uniq | grep -iFc "amd")"; then
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

                # Hardware codecs with NVIDIA
                sudo dnf install -y nvidia-vaapi-driver --allowerasing --skip-unavailable

                # Play a DVD
                sudo dnf install -y rpmfusion-free-release-tainted && sudo dnf install -y libdvdcss

                # Various firmware
                sudo dnf install -y rpmfusion-nonfree-release-tainted
                sudo dnf --repo=rpmfusion-nonfree-tainted install -y "*-firmware"

                #
                # Chromium Browser (For use with eww)
                #

                # sudo dnf install -y chromium

                #
                # Brave Browser
                #

                sudo dnf install -y dnf-plugins-core
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
                # Wireguard
                #

                sudo dnf install -y wireguard-tools

                #
                # OpenVPN
                #

                sudo dnf install -y openvpn

                #
                # Docker (using podman)
                #
                #
                # Install and Enable Docker (not working on fedora-38)
                # sudo dnf install -y dnf-plugins-core
                # sudo dnf config-manager addrepo --from-repofile=https://download.docker.com/linux/fedora/docker-ce.repo
                # sudo dnf install -y docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin
                # sudo systemctl enable --now docker && sudo systemctl start docker
                #
                # add user
                # sudo usermod -G docker -a "$USER"
                # sudo systemctl restart docker

                #
                # Podman
                #

                sudo dnf install -y podman podman-compose podman-docker

                #
                # Latex
                #

                sudo dnf install -y texlive-scheme-full

                #
                # Steam extras
                #

                sudo dnf install -y steam-devices

                #
                # Fonts
                #

                sudo dnf install -y libreoffice-opensymbol-fonts
                sudo dnf install -y adobe-source-code-pro-fonts
                sudo dnf install -y jetbrains-mono-fonts-all
                sudo dnf install -y ibm-plex-mono-fonts ibm-plex-sans-fonts ibm-plex-serif-fonts
                sudo dnf install -y rsms-inter-fonts rsms-inter-vf-fonts
                install_berkeley_mono_font

                if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                    gsettings set org.gnome.desktop.interface font-hinting 'full'
                    gsettings set org.gnome.desktop.interface font-antialiasing 'rgba'
                    gsettings set org.gnome.desktop.interface font-name 'Inter Variable 11'
                    gsettings set org.gnome.desktop.interface document-font-name 'Inter 11'
                    gsettings set org.gnome.desktop.interface monospace-font-name 'Berkeley Mono 11'
                    # gsettings set org.gnome.desktop.interface text-scaling-factor 1.25 # using fractional scaling instead!!
                fi

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            6)
                echo "$CHOICE) Enabling Flatpak"

                # Just in case it is not installed
                sudo dnf install -y flatpak

                # Remove the limited Fedora repo
                flatpak remote-delete fedora

                # Add the real Flathub and update everything
                flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
                flatpak update --appstream

                # Install flatpaks
                if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                    flatpak install --user -y flathub org.gnome.TextEditor
                    flatpak install --user -y flathub org.gnome.Firmware
                    flatpak install --user -y flathub com.mattjakeman.ExtensionManager
                fi
                flatpak install --user -y flathub com.discordapp.Discord
                flatpak install --user -y flathub com.dropbox.Client
                flatpak install --user -y flathub com.slack.Slack
                flatpak install --user -y flathub org.videolan.VLC
                flatpak install --user -y flathub org.wireshark.Wireshark
                flatpak install --user -y flathub com.github.tchx84.Flatseal
                flatpak install --user -y flathub com.github.johnfactotum.Foliate
                flatpak install --user -y flathub org.gimp.GIMP
                flatpak install --user -y flathub com.transmissionbt.Transmission
                flatpak install --user -y flathub org.telegram.desktop
                flatpak install --user -y flathub io.podman_desktop.PodmanDesktop
                flatpak install --user -y flathub com.valvesoftware.Steam
                flatpak install --user -y org.freedesktop.Platform.VulkanLayer.MangoHud # this is needed for steam!
                # flatpak install --user -y flathub org.keepassxc.KeePassXC
                # flatpak install --user -y flathub com.borgbase.Vorta
                # flatpak install --user -y flathub io.github.Hexchat
                # flatpak install --user -y flathub engineer.atlas.Nyxt
                # flatpak install --user -y flathub org.mozilla.firefox
                # flatpak install --user -y flathub org.gnucash.GnuCash
                # flatpak install --user -y flathub im.riot.Riot
                # flatpak install --user -y flathub com.obsproject.Studio
                # flatpak install --user -y flathub com.brave.Browser
                # flatpak install --user -y flathub org.shotcut.Shotcut
                # flatpak install --user -y flathub dev.geopjr.Tuba
                # flatpak install --user -y flathub io.github.mimbrero.WhatsAppDesktop
                # flatpak install --user -y flathub org.signal.Signal
                # flatpak install --user -y flathub com.microsoft.Edge
                # flatpak install --user -y flathub com.visualstudio.code
                # flatpak install --user -y flathub com.github.GradienceTeam.Gradience

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            7)
                echo "$CHOICE) Setting up secrets and repos"
                setup-secrets-and-repos

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            8)
                echo "$CHOICE) Installing Emacs"

                # Is it already installed?
                hash emacs 2> /dev/null && echo "Emacs is already installed" && exit 1

                # Install dependencies
                sudo dnf group install "development-tools"
                sudo dnf -y builddep emacs

                # images packages install
                sudo dnf install -y Xaw3d Xaw3d-devel libpng libpng-devel zlib libjpeg-turbo libjpeg-devel \
                     libtiff libtiff-devel giflib giflib-devel librsvg2 librsvg2-devel libwebp libwebp-devel \
                     ImageMagick ImageMagick-devel

                # emacs packages install
                sudo dnf install -y autoconf gnutls-devel gmp-devel gtk3-devel gtk4-devel jansson-devel \
                     libgccjit libgccjit-devel libmpc-devel libvterm libvterm-devel mpfr-devel ncurses-devel texinfo

                # tree-sitter packages
                sudo dnf install -y libtree-sitter libtree-sitter-devel tree-sitter-cli gcc-c++

                # enchant package (jinx)
                sudo dnf install -y enchant2-devel pkgconf # necessary for jinx

                # do install emacs from source
                install-emacs

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            9)
                echo "$CHOICE Installing StyLua"

                 # Is it already installed?
                hash stylua 2> /dev/null && echo "StyLua is already installed" && exit 1

                # do install
                install_stylua

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            10)
                echo "$CHOICE) Installing App Launcher"

                # KDE has it's own launcher
                if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                    #
                    # Ulauncher
                    #

                    # sudo dnf install -y ulauncher wmctrl

                    #
                    # Vicinae
                    #

                    # Is it already installed?
                    hash vicinae 2> /dev/null && echo "Vicinae is already installed" && exit 1

                    # Install
                    curl -fsSL https://vicinae.com/install.sh | bash

                    # Enable and start the service
                    systemctl --user enable vicinae --now
                fi

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            11)
                echo "$CHOICE) Installing PSD (profile-sync-daemon)"

                # Dependencies
                sudo dnf install -y coreutils findutils glib2 kmod rsync systemd

                # Install
                install_psd

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            12)
                echo "$CHOICE) Installing Xremap"

                [[ "$WINDOW_SYSTEM" == "$WS_X11" ]] && sudo dnf install -y libx11-devel

                # Is it already installed?
                hash xremap 2> /dev/null && echo "Xremap is already installed" && exit 1

                # do install
                install_xremap

                read -rp "$CHOICE) Xremap installed. Reboot for udev rules to take effect.
                              On Gnome, install the extension at https://extensions.gnome.org/extension/5060/xremap/.
                              Press enter to continue..."
                ;;
            13)
                echo "$CHOICE) Installing Harper"

                # Is it already installed?
                hash harper-cli 2> /dev/null && echo "Harper is already installed" && exit 1
                hash harper-ls 2> /dev/null && echo "Harper is already installed" && exit 1

                # do install
                install_harper

                read -rp "$CHOICE) Harper installed. Press enter to continue..."
                ;;
            14)
                exit 0
                ;;
        esac
    done
}

main() {
    nargs=$#
    cmd=${1-}
    rc=0
    if [ "$#" -gt 0 ]; then shift; fi
    case $cmd in
        install)
            [ "$nargs" -eq 1 ] || usage 1
            fedora_install "$@"
            ;;
        help | --help | -h)
            usage 0
            ;;
        *)
            usage 1
            ;;
    esac
    return $rc
}

main "$@"
