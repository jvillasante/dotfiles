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

if [ ! -f /etc/debian_version ]; then
    echo "Error: Debian is not running on this system, exiting..."
    exit 1
fi

if ! hash apt 2> /dev/null; then
    echo "Error: apt not installed on this system, exiting..."
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
    echo "        Install Debian System"
    echo
    echo " e.g: $SCRIPT_NAME install"
    exit "$1"
}

debian_install() {
    HEIGHT=25
    WIDTH=100
    CHOICE_HEIGHT=4
    BACKTITLE="Debian Setup Util"
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

    # Check to see if dialog is installed, if not install it
    type dialog 2> /dev/null || sudo apt install -y dialog

    # kick off the installer
    OPTIONS=(
        1 "Set Defaults - Set some defaults & update"
        2 "Install Firmware - Install Firmware Updates"
        3 "Install Nvidia Drivers - Install Nvidia Drivers"
        4 "Install Software - Installs a bunch of my most used software"
        5 "Install Extras - TLP, Fonts, etc"
        6 "Enable Flatpak - Enables the Flatpak repo and installs packages"
        7 "Setup Secrets and Repos - Setup ssh and gpg from backups and get git repos"
        8 "Install Emacs - Install Emacs29 from source"
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

                # hostname
                read -rp "Enter pretty hostname (defaults to 'Julio's Personal Laptop'): " HOSTNAME_PRETTY
                [ -z "$HOSTNAME_PRETTY" ] && HOSTNAME_PRETTY="Julio's Personal Laptop"
                hostnamectl set-hostname --pretty "$HOSTNAME_PRETTY"

                read -rp "Enter static hostname (defaults to 'debian-xps-9710'): " HOSTNAME_STATIC
                [ -z "$HOSTNAME_STATIC" ] && HOSTNAME_STATIC="debian-xps-9710"
                hostnamectl set-hostname --static "$HOSTNAME_STATIC"

                # Sources
                [ -f /etc/apt/sources.list ] && \
                    sudo cp /etc/apt/sources.list /etc/apt/sources.list.bak
                [ ! -f /etc/apt/sources.list ] && \
                    sudo touch /etc/apt/sources.list
                cat << EOF | sudo tee /etc/apt/sources.list > /dev/null
# Comment old /etc/apt/sources.list
# deb http://deb.debian.org/debian trixie main contrib non-free
# deb http://deb.debian.org/debian-security trixie-security main contrib non-free
EOF
                [ -f /etc/apt/sources.list.d/debian.sources ] && \
                    sudo cp /etc/apt/sources.list.d/debian.sources /etc/apt/sources.list.d/debian.sources.bak
                [ ! -f /etc/apt/sources.list.d/debian.sources ] && \
                    sudo touch /etc/apt/sources.list.d/debian.sources
                cat << EOF | sudo tee /etc/apt/sources.list.d/debian.sources > /dev/null
# Debian 13 Trixie
Types: deb deb-src
URIs: https://deb.debian.org/debian
Suites: trixie trixie-updates
Components: main non-free-firmware contrib non-free
Enabled: yes
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg

# Security Updates
Types: deb deb-src
URIs: https://security.debian.org/debian-security
Suites: trixie-security
Components: main non-free-firmware contrib non-free
Enabled: yes
Signed-By: /usr/share/keyrings/debian-archive-keyring.gpg
EOF

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

                # Install 3rd party apps using extrepo
                #   https://salsa.debian.org/extrepo-team/extrepo-data/-/tree/master/repos/debian
                # After install uncomment '- contrib' and '- non-free' lines in /etc/extrepo/config.yaml
                # List available apps: extrepo search | grep Found | sed 's/Found//g' | sed 's/://g' | sort
                sudo apt install -y extrepo
                echo "Extrepo: Uncomment '- contrib' and '- non-free' lines in /etc/extrepo/config.yaml"
                read -rp "Is /etc/extrepo/config.yaml good to go? (Y/N): " confirm &&
                    [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]] || exit 1
                sudo extrepo update -y

                # Update system with the new sources
                sudo apt update -y
                sudo apt upgrade -y
                sudo apt full-upgrade
                sudo apt autoremove

                read -rp "$CHOICE) Defaults has been set. Reboot may be required. Press enter to continue..."
                ;;
            2)
                echo "$CHOICE) Installing Firmware Updates"

                # Install Software
                sudo apt install -y fwupd
                sudo apt install -y linux-headers-"$(dpkg --print-architecture)"
                sudo apt install -y firmware-linux
                sudo apt install -y firmware-misc-nonfree

                # Install Firmware
                sudo fwupdmgr refresh --force
                sudo fwupdmgr get-devices
                sudo fwupdmgr get-updates
                sudo fwupdmgr update

                read -rp "$CHOICE) Firmware updates has been installed. Reboot may be required. Press enter to continue..."
                ;;
            3)
                echo "$CHOICE) Installing Nvidia Drivers (https://wiki.debian.org/NvidiaGraphicsDrivers)"

                # Prerequisites
                sudo apt install -y linux-headers-"$(dpkg --print-architecture)"

                # Install
                sudo apt install -y nvidia-kernel-dkms nvidia-driver firmware-misc-nonfree

                # before rebooting, make sure the driver installed successfully:
                # dkms status

                read -rp "$CHOICE) Nividia Drivers installed. Reboot after checking 'dkms status'. Press enter to continue..."
                ;;
            4)
                echo "$CHOICE) Installing Software"

                # Remove Firefox ESR
                sudo apt remove --purge --yes firefox-esr*
                sudo apt autoremove

                # General
                sudo apt install -y apt-listbugs apt-listchanges software-properties-common
                sudo apt install -y build-essential binutils coreutils openssl libssl-dev
                sudo apt install -y tar p7zip-full zip unzip rsync rar unrar atool
                sudo apt install -y silversearcher-ag aspell aspell-en aspell-es autojump autoconf automake
                sudo apt install -y bat curl dnsutils dos2unix doxygen fd-find font-manager
                sudo apt install -y btop htop nvtop isync libpcap-dev make mercurial msmtp fastfetch
                sudo apt install -y vim neovim pandoc pass poppler-utils poppler-data ripgrep
                sudo apt install -y scdaemon subversion telnet tldr tree w3m wget
                sudo apt install -y wordnet imagemagick
                sudo apt install -y pandoc poppler-utils poppler-data libtool
                sudo apt install -y shfmt editorconfig glslang-tools glslang-dev shellcheck parallel
                sudo apt install -y tidy sqlite3 pkg-config bison flex
                sudo apt install -y cmake ninja-build ccache meson
                sudo apt install -y valgrind minicom mc strace
                sudo apt install -y ffmpegthumbnailer mediainfo
                sudo apt install -y feh mpv qrencode findutils locate
                sudo apt install -y ncurses-term
                sudo apt install -y gnuplot
                sudo apt install -y telnet
                sudo apt install -y libreoffice

                # Python
                sudo apt install -y python-is-python3 python3-pip

                # Install Firefox Browser from Extrarepo
                sudo extrepo search mozilla
                sudo extrepo enable mozilla
                sudo apt --update upgrade
                sudo apt install firefox

                # Install Brave Browser from Extrarepo
                sudo extrepo search brave
                sudo extrepo enable brave_release
                sudo apt --update upgrade
                sudo apt install brave-browser

                # TODO: Install latest Node.js
                # TODO: Install latest LLVM
                # TODO: PSD (profile-sync-daemon) from source

                # Wayland Software
                if [[ "$WINDOW_SYSTEM" == "$WS_WAYLAND" ]]; then
                    sudo apt install -y wl-clipboard wayland-utils
                fi

                # X11 Software
                if [[ "$WINDOW_SYSTEM" == "$WS_X11" ]]; then
                    sudo apt install -y xsel xclip xinput xdotool x11-utils
                fi

                # Gnome Software
                if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                    sudo apt -y install -y dconf-editor gnome-tweaks
                fi

                # Plasma Software
                if [[ "$WINDOW_MANAGER" == "$WM_KDE" ]]; then
                    sudo apt install -y kdeconnect
                    sudo apt install -y kdotool
                    # sudo dnf install -y yakuake
                    # sudo dnf install -y latte-dock
                    # sudo zypper install -y kazam
                fi

                # Git stuff
                sudo apt install -y git
                # sudo apt install -y git-email   # support for email based workflow
                # sudo apt install -y difftastic  # better diffs
                # sudo apt install -y git-delta   # better diffs
                # sudo apt install -y lazygit     # terminal git

                # tmux: A terminal multiplexer
                sudo apt install -y tmux

                # Alacritty - A fast, cross-platform, OpenGL terminal emulator
                # sudo apt install -y alacritty

                # foot - the fast, lightweight and minimalistic Wayland terminal emulator.
                [[ "$WINDOW_SYSTEM" == "$WS_WAYLAND" ]] && sudo apt install -y foot

                # FZF
                sudo apt install -y fzf
                # dpkg -L fzf | grep bindings # Source both completion and keybindings from output of

                # gpg
                sudo apt install -y gpg gnupg2 gnupg-pkcs11-scd pcsc-tools opensc vsmartcard-vpcd
                sudo systemctl enable pcscd.service && sudo systemctl start pcscd.service

                # yubikey
                sudo apt install -y yubikey-manager

                # starship (https://starship.rs/)
                curl -sS https://starship.rs/install.sh | sh

                # ncdu (text-based disk usage viewer)
                sudo apt install -y ncdu

                # borg
                sudo apt install -y borgbackup

                # Neovim
                sudo apt install -y neovim

                # latex base
                sudo apt install -y texlive-base texinfo

                # non apt software
                sudo pip3 install cmake-language-server --break-system-packages
                sudo pip3 install pyright --break-system-packages
                # sudo npm install --location=global npm@latest
                # sudo npm install --location=global prettier
                # sudo npm install --location=global js-beautify
                # sudo npm install --location=global typescript-language-server typescript
                # sudo npm install --location=global dockerfile-language-server-nodejs
                # sudo npm install --location=global bash-language-server
                # sudo npm install --location=global @devcontainers/cli
                # sudo npm install --location=global @google/gemini-cli

                read -rp "$CHOICE) Software has been installed. Press enter to continue..."
                ;;
            5)
                echo "$CHOICE) Installing Extras"

                #
                # non-free codecs and other media components
                #

                sudo apt --update upgrade -y
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
                     kdegraphics-thumbnailers && \
                    sudo dpkg-reconfigure libdvd-pkg

                #
                # Firewall (ufw)
                #

                sudo apt install -y ufw
                sudo ufw allow ssh
                sudo ufw allow http
                sudo ufw allow https
                sudo ufw enable && sudo ufw status

                #
                # Docker (https://docs.docker.com/engine/install/debian/)
                #

                #
                # Install podman
                #

                sudo apt install -y podman

                #
                # Latex Full
                #

                sudo apt install -y texlive-full

                #
                # Steam extras
                #

                sudo apt install -y steam-devices

                #
                # Fonts
                #

                sudo apt install -y fonts-jetbrains-mono
                install_berkeley_mono_font

                # Remove pre-installed Gnome games
                if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                    sudo apt purge iagno lightsoff four-in-a-row gnome-robots pegsolitaire gnome-2048 \
                         hitori gnome-klotski gnome-mines gnome-mahjongg gnome-sudoku quadrapassel swell-foop \
                         gnome-tetravex gnome-taquin aisleriot gnome-chess five-or-more gnome-nibbles tali
                    sudo apt autoremove
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

                read -rp "$CHOICE) Extras installed. Press enter to continue..."
                ;;

            6)
                echo "$CHOICE) Enabling Flatpak"

                sudo apt install -y flatpak
                flatpak --user remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
                sudo flatpak update

                if [[ "$WINDOW_MANAGER" == "$WM_KDE" ]]; then
                    sudo apt install -y plasma-discover-backend-flatpak
                fi

                if [[ "$WINDOW_MANAGER" == "$WM_GNOME" ]]; then
                    sudo apt install -y gnome-software-plugin-flatpak
                    flatpak install --user -y flathub org.gnome.TextEditor
                    flatpak install --user -y flathub org.gnome.Firmware
                    flatpak install --user -y flathub com.mattjakeman.ExtensionManager
                fi
                # flatpak install --user -y flathub io.github.kolunmi.Bazaar
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
                flatpak install --user -y flathub com.valvesoftware.Steam
                flatpak install --user -y org.freedesktop.Platform.VulkanLayer.MangoHud # this is needed for steam!
                # TODO: flatpak install --user -y flathub io.podman_desktop.PodmanDesktop
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

                read -rp "$CHOICE) Flatpak has now been enabled. Press enter to continue..."
                ;;
            7)
                echo "$CHOICE) Setting up secrets and repos"
                setup-secrets-and-repos

                read -rp "$CHOICE) Secrets and repos are set. Press enter to continue..."
                ;;
            8)
                echo "$CHOICE) Installing Emacs"

                # Is it already installed?
                hash emacs 2> /dev/null &&
                    echo "$CHOICE) Emacs is already installed"  && exit 1

                # libgccjit-12 should match the current gcc in the system (gcc --version)
                echo "Current gcc version $(gcc --version)"
                read -rp "Is gcc version 12.*? (Y/N): " confirm &&
                    [[ $confirm == [yY] || $confirm == [yY][eE][sS] ]] || exit 1

                # Install dependencies
                sudo apt build-dep emacs
                sudo apt install -y build-essential autoconf automake libtool texinfo libgtk-3-dev libxpm-dev \
                     libjpeg-dev libgif-dev libtiff5-dev gnutls-bin libgnutls30 libgnutlsxx30 libncurses-dev \
                     libxml2-dev libgpm-dev libdbus-1-dev libgtk2.0-dev libpng-dev libotf-dev libm17n-dev \
                     librsvg2-dev libmagickcore-dev libmagickwand-dev libglib2.0-dev libgirepository1.0-dev
                sudo apt install -y libgccjit0 libgccjit-12-dev # make sure is the same version as `gcc --version`
                sudo apt install -y libtree-sitter0 libtree-sitter-dev
                sudo apt install -y libjansson4 libjansson-dev
                sudo apt install -y libvterm0 libvterm-dev
                sudo apt install -y libwebp-dev webp
                sudo apt install -y libxft-dev libxft2
                sudo apt install -y libenchant-2-dev pkgconf # necessary for jinx
                sudo apt install -y libtool-bin # vterm

                # do install emacs from source
                install-emacs

                read -rp "$CHOICE) Emacs has been installed. Press enter to continue..."
                ;;
            9)
                echo "$CHOICE) Installing Xremap"

                [[ "$WINDOW_SYSTEM" == "$WS_X11" ]] && sudo apt install -y libx11-dev

                # Is it already installed?
                hash xremap 2> /dev/null && echo "Xremap is already installed" && exit 1

                # do install
                install_xremap

                read -rp "$CHOICE) Xremap installed. Reboot for udev rules to take effect.
                              On Gnome, install the extension at https://extensions.gnome.org/extension/5060/xremap/.
                              Press enter to continue..."
                ;;
            10)
                echo "$CHOICE) Installing PSD (profile-sync-daemon)"

                # Dependencies
                sudo apt install -y coreutils findutils glib2 kmod rsync systemd

                # Install
                install_psd

                read -rp "$CHOICE) Done. Press enter to continue..."
                ;;
            11)
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
            debian_install "$@"
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
