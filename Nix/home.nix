{ config, pkgs, lib, currentSystem, ... }:

let
  inherit (builtins) currentSystem;
  inherit (lib.systems.elaborate { system = currentSystem; }) isLinux isDarwin;
  wayland-packages = with pkgs; [
    emacsPgtkGcc
    firefox-wayland
    grim
    slurp
    swaylock-fancy
    wlsunset
    wofi
    xdg-desktop-portal-wlr
  ];
  linuxPackages = with pkgs; [
    anki
    bemenu
    discord
    docker
    docker-compose
    dragon-drop
    evince
    exfat
    feh
    gnome3.cheese
    gnumake
    gnupg
    killall
    libreoffice
    lightlocker
    mpv
    musescore
    networkmanager
    nextcloud-client
    nix-update
    paper-icon-theme
    poppler_utils
    rhythmbox
    slack
    system-config-printer
    tdesktop
    thunderbird
    tor-browser-bundle-bin
    transmission-gtk
    ungoogled-chromium
    vlc
    whois
    wpa_supplicant
    xorg.xkill
    zoom-us
  ];
  darwinPackages = with pkgs; [ binutils coreutils ];
  commonPackages = with pkgs; [
    ag
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    autojump
    atool
    autoconf
    automake
    bat
    cmake
    cppcheck
    curl
    dos2unix
    doxygen
    exa
    fd
    fzf
    gd
    gnumake
    graphviz
    htop
    isync
    libpcap
    mercurial
    msmtp
    neofetch
    neovim
    nixfmt
    pandoc
    poppler
    ranger
    ripgrep
    shfmt
    subversion
    (texlive.combine {
      inherit (texlive)
        scheme-full latexmk wrapfig rotfloat capt-of minted fvextra upquote
        catchfile xstring framed biblatex csquotes;
    })
    telnet
    tldr
    tmux
    tree
    unzip
    w3m
    wget
    wordnet
    youtube-dl
    zip
    zlib
  ];
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jvillasante";
  home.homeDirectory =
    if isDarwin then "/Users/jvillasante" else "/home/jvillasante";

  # Packages to install
  home.packages = commonPackages ++ (lib.optionals isLinux linuxPackages)
    ++ (lib.optionals isDarwin darwinPackages);

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
