#!/usr/bin/env bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in $HOME/dotfiles
############################

. "$(dirname "$0")/Common/shell/common.sh"
CURRENT_SHELL=$(find_current_shell)
CURRENT_HOST=$(hostname)
CURRENT_OS=$(find_os)
DOTFILES_DIR=$(find_dotfiles)
echo ">>> Running ($CURRENT_SHELL) for '$CURRENT_HOST' on '$CURRENT_OS' at '$DOTFILES_DIR'."

install_shell() {
    case $1 in
        bash)
            if type bash > /dev/null 2>&1; then
                # Set the default shell to bash if it isn't currently set to bash
                if [ ! "$(readlink /proc/$$/exe)" = "$(which bash)" ]; then
                    chsh -s "$(which bash)"
                    echo ">>> You need to re-login :(                                        "
                fi
            else
                # Bash not installed
                [ -f /etc/fedora-release ] && sudo dnf install bash || exit 1
                [ -f /etc/debian_version ] && sudo apt install bash || exit 1

                # Run again
                install_shell "bash"
            fi
            ;;
        zsh)
            if type zsh > /dev/null 2>&1; then
                # Set the default shell to zsh if it isn't currently set to zsh
                if [ ! "$(readlink /proc/$$/exe)" = "$(which zsh)" ]; then
                    chsh -s "$(which zsh)"
                    echo ">>> You need to re-login :(                                        "
                fi
            else
                # Zsh not installed
                [ -f /etc/fedora-release ] && sudo dnf install zsh || exit 1
                [ -f /etc/debian_version ] && sudo apt install zsh || exit 1

                # Run again
                install_shell "zsh"
            fi
            ;;
        *)
            echo ">>> Unknown shell ($1), exiting..." && exit 1
            ;;
    esac
}

install_emacs() {
    if ! type emacs > /dev/null 2>&1; then
        echo ">>> Emacs is not installed, exiting..." && exit 1
    fi

    # Install Doom Emacs
    # if [ ! -d "$DOTFILES_DIR/.emacs.doom" ]; then
    #     echo ">>> Installing Doom Emacs at $DOTFILES_DIR/.emacs.doom"
    #     git clone git@github.com:hlissner/doom-emacs.git "$DOTFILES_DIR/.emacs.doom"
    #     check $?
    #
    #     env EMACSDIR="$DOTFILES_DIR/.emacs.doom" \
    #         env DOOMDIR="$DOTFILES_DIR/Common/emacs/doom" \
    #         "$DOTFILES_DIR/.emacs.doom/bin/doom" install
    #     check $?
    # fi

    # Install Crafted Emacs
    # if [ ! -d "$DOTFILES_DIR/.emacs.crafted" ]; then
    #     echo ">>> Installing Crafted Emacs at $DOTFILES_DIR/.emacs.crafted"
    #     git clone --depth 1 git@github.com:SystemCrafters/crafted-emacs.git "$DOTFILES_DIR/.emacs.crafted"
    #     check $?
    # fi

    # Install Prelude
    # if [ ! -d "$DOTFILES_DIR/.emacs.prelude" ]; then
    #     echo ">>> Installing Prelude Emacs at $DOTFILES_DIR/.emacs.prelude"
    #     curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh |
    #         env PRELUDE_INSTALL_DIR="$DOTFILES_DIR/.emacs.prelude" sh
    #     check $?
    # fi

    # Install yasnippet shim for treesitter (lives on personal/etc)
    # if [ ! -d "$DOTFILES_DIR/.yasnippet-treesitter-shim" ]; then
    #     echo ">>> Installing Yasnippet Treesitter Ship at $DOTFILES_DIR/.yasnippet-treesitter-shim"
    #     git clone git@github.com:fbrosda/yasnippet-treesitter-shim.git "$DOTFILES_DIR/.yasnippet-treesitter-shim"
    #     check $?
    # fi

    # Install Emacs Minimal
    # if [ ! -d "$DOTFILES_DIR/.emacs.minimal" ]; then
    #     echo ">>> Installing Emacs Minimal at $DOTFILES_DIR/.emacs.minimal"
    #     git clone --depth 1 git@github.com:jamescherti/minimal-emacs.d.git "$DOTFILES_DIR/.emacs.minimal"
    #     check $?
    # else
    #     echo ">>> Updating Emacs Minimal at $DOTFILES_DIR/.emacs.minimal"
    #     git -C "$DOTFILES_DIR/.emacs.minimal" pull
    # fi
}

install_shell "bash"
# install_emacs

echo ">>> Linking common files in $HOME..."
files=".inputrc .editorconfig .sbclrc .mbsyncrc .msmtprc .tidyrc"
[ -n "$BASH_VERSION" ] && files+=" .bash_profile .bashrc"
[ -n "$ZSH_VERSION" ] && files+=" .zshenv .zshrc"
for file in $files; do
    [ -L "$HOME/$file" ] && unlink "$HOME/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/"
done

echo ">>> Linking common files in $HOME/.config..."
files="git btop nyxt alacritty foot shell tmux zellij ranger rofi psd i3 nushell keyd xkeysnail xremap starship.toml clangd"
for file in $files; do
    [ -L "$HOME/.config/$file" ] && unlink "$HOME/.config/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/.config"
done

echo ">>> Linking nvim files in $HOME/.config..."
for dir in "$DOTFILES_DIR"/Common/nvim/*/; do
    dir=${dir%*/}
    dir_name=${dir##*/}
    [ -L "$HOME/.config/nvim-${dir_name}" ] && unlink "$HOME/.config/nvim-${dir_name}"
    ln -s "$dir" "$HOME/.config/nvim-${dir_name}"
done

echo ">>> Linking scripts files in $HOME/.local/bin..."
for file in "$DOTFILES_DIR"/Common/shell/scripts/*; do
    [ -L "$HOME"/.local/bin/"$(basename "$file")" ] && unlink "$HOME"/.local/bin/"$(basename "$file")"
    ln -s "$file" "$HOME"/.local/bin
done

echo ">>> Linking systemd user files in $HOME/.config/systemd/user..."
files="emacs.service xkeysnail.service xremap.service mullvad.service"
for file in $files; do
    [ -L "$HOME/.config/systemd/user/$file" ] && unlink "$HOME/.config/systemd/user/$file"
    ln -s "$DOTFILES_DIR/Common/systemd/user/$file" "$HOME/.config/systemd/user"
done

echo ">>> Linking desktop application files in $HOME/.local/share/applications..."
files="emacs.desktop emacsclient.desktop thinkorswim.desktop"
for file in $files; do
    # Unfortunaly symlinks don't work on KDE, so copy instead
    cp -f "$DOTFILES_DIR/Common/applications/$file" "$HOME/.local/share/applications"
done

echo ">>> Linking flatpak application files in $HOME/.var/app/..."
if [ -d "$HOME/.var/app/engineer.atlas.Nyxt/config/" ]; then
    [ -L "$HOME/.var/app/engineer.atlas.Nyxt/config/nyxt" ] && unlink "$HOME/.var/app/engineer.atlas.Nyxt/config/nyxt"
    ln -s "$DOTFILES_DIR/Common/nyxt" "$HOME/.var/app/engineer.atlas.Nyxt/config/"
fi

echo ">>> Linking work files in $HOME/Workspace/Work..."
if [ -d "$HOME/Workspace/Work/Omicron/Projects" ]; then
    [ -L "$HOME/Workspace/Work/Omicron/Projects/.gitconfig" ] &&
        unlink "$HOME/Workspace/Work/Omicron/Projects/.gitconfig"
    ln -s "$DOTFILES_DIR/Misc/work/.gitconfig" "$HOME/Workspace/Work/Omicron/Projects"

    if [ -d "$HOME/Workspace/Work/Omicron/Projects/nntpcode" ]; then
        [ -L "$HOME/Workspace/Work/Omicron/Projects/nntpcode/compile_flags.txt" ] &&
            unlink "$HOME/Workspace/Work/Omicron/Projects/nntpcode/compile_flags.txt"
        if [ -f /etc/fedora-release ]; then
            ln -s "$DOTFILES_DIR/Misc/work/nntpcode/compile_flags.fedora.txt" \
                "$HOME/Workspace/Work/Omicron/Projects/nntpcode/compile_flags.txt"
        elif [ -f /etc/debian_version ]; then
            ln -s "$DOTFILES_DIR/Misc/work/nntpcode/compile_flags.debian.txt" \
                "$HOME/Workspace/Work/Omicron/Projects/nntpcode/compile_flags.txt"
        elif [ -f /etc/products.d/openSUSE.prod ]; then
            ln -s "$DOTFILES_DIR/Misc/work/nntpcode/compile_flags.suse.txt" \
                "$HOME/Workspace/Work/Omicron/Projects/nntpcode/compile_flags.txt"
        else
            echo ">>> Unknown OS (only fedora and debian are supported)..."
        fi

        # [ -L "$HOME/Workspace/Work/Omicron/Projects/nntpcode/.clangd" ] &&
        #     unlink "$HOME/Workspace/Work/Omicron/Projects/nntpcode/.clangd"
        # ln -s "$DOTFILES_DIR/Misc/.clangd" "$HOME/Workspace/Work/Omicron/Projects/nntpcode/"

        [ -L "$HOME/Workspace/Work/Omicron/Projects/nntpcode/.clang-tidy" ] &&
            unlink "$HOME/Workspace/Work/Omicron/Projects/nntpcode/.clang-tidy"
        ln -s "$DOTFILES_DIR/Misc/work/nntpcode/.clang-tidy" "$HOME/Workspace/Work/Omicron/Projects/nntpcode/"

        [ -L "$HOME/Workspace/Work/Omicron/Projects/nntpcode/.dir-locals.el" ] &&
            unlink "$HOME/Workspace/Work/Omicron/Projects/nntpcode/.dir-locals.el"
        ln -s "$DOTFILES_DIR/Misc/work/nntpcode/.dir-locals.el" "$HOME/Workspace/Work/Omicron/Projects/nntpcode/"
    fi
fi

if [ -d "$HOME/Workspace/Work/Nielsen" ]; then
    [ -L "$HOME/Workspace/Work/Nielsen/.gitconfig" ] && unlink "$HOME/Workspace/Work/Nielsen/.gitconfig"
    ln -s "$DOTFILES_DIR/Misc/work/.gitconfig" "$HOME/Workspace/Work/Nielsen"

    if [ -d "$HOME/Workspace/Work/Nielsen/Projects/dmxs" ]; then
        [ -L "$HOME/Workspace/Work/Nielsen/Projects/dmxs/compile_flags.txt" ] &&
            unlink "$HOME/Workspace/Work/Nielsen/Projects/dmxs/compile_flags.txt"
        if [ -f /etc/fedora-release ]; then
            ln -s "$DOTFILES_DIR/Misc/work/dmxs/compile_flags.fedora.txt" \
                "$HOME/Workspace/Work/Nielsen/Projects/dmxs/compile_flags.txt"
        elif [ -f /etc/debian_version ]; then
            ln -s "$DOTFILES_DIR/Misc/work/dmxs/compile_flags.debian.txt" \
                "$HOME/Workspace/Work/Nielsen/Projects/dmxs/compile_flags.txt"
        elif [ -f /etc/products.d/openSUSE.prod ]; then
            ln -s "$DOTFILES_DIR/Misc/work/dmxs/compile_flags.suse.txt" \
                "$HOME/Workspace/Work/Nielsen/Projects/dmxs/compile_flags.txt"
        else
            echo ">>> Unknown OS (only fedora and debian are supported)..."
        fi

        # [ -L "$HOME/Workspace/Work/Nielsen/Projects/dmxs/.clangd" ] &&
        #     unlink "$HOME/Workspace/Work/Nielsen/Projects/dmxs/.clangd"
        # ln -s "$DOTFILES_DIR/Misc/.clangd" "$HOME/Workspace/Work/Nielsen/Projects/dmxs/"

        [ -L "$HOME/Workspace/Work/Nielsen/Projects/dmxs/.dir-locals.el" ] &&
            unlink "$HOME/Workspace/Work/Nielsen/Projects/dmxs/.dir-locals.el"
        ln -s "$DOTFILES_DIR/Misc/work/dmxs/.dir-locals.el" "$HOME/Workspace/Work/Nielsen/Projects/dmxs/"
    fi

    if [ -d "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee" ]; then
        [ -L "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/compile_flags.txt" ] &&
            unlink "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/compile_flags.txt"
        if [ -f /etc/fedora-release ]; then
            ln -s "$DOTFILES_DIR/Misc/work/sm2-dhcpee/compile_flags.fedora.txt" \
                "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/compile_flags.txt"
        elif [ -f /etc/debian_version ]; then
            ln -s "$DOTFILES_DIR/Misc/work/sm2-dhcpee/compile_flags.debian.txt" \
                "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/compile_flags.txt"
        elif [ -f /etc/products.d/openSUSE.prod ]; then
            ln -s "$DOTFILES_DIR/Misc/work/sm2-dhcpee/compile_flags.suse.txt" \
                "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/compile_flags.txt"
        else
            echo ">>> Unknown OS (only fedora and debian are supported)..."
        fi

        # [ -L "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/ /.clangd" ] &&
        #     unlink "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/ /.clangd"
        # ln -s "$DOTFILES_DIR/Misc/.clangd" "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/"

        [ -L "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/.dir-locals.el" ] &&
            unlink "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/.dir-locals.el"
        ln -s "$DOTFILES_DIR/Misc/work/sm2-dhcpee/.dir-locals.el" "$HOME/Workspace/Work/Nielsen/Projects/sm2-dhcpee/"
    fi

    if [ -d "$HOME/Workspace/Work/Nielsen/Projects/smi" ]; then
        [ -L "$HOME/Workspace/Work/Nielsen/Projects/smi/.dir-locals.el" ] &&
            unlink "$HOME/Workspace/Work/Nielsen/Projects/smi/.dir-locals.el"
        ln -s "$DOTFILES_DIR/Misc/work/smi/.dir-locals.el" "$HOME/Workspace/Work/Nielsen/Projects/smi/"
    fi
fi

echo ">>> All Done!"
