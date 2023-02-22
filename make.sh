#!/usr/bin/env bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in $HOME/dotfiles
############################

. "$(dirname "$0")/Common/shell/system/common.sh"
CURRENT_SHELL=$(find_current_shell)
CURRENT_HOST=$(find_host)
CURRENT_OS=$(find_os)
DOTFILES_DIR=$(find_dotfiles)
echo ">>> Running ($CURRENT_SHELL) for '$CURRENT_HOST' on '$CURRENT_OS' at '$DOTFILES_DIR'."

install_shell() {
    case $1 in
        bash)
            if type bash >/dev/null 2>&1; then
                # Set the default shell to bash if it isn't currently set to bash
                if [ ! "$SHELL" = "$(which bash)" ]; then
                    chsh -s "$(which bash)"
                    echo ">>> You need to re-login :(                                        "
                fi
            else
                # Bash not installed
                sudo dnf install bash || exit 1

                # Run again
                install_shell "bash"
            fi
            ;;
        zsh)
            if type zsh >/dev/null 2>&1; then
                # Set the default shell to zsh if it isn't currently set to zsh
                if [ ! "$SHELL" = "$(which zsh)" ]; then
                    chsh -s "$(which zsh)"
                    echo ">>> You need to re-login :(                                        "
                fi
            else
                # Zsh not installed
                sudo dnf install zsh || exit 1

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
    if ! type emacs >/dev/null 2>&1; then
        echo ">>> Emacs is not installed, exiting..." && exit 1
    fi

    # Install Chemacs2
    if [ ! -d "$DOTFILES_DIR/.emacs.chemacs2" ]; then
        git clone git@github.com:plexus/chemacs2.git "$DOTFILES_DIR/.emacs.chemacs2"
        check $?

        rm -rf "$DOTFILES_DIR/.emacs.d"
        ln -s "$DOTFILES_DIR/.emacs.chemacs2" "$DOTFILES_DIR/.emacs.d"
    fi

    # Install Doom Emacs
    if [ ! -d "$DOTFILES_DIR/.emacs.doom" ]; then
        echo ">>> Installing Doom Emacs at $DOTFILES_DIR/.emacs.doom"
        git clone git@github.com:hlissner/doom-emacs.git "$DOTFILES_DIR/.emacs.doom"
        check $?

        env EMACSDIR="$DOTFILES_DIR/.emacs.doom" \
            env DOOMDIR="$DOTFILES_DIR/Common/emacs/doom" \
            "$DOTFILES_DIR/.emacs.doom/bin/doom" install
        check $?
    fi

    # Install Crafted Emacs
    if [ ! -d "$DOTFILES_DIR/.emacs.crafted" ]; then
        echo ">>> Installing Crafted Emacs at $DOTFILES_DIR/.emacs.crafted"
        git clone git@github.com:SystemCrafters/crafted-emacs.git "$DOTFILES_DIR/.emacs.crafted"
        check $?
    fi

    # Install Prelude
    if [ ! -d "$DOTFILES_DIR/.emacs.prelude" ]; then
        echo ">>> Installing Prelude Emacs at $DOTFILES_DIR/.emacs.prelude"
        curl -L https://github.com/bbatsov/prelude/raw/master/utils/installer.sh |
            env PRELUDE_INSTALL_DIR="$DOTFILES_DIR/.emacs.prelude" sh
        check $?
    fi
}

install_shell "bash"
install_emacs

echo ">>> Linking global files in $HOME"
files=".emacs.d"
for file in $files; do
    [ -L "$HOME/$file" ] && unlink "$HOME/$file"
    ln -s "$DOTFILES_DIR/$file" "$HOME/"
done

echo ">>> Linking common files in $HOME..."
files=".inputrc .editorconfig .emacs-profiles.el .sbclrc .mbsyncrc .msmtprc .tidyrc"
[ -n "$BASH_VERSION" ] && files+=" .bash_profile .bashrc"
[ -n "$ZSH_VERSION" ] && files+=" .zshenv .zshrc"
for file in $files; do
    [ -L "$HOME/$file" ] && unlink "$HOME/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/"
done

echo ">>> Linking common files in $HOME/.config..."
files="git nyxt alacritty foot shell tmux zellij ranger rofi nvim psd i3 nushell starship.toml"
for file in $files; do
    [ -L "$HOME/.config/$file" ] && unlink "$HOME/.config/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/.config"
done

echo ">>> Linking scripts files in $HOME/.local/bin..."
files="+backup +colors +crypt +dotfiles +fedora +firefox +fs +go +mac +pass +project +rust +spell +stocks +ubuntu +zig +zombie"
for file in $files; do
    [ -L "$HOME/.local/bin/$file" ] && unlink "$HOME/.local/bin/$file"
    ln -s "$DOTFILES_DIR/Common/shell/scripts/$file" "$HOME/.local/bin"
done

echo ">>> Linking systemd user files in $HOME/.config/systemd/user..."
files="emacs.service"
for file in $files; do
    [ -L "$HOME/.config/systemd/user/$file" ] && unlink "$HOME/.config/systemd/user/$file"
    ln -s "$DOTFILES_DIR/Common/systemd/user/$file" "$HOME/.config/systemd/user"
done

echo ">>> Linking desktop application files in $HOME/.local/share/applications..."
files="emacs.desktop emacsclient.desktop"
for file in $files; do
    [ -L "$HOME/.local/share/applications/$file" ] && unlink "$HOME/.local/share/applications/$file"
    ln -s "$DOTFILES_DIR/Common/applications/$file" "$HOME/.local/share/applications"
done

echo ">>> Linking work files in $HOME/Workspace/Work..."
if [ -d "$HOME/Workspace/Work" ]; then
    [ -L "$HOME/Workspace/Work/.gitconfig" ] && unlink "$HOME/Workspace/Work/.gitconfig"
    ln -s "$DOTFILES_DIR/Misc/work/.gitconfig" "$HOME/Workspace/Work/"

    if [ -d "$HOME/Workspace/Work/Projects/dmxs" ]; then
        [ -L "$HOME/Workspace/Work/Projects/dmxs/compile_flags.txt" ] && unlink "$HOME/Workspace/Work/Projects/dmxs/compile_flags.txt"
        ln -s "$DOTFILES_DIR/Misc/work/compile_flags.txt" "$HOME/Workspace/Work/Projects/dmxs/"

        [ -L "$HOME/Workspace/Work/Projects/dmxs/.dir-locals.el" ] && unlink "$HOME/Workspace/Work/Projects/dmxs/.dir-locals.el"
        ln -s "$DOTFILES_DIR/Misc/work/dmxs/.dir-locals.el" "$HOME/Workspace/Work/Projects/dmxs/"
    fi
fi

echo ">>> All Done!"
