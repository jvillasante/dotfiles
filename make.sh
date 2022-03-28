#!/bin/bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in $HOME/dotfiles
############################

. "$(dirname "$0")/Common/.bin/common.sh"

CURRENT_SHELL=$(find_current_shell)
CURRENT_HOST=$(find_host)
CURRENT_OS=$(find_os)
DOTFILES_DIR=$(find_dotfiles)

echo "===================================================================================="
echo ">> Running ($CURRENT_SHELL) for '$CURRENT_HOST' on '$CURRENT_OS' at '$DOTFILES_DIR'."

install_zsh() {
    # Test to see if zshell is installed. If it is:
    if [ "$CURRENT_OS" = "LINUX" ]; then
        if [ -f /bin/zsh ] || [ -f /usr/bin/zsh ]; then
            # Clone my oh-my-zsh repository from GitHub only if it isn't already present
            if [ ! -d "$DOTFILES_DIR/.oh-my-zsh/" ]; then
                git clone https://github.com/ohmyzsh/ohmyzsh.git "$DOTFILES_DIR/.oh-my-zsh"
            fi

            # Set the default shell to zsh if it isn't currently set to zsh
            if [ ! "$SHELL" = "$(which zsh)" ]; then
                chsh -s "$(which zsh)"
                echo ">> Your need to re-login :(                                        "
            fi
        else
            # If zsh isn't installed, If the platform is Linux, try an apt-get to install zsh and then recurse
            sudo apt-get install zsh
            install_zsh
        fi
    elif [ "$CURRENT_OS" = "OSX" ]; then
        if [ -f /bin/zsh ] || [ -f /usr/local/bin/zsh ] || [ -f /usr/bin/zsh ]; then
            # Clone my oh-my-zsh repository from GitHub only if it isn't already present
            if [ ! -d "$DOTFILES_DIR/.oh-my-zsh/" ]; then
                git clone https://github.com/ohmyzsh/ohmyzsh.git "$DOTFILES_DIR/.oh-my-zsh"
            fi

            echo ">> Remember to set the default shell to zsh if it isn't already         "
        else
            echo ">> Remember to install zsh                                              "
        fi
    fi
}

# CURRENT_EMACS_DISTRO="SPACEMACS"
CURRENT_EMACS_DISTRO="DOOM_EMACS"
install_emacs() {
    if [ ! -d "$DOTFILES_DIR/.emacs.d/" ]; then
        if [ "$CURRENT_EMACS_DISTRO" = "SPACEMACS" ]; then
            git clone -b develop --single-branch git@github.com:syl20bnr/spacemacs.git "$DOTFILES_DIR/.emacs.d"
        elif [ "$CURRENT_EMACS_DISTRO" = "DOOM_EMACS" ]; then
            git clone --depth 1 git@github.com:hlissner/doom-emacs.git "$DOTFILES_DIR/.emacs.d"
            "$DOTFILES_DIR/.emacs.d/bin/doom" install

            # git clone -b develop --single-branch git@github.com:hlissner/doom-emacs.git "$DOTFILES_DIR/.emacs.d"
            # "$DOTFILES_DIR/.emacs.d/bin/doom" install
        else
            echo "Set CURRENT_EMACS_DISTRO!"
        fi
    fi
}

install_zsh
install_emacs

echo ">> Linking global files in $HOME"
files=".emacs.d .oh-my-zsh"
for file in $files; do
    unlink "$HOME/$file"
    ln -s "$DOTFILES_DIR/$file" "$HOME/"
done

echo ">> Linking common files in $HOME..."
files=".doom.d .oh-my-zsh.d .bin .profile .bashrc .zshenv .zshrc .editorconfig .sbclrc"
for file in $files; do
    unlink "$HOME/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/"
done

echo ">> Linking other files in $HOME..."
files=".fzf.zsh"
for file in $files; do
    unlink "$HOME/$file"

    if [ -d "$DOTFILES_DIR/Hosts/$CURRENT_HOST/" ]; then
        ln -s "$DOTFILES_DIR/Hosts/$CURRENT_HOST/$file" "$HOME/"
    else
        echo ">> $DOTFILES_DIR/Hosts/$CURRENT_HOST does not exits, exiting..."
        exit 1
    fi
done

echo ">> Linking common files in $HOME/.config..."
files="shell tmux ranger rofi nvim psd i3 nushell starship.toml"
for file in $files; do
    unlink "$HOME/.config/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/.config"
done

echo ">> Linking other files in $HOME/.config..."
files="alacritty git"
for file in $files; do
    unlink "$HOME/.config/$file"

    if [ -d "$DOTFILES_DIR/Hosts/$CURRENT_HOST/" ]; then
        ln -s "$DOTFILES_DIR/Hosts/$CURRENT_HOST/$file" "$HOME/.config"
    else
        echo ">> $DOTFILES_DIR/Hosts/$CURRENT_HOST does not exits, exiting..."
        exit 1
    fi
done

echo ">> Linking systemd user files in $HOME/.config/systemd/user..."
files="emacs.service"
for file in $files; do
    unlink "$HOME/.config/systemd/user/$file"
    ln -s "$DOTFILES_DIR/Common/systemd/user/$file" "$HOME/.config/systemd/user"
done

echo ">> Linking desktop application files in $HOME/.local/share/applications..."
files="emacsclient.desktop"
for file in $files; do
    unlink "$HOME/.local/share/applications/$file"
    ln -s "$DOTFILES_DIR/Common/applications/$file" "$HOME/.local/share/applications"
done

echo ">> All Done!"
echo "===================================================================================="
