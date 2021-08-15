#!/bin/bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in $HOME/dotfiles
############################

. "$(dirname "$0")/Common/.bin/common.sh"

CURRENT_SHELL=$(find_current_shell)
CURRENT_ENV=$(find_env)
CURRENT_OS=$(find_os)
DOTFILES_DIR=$(find_dotfiles)

echo "===================================================================================="
echo ">> Running ($CURRENT_SHELL) for '$CURRENT_ENV' on '$CURRENT_OS' at '$DOTFILES_DIR'."

install_zsh() {
    # Test to see if zshell is installed.  If it is:
    if [ "$CURRENT_OS" = "LINUX" ]; then
        if [ -f /bin/zsh ] || [ -f /usr/bin/zsh ]; then
            # Clone my oh-my-zsh repository from GitHub only if it isn't already present
            if [ ! -d "$DOTFILES_DIR/.oh-my-zsh/" ]; then
                git clone http://github.com/robbyrussell/oh-my-zsh.git "$DOTFILES_DIR/.oh-my-zsh"
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
        if [ -f /usr/local/bin/zsh ] || [ -f /usr/bin/zsh ]; then
            # Clone my oh-my-zsh repository from GitHub only if it isn't already present
            if [ ! -d "$DOTFILES_DIR/.oh-my-zsh/" ]; then
                git clone http://github.com/robbyrussell/oh-my-zsh.git "$DOTFILES_DIR/.oh-my-zsh"
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
            git clone -b develop --single-branch git@github.com:hlissner/doom-emacs.git "$DOTFILES_DIR/.emacs.d"
            "$DOTFILES_DIR/.emacs.d/bin/doom" install
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
files=".doom.d .oh-my-zsh.d .bin .profile .bashrc .zshenv .zshrc .tmux.conf .tmux-macos.conf .tmux-linux.conf .clang-tidy .editorconfig .sbclrc .rustfmt.toml .stylelint.rc"
for file in $files; do
    unlink "$HOME/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/"
done

echo ">> Linking other files in $HOME..."
files=".clang-format .fzf.zsh"
for file in $files; do
    unlink "$HOME/$file"

    if [ "$CURRENT_ENV" = "PERSONAL" ]; then
        ln -s "$DOTFILES_DIR/Personal/$file" "$HOME/"
    elif [ "$CURRENT_ENV" = "WORK" ]; then
        ln -s "$DOTFILES_DIR/Work/$file" "$HOME/"
    fi
done

echo ">> Linking other files in $HOME/.config"
files="alacritty git"
for file in $files; do
    unlink "$HOME/.config/$file"

    if [ "$CURRENT_ENV" = "PERSONAL" ]; then
        ln -s "$DOTFILES_DIR/Personal/$file" "$HOME/.config"
    elif [ "$CURRENT_ENV" = "WORK" ]; then
        ln -s "$DOTFILES_DIR/Work/$file" "$HOME/.config"
    fi
done

echo ">> All Done!"
echo "===================================================================================="
