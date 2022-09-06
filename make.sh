#!/usr/bin/env bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in $HOME/dotfiles
############################

. "$(dirname "$0")/Common/.bin/common.sh"

CURRENT_SHELL=$(find_current_shell)
CURRENT_HOST=$(find_host)
CURRENT_OS=$(find_os)
DOTFILES_DIR=$(find_dotfiles)
echo ">>> Running ($CURRENT_SHELL) for '$CURRENT_HOST' on '$CURRENT_OS' at '$DOTFILES_DIR'."

install_shell() {
    case $1 in
        bash)
            if type bash >/dev/null 2>&1; then
                if [ ! -d "$DOTFILES_DIR/.oh-my-bash/" ]; then
                    export OSH="$DOTFILES_DIR/.oh-my-bash"
                    bash -c "$(curl -fsSL https://raw.githubusercontent.com/ohmybash/oh-my-bash/master/tools/install.sh)"
                fi

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
                if [ ! -d "$DOTFILES_DIR/.oh-my-zsh/" ]; then
                    git clone https://github.com/ohmyzsh/ohmyzsh.git "$DOTFILES_DIR/.oh-my-zsh"
                fi

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
    if [ ! -d "$EMACSDIR" ]; then
        echo ">>> Installing Doom Emacs at $EMACSDIR with personal config at $DOOMDIR"
        git clone git@github.com:hlissner/doom-emacs.git "$EMACSDIR"
        check $?

        "$EMACSDIR"/bin/doom install
        check $?
    fi

    # Install Crafted Emacs
    if [ ! -d "$DOTFILES_DIR/.emacs.crafted" ]; then
        git clone git@github.com:SystemCrafters/crafted-emacs.git "$DOTFILES_DIR/.emacs.crafted"
        check $?
    fi
}

install_vim() {
    # TODO: Try to install nvim here if it is not installed already
    echo ">>> Installing vim... (TODO)"
}

install_tmuxifier() {
    if [ ! -d "$DOTFILES_DIR/.tmuxifier" ]; then
        git clone --depth 1 git@github.com:jimeh/tmuxifier.git "$DOTFILES_DIR/.tmuxifier"
        check $?
    fi
}

install_shell "bash"
install_emacs
install_vim
install_tmuxifier

echo ">>> Linking global files in $HOME"
files=".emacs.d .tmuxifier"
[ -n "$ZSH_VERSION" ] && files+=" .oh-my-zsh"
for file in $files; do
    unlink "$HOME/$file"
    ln -s "$DOTFILES_DIR/$file" "$HOME/"
done

echo ">>> Linking common files in $HOME..."
files=".bin .editorconfig .emacs-profiles.el .sbclrc .mbsyncrc .msmtprc"
[ -n "$BASH_VERSION" ] && files+=" .bash_profile .bashrc"
[ -n "$ZSH_VERSION" ] && files+=" .oh-my-zsh.d .zshenv .zshrc"
for file in $files; do
    unlink "$HOME/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/"
done

echo ">>> Linking common files in $HOME/.config..."
files="git nyxt alacritty shell tmux ranger rofi nvim psd i3 nushell starship.toml"
for file in $files; do
    unlink "$HOME/.config/$file"
    ln -s "$DOTFILES_DIR/Common/$file" "$HOME/.config"
done

echo ">>> Linking systemd user files in $HOME/.config/systemd/user..."
files="emacs.service"
for file in $files; do
    unlink "$HOME/.config/systemd/user/$file"
    ln -s "$DOTFILES_DIR/Common/systemd/user/$file" "$HOME/.config/systemd/user"
done

echo ">>> Linking desktop application files in $HOME/.local/share/applications..."
files="emacs.desktop emacsclient.desktop"
for file in $files; do
    unlink "$HOME/.local/share/applications/$file"
    ln -s "$DOTFILES_DIR/Common/applications/$file" "$HOME/.local/share/applications"
done

echo ">>> All Done!"
