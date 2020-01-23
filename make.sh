#!/bin/bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

ask() {
    # https://djm.me/ask
    local prompt default reply

    while true; do
        if [ "${2:-}" = "Y" ]; then
            prompt="Y/n"
            default=Y
        elif [ "${2:-}" = "N" ]; then
            prompt="y/N"
            default=N
        else
            prompt="y/n"
            default=
        fi

        # Ask the question (not using "read -p" as it uses stderr not stdout)
        echo "$1 [$prompt] "

        # Read the answer (use /dev/tty in case stdin is redirected from somewhere else)
        read reply </dev/tty

        # Default?
        if [ -z "$reply" ]; then
            reply=$default
        fi

        # Check if the reply is valid
        case "$reply" in
            Y*|y*) return 0 ;;
            N*|n*) return 1 ;;
        esac

    done
}

CURRENT_ENV="$1"
find_current_env() {
    case "$CURRENT_ENV" in
        "home")
            {
                CURRENT_ENV="HOME"
            } ;;
        "work")
            {
                CURRENT_ENV="WORK"
            } ;;
        *)
            {
                echo ">> Unsupported ENV: '$CURRENT_ENV'. <home|work> only allowed, exiting!!!"
                exit
            } ;;
    esac
}

CURRENT_OS="OSX"
find_current_os() {
    platform=$(uname)
    case "$platform" in
        "Darwin")
            {
                CURRENT_OS="OSX"
            } ;;
        "Linux")
            {
                CURRENT_OS="LINUX"
            } ;;
        *)
            {
                echo ">> Unsupported OS: '$platform', exiting!!!"
                exit
            } ;;
    esac
}

find_current_env
find_current_os

if [[ $CURRENT_ENV == "HOME" ]]; then
    dir=~/Workspace/Others/dotfiles         # dotfiles directory
elif [[ $CURRENT_ENV == "WORK" ]]; then
    dir=~/Hacking/Software/dotfiles         # dotfiles directory
fi

echo "===================================================================================="
echo ">> Running for '$CURRENT_ENV' on '$CURRENT_OS' at '$dir'."
if ! ask "Do you want to continue?"; then
    echo ">> Done!"
    echo "===================================================================================="
    exit
fi

install_zsh () {
    # Test to see if zshell is installed.  If it is:
    if [[ $CURRENT_OS == "LINUX" ]]; then
        if [ -f /bin/zsh -o -f /usr/bin/zsh ]; then
            # Clone my oh-my-zsh repository from GitHub only if it isn't already present
            if [[ ! -d $dir/.oh-my-zsh/ ]]; then
                git clone http://github.com/robbyrussell/oh-my-zsh.git $dir/.oh-my-zsh
            fi

            # Set the default shell to zsh if it isn't currently set to zsh
            if [[ ! $(echo $SHELL) == $(which zsh) ]]; then
                chsh -s $(which zsh)
            fi
        else
            # If zsh isn't installed, If the platform is Linux, try an apt-get to install zsh and then recurse
            sudo apt-get install zsh
            install_zsh
        fi
    elif [[ $CURRENT_OS == "OSX" ]]; then
        if [ -f /usr/local/bin/zsh -o -f /usr/bin/zsh ]; then
            # Clone my oh-my-zsh repository from GitHub only if it isn't already present
            if [[ ! -d $dir/.oh-my-zsh/ ]]; then
                git clone http://github.com/robbyrussell/oh-my-zsh.git $dir/.oh-my-zsh
            fi

            echo ">> Remember to set the default shell to zsh if it isn't already         "
        else
            echo ">> Remember to install zsh                                              "
        fi
    fi
}

CURRENT_EMACS_DISTRO="SPACEMACS"
# CURRENT_EMACS_DISTRO="DOOM_EMACS"
install_emacs () {
    if [[ ! -d $dir/.emacs.d/ ]]; then
        if [[ $CURRENT_EMACS_DISTRO == "SPACEMACS" ]]; then
            # git clone https://github.com/syl20bnr/spacemacs $dir/.emacs.d             # master branch
            # git clone https://github.com/syl20bnr/spacemacs $dir/.emacs.d -b develop  # develop branch
            git clone -b develop --single-branch git@github.com:syl20bnr/spacemacs.git $dir/.emacs.d
        elif [[ $CURRENT_EMACS_DISTRO == "DOOM_EMACS" ]]; then
            # git clone https://github.com/hlissner/doom-emacs $dir/.emacs.d -b develop
            git clone -b develop --single-branch git@github.com:hlissner/doom-emacs.git $dir/.emacs.d
            $dir/.emacs.d/bin/doom quickstart
        else
            echo "Set CURRENT_EMACS_DISTRO!"
        fi
    fi
}

install_zsh
install_emacs

common_files=".emacs.d .oh-my-zsh .oh-my-zsh.d"
files="bin .spacemacs.d .doom.d .offlineimaprc .offlineimap.py .msmtprc .percol.d .ccls .ycm_extra_conf.py .clang_complete .clang-format .clang-tidy .bashrc .editorconfig .gitconfig .jsbeautifyrc .jshintrc .profile .tmux.conf .tmux_light.conf .tmux_dark.conf .zshenv .zshrc .sbclrc .rustfmt.toml"

echo ">> Linking common files..."
for file in $common_files; do
    unlink ~/$file
    ln -s $dir/$file ~/
done

echo ">> Linking files..."
for file in $files; do
    unlink ~/$file

    if [[ $CURRENT_ENV == "HOME" ]]; then
        ln -s $dir/Home/$file ~/
    elif [[ $CURRENT_ENV == "WORK" ]]; then
        ln -s $dir/Work/$file ~/
    fi
done

echo ">> Done!"
echo "===================================================================================="
