#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

dir=~/Hacking/workspace/dotfiles             # dotfiles directory
dir_bak=~/Hacking/workspace/dotfiles/backup  # existing dotfiles backup

install_zsh () {
    # Test to see if zshell is installed.  If it is:
    if [ -f /bin/zsh -o -f /usr/bin/zsh ]; then
        # Clone my oh-my-zsh repository from GitHub only if it isn't already present
        if [[ ! -d $dir/.oh-my-zsh/ ]]; then
            git clone http://github.com/robbyrussell/oh-my-zsh.git $dir/.oh-my-zsh
        fi
        # Set the default shell to zsh if it isn't currently set to zsh
        # if [[ ! $(echo $SHELL) == $(which zsh) ]]; then
        #     chsh -s $(which zsh)
        # fi
    else
        # If zsh isn't installed, get the platform of the current machine
        platform=$(uname);
        # If the platform is Linux, try an apt-get to install zsh and then recurse
        if [[ $platform == 'Linux' ]]; then
            sudo apt-get install zsh
            install_zsh
            # If the platform is OS X, tell the user to install zsh :)
        elif [[ $platform == 'Darwin' ]]; then
            echo "Please install zsh, then re-run this script!"
            exit
        fi
    fi
}

install_spacemacs () {
    if [[ ! -d $dir/.emacs.d/ ]]; then
        git clone https://github.com/syl20bnr/spacemacs $dir/.emacs.d
    fi
}

install_zsh
install_spacemacs

# list of files/folders to symlink in homedir
files="bin .spacemacs.d .oh-my-zsh .emacs.d .percol.d .clang-format .bashrc .editorconfig .gitconfig .jsbeautifyrc .jshintrc .profile .tmux.conf .zshenv .zshrc .sbclrc"

for file in $files; do
    echo "Deleting old file $file in home directory..."
    unlink ~/$file

    echo "Creating symlink to $file in home directory...."
    ln -s $dir/$file ~/
done
