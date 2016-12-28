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
        if [[ ! $(echo $SHELL) == $(which zsh) ]]; then
            chsh -s $(which zsh)
        fi
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
        # git clone --recursive http://github.com/syl20bnr/spacemacs $dir/.emacs.d
        git clone https://github.com/syl20bnr/spacemacs $dir/.emacs.d
    fi
}

# install_neovim () {
#     if [[ ! -d ~/.config/nvim/ ]]; then
#         mkdir ~/.config/nvim/
#     fi
# }

install_zsh
install_spacemacs
# install_neovim

# Create backup folder if not exist
if [[ ! -d $dir_bak/ ]]; then
    mkdir $dir_bak/
fi

# spacemacs private
echo "Removing existing version of $dir/.emacs.d/private/"
rm -rf $dir/.emacs.d/private/*
echo "Copying $dir/private_layers/ on ~/.emacs.d/private."
cp -R $dir/private_layers/contrib/* $dir/.emacs.d/private
echo "Copying $dir/private_layers/snippets on ~/.emacs.d/private."
cp -R $dir/private_layers/snippets $dir/.emacs.d/private
echo "Copying $dir/private_layers/local on ~/.emacs.d/private."
cp -R $dir/private_layers/local $dir/.emacs.d/private
echo "Copying $dir/private_layers/README.md on ~/.emacs.d/private."
cp -R $dir/private_layers/README.md $dir/.emacs.d/private
echo "Removing old backup."
rm -rf $dir_bak/*

# list of files/folders to symlink in homedir
files="bin .clang-format .bashrc .editorconfig .gitconfig .jsbeautifyrc .jshintrc .mbsyncrc .msmtprc .profile .spacemacs .tern-project .tmux.conf .zshenv .zshrc .oh-my-zsh .emacs.d .percol.d .tmuxp"

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
for file in $files; do
    echo "Backing upexisting $file from home directory."
    mv -f ~/$file $dir_bak

    echo "Creating symlink to $file in home directory."
    ln -fs $dir/$file ~/
done

echo "linking redshift.conf"
ln -fs $dir/redshift.conf ~/.config/redshift.conf

# echo "linking .neovimrc"
# ln -fs $dir/.neovimrc ~/.config/nvim/init.vim

echo "linking emacs.service"
if [[ ! -d ~/.config/systemd/user/ ]]; then
    mkdir -p ~/.config/systemd/user/
fi
cp -u $dir/emacs.service ~/.config/systemd/user/emacs.service
