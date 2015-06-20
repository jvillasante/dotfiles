#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

dir=~/Hacking/workspace/dotfiles           # dotfiles directory

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
        git clone --recursive http://github.com/syl20bnr/spacemacs $dir/.emacs.d
    fi
}

install_neobundle() {
    if [[ ! -d $dir/.vim/bundle/ ]]; then
        mkdir -p ~/.vim/bundle
        git clone https://github.com/Shougo/neobundle.vim $dir/.vim/bundle/neobundle.vim
    fi
}
install_zsh
install_spacemacs
# install_neobundle

# list of files/folders to symlink in homedir
files=".bashrc .editorconfig .gitconfig .profile .spacemacs .tmux.conf .zshrc .oh-my-zsh .vimrc .vimrc.before .vim .emacs.d .percol.d"

# spacemacs private contribs
echo "Removing existing version of $dir/.emacs.d/private/mycontribs"
rm -rf $dir/.emacs.d/private/mycontribs
echo "Creating symlink to $dir/.spc_private/mycontribs on ~/.emacs.d/private."
ln -s $dir/.spc_private/mycontribs ~/.emacs.d/private

# spacemacs private snippets
echo "Removing existing version of $dir/.emacs.d/private/snippets"
rm -rf $dir/.emacs.d/private/snippets
echo "Creating symlink to $dir/.spc_private/snippets on ~/.emacs.d/private."
ln -s $dir/.spc_private/snippets ~/.emacs.d/private

# move any existing dotfiles in homedir to dotfiles_old directory, then create symlinks from the homedir to any files in the ~/dotfiles directory specified in $files
for file in $files; do
    echo "Removing existing $file from home directory"
    rm -rf ~/$file

    echo "Creating symlink to $file in home directory."
    ln -s $dir/$file ~/
done
