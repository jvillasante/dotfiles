#!/bin/bash
############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

CURRENT_OS="OSX"
find_current_os() {
  platform=$(uname)
  case "$platform" in
    "Darwin")
      {
        echo ">> Running on Mac OSX."
        CURRENT_OS="OSX"
      } ;;
    "Linux")
      {
        echo ">> Running on LINUX."
        CURRENT_OS="LINUX"
      } ;;
    *)
      {
        echo ">> Unsupported OS, exiting!!!"
        exit
      } ;;
  esac
}

echo "========================================================================"
find_current_os

if [[ $CURRENT_OS == "OSX" ]]; then
  dir=~/Hacking/workspace/dotfiles             # dotfiles directory
  dir_bak=~/Hacking/workspace/dotfiles/backup  # existing dotfiles backup
elif [[ $CURRENT_OS == "LINUX" ]]; then
  dir=~/Hacking/workspace/dotfiles             # dotfiles directory
  dir_bak=~/Hacking/workspace/dotfiles/backup  # existing dotfiles backup
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

      echo "========================================================================"
      echo ">> Remember to set the default shell to zsh if it isn't already         "
      echo "========================================================================"
    else
      echo "========================================================================"
      echo ">> Remember to install zsh                                              "
      echo "========================================================================"
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
files="bin .spacemacs.d .oh-my-zsh .emacs.d .percol.d .ycm_extra_conf.py .clang_complete .clang-format .bashrc .editorconfig .gitconfig .jsbeautifyrc .jshintrc .profile .tmux.conf .zshenv .zshrc .sbclrc"

for file in $files; do
  # echo "Deleting old file $file in home directory..."
  unlink ~/$file

  # echo "Creating symlink to $file in home directory...."
  ln -s $dir/$file ~/
done

echo "==========================================================================="
echo ">> Remember to change alias ls='/usr/local/bin/gls -AlFh --color' in .zshrc"
echo ">> Remember to change 'email = jvillasantegomez@gmail.com' in .gitconfig   "
echo "==========================================================================="
