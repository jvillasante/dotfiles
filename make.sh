#!/bin/bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

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
        echo ">> Unsupported ENV: '$CURRENT_ENV', exiting!!!"
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

echo "========================================================================"
find_current_env
find_current_os
echo ">> Running for '$CURRENT_ENV' on '$CURRENT_OS'."

if [[ $CURRENT_ENV == "HOME" ]]; then
  dir=~/Hacking/workspace/dotfiles             # dotfiles directory
  dir_bak=~/Hacking/workspace/dotfiles/backup  # existing dotfiles backup
elif [[ $CURRENT_ENV == "WORK" ]]; then
  dir=~/Hacking/Software/dotfiles              # dotfiles directory
  dir_bak=~/Hacking/Software/dotfiles/backup   # existing dotfiles backup
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

CURRENT_EMACS_DISTRO="SPACEMACS"
# CURRENT_EMACS_DISTRO="DOOM_EMACS"
install_emacs () {
  if [[ ! -d $dir/.emacs.d/ ]]; then
    if [[ $CURRENT_EMACS_DISTRO == "SPACEMACS" ]]; then
      # git clone https://github.com/syl20bnr/spacemacs $dir/.emacs.d             # master branch
      git clone https://github.com/syl20bnr/spacemacs $dir/.emacs.d -b develop  # develop branch
    elif [[ $CURRENT_EMACS_DISTRO == "DOOM_EMACS" ]]; then
      git clone https://github.com/hlissner/doom-emacs $dir/.emacs.d -b develop
      cd $dir/.emacs.d
      make install
      cd -
    else
      echo "Set CURRENT_EMACS_DISTRO!"
    fi
  fi
}

install_zsh
install_emacs

common_files=".emacs.d .oh-my-zsh"
files="bin .spacemacs.d .doom.d .percol.d .ycm_extra_conf.py .clang_complete .clang-format .bashrc .editorconfig .gitconfig .jsbeautifyrc .jshintrc .profile .tmux.conf .zshenv .zshrc .sbclrc"

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

# echo ">> Linking neovim files..."
# unlink ~/.config/nvim
# ln -s $dir/nvim ~/.config/

echo "==========================================================================="
echo ">> Remember to change alias ls='/usr/local/bin/gls -AlFh --color' in .zshrc"
echo ">> Remember to change 'email = jvillasantegomez@gmail.com' in .gitconfig   "
echo "==========================================================================="
