#!/bin/bash

############################
# .make.sh
# This script creates symlinks from the home directory to any desired dotfiles in ~/dotfiles
############################

. $(dirname "$0")/Common/.bin/common.sh

CURRENT_ENV=$(find_env)
CURRENT_OS=$(find_os)
DOTFILES_DIR=$(find_dotfiles)

echo "===================================================================================="
echo ">> Running for '$CURRENT_ENV' on '$CURRENT_OS' at '$DOTFILES_DIR'."

install_zsh() {
	# Test to see if zshell is installed.  If it is:
	if [ $CURRENT_OS = "LINUX" ]; then
		if [ -f /bin/zsh -o -f /usr/bin/zsh ]; then
			# Clone my oh-my-zsh repository from GitHub only if it isn't already present
			if [ ! -d $DOTFILES_DIR/.oh-my-zsh/ ]; then
				git clone http://github.com/robbyrussell/oh-my-zsh.git $DOTFILES_DIR/.oh-my-zsh
			fi

			# Set the default shell to zsh if it isn't currently set to zsh
			if [ ! $(echo $SHELL) = $(which zsh) ]; then
				chsh -s $(which zsh)
			fi
		else
			# If zsh isn't installed, If the platform is Linux, try an apt-get to install zsh and then recurse
			sudo apt-get install zsh
			install_zsh
		fi
	elif [ $CURRENT_OS = "OSX" ]; then
		if [ -f /usr/local/bin/zsh -o -f /usr/bin/zsh ]; then
			# Clone my oh-my-zsh repository from GitHub only if it isn't already present
			if [ ! -d $DOTFILES_DIR/.oh-my-zsh/ ]; then
				git clone http://github.com/robbyrussell/oh-my-zsh.git $DOTFILES_DIR/.oh-my-zsh
			fi

			echo ">> Remember to set the default shell to zsh if it isn't already         "
		else
			echo ">> Remember to install zsh                                              "
		fi
	fi
}

# CURRENT_VIM_DISTRO="vim"
CURRENT_VIM_DISTRO="nvim"
install_vimplug() {
	if [ $CURRENT_VIM_DISTRO = "nvim" ]; then
		if [ ! -f ~/.local/share/nvim/site/autoload/plug.vim ]; then
			curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
				https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
		fi
	elif [ $CURRENT_VIM_DISTRO = "vim" ]; then
		if [ ! -f ~/.vim/autoload/plug.vim ]; then
			curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
				https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
		fi
	else
		echo "Set CURRENT_VIM_DISTRO!"
	fi
}

# CURRENT_EMACS_DISTRO="SPACEMACS"
CURRENT_EMACS_DISTRO="DOOM_EMACS"
install_emacs() {
	if [ ! -d $DOTFILES_DIR/.emacs.d/ ]; then
		if [ $CURRENT_EMACS_DISTRO = "SPACEMACS" ]; then
			git clone -b develop --single-branch git@github.com:syl20bnr/spacemacs.git $DOTFILES_DIR/.emacs.d
		elif [ $CURRENT_EMACS_DISTRO = "DOOM_EMACS" ]; then
			git clone -b develop --single-branch git@github.com:hlissner/doom-emacs.git $DOTFILES_DIR/.emacs.d
			$DOTFILES_DIR/.emacs.d/bin/doom install
		else
			echo "Set CURRENT_EMACS_DISTRO!"
		fi
	fi
}

install_zsh
install_vimplug
install_emacs

echo ">> Linking global files in ~/home..."
files=".emacs.d .oh-my-zsh"
for file in $files; do
	unlink ~/$file
	ln -s $DOTFILES_DIR/$file ~/
done

echo ">> Linking common files in ~/home..."
files=".doom.d .oh-my-zsh.d .bin .profile .bashrc .zshenv .zshrc .tmux.conf .tmux-macos.conf .tmux-linux.conf .clang-tidy .editorconfig .mbsyncrc .msmtprc .sbclrc .rustfmt.toml"
for file in $files; do
	unlink ~/$file
	ln -s $DOTFILES_DIR/Common/$file ~/
done

echo ">> Linking other files in ~/home..."
files=".clang-format .fzf.zsh"
for file in $files; do
	unlink ~/$file

	if [ $CURRENT_ENV = "HOME" ]; then
		ln -s $DOTFILES_DIR/Home/$file ~/
	elif [ $CURRENT_ENV = "WORK" ]; then
		ln -s $DOTFILES_DIR/Work/$file ~/
	fi
done

echo ">> Linking nix files in ~/.config..."
unlink ~/.config/nixpkgs
ln -s $DOTFILES_DIR/Nix ~/.config/nixpkgs

echo ">> Linking other files in ~/.config"
files="alacritty git"
for file in $files; do
	unlink ~/.config/$file

	if [ $CURRENT_ENV = "HOME" ]; then
		ln -s $DOTFILES_DIR/Home/$file ~/.config
	elif [ $CURRENT_ENV = "WORK" ]; then
		ln -s $DOTFILES_DIR/Work/$file ~/.config
	fi
done

echo ">> Done!"
echo "===================================================================================="
