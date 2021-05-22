#!/bin/bash

. $(dirname "$0")/common.sh

dotfiles_pull() {
	local DOTFILES_DIR=$(find_dotfiles)

	git -C ${DOTFILES_DIR} status
	check $?

	if ask "Do you want to pull?"; then
		git -C ${DOTFILES_DIR} pull
		check $?
	fi
}

dotfiles_sync() {
	local DOTFILES_DIR=$(find_dotfiles)
	if [ ! -f ${DOTFILES_DIR}/make.sh ]; then
		echo "make script not present, exiting..."
		exit 1
	fi

	${DOTFILES_DIR}/make.sh
	check $?
}

doom_sync() {
	if [ ! -f ${HOME}/.emacs.d/bin/doom ]; then
		echo "doom script not present, exiting..."
		exit 1
	fi

	dotfiles_sync
	${HOME}/.emacs.d/bin/doom sync
	check $?
}

doom_upgrade() {
	if [ ! -f ${HOME}/.emacs.d/bin/doom ]; then
		echo "doom script not present, exiting..."
		exit 1
	fi

	dotfiles_sync
	${HOME}/.emacs.d/bin/doom -y upgrade -f
	check $?
}

homemanager_switch() {
	if [ ! -f ${HOME}/.nix-profile/bin/home-manager ]; then
		echo "home-manager script not present, exiting..."
		exit 1
	fi

	dotfiles_sync
	home-manager switch
	check $?
}

while true; do
	PS3="Choose an option: "
	options=("Dotfiles Pull" "Dotfiles Sync" "Home-Manager Switch" "Doom Sync" "Doom Upgrade" "Quit")

	select opt in "${options[@]}"; do
		case $REPLY in
		1)
			dotfiles_pull
			break
			;;
		2)
			dotfiles_sync
			break
			;;
		3)
			homemanager_switch
			break
			;;
		4)
			doom_sync
			break
			;;
		5)
			doom_upgrade
			break
			;;
		6) break 2 ;;
		*) echo "Invalid option!" >&2 ;;
		esac
	done

	echo ""

	if ask "Are we done?"; then
		break
	else
		echo ""
	fi
done
