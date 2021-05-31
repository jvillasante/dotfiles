#!/bin/bash

. $(dirname "$0")/common.sh

update_zig() {
	local ZIG_PATH="$HOME/Workspace/Software/zig/zig"
	rm -rf $ZIG_PATH/*

	local ZIG_URL="$1"
	local ZIG_FILE="${ZIG_URL##*/}"
	local ZIG_DIR=$(basename "$ZIG_FILE" .tar.xz)

	curl -o $ZIG_PATH/$ZIG_FILE $ZIG_URL
	tar xf $ZIG_PATH/$ZIG_FILE -C $ZIG_PATH
	rm -rf $ZIG_PATH/$ZIG_FILE
	mv $ZIG_PATH/$ZIG_DIR/* $ZIG_PATH
	rm -rf $ZIG_PATH/$ZIG_DIR
}

update_zls() {
	# Do not work on MacOS!
	# local ZLS_PATH="$HOME/Workspace/Software/zig/zls"
	# rm -rf $ZLS_PATH/*
	#
	# local ZLS_URL="$1"
	# curl -L $ZLS_URL | tar -xJ --strip-components=1 -C $ZLS_PATH

	local ZIG_PATH="$HOME/Workspace/Software/zig"
	rm -rf $ZIG_PATH/zls/

	cd $ZIG_PATH
	git clone --recurse-submodules https://github.com/zigtools/zls
	cd zls
	zig build -Drelease-safe
}

do_update_zig() {
	local CURRENT_OS=$(find_os)
	if [ $CURRENT_OS = "OSX" ]; then
		update_zig "https://ziglang.org/builds/zig-macos-x86_64-0.8.0-dev.1981+fbda9991f.tar.xz"
	elif [ $CURRENT_OS = "LINUX" ]; then
		update_zig "https://ziglang.org/builds/zig-linux-x86_64-0.8.0-dev.2713+57cf9f7ea.tar.xz"
	fi
}

do_update_zls() {
	local CURRENT_OS=$(find_os)
	if [ $CURRENT_OS = "OSX" ]; then
		update_zls "https://github.com/zigtools/zls/releases/download/0.1.0/x86_64-macos.tar.xz"
	elif [ $CURRENT_OS = "LINUX" ]; then
		update_zls "https://github.com/zigtools/zls/releases/download/0.1.0/x86_64-linux.tar.xz"
	fi
}

while true; do
	PS3="Choose an option: "
	options=("Update Zig" "Update Zls" "Quit")

	select opt in "${options[@]}"; do
		case $REPLY in
		1)
			do_update_zig
			break
			;;
		2)
			do_update_zls
			break
			;;
		3) break 2 ;;
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
