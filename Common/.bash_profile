#!/usr/bin/env bash

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 022

# if running bash include `.bashrc` if it exists
# shellcheck source=/dev/null
[ -f "$HOME/.bashrc" ] && source "$HOME/.bashrc"
