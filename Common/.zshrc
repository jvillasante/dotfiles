#!/usr/bin/env bash

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 022

# Source our custom files
source "${HOME}/.config/shell/system/custom"
