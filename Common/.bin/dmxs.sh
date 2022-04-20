#!/bin/bash

tmuxifier load-window ~/.bin/tmuxifier/DmxsWindow.sh
tmux send-keys C-u "$1" Enter
