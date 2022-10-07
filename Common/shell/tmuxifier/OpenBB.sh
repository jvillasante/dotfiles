#!/usr/bin/env bash

WINDOW_NAME=OpenBB
if tmux list-windows -F '#W' | grep -q "^$WINDOW_NAME\$"; then
    select_window $WINDOW_NAME
else
    # Set window root path. Default is `$session_root`.
    # Must be called before `new_window`.
    window_root "$HOME/Workspace/Software/System/OpenBBTerminal"

    # Create new window. If no argument is given, window name will be based on
    # layout file name.
    new_window "$WINDOW_NAME"

    # Split window into panes.
    run_cmd "conda activate obb"
    run_cmd "python terminal.py"
fi
