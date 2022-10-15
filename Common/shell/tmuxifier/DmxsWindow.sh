#!/usr/bin/env bash

WINDOW_NAME=Dmxs
if tmux list-windows -F '#W' | grep -q "^$WINDOW_NAME\$"; then
    select_window $WINDOW_NAME
    select_pane 2
else
    WINDOW_ROOT="$HOME/Workspace/Work/Projects/dmxs"

    # Set window root path. Default is `$session_root`.
    # Must be called before `new_window`.
    window_root "$WINDOW_ROOT"

    # Create new window. If no argument is given, window name will be based on
    # layout file name.
    new_window "$WINDOW_NAME"

    # Split window into panes.
    split_v 80
    run_cmd "docker run --rm --interactive --tty --volume $WINDOW_ROOT:/tmp/sm -u jenkins -w /tmp/sm --name dmxs sm:latest /bin/bash"

    # Set active pane.
    select_pane 2
fi
