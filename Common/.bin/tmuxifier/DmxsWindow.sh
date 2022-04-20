#!/bin/bash

WINDOW_NAME=Dmxs
if tmux list-windows -F '#W' | grep -q "^$WINDOW_NAME\$"; then
    select_window $WINDOW_NAME
    select_pane 2
else
    # Set window root path. Default is `$session_root`.
    # Must be called before `new_window`.
    window_root "$HOME/Workspace/Work/Projects/dmxs"

    # Create new window. If no argument is given, window name will be based on
    # layout file name.
    new_window "$WINDOW_NAME"

    # Split window into panes.
    split_v 80
    run_cmd "docker run --rm --interactive --tty --volume $HOME/Workspace/Work/Projects/dmxs:/tmp/sm -u $(id -u "${USER}"):$(id -g "${USER}") sm:latest /bin/bash"
    run_cmd "cd /tmp/sm"

    # Set active pane.
    select_pane 2
fi
