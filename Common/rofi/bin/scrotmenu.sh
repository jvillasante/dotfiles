#!/bin/sh

# options to be displayed
option0="Screen"
option1="Area"
option2="Window"

# options to be displyed
options="$option0\n$option1\n$option2"

selected="$(echo -e "$options" | rofi -lines 3 -dmenu -p "scrot")"
case $selected in
    $option0)
        scrot -d 1 -F ~/Pictures/scrots/'%F-%H%M%S_$wx$h_screen.png'
        ;;
    $option1)
        scrot -s -f -F ~/Pictures/scrots/'%F-%H%M%S_$wx$h_area.png'
        ;;
    $option2)
        scrot -d 1 -u -F ~/Pictures/scrots/'%F-%H%M%S_$wx$h_window.png'
        ;;
esac
