#!/bin/sh

# options to be displayed
option0="Lock"
option1="Logout"
option2="Suspend"
option3="Scheduled suspend (10min)"
option4="Scheduled suspend (20min)"
option5="Scheduled suspend (30min)"
option6="Reboot"
option7="Shutdown"

# options passed into variable
options="$option0\n$option1\n$option2\n$option3\n$option4\n$option5\n$option6\n$option7"

chosen="$(echo -e "$options" | rofi -lines 8 -dmenu -p "power")"
case $chosen in
    $option0)
        dbus-send --print-reply --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock
        ;;
    $option1)
        gnome-session-quit
        ;;
    $option2)
        systemctl suspend
        ;;
    $option3)
        sleep 600 && systemctl suspend
        ;;
    $option4)
        sleep 1200 && systemctl suspend
        ;;
    $option5)
        sleep 1800 && systemctl suspend
        ;;
    $option6)
        systemctl reboot
        ;;
    $option7)
        systemctl poweroff
        ;;
esac
