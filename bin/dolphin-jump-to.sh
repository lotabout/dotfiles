#!/bin/bash

# Requires xdotool, rofi, fasd

active_window="$(xdotool getactivewindow getwindowname)"

if [[ ! "$active_window" =~ .*(Dolphin|Krusader)$ ]]; then
    echo "Current window is not Dolphin, exit."
    exit 1
fi

target="$(fasd -Rdl "$1" | rofi -dmenu -p 'Jump To:')"

if [[ ! -d $target ]]; then
    exit 1
fi

xdotool key "Escape" "ctrl+l"
xdotool type "$target"
xdotool key "Return"
