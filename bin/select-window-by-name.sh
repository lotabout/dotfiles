#!/bin/env bash
# require wmctrl and dmenu

set -x


OFFSET_RE="([0-9]+)x([0-9]+)"
screen_size=($(xdpyinfo | sed -nr "s/^.*dimensions: *$OFFSET_RE.*$/\1 \2/p"))

screen_width=${screen_size[0]}
screen_height=${screen_size[1]}
dialog_width=800
dialog_height=200

x=$((($screen_width - $dialog_width) / 2))
y=$((($screen_height - $dialog_height) / 2))

# Note that xdotool will only search for regular expression
# So we need to escape the names we got from getwindowname

window_list=($(wmctrl -l))
window_name=$(wmctrl -l \
    | cut -d ' ' -f 5- \
    | nl -w 2 -s '  ' \
    | dmenu -i -l 10 -fn 'WenQuanYi Micro Hei' -x $x -y $y -w $dialog_width \
    | cut -c 5-)
wmctrl -a "$window_name"
