#!/bin/env bash
# require xdotool and dmenu


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

window_name=$(xdotool search --onlyvisible --name '.+' \
    | xargs -I{} xdotool getwindowname '{}' \
    | dmenu -i -l 10 -fn 'WenQuanYi Micro Hei' -x $x -y $y -w $dialog_width \
    | sed 's/[]\[\(\).\\]/\\&/g')
xdotool search "$window_name" windowactivate
