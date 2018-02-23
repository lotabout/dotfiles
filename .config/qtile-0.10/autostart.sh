#!/bin/sh

# setup background
# fbsetbg /home/ice/wallpapers/Grey033.jpg &

# set wmname to LG3D to solve JAVA problem
which wmname &>/dev/null
if [ $? == 0 ]; then
    wmname "LG3D"
fi
fcitx &

# shadow socks
# echo "Starting proxy"
if which shadowsocks.sh &> /dev/null; then
    shadowsocks.sh &
fi

# xscreensaver
xscreensaver -no-splash &

if hash xscreensaver 2> /dev/null; then
    xscreensaver -no-splash &
elif hash gnome-screensaver 2> /dev/null; then
    gnome-screensaver &
fi

# change pointer to left mouse
xmodmap -e "pointer = 3 2 1" &

# touchpad pointer to left hand
if xinput list | grep Synaptics &> /dev/null; then
    xinput set-button-map "SynPS/2 Synaptics TouchPad" 3 2 1 &
fi

if hash dunst 2> /dev/null; then
    dunst &
fi

# echo "Starting auto-sync tools"
if hash dropbox 2> /dev/null; then
    sleep 60 && dropbox start &
fi

# allow all users to access X
xhost +
