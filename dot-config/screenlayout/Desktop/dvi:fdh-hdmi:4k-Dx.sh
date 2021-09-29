#!/bin/sh

xrandr --output DVI-D-0 --mode 1920x1080 --scale 2 --pos 0x540 --rotate normal --output HDMI-0 --primary --mode 3840x2160 --pos 1920x0 --rotate normal
xrdb -load ${XDG_CONFIG_HOME:-$HOME/.config}/x11/xresources_dir/desktop-highdpi
setbg
