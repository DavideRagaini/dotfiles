#!/bin/sh

xrandr --output HDMI-0 --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DVI-D-0 --mode 1920x1080 --scale 2 --pos 3840x540
xrdb -load ${XDG_CONFIG_HOME:-$HOME/.config}/x11/xresources_dir/desktop-highdpi
setbg
