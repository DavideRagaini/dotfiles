#!/bin/sh

xrandr --output DVI-D-0 --primary --mode 1920x1080 --scale 2 --pos 0x420 --rotate normal --output HDMI-0 --mode 3840x2160 --pos 1920x0 --rotate left
xrdb -load ${XDG_CONFIG_HOME:-$HOME/.config}/X11/xresources_dir/desktop-highdpi
setbg
