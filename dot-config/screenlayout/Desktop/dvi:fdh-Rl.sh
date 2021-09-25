#!/bin/sh

xrandr --output DVI-D-0 --primary --mode 1920x1080 --pos 0x0 --rotate left --output HDMI-0 --off
xrdb -load ${XDG_CONFIG_HOME:-$HOME/.config}/X11/xresources_dir/desktop
setbg
