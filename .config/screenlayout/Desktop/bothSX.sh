#!/bin/sh
xrandr --output DVI-D-0 --mode 1920x1080 --pos 3840x494 --rotate normal --output HDMI-0 --primary --mode 3840x2160 --pos 0x0 --rotate normal

xwallpaper --output DVI-D-0 --maximize ~/.config/Icons/wallpaper1.jpg --output HDMI-0 --center ~/.config/Icons/wallpaper2.png
