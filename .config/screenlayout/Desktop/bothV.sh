#!/bin/sh
xrandr --output HDMI-0 --primary --mode 3840x2160 --pos 1080x0 --rotate normal --output DVI-D-0 --mode 1920x1080 --pos 0x120 --rotate left

xwallpaper --output DVI-D-0 --center ~/.config/Icons/Vertical-Wallpaper.jpg --output HDMI-0 --maximize ~/.config/Icons/wallpaper2.png
