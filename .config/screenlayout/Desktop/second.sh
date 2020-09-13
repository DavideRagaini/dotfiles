#!/bin/sh

xrandr --output DVI-D-0 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-0 --off

xrdb ~/.config/Xresources

# out=$( pactl list short sinks | grep hdmi | awk '{print $1}')
# pacmd set-default-sink $out || notify-send "Failed setting default sink"
# pacmd list-sink-inputs | awk '/index:/{print $2}' | xargs -r -I{} pacmd move-sink-input {} $out || notify-send "Failed switching inputs"

# pkill -RTMIN+12 dwmblocks

setbg
# xwallpaper --output DVI-D-0 --maximize ~/.config/Icons/wallpaper2.png
sleep 3 &&
kill -HUP "$(pidof -s dwm)"
