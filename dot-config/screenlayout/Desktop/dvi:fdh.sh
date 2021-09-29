#!/bin/sh

# export GDK_DPI_SCALE=1.5
xrandr --output DVI-D-0 --primary --mode 1920x1080 --output HDMI-0 --off
xrdb -load ${XDG_CONFIG_HOME:-$HOME/.config}/x11/xresources_dir/desktop
setbg

# out=$( pactl list short sinks | grep hdmi | awk '{print $1}')
# pacmd set-default-sink $out || notify-send "Failed setting default sink"
# pacmd list-sink-inputs | awk '/index:/{print $2}' | xargs -r -I{} pacmd move-sink-input {} $out || notify-send "Failed switching inputs"

# pkill -RTMIN+12 dwmblocks
