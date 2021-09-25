#!/bin/sh

# export GDK_DPI_SCALE=1.5
xrandr --output HDMI-0 --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DVI-D-0 --off
xrdb -load ${XDG_CONFIG_HOME:-$HOME/.config}/X11/xresources_dir/desktop-highdpi
setbg

# out=$( pactl list short sinks | grep 'analog-stereo' | awk '{print $1}')
# pacmd set-default-sink $out || notify-send "Failed setting default sink"
# pacmd list-sink-inputs | awk '/index:/{print $2}' | xargs -r -I{} pacmd move-sink-input {} $out || notify-send "Failed switching inputs"

# pkill -RTMIN+12 dwmblocks
