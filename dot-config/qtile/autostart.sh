#!/bin/sh

# sleep 1 && wlr-randr --output HDMI-A-1 --scale 1 --on --output DVI-D-1 --on --transform 90 --pos &
# sleep 1 && wlr-randr --output HDMI-A-1 --off --output DVI-D-1 --on &
# sleep 1 && wlr-randr --output HDMI-A-1 --on --output DVI-D-1 --off &
toggle-screens mono &

sed -i 's/size = 9/size = 18/' ~/.config/alacritty/alacritty.toml &
wl-paste --watch cliphist store &
dunst --conf "$HOME/.config/dunst-stow/dunstrc" &
# swayidle -w timeout 300 'wlopm --off "*"' &
# swayidle -w \
#     timeout 150 'notify-send \"Shutdown in 30 seconds\"' \
#     timeout 180 'systemctl suspend' &
wlsunset -t 3700 -T 6500 -S 07:30 -s 20:30 -d 1800 &
# foot --server &

# sleep 15 && firefox &
sleep 15 && emacsclient -c -e '(doom/quickload-session t)' &
# sleep 15 && $TERMINAL -e emacsclient -nw -e '(doom/quickload-session t)' &
sleep 30 && xrdb -load "${XDG_CONFIG_HOME:-$HOME/.config}/x11/Xresources" &
