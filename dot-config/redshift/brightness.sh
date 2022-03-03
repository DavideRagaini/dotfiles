!/bin/sh
# Set brightness via xbrightness when redshift status changes

# Set brightness values for each status.
# Range from 1 to 100 is valid
brightness_day=25
brightness_transition=20
brightness_night=15
# Set fps for smoooooth transition
fps=5000
# Adjust this grep to filter only the backlights you want to adjust
backlights=($(xbacklight -list | grep ddcci*))

set_brightness() {
	for backlight in "${backlights[@]}"
	do
		xbacklight -set $1 -fps $fps -ctrl $backlight &
	done
}

if [ "$1" = period-changed ]; then
	case $3 in
		night)
			set_brightness $brightness_night
			;;
		transition)
			set_brightness $brightness_transition
			;;
		daytime)
			set_brightness $brightness_day
			;;
	esac
fi
