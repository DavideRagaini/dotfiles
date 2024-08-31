import os
import subprocess
from libqtile import hook


@hook.subscribe.startup_once
def autostart():
    main = os.path.expanduser("~/.config/qtile/autostart.sh")
    subprocess.Popen([main])


# @hook.subscribe.startup
# @hook.subscribe.startup_complete
