import os
import subprocess
from libqtile import hook

class Autostart(object):
    @hook.subscribe.startup_once
    def autostart ():
        home = os.path.expanduser('~/.config/qtile/autostart.sh')
        subprocess.call([home])


    @hook.subscribe.startup_once
    def startup_once():
        subprocess.Popen(["nm-applet"])
