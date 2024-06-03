# ======================= Imports ============= {{{
from libqtile.config import Screen

from typing import List

from groups import Groups
from scratchpad import Scratchpad
from colors import dracula
from layouts import layout_defaults
from bindings import *
# import autostart

# }}}
# ======================= init ============= {{{
if __name__ in ["config", "__main__"]:
    obj_groups = Groups()

    obj_scratchpad = Scratchpad()
    # obj_dd_keys = DropDown_Keys()

    groups = obj_groups.init_groups()

groups = obj_groups.init_groups()
groups += obj_scratchpad.init_scratchpad()

layouts = layout_defaults()
mouse = mouse()
keys = bindings()
# keys += obj_dd_keys.init_dropdown_keybindings()
# }}}
# ======================= Colors ============= {{{
colorscheme = dracula()
(
    colors,
    backgroundColor,
    foregroundColor,
    workspaceColor,
    foregroundColorTwo,
) = colorscheme
# }}}
# ======================= Bar & Widgets ============= {{{
from bars import main_bar, vmon_bar_top, vmon_bar_bottom

widget_defaults = dict(
    # font="DaddyTimeMono Nerd Font Bold",
    font="JetBrainsMono Nerd Font ExtraBold",
    # font="VictorMono Nerd Font Bold",
    fontsize=10,
    padding=4,
    background=colors[0],
)

wallpaperPath = "/home/davide/.local/share/"
main_screen = Screen(
    bottom=main_bar,
    wallpaper=wallpaperPath + "mainbg",
    wallpaper_mode="fill",
)

vert_screen = Screen(
    top=vmon_bar_top,
    bottom=vmon_bar_bottom,
    wallpaper=wallpaperPath + "vertbg",
    wallpaper_mode="fill",
)

screens = [ main_screen, vert_screen ]
# }}}
# ======================= Misc ============= {{{
auto_fullscreen = True
auto_minimize = True
bring_front_click = "floating_only"
cursor_warp = False
dgroups_app_rules = []  # type: List
floats_kept_above = True
focus_on_window_activation = "smart"
follow_mouse_focus = True
reconfigure_screens = True
wmname = "LG3D"
# }}}
