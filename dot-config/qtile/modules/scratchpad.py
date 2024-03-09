from libqtile.config import ScratchPad, DropDown, Key
from libqtile.command import lazy
from os import environ as env


class Scratchpad(object):
    def init_scratchpad(self):
        terminal = env["TERMINAL"]
        opacity = 1
        on_focus_lost_hide = False
        # warp_pointer = False

        return [
            ScratchPad(
                "SPD",
                dropdowns = [
                    DropDown(
                        "Tmux Dropdown",
                        terminal + " -e tmux new-session -A -s 'dropdown'",
                        width=0.9,
                        height=0.9,
                        x=0.04,
                        y=0.04,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "file manager",
                        terminal + " -e tmux new-session -A -s 'files'",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "btop",
                        terminal + " -e btop",
                        width=0.9,
                        height=0.9,
                        x=0.04,
                        y=0.04,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "htop",
                        terminal + " -e htop",
                        width=0.9,
                        height=0.9,
                        x=0.04,
                        y=0.04,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "news",
                        terminal
                        + " --class 'newsboat,newsboat' -T 'newsboat' -o 'font.size=13' -e newsboat",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "podcasts",
                        terminal + " --class 'podboat,podboat' -T 'podboat' -e podboat",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "music",
                        terminal
                        + " --class 'ncmpcpp,ncmpcpp' -T 'ncmpcpp'  -e ncmpcpp",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "spotify",
                        terminal
                        + " --class 'spotify-tui,spotify-tui' -T 'spotify-tui'  -e spt",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "mixer",
                        terminal + " -e pulsemixer",
                        width=0.6,
                        height=0.3,
                        x=0.2,
                        y=0.2,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "calculator",
                        terminal + " -e wcalc -P -1 -c -q --ints -C -p -r --remember",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "qtile_shell",
                        terminal + " -e qtile shell",
                        x=0.05,
                        y=0.05,
                        width=0.9,
                        height=0.6,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    DropDown(
                        "mails",
                        "ferdium",
                        width=0.9,
                        height=0.9,
                        x=0.04,
                        y=0.04,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide
                    ),

                    # DropDown(
                    #     "mpv",
                    #     "mpv --x11-name=fmpv --no-terminal --player-operation-mode=pseudo-gui",
                    #     width=0.1,
                    #     height=0.1,
                    #     x=0.9,
                    #     y=0.9,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide
                    # ),

                ]
            ),
        ]


class DropDown_Keys(object):
    ##### DROPDOWNS KEYBINDINGS #####

    def init_dropdown_keybindings(self):
        mod = "mod4"
        # alt = "mod1"
        shift = "shift"
        ctrl = "control"

        return [
            # Key([mod], "c", lazy.group["SPD"].dropdown_toggle("mpvfloat")),
            # Key([mod], "e", lazy.group["SPD"].dropdown_toggle("agenda")),
            Key([], "XF86Calculator", lazy.group["SPD"].dropdown_toggle("calculator"),),
            Key([], "XF86HomePage", lazy.group["SPD"].dropdown_toggle("btop")),
            Key([], "XF86Launch7", lazy.group["SPD"].dropdown_toggle("mixer")),
            Key([], "XF86Mail", lazy.group["SPD"].dropdown_toggle("mails")),
            Key([mod, "shift"], "g", lazy.window.togroup("SPD"), desc="Move Window to scratchpad",),
            Key([mod, shift], "Escape", lazy.group["SPD"].dropdown_toggle("btop")),
            Key([mod, shift], "m", lazy.group["SPD"].dropdown_toggle("spotify")),
            Key([mod, shift], "n", lazy.group["SPD"].dropdown_toggle("podcasts")),
            Key([mod, shift], "up", lazy.group["SPD"].dropdown_toggle("mixer")),
            Key([mod], "Escape", lazy.group["SPD"].dropdown_toggle("btop")),
            Key([mod, shift], "Escape", lazy.group["SPD"].dropdown_toggle("btop")),
            Key([mod], "Return", lazy.group["SPD"].dropdown_toggle("Tmux Dropdown"),),
            Key([mod], "XF86HomePage", lazy.group["SPD"].dropdown_toggle("htop")),
            Key([mod], "apostrophe", lazy.group["SPD"].dropdown_toggle("calculator"),),
            Key([mod], "m", lazy.group["SPD"].dropdown_toggle("music")),
            Key([mod], "n", lazy.group["SPD"].dropdown_toggle("news")),
            Key([mod], "r", lazy.group["SPD"].dropdown_toggle("file manager")),
            Key([mod], "y", lazy.group["SPD"].dropdown_toggle("qtile_shell")),

            Key([mod], "g", lazy.group["SPD"].toscreen(toggle=True), desc="Toggle scratchpad group",),

            # Key([mod], "w", lazy.group["SPD"].dropdown_toggle("float_mpv")),
        ]
