from libqtile.config import ScratchPad, DropDown

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
                dropdowns=[
                    DropDown(
                        "terminal",
                        terminal + " -e tmux new-session -A -s 'dropdown'",
                        width=0.9,
                        height=0.9,
                        x=0.05,
                        y=0.05,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "file manager",
                        terminal + " -e tmux new-session -A -s 'files'",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "btop",
                        terminal + " -e btop",
                        width=0.9,
                        height=0.9,
                        x=0.05,
                        y=0.05,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "htop",
                        terminal + " -e htop",
                        width=0.9,
                        height=0.9,
                        x=0.05,
                        y=0.05,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "newmacs",
                        "emacs --eval='(dr/start-elfeed)'",
                        width=0.9,
                        height=0.9,
                        x=0.05,
                        y=0.05,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "flemacs",
                        "emacsclient -c -F '((name . \"flemacs\")(width . 200)(height . 55))'",
                        width=0.9,
                        height=0.9,
                        x=0.05,
                        y=0.05,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    # DropDown(
                    #     "news",
                    #     terminal
                    # + " --class 'newsboat,newsboat' -T 'newsboat' -e newsboat",
                    #     width=0.8,
                    #     height=0.8,
                    #     x=0.1,
                    #     y=0.1,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide,
                    # ),
                    # DropDown(
                    #     "podcasts",
                    #     terminal
                    # + " --class 'podboat,podboat' -T 'podboat' -e podboat",
                    #     width=0.8,
                    #     height=0.8,
                    #     x=0.1,
                    #     y=0.1,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide,
                    # ),
                    DropDown(
                        "music",
                        terminal + " --class 'music' -T 'music'  -e mp",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    # DropDown(
                    #     "music",
                    #     terminal
                    #     + " --class 'ncmpcpp,ncmpcpp' -T 'ncmpcpp'  -e ncmpcpp",
                    #     width=0.8,
                    #     height=0.8,
                    #     x=0.1,
                    #     y=0.1,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide,
                    # ),
                    # DropDown(
                    #     "spotify",
                    #     terminal
                    #     + " --class 'spotify,spotify' -T 'spotify'  -e spotify_player",
                    #     width=0.8,
                    #     height=0.8,
                    #     x=0.1,
                    #     y=0.1,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide,
                    # ),
                    DropDown(
                        "mixer",
                        terminal + " -e pulsemixer",
                        width=0.6,
                        height=0.4,
                        x=0.2,
                        y=0.2,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "calculator",
                        terminal + " -e wcalc -P -1 -c -q --ints -C -p -r --remember",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "qtile_shell",
                        terminal + " -e qtile shell",
                        x=0.05,
                        y=0.05,
                        width=0.9,
                        height=0.6,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "mails",
                        "ferdium",
                        width=0.9,
                        height=0.9,
                        x=0.05,
                        y=0.05,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
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
                ],
            ),
        ]


# from libqtile.config import EzKey as Key
# from libqtile.command import lazy


# class DropDown_Keys(object):
#     def init_dropdown_keybindings(self):
#         return [
#             # Key("M-c", lazy.group["SPD"].dropdown_toggle("mpvfloat")),
#             # Key("M-e", lazy.group["SPD"].dropdown_toggle("agenda")),
#             Key("<XF86Calculator>", lazy.group["SPD"].dropdown_toggle("calculator")),
#             Key("<XF86HomePage>", lazy.group["SPD"].dropdown_toggle("btop")),
#             Key("<XF86Launch7>", lazy.group["SPD"].dropdown_toggle("mixer")),
#             Key("<XF86Mail>", lazy.group["SPD"].dropdown_toggle("mails")),
#             Key("M-S-g", lazy.window.togroup("SPD")),
#             Key("M-S-<Escape>", lazy.group["SPD"].dropdown_toggle("btop")),
#             Key("M-S-m", lazy.group["SPD"].dropdown_toggle("spotify")),
#             Key("M-S-n", lazy.group["SPD"].dropdown_toggle("podcasts")),
#             # Key("M-S-slash", lazy.group["SPD"].dropdown_toggle("mixer")),
#             Key("M-<Escape>", lazy.group["SPD"].dropdown_toggle("btop")),
#             Key("M-S-<Escape>", lazy.group["SPD"].dropdown_toggle("btop")),
#             Key("M-<Return>", lazy.group["SPD"].dropdown_toggle("Tmux Dropdown")),
#             Key("M-<XF86HomePage>", lazy.group["SPD"].dropdown_toggle("htop")),
#             Key("M-<apostrophe>", lazy.group["SPD"].dropdown_toggle("calculator")),
#             Key("M-m", lazy.group["SPD"].dropdown_toggle("music")),
#             Key("M-n", lazy.group["SPD"].dropdown_toggle("news")),
#             Key("M-r", lazy.group["SPD"].dropdown_toggle("file manager")),
#             Key("M-y", lazy.group["SPD"].dropdown_toggle("qtile_shell")),
#             Key("M-g", lazy.group["SPD"].toscreen(toggle=True)),
#             # Key("M-w", lazy.group["SPD"].dropdown_toggle("float_mpv")),
#         ]
