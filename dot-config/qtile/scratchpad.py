from libqtile.config import ScratchPad, DropDown

from os import environ as env


class Scratchpad(object):
    def init_scratchpad(self):
        terminal = env["TERMINAL"]
        opacity = 1
        on_focus_lost_hide = False
        big_font = " -f 'IosevkaTerm Nerd Font Propo:weight=bold:size=18'"
        # warp_pointer = False

        return [
            ScratchPad(
                "SPD",
                # position=10,
                single=True,
                dropdowns=[
                    DropDown(
                        "terminal",
                        terminal + " -e tmux new-session -A -s 'dropdown'",
                        width=0.95,
                        height=0.95,
                        x=0.025,
                        y=0.025,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "file manager",
                        terminal + " lf",
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
                    # DropDown(
                    #     "clipmenu",
                    #     terminal + big_font + " -e cliphist-fzf",
                    #     width=0.7,
                    #     height=0.5,
                    #     x=0.15,
                    #     y=0.10,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide,
                    # ),
                    DropDown(
                        "newmacs",
                        # "emacs --eval='(dr/start-elfeed)'",
                        terminal + " -e emacsclient -nw --eval '(dr/start-elfeed)'",
                        width=0.9,
                        height=0.9,
                        x=0.05,
                        y=0.05,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    # DropDown(
                    #     "flemacs",
                    #     "emacsclient -c -F '((name . \"flemacs\")(width . 200)(height . 55))'",
                    #     width=0.9,
                    #     height=0.9,
                    #     x=0.05,
                    #     y=0.05,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide,
                    # ),
                    # DropDown(
                    #     "news",
                    #     terminal
                    # + " -T 'newsboat' -e newsboat",
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
                    # + " -T 'podboat' -e podboat",
                    #     width=0.8,
                    #     height=0.8,
                    #     x=0.1,
                    #     y=0.1,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide,
                    # ),
                    DropDown(
                        "music",
                        terminal + " -T 'music' -e mp",
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
                    #     + " -T 'ncmpcpp'  -e ncmpcpp",
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
                    #     + " -T 'spotify'  -e spotify_player",
                    #     width=0.8,
                    #     height=0.8,
                    #     x=0.1,
                    #     y=0.1,
                    #     opacity=opacity,
                    #     on_focus_lost_hide=on_focus_lost_hide,
                    # ),
                    DropDown(
                        "mixer",
                        terminal + " -T 'mixer' -e pulsemixer",
                        width=0.6,
                        height=0.4,
                        x=0.2,
                        y=0.2,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "calculator",
                        terminal
                        + big_font
                        + " -T 'wcalc' -e wcalc -P -1 -c -q --ints -C -p -r --remember",
                        width=0.8,
                        height=0.8,
                        x=0.1,
                        y=0.1,
                        opacity=opacity,
                        on_focus_lost_hide=on_focus_lost_hide,
                    ),
                    DropDown(
                        "qtile_shell",
                        terminal + " -T 'qtile_shell' -e qtile shell",
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
