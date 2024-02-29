class Colors(object):
    black = ["#282a36", "#282a36"]  # background (dark grey) [0]
    grey = ["#44475a", "#44475a"]  # light grey [1]
    lightgrey = ["#f8f8f2", "#f8f8f2"]  # forground (white) [2]
    blue = ["#6272a4", "#6272a4"]  # blue/grey) [3]
    cyan = ["#8be9fd", "#8be9fd"]  # cyan [4]
    green = ["#50fa7b", "#50fa7b"]  # green [5]
    orange = ["#ffb86c", "#ffb86c"]  # orange [6]
    pink = ["#ff79c6", "#ff79c6"]  # pink [7]
    purple = ["#bd93f9", "#bd93f9"]  # purple [8]
    red = ["#ff5555", "#ff5555"]  # red [9]
    yellow = ["#f1fa8c", "#f1fa8c"]  # yellow [10]
    background = "#282a36"  # colors[0]
    foreground = "#f8f8f2"  # colors[2]
    workspace = "#bd93f9"  # colors[8]
    foregroundTwo = "#44475a"  # colors[1]
    # colors = [
    #     ["#282a36", "#282a36"],  # background (dark grey) [0]
    #     ["#44475a", "#44475a"],  # light grey [1]
    #     ["#f8f8f2", "#f8f8f2"],  # forground (white) [2]
    #     ["#6272a4", "#6272a4"],  # blue/grey) [3]
    #     ["#8be9fd", "#8be9fd"],  # cyan [4]
    #     ["#50fa7b", "#50fa7b"],  # green [5]
    #     ["#ffb86c", "#ffb86c"],  # orange [6]
    #     ["#ff79c6", "#ff79c6"],  # pink [7]
    #     ["#bd93f9", "#bd93f9"],  # purple [8]
    #     ["#ff5555", "#ff5555"],  # red [9]
    #     ["#f1fa8c", "#f1fa8c"],  # yellow [10]
    # ]


class Fonts(object):
    base = "IosevkaTerm Nerd Font Propo"
    bold = "IosevkaTerm Nerd Font Propo Bold"


class Layout_Aesthetics(object):
    layout_theme = {
        "border_focus": Colors.pink,
        "border_normal": Colors.purple,
        "border_width": 1,
        "margin": 8,
    }

    from libqtile import layout
    from libqtile.layout.floating import Floating
    from libqtile.config import Match

    floating_layout = (
        Floating(
            border_focus=Colors.cyan,
            border_width=2,
            float_rules=[
                *layout.Floating.default_float_rules,
                Match(wm_class="ssh-askpass"),  # ssh-askpass
                Match(wm_class="fzfmenu"),
                # Match(wm_instance_class="mpvFloat"),
                # Match(title="Eagenda"),
                Match(title="branchdialog"),  # gitk
                Match(wm_class="pinentry-gtk-2"),  # GPG key password entry
                Match(wm_class="pinentry-qt"),  # GPG key password entry
                Match(title="pinentry"),  # GPG key password entry
                Match(func=lambda c: c.has_fixed_size()),
                Match(func=lambda c: c.has_fixed_ratio()),
                Match(wm_class="confirm"),
                Match(wm_class="dialog"),
                Match(wm_class="download"),
                Match(wm_class="error"),
                Match(wm_class="file_progress"),
                Match(wm_class="notification"),
                Match(wm_class="splash"),
                Match(wm_class="toolbar"),
                Match(wm_class="confirmreset"),
                Match(wm_class="makebranch"),
                Match(wm_class="maketag"),
                Match(title="branchdialog"),
                Match(title="Xephyr on :1.0 (ctrl+shift grabs mouse and keyboard)"),
                Match(title="Bitwarden"),
                Match(wm_class="nextcloud"),
                Match(wm_class="system-config-printer"),
            ],
        ),
    )


class Widget_Aesthetics(object):
    widget_defaults = dict(
        font=Fonts.bold,
        fontsize=13,
        padding=4,
        foreground=Colors.foreground,
        background=Colors.background,
    )


class Extension_Aesthetics(object):

    def dmenu_defaults():
        opts = [
            "IosevkaTerm Nerd Font Mono:style=bold:size=12",
            "#181321",  # normal backgroun
            "#6e5e89",  # normal foregroun
            "#4e4262",  # selected backgroun
            "#181321",  # selected foregroun
        ]
        j = 0

        from os import environ as env
        for i in ["DMENU_FN", "DMENU_NB", "DMENU_NF", "DMENU_SB", "DMENU_SF"]:
            if i in env:
                opts[j] = env[i]
            j = j + 1

        return opts

    dmenu_env = dmenu_defaults()
    extension_defaults = dict(
        font=dmenu_env[1],
        dmenu_ignorecase=True,
        # dmenu_prompt =            ">",
        background=dmenu_env[1],
        foreground=dmenu_env[2],
        selected_background=dmenu_env[3],
        selected_foreground=dmenu_env[4],
    )
