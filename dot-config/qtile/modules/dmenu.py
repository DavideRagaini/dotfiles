class Dmenu(object):
        def dmenu_defaults():
            from os import environ as env
            opts = [
                "IosevkaTerm Nerd Font Mono:style=bold:size=12",
                "#181321", # normal backgroun
                "#6e5e89", # normal foregroun
                "#4e4262", # selected backgroun
                "#181321", # selected foregroun
            ]

            j = 0
            for i in [ "DMENU_FN", "DMENU_NB", "DMENU_NF", "DMENU_SB", "DMENU_SF" ]:
                if i in env:
                    opts[j] = env[i]
                j = j + 1

            dmenu_defaults = dict(
                dmenu_font = opts[0],
                dmenu_ignorecase = True,
                background = opts[1],
                foreground = opts[2],
                selected_background = opts[3],
                selected_foreground = opts[4],
            )

            return dmenu_defaults
