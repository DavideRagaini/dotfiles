from libqtile import layout
from libqtile.config import Match
from colors import dracula

def layout_defaults():
    colorscheme = dracula()
    (
        colors,
        backgroundColor,
        foregroundColor,
        workspaceColor,
        foregroundColorTwo,
    ) = colorscheme

    layout_defaults = dict(
        border_focus=colors[7],
        border_normal=colors[8],
        border_width=1,
        margin=8,
    )

    return [
        layout.Columns(**layout_defaults),
        layout.Max(
            border_focus=colors[7], border_normal=colors[8], border_width=0, margin=0
        ),
        layout.MonadTall(**layout_defaults, single_border_width=0, single_margin=0),
        layout.Bsp(**layout_defaults),
        layout.RatioTile(**layout_defaults),
        layout.MonadThreeCol(**layout_defaults),
        layout.TreeTab(
            **layout_defaults,
            active_bg=colors[3],
            active_fg=colors[5],
            bg_color=backgroundColor,
            fontsize=11,
            inactive_bg="151515",
            inactive_fg=colors[2],
            level_shift=8,
            padding_left=4,
            padding_x=4,
            padding_y=2,
            panel_width=250,
            place_right=False,
            previous_on_rm=True,
            section_fg=colors[2],
            section_fontsize=10,
            section_left=100,
            section_top=15,
            sections=["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"],
        ),
        layout.Tile(**layout_defaults),
        layout.Stack(**layout_defaults, num_stacks=2),
        layout.Matrix(**layout_defaults),
        layout.MonadWide(**layout_defaults),
        # layout.VerticalTile(),
        # layout.Zoomy(columnwidth=500),
        layout.Floating(
            border_focus=colors[4],
            border_width=2,
            float_rules=[
                *layout.Floating.default_float_rules,
                Match(wm_class="ssh-askpass"),  # ssh-askpass
                Match(title="fzfmenu"),
                Match(wm_instance_class="Places"),
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
    ]
