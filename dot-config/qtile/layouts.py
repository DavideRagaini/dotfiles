from libqtile import layout
from libqtile.config import InvertMatch, Match, MatchAll
from colors import dracula
from re import compile as regex


def layout_defaults():
    colorscheme = dracula()
    (
        colors,
        background,
        foreground,
        workspace,
        foregroundTwo,
    ) = colorscheme

    layout_defaults = dict(
        border_focus=colors[7],
        border_normal=colors[8],
        border_width=2,
        margin=8,
    )

    return [
        layout.Columns(**layout_defaults),
        layout.Plasma(**layout_defaults, border_width_single=0),
        layout.Max(
            border_focus=colors[7], border_normal=colors[8], border_width=0, margin=0
        ),
        layout.MonadTall(**layout_defaults, single_border_width=0, single_margin=0),
        layout.Bsp(
            border_focus=colors[7],
            border_normal=colors[8],
            border_width=2,
            ratio=1.6,
            grow_amount=1,
            lower_right=True,
            wrap_client=False,
            margin_on_single=None,
            border_on_single=False,
        ),
        layout.RatioTile(
            **layout_defaults,
            ratio=2,
            ratio_increment=0.1,
        ),
        layout.MonadThreeCol(**layout_defaults),
        layout.TreeTab(
            **layout_defaults,
            active_bg=colors[3],
            active_fg=colors[5],
            bg_color=background,
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
        # layout.ScreenSplit(**layout_defaults),
        # layout.VerticalTile(),
        # layout.Zoomy(columnwidth=500),
        layout.Floating(
            border_focus=colors[4],
            border_width=2,
            float_rules=[
                *layout.Floating.default_float_rules,
                # Match(func=lambda c: c.has_fixed_ratio()),
                MatchAll(Match(func=lambda c: c.has_fixed_ratio()), InvertMatch(Match(wm_class="mpv"))),
                Match(func=lambda c: c.has_fixed_size()),
                Match(title=regex("Bitwardenbranchdialog|fzfmenu|pinentry")),
                Match(wm_class=regex(r"confirm|confirmreset|dialog|download|error|file_progress|makebranch|maketag|nextcloud")),
                Match(wm_class=regex(r"notification|pinentry-gtk-2|pinentry-qt|splash|ssh-askpass|system-config-printer|toolbar")),
                Match(wm_instance_class=regex(r"Places")),
            ],
        ),
    ]
