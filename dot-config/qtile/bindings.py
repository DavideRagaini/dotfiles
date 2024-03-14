from libqtile.config import EzKey as Key


# ======================= dmenu ============= {{{
def dmenu_defs():
    opts = [
        "IosevkaTerm Nerd Font Mono:style=bold:size=12",
        "#181321",  # normal backgroun
        "#6e5e89",  # normal foregroun
        "#4e4262",  # selected backgroun
        "#181321",  # selected foregroun
    ]

    j = 0
    for i in ["DMENU_FN", "DMENU_NB", "DMENU_NF", "DMENU_SB", "DMENU_SF"]:
        if i in env:
            opts[j] = env[i]
        j = j + 1

    return opts


# }}}
# ======================= Keybindings ============= {{{
from libqtile.extension import WindowList, DmenuRun, CommandSet
from os import environ as env
from functions import *


def bindings():
    terminal = env["TERMINAL"]
    browser = env["BROWSER"]
    browser_A = env["BROWSER2"]
    browser_priv = env["BROWSER_PRIVATE"]
    # text_editor = terminal + " --class 'emacs,emacs' -T 'term-emacsclient' -e emacsclient -nw"
    text_editor = "emacsclient -c"
    launcher = "run"

    dmenu_options = dmenu_defs()
    dmenu_defaults = dict(
        dmenu_font=dmenu_options[0],
        dmenu_ignorecase=True,
        background=dmenu_options[1],
        foreground=dmenu_options[2],
        selected_background=dmenu_options[3],
        selected_foreground=dmenu_options[4],
    )

    return [
        # === Special Keys=== {{{
        # Key("M-S", "XF86Back", lazy.spawn("dmpc seekp")),
        # Key("M-S", "XF86Forward", lazy.spawn("dmpc seekf")),
        # Key([], "XF86HomePage", lazy.spawn("")),
        # Key([], "XF86Search", lazy.spawn("")),
        # Key([], "XF86Search", lazy.spawn("ferdium")),
        Key("<XF86Calculator>", lazy.group["SPD"].dropdown_toggle("calculator")),
        Key("<XF86HomePage>", lazy.group["SPD"].dropdown_toggle("btop")),
        Key("<XF86Launch7>", lazy.group["SPD"].dropdown_toggle("mixer")),
        Key("<XF86Mail>", lazy.group["SPD"].dropdown_toggle("mails")),
        Key("M-<XF86HomePage>", lazy.group["SPD"].dropdown_toggle("htop")),
        Key("M-C-<XF86Back>", lazy.spawn("dmpc prev")),
        Key("M-C-<XF86Forward>", lazy.spawn("dmpc next")),
        Key("M-S-<XF86AudioPlay>", lazy.spawn("tppctl invert")),
        Key("M-S-<XF86Back>", lazy.spawn("tppctl seek -10")),
        Key("M-S-<XF86Forward>", lazy.spawn("tppctl seek 10")),
        Key("M-<XF86AudioMute>", lazy.spawn("output-audio")),
        Key(
            "<XF86AudioMute>",
            lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),
        ),
        Key("M-<XF86AudioNext>", lazy.spawn("tppctl seek 10")),
        Key("M-<XF86AudioPlay>", lazy.spawn("tppctl toggle")),
        Key("M-<XF86AudioPrev>", lazy.spawn("tppctl seek -10")),
        Key("M-<XF86Calculator>", lazy.spawn("systemctl suspend")),
        Key("M-<XF86Favorites>", focus_previous_window()),
        Key("M-<XF86Launch8>", lazy.spawn("tppctl seek -10")),
        Key("M-<XF86Launch9>", lazy.spawn("tppctl seek 10")),
        Key("M-<XF86MonBrightnessDown>", lazy.spawn("xbacklight -dec 15")),
        Key("M-<XF86MonBrightnessUp>", lazy.spawn("xbacklight -inc 15")),
        Key(
            "<XF86AudioLowerVolume>",
            lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-"),
        ),
        Key("<XF86AudioNext>", lazy.spawn("dmpc next")),
        Key("<XF86AudioPlay>", lazy.spawn("dmpc toggle")),
        Key("<XF86AudioPrev>", lazy.spawn("dmpc prev")),
        Key(
            "<XF86AudioRaiseVolume>",
            lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"),
        ),
        Key("<XF86Explorer>", lazy.screen.toggle_group()),
        Key("<XF86Favorites>", lazy.screen.toggle_group()),
        Key("<XF86Launch5>", lazy.spawn("tppctl invert")),
        Key("<XF86MonBrightnessDown>", lazy.spawn("xbacklight -dec 5")),
        Key("<XF86MonBrightnessUp>", lazy.spawn("xbacklight -inc 5")),
        # }}}
        # === Function Keys=== {{{
        Key("M-<Escape>", lazy.group["SPD"].dropdown_toggle("btop")),
        Key("M-S-<Escape>", lazy.group["SPD"].dropdown_toggle("btop")),
        # Key("M-F1", lazy.spawn("")),
        # Key("M-F2", lazy.spawn("")),
        Key("M-<F3>", lazy.spawn("wifi-toggle")),
        # Key("M-F4", lazy.spawn("")),
        # Key("M-F5", lazy.spawn("")),
        # Key("M-F6", lazy.spawn("")),
        # Key("M-F7", lazy.spawn("")),
        Key("M-<F8>", lazy.spawn("dmenumount")),
        Key("M-S-<F8>", lazy.spawn("dmenuumount")),
        # Key("M-F9", lazy.spawn("")),
        # Key("M-F10", lazy.spawn("")),
        # Key("M-F11", lazy.spawn("")),
        Key("M-<F12>", lazy.spawn("maimpick")),
        Key("M-S-<F12>", lazy.spawn("maim ~/Storage/F$(date '+%y%m%d-%H%M-%S').png")),
        # }}}
        # === Number Keys=== {{{
        Key("M-<grave>", lazy.spawn("dunstctl close")),
        Key("M-S-<grave>", lazy.spawn("dunstctl history-pop")),
        Key("A-C-1", lazy.group.setlayout("max")),
        Key("A-C-2", lazy.group.setlayout("monadtall")),
        Key("A-C-3", lazy.group.setlayout("treetab")),
        Key("A-C-4", lazy.group.setlayout("bsp")),
        Key("A-C-5", lazy.group.setlayout("matrix")),
        Key("A-C-6", lazy.group.setlayout("ratiotile")),
        Key("A-C-7", lazy.group.setlayout("tile")),
        Key("A-C-8", lazy.group.setlayout("stack")),
        Key("A-C-9", lazy.group.setlayout("monadwide")),
        Key("A-C-0", lazy.group.setlayout("monadthreecol")),
        #
        # Key("M-<minus>", lazy.spawn("")),
        #
        Key("M-<equal>", lazy.spawn("dmenuunicode")),
        #
        Key("M-<backspace>", lazy.spawn("sysact Shutdown")),
        Key("M-S-<backspace>", lazy.spawn("sysact")),
        Key("M-C-<backspace>", lazy.spawn("sysact Suspend")),
        # }}}
        # === First Row === {{{
        # Key([mod], "c", lazy.group["SPD"].dropdown_toggle("mpvfloat")),
        Key("M-S-m", lazy.group["SPD"].dropdown_toggle("spotify")),
        Key("M-m", lazy.group["SPD"].dropdown_toggle("music")),
        Key("M-n", lazy.group["SPD"].dropdown_toggle("news")),
        Key("M-S-n", lazy.group["SPD"].dropdown_toggle("podcasts")),
        Key("A-<Tab>", focus_previous_window()),
        Key("M-<Tab>", lazy.screen.toggle_group()),
        Key("M-S-<Tab>", lazy.run_extension(WindowList(**dmenu_defaults))),
        #
        Key("M-q", lazy.window.kill()),
        Key("M-C-q", lazy.shutdown()),
        #
        Key("M-w", lazy.spawn(browser)),
        Key("M-C-w", lazy.spawn(browser_priv)),
        Key("M-S-w", lazy.spawn(browser_A)),
        # Key([mod], "w", lazy.group["SPD"].dropdown_toggle("float_mpv")),
        #
        # Key([mod], "e", lazy.group["SPD"].dropdown_toggle("agenda")),
        Key("M-e", lazy.spawn(text_editor)),
        Key("M-S-e", lazy.spawn(terminal + " -e emacsclient -c")),
        #
        Key("M-S-r", lazy.spawn("via -a")),
        Key("M-C-r", lazy.restart()),
        #
        Key("M-t", lazy.window.toggle_minimize()),
        Key("M-C-t", lazy.spawn("switch-theme")),
        Key("M-S-t", lazy.group.unminimize_all()),
        #
        Key("M-y", lazy.group["SPD"].dropdown_toggle("qtile_shell")),
        #
        Key("M-u", lazy.layout.normalize()),
        Key("M-C-u", move_mpv_to_current_group()),
        #
        Key("M-i", float_cycle(False)),
        Key("M-C-i", window_opacity("set", 0.1)),
        Key("M-S-i", lazy.prev_layout()),
        Key("M-S-C-i", window_opacity("dec")),
        #
        Key("M-o", float_cycle(True)),
        Key("M-C-o", window_opacity("set", 1)),
        Key("M-S-o", lazy.next_layout()),
        Key("M-S-C-o", window_opacity("inc")),
        #
        Key("M-p", lazy.spawn("dmpv append")),
        Key("M-C-p", lazy.spawn("dmpv")),
        Key("M-S-p", lazy.spawn("dmpv aplay ")),
        #
        Key("M-<bracketleft>", lazy.screen.prev_group(skip_empty=True)),
        Key("M-S-<bracketleft>", lazy.window.move_down()),
        Key("M-<bracketright>", lazy.screen.next_group(skip_empty=True)),
        #
        Key("M-S-<bracketright>", lazy.window.move_up()),
        Key("M-<backslash>", floating_corner_window("bottom right")),
        Key("M-S-<backslash>", floating_corner_window("top right")),
        Key("M-C-<backslash>", floating_corner_window("bottom left")),
        Key("M-S-C-<backslash>", floating_corner_window("top left")),
        # }}}
        # === Second Row === {{{
        Key("M-a", lazy.next_screen()),
        Key("M-S-a", lazy.function(window_to_next_screen, switch_screen=True)),
        # Key("M-A-a", add_treetab_section),
        #
        # Key("M-s", toggle_sticky_windows()),
        #
        Key("M-d", lazy.run_extension(DmenuRun(**dmenu_defaults))),
        Key("M-S-d", lazy.spawn("via -r")),
        Key("M-C-d", lazy.spawncmd(launcher)),
        #
        Key("M-f", lazy.window.toggle_maximize(), lazy.window.keep_above()),
        Key("M-S-f", lazy.window.toggle_fullscreen()),
        Key("M-C-f", float_to_front()),
        # Key([M, A], "f", focus_floating_window()),
        Key("M-g", lazy.group["SPD"].toscreen(toggle=True)),
        Key("M-S-g", lazy.window.togroup("SPD")),
        #
        Key("M-h", lazy.layout.left()),
        Key("M-l", lazy.layout.right()),
        Key("M-j", lazy.layout.down()),
        Key("M-k", lazy.layout.up()),
        #
        Key("M-S-h", lazy.layout.shuffle_left()),
        Key("M-S-l", lazy.layout.shuffle_right()),
        Key("M-S-j", lazy.layout.shuffle_down()),
        Key("M-S-k", lazy.layout.shuffle_up()),
        Key("M-<semicolon>", focus_previous_window()),
        Key("M-S-<semicolon>", lazy.group.focus_back()),
        #
        Key(
            "M-C-l",
            lazy.layout.grow_right(),
            lazy.layout.grow(),
            lazy.layout.increase_ratio(),
            lazy.layout.delete(),
        ),
        Key(
            "M-C-h",
            lazy.layout.grow_left(),
            lazy.layout.shrink(),
            lazy.layout.decrease_ratio(),
            lazy.layout.add(),
        ),
        Key(
            "M-C-k",
            lazy.layout.grow_up(),
            lazy.layout.grow(),
            lazy.layout.decrease_nmaster(),
        ),
        Key(
            "M-C-j",
            lazy.layout.grow_down(),
            lazy.layout.shrink(),
            lazy.layout.increase_nmaster(),
        ),
        Key(
            "M-A-h",
            lazy.layout.shuffle_left(),
            lazy.layout.move_left().when(layout=["treetab"]),
        ),
        Key(
            "M-A-l",
            lazy.layout.shuffle_right(),
            lazy.layout.move_right().when(layout=["treetab"]),
        ),
        Key(
            "M-A-j",
            lazy.layout.shuffle_down(),
            lazy.layout.section_down().when(layout=["treetab"]),
        ),
        Key(
            "M-A-k",
            lazy.layout.shuffle_up(),
            lazy.layout.section_up().when(layout=["treetab"]),
        ),
        #
        Key("A-S-h", lazy.spawn("xdotool mousemove_relative -- -15 0")),
        Key("A-S-l", lazy.spawn("xdotool mousemove_relative -- 15 0")),
        Key("A-S-j", lazy.spawn("xdotool mousemove_relative -- 0 15")),
        Key("A-S-k", lazy.spawn("xdotool mousemove_relative -- 0 -15")),
        #
        Key("A-C-h", lazy.spawn("xdotool mousemove_relative -- -50 0")),
        Key("A-C-l", lazy.spawn("xdotool mousemove_relative -- 50 0")),
        Key("A-C-j", lazy.spawn("xdotool mousemove_relative -- 0 50")),
        Key("A-C-k", lazy.spawn("xdotool mousemove_relative -- 0 -50")),
        #
        Key("M-<apostrophe>", lazy.group["SPD"].dropdown_toggle("calculator")),
        #
        Key("M-<Return>", lazy.group["SPD"].dropdown_toggle("Tmux Dropdown")),
        Key("A-<Return>", lazy.spawn("xdotool click 1")),
        Key("M-S-<Return>", lazy.spawn(terminal + " msg create-window -e tmux")),
        # }}}
        # === Third Row === {{{
        Key("M-z", lazy.window.move_to_top()),
        Key("M-S-z", lazy.window.move_to_bottom()),
        #
        Key("M-x", lazy.spawn("alm -d")),
        #
        Key("M-c", lazy.screen.togglegroup()),
        #
        Key("M-v", restore_all_merged_groups()),
        #
        Key("M-b", lazy.hide_show_bar("top")),
        Key("M-S-b", lazy.spawn("bm S")),
        Key("M-C-b", lazy.spawn("bm d")),
        #
        Key(
            "M-C-m",
            lazy.run_extension(
                CommandSet(
                    commands={
                        "play/pause": "mpc toggle",
                        "next": "mpc next",
                        "previous": "mpc prev",
                        "quit": "mpc stop",
                        "open": terminal + " -e ncmpcpp",
                        "shuffle": "mpc shuffle",
                        "repeat": "mpc repeat",
                        "info": "mpc-notify",
                        "volume set": CommandSet(
                            commands={
                                "5": "mpc volume 5",
                                "10": "mpc volume 10",
                                "15": "mpc volume 15",
                                "20": "mpc volume 20",
                                "25": "mpc volume 25",
                                "30": "mpc volume 30",
                                "35": "mpc volume 35",
                                "40": "mpc volume 40",
                                "45": "mpc volume 45",
                                "50": "mpc volume 50",
                                "55": "mpc volume 55",
                                "60": "mpc volume 60",
                                "65": "mpc volume 65",
                                "70": "mpc volume 75",
                                "75": "mpc volume 75",
                                "80": "mpc volume 85",
                                "85": "mpc volume 85",
                                "90": "mpc volume 90",
                                "95": "mpc volume 95",
                                "100": "mpc volume 100",
                            },
                            **dmenu_defaults,
                        ),
                    },
                    pre_commands=["mpc"],
                    **dmenu_defaults,
                )
            ),
        ),
        #
        Key("M-C-<period>", lazy.spawn("tppctl pauseall")),
        Key("A-<comma>", lazy.prev_screen()),
        Key("M-<comma>", lazy.spawn("dmpc toggle")),
        #
        Key("A-<period>", lazy.next_screen()),
        Key("M-<period>", lazy.spawn("tppctl invert")),
        Key("M-S-<period>", lazy.spawn("tppctl toggle")),
        #
        # Key("M-S-<slash>", lazy.group["SPD"].dropdown_toggle("mixer")),
        # }}}
        # === Middle Keys === {{{
        # Toggle between split and unsplit sides of stack.
        # Split = all windows displayed
        # Unsplit = 1 window displayed, like Max layout, but still with multiple stack panes
        # Key("M-C", "<space>", lazy.layout.toggle_split()),
        Key("M-<space>", toggle_focus_main()),
        Key("M-C-<space>", lazy.layout.swap_main()),
        Key("M-S-<space>", lazy.window.toggle_floating()),
        Key("M-A-<space>", lazy.layout.flip()),
        #
        Key("M-<Insert>", lazy.spawn("clipmenu -i")),
        Key("M-S-<Insert>", lazy.spawn("clipboard-content.sh")),
        #
        Key("<cancel>", lazy.spawn("tppctl invert")),
        Key("M-<cancel>", lazy.spawn("tppctl pauseall")),
        Key("M-S-<cancel>", lazy.spawn("dmpc toggle")),
        #
        Key("M-<up>", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+")),
        Key("M-S-<up>", lazy.spawn("mpc volume +5")),
        Key("M-C-<up>", lazy.spawn("output-audio")),
        #
        Key("M-<down>", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-")),
        Key("M-S-<down>", lazy.spawn("mpc volume -5")),
        Key("M-C-<down>", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")),
        #
        Key("M-<left>", lazy.spawn("tppctl seek -10")),
        #
        Key("M-<right>", lazy.spawn("tppctl seek 10")),
        # }}}
    ]


# }}}
# ======================= Mouse ============= {{{
from libqtile.config import Click, Drag


def mouse(mod="mod4"):
    return [
        Drag(
            [mod],
            "Button1",
            lazy.window.set_position_floating(),
            start=lazy.window.get_position(),
        ),
        Drag(
            [mod],
            "Button3",
            lazy.window.set_size_floating(),
            start=lazy.window.get_size(),
        ),
        Click([mod], "Button2", lazy.window.bring_to_front()),
    ]


# }}}
# ======================= Groups ============= {{{
# from libqtile.config import Key


def dr_key_binder(keynames=None):
    """Bind keys to mod+group position or to the keys specified as second argument"""

    def func(dgroup):
        # unbind all
        for key in dgroup.keys[:]:
            dgroup.qtile.ungrab_key(key)
            dgroup.qtile.config.keys.remove(key)
            dgroup.keys.remove(key)

        if keynames:
            keys = keynames
        else:
            # keys 1 to 9 and 0
            keys = list(map(str, list(range(1, 10)) + [0]))

        # bind all keys
        for keyname, group in zip(keys, dgroup.qtile.groups):
            name = group.name
            key = Key("M-" + str(keyname), lazy.group[name].toscreen())
            key_s = Key("M-S-" + str(keyname), lazy.window.togroup(name))
            key_c = Key("M-C-" + str(keyname), merge_groups(int(name)))
            dgroup.keys.extend([key, key_s, key_c])
            dgroup.qtile.config.keys.extend([key, key_s, key_c])
            dgroup.qtile.grab_key(key)
            dgroup.qtile.grab_key(key_s)
            dgroup.qtile.grab_key(key_c)

    return func


dgroups_key_binder = dr_key_binder()
# }}}
