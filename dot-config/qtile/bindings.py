from libqtile.config import EzKey, KeyChord


# ======================= Variables ============= {{{
M = "mod4"
A = "mod1"
S = "shift"
C = "control"


# }}}
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
from libqtile.extension import WindowList, DmenuRun
from os import environ as env
from functions import *


def bindings():
    terminal = env["TERMINAL"]
    browser = env["BROWSER"]
    browserA = env["BROWSER2"]
    browserP = env["BROWSER_PRIVATE"]
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
        # ======================= Special Keys ======================= {{{
        EzKey(
            "<XF86AudioLowerVolume>",
            lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%-"),
        ),
        EzKey("A-<XF86AudioLowerVolume>", lazy.spawn("mpc volume -2")),
        EzKey("C-<XF86AudioLowerVolume>", lazy.spawn("tppctl volume -2")),
        #
        EzKey(
            "<XF86AudioRaiseVolume>",
            lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%+"),
        ),
        EzKey("A-<XF86AudioRaiseVolume>", lazy.spawn("mpc volume +2")),
        EzKey("C-<XF86AudioRaiseVolume>", lazy.spawn("tppctl volume +2")),
        #
        EzKey(
            "<XF86AudioMute>", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
        ),
        EzKey("M-<XF86AudioMute>", lazy.spawn("output-audio")),
        EzKey("C-<XF86AudioMute>", lazy.group["SPD"].dropdown_toggle("mixer")),
        #
        EzKey("<XF86AudioNext>", lazy.spawn("dmpc next")),
        EzKey("M-<XF86AudioNext>", lazy.spawn("tppctl seek 10")),
        #
        EzKey("<XF86AudioPrev>", lazy.spawn("dmpc prev")),
        EzKey("M-<XF86AudioPrev>", lazy.spawn("tppctl seek -10")),
        #
        EzKey("<XF86AudioPlay>", lazy.spawn("dmpc toggle")),
        EzKey("C-<XF86AudioPlay>", lazy.spawn("tppctl pauseall")),
        EzKey("M-<XF86AudioPlay>", lazy.spawn("tppctl invert")),
        EzKey("S-<XF86AudioPlay>", lazy.spawn("tppctl toggle")),
        #
        EzKey("<XF86Calculator>", lazy.group["SPD"].dropdown_toggle("calculator")),
        EzKey("M-<XF86Calculator>", lazy.spawn("systemctl suspend")),
        #
        EzKey("<XF86HomePage>", lazy.group["SPD"].dropdown_toggle("btop")),
        EzKey("M-<XF86HomePage>", lazy.group["SPD"].dropdown_toggle("htop")),
        #
        EzKey("<XF86Favorites>", lazy.screen.toggle_group()),
        EzKey("M-<XF86Favorites>", focus_previous_window()),
        #
        EzKey("<XF86Explorer>", lazy.screen.toggle_group()),
        #
        EzKey("<XF86Mail>", lazy.group["SPD"].dropdown_toggle("mails")),
        #
        # EzKey("<XF86Launch5>", lazy.spawn("")),
        # EzKey("<XF86Launch6>", lazy.spawn("")),
        EzKey("<XF86Launch7>", lazy.group["SPD"].dropdown_toggle("mixer")),
        EzKey("M-<XF86Launch8>", lazy.spawn("tppctl seek -10")),
        EzKey("M-<XF86Launch9>", lazy.spawn("tppctl seek 10")),
        #
        EzKey("<XF86MonBrightnessDown>", lazy.spawn("xbacklight -dec 5")),
        EzKey("M-<XF86MonBrightnessDown>", lazy.spawn("xbacklight -dec 15")),
        #
        EzKey("<XF86MonBrightnessUp>", lazy.spawn("xbacklight -inc 5")),
        EzKey("M-<XF86MonBrightnessUp>", lazy.spawn("xbacklight -inc 15")),
        #
        EzKey("M-C-<XF86Back>", lazy.spawn("dmpc prev")),
        EzKey("M-S-<XF86Back>", lazy.spawn("tppctl seek -10")),
        # EzKey("M-S", "XF86Back", lazy.spawn("dmpc seekp")),
        #
        EzKey("M-C-<XF86Forward>", lazy.spawn("dmpc next")),
        EzKey("M-S-<XF86Forward>", lazy.spawn("tppctl seek 10")),
        # EzKey("M-S", "XF86Forward", lazy.spawn("dmpc seekf")),
        # }}}
        # ======================= Function Keys ======================= {{{
        EzKey("M-<Escape>", lazy.group["SPD"].dropdown_toggle("btop")),
        EzKey("M-C-<Escape>", lazy.group["SPD"].dropdown_toggle("htop")),
        # EzKey("M-F1", lazy.spawn("")),
        # EzKey("M-F2", lazy.spawn("")),
        EzKey("M-<F3>", lazy.spawn("wifi-toggle")),
        # EzKey("M-F4", lazy.spawn("")),
        # EzKey("M-F5", lazy.spawn("")),
        # EzKey("M-F6", lazy.spawn("")),
        # EzKey("M-F7", lazy.spawn("")),
        EzKey("M-<F8>", lazy.spawn("dmenumount")),
        EzKey("M-S-<F8>", lazy.spawn("dmenuumount")),
        # EzKey("M-F9", lazy.spawn("")),
        # EzKey("M-F10", lazy.spawn("")),
        # EzKey("M-F11", lazy.spawn("")),
        EzKey("M-<F12>", lazy.spawn("maimpick")),
        EzKey("M-S-<F12>", lazy.spawn("maim ~/Storage/F$(date '+%y%m%d-%H%M-%S').png")),
        # }}}
        # ======================= Number Keys ======================= {{{
        EzKey("M-<grave>", lazy.spawn("dunstctl close")),
        EzKey("M-S-<grave>", lazy.spawn("dunstctl history-pop")),
        #
        EzKey("A-C-1", lazy.group.setlayout("max")),
        EzKey("A-C-2", lazy.group.setlayout("monadtall")),
        EzKey("A-C-3", lazy.group.setlayout("treetab")),
        EzKey("A-C-4", lazy.group.setlayout("bsp")),
        EzKey("A-C-5", lazy.group.setlayout("matrix")),
        EzKey("A-C-6", lazy.group.setlayout("ratiotile")),
        EzKey("A-C-7", lazy.group.setlayout("tile")),
        EzKey("A-C-8", lazy.group.setlayout("stack")),
        EzKey("A-C-9", lazy.group.setlayout("monadwide")),
        EzKey("A-C-0", lazy.group.setlayout("monadthreecol")),
        #
        EzKey("M-<minus>", lazy.layout.toggle_split()),
        #
        EzKey("M-<equal>", lazy.spawn("dmenuunicode")),
        #
        EzKey("M-<backspace>", lazy.spawn("sysact Shutdown")),
        EzKey("M-S-<backspace>", lazy.spawn("sysact")),
        EzKey("M-C-<backspace>", lazy.spawn("sysact Suspend")),
        # }}}
        # ======================= First Row ======================= {{{
        # EzKey("M-c", lazy.group["SPD"].dropdown_toggle("mpvfloat")),
        EzKey("A-<Tab>", focus_previous_window()),
        EzKey("M-<Tab>", lazy.screen.toggle_group()),
        EzKey("M-S-<Tab>", lazy.run_extension(WindowList(**dmenu_defaults))),
        #
        EzKey("M-q", lazy.window.kill()),
        EzKey("M-C-q", lazy.shutdown()),
        #
        EzKey("M-w", lazy.spawn(browser)),
        EzKey("M-C-w", lazy.spawn(browserP)),
        EzKey("M-S-w", lazy.spawn(browserA)),
        # # EzKey("M-w", lazy.group["SPD"].dropdown_toggle("float_mpv")),
        # #
        # # EzKey("M-e", lazy.group["SPD"].dropdown_toggle("agenda")),
        EzKey("M-e", lazy.spawn(text_editor)),
        EzKey("M-S-e", lazy.spawn(terminal + " -e emacsclient -c")),
        #
        EzKey("M-S-r", lazy.spawn("via -a")),
        EzKey("M-C-r", lazy.restart()),
        #
        EzKey("M-t", lazy.window.toggle_minimize()),
        EzKey("M-C-t", lazy.group.unminimize_all()),
        EzKey("M-S-t", lazy.spawn("switch-theme")),
        #
        EzKey("M-y", lazy.group["SPD"].dropdown_toggle("qtile_shell")),
        #
        EzKey("M-u", lazy.layout.normalize()),
        # EzKey("M-C-u", move_mpv_to_current_group()),
        #
        EzKey("M-i", float_cycle(False)),
        EzKey("M-C-i", lazy.prev_layout()),
        KeyChord(
            [M, S],
            "i",
            [
                EzKey("h", lazy.spawn("xdotool mousemove_relative -- -15 0")),
                EzKey("l", lazy.spawn("xdotool mousemove_relative -- 15 0")),
                EzKey("j", lazy.spawn("xdotool mousemove_relative -- 0 15")),
                EzKey("k", lazy.spawn("xdotool mousemove_relative -- 0 -15")),
                #
                EzKey("S-h", lazy.spawn("xdotool mousemove_relative -- -50 0")),
                EzKey("S-l", lazy.spawn("xdotool mousemove_relative -- 50 0")),
                EzKey("S-j", lazy.spawn("xdotool mousemove_relative -- 0 50")),
                EzKey("S-k", lazy.spawn("xdotool mousemove_relative -- 0 -50")),
                #
                EzKey("C-h", lazy.spawn("xdotool mousemove_relative -- -100 0")),
                EzKey("C-l", lazy.spawn("xdotool mousemove_relative -- 100 0")),
                EzKey("C-j", lazy.spawn("xdotool mousemove_relative -- 0 100")),
                EzKey("C-k", lazy.spawn("xdotool mousemove_relative -- 0 -100")),
                #
                EzKey("<Return>", lazy.spawn("xdotool click 1")),
            ],
            mode=True,
            name="Mouse",
        ),
        #
        EzKey("M-o", float_cycle(True)),
        EzKey("M-C-o", lazy.next_layout()),
        KeyChord(
            [M, S],
            "o",
            [
                EzKey("h", window_opacity("set", 1)),
                EzKey("l", window_opacity("set", 0.1)),
                EzKey("j", window_opacity("dec", 1)),
                EzKey("k", window_opacity("inc", 1)),
                EzKey("1", window_opacity("set", 0.1)),
                EzKey("2", window_opacity("set", 0.2)),
                EzKey("3", window_opacity("set", 0.3)),
                EzKey("4", window_opacity("set", 0.4)),
                EzKey("5", window_opacity("set", 0.5)),
                EzKey("6", window_opacity("set", 0.6)),
                EzKey("7", window_opacity("set", 0.7)),
                EzKey("8", window_opacity("set", 0.8)),
                EzKey("9", window_opacity("set", 0.9)),
                EzKey("0", window_opacity("set", 1)),
            ],
            mode=True,
            name="Opacity",
        ),
        #
        EzKey("M-p", lazy.spawn("dmpv append")),
        EzKey("M-C-p", lazy.spawn("dmpv aplay ")),
        EzKey("M-S-p", lazy.spawn("dmpv")),
        #
        EzKey("M-<bracketleft>", lazy.screen.prev_group(skip_empty=True)),
        EzKey("M-S-<bracketleft>", lazy.window.move_down()),
        #
        EzKey("M-<bracketright>", lazy.screen.next_group(skip_empty=True)),
        EzKey("M-S-<bracketright>", lazy.window.move_up()),
        #
        EzKey("M-<backslash>", floating_corner_window("bottom right")),
        EzKey("M-S-<backslash>", floating_corner_window("top right")),
        EzKey("M-C-<backslash>", floating_corner_window("bottom left")),
        EzKey("M-S-C-<backslash>", floating_corner_window("top left")),
        # }}}
        # ======================= Second Row ======================= {{{
        EzKey("M-a", lazy.next_screen()),
        EzKey("M-S-a", lazy.function(window_to_next_screen, switch_screen=True)),
        # EzKey("M-A-a", add_treetab_section),
        #
        EzKey("M-s", toggle_sticky_windows()),
        #
        EzKey("M-d", lazy.run_extension(DmenuRun(**dmenu_defaults))),
        EzKey("M-C-d", lazy.spawn("via -r")),
        EzKey("M-S-d", lazy.spawncmd(launcher)),
        #
        EzKey("M-f", lazy.window.toggle_maximize(), lazy.window.keep_above()),
        EzKey("M-S-f", lazy.window.toggle_fullscreen()),
        EzKey("M-C-f", float_to_front()),
        #
        EzKey("M-g", lazy.group["SPD"].toscreen(toggle=True)),
        # EzKey("M-C-g", focus_floating_window()),
        EzKey("M-S-g", lazy.window.togroup("SPD")),
        #
        EzKey("M-h", lazy.layout.left()),
        EzKey("M-S-h", lazy.layout.shuffle_left()),
        EzKey(
            "M-C-h",
            lazy.layout.grow_left(),
            lazy.layout.shrink(),
            lazy.layout.decrease_ratio(),
            lazy.layout.add(),
        ),
        EzKey(
            "M-A-h",
            lazy.layout.shuffle_left(),
            lazy.layout.move_left().when(layout=["treetab"]),
        ),
        #
        EzKey("M-j", lazy.layout.down()),
        EzKey("M-S-j", lazy.layout.shuffle_down()),
        EzKey(
            "M-C-j",
            lazy.layout.grow_down(),
            lazy.layout.shrink(),
            lazy.layout.increase_nmaster(),
        ),
        EzKey(
            "M-A-j",
            lazy.layout.shuffle_down(),
            lazy.layout.section_down().when(layout=["treetab"]),
        ),
        #
        EzKey("M-k", lazy.layout.up()),
        EzKey("M-S-k", lazy.layout.shuffle_up()),
        EzKey(
            "M-C-k",
            lazy.layout.grow_up(),
            lazy.layout.grow(),
            lazy.layout.decrease_nmaster(),
        ),
        EzKey(
            "M-A-k",
            lazy.layout.shuffle_up(),
            lazy.layout.section_up().when(layout=["treetab"]),
        ),
        #
        EzKey("M-l", lazy.layout.right()),
        EzKey("M-S-l", lazy.layout.shuffle_right()),
        EzKey(
            "M-C-l",
            lazy.layout.grow_right(),
            lazy.layout.grow(),
            lazy.layout.increase_ratio(),
            lazy.layout.delete(),
        ),
        EzKey(
            "M-A-l",
            lazy.layout.shuffle_right(),
            lazy.layout.move_right().when(layout=["treetab"]),
        ),
        #
        EzKey("M-<semicolon>", focus_previous_window()),
        EzKey("M-C-<semicolon>", lazy.group.focus_back()),
        #
        EzKey("M-<apostrophe>", lazy.group["SPD"].dropdown_toggle("calculator")),
        #
        EzKey("M-<Return>", lazy.group["SPD"].dropdown_toggle("Tmux Dropdown")),
        EzKey("M-S-<Return>", lazy.spawn(terminal + " msg create-window -e tmux")),
        # }}}
        # ======================= Third Row ======================= {{{
        EzKey("M-z", lazy.window.move_to_top()),
        EzKey("M-S-z", lazy.window.move_to_bottom()),
        #
        EzKey("M-x", lazy.spawn("alm -d")),
        #
        EzKey("M-c", lazy.screen.togglegroup()),
        #
        EzKey("M-v", restore_all_merged_groups()),
        #
        EzKey("M-b", lazy.hide_show_bar("top")),
        EzKey("M-S-b", lazy.spawn("bm S")),
        EzKey("M-C-b", lazy.spawn("bm d")),
        #
        EzKey("M-m", lazy.group["SPD"].dropdown_toggle("music")),
        EzKey("M-S-m", lazy.group["SPD"].dropdown_toggle("spotify")),
        #
        EzKey("M-n", lazy.group["SPD"].dropdown_toggle("news")),
        EzKey("M-S-n", lazy.group["SPD"].dropdown_toggle("podcasts")),
        #
        EzKey("A-<comma>", lazy.prev_screen()),
        EzKey("M-<comma>", lazy.spawn("dmpc toggle")),
        KeyChord(
            [M, C],
            "comma",
            [
                EzKey("t", lazy.spawn("mpc toggle")),
                EzKey("n", lazy.spawn("mpc next")),
                EzKey("p", lazy.spawn("mpc prev")),
                EzKey("s", lazy.spawn("mpc shuffle")),
                EzKey("r", lazy.spawn("mpc repeat")),
                EzKey("u", lazy.spawn("mpc volume +2")),
                EzKey("d", lazy.spawn("mpc volume -2")),
                EzKey("o", lazy.spawn("mpc-notify")),
                EzKey("1", lazy.spawn("mpc volume 10")),
                EzKey("2", lazy.spawn("mpc volume 20")),
                EzKey("3", lazy.spawn("mpc volume 30")),
                EzKey("4", lazy.spawn("mpc volume 40")),
                EzKey("5", lazy.spawn("mpc volume 50")),
                EzKey("6", lazy.spawn("mpc volume 60")),
                EzKey("7", lazy.spawn("mpc volume 70")),
                EzKey("8", lazy.spawn("mpc volume 80")),
                EzKey("9", lazy.spawn("mpc volume 90")),
                EzKey("0", lazy.spawn("mpc volume 100")),
            ],
            mode=True,
            name="Music",
        ),
        KeyChord(
            [M, S],
            "comma",
            [
                EzKey("t", lazy.spawn("tppctl toggle")),
                EzKey("i", lazy.spawn("tppctl invert")),
                EzKey("a", lazy.spawn("tppctl pauseall")),
                EzKey("n", lazy.spawn("tppctl next")),
                EzKey("p", lazy.spawn("tppctl prev")),
                EzKey("k", lazy.spawn("tppctl volume 2")),
                EzKey("j", lazy.spawn("tppctl volume -2")),
                EzKey("1", lazy.spawn("tppctl set 'volume 10'")),
                EzKey("2", lazy.spawn("tppctl set 'volume 20'")),
                EzKey("3", lazy.spawn("tppctl set 'volume 30'")),
                EzKey("4", lazy.spawn("tppctl set 'volume 40'")),
                EzKey("5", lazy.spawn("tppctl set 'volume 50'")),
                EzKey("6", lazy.spawn("tppctl set 'volume 60'")),
                EzKey("7", lazy.spawn("tppctl set 'volume 70'")),
                EzKey("8", lazy.spawn("tppctl set 'volume 80'")),
                EzKey("9", lazy.spawn("tppctl set 'volume 90'")),
                EzKey("0", lazy.spawn("tppctl set 'volume 100'")),
            ],
            mode=True,
            name="MPV",
        ),
        # volume apps
        EzKey("A-<period>", lazy.next_screen()),
        EzKey("M-<period>", lazy.spawn("tppctl invert")),
        EzKey("M-C-<period>", lazy.spawn("tppctl pauseall")),
        EzKey("M-S-<period>", lazy.spawn("tppctl toggle")),
        #
        EzKey("M-<slash>", lazy.group["SPD"].dropdown_toggle("mixer")),
        # }}}
        # ======================= Middle Keys ======================= {{{
        EzKey("M-<space>", toggle_focus_main()),
        EzKey("M-C-<space>", lazy.layout.swap_main()),
        EzKey("M-S-<space>", lazy.window.toggle_floating()),
        EzKey("M-A-<space>", lazy.layout.flip()),
        #
        EzKey("M-<Insert>", lazy.spawn("clipmenu -i")),
        EzKey("M-S-<Insert>", lazy.spawn("clipboard-content.sh")),
        #
        EzKey("<cancel>", lazy.spawn("tppctl invert")),
        EzKey("M-<cancel>", lazy.spawn("tppctl pauseall")),
        EzKey("M-S-<cancel>", lazy.spawn("dmpc toggle")),
        #
        EzKey("M-<up>", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%+")),
        EzKey("M-C-<up>", lazy.spawn("output-audio")),
        EzKey("A-C-<up>", lazy.spawn("tppctl volume 2")),
        EzKey("A-S-<up>", lazy.spawn("mpc volume +2")),
        #
        EzKey("M-<down>", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%-")),
        EzKey("M-C-<down>", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")),
        EzKey("A-C-<down>", lazy.spawn("tppctl volume -2")),
        EzKey("A-S-<down>", lazy.spawn("mpc volume -2")),
        #
        EzKey("M-<left>", lazy.spawn("tppctl seek -10")),
        # EzKey("A-<left>", lazy.spawn("mpc volume -2")),
        # EzKey("C-<left>", lazy.spawn("tppctl volume -2")),
        #
        EzKey("M-<right>", lazy.spawn("tppctl seek 10")),
        # EzKey("A-<right>", lazy.spawn("mpc volume +2")),
        # EzKey("C-<right>", lazy.spawn("tppctl volume 2")),
        # }}}
    ]


# }}}
# ======================= Mouse ============= {{{
from libqtile.config import EzClick, EzDrag


def mouse():
    return [
        EzDrag(
            "M-1", lazy.window.set_position_floating(), start=lazy.window.get_position()
        ),
        EzDrag("M-3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
        EzClick("M-2", lazy.window.bring_to_front()),
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
            key = EzKey("M-" + str(keyname), lazy.group[name].toscreen())
            key_s = EzKey("M-S-" + str(keyname), lazy.window.togroup(name))
            key_c = EzKey("M-C-" + str(keyname), merge_groups(int(name)))
            dgroup.keys.extend([key, key_s, key_c])
            dgroup.qtile.config.keys.extend([key, key_s, key_c])
            dgroup.qtile.grab_key(key)
            dgroup.qtile.grab_key(key_s)
            dgroup.qtile.grab_key(key_c)

    return func


dgroups_key_binder = dr_key_binder()
# }}}
