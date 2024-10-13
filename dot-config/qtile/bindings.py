# ======================= Imports ============= {{{
from os import environ as env
from functions import *
from libqtile.config import EzKey, KeyChord
# from libqtile.extension import DmenuRun

# from libqtile.log_utils import logger
# }}}
# ======================= Keybindings ============= {{{
def bindings():
    M = "mod4"
    A = "mod1"
    S = "shift"
    C = "control"
    terminal = env["TERMINAL"]

    return [
        # ======================= Special Keys ======================= {{{
        EzKey("<XF86HomePage>", lazy.spawn("wlopm --toggle \"*\"")),
        EzKey("M-<XF86HomePage>", lazy.spawn("xset dpms force off")),
        #
        # EzKey("<XF86Search>", lazy.spawn("")),
        #
        EzKey("<XF86Mail>", lazy.group["SPD"].dropdown_toggle("mails")),
        #
        # EzKey("<XF86Launch5>", lazy.spawn("")),
        # EzKey("<XF86Launch6>", lazy.spawn("")),
        EzKey("<XF86Launch7>", lazy.group["SPD"].dropdown_toggle("mixer")),
        EzKey("M-<XF86Launch8>", lazy.spawn("tppctl seek -10")),
        EzKey("M-<XF86Launch9>", lazy.spawn("tppctl seek 10")),
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
        EzKey(
            "<XF86AudioMute>", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")
        ),
        EzKey("M-<XF86AudioMute>", lazy.spawn("output-audio")),
        EzKey("C-<XF86AudioMute>", lazy.group["SPD"].dropdown_toggle("mixer")),
        #
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
        EzKey("<XF86Calculator>", lazy.group["SPD"].dropdown_toggle("calculator")),
        EzKey("M-<XF86Calculator>", lazy.spawn("systemctl suspend")),
        #
        EzKey("<XF86Favorites>", lazy.group["SPD"].dropdown_toggle("btop")),
        EzKey("M-<XF86Favorites>", lazy.group["SPD"].dropdown_toggle("htop")),
        #
        EzKey("<XF86Explorer>", lazy.screen.toggle_group()),
        #
        EzKey("<XF86MonBrightnessDown>", lazy.spawn("xbacklight -dec 5")),
        EzKey("M-<XF86MonBrightnessDown>", lazy.spawn("xbacklight -dec 15")),
        #
        EzKey("<XF86MonBrightnessUp>", lazy.spawn("xbacklight -inc 5")),
        EzKey("M-<XF86MonBrightnessUp>", lazy.spawn("xbacklight -inc 15")),
        #
        EzKey("M-<XF86Back>", lazy.spawn("dmpc prev")),
        EzKey("A-<XF86Back>", lazy.spawn("tppctl seek -10")),
        EzKey("A-C-<XF86Back>", lazy.spawn("tppctl prev")),
        #
        EzKey("M-<XF86Forward>", lazy.spawn("dmpc next")),
        EzKey("A-<XF86Forward>", lazy.spawn("tppctl seek 10")),
        EzKey("A-C-<XF86Forward>", lazy.spawn("tppctl next")),
        # EzKey("M-S", "XF86Forward", lazy.spawn("dmpc seekf")),
        #
        EzKey("<XF86Forward>", lazy.spawn("dmpc next")),
        # }}}
        # ======================= Function Keys ======================= {{{
        EzKey("A-<Escape>", lazy.group["SPD"].dropdown_toggle("btop")),
        EzKey("M-<Escape>", lazy.group["SPD"].dropdown_toggle("htop")),
        #
        # EzKey("M-<F1>", lazy.spawn("")),
        #
        # EzKey("M-<F2>", lazy.spawn("")),
        #
        EzKey("M-<F3>", lazy.spawn("wifi-toggle")),
        #
        # EzKey("M-<F4>", lazy.spawn("")),
        #
        # EzKey("M-<F5>", lazy.spawn("")),
        #
        # EzKey("M-<F6>", lazy.spawn("")),
        #
        # EzKey("M-<F7>", lazy.spawn("")),
        #
        EzKey("M-<F8>", lazy.spawn("dmenumount")),
        EzKey("M-S-<F8>", lazy.spawn("dmenuumount")),
        #
        EzKey("M-<F9>", lazy.spawn("toggle-screens")),
        #
        # EzKey("M-<F10>", lazy.spawn("")),
        #
        # EzKey("M-<F11>", lazy.spawn("")),
        #
        EzKey("M-C-<F12>", lazy.shutdown()),
        #
        EzKey("M-<Print>", lazy.spawn("maimpick")),
        EzKey(
            "M-S-<Print>", lazy.spawn("maim ~/Storage/F$(date '+%y%m%d-%H%M-%S').png")
        ),
        #
        # EzKey("M-<Scroll_Lock>", lazy.spawn("")),
        #
        EzKey("M-<Pause>", toggle_cursor()),
        # }}}
        # ======================= Number Keys ======================= {{{
        EzKey("M-<grave>", lazy.spawn("dunstctl close")),
        EzKey("M-S-<grave>", lazy.spawn("dunstctl history-pop")),
        # EzKey("M-<grave>", lazy.spawn("makoctl dismiss -a")),
        # EzKey("M-S-<grave>", lazy.spawn("makoctl restore")),
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
        # EzKey("M-S-<Tab>", lazy.run_extension(WindowList(**dmenu_defaults))),
        #
        EzKey("M-q", lazy.next_screen()),
        EzKey("M-C-q", lazy.window.kill()),
        # EzKey("M-q", lazy.window.kill()),
        # EzKey("M-C-q", lazy.shutdown()),
        #
        EzKey("M-w", lazy.spawn(env["BROWSER"])),
        EzKey("M-C-w", lazy.spawn(env["BROWSER2"])),
        EzKey("M-S-w", lazy.spawn(env["BROWSER_PRIVATE"])),
        # # EzKey("M-w", lazy.group["SPD"].dropdown_toggle("float_mpv")),
        # #
        # # EzKey("M-e", lazy.group["SPD"].dropdown_toggle("agenda")),
        EzKey("M-e", lazy.spawn("emacsclient -c")),
        EzKey("M-C-e", lazy.spawn(terminal + " -e emacsclient -nw")),
        #
        EzKey("M-r", lazy.group["SPD"].dropdown_toggle("file manager")),
        EzKey("M-C-r", lazy.spawn("qtile cmd-obj -o cmd -f validate_config") and lazy.restart()),
        KeyChord(
            [M, S],
            "r",
            [
                EzKey("h", move_floating_window(x=-10)),
                EzKey("j", move_floating_window(y=10)),
                EzKey("k", move_floating_window(y=-10)),
                EzKey("l", move_floating_window(x=10)),
                EzKey("C-h", resize_floating_window(width=-10)),
                EzKey("C-j", resize_floating_window(height=10)),
                EzKey("C-k", resize_floating_window(height=-10)),
                EzKey("C-l", resize_floating_window(width=10)),
                EzKey("S-h", resize_floating_window(width=-10,height=-10)),
                EzKey("S-j", resize_floating_window(width=-30,height=-30)),
                EzKey("S-k", resize_floating_window(width=30,height=30)),
                EzKey("S-l", resize_floating_window(width=10,height=10)),
            ],
            mode=True,
            name="resize/move floating window",
        ),
        #
        EzKey("M-t", lazy.window.toggle_minimize()),
        EzKey("M-C-t", lazy.group.unminimize_all()),
        EzKey("M-S-t", lazy.spawn("switch-theme")),
        #
        EzKey("M-y", lazy.group["SPD"].dropdown_toggle("qtile_shell")),
        #
        EzKey("M-u", lazy.layout.normalize()),
        EzKey("M-C-u", move_mpv_to_current_group()),
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
        EzKey("M-p", lazy.spawn("dmpv queue")),
        EzKey("M-C-p", lazy.spawn("dmpv")),
        EzKey("M-S-p", lazy.spawn("dmpv aplay")),
        #
        EzKey("M-<bracketleft>", lazy.screen.prev_group(skip_empty=True)),
        EzKey("M-S-<bracketleft>", lazy.window.move_down()),
        EzKey("A-<bracketleft>", lazy.prev_screen()),
        #
        EzKey("M-<bracketright>", lazy.screen.next_group(skip_empty=True)),
        EzKey("M-S-<bracketright>", lazy.window.move_up()),
        EzKey("A-<bracketright>", lazy.next_screen()),
        #
        #
        EzKey("M-<backslash>", floating_corner_window("bottom right")),
        EzKey("M-S-<backslash>", floating_corner_window("top right")),
        EzKey("M-C-<backslash>", floating_corner_window("bottom left")),
        EzKey("M-S-C-<backslash>", floating_corner_window("top left")),
        # }}}
        # ======================= Second Row ======================= {{{
        KeyChord(
            [M],
            "a",
            [
                EzKey("x", lazy.spawn("keepassxc")),
                EzKey("c", lazy.spawn("calibre")),
            ],
            mode=False,
            name="Quick Launch",
        ),
        # EzKey("M-a", lazy.next_screen()),
        # EzKey("M-C-a", lazy.function(window_to_next_screen, switch_screen=True)),
        # EzKey("M-A-a", add_treetab_section),
        #
        EzKey("M-s", toggle_sticky_windows()),
        #
        EzKey("M-d", lazy.spawncmd("run: ")),
        EzKey("M-C-d", lazy.spawn("via -r")),
        EzKey("M-S-d", lazy.spawn("via -a")),
        # EzKey("M-S-d", lazy.run_extension(DmenuRun(**dmenu_defaults))),
        #
        EzKey("M-f", lazy.window.toggle_maximize(), lazy.window.keep_above()),
        EzKey("M-S-f", lazy.window.toggle_fullscreen()),
        EzKey("M-C-f", float_to_front()),
        #
        EzKey("M-g", toggle_layout_gap()),
        KeyChord(
            [M, C],
            "g",
            [
                EzKey("k", resize_layout_gap(+1)),
                EzKey("j", resize_layout_gap(-1)),
                EzKey("C-k", resize_layout_gap(+5)),
                EzKey("C-j", resize_layout_gap(-5)),
                EzKey("S-k", resize_layout_gap(+10)),
                EzKey("S-j", resize_layout_gap(-10)),
            ],
            mode=True,
            name="Gaps",
        ),
        EzKey("M-S-g", lazy.group["SPD"].toscreen(toggle=True)),
        EzKey("A-S-g", lazy.window.togroup("SPD")),
        # EzKey("M-C-g", focus_floating_window()),
        #
        EzKey("M-h", lazy.layout.left()),
        EzKey("M-S-h", lazy.layout.shuffle_left()),
        EzKey(
            "M-C-h",
            lazy.layout.grow_left(),
            lazy.layout.grow_width(10),
            lazy.layout.shrink(),
            lazy.layout.decrease_ratio(),
            lazy.layout.add(),
        ),
        EzKey(
            "M-A-h",
            lazy.layout.shuffle_left(),
            # lazy.layout.move_left().when(layout=["treetab"]),
            lazy.layout.move_left(),
        ),
        #
        EzKey("M-j", lazy.layout.down()),
        EzKey("M-S-j", lazy.layout.shuffle_down()),
        EzKey(
            "M-C-j",
            lazy.layout.grow_down(),
            lazy.layout.grow_height(-10),
            lazy.layout.shrink(),
            lazy.layout.increase_nmaster(),
        ),
        EzKey(
            "M-A-j",
            lazy.layout.shuffle_down(),
            # lazy.layout.section_down().when(layout=["treetab"]),
            lazy.layout.section_down(),
        ),
        #
        EzKey("M-k", lazy.layout.up()),
        EzKey("M-S-k", lazy.layout.shuffle_up()),
        EzKey(
            "M-C-k",
            lazy.layout.grow_up(),
            lazy.layout.grow_height(10),
            lazy.layout.grow(),
            lazy.layout.decrease_nmaster(),
        ),
        EzKey(
            "M-A-k",
            lazy.layout.shuffle_up(),
            # lazy.layout.section_up().when(layout=["treetab"]),
            lazy.layout.section_up(),
        ),
        #
        EzKey("M-l", lazy.layout.right()),
        EzKey("M-S-l", lazy.layout.shuffle_right()),
        EzKey(
            "M-C-l",
            lazy.layout.grow_right(),
            lazy.layout.grow_width(-10),
            lazy.layout.grow(),
            lazy.layout.increase_ratio(),
            lazy.layout.delete(),
        ),
        EzKey(
            "M-A-l",
            lazy.layout.shuffle_right(),
            # lazy.layout.move_right().when(layout=["treetab"]),
            lazy.layout.move_right(),
        ),
        #
        EzKey("M-<semicolon>", focus_previous_window()),
        EzKey("M-C-<semicolon>", lazy.group.focus_back()),
        #
        EzKey("M-<apostrophe>", lazy.group["SPD"].dropdown_toggle("calculator")),
        #
        EzKey("M-<Return>", lazy.group["SPD"].dropdown_toggle("terminal")),
        EzKey("M-S-<Return>", lazy.spawn(terminal)),
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
        EzKey("M-b", toggle_bar()),
        EzKey("M-S-b", lazy.spawn("bm S")),
        EzKey("M-C-b", lazy.spawn("bm d")),
        #
        EzKey("M-m", lazy.group["SPD"].dropdown_toggle("music")),
        EzKey("M-S-m", lazy.group["SPD"].dropdown_toggle("spotify")),
        #
        EzKey("M-n", lazy.group["SPD"].dropdown_toggle("newmacs")),
        # EzKey("M-S-n", lazy.group["SPD"].dropdown_toggle("podcasts")),
        #
        EzKey("M-<comma>", lazy.spawn("dmpc toggle")),
        KeyChord(
            [M, C],
            "comma",
            [
                EzKey("t", lazy.spawn("dmpc toggle")),
                EzKey("n", lazy.spawn("dmpc next")),
                EzKey("p", lazy.spawn("dmpc prev")),
                EzKey("s", lazy.spawn("dmpc shuffle")),
                EzKey("r", lazy.spawn("dmpc repeat")),
                EzKey("u", lazy.spawn("dmpc volume +2")),
                EzKey("d", lazy.spawn("dmpc volume -2")),
                EzKey("o", lazy.spawn("dmpc-notify")),
                EzKey("1", lazy.spawn("dmpc volume 10")),
                EzKey("2", lazy.spawn("dmpc volume 20")),
                EzKey("3", lazy.spawn("dmpc volume 30")),
                EzKey("4", lazy.spawn("dmpc volume 40")),
                EzKey("5", lazy.spawn("dmpc volume 50")),
                EzKey("6", lazy.spawn("dmpc volume 60")),
                EzKey("7", lazy.spawn("dmpc volume 70")),
                EzKey("8", lazy.spawn("dmpc volume 80")),
                EzKey("9", lazy.spawn("dmpc volume 90")),
                EzKey("0", lazy.spawn("dmpc volume 100")),
                # EzKey("u", lazy.spawn("dmpc volume +2"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("d", lazy.spawn("dmpc volume -2"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("o", lazy.spawn("dmpc-notify")),
                # EzKey("1", lazy.spawn("dmpc volume 10"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("2", lazy.spawn("dmpc volume 20"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("3", lazy.spawn("dmpc volume 30"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("4", lazy.spawn("dmpc volume 40"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("5", lazy.spawn("dmpc volume 50"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("6", lazy.spawn("dmpc volume 60"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("7", lazy.spawn("dmpc volume 70"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("8", lazy.spawn("dmpc volume 80"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("9", lazy.spawn("dmpc volume 90"),lazy.widget["genpollcommand"].force_update()),
                # EzKey("0", lazy.spawn("dmpc volume 100"),lazy.widget["genpollcommand"].force_update()),
            ],
            mode=True,
            name="Music",
        ),
        KeyChord(
            [M, S],
            "comma",
            [
                EzKey("t", lazy.spawn("tppctl toggle; notify-send \"$(mpc)\"")),
                EzKey("i", lazy.spawn("tppctl invert; notify-send \"$(mpc)\"")),
                EzKey("a", lazy.spawn("tppctl pauseall; notify-send \"$(mpc)\"")),
                EzKey("n", lazy.spawn("tppctl next; notify-send \"$(mpc)\"")),
                EzKey("p", lazy.spawn("tppctl prev; notify-send \"$(mpc)\"")),
                EzKey("k", lazy.spawn("tppctl volume 2; notify-send \"$(mpc)\"")),
                EzKey("j", lazy.spawn("tppctl volume -2; notify-send \"$(mpc)\"")),
                EzKey("1", lazy.spawn("tppctl set 'volume 10'; notify-send \"$(mpc)\"")),
                EzKey("2", lazy.spawn("tppctl set 'volume 20'; notify-send \"$(mpc)\"")),
                EzKey("3", lazy.spawn("tppctl set 'volume 30'; notify-send \"$(mpc)\"")),
                EzKey("4", lazy.spawn("tppctl set 'volume 40'; notify-send \"$(mpc)\"")),
                EzKey("5", lazy.spawn("tppctl set 'volume 50'; notify-send \"$(mpc)\"")),
                EzKey("6", lazy.spawn("tppctl set 'volume 60'; notify-send \"$(mpc)\"")),
                EzKey("7", lazy.spawn("tppctl set 'volume 70'; notify-send \"$(mpc)\"")),
                EzKey("8", lazy.spawn("tppctl set 'volume 80'; notify-send \"$(mpc)\"")),
                EzKey("9", lazy.spawn("tppctl set 'volume 90'; notify-send \"$(mpc)\"")),
                EzKey("0", lazy.spawn("tppctl set 'volume 100'; notify-send \"$(mpc)\"")),
            ],
            mode=True,
            name="MPV",
        ),
        #
        EzKey("A-<period>", lazy.spawn("tppctl")),
        EzKey("A-C-<period>", lazy.spawn("tppctl next")),
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
        EzKey("A-<space>", lazy.layout.flip()),
        #
        # EzKey("M-<Insert>", lazy.group["SPD"].dropdown_toggle("clipmenu")),
        EzKey("M-<Insert>", lazy.spawn("clipboardmenu")),
        EzKey("M-S-<Insert>", lazy.spawn("clipboard-content.sh")),
        #
        # EzKey("M-<Home>", lazy.spawn("")),
        #
        # EzKey("M-<XF86ScrollUp>", lazy.spawn("")),
        #
        # EzKey("M-<Cancel>", lazy.spawn("")),
        #
        # EzKey("M-<End>", lazy.spawn("")),
        #
        # EzKey("M-<XF86ScrollDown>", lazy.spawn("")),
        #
        EzKey("M-<up>", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%+")),
        EzKey("M-C-<up>", lazy.spawn("output-audio")),
        KeyChord(
            [M, S],
            "up",
            [
                EzKey("t", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")),
                EzKey("n", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%+")),
                EzKey("p", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%-")),
                EzKey("k", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%+")),
                EzKey("j", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%-")),
                EzKey("1", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.10'")),
                EzKey("2", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.20'")),
                EzKey("3", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.30'")),
                EzKey("4", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.40'")),
                EzKey("5", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.50'")),
                EzKey("6", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.60'")),
                EzKey("7", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.70'")),
                EzKey("8", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.80'")),
                EzKey("9", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.90'")),
                EzKey("0", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.99'")),
            ],
            mode=True,
            name="wpctl",
        ),
        # EzKey("A-C-<up>", lazy.spawn("tppctl volume 2")),
        # EzKey("A-S-<up>", lazy.spawn("mpc volume +2")),
        #
        EzKey("M-<down>", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%-")),
        EzKey("M-C-<down>", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")),
        # EzKey("A-C-<down>", lazy.spawn("tppctl volume -2")),
        # EzKey("A-S-<down>", lazy.spawn("mpc volume -2")),
        #
        EzKey("M-<left>", lazy.spawn("tppctl seek -5")),
        EzKey("A-<left>", lazy.spawn("mpc volume -2")),
        EzKey("C-<left>", lazy.spawn("tppctl volume -2")),
        #
        EzKey("M-<right>", lazy.spawn("tppctl seek 5")),
        EzKey("A-<right>", lazy.spawn("mpc volume +2")),
        EzKey("C-<right>", lazy.spawn("tppctl volume 2")),
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
            g = list(range(1, 10))
            g.append(0)
            keys = list(map(str, g))

        # bind all keys
        for keyname, group in zip(keys, dgroup.qtile.groups):
            name = group.name
            key = EzKey("M-" + str(keyname), lazy.group[name].toscreen())
            key_s = EzKey("M-S-" + str(keyname), lazy.window.togroup(name))
            key_c = EzKey("M-C-" + str(keyname), merge_groups(int(name)))
            # key_c = EzKey("M-C-" + str(keyname), merge_groups_v2(int(name)))
            dgroup.keys.extend([key, key_s, key_c])
            dgroup.qtile.config.keys.extend([key, key_s, key_c])
            dgroup.qtile.grab_key(key)
            dgroup.qtile.grab_key(key_s)
            dgroup.qtile.grab_key(key_c)

    return func


dgroups_key_binder = dr_key_binder()
# }}}
# ======================= Multi-Screens ============= {{{
# from libqtile import qtile, hook

# @hook.subscribe.startup
# def _():
#     # Set initial groups
#     if len(qtile.screens) > 1:
#         for i in gr
#         qtile.groups_map["1"].cmd_toscreen(0, toggle=False)
#         qtile.groups_map["q"].cmd_toscreen(1, toggle=False)
# }}}
