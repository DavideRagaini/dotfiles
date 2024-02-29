from libqtile.config import Key, Drag, Click
from libqtile.extension import WindowList, DmenuRun, CommandSet
from libqtile.lazy import lazy

from functions import Function

def dmenu_defaults_init():
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
    defaults = dict(
        dmenu_font = opts[0],
        dmenu_ignorecase = True,
        background = opts[1],
        foreground = opts[2],
        selected_background = opts[3],
        selected_foreground = opts[4],
    )
    return defaults

dmenu_defaults = dmenu_defaults_init()

class Keys(object):
    def init_keys(self):
        mod = "mod4"
        alt = "mod1"
        shift = "shift"
        ctrl = "control"
        alt = "mod1"

        from os import environ as env

        terminal = env["TERMINAL"]
        terminal = env["TERMINAL"]
        browser = env["BROWSER"]
        browser_alt = env["BROWSER2"]
        browser_priv = env["BROWSER_PRIVATE"]
        # text_editor = terminal + " --class 'emacs,emacs' -T 'term-emacsclient' -e emacsclient -nw"
        text_editor = "emacsclient -c"
        launcher = "run"

        return [
            Key([alt, ctrl], "1", lazy.group.setlayout("max")),
            Key([alt, ctrl], "2", lazy.group.setlayout("monadtall")),
            Key([alt, ctrl], "3", lazy.group.setlayout("treetab")),
            Key([alt, ctrl], "4", lazy.group.setlayout("bsp")),
            Key([alt, ctrl], "5", lazy.group.setlayout("matrix")),
            Key([alt, ctrl], "6", lazy.group.setlayout("ratiotile")),
            Key([alt, ctrl], "7", lazy.group.setlayout("tile")),
            Key([alt, ctrl], "8", lazy.group.setlayout("stack")),
            Key([alt, ctrl], "9", lazy.group.setlayout("monadwide")),
            Key([alt, ctrl], "0", lazy.group.setlayout("monadthreecol")),
            Key([mod], "backslash", lazy.function(Function.floating_window_corner(1))),
            Key(
                [mod, shift],
                "backslash",
                lazy.function(Function.floating_window_corner(2)),
            ),
            Key(
                [mod, ctrl],
                "backslash",
                lazy.function(Function.floating_window_corner(3)),
            ),
            Key(
                [mod, ctrl, shift],
                "backslash",
                lazy.function(Function.floating_window_corner(4)),
            ),
            Key([mod], "a", lazy.next_screen()),
            Key(
                [mod, shift],
                "a",
                lazy.function(Function.move_window_to_next_screen, switch_screen=True),
            ),
            # Key([mod], "s", lazy.function(Function.sticky_window.toggle())),
            Key([mod], "c", lazy.screen.togglegroup()),
            Key([mod], "z", lazy.window.move_to_top()),
            Key([mod, shift], "z", lazy.window.move_to_bottom()),
            Key([mod], "x", lazy.spawn("alm -d")),
            Key([mod], "bracketright", lazy.screen.next_group(skip_empty=True)),
            Key(
                [mod],
                "bracketleft",
                lazy.screen.prev_group(skip_empty=True),
            ),
            Key([mod, shift], "bracketleft", lazy.window.move_down()),
            Key([mod, shift], "bracketright", lazy.window.move_up()),
            #
            Key([mod], "t", lazy.window.toggle_minimize()),
            Key([mod], "h", lazy.layout.left()),
            Key([mod], "l", lazy.layout.right()),
            Key([mod], "j", lazy.layout.down()),
            Key([mod], "k", lazy.layout.up()),
            # Move windows between left/right columns or move up/down in current stack.
            Key(
                [mod, shift],
                "h",
                lazy.layout.shuffle_left(),
            ),
            Key(
                [mod, shift],
                "l",
                lazy.layout.shuffle_right(),
            ),
            Key([mod, shift], "j", lazy.layout.shuffle_down()),
            Key([mod, shift], "k", lazy.layout.shuffle_up()),
            # Grow windows
            Key(
                [mod, ctrl], "h", lazy.layout.shrink(), desc="Grow window to the right"
            ),
            Key([mod, ctrl], "l", lazy.layout.grow()),
            Key([mod, ctrl], "j", lazy.layout.shrink_down()),
            Key([mod, ctrl], "k", lazy.layout.shrink_up()),
            Key([mod], "space", lazy.function(Function.dwm_swap.swap_focus_main())),
            Key([mod, ctrl], "space", lazy.function(Function.dwm_swap.focus_main())),
            Key(
                [mod, shift],
                "space",
                lazy.window.toggle_floating(),
                desc="Toggle Current Window Floating",
            ),
            Key([mod, alt], "space", lazy.layout.flip()),
            Key([mod], "u", lazy.layout.normalize()),
            Key(
                [mod], "Tab", lazy.screen.toggle_group(), desc="Reset all window sizes"
            ),
            # Key([mod], "Tab", focus_previous_window()),
            Key([mod, shift], "Tab", lazy.run_extension(WindowList(**dmenu_defaults))),
            # Toggle between split and unsplit sides of stack.
            # Split = all windows displayed
            # Unsplit = 1 window displayed, like Max layout, but still with multiple stack panes
            # Key([mod, ctrl], "space", lazy.layout.toggle_split()),
            Key([mod], "q", lazy.window.kill()),
            Key([mod], "o", lazy.function(Function.float_cycle(True))),
            Key([mod, shift], "o", lazy.next_layout()),
            Key([mod, ctrl], "o", lazy.function(Function.opacity.reset(1))),
            Key([mod, shift, ctrl], "o", lazy.function(Function.opacity.inc())),
            Key([mod], "i", lazy.function(Function.float_cycle(False))),
            Key([mod, shift], "i", lazy.prev_layout()),
            Key([mod, ctrl], "i", lazy.function(Function.opacity().reset(0.1))),
            Key([mod, shift, ctrl], "i", lazy.function(Function.opacity().dec())),
            Key([mod, ctrl], "r", lazy.restart()),
            Key([mod, ctrl], "q", lazy.shutdown()),
            Key([mod], "backspace", lazy.spawn("sysact Shutdown")),
            Key([mod, shift], "backspace", lazy.spawn("sysact")),
            Key([mod, ctrl], "backspace", lazy.spawn("sysact Suspend")),
            Key([mod], "f", lazy.window.toggle_maximize()),
            Key(
                [mod, shift],
                "f",
                lazy.window.toggle_fullscreen(),
                desc="Toggle Current Window Fullscreen",
            ),
            Key([mod, ctrl], "f", lazy.function(Function.float_to_front())),
            # Key([mod, alt], "f", focus_floating_window()),
            Key([mod], "v", lazy.function(Function.dwm_merge_groups().restore_all())),
            Key([mod], "b", lazy.hide_show_bar("top")),
            # Key([mod, alt], "a", add_treetab_section),
            Key(
                [mod, alt],
                "h",
                lazy.layout.shuffle_left(),
                lazy.layout.move_left().when(layout=["treetab"]),
            ),
            Key(
                [mod, alt],
                "l",
                lazy.layout.shuffle_right(),
                lazy.layout.move_right().when(layout=["treetab"]),
            ),
            Key(
                [mod, alt],
                "j",
                lazy.layout.shuffle_down(),
                lazy.layout.section_down().when(layout=["treetab"]),
            ),
            Key(
                [mod, alt],
                "k",
                lazy.layout.shuffle_up(),
                lazy.layout.section_up().when(layout=["treetab"]),
            ),
            Key([alt], "comma", lazy.prev_screen()),
            Key([alt], "period", lazy.next_screen()),
            # Applications
            Key([mod], "grave", lazy.spawn("dunstctl close")),
            Key(
                [mod, shift],
                "grave",
                lazy.spawn("dunstctl history-pop"),
            ),
            Key(
                [mod, shift],
                "Return",
                lazy.spawn(terminal + " msg create-window -e tmux"),
            ),
            Key([mod], "w", lazy.spawn(browser)),
            Key(
                [mod, shift],
                "w",
                lazy.spawn(browser_alt),
            ),
            Key(
                [mod, ctrl],
                "w",
                lazy.spawn(browser_priv),
            ),
            Key([mod, shift], "b", lazy.spawn("bm S")),
            Key([mod, ctrl], "b", lazy.spawn("bm d")),
            Key([mod], "e", lazy.spawn(text_editor)),
            Key(
                [mod, shift],
                "e",
                lazy.spawn(terminal + " -e emacsclient -c"),
            ),
            # Key([mod], "d", lazy.spawn("dmenu_run")),
            Key(
                [mod],
                "d",
                lazy.run_extension(DmenuRun(**dmenu_defaults)),
            ),
            Key([mod, shift], "d", lazy.spawn("via -r")),
            Key(
                [mod, ctrl], "t", lazy.spawn("switch-theme"), desc="Global Theme Toggle"
            ),
            Key([mod, shift], "r", lazy.spawn("via -a")),
            Key([mod, ctrl], "d", lazy.spawncmd(launcher)),
            Key(
                [mod], "equal", lazy.spawn("dmenuunicode"), desc="Unicode Search Prompt"
            ),
            Key([mod], "Insert", lazy.spawn("clipmenu -i")),
            Key([mod, shift], "Insert", lazy.spawn("clipboard-content.sh")),
            Key([mod], "F12", lazy.spawn("maimpick")),
            Key(
                [mod, shift],
                "F12",
                lazy.spawn("maim ~/Storage/F$(date '+%y%m%d-%H%M-%S').png"),
            ),
            Key([], "XF86Favorites", lazy.spawn("bm s")),
            # Key([], "XF86Search", lazy.spawn("ferdium")),
            Key(
                [], "XF86Explorer", lazy.screen.toggle_group(), desc="Switch to group 1"
            ),
            # Media
            Key([], "XF86AudioPlay", lazy.spawn("dmpc toggle")),
            Key([mod], "XF86AudioPlay", lazy.spawn("tppctl toggle")),
            Key([mod, shift], "XF86AudioPlay", lazy.spawn("tppctl invert")),
            Key([], "XF86AudioNext", lazy.spawn("dmpc next")),
            Key([mod], "XF86AudioNext", lazy.spawn("tppctl seek 10")),
            Key([mod], "XF86Launch8", lazy.spawn("tppctl seek -10")),
            Key([], "XF86AudioPrev", lazy.spawn("dmpc prev")),
            Key([mod], "XF86AudioPrev", lazy.spawn("tppctl seek -10")),
            Key([mod], "XF86Launch9", lazy.spawn("tppctl seek 10")),
            Key([], "XF86Launch5", lazy.spawn("tppctl invert")),
            Key([mod, shift], "XF86Back", lazy.spawn("tppctl seek -10")),
            Key([mod, ctrl], "XF86Back", lazy.spawn("dmpc prev")),
            # Key([mod, shift], "XF86Back", lazy.spawn("dmpc seekp")),
            Key([mod, shift], "XF86Forward", lazy.spawn("tppctl seek 10")),
            Key([mod, ctrl], "XF86Forward", lazy.spawn("dmpc next")),
            # Key([mod, shift], "XF86Forward", lazy.spawn("dmpc seekf")),
            Key([], "cancel", lazy.spawn("tppctl invert")),
            Key([mod], "cancel", lazy.spawn("tppctl pauseall")),
            Key([mod, shift], "cancel", lazy.spawn("dmpc toggle")),
            # Key([], "XF86Search", lazy.spawn("")),
            # Key([], "XF86HomePage", lazy.spawn("")),
            # Media Control
            Key([mod], "p", lazy.spawn("dmpv append")),
            Key([mod, shift], "p", lazy.spawn("dmpv aplay ")),
            Key([mod, ctrl], "p", lazy.spawn("dmpv")),
            Key([mod], "comma", lazy.spawn("dmpc toggle")),
            Key(
                [mod],
                "period",
                lazy.spawn("tppctl invert"),
                desc="Inverte Playing MPVs",
            ),
            Key(
                [mod, shift], "period", lazy.spawn("tppctl toggle"), desc="Toggle MPVs"
            ),
            Key(
                [mod, ctrl],
                "m",
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
            Key(
                [mod, ctrl],
                "period",
                lazy.spawn("tppctl pauseall"),
            ),
            Key(
                [mod],
                "semicolon",
                lazy.function(Function.change_focus_previous_window()),
            ),
            Key(
                [mod, shift],
                "semicolon",
                lazy.group.focus_back(),
            ),
            # Hardware/system control
            Key(
                [mod],
                "up",
                lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"),
            ),
            Key([mod, ctrl], "up", lazy.spawn("output-audio")),
            Key(
                [],
                "XF86AudioRaiseVolume",
                lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"),
            ),
            Key(
                [mod],
                "down",
                lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-"),
            ),
            Key(
                [mod, shift],
                "down",
                lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),
            ),
            Key(
                [],
                "XF86AudioLowerVolume",
                lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-"),
            ),
            Key([mod], "XF86AudioMute", lazy.spawn("output-audio")),
            Key(
                [],
                "XF86AudioMute",
                lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),
            ),
            Key([mod], "left", lazy.spawn("tppctl seek -10")),
            Key([mod], "right", lazy.spawn("tppctl seek 10")),
            Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 5")),
            Key([mod], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 15")),
            Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 5")),
            Key([mod], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 15")),
            Key([mod], "F3", lazy.spawn("wifi-toggle")),
            Key([mod], "F8", lazy.spawn("dmenumount")),
            Key([mod, shift], "F8", lazy.spawn("dmenuumount")),
            # Key([mod], "kp-add", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+")),
            # Key([mod], "kp-subtract", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-")),
        ]


class Mouses(object):
    def init_mouse(self):
        mod = "mod4"

        return [
            # Move floating windows
            Drag(
                [mod],
                "Button1",
                lazy.window.set_position_floating(),
                start=lazy.window.get_position(),
            ),
            # Resize floating windows
            Drag(
                [mod],
                "Button3",
                lazy.window.set_size_floating(),
                start=lazy.window.get_size(),
            ),
            # Bring to front
            Click([mod], "Button2", lazy.window.bring_to_front()),
        ]
