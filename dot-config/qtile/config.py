# ======================= TODOs ============= {{{
# TODO Move to Keychord
# TODO Keychord to resize floating windows
# TODO STICKY: when group switch focus, then unfocus floating
# TODO MPD widget empty string when paused
# }}}
# ======================= Imports ============= {{{
from typing import List

from libqtile import bar, hook, layout, widget
from libqtile.config import Click, Drag, DropDown, Group, Key, Match, ScratchPad, Screen
from libqtile.dgroups import simple_key_binder
from libqtile.extension import WindowList, DmenuRun, CommandSet
from libqtile.lazy import lazy

from os import environ as env
from re import compile as regex

# from datetime import datetime
# from libqtile.log_utils import logger

# from libqtile.utils import guess_terminal
# from libqtile.widget import Spacer, Backlight
# from libqtile.widget.image import Image
# import socket
# }}}
# ======================= Variables ============= {{{
mod = "mod4"
alt = "mod1"
shift = "shift"
ctrl = "control"

browser = env["BROWSER"]
browser_alt = env["BROWSER2"]
browser_priv = env["BROWSER_PRIVATE"]
terminal = env["TERMINAL"]
# text_editor = terminal + " --class 'emacs,emacs' -T 'term-emacsclient' -e emacsclient -nw"
text_editor = "emacsclient -c"
file_manager = terminal + " -e tmux new-session -A -s 'files'"
launcher = "run"


# }}}
# ======================= Colors ============= {{{
def dracula():
    colors = [
        ["#282a36", "#282a36"],  # background (dark grey) [0]
        ["#44475a", "#44475a"],  # light grey [1]
        ["#f8f8f2", "#f8f8f2"],  # forground (white) [2]
        ["#6272a4", "#6272a4"],  # blue/grey) [3]
        ["#8be9fd", "#8be9fd"],  # cyan [4]
        ["#50fa7b", "#50fa7b"],  # green [5]
        ["#ffb86c", "#ffb86c"],  # orange [6]
        ["#ff79c6", "#ff79c6"],  # pink [7]
        ["#bd93f9", "#bd93f9"],  # purple [8]
        ["#ff5555", "#ff5555"],  # red [9]
        ["#f1fa8c", "#f1fa8c"],  # yellow [10]
    ]
    backgroundColor = "#282a36"
    foregroundColor = "#f8f8f2"
    workspaceColor = "#bd93f9"
    foregroundColorTwo = "#44475a"
    return colors, backgroundColor, foregroundColor, workspaceColor, foregroundColorTwo

def dmenu_defaults():
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

    return opts

colorscheme = dracula()
(
    colors,
    backgroundColor,
    foregroundColor,
    workspaceColor,
    foregroundColorTwo,
) = colorscheme

dmenu_options = dmenu_defaults()
# }}}
# ======================= Groups ============= {{{
#  󰇮   󰈙     
groups = [
    Group(
        name="1",
        position=1,
        layout="monadtall",
        exclusive=True,
        matches=[
            Match(wm_instance_class="emacs"),
            Match(wm_class="Alacritty"),
        ],
    ),
    Group(
        name="2",
        position=2,
        layout="monadtall",
        exclusive=True,
        matches=Match(
            wm_class=[
                "firefox",
                "Firefox",
                "librewolf",
                "LibreWolf",
                "Brave-browser",
                "Thorium-browser",
                "qutebrowser",
            ]
        ),
    ),
    Group(
        name="3",
        position=3,
        layout="max",
        exclusive=True,
        matches=Match(
            wm_class=[
                "Zathura",
                "sioyek",
                "Evince",
                "okular",
                "ebook-viewer",
                "calibre-ebook-viewer",
                "Nsxiv",
            ]
        ),
    ),
    Group(
        name="4",
        layout="max",
        position=4,
        matches=[
            Match(
                wm_instance_class="sun-awt-X11-XDialogPeer",
                title="MATLAB Editor",
            ),
            Match(
                wm_instance_class="Matlab-GLEE",
                wm_class=regex("MATLAB R202[0-9][a-b] Update [0-9]+"),
            ),
            Match(
                wm_instance_class="sun-awt-X11-XFramePeer",
                title=regex("MATLAB R202[0-9][a-b] - academic use"),
            ),
            Match(
                title="MathWorks Product Installer",
            ),
            Match(
                wm_class="MATLABWindow",
                wm_instance_class="MATLABWindow",
                title="Control System Designer*",
            ),
            Match(
                wm_class=[
                    "VirtualBox Machine",
                    regex("MATLAB R202[0-9][a-b] - academic use"),
                    regex("MATLAB R202[0-9][a-b]"),
                    "MATLABWindow",
                    "Matlab-GLEE",
                ],
                wm_instance_class=["sun-awt-X11-XDialogPeer"],
            ),
        ],
    ),
    Group(
        name="5",
        position=5,
        layout="max",
        matches=[
            Match(
                wm_instance_class="MATLABWindow",
                wm_class="MATLABWindow",
                title="Variable-references - Signal Editor",
            ),
        ],
    ),
    Group(
        name="6",
        position=6,
        layout="ratiotile",
        matches=[
            Match(
                wm_instance_class="sun-awt-X11-XFramePeer",
                title=" ",
            ),
            Match(
                wm_instance_class="MATLABWindow",
                wm_class="MATLABWindow",
                title="Variable-references - Signal Editor",
            ),
            Match(
                title="asbQuadcopter/Command/Signal Editor * - Simulink academic use",
                wm_instance_class="Matlab-GLEE",
                wm_class=regex("MATLAB R202[0-9][a-b] Update [0-9]+"),
            ),
            Match(
                title="Block Parameters: Position/Attitude Reference",
                wm_instance_class="Matlab-GLEE",
                wm_class=regex("MATLAB R202[0-9][a-b] Update [0-9]+"),
            ),
            Match(
                title="Block Parameters: Position/Attitude Reference",
                wm_instance_class="Matlab-GLEE",
                wm_class=regex("MATLAB R202[0-9][a-b] Update [0-9]+"),
            ),
            Match(wm_instance_class=["sun-awt-X11-XFramePeer"]),
        ],
    ),
    Group(
        name="7",
        position=7,
        layout="monadtall",
        # matches=Match(wm_class=[
        #                 ]),
    ),
    Group(
        name="8",
        position=8,
        layout="max",
        matches=Match(
            wm_class=[
                "teams-for-linux",
                "microsoft teams - preview",
                "Ferdium",
                "KeePassXC",
                "qBittorrent",
                "calibre-gui",
                "calibre",
            ]
        ),
    ),
    Group(
        name="9",
        position=9,
        layout="max",
        # exclusive=True,
        matches=[Match(wm_class=["mpv", "Kodi"])],
    ),
    Group(
        name="0",
        position=10,
        layout="columns",
    ),
]
# }}}
# ======================= Functions / Hooks ============= {{{
import subprocess
@lazy.function
def floating_border_window(qtile, position=1):
    window = qtile.current_screen.group.current_window
    current_screen = qtile.current_screen.info()
    screen_width = current_screen["width"]
    screen_height = current_screen["height"]
    div = 6
    window_width = int(int(screen_width)/div)
    window_height = int(int(screen_height)/div)
    border_padding = 3
    bar_padding = 20
    window.toggle_floating()
    match position:
        case 1:  # bottom right
            window_x = screen_width - window_width - border_padding
            window_y = screen_height - window_height - border_padding
        case 2:  # top right
            window_x = screen_width - window_width - border_padding
            window_y = bar_padding
        case 3:  # bottom left
            window_x = border_padding
            window_y = screen_height - window_height - border_padding
        case 4:  # top left
            window_x = border_padding
            window_y = bar_padding
    window.place(
        window_x, window_y, window_width, window_height, 1, "#FF00FF", True, None, True
    )


@lazy.function
def window_opacity(qtile, cmd, value=1):
    match cmd:
        case "inc":
            qtile.current_group.current_window.up_opacity()
        case "dec":
            qtile.current_group.current_window.down_opacity()
        case "set":
            qtile.current_group.current_window.set_opacity(value)


sticky_windows: List[str] = []


@lazy.function
def toggle_sticky_windows(qtile, window=None):
    if window is None:
        window = qtile.current_screen.group.current_window
    if window in sticky_windows:
        sticky_windows.remove(window)
    else:
        sticky_windows.append(window)
    return window


@hook.subscribe.setgroup
def move_sticky_windows():
    for window in sticky_windows:
        window.togroup()
    return


@hook.subscribe.client_killed
def remove_sticky_windows(window):
    if window in sticky_windows:
        sticky_windows.remove(window)


# @hook.subscribe.client_managed
# def auto_sticky_windows(window):
#     info = window.info()
#     if info["wm_class"] == ["mpvFloat", "mpv"]:
#         sticky_windows.append(window)

merged = dict()

def check_merged_already_in(g):
    global merged
    if len(merged) <= 0:
        return False
    else:
        for index in list(merged.keys()):
            w = merged[index]
            if w['original_group'] == str(g):
                return True
        return False

@lazy.function
def merge_groups(qtile, g=0):
    global merged
    len_merged = len(merged)
    if (check_merged_already_in(g) == True):
        for index in list(merged.keys()):
            w = merged[index]
            if w['original_group'] == str(g):
                w['window'].togroup(w['original_group'])
                del merged[index]
    else:
        windows_tomerge = qtile.groups[g-1].windows
        for i in reversed(range(len(windows_tomerge))):
            j = 0
            while i != list(merged.keys()) or j<10:
                index = len_merged + i + j
                w = windows_tomerge[i]
                merged[index] = { 'window': w, 'original_group': w.info()['group'] }
                w.togroup()

@lazy.function
def restore_merge_all_groups(qtile):
    global merged
    for index in range(len(merged)):
        w = merged[index]
        w['window'].togroup(w['original_group'])
        del merged[index]


last_focus_index = -1

def swap_focus_main(qtile):
    """
    https://github.com/qtile/qtile/discussions/3621
    """

    layout = qtile.current_layout

    if layout.name == "monadtall":
        global last_focus_index
        current_index = layout.clients.current_index
        # 0 is main window
        if current_index == 0:
            if last_focus_index < 0:
                # nothing to swap with
                return
            # swap with last
            target_index = last_focus_index
        else:
            # swap subordinates with main
            target_index = current_index
            last_focus_index = current_index

        main_window = layout.clients[0]
        target_window = layout.clients[target_index]
        # swaps windows and keeps focus on main
        layout.swap(target_window, main_window)

def focus_main(qtile):
    layout = qtile.current_layout
    if layout.align == 1:
        layout.right()
        return
    layout.left()

@lazy.function
def float_to_front(qtile):
    for group in qtile.groups:
        for window in group.windows:
            if window.floating:
                window.bring_to_front()


floating_window_index = 0


def float_cycle(qtile, forward: bool):
    global floating_window_index
    floating_windows = []
    for window in qtile.current_group.windows:
        if window.floating:
            floating_windows.append(window)
    if not floating_windows:
        return
    floating_window_index = min(floating_window_index, len(floating_windows) - 1)
    if forward:
        floating_window_index += 1
    else:
        floating_window_index += 1
    if floating_window_index >= len(floating_windows):
        floating_window_index = 0
    if floating_window_index < 0:
        floating_window_index = len(floating_windows) - 1
    win = floating_windows[floating_window_index]
    win.bring_to_front()
    win.focus()


@lazy.function
def float_cycle_backward(qtile):
    float_cycle(qtile, False)


@lazy.function
def float_cycle_forward(qtile):
    float_cycle(qtile, True)


previous_focused: List[str] = []


@hook.subscribe.client_focus
def client_focused(window):
    global previous_focused
    if len(previous_focused) < 2:
        previous_focused.append(window)
    elif previous_focused[1] != window:
        previous_focused[0] = previous_focused[1]
        previous_focused[1] = window


@lazy.function
def focus_previous_window(qtile):
    global previous_focused
    if len(previous_focused) == 2:
        group = previous_focused[0].group
        qtile.current_screen.set_group(group)
        group.focus(previous_focused[0])


# # kick a window to another screen (handy during presentations)
# def kick_to_next_screen(qtile, direction=1):
# 	other_scr_index = (qtile.screens.index(qtile.currentScreen) + direction) % len(qtile.screens)
# 	othergroup = None
# 	for group in qtile.groups().values():
# 		if group['screen'] == other_scr_index:
# 			othergroup = group['name']
# 			break
# 	if othergroup:
# 		qtile.moveToGroup(othergroup)


def window_to_next_screen(qtile, switch_group=False, switch_screen=False):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i - 1].group.name
        qtile.current_window.togroup(group, switch_group=switch_group)
        if switch_screen == True:
            qtile.to_screen(i - 0)
    elif i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group, switch_group=switch_group)
        if switch_screen == True:
            qtile.to_screen(i + 1)


@hook.subscribe.client_focus
def set_hint(window):
    window.window.set_property(
        "IS_FLOATING_WINDOW", str(window.floating), type="STRING", format=8
    )


# Allows you to input a name when adding treetab section.
# @lazy.layout.function
# def add_treetab_section(layout):
#     prompt = qtile.widgets_map["prompt"]
#     prompt.start_input("Section name: ", layout.add_section)

# @hook.subscribe.client_new
# def disable_floating(window):
#     rules = [
#         Match(wm_class="mpv")
#     ]

#     if any(window.match(rule) for rule in rules):
#         window.togroup(qtile.current_group.name)
#         window.disable_floating()
# }}}
# ======================= Scratchpads ============= {{{
groups.append(
    ScratchPad(
        "scratchpad",
        dropdowns=[
            DropDown(
                "Dropdown",
                terminal + " -e tmux new-session -A -s 'dropdown'",
                width=0.9,
                height=0.9,
                x=0.04,
                y=0.04,
                opacity=0.90,
                on_focus_lost_hide=True,
            ),
            # DropDown(
            #     "agenda",
            #     "Eagenda",
            #     width=0.8,
            #     height=0.8,
            #     x=0.1,
            #     y=0.1,
            #     opacity=0.9,
            #     on_focus_lost_hide = False
            # ),
            DropDown(
                "file manager",
                file_manager,
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "btop",
                terminal + " -e btop",
                width=0.9,
                height=0.9,
                x=0.04,
                y=0.04,
                opacity=0.90,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "htop",
                terminal + " -e btop",
                width=0.9,
                height=0.9,
                x=0.04,
                y=0.04,
                opacity=0.90,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "news",
                terminal + " --class 'newsboat,newsboat' -T 'newsboat' -e newsboat",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "podcasts",
                terminal + " --class 'podboat,podboat' -T 'podboat' -e podboat",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "music",
                terminal + " --class 'ncmpcpp,ncmpcpp' -T 'ncmpcpp'  -e ncmpcpp",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "spotify",
                terminal + " --class 'spotify-tui,spotify-tui' -T 'spotify-tui'  -e spt",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "mixer",
                terminal + " -e pulsemixer",
                width=0.6,
                height=0.3,
                x=0.2,
                y=0.2,
                opacity=0.9,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "calculator",
                terminal + " -e wcalc -P -1 -c -q --ints -C -p -r --remember",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "qtile_shell",
                terminal + " -e qtile shell",
                x=0.05,
                y=0.05,
                width=0.9,
                height=0.6,
                opacity=0.9,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "mails",
                "ferdium",
                width=0.9,
                height=0.9,
                x=0.04,
                y=0.04,
                opacity=0.9,
                on_focus_lost_hide=False,
            ),
        ],
    )
)
# }}}
# ======================= Mouse ============= {{{
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]
# }}}
dmenu_defaults = dict(
    dmenu_font = dmenu_options[0],
    dmenu_ignorecase = True,
    background = dmenu_options[1],
    foreground = dmenu_options[2],
    selected_background = dmenu_options[3],
    selected_foreground = dmenu_options[4],
)
# ======================= Keybindings ============= {{{
keys = [
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
    # Switch between windows
    Key([mod], "backslash", floating_border_window(1)),
    Key([mod, shift], "backslash", floating_border_window(2)),
    Key([mod, ctrl], "backslash", floating_border_window(3)),
    Key([mod, ctrl, shift], "backslash", floating_border_window(4)),
    Key([mod], "a", lazy.next_screen()),
    Key([mod, shift], "a", lazy.function(window_to_next_screen, switch_screen=True)),
    Key(
        [mod],
        "s",
        toggle_sticky_windows(),
        desc="Toggle state of sticky for current window",
    ),
    Key([mod], "c", lazy.screen.togglegroup()),
    Key([mod], "z", lazy.window.move_to_top()),
    Key([mod, shift], "z", lazy.window.move_to_bottom()),
    Key([mod], "x", lazy.spawn("alm -d")),
    Key([mod], "bracketright", lazy.screen.next_group(skip_empty=True), desc="Cycle Forward to Active Groups",),
    Key([mod], "bracketleft", lazy.screen.prev_group(skip_empty=True), desc="Cycle Backward to Active Groups",),
    Key([mod, shift], "bracketleft", lazy.window.move_down()),
    Key([mod, shift], "bracketright", lazy.window.move_up()),
    #
    Key([mod], "t", lazy.window.toggle_minimize(), desc="Toggle Minimize"),
    Key(
        [mod],
        "g",
        lazy.group["scratchpad"].toscreen(toggle=True),
        desc="Toggle scratchpad group",
    ),
    Key(
        [mod, "shift"],
        "g",
        lazy.window.togroup("scratchpad"),
        desc="Move Window to scratchpad",
    ),
    #
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    Key([mod, shift], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key(
        [mod, shift],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, shift], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, shift], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows
    Key([mod, ctrl], "h", lazy.layout.shrink(), desc="Grow window to the right"),
    Key([mod, ctrl], "l", lazy.layout.grow(), desc="Grow window to the left"),
    Key([mod, ctrl], "j", lazy.layout.shrink_down(), desc="Grow window down"),
    Key([mod, ctrl], "k", lazy.layout.shrink_up(), desc="Grow window up"),
    Key([mod], "space", lazy.function(swap_focus_main)),
    Key([mod, ctrl], "space", lazy.function(focus_main)),
    Key(
        [mod, shift],
        "space",
        lazy.window.toggle_floating(),
        desc="Toggle Current Window Floating",
    ),
    Key([mod,alt], "space", lazy.layout.flip(), desc="Flip windows"),
    Key([mod], "u", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod], "Tab", lazy.screen.toggle_group(), desc="Reset all window sizes"),
    # Key([mod], "Tab", focus_previous_window()),
    Key([mod, shift], "Tab", lazy.run_extension(WindowList(**dmenu_defaults))),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with multiple stack panes
    # Key([mod, ctrl], "space", lazy.layout.toggle_split(), desc="Toggle split sides of stack"),
    Key([mod], "q", lazy.window.kill(), desc="Kill Focused Window"),
    Key([mod], "o", float_cycle_forward()),
    Key([mod, shift], "o", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod, ctrl], "o", window_opacity("set", 1)),
    Key([mod, shift, ctrl], "o", window_opacity("inc")),
    Key([mod], "i", float_cycle_backward()),
    Key([mod, shift], "i", lazy.prev_layout(), desc="Toggle between layouts"),
    Key([mod, ctrl], "i", window_opacity("set", 0.1)),
    Key([mod, shift, ctrl], "i", window_opacity("dec")),
    Key([mod, ctrl], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, ctrl], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "backspace", lazy.spawn("sysact Shutdown"), desc="Shutdown"),
    Key([mod, shift], "backspace", lazy.spawn("sysact"), desc="System Actions"),
    Key([mod, ctrl], "backspace", lazy.spawn("sysact Suspend"), desc="Suspend"),
    Key([mod], "f", lazy.window.toggle_maximize(), desc="Toggle maximize"),
    Key(
        [mod, shift],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="Toggle Current Window Fullscreen",
    ),
    Key([mod, ctrl], "f", float_to_front()),
    # Key([mod, alt], "f", focus_floating_window()),
    Key([mod], "v", restore_merge_all_groups()),
    Key([mod], "b", lazy.hide_show_bar("top"), desc="Toggle Top Bar"),
    # Key([mod, alt], "a", add_treetab_section, desc='Prompt to add new section in treetab'),
    Key(
        [mod, alt],
        "h",
        lazy.layout.shuffle_left(),
        lazy.layout.move_left().when(layout=["treetab"]),
        desc="Move window to the left/move tab left in treetab",
    ),
    Key(
        [mod, alt],
        "l",
        lazy.layout.shuffle_right(),
        lazy.layout.move_right().when(layout=["treetab"]),
        desc="Move window to the right/move tab right in treetab",
    ),
    Key(
        [mod, alt],
        "j",
        lazy.layout.shuffle_down(),
        lazy.layout.section_down().when(layout=["treetab"]),
        desc="Move window down/move down a section in treetab",
    ),
    Key(
        [mod, alt],
        "k",
        lazy.layout.shuffle_up(),
        lazy.layout.section_up().when(layout=["treetab"]),
        desc="Move window downup/move up a section in treetab",
    ),
    Key([alt], "comma", lazy.prev_screen()),
    Key([alt], "period", lazy.next_screen()),
    # Applications
    Key([mod], "grave", lazy.spawn("dunstctl close"), desc="Dunst Close"),
    Key(
        [mod, shift],
        "grave",
        lazy.spawn("dunstctl history-pop"),
        desc="Dunst History Pop",
    ),
    Key(
        [mod, shift],
        "Return",
        lazy.spawn(terminal + " -e tmux"),
        desc="Launch Terminal",
    ),
    Key([mod], "w", lazy.spawn(browser), desc="Launch Browser"),
    Key([mod, shift], "w", lazy.spawn(browser_alt), desc="Launch Alternative Browser"),
    Key([mod, ctrl], "w", lazy.spawn(browser_priv), desc="Launch Private Browser"),

    Key([mod, shift], "b", lazy.spawn("bm S"), desc="Bookmarks"),
    Key([mod, ctrl], "b", lazy.spawn("bm d"), desc="Bookmarks"),

    Key([mod], "e", lazy.spawn(text_editor), desc="Launch Text Editor"),
    Key(
        [mod, shift],
        "e",
        lazy.spawn(terminal + " -e emacsclient -c"),
        desc="Launch Text Editor",
    ),
    # Key([mod], "d", lazy.spawn("dmenu_run"), desc="Dmenu Run History Prompt"),
    Key(
        [mod],
        "d",
        lazy.run_extension(DmenuRun(**dmenu_defaults)),
        desc="Windows Selector",
    ),
    Key([mod, shift], "d", lazy.spawn("via -r"), desc="Document Search"),
    Key([mod, ctrl], "t", lazy.spawn("switch-theme"), desc="Global Theme Toggle"),
    Key([mod, shift], "r", lazy.spawn("via -a"), desc="Global Search"),
    Key([mod, ctrl], "d", lazy.spawncmd(launcher), desc="Run Prompt"),
    Key([mod], "equal", lazy.spawn("dmenuunicode"), desc="Unicode Search Prompt"),
    Key([mod], "Insert", lazy.spawn("clipmenu -i"), desc="Clipmenu Prompt"),
    Key([mod, shift], "Insert", lazy.spawn("clipboard-content.sh")),
    Key([mod], "F12", lazy.spawn("maimpick"), desc="MaimPick Prompt"),
    Key(
        [mod, shift], "F12", lazy.spawn("maim ~/Storage/F$(date '+%y%m%d-%H%M-%S').png")
    ),
    Key([], "XF86Favorites", lazy.spawn("bm s"), desc="Save bookmark script"),
    # Key([], "XF86Search", lazy.spawn("ferdium"), desc="Emails & Chatting"),
    Key([], "XF86Explorer", lazy.screen.toggle_group(), desc="Switch to group 1"),
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
    Key([], "XF86Calculator", lazy.group["scratchpad"].dropdown_toggle("calculator")),
    Key([], "XF86HomePage", lazy.group["scratchpad"].dropdown_toggle("btop")),
    Key([mod], "XF86HomePage", lazy.group["scratchpad"].dropdown_toggle("htop")),
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
    Key([mod], "p", lazy.spawn("dmpv append"), desc="Dmpv append"),
    Key([mod, shift], "p", lazy.spawn("dmpv aplay "), desc="Dmpv Prompt"),
    Key([mod, ctrl], "p", lazy.spawn("dmpv"), desc="Dmpv Prompt"),
    Key([mod], "comma", lazy.spawn("dmpc toggle"), desc="Toggle Music"),
    Key([mod], "period", lazy.spawn("tppctl invert"), desc="Inverte Playing MPVs"),
    Key([mod, shift], "period", lazy.spawn("tppctl toggle"), desc="Toggle MPVs"),
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
    Key([mod, ctrl], "period", lazy.spawn("tppctl pauseall"), desc="Pause All MPVs"),
    Key([mod], "semicolon", focus_previous_window(), desc="Switch to last group"),
    Key(
        [mod, shift], "semicolon", lazy.group.focus_back(), desc="Switch to last group"
    ),
    # Scratchpads
    Key([], "XF86Launch7", lazy.group["scratchpad"].dropdown_toggle("mixer")),
    Key([mod], "Return", lazy.group["scratchpad"].dropdown_toggle("Dropdown")),
    Key([mod], "Escape", lazy.group["scratchpad"].dropdown_toggle("btop")),
    Key([mod, shift], "Escape", lazy.group["scratchpad"].dropdown_toggle("btop")),
    # Key([mod], "e", lazy.group["scratchpad"].dropdown_toggle("agenda")),
    Key([mod], "r", lazy.group["scratchpad"].dropdown_toggle("file manager")),
    # Key([mod], "c", lazy.group["scratchpad"].dropdown_toggle("mpvfloat")),
    Key([mod], "n", lazy.group["scratchpad"].dropdown_toggle("news")),
    Key([mod, shift], "n", lazy.group["scratchpad"].dropdown_toggle("podcasts")),
    Key([mod], "m", lazy.group["scratchpad"].dropdown_toggle("music")),
    Key([mod, shift], "m", lazy.group["scratchpad"].dropdown_toggle("spotify")),
    Key([mod], "apostrophe", lazy.group["scratchpad"].dropdown_toggle("calculator")),
    Key([mod], "y", lazy.group["scratchpad"].dropdown_toggle("qtile_shell")),
    Key([], "XF86Mail", lazy.group["scratchpad"].dropdown_toggle("mails")),
    # Hardware/system control
    Key([mod], "up", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+")),
    Key([mod, shift], "up", lazy.group["scratchpad"].dropdown_toggle("mixer")),
    Key([mod, ctrl], "up", lazy.spawn("output-audio")),
    Key(
        [],
        "XF86AudioRaiseVolume",
        lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"),
    ),
    Key([mod], "down", lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-")),
    Key([mod, shift], "down", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")),
    Key(
        [],
        "XF86AudioLowerVolume",
        lazy.spawn("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%-"),
    ),
    Key([mod], "XF86AudioMute", lazy.spawn("output-audio")),
    Key([], "XF86AudioMute", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")),
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
# }}}
# ======================= Groups ============= {{{
def dr_key_binder(mod, keynames=None):
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
            key = Key([mod], keyname, lazy.group[name].toscreen())
            key_s = Key([mod, shift], keyname, lazy.window.togroup(name))
            key_c = Key([mod, ctrl], keyname, merge_groups(int(name)))
            dgroup.keys.extend([key, key_s,key_c])
            dgroup.qtile.config.keys.extend([key, key_s, key_c])
            dgroup.qtile.grab_key(key)
            dgroup.qtile.grab_key(key_s)
            dgroup.qtile.grab_key(key_c)

    return func

dgroups_key_binder = dr_key_binder(mod)
# https://github.com/qtile/qtile/issues/1271
# groups = [Group(i) for i in "1234567890"]

# if len(screens) == 2:
#     for i in groups:
#         keys.extend([
#             # Switch to group N
#             Key(
#                 [ctrl],
#                 i.name,
#                 lazy.to_screen(0) if i.name in '12345' else lazy.to_screen(1),
#                 lazy.group[i.name].toscreen()
#             ),

#             # Move window to group N
#             Key([ctrl, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),),
#         ])

# else:
#     for i in groups:
#         keys.extend([
#             # Switch to group N
#             Key([ctrl], i.name, lazy.group[i.name].toscreen()),

#             # Move window to group N
#             Key([ctrl, "shift"], i.name, lazy.window.togroup(i.name, switch_group=True),),
#         ])

layout_defaults = dict(
    border_focus=colors[7],
    border_normal=colors[8],
    border_width=1,
    margin=8,
)
layouts = [
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
            Match(wm_class="fzfmenu"),
            # Match(wm_instance_class="mpvFloat"),
            # Match(title="Eagenda"),
            Match(title="branchdialog"),  # gitk
            Match(wm_class="pinentry-gtk-2"),  # GPG key password entry
            Match(wm_class="pinentry-qt"),  # GPG key password entry
            Match(title="pinentry"),  # GPG key password entry
            Match(func=lambda c: c.has_fixed_size()),
            Match(func=lambda c: c.has_fixed_ratio()),
            Match(wm_class='confirm'),
            Match(wm_class='dialog'),
            Match(wm_class='download'),
            Match(wm_class='error'),
            Match(wm_class='file_progress'),
            Match(wm_class='notification'),
            Match(wm_class='splash'),
            Match(wm_class='toolbar'),
            Match(wm_class='confirmreset'),
            Match(wm_class='makebranch'),
            Match(wm_class='maketag'),
            Match(title='branchdialog'),
            Match(title='Xephyr on :1.0 (ctrl+shift grabs mouse and keyboard)'),
            Match(title='Bitwarden'),
            Match(wm_class='nextcloud'),
            Match(wm_class='system-config-printer'),
        ],
    ),
]
# }}}
# ======================= Bar & Widgets ============= {{{
widget_defaults = dict(
    font="DaddyTimeMono Nerd Font Bold",
    # font="VictorMono Nerd Font Bold",
    fontsize=13,
    padding=4,
    background=colors[0],
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        wallpaper="~/.local/share/bg",
        wallpaper_mode="stretch",
        top=bar.Bar(
            [
                widget.GroupBox(
                    padding=4,
                    active=colors[2],
                    inactive=colors[1],
                    highlight_color=[backgroundColor, workspaceColor],
                    this_screen_border=colors[2],
                    this_current_screen_border=colors[2],
                    other_screen_border=colors[2],
                    other_current_screen_border=colors[2],
                    highlight_method="line",
                    hide_unused=True,
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=backgroundColor,
                    foreground=workspaceColor,
                ),
                widget.CurrentLayout(
                    background=workspaceColor,
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=workspaceColor,
                    foreground=backgroundColor,
                ),
                widget.Spacer(
                    length=20,
                ),
                widget.WindowCount(foreground=colors[7]),
                widget.WindowName(
                    foreground=colors[8],
                ),
                # widget.WindowTabs(
                #     selected=('<u>« ',' »</u>'),
                #     separator='      ',
                #     foreground=colors[8],
                #     # background=colors[2],
                # ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=backgroundColor,
                    foreground=foregroundColorTwo,
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=foregroundColorTwo,
                    foreground=foregroundColorTwo,
                ),
                widget.Prompt(
                    bell_style="visual",
                    cursorblink=0.5,
                    fontsize=18,
                    prompt="&:",
                    foreground=colors[9],
                    background=foregroundColorTwo,
                ),
                # widget.GenPollText( # sb-music
                #     update_interval=10,
                #     func=lambda: subprocess.check_output(os.path.expanduser("~/.local/bin/statusbar/sb-music")),
                # ),
                # widget.Mpd2(
                #     status_format='{play_status} [{artist:.15s}]-[{album:.15s}]-[{title:.30s}]',
                #     idle_message="mpd",
                #     color_progress="#9497aa",
                #     update_interval=10,
                #     foreground=colors[9],
                #     background=foregroundColorTwo,
                # ),
                widget.CPU(
                    format="{load_percent:-2.1f}% {freq_current}GHz",
                    update_interval=3,
                    foreground=colors[5],
                    background=foregroundColorTwo,
                ),
                widget.Load(
                    update_interval=3,
                    format="{time}:{load:.2f}",
                    foreground=colors[4],
                    background=foregroundColorTwo,
                ),
                widget.ThermalSensor(
                    update_interval=30,
                    format=" {temp:.0f}{unit}",
                    tag_sensor="Package id 0",
                    foreground=colors[6],
                    background=foregroundColorTwo,
                ),
                widget.NvidiaSensors(
                    update_interval=30,
                    # format=' {temp:.0f}{unit}',
                    gpu_bus_id="01:00.0",
                    tag_sensor="Package id 0",
                    foreground=colors[6],
                    background=foregroundColorTwo,
                ),
                widget.Memory(
                    update_interval=15,
                    format=" {MemUsed:.0f}{ms}/{SwapUsed:.0f}{ms}",
                    foreground=colors[7],
                    background=foregroundColorTwo,
                ),
                # widget.Wlan(
                #     disconnected_message='❌',
                #     format='{essid} {percent:2.0%}',
                #     update_interval=10,
                #     foreground=colors[4],
                #     background=foregroundColorTwo,
                # ),
                widget.Volume(
                    fmt=" {}",
                    update_interval=1,
                    step=5,
                    foreground=colors[8],
                    background=foregroundColorTwo,
                    # fmt=" {}"
                ),
                # widget.Net(
                #     fmt=' {}',
                #     update_interval=2,
                #     foreground=colors[9],
                #     background=foregroundColorTwo,
                # ),
                # widget.Backlight(
                #     update_interval = 15,
                #     foreground=colors[8],
                #     background=foregroundColorTwo,
                #     ),
                # widget.Battery(
                #     update_interval=15,
                #     format='{char} {percent:2.0%} {hour:d}:{min:02d} {watt:.2f} W',
                #     low_precentage=0.35,
                #     charge_char='',
                #     discharge_char='',
                #     empty_char='',
                #     foreground=colors[6],
                #     background=foregroundColorTwo,
                #      ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=foregroundColorTwo,
                    foreground=workspaceColor,
                ),
                widget.Systray(background=workspaceColor, foreground=backgroundColor),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=workspaceColor,
                    foreground=backgroundColor,
                ),
                widget.Clock(
                    format=" %V %a %d/%B/%y",
                    update_interval=86400,
                    foreground=colors[10],
                    background=backgroundColor,
                ),
                widget.Clock(
                    format=" %T",
                    update_interval=1,
                    foreground=colors[2],
                    background=backgroundColor,
                ),
                widget.Pomodoro(
                    update_interval=1,
                    prefix_active=" ",
                    prefix_break="🛑",
                    prefix_long_break="🛑🛑",
                    prefix_inactive="🔃",
                    # prefix_paused=️'',
                    foreground=colors[1],
                    background=backgroundColor,
                    color_active=colors[4],
                    color_inactive=colors[6],
                ),
            ],
            20,
        ),
    ),
    Screen(
        wallpaper="~/.local/share/bg",
        wallpaper_mode="stretch",
        top=bar.Bar(
            [
                widget.GroupBox(
                    padding=4,
                    active=colors[2],
                    inactive=colors[1],
                    highlight_color=[backgroundColor, workspaceColor],
                    this_screen_border=colors[2],
                    this_current_screen_border=colors[2],
                    other_screen_border=colors[2],
                    other_current_screen_border=colors[2],
                    highlight_method="line",
                    hide_unused=True,
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=backgroundColor,
                    foreground=workspaceColor,
                ),
                widget.CurrentLayout(
                    scale=0.7,
                    background=workspaceColor,
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=workspaceColor,
                    foreground=backgroundColor,
                ),
                widget.WindowCount(foreground=colors[7]),
                widget.WindowName(
                    foreground=colors[8],
                ),
                # widget.WindowTabs(
                #     selected=('<u>« ',' »</u>'),
                #     separator='      ',
                #     foreground=colors[8],
                #     # background=colors[2],
                # ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=backgroundColor,
                    foreground=foregroundColorTwo,
                ),
                # widget.Mpd2(
                #     status_format='{play_status} [{artist:.15s}]-[{album:.15s}]-[{title:.30s}]',
                #     idle_message="mpd",
                #     color_progress="#9497aa",
                #     update_interval=10,
                #     foreground=colors[9],
                #     background=foregroundColorTwo,
                # ),
                # widget.Net(
                #     fmt=' {}',
                #     update_interval=2,
                #     foreground=colors[9],
                #     background=foregroundColorTwo,
                # ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=14,
                    background=foregroundColorTwo,
                    foreground=foregroundColorTwo,
                ),
                widget.Prompt(
                    bell_style="visual",
                    cursorblink=0.5,
                    fontsize=18,
                    prompt="&:",
                    foreground=colors[9],
                    background=foregroundColorTwo,
                ),
                # widget.Countdown(
                #     # format='{D}d {H}h {M}m {S}s',
                #     date=datetime(2023, 10, 5, 0, 0, 0, 0),
                #     foreground=colors[3],
                #     background=foregroundColorTwo,
                #     color_active=colors[4],
                #     color_inactive=foregroundColorTwo
                # ),
                # widget.Backlight(
                #     update_interval = 15,
                #     foreground=colors[8],
                #     background=foregroundColorTwo,
                #     ),
                # widget.Battery(
                #     update_interval=15,
                #     format='{char} {percent:2.0%} {hour:d}:{min:02d} {watt:.2f} W',
                #     low_precentage=0.35,
                #     charge_char='',
                #     discharge_char='',
                #     empty_char='',
                #     foreground=colors[6],
                #     background=foregroundColorTwo,
                #      ),
                # widget.HDDBusyGraph(
                #     frequency=5,
                #     start_pos='top',
                #     graph_color=colors[8],
                #     background=foregroundColorTwo,
                # ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=foregroundColorTwo,
                    foreground=backgroundColor,
                ),
                widget.Clipboard(
                    # update_interval=1,
                    foreground=colors[3],
                    background=backgroundColor,
                    color_active=colors[4],
                    color_inactive=foregroundColorTwo,
                ),
            ],
            20,
        ),
    ),
]
# }}}
# ======================= Misc ============= {{{
auto_fullscreen = True
auto_minimize = True
bring_front_click = "floating_only"
cursor_warp = False
dgroups_app_rules = []  # type: List
floats_kept_above = True
focus_on_window_activation = "smart"
follow_mouse_focus = True
reconfigure_screens = True
wmname = "LG3D"
# }}}
# ======================= Autostart ============= {{{
# import os
# import subprocess

# Programms to start on log in
# @hook.subscribe.startup_once
# def autostart ():
#     home = os.path.expanduser('~/.config/qtile/autostart.sh')
#     subprocess.call([home])


# @hook.subscribe.startup_once
# def startup_once():
#     subprocess.Popen(["nm-applet"])
# }}}
