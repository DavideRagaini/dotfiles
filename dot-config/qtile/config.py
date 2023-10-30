# ======================= TODOs ============= {{{
# TODO Keychord to resize floating windows
# TODO when group switch focus, then unfocus floating
# }}}
# ======================= Imports ============= {{{
from typing import List

from libqtile import bar, hook, layout, widget
from libqtile.config import (Click, Drag, DropDown, Group, Key, Match,
                             ScratchPad, Screen)
from libqtile.dgroups import simple_key_binder
from libqtile.extension import WindowList
from libqtile.lazy import lazy

from re import compile as regex
from datetime import datetime
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

browser = "thorium"
browser_alt = "qutebrowser"
browser_priv = "librewolf --private-window"
terminal = "alacritty"
text_editor = terminal + " --class 'emacs,emacs' -T 'alacritty-emacsclient' -e emacsclient -nw"
file_manager = terminal + " -e tmux new-session -A -s 'files'"
launcher = "run"
process_viewer = terminal + " -e htop"
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


colorscheme = dracula()
(
    colors,
    backgroundColor,
    foregroundColor,
    workspaceColor,
    foregroundColorTwo,
) = colorscheme
# }}}
# ======================= Groups ============= {{{
#  󰇮   󰈙     
groups = [
    Group(
        name="1",
        position=1,
        layout="max",
        exclusive=True,
        matches=[
            Match(wm_instance_class="emacs"),
            Match(wm_class = "Alacritty"),
        ],
    ),
    Group(
        name="2",
        position=2,
        layout="max",
        exclusive=True,
        matches=Match(wm_class= [
                       "librewolf",
                       "LibreWolf",
                       "Brave-browser",
                       "Thorium-browser"
                    ])
    ),
    Group(
        name="3",
        position=3,
        layout="treetab",
        exclusive=True,
        matches=Match(wm_class= [
                        "Zathura",
                        "Evince",
                        "okular",
                        "ebook-viewer",
                        "Nsxiv",
                    ])
    ),
    Group(
        name="4",
        layout="max",
        position=4,
        matches=[
            Match(wm_class= [
                        "VirtualBox Machine",
                        regex("MATLAB R202[0-9][a-b] - academic use"),
                        regex("MATLAB R202[0-9][a-b]"),
                        "MATLABWindow",
                        "Matlab-GLEE",
            ],
                    wm_instance_class = ["sun-awt-X11-XDialogPeer"],
                  ),
            Match(
                    wm_instance_class = "sun-awt-X11-XDialogPeer",
                    title="MATLAB Editor",
            ),
            Match(
                    wm_instance_class = "Matlab-GLEE",
                    wm_class = regex("MATLAB R202[0-9][a-b] Update [0-9]+"),
            ),
            Match(
                    wm_instance_class="sun-awt-X11-XFramePeer",
                    title = regex("MATLAB R202[0-9][a-b] - academic use"),
            ),
            Match(
                    title = "MathWorks Product Installer",
            ),
            Match(
                    wm_class="MATLABWindow",
                    wm_instance_class="MATLABWindow",
                    title="Control System Designer",
            ),
        ],
    ),
    Group(
        name="5",
        position=5,
        layout="treetab",
        matches=[
            Match(wm_instance_class = ["sun-awt-X11-XFramePeer"]
                  ),
            Match(
                    wm_instance_class = "sun-awt-X11-XFramePeer",
                    title = " ",
                ),
            ],
    ),
    Group(
        name="6",
        position=6,
        layout="ratiotile",
        matches=[
            Match(
                wm_instance_class = "MATLABWindow",
                wm_class = "MATLABWindow",
                title = "Variable-references - Signal Editor",
            ),
            Match(
                title = "asbQuadcopter/Command/Signal Editor * - Simulink academic use",
                wm_instance_class = "Matlab-GLEE",
                wm_class = regex("MATLAB R202[0-9][a-b] Update [0-9]+"),
            ),
            Match(
                title = "Block Parameters: Position/Attitude Reference",
                wm_instance_class = "Matlab-GLEE",
                wm_class = regex("MATLAB R202[0-9][a-b] Update [0-9]+"),
            ),
            Match(
                wm_instance_class = "MATLABWindow",
                wm_class =  "MATLABWindow",
                title = "Variable-references - Signal Editor"
            ),
            Match(
                title = "Block Parameters: Position/Attitude Reference",
                wm_instance_class = "Matlab-GLEE",
                wm_class = regex("MATLAB R202[0-9][a-b] Update [0-9]+"),
            ),
        ],
    ),
    Group(
        name="7",
        position=7,
        layout="max",
        matches=Match(wm_class=[
                        "KeePassXC",
                        "qBittorrent",
                        'calibre-gui',
                        ]),
    ),
    Group(
        name="8",
        position=8,
        layout="max",
        matches=Match(wm_class=[
                        "qutebrowser",
                        'teams-for-linux',
                        'microsoft teams - preview',
                        'Ferdium',
                        ]),
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
        layout="max",
    ),
]
# }}}
# ======================= Functions ============= {{{
## }}}
# ======================= Hooks ============= {{{
@lazy.function
def floating_bottom_right_window(qtile, window=None):
    if window is None:
        window = qtile.current_screen.group.current_window
        window.toggle_floating()
        window.place(1597,897,320,180,1,'#FF00FF',True,None,True)
    return window

@lazy.function
def floating_top_right_window(qtile, window=None):
    if window is None:
        window = qtile.current_screen.group.current_window
        window.toggle_floating()
        window.place(1597,20,320,180,1,'#FF00FF',True,None,True)
    return window

@lazy.function
def floating_bottom_left_window(qtile, window=None):
    if window is None:
        window = qtile.current_screen.group.current_window
        window.toggle_floating()
        window.place(0,897,320,180,1,'#FF00FF',True,None,True)
    return window

@lazy.function
def floating_top_left_window(qtile, window=None):
    if window is None:
        window = qtile.current_screen.group.current_window
        window.toggle_floating()
        window.place(5,20,320,180,1,'#FF00FF',True,None,True)
    return window


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


@lazy.function
def float_to_front(qtile):
    for group in qtile.groups:
        for window in group.windows:
            if window.floating:
                window.cmd_bring_to_front()


floating_window_index = 0
def float_cycle(qtile, forward: bool):
    global floating_window_index
    floating_windows = []
    for window in qtile.current_group.windows:
        if window.floating:
            floating_windows.append(window)
    if not floating_windows:
        return
    floating_window_index = min(floating_window_index, len(floating_windows) -1)
    if forward:
        floating_window_index += 1
    else:
        floating_window_index += 1
    if floating_window_index >= len(floating_windows):
        floating_window_index = 0
    if floating_window_index < 0:
        floating_window_index = len(floating_windows) - 1
    win = floating_windows[floating_window_index]
    win.cmd_bring_to_front()
    win.cmd_focus()

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


def window_to_next_screen(qtile, switch_group=False, switch_screen=False):
    i = qtile.screens.index(qtile.current_screen)
    if i != 0:
        group = qtile.screens[i-1].group.name
        qtile.current_window.togroup(group, switch_group=switch_group)
        if switch_screen == True:
            qtile.cmd_to_screen(i-0)
    elif i + 1 != len(qtile.screens):
        group = qtile.screens[i + 1].group.name
        qtile.current_window.togroup(group, switch_group=switch_group)
        if switch_screen == True:
            qtile.cmd_to_screen(i + 1)

# Allows you to input a name when adding treetab section.
# @lazy.layout.function
# def add_treetab_section(layout):
#     prompt = qtile.widgets_map["prompt"]
#     prompt.start_input("Section name: ", layout.cmd_add_section)

# @hook.subscribe.client_new
# def disable_floating(window):
#     rules = [
#         Match(wm_class="mpv")
#     ]

#     if any(window.match(rule) for rule in rules):
#         window.togroup(qtile.current_group.name)
#         window.cmd_disable_floating()
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
                on_focus_lost_hide = False
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
                on_focus_lost_hide = False
            ),
            DropDown(
                "process_viewer",
                process_viewer,
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide = False
            ),
            DropDown(
                "news",
                terminal + " -e newsboat",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide = True
            ),
            DropDown(
                "podcasts",
                terminal + " -e podboat",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide = True
            ),
            DropDown(
                "music",
                terminal + " -e ncmpcpp",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide = True
            ),
            DropDown(
                "spotify",
                terminal + " -e spt",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide = True
            ),
            DropDown(
                "mixer",
                terminal + " -e pulsemixer",
                width=0.6,
                height=0.3,
                x=0.2,
                y=0.2,
                opacity=0.9,
                on_focus_lost_hide = True
            ),
            DropDown(
                "calculator",
                terminal + " -e wcalc -P -1 -c -q --ints -C -p -r --remember",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
                on_focus_lost_hide = False
            ),
            DropDown(
                "qtile_shell",
                terminal + " -e qtile shell",
                x=0.05,
                y=0.05,
                width=0.9,
                height=0.6,
                opacity=0.9,
                on_focus_lost_hide = False
            ),
            DropDown(
                "mails",
                "ferdium",
                width=0.9,
                height=0.9,
                x=0.04,
                y=0.04,
                opacity=0.9,
                on_focus_lost_hide = False
            ),
        ],
    )
)
# }}}
# ======================= Mouse ============= {{{
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod],"Button2", lazy.window.bring_to_front()),
]
# }}}
# ======================= Keybindings ============= {{{
keys = [
    Key([mod, ctrl], "1", lazy.group.setlayout('max')),
    Key([mod, ctrl], "2", lazy.group.setlayout('monadtall')),
    Key([mod, ctrl], "3", lazy.group.setlayout('treetab')),
    Key([mod, ctrl], "4", lazy.group.setlayout('bsp')),
    Key([mod, ctrl], "5", lazy.group.setlayout('matrix')),
    Key([mod, ctrl], "6", lazy.group.setlayout('ratiotile')),
    Key([mod, ctrl], "7", lazy.group.setlayout('tile')),
    Key([mod, ctrl], "8", lazy.group.setlayout('stack')),
    Key([mod, ctrl], "9", lazy.group.setlayout('monadwide')),
    Key([mod, ctrl], "0", lazy.group.setlayout('max')),
    # Switch between windows
    Key([mod], "backslash", floating_bottom_right_window()),
    Key([mod, shift], "backslash", floating_top_right_window()),
    Key([mod, ctrl], "backslash", floating_bottom_left_window()),
    Key([mod, shift, ctrl], "backslash", floating_top_left_window()),

    Key([mod], "a", lazy.next_screen()),
    Key([mod, shift], "a", lazy.function(window_to_next_screen, switch_screen=True)),
    Key([mod], "s", toggle_sticky_windows(), desc="Toggle state of sticky for current window",),
    # Key([mod], "z", lazy.screen.togglegroup()),

    Key([mod], "bracketright", lazy.screen.next_group(skip_empty=True), desc="Cycle Forward to Active Groups"),
    Key([mod], "bracketleft", lazy.screen.prev_group(skip_empty=True), desc="Cycle Backward to Active Groups"),
    #
    Key([mod, shift], "t", lazy.window.toggle_minimize(), desc="Toggle Minimize"),
    Key([mod], "g", lazy.group["scratchpad"].toscreen(toggle=True), desc="Toggle scratchpad group"),
    Key([mod, 'shift'], "g", lazy.window.togroup("scratchpad"), desc="Move Window to scratchpad"),
    #
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    Key([mod, shift], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, shift], "l", lazy.layout.shuffle_right(), desc="Move window to the right",),
    Key([mod, shift], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, shift], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows
    Key([mod, ctrl], "h", lazy.layout.shrink(), desc="Grow window to the right"),
    Key([mod, ctrl], "l", lazy.layout.grow(), desc="Grow window to the left"),
    Key([mod, ctrl], "j", lazy.layout.shrink_down(), desc="Grow window down"),
    Key([mod, ctrl], "k", lazy.layout.shrink_up(), desc="Grow window up"),
    Key([mod], "space", lazy.layout.flip(), desc="Flip windows"),
    Key([mod, shift], "space", lazy.window.toggle_floating(), desc="Toggle Current Window Floating"),
    Key([mod], "u", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod], "Tab", lazy.screen.toggle_group(), desc="Reset all window sizes"),
    # Key([mod], "Tab", focus_previous_window()),
    Key([mod, shift], "Tab",
        lazy.run_extension(WindowList(
            # item_format="{id}: {group} {window}",
                font="CaskaydiaCove Nerd Font Mono",
                fontsize=18,
                dmenu_ignorecase=True,
                # selected_background=colors[3]
            )), desc="Windows Selector"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with multiple stack panes
    # Key([mod, ctrl], "space", lazy.layout.toggle_split(), desc="Toggle split sides of stack"),
    Key([mod], "q", lazy.window.kill(), desc="Kill Focused Window"),
    Key([mod], "o", float_cycle_forward()),
    Key([mod, shift], "o", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "i", float_cycle_backward()),
    Key([mod, shift], "i", lazy.prev_layout(), desc="Toggle between layouts"),
    Key([mod, ctrl], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, ctrl], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "backspace", lazy.spawn("sysact Shutdown"), desc="Shutdown"),
    Key([mod, shift], "backspace", lazy.spawn("sysact"), desc="System Actions"),
    Key([mod, ctrl], "backspace", lazy.spawn("sysact Suspend"), desc="Suspend"),
    Key([mod], "f", lazy.window.toggle_maximize(), desc="Toggle maximize"),
    Key([mod, shift], "f", lazy.window.toggle_fullscreen(), desc="Toggle Current Window Fullscreen"),
    Key([mod, ctrl], "f", float_to_front()),
    # Key([mod, alt], "f", focus_floating_window()),
    Key([mod], "b", lazy.hide_show_bar("top"), desc="Toggle Top Bar"),

    # Key([mod, alt], "a", add_treetab_section, desc='Prompt to add new section in treetab'),
    Key([mod, alt], "h", lazy.layout.shuffle_left(), lazy.layout.move_left().when(layout=["treetab"]),
        desc="Move window to the left/move tab left in treetab"),
    Key([mod, alt], "l", lazy.layout.shuffle_right(), lazy.layout.move_right().when(layout=["treetab"]),
        desc="Move window to the right/move tab right in treetab"),
    Key([mod, alt], "j", lazy.layout.shuffle_down(), lazy.layout.section_down().when(layout=["treetab"]),
        desc="Move window down/move down a section in treetab"),
    Key([mod, alt], "k", lazy.layout.shuffle_up(), lazy.layout.section_up().when(layout=["treetab"]),
        desc="Move window downup/move up a section in treetab"),
    # EzKey("M-<comma>", lazy.prev_screen()),
    # EzKey("M-<period>", lazy.next_screen()),

    # Applications
    Key([mod], "grave", lazy.spawn("dunstctl close"), desc="Dunst Close"),
    Key([mod, shift], "grave", lazy.spawn("dunstctl history-pop"), desc="Dunst History Pop"),
    Key([mod, shift], "Return", lazy.spawn(terminal + " -e tmux"), desc="Launch Terminal"),
    Key([mod], "w", lazy.spawn(browser), desc="Launch Browser"),
    Key([mod, shift], "w", lazy.spawn(browser_alt), desc="Launch Alternative Browser"),
    Key([mod, ctrl], "w", lazy.spawn(browser_priv), desc="Launch Private Browser"),
    Key([mod], "e", lazy.spawn(text_editor), desc="Launch Text Editor"),
    Key([mod, shift], "e", lazy.spawn("alacritty -e emacsclient -c"), desc="Launch Text Editor"),
    Key([mod], "d", lazy.spawn("dmenu_run_history"), desc="Dmenu Run History Prompt"),
    Key([mod, shift], "d", lazy.spawn("via -r"), desc="Document Search"),
    Key([mod, ctrl], "t", lazy.spawn("switch-theme"), desc="Global Theme Toggle"),
    Key([mod, shift], "r", lazy.spawn("via -a"), desc="Global Search"),
    Key([mod, ctrl], "d", lazy.spawncmd(launcher), desc="Run Prompt"),
    Key([mod], "equal", lazy.spawn("dmenuunicode"), desc="Unicode Search Prompt"),
    Key([mod], "Insert", lazy.spawn("clipmenu -i"), desc="Clipmenu Prompt"),
    # Key([mod, shift], "Insert",
    #     lazy.spawn('notify-send " Clipboard contents:" "$(xclip -o -selection clipboard)"'),
    #     desc="Clipboard Current"),
    Key([mod], "F12", lazy.spawn("maimpick"), desc="MaimPick Prompt"),
    Key([mod, shift], "F12",
        lazy.spawn('maim ~/Storage/F$(date \'+%y%m%d-%H%M-%S\').png'),
        desc="Maim Fullscreen Screenshot"),
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
    Key([], "XF86HomePage", lazy.group["scratchpad"].dropdown_toggle("process_viewer")),
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

    Key([mod], "p", lazy.spawn("dmpv2"), desc="Dmpv2"),
    Key([mod, shift], "p", lazy.spawn("dmpv"), desc="Dmpv Prompt"),
    Key([mod], "comma", lazy.spawn("dmpc toggle"), desc="Toggle Music"),
    Key([mod], "period", lazy.spawn("tppctl invert"), desc="Inverte Playing MPVs"),
    Key([mod, shift], "period", lazy.spawn("tppctl toggle"), desc="Toggle MPVs"),
    # Key([mod, shift], "m", lazy.spawn("mp down"), desc="Kill Spotify Music"),
    Key([mod, ctrl], "period", lazy.spawn("tppctl pauseall"), desc="Pause All MPVs"),
    Key([mod], "semicolon", focus_previous_window(), desc="Switch to last group"),
    Key([mod, shift], "semicolon", lazy.group.focus_back(), desc="Switch to last group"),
    # Scratchpads
    Key([], "XF86Launch7", lazy.group["scratchpad"].dropdown_toggle("mixer")),
    Key([mod], "Return", lazy.group["scratchpad"].dropdown_toggle("Dropdown")),
    Key([mod], "Escape", lazy.group["scratchpad"].dropdown_toggle("process_viewer")),
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
    Key([mod], "up", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")),
    Key([mod, shift], "up", lazy.spawn("output-audio")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")),
    Key([mod], "down", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")),
    Key([mod, shift], "down", lazy.group["scratchpad"].dropdown_toggle("mixer")),
    Key([mod], "down", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")),
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

    # Key([mod], "kp-add", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")),
    # Key([mod], "kp-subtract", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")),
]
# }}}
# ======================= Groups ============= {{{
dgroups_key_binder = simple_key_binder(mod)

layout_defaults = dict(
    border_focus=colors[7],
    border_normal=colors[8],
)
layouts = [
    layout.Columns(**layout_defaults, border_width=2, margin=10),
    layout.Max(**layout_defaults, border_width=0, margin=0),
    layout.MonadTall(**layout_defaults, border_width=2, margin=10),
    layout.Bsp(**layout_defaults, border_width=2, margin=10),
    layout.RatioTile(**layout_defaults, border_width=2, margin=2),
    layout.TreeTab(
            **layout_defaults,
            active_bg=colors[3],
            active_fg = colors[5],
            bg_color=backgroundColor,
            fontsize = 11,
            inactive_bg="151515",
            inactive_fg = colors[2],
            level_shift = 8,
            padding_left = 4,
            padding_x = 4,
            padding_y = 2,
            panel_width = 250,
            place_right = False,
            previous_on_rm=True,
            section_fg = colors[2],
            section_fontsize = 10,
            section_left=100,
            section_top = 15,
            sections = ["I","II","III","IV","V","VI","VII","VIII","IX","X"],
    ),
    layout.Tile(**layout_defaults, border_width=2, margin=10),
    layout.Stack(**layout_defaults, border_width=2, margin=10,num_stacks=2),
    layout.Matrix(**layout_defaults, border_width=2, margin=10),
    layout.MonadWide(**layout_defaults, border_width=2, margin=10),
    # layout.VerticalTile(),
    # layout.Zoomy(columnwidth=500),
    layout.Floating(
        border_focus=colors[4],
        border_width = 2,
        float_rules=[
            *layout.Floating.default_float_rules,
            Match(wm_class="ssh-askpass"),  # ssh-askpass
            Match(wm_class="fzfmenu"),
            # Match(wm_instance_class="mpvFloat"),
            # Match(title="Eagenda"),
            Match(title="branchdialog"),  # gitk
            Match(wm_class='pinentry-gtk-2'), # GPG key password entry
            Match(wm_class='pinentry-qt'), # GPG key password entry
            Match(title="pinentry"),  # GPG key password entry
            Match(func=lambda c: c.has_fixed_size()),
            Match(func=lambda c: c.has_fixed_ratio()),
        ],
    ),
]
# }}}
# ======================= Bar & Widgets ============= {{{
widget_defaults = dict(
    font='3270 Nerd Font Mono Bold',
    fontsize=12,
    padding=4,
    background=colors[0]
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    padding=4,
                    active=colors[2],
                    inactive=colors[1],
                    highlight_color=[backgroundColor, workspaceColor],
                    highlight_method="line",
                    hide_unused=True,
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=60,
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
                widget.WindowCount(
                    foreground=colors[7]
                ),
                # widget.WindowName(
                #     foreground=colors[8],
                # ),
                widget.WindowTabs(
                    selected=('<u>« ',' »</u>'),
                    separator='      ',
                    foreground=colors[8],
                    # background=colors[2],
                ),
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
                    fontsize=14,
                    background=foregroundColorTwo,
                    foreground=foregroundColorTwo,
                ),
                widget.Prompt(
                    bell_style='visual',
                    cursorblink=0.5,
                    fontsize=18,
                    prompt='&:',
                    foreground=colors[9],
                    background=foregroundColorTwo
                ),
                # widget.Mpd2(
                #     status_format='{play_status} {artist}/{album}/{title}',
                #     idle_message="empty list",
                #     color_progress="#9497aa",
                #     foreground=colors[9],
                #     background=foregroundColorTwo,
                #     # update_interval=10
                # ),
                widget.CPU(
                    format="{load_percent:-2.1f}% {freq_current}GHz",
                    update_interval=3,
                    foreground=colors[5],
                    background=foregroundColorTwo
                ),
                widget.Load(
                    update_interval=3,
                    format="{time}:{load:.2f}",
                    foreground=colors[4],
                    background=foregroundColorTwo
                ),
                widget.ThermalSensor(
                    update_interval=30,
                    format=' {temp:.0f}{unit}',
                    tag_sensor='Package id 0',
                    foreground=colors[6],
                    background=foregroundColorTwo
                ),
                widget.NvidiaSensors(
                    update_interval=30,
                    # format=' {temp:.0f}{unit}',
                    gpu_bus_id='01:00.0',
                    tag_sensor='Package id 0',
                    foreground=colors[6],
                    background=foregroundColorTwo
                ),
                widget.Memory(
                    update_interval=15,
                    format=' {MemUsed:.0f}{ms}/{SwapUsed:.0f}{ms}',
                    foreground=colors[7],
                    background=foregroundColorTwo
                ),
                # widget.Wlan(
                #     disconnected_message='❌',
                #     format='{essid} {percent:2.0%}',
                #     update_interval=10,
                #     foreground=colors[4],
                #     background=foregroundColorTwo,
                # ),
                widget.Volume(
                    fmt=' {}',
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
                    foreground=workspaceColor
                ),
                widget.Systray(
                    background=workspaceColor,
                    foreground=backgroundColor
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=workspaceColor,
                    foreground=backgroundColor
                ),
                widget.Clock(
                    format=" %V %a %d/%B/%y",
                    update_interval=86400,
                    foreground=colors[10],
                    background=backgroundColor
                ),
                widget.Clock(
                    format=" %T",
                    update_interval=1,
                    foreground=colors[2],
                    background=backgroundColor
                ),
            ],
            20,
        ),
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    padding=4,
                    active=colors[2],
                    inactive=colors[1],
                    highlight_color=[backgroundColor, workspaceColor],
                    highlight_method="line",
                    hide_unused=True,
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=60,
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
                widget.WindowCount(
                    foreground=colors[7]
                ),
                # widget.WindowName(
                #     foreground=colors[8],
                # ),
                widget.WindowTabs(
                    selected=('<u>« ',' »</u>'),
                    separator='      ',
                    foreground=colors[8],
                    # background=colors[2],
                ),
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
                    fontsize=14,
                    background=foregroundColorTwo,
                    foreground=foregroundColorTwo,
                ),
                widget.Prompt(
                    bell_style='visual',
                    cursorblink=0.5,
                    fontsize=18,
                    prompt='&:',
                    foreground=colors[9],
                    background=foregroundColorTwo
                ),
                widget.Countdown(
                    # format='{D}d {H}h {M}m {S}s',
                    date=datetime(2023, 10, 5, 0, 0, 0, 0),
                    foreground=colors[3],
                    background=foregroundColorTwo,
                    color_active=colors[4],
                    color_inactive=foregroundColorTwo
                ),
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
                widget.HDDBusyGraph(
                    frequency=5,
                    start_pos='top',
                    graph_color=colors[8],
                    background=foregroundColorTwo,
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=foregroundColorTwo,
                    foreground=backgroundColor
                ),
                widget.Pomodoro(
                    # update_interval=1,
                    foreground=colors[3],
                    background=backgroundColor,
                    color_active=colors[4],
                    color_inactive=foregroundColorTwo
                ),
                widget.Clipboard(
                    # update_interval=1,
                    foreground=colors[3],
                    background=backgroundColor,
                    color_active=colors[4],
                    color_inactive=foregroundColorTwo
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
bring_front_click = 'floating_only'
cursor_warp = True
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
