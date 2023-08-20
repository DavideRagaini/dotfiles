# ======================= TODOs ============= {{{
# TODO Keychord to resize floating windows
# }}}
# ======================= Imports ============= {{{
from typing import List

from libqtile import bar, hook, layout, widget
from libqtile.config import (Click, Drag, DropDown, Group, Key, Match,
                             ScratchPad, Screen)
from libqtile.dgroups import simple_key_binder
from libqtile.extension import WindowList
from libqtile.lazy import lazy

# from libqtile.utils import guess_terminal
# from libqtile.widget import Spacer, Backlight
# from libqtile.widget.image import Image
# import os
# import re
# import socket
# import subprocess
# }}}
# ======================= Variables ============= {{{
mod = "mod4"
mod1 = "mod1"
browser = "librewolf"
browser_alt = "qutebrowser"
browser_priv = "librewolf --private-window"
terminal = "alacritty"
text_editor = "emacsclient -c"
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
        ["#f1fa8c", "#f1fa8c"],
    ]  # yellow [10]
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
        # label="1",
        layout="monadtall",
        exclusive=True,
        matches=[Match(wm_instance_class="emacs"),
                 Match(wm_class="Alacritty")],
    ),
    Group(
        name="2",
        position=2,
        # label="2",
        layout="monadtall",
        exclusive=True,
        matches=Match(wm_class= [
                       "qutebrowser",
                       "librewolf",
                       "LibreWolf"
                    ])
    ),
    Group(
        name="3",
        position=3,
        # label="3",
        layout="treetab",
        exclusive=True,
        matches=Match(wm_class= [
                        "Zathura",
                        "Evince",
                        "ebook-viewer",
                    ])
    ),
    Group(
        name="4",
        position=4,
        # label="4",
        layout="treetab",
        matches=Match(wm_class= [
                        "VirtualBox Machine",
                        "MATLAB R2021b - academic use",
                        "MATLAB R2023a - academic use",
                        "MATLAB R2023a",
                        "MATLABWindow",
                        "Matlab-GLEE",
                        "Nsxiv"])
    ),
    Group(
        name="5",
        position=5,
        # label="5",
        layout="bsp",
    ),
    Group(
        name="6",
        position=6,
        # label="6",
        layout="bsp",
    ),
    Group(
        name="7",
        position=7,
        # label="7",
        layout="max",
        matches=Match(wm_class=[
                        'calibre-gui',
                        'teams-for-linux',
                        'microsoft teams - preview',
                        'discord' ]),
    ),
    Group(
        name="8",
        position=8,
        # label="8",
        layout="max",
        matches=Match(wm_class=[
            'Transmission-remote-gtk',
            'Ferdium' ]),
    ),
    Group(
        name="9",
        position=9,
        # label="9",
        layout="max",
        # exclusive=True,
        matches=[Match(wm_class="mpv")],
    ),
]
# dgroups_key_binder = None
dgroups_key_binder = simple_key_binder(mod)
# }}}
# ======================= Functions ============= {{{
## }}}
# ======================= Hooks ============= {{{
# Programms to start on log in
# @hook.subscribe.startup_once
# def autostart ():
#     home = os.path.expanduser('~/.config/qtile/autostart.sh')
#     subprocess.call([home])


# @hook.subscribe.startup_once
# def startup_once():
#     subprocess.Popen(["nm-applet"])


@lazy.function
def floating_bottom_right_window(qtile, window=None):
    if window is None:
        window = qtile.current_screen.group.current_window
        window.toggle_floating()
        window.place(1597,897,320,180,1,'#FF00FF',True,None,True)
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
    # logging.info("bring floating windows to front")
    for group in qtile.groups:
        for window in group.windows:
            if window.floating:
                window.cmd_bring_to_front()


previous_focused = []
@hook.subscribe.client_focus
def client_focused(window):
    global previous_focused
    if len(previous_focused) < 2:
        previous_focused.append(window)
    elif previous_focused[1] != window:
        previous_focused[0] = previous_focused[1]
        previous_focused[1] = window
    # logger.info(f"FOCUSED {window}, {previous_focused}")


@lazy.function
def focus_previous_window(qtile):
    global previous_focused
    if len(previous_focused) == 2:
        group = previous_focused[0].group
        qtile.current_screen.set_group(group)
        # logger.info(f"FOCUS PREVIOUS {previous_focused[0]}")
        group.focus(previous_focused[0])


# @hook.subscribe.client_new
# def disable_floating(window):
#     rules = [
#         Match(wm_class="mpv")
#     ]

#     if any(window.match(rule) for rule in rules):
#         window.togroup(qtile.current_group.name)
#         window.cmd_disable_floating()
# }}}
# ======================= Floating Windows Rules ============= {{{
floating_layout = layout.Floating(
    border_focus=colors[4],
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class="fzfmenu"),
        # Match(wm_instance_class="mpvFloat"),
        # Match(title="Eagenda"),
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(func=lambda c: c.has_fixed_size()),
        Match(func=lambda c: c.has_fixed_ratio()),
    ],
)
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
    # Switch between windows
    Key([mod], "backslash", floating_bottom_right_window()),
    Key([mod], "s", toggle_sticky_windows(), desc="Toggle state of sticky for current window",),
    Key([mod], "z", lazy.screen.togglegroup()),
    #
    Key([mod], "bracketright", lazy.screen.next_group(skip_empty=True), desc="Cycle Forward to Active Groups"),
    Key([mod], "bracketleft", lazy.screen.prev_group(skip_empty=True), desc="Cycle Backward to Active Groups"),
    #
    Key([mod, "shift"], "t", lazy.window.toggle_minimize(), desc="Toggle Minimize"),
    Key([mod], "g", lazy.group["scratchpad"].toscreen(toggle=True), desc="Toggle scratchpad group"),
    Key([mod, 'shift'], "g", lazy.window.togroup("scratchpad"), desc="Move Window to scratchpad"),
    #
    Key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="Move focus up"),
    # Move windows between left/right columns or move up/down in current stack.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right",),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows
    Key([mod, "control"], "h", lazy.layout.shrink(), desc="Grow window to the right"),
    Key([mod, "control"], "l", lazy.layout.grow(), desc="Grow window to the left"),
    Key([mod, "control"], "j", lazy.layout.shrink_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.shrink_up(), desc="Grow window up"),
    Key([mod], "space", lazy.layout.flip(), desc="Flip windows"),
    Key([mod, "shift"], "space", lazy.window.toggle_floating(), desc="Toggle Current Window Floating"),
    Key([mod], "u", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod], "Tab", lazy.screen.toggle_group(), desc="Reset all window sizes"),
    # Key([mod], "Tab", focus_previous_window()),
    Key([mod, "shift"], "Tab",
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
    # Key([mod, "control"], "space", lazy.layout.toggle_split(), desc="Toggle split sides of stack"),
    Key([mod], "q", lazy.window.kill(), desc="Kill Focused Window"),
    Key([mod], "o", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "i", lazy.prev_layout(), desc="Toggle between layouts"),
    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "backspace", lazy.spawn("sysact Shutdown"), desc="Shutdown"),
    Key([mod, "shift"], "backspace", lazy.spawn("sysact"), desc="System Actions"),
    Key([mod], "f", lazy.window.toggle_maximize(), desc="Toggle maximize"),
    Key([mod, "shift"], "f", lazy.window.toggle_fullscreen(), desc="Toggle Current Window Fullscreen"),
    Key([mod, "control"], "f", float_to_front()),
    Key([mod], "b", lazy.hide_show_bar("top"), desc="Toggle Top Bar"),
    # EzKey("M-<comma>", lazy.prev_screen()),
    # EzKey("M-<period>", lazy.next_screen()),

    # Applications
    Key([mod], "grave", lazy.spawn("dunstctl close"), desc="Dunst Close"),
    Key([mod, "shift"], "grave", lazy.spawn("dunstctl history-pop"), desc="Dunst History Pop"),
    Key([mod, "shift"], "Return", lazy.spawn(terminal + " -e tmux"), desc="Launch Terminal"),
    Key([mod], "w", lazy.spawn(browser), desc="Launch Browser"),
    Key([mod, "shift"], "w", lazy.spawn(browser_alt), desc="Launch Alternative Browser"),
    Key([mod, "control"], "w", lazy.spawn(browser_priv), desc="Launch Private Browser"),
    Key([mod, "shift"], "e", lazy.spawn(text_editor), desc="Launch Text Editor"),
    Key([mod], "d", lazy.spawn("dmenu_run_history"), desc="Dmenu Run History Prompt"),
    Key([mod, "shift"], "d", lazy.spawn("via -r"), desc="Document Search"),
    Key([mod, "control"], "t", lazy.spawn("switch-theme"), desc="Global Theme Toggle"),
    Key([mod, "shift"], "r", lazy.spawn("via -a"), desc="Global Search"),
    Key([mod, "control"], "d", lazy.spawncmd(launcher), desc="Run Prompt"),
    Key([mod], "equal", lazy.spawn("dmenuunicode"), desc="Unicode Search Prompt"),
    Key([mod], "Insert", lazy.spawn("clipmenu -i"), desc="Clipmenu Prompt"),
    # Key([mod, "shift"], "Insert",
    #     lazy.spawn('notify-send " Clipboard contents:" "$(xclip -o -selection clipboard)"'),
    #     desc="Clipboard Current"),
    Key([mod], "F12", lazy.spawn("maimpick"), desc="MaimPick Prompt"),
    Key([mod, "shift"], "F12",
        lazy.spawn('maim ~/Storage/F$(date \'+%y%m%d-%H%M-%S\').png'),
        desc="Maim Fullscreen Screenshot"),
    # Key([], "XF86Favorites", lazy.spawn("remaps"), desc="Remaps script"),
    # Key([], "XF86Search", lazy.spawn("ferdium"), desc="Emails & Chatting"),
    Key([], "XF86HomePage", lazy.screen.toggle_group(), desc="Switch to group 1"),
    # Media
    Key([], "XF86AudioPlay", lazy.spawn("dmpc toggle")),
    Key([mod], "XF86AudioPlay", lazy.spawn("tppctl toggle")),
    Key([mod, "shift"], "XF86AudioPlay", lazy.spawn("tppctl invert")),
    Key([], "XF86AudioNext", lazy.spawn("dmpc next")),
    Key([mod], "XF86AudioNext", lazy.spawn("tppctl seek 10")),
    Key([mod], "XF86Launch8", lazy.spawn("tppctl seek -10")),
    Key([], "XF86AudioPrev", lazy.spawn("dmpc prev")),
    Key([mod], "XF86AudioPrev", lazy.spawn("tppctl seek -10")),
    Key([mod], "XF86Launch9", lazy.spawn("tppctl seek 10")),
    Key([], "XF86Launch5", lazy.spawn("tppctl invert")),
    Key([], "XF86Calculator", lazy.group["scratchpad"].dropdown_toggle("calculator")),
    Key([], "XF86Explorer", lazy.group["scratchpad"].dropdown_toggle("process_viewer")),
    Key([mod, "shift"], "XF86Back", lazy.spawn("tppctl seek -10")),
    Key([mod, "control"], "XF86Back", lazy.spawn("dmpc prev")),
    # Key([mod, "shift"], "XF86Back", lazy.spawn("dmpc seekp")),
    Key([mod, "shift"], "XF86Forward", lazy.spawn("tppctl seek 10")),
    Key([mod, "control"], "XF86Forward", lazy.spawn("dmpc next")),
    # Key([mod, "shift"], "XF86Forward", lazy.spawn("dmpc seekf")),
    Key([], "cancel", lazy.spawn("tppctl invert")),
    Key([mod], "cancel", lazy.spawn("tppctl pauseall")),
    Key([mod, "shift"], "cancel", lazy.spawn("dmpc toggle")),
    # Key([], "XF86Search", lazy.spawn("")),
    # Key([], "XF86HomePage", lazy.spawn("")),

    Key([mod], "p", lazy.spawn("dmpv2"), desc="Dmpv2"),
    Key([mod, "shift"], "p", lazy.spawn("dmpv"), desc="Dmpv Prompt"),
    Key([mod], "comma", lazy.spawn("dmpc toggle"), desc="Toggle Music"),
    Key([mod], "period", lazy.spawn("tppctl toggle"), desc="Toggle MPVs"),
    # Key([mod, "shift"], "m", lazy.spawn("mp down"), desc="Kill Spotify Music"),
    Key([mod, "shift"], "period", lazy.spawn("tppctl invert"), desc="Inverte Playing MPVs"),
    Key([mod, "control"], "period", lazy.spawn("tppctl pauseall"), desc="Pause All MPVs"),
    Key([mod], "semicolon", focus_previous_window(), desc="Switch to last group"),
    # Scratchpads
    Key([], "XF86Launch7", lazy.group["scratchpad"].dropdown_toggle("mixer")),
    Key([mod], "Return", lazy.group["scratchpad"].dropdown_toggle("Dropdown")),
    Key([mod], "Escape", lazy.group["scratchpad"].dropdown_toggle("process_viewer")),
    # Key([mod], "e", lazy.group["scratchpad"].dropdown_toggle("agenda")),
    Key([mod], "r", lazy.group["scratchpad"].dropdown_toggle("file manager")),
    # Key([mod], "c", lazy.group["scratchpad"].dropdown_toggle("mpvfloat")),
    Key([mod], "n", lazy.group["scratchpad"].dropdown_toggle("news")),
    Key([mod, "Shift"], "n", lazy.group["scratchpad"].dropdown_toggle("podcasts")),
    Key([mod], "m", lazy.group["scratchpad"].dropdown_toggle("music")),
    Key([mod, "Shift"], "m", lazy.group["scratchpad"].dropdown_toggle("spotify")),
    Key([mod], "apostrophe", lazy.group["scratchpad"].dropdown_toggle("calculator")),
    Key([mod], "y", lazy.group["scratchpad"].dropdown_toggle("qtile_shell")),
    Key([], "XF86Mail", lazy.group["scratchpad"].dropdown_toggle("mails")),
    # Hardware/system control
    Key([mod], "up", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")),
    Key([mod, "shift"], "up", lazy.spawn("output-audio")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")),
    Key([mod], "down", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")),
    Key([mod, "shift"], "down", lazy.group["scratchpad"].dropdown_toggle("mixer")),
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
    Key([mod, "shift"], "F8", lazy.spawn("dmenuumount")),
]
# }}}
# ======================= Groups ============= {{{
layouts = [
    layout.Columns(  border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.Max(      border_focus=colors[7], border_normal=colors[8], border_width=0, margin=0 ),
    layout.MonadTall(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.Bsp(      border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.RatioTile(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    # layout.TreeTab(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.TreeTab(
            # font='Liberation Sans',
            # name="looking good",
            bg_color=backgroundColor,
            inactive_bg="151515",
            fontsize=12,
            panel_width=300,
            # margin_left=10,
            # margin_y=10,
            sections=['Tree View'],
            section_left=0,
            previous_on_rm=True,
            # padding_x=4,
            active_bg=colors[3],
            rounded=True,
            border_focus=colors[7],
            border_normal=colors[8],
            border_width=2,
            margin=20
    ),
    layout.Tile(     border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.Stack(    border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20,num_stacks=2),
    layout.Matrix(   border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.MonadWide(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    # layout.VerticalTile(),
    # layout.Zoomy(columnwidth=500),
]
# }}}
# ======================= Bar & Widgets ============= {{{
widget_defaults = dict(
    font='3270 Nerd Font Mono Bold',
    fontsize=13,
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
                widget.Mpd2(
                    idle_format="{play_status}",
                    color_progress="#9497aa",
                    foreground=colors[9],
                    background=foregroundColorTwo,
                    # update_interval=10
                ),
                widget.CPU(
                    format="{load_percent:-2.1f}% {freq_current}GHz",
                    update_interval=3,
                    foreground=colors[5],
                    background=foregroundColorTwo
                ),
                widget.Load(
                    update_interval=3,
                    format="{time}:{load:.2f}",
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
                widget.Memory(
                    update_interval=15,
                    format=' {MemUsed:.0f}{mm}/{SwapUsed:.0f}{mm}',
                    foreground=colors[7],
                    background=foregroundColorTwo
                ),
                widget.Volume(
                    fmt='🔈{}',
                    update_interval=1,
                    foreground=colors[8],
                    background=foregroundColorTwo,
                    # fmt=" {}"
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
                # widget.Net(
                #     fmt='☁{}',
                #     # update_interval=2,
                #     foreground=colors[9],
                #     background=foregroundColorTwo,
                # ),
                # widget.HDDBusyGraph(
                #     frequency=5,
                #     start_pos='top',
                #     graph_color=colors[8],
                #     background=foregroundColorTwo,
                # ),
                # widget.Wlan(
                #     disconnected_message='❌',
                #     format='{essid} {percent:2.0%}',
                #     update_interval=10,
                #     foreground=colors[4],
                #     background=foregroundColorTwo,
                # ),
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
                    format="🗓 %V %a %d/%B/%y",
                    update_interval=86400,
                    foreground=colors[10],
                    background=backgroundColor
                ),
                widget.Clock(
                    format="🕓 %T",
                    update_interval=1,
                    foreground=colors[2],
                    background=backgroundColor
                ),
                widget.Pomodoro(
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
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = 'floating_only'
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = False
auto_minimize = True
wmname = "LG3D"
# }}}
