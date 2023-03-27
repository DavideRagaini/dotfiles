# ======================= Imports ============= {{{
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.dgroups import simple_key_binder
from libqtile import hook, qtile
from libqtile.extension import WindowList
from libqtile.lazy import lazy
from typing import List
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
browser = "qutebrowser"
browser_alt = "librewolf"
browser_priv = "librewolf --private-window"
terminal = "alacritty"
text_editor = "em"
file_manager = terminal + " -e lf"
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
# ======================= Functions ============= {{{
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
    win.cmd_bring_to_front()
    win.cmd_focus()


@lazy.function
def float_cycle_backward(qtile):
    float_cycle(qtile, False)


@lazy.function
def float_cycle_forward(qtile):
    float_cycle(qtile, True)


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


@hook.subscribe.client_managed
def auto_sticky_windows(window):
    info = window.info()
    if info["wm_class"] == ["mpvFloat", "mpv"]:
        sticky_windows.append(window)


# }}}
# ======================= Keybindings ============= {{{
keys = [
    # Switch between windows
    Key([mod], "backslash", float_cycle_forward),
    Key([mod, "shift"], "backslash", float_cycle_backward),
    Key( [mod], "s", toggle_sticky_windows(), desc="Toggle state of sticky for current window",),
    # Key([mod], "bracketleft", lazy.screen.next_group(skip_empty=True), desc="Cycle Forward to Active Groups"),
    # Key([mod], "bracketright", lazy.screen.prev_group(skip_empty=True), desc="Cycle Backward to Active Groups"),
    # Key([mod, 'shift'], "g", lazy.window.togroup("scratchpad"), desc="Move Window to scratchpad"),
    # Key([mod], "g", lazy.group["scratchpad"].toscreen(toggle=True), desc="Toggle scratchpad group"),
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
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "o", lazy.layout.normalize(), desc="Reset all window sizes"),
    Key([mod, "shift"], "Tab",
        lazy.run_extension(
            WindowList(
                # item_format="{id}: {group} {window}",
                font="FantasqueSansMono",
                fontsize=18,
                dmenu_ignorecase=True,
                # selected_background=colors[3]
            )
        ),
        desc="Windows Selector",
    ),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with multiple stack panes
    Key([mod, "control"], "space", lazy.layout.toggle_split(), desc="Toggle split sides of stack"),
    Key([mod], "q", lazy.window.kill(), desc="Kill Focused Window"),
    Key([mod], "u", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "i", lazy.prev_layout(), desc="Toggle between layouts"),
    Key([mod, "control"], "r", lazy.restart(), desc="Restart Qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
    Key([mod], "backspace", lazy.spawn("sysact Shutdown"), desc="Shutdown"),
    Key([mod, "shift"], "backspace", lazy.spawn("sysact"), desc="System Actions"),
    Key([mod], "f", lazy.window.toggle_fullscreen(), desc="Toggle Current Window Fullscreen"),
    Key([mod, "shift"], "space", lazy.window.toggle_floating(), desc="Toggle Current Window Floating"),
    Key([mod], "b", lazy.hide_show_bar("top"), desc="Toggle Top Bar"),
    # EzKey("M-<comma>", lazy.prev_screen()),
    # EzKey("M-<period>", lazy.next_screen()),
    # Applications
    Key([mod], "grave", lazy.spawn("dunstctl close"), desc="Dunst Close"),
    Key([mod, "shift"], "grave", lazy.spawn("dunstctl history-pop"), desc="Dunst History Pop"),
    Key([mod, "shift"], "Return", lazy.spawn(terminal + " -e tmux"), desc="Launch Terminal"),
    Key([mod], "w", lazy.spawn(browser), desc="Launch Browser"),
    Key([mod, "shift"], "w", lazy.spawn(browser_alt), desc="Launch Browser"),
    Key([mod, "control"], "w", lazy.spawn(browser), desc="Launch Browser"),
    Key([mod, "shift"], "e", lazy.spawn(text_editor), desc="Launch Text Editor"),
    Key([mod], "d", lazy.spawn("dmenu_run_history"), desc="Run Prompt"),
    Key([mod, "shift"], "d", lazy.spawn("via -r"), desc="Document Search"),
    Key([mod, "shift"], "r", lazy.spawn("via -a"), desc="Global Search"),
    Key([mod, "control"], "d", lazy.spawncmd(launcher), desc="Run Prompt"),
    Key([mod], "bracketleft", lazy.spawn("dmenuunicode"), desc="Unicode Search Prompt"),
    Key([mod], "Insert", lazy.spawn("clipmenu"), desc="Clipmenu Prompt"),
    Key(
        [mod, "shift"],
        "Insert",
        lazy.spawn(
            'notify-send " Clipboard contents:" "$(xclip -o -selection clipboard)"'
        ),
        desc="Clipboard Current",
    ),
    # Media
    Key([], "XF86AudioPlay", lazy.spawn("dmpc toggle")),
    Key([mod], "XF86AudioPlay", lazy.spawn("tppctl toggle")),
    Key([mod, "shift"], "XF86AudioPlay", lazy.spawn("tppctl invert")),
    Key([], "XF86AudioNext", lazy.spawn("dmpc next")),
    Key([mod], "XF86AudioNext", lazy.spawn("tppclt seek 10")),
    Key([mod], "XF86Launch8", lazy.spawn("tppctl seek -10")),
    Key([], "XF86AudioPrev", lazy.spawn("dmpc previous")),
    Key([mod], "XF86AudioPrev", lazy.spawn("tppctl seek -10")),
    Key([mod], "XF86Launch9", lazy.spawn("tppclt seek 10")),
    Key([mod], "p", lazy.spawn("dmpv"), desc="Dmpv Prompt"),
    Key([mod], "comma", lazy.spawn("dmpc toggle"), desc="Toggle Music"),
    Key([mod], "period", lazy.spawn("tppctl toggle"), desc="Toggle MPV"),
    # Hardware/system control
    Key([mod], "up", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+")),
    Key([mod], "down", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")),
    Key([mod], "XF86AudioMute", lazy.spawn("output-audio")),
    Key([], "XF86AudioMute", lazy.spawn("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -set +10%")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -set -10%")),
]
# }}}
# ======================= Groups ============= {{{
#  󰇮   󰈙     
groups = [
    Group(
        name="1",
        position=1,
        label="1",
        layout="max",
        exclusive=True,
        matches=[Match(wm_class="Emacs")],
    ),
    Group(
        name="2",
        position=2,
        label="2",
        layout="monadtall",
        exclusive=True,
        matches=[Match(wm_class="qutebrowser"), Match(wm_class="LibreWolf")],
    ),
    Group(
        name="3",
        position=3,
        label="3",
        layout="max",
        exclusive=True,
        matches=[Match(wm_instance_class="Zathura"), Match(wm_class="ebook-viewer")],
    ),
    Group(
        name="4",
        position=4,
        label="4",
        layout="max",
        matches=[Match(wm_class="VirtualBox Machine")],
    ),
    Group(
        name="5",
        position=5,
        label="5",
        layout="bsp",
        # matches=[Match(title='irc'), Match(wm_class='Skype'), Match(wm_class='microsoft teams - preview'), Match(wm_class='discord')]
    ),
    Group(
        name="6",
        position=6,
        label="6",
        layout="bsp",
        # matches=[Match(title='updates'), Match(wm_class='Gnome-system-monitor'), Match(wm_class='VirtualBox Manager')]
    ),
    Group(
        name="7",
        position=7,
        label="7",
        layout="bsp",
        # matches=[Match(title='updates'), Match(wm_class='Gnome-system-monitor'), Match(wm_class='VirtualBox Manager')]
    ),
    Group(
        name="8",
        position=8,
        label="8",
        layout="bsp",
        # matches=[Match(title='updates'), Match(wm_class='Gnome-system-monitor'), Match(wm_class='VirtualBox Manager')]
    ),
    Group(
        name="9",
        position=9,
        label="9",
        layout="bsp",
        exclusive=True,
        matches=[Match(wm_class="mpv")],
    ),
]
dgroups_key_binder = simple_key_binder(mod)
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
            ),
            DropDown(
                "Agenda",
                "eagenda",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
            ),
            DropDown(
                "file_manager",
                file_manager,
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
            ),
            DropDown(
                "process_viewer",
                process_viewer,
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
            ),
            DropDown(
                "news",
                terminal + " -e newsboat",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
            ),
            DropDown(
                "music",
                terminal + " -e mp",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
            ),
            DropDown(
                "mixer",
                terminal + " -e pulsemixer",
                width=0.6,
                height=0.3,
                x=0.2,
                y=0.2,
                opacity=0.9,
            ),
            DropDown(
                "calculator",
                terminal + " -e wcalc -P -1 -c -q --ints -C -p -r --remember",
                width=0.8,
                height=0.8,
                x=0.1,
                y=0.1,
                opacity=0.9,
            ),
        ],
    )
)
keys.extend(
    [
        Key([], "XF86Launch7", lazy.group["scratchpad"].dropdown_toggle("mixer")),
        Key([mod], "Return", lazy.group["scratchpad"].dropdown_toggle("Dropdown")),
        Key(
            [mod], "Escape", lazy.group["scratchpad"].dropdown_toggle("process_viewer")
        ),
        Key([mod], "e", lazy.group["scratchpad"].dropdown_toggle("Agenda")),
        Key([mod], "r", lazy.group["scratchpad"].dropdown_toggle("file_manager")),
        Key([mod], "n", lazy.group["scratchpad"].dropdown_toggle("news")),
        Key([mod], "m", lazy.group["scratchpad"].dropdown_toggle("music")),
        Key(
            [mod], "apostrophe", lazy.group["scratchpad"].dropdown_toggle("calculator")
        ),
    ]
)
# }}}
# ======================= Groups ============= {{{
layouts = [
    layout.Columns(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.Max(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.MonadTall(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.Bsp(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.RatioTile(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.TreeTab(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.Tile(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.Stack(num_stacks=2, border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20,),
    layout.Bsp(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.Matrix(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.MonadWide(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    layout.TreeTab(border_focus=colors[7], border_normal=colors[8], border_width=2, margin=20),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]
# }}}
# ======================= Bar & Widgets ============= {{{
widget_defaults = dict(
    font="Hack Nerd Font Bold", fontsize=10, padding=4, background=colors[0]
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    fontsize=12,
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
                    foreground=colors[7],
                    # background = foregroundColorTwo,
                ),
                widget.WindowName(
                    foreground=colors[8],
                ),
                widget.Chord(
                    chords_colors={
                        "launch": (foregroundColor, foregroundColor),
                    },
                    name_transform=lambda name: name.upper(),
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
                # widget.Mpd2(
                #         fontsize = 10,
                #         foreground = colors[4],
                #         background = foregroundColorTwo
                #         ),
                widget.Prompt(
                    foreground=colors[2],
                    background=foregroundColorTwo,
                ),
                # widget.CPUGraph(
                #         update_interval = 10,
                #         fontsize = 10,
                #         foreground = colors[5],
                #         background = foregroundColorTwo,
                #         ),
                widget.CPU(
                    update_interval=10,
                    format="{load_percent}% {freq_current}",
                    fontsize=10,
                    foreground=colors[5],
                    background=foregroundColorTwo,
                ),
                # widget.MemoryGraph(
                #         update_interval = 10,
                #         fontsize = 10,
                #         foreground = colors[6],
                #         background = foregroundColorTwo,
                #         ),
                widget.Memory(
                    update_interval=10,
                    format='{MemUsed: .0f}{ms}/{SwapUsed: .0f}{ms}',
                    fontsize=10,
                    foreground=colors[6],
                    background=foregroundColorTwo,
                ),
                widget.Net(
                    update_interval=10,
                    interface="enp2s0",
                    format=" {down} ↓↑ {up}",
                    foreground=colors[7],
                    background=foregroundColorTwo,
                ),
                # widget.CheckUpdates(
                #     update_interval = 3600,
                #     distro = "Ubuntu",
                #     display_format = "Updates: {updates} ",
                #     no_update_string = " No Updates",
                #     colour_have_updates = colors[9],
                #     colour_no_updates = colors[5],
                #     padding = 8,
                #     background = foregroundColorTwo
                #     ),
                widget.Volume(
                    foreground=colors[4],
                    background=foregroundColorTwo,
                    fmt=" {}",
                ),
                widget.TextBox(
                    text="\u25e2",
                    padding=0,
                    fontsize=50,
                    background=foregroundColorTwo,
                    foreground=backgroundColor,
                ),
                widget.Systray(),
                widget.Clock(
                    format=" %V %a %d/%B/%y",
                    update_interval=86400,
                    foreground=colors[10],
                    background=backgroundColor,
                ),
                widget.Clock(
                    format=" %T",
                    update_interval=1,
                    foreground=colors[5],
                    background=backgroundColor,
                ),
            ],
            20,
        ),
    ),
]
# }}}
# ======================= Floating Windows Rules ============= {{{
floating_layout = layout.Floating(
    border_focus=colors[4],
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(wm_class="fzfmenu"),
        Match(wm_instance_class="mpvFloat"),
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(func=lambda c: c.has_fixed_size()),
        Match(func=lambda c: c.has_fixed_ratio()),
    ],
)
# }}}
# ======================= Hooks ============= {{{
# Programms to start on log in
# @hook.subscribe.startup_once
# def autostart ():
#     home = os.path.expanduser('~/.config/qtile/autostart.sh')
#     subprocess.call([home])

# @hook.subscribe.startup_once
# def startup_once():
#     subprocess.Popen(["nm-applet"])
# }}}
# ======================= Misc ============= {{{
# dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = True
cursor_warp = True
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
# }}}
