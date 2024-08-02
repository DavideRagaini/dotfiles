from libqtile import hook, qtile
from libqtile.config import Match
from libqtile.lazy import lazy
from typing import List
from libqtile.log_utils import logger


# ======================= testing ============= {{{
cursor_hide = True


@lazy.function
def toggle_cursor(qtile):
    global cursor_hide
    if cursor_hide is True:
        qtile.core.hide_cursor()
        cursor_hide = False
    elif cursor_hide is False:
        qtile.core.unhide_cursor()
        cursor_hide = True


@lazy.window.function
def resize_floating_window(window, width: int = 0, height: int = 0):
    window.set_size_floating(window.width + width, window.height + height)


@lazy.window.function
def move_floating_window(window, x: int = 0, y: int = 0):
    window.set_position_floating(window.float_x + x, window.float_y + y)


# }}}
# ======================= floating corner window ============= {{{
@lazy.function
def floating_corner_window(
    qtile,
    position: str = "bottom right",
    window_x: int = 0,
    window_y: int = 0,
    div: int = 8,
    border_padding: int = 3,
    bar_padding: int = 37,
):
    current_screen = qtile.current_screen.info()
    screen_width = current_screen["width"]
    screen_height = current_screen["height"]
    if screen_width < screen_height:
        window_width = int(int(screen_height) / div)
        window_height = int(int(screen_width) / div)
    else:
        window_width = int(int(screen_width) / div)
        window_height = int(int(screen_height) / div)
    main_monitor_width = 0
    if current_screen["index"] >= 1:
        main_monitor_width = qtile.screens[0].info()["width"]
    top = bar_padding
    bottom = screen_height - window_height - border_padding - bar_padding
    left = main_monitor_width + border_padding
    right = main_monitor_width + screen_width - window_width - border_padding
    match position:
        case "bottom right":
            window_x = right
            window_y = bottom
        case "top right":
            window_x = right
            window_y = top
        case "bottom left":
            window_x = left
            window_y = bottom
        case "top left":
            window_x = left
            window_y = top
    window = qtile.current_screen.group.current_window
    window.enable_floating()
    window.place(
        window_x, window_y, window_width, window_height, 1, "#FFFFFF", True, None, True
    )
    window.bring_to_front()
    # window.static(0, int(window_x), int(window_y), int(window_width), int(window_height))


# }}}
# ======================= mpv ============= {{{
@lazy.function
def move_mpv_to_current_group(qtile):
    for group in qtile.groups:
        for window in group.windows:
            if (
                Match(wm_class="mpv")
                and window.info()["group"] != qtile.current_group.name
            ):
                window.togroup()


show_bar = True


@lazy.function
def toggle_bar(qtile):
    global show_bar
    show_bar = not show_bar
    qtile.hide_show_bar()


@hook.subscribe.setgroup
def autohide_bar_9th_group():
    global show_bar
    bar_off = qtile.current_screen.bottom.info()["size"] == 0
    if qtile.current_group.name == "9" and not bar_off:
        qtile.hide_show_bar()
    elif bar_off and show_bar:
        qtile.hide_show_bar()


# @hook.subscribe.setgroup
# async def mpv_auto_toggle_minimize():
#     # if len(qtile.screens) == 0:
#     logger.warning(qtile.screens[0].info())
#     logger.warning(qtile.screens[-1].info())
#     for group in qtile.groups:
#         for window in group.windows:
#             if window.info()["wm_class"][1] == "mpv":
#                 if (
#                     window.info()["floating"] == False
#                     and window.info()["minimized"] == True
#                 ) and (
#                     window.info()["group"] == qtile.current_group.name
#                     or qtile.screens[-1].current_group.name
#                 ):
#                     window.toggle_minimize()
#                 else:
#                     if (
#                         window.info()["group"] != qtile.current_group.name
#                         or window.info()["group"] != qtile.screens[-1].current_group.name
#                     ) and window.info()["floating"] == False:
#                         window.toggle_minimize()


# }}}
# ======================= Sticky ============= {{{


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
async def move_sticky_windows():
    if sticky_windows:
        for window in sticky_windows:
            window.togroup()
        qtile.current_layout.next()


@hook.subscribe.client_killed
def remove_sticky_windows(window):
    if window in sticky_windows:
        sticky_windows.remove(window)


# @hook.subscribe.client_managed
# def auto_sticky_windows(window):
#     info = window.info()
#     if info["wm_class"] == ["mpv"] and info["floating"] == True:
#         sticky_windows.append(window)


# }}}
# ======================= Merge Groups ============= {{{
groupsMerged = {1: [], 2: [], 3: [], 4: [], 5: [], 6: [], 7: [], 8: [], 9: [], 0: []}
# groupsMerged: Dict[int,str] = {}


@lazy.function
def merge_groups(qtile, g=1):
    global groupsMerged
    g = g - 1
    windowsToMerge = qtile.groups[g].windows
    if len(groupsMerged[g]) == 0:  # if empty then append and move to current group
        for i in reversed(range(len(windowsToMerge))):
            w = windowsToMerge[i]
            groupsMerged[g].append(w)
            w.togroup()
            if w.info()["minimized"] == True:
                w.toggle_minimize()
    else:  # if there are some elements then restore the positions
        for group in groupsMerged[g]:
            group.togroup(str(g + 1))
        groupsMerged[g].clear()


@lazy.function
@hook.subscribe.restart
def restore_all_merged_groups(qtile):
    global groupsMerged
    for group in groupsMerged:
        for window in groupsMerged[group]:
            window.togroup(str(group + 1))
        groupsMerged[group].clear()


@hook.subscribe.client_killed
async def remove_merge_group_windows(window):
    for group in groupsMerged:
        if window in groupsMerged[group]:
            groupsMerged[group].remove(window)


# @hook.subscribe.restart
# def restore_all_merged_groups_hook():
#     restore_all_merged_groups(qtile)


# }}}
# ======================= Window Focus ============= {{{
last_focus_index = -1


@lazy.function
def toggle_focus_main(qtile):
    layout = qtile.current_layout
    global last_focus_index
    current_index = layout.clients.current_index
    if current_index == 0:
        if last_focus_index < 0:
            return
        else:
            qtile.current_group.focus_by_index(last_focus_index)
    else:
        qtile.current_group.focus_by_index(0)
        last_focus_index = current_index


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


# }}}
# ======================= Floating ============= {{{
@lazy.function
def float_to_front(qtile):
    for group in qtile.groups:
        for window in group.windows:
            if window.floating:
                window.bring_to_front()


floating_window_index = 0


@lazy.function
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


# }}}
# ======================= Screens ============= {{{
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


# @lazy.function
# def window_to_next_screen(qtile, switch_group=False, switch_screen=False):
#     i = qtile.screens.index(qtile.current_screen)
#     if i != 0:
#         group = qtile.screens[i - 1].group.name
#         qtile.current_window.togroup(group, switch_group=switch_group)
#         if switch_screen == True:
#             qtile.to_screen(i - 0)
#     elif i + 1 != len(qtile.screens):
#         group = qtile.screens[i + 1].group.name
#         qtile.current_window.togroup(group, switch_group=switch_group)
#         if switch_screen == True:
#             qtile.to_screen(i + 1)


# }}}
# ======================= TreeTab ============= {{{
# Allows you to input a name when adding treetab section.
# @lazy.layout.function
# def add_treetab_section(layout):
#     prompt = qtile.widgets_map["prompt"]
#     prompt.start_input("Section name: ", layout.add_section)


# }}}
# ======================= Opacity ============= {{{
@lazy.function
def window_opacity(qtile, cmd, value=1):
    match cmd:
        case "inc":
            qtile.current_group.current_window.up_opacity()
        case "dec":
            qtile.current_group.current_window.down_opacity()
        case "set":
            qtile.current_group.current_window.set_opacity(value)


# }}}
