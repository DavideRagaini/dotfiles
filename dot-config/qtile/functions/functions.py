from libqtile.command import lazy
from typing import List

# sticky_windows: List[str] = []
last_focus_index = -1
previous_focused: List[str] = []

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

class Function(object):
    @staticmethod
    def floating_window_corner(position=1):
        from libqtile.config import Screen

        @lazy.function
        def __inner(qtile, position=1):
            window = qtile.current_screen.group.current_window
            current_screen = qtile.current_screen.info()
            screen_width = current_screen["width"]
            screen_height = current_screen["height"]
            div = 6
            window_width = int(int(screen_width) / div)
            window_height = int(int(screen_height) / div)
            border_padding = 3
            bar_padding = 20
            window.toggle_floating()
            window_x = 0
            window_y = 0
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
                window_x,
                window_y,
                window_width,
                window_height,
                1,
                "#FF00FF",
                True,
                None,
                True,
            )

    class opacity(object):
        @staticmethod
        @lazy.function
        def reset(qtile, value=1):
            qtile.current_group.current_window.set_opacity(value)

        @staticmethod
        @lazy.function
        def inc():
            qtile.current_group.current_window.up_opacity()

        @staticmethod
        @lazy.function
        def dec():
            qtile.current_group.current_window.down_opacity()

    # class sticky_window():
    #     from libqtile import hook

    #     @staticmethod
    #     @lazy.function
    #     def toggle(qtile, window=None):
    #         if window is None:
    #             window = qtile.current_screen.group.current_window
    #         if window in sticky_windows:
    #             sticky_windows.remove(window)
    #         else:
    #             sticky_windows.append(window)
    #         return window

    #     @hook.subscribe.setgroup
    #     def move(self):
    #         for window in sticky_windows:
    #             window.togroup()
    #         return

    #     @hook.subscribe.client_killed
    #     def remove(window):
    #         if window in sticky_windows:
    #             sticky_windows.remove(window)

    # @hook.subscribe.client_managed
    # def auto_sticky_windows(window):
    #     info = window.info()
    #     if info["wm_class"] == ["mpvFloat", "mpv"]:
    #         sticky_windows.append(window)

    class dwm_merge_groups(object):
        groupsMerged = {
            1: [],
            2: [],
            3: [],
            4: [],
            5: [],
            6: [],
            7: [],
            8: [],
            9: [],
            0: [],
        }

        @lazy.function
        @staticmethod
        def __inner(qtile, g=1):
            global merged
            g = g - 1
            windowsToMerge = qtile.groups[g].windows
            if (
                len(groupsMerged[g]) == 0
            ):  # if empty then append and move to current group
                for i in reversed(range(len(windowsToMerge))):
                    w = windowsToMerge[i]
                    groupsMerged[g].append(w)
                    w.togroup()
            else:  # if there are some elements then restore the positions
                for group in groupsMerged[g]:
                    group.togroup(str(g + 1))
                groupsMerged[g].clear()

        @lazy.function
        @staticmethod
        def restore_all():
            global groupsMerged
            for group in groupsMerged:
                for window in groupsMerged[i]:
                    window.togroup(str(group + 1))
                groupsMerged[group].clear()

    class dwm_swap(object):
        # https://github.com/qtile/qtile/discussions/3621
        @staticmethod
        @lazy.function
        def swap_focus_main():
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

        @lazy.function
        @staticmethod
        def focus_main():
            layout = qtile.current_layout
            if layout.align == 1:
                layout.right()
                return
            layout.left()

    @staticmethod
    def float_to_front():
        @lazy.function
        def __inner(qtile):
            for group in qtile.groups:
                for window in group.windows:
                    if window.floating:
                        window.bring_to_front()

    @staticmethod
    def float_cycle(direction):
        def __inner(qtile, forward: bool):
            floating_window_index = 0
            floating_windows = []
            for window in qtile.current_group.windows:
                if window.floating:
                    floating_windows.append(window)
            if not floating_windows:
                return
            floating_window_index = min(
                floating_window_index, len(floating_windows) - 1
            )
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

    @staticmethod
    def change_focus_previous_window():
        from libqtile import hook
        from typing import List

        @hook.subscribe.client_focus
        def client_focused(window):
            global previous_focused
            if len(previous_focused) < 2:
                previous_focused.append(window)
            elif previous_focused[1] != window:
                previous_focused[0] = previous_focused[1]
                previous_focused[1] = window

        @lazy.function
        def __inner(qtile):
            global previous_focused
            if len(previous_focused) == 2:
                group = previous_focused[0].group
                qtile.current_screen.set_group(group)
                group.focus(previous_focused[0])

    # # kick a window to another screen (handy during presentations)
    # def kick_to_next_screen(qtile, direction=1):
    #     other_scr_index = (qtile.screens.index(qtile.currentScreen) + direction) % len(qtile.screens)
    #     othergroup = None
    #     for group in qtile.groups().values():
    #         if group['screen'] == other_scr_index:
    #             othergroup = group['name']
    #             break
    #     if othergroup:
    #         qtile.moveToGroup(othergroup)

    @staticmethod
    def move_window_to_next_screen():
        def __inner(qtile, switch_group=False, switch_screen=False):
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

    # @staticmethod
    # def picom_floating_flags():

    #     from libqtile import hook

    #     @hook.subscribe.client_focus
    #     def set_hint(window):
    #         window.window.set_property(
    #             "IS_FLOATING_WINDOW", str(window.floating), type="STRING", format=8
    #         )

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
