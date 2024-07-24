# ======================= Imports ============= {{{
from bindings import *
from groups import Groups
from layouts import layout_defaults
from scratchpad import Scratchpad
from bars import widget_defaults, screens

# }}}
# ======================= init ============= {{{
if __name__ in ["config", "__main__"]:
    obj_groups = Groups()

    obj_scratchpad = Scratchpad()
    # obj_dd_keys = DropDown_Keys()

    groups = obj_groups.init_groups()

groups = obj_groups.init_groups()
groups += obj_scratchpad.init_scratchpad()

layouts = layout_defaults()
mouse = mouse()
keys = bindings()
# keys += obj_dd_keys.init_dropdown_keybindings()
# }}}
# =======================  ============= {{{
if qtile.core.name == "wayland":
    wl_xcursor_theme = "Catppuccin-Macchiato-Dark-Cursors"
    wl_xcursor_size = 48
    widget_defaults.update({"fontsize": 18})

    from autostart import *
    from libqtile.backend.wayland import InputConfig
    wl_input_rules = {
        "type:touchpad": InputConfig(accel_profile="adaptive", tap=True),
        "type:mouse": InputConfig(accel_profile="adaptive", drag=True),
        "type:keyboard": InputConfig(
            kb_layout="us",
            kb_variant="altgr-intl",
            kb_options="ctrl:swapcaps,altwin:swap_alt_win",
            # kb_options="ctrl:swapcaps,altwin:swap_alt_win,altwin:escape,altwin:menu_win",
            kb_numlock="enabled",
            kb_repeat_delay=300,
            kb_repeat_rate=50,
        ),
    }
# ======================= Misc ============= {{{
auto_fullscreen = True
auto_minimize = True
bring_front_click = "floating_only"
cursor_warp = False
dgroups_app_rules = []
floats_kept_above = True
focus_on_window_activation = "smart"
follow_mouse_focus = True
reconfigure_screens = True
wmname = "LG3D"
# }}}
