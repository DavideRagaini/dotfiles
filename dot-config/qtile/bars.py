from libqtile.config import Screen
from libqtile import bar, widget
from colors import dracula
import os

colorscheme = dracula()
(
    colors,
    background,
    foreground,
    workspace,
    foregroundTwo,
) = colorscheme

widget_defaults = dict(
    font="JetBrainsMono Nerd Font ExtraBold",
    fontsize=9,
    padding=8,
    # background=colors[0],  # #282a36
)

main_bar = [
    widget.GroupBox(
        # borderwidth=3,
        # highlight_method='block',
        # active='#CAA9E0',
        # block_highlight_text_color="#91B1F0",
        # highlight_color='#4B427E',
        # inactive='#282738',
        # foreground='#4B427E',
        # background='#353446',
        # this_current_screen_border='#353446',
        # this_screen_border='#353446',
        # other_current_screen_border='#353446',
        # other_screen_border='#353446',
        # urgent_border='#353446',
        rounded=True,
        disable_drag=True,
        padding=4,
        active=colors[2],
        inactive=colors[0],
        highlight_color=[background, workspace],
        this_screen_border=colors[8],
        this_current_screen_border=colors[7],
        other_screen_border=colors[3],
        other_current_screen_border=colors[3],
        highlight_method="block",
        hide_unused=True,
    ),
    #
    widget.Spacer(length=10),
    widget.CurrentLayout(foreground=colors[2]),
    #
    widget.Spacer(length=10),
    #
    widget.Chord(background=background),
    #
    widget.WindowCount(foreground=colors[7]),
    #
    widget.WindowName(
        foreground=colors[8],
    ),
    #
    widget.Prompt(
        cursorblink=0.5,
        prompt="&:",
        foreground=colors[9],
    ),
    #
    widget.Load(
        update_interval=1,
        format="{load:-3.2f}",  # 
        foreground=colors[5],
        # background=foregroundTwo,
    ),
    widget.GenPollCommand(
        cmd=os.path.expanduser("~/.local/bin/statusbar/sb-cpubars"),
        foreground=colors[5],
        # background=foregroundTwo,
        update_interval=1,
    ),
    #
    widget.CPU(
        format="{load_percent:-2.1f}% {freq_current}GHz",
        update_interval=1,
        foreground=colors[5],
        # background=foregroundTwo,
    ),
    widget.ThermalSensor(
        update_interval=30,
        format="{temp:.0f}{unit}",
        tag_sensor="Package id 0",
        foreground=colors[5],
        # background=foregroundTwo,
    ),
    # widget.GenPollCommand(
    #     cmd=os.path.expanduser("~/.local/bin/statusbar/sb-tlp"),
    #     foreground=colors[5],
    #     background=foregroundTwo,
    #     update_interval=60,
    # ),
    #
    widget.GenPollCommand(
        cmd=os.path.expanduser("~/.local/bin/statusbar/sb-nvgpu"),
        foreground=colors[6],
        # background=foregroundTwo,
        update_interval=10,
    ),
    #
    widget.Memory(
        update_interval=15,
        format=" {MemUsed:.0f}{ms}/{SwapUsed:.0f}{ms}",
        foreground=colors[7],
        # background=foregroundTwo,
    ),
    #
    widget.DF(
        format="H {uf} {r:.0f}%",
        partition="/home/davide",
        foreground=colors[4],
        warn_space=100,
    ),
    widget.DF(
        format="D {uf} {r:.0f}%",
        partition="/media/data",
        foreground=colors[4],
        warn_space=100,
    ),
    widget.DF(
        format="L {uf} {r:.0f}%",
        partition="/media/d25l1tb",
        foreground=colors[4],
        warn_space=100,
    ),
    widget.HDD(
        device="nvme0n1",
        format="{HDDPercent}%",
        foreground=colors[4],
    ),
    widget.HDD(
        device="sda",
        format="{HDDPercent}%",
        foreground=colors[4],
    ),
    widget.HDD(
        device="sdb",
        format="{HDDPercent}%",
        foreground=colors[4],
    ),
    #
    # widget.Volume(
    #     fmt=" {}",
    #     update_interval=1,
    #     step=2,
    #     foreground=colors[8],
        # volume_app= "foot -f 'IosevkaTerm Nerd Font Propo:weight=bold:size=18' -e pulsemixer",
        # volume_down_command="wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%-",
        # volume_up_command="wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%+",
        # check_mute_command="wpctl get-volume @DEFAULT_SINK@ | awk '{print $3}'",
        # check_mute_string="[MUTED]",
        # mute_command="wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle",
    #     # background=foregroundTwo,
    # ),
    widget.PulseVolume(
        fmt=" {}",
        update_interval=1,
        step=2,
        foreground=colors[8],
        # background=foregroundTwo,
    ),
    widget.GenPollCommand(
        cmd=os.path.expanduser("~/.local/bin/statusbar/sb-sink"),
        foreground=colors[8],
        # background=foregroundTwo,
        update_interval=60,
    ),
    widget.GenPollCommand(
        cmd=os.path.expanduser("~/.local/bin/statusbar/sb-audio-levels"),
        foreground=colors[8],
        # background=foregroundTwo,
        update_interval=60,
    ),
    #
    widget.Net(
        # fmt=" {}",
        # fmt="{}",
        format="{down:4.0f}{down_suffix} {up:4.0f}{up_suffix}",
        prefix="k",
        update_interval=1,
        foreground=colors[9],
        # background=foregroundTwo,
    ),
    #
    # widget.Systray(background=workspace, foreground=background),
    #
    widget.Clock(
        format=" %V %a %d/%B/%y",
        update_interval=3600,
        foreground=colors[10],
        # background=background,
    ),
    widget.Clock(
        format=" %T",
        update_interval=1,
        foreground=colors[10],
        # background=background,
    ),
    #
    widget.Pomodoro(
        update_interval=1,
        prefix_active=" ",
        prefix_break="🛑",
        prefix_long_break="🛑🛑",
        prefix_inactive="🔃",
        # prefix_paused=️'',
        foreground=colors[1],
        # background=background,
        color_active=colors[4],
        color_inactive=colors[6],
    ),
]

# from libqtile.lazy import lazy

# sptctl = "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player."
# scroll_defaults = dict(
#     scroll=False,
#     # scroll=True,
#     # width=400
#     scroll_chars=60,
#     scroll_interval=5,
#     scroll_step=50,
#     scroll_fixed_width=False,
# )

# widget.Mpd2(
#     # status_format='{play_status} [{artist:.15s}]-[{album:.15s}]-[{title:.30s}]',
#     status_format="{play_status} {artist} - {album} - {title} [{repeat}{random}{single}{consume}{updating_db}]",
#     idle_message="mpd",
#     color_progress="#f1fa8c",
#     update_interval=10,
#     foreground=colors[9],
#     background=foregroundTwo,
#     **scroll_defaults
# ),

# widget.CPUGraph(
#     frequency=5,
#     graph_color=colors[5],
#     background=background,
# ),

# widget.MemoryGraph(
#     frequency=5,
#     graph_color=colors[7],
#     background=background,
# ),

# widget.HDDBusyGraph(
#     frequency=5,
#     graph_color=colors[8],
#     background=background,
# ),

# widget.NetGraph(
#     frequency=5,
#     graph_color=colors[8],
#     background=background,
# ),

# widget.Wttr(
#     fmt="<b>{}</b>",
#     format="%C %h %t %w %M %p %P",
#     # user_agent='',
#     location={"34.50,13.01": "H"},
#     foreground=colors[9],
#     background=background,
# ),

# widget.NvidiaSensors(
#     update_interval=30,
#     # format=' {temp:.0f}{unit}',
#     gpu_bus_id="01:00.0",
#     tag_sensor="Package id 0",
#     foreground=colors[6],
#     background=foregroundTwo,
# ),

# widget.WindowTabs(
#     selected=('<u>« ',' »</u>'),
#     separator='      ',
#     foreground=colors[8],
#     # background=colors[2],
# ),

# widget.Wlan(
#     disconnected_message='❌',
#     format='{essid} {percent:2.0%}',
#     update_interval=10,
#     foreground=colors[4],
#     background=foregroundTwo,
# ),

# widget.Backlight(
#     update_interval = 15,
#     foreground=colors[8],
#     background=foregroundTwo,
#     ),

# widget.Countdown(
#     # format='{D}d {H}h {M}m {S}s',
#     date=datetime(2023, 10, 5, 0, 0, 0, 0),
#     foreground=colors[3],
#     background=foregroundTwo,
#     color_active=colors[4],
#     color_inactive=foregroundTwo
# ),

# widget.Backlight(
#     update_interval = 15,
#     foreground=colors[8],
#     background=foregroundTwo,
#     ),

# widget.Battery(
#     update_interval=15,
#     format='{char} {percent:2.0%} {hour:d}:{min:02d} {watt:.2f} W',
#     low_precentage=0.35,
#     charge_char='',
#     discharge_char='',
#     empty_char='',
#     foreground=colors[6],
#     background=foregroundTwo,
#      ),

# widget.HDDBusyGraph(
#     frequency=5,
#     start_pos="top",
#     graph_color=colors[8],
#     background=foregroundTwo,
# ),

# widget.Clipboard(
#     # update_interval=1,
#     foreground=colors[3],
#     background=background,
#     color_active=colors[4],
#     color_inactive=foregroundTwo,
# ),

wallpaperPath = os.path.expanduser("~/.local/share/")

main_screen = Screen(
    bottom=bar.Bar(
        main_bar,
        30,
        background="#1e1f24f2",
        border_color="#aaaaaaf2",
        border_width=1,
        margin=[ 3, 15 ,5 ,15 ],
    ),
    wallpaper=wallpaperPath + "mainbg",
    wallpaper_mode="fill",
)

screens = [main_screen]
