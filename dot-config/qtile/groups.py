from libqtile.config import Group, Match
from re import compile as regex

#  󰇮   󰈙     
class Groups(object):
    def init_groups(self):
        return [
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
