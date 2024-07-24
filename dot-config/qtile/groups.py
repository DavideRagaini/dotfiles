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
                screen_affinity=0,
                matches=[
                    Match(wm_instance_class=regex("[Ee]macs")),
                    Match(wm_class=regex("foot(client)?|[Aa]lacritty")),
                ],
            ),
            Group(
                name="2",
                position=2,
                layout="monadtall",
                exclusive=True,
                screen_affinity=1,
                matches=Match(
                    wm_class=regex(
                        "[Ff]irefox|[Ll]ibre[Ww]olf|[Bb]rave-browser",
                    ),
                ),
            ),
            Group(
                name="3",
                position=3,
                layout="max",
                exclusive=True,
                screen_affinity=1,
                matches=Match(
                    wm_class=regex(
                        "org.pwmt.zathura|[Zz]athura|sioyek|Evince|okular|ebook-viewer|calibre-ebook-viewer|Nsxiv|imv",
                    )
                ),
            ),
            Group(
                name="4",
                layout="max",
                position=4,
                screen_affinity=0,
                matches=[
                    Match(
                        wm_instance_class=regex("sun-awt-X11-XDialogPeer"),
                        title=regex("MATLAB Editor"),
                    ),
                    Match(
                        wm_instance_class=regex("Matlab-GLEE"),
                        wm_class=regex("MATLAB R202[0-9](a|b) Update [0-9]+"),
                    ),
                    Match(
                        wm_instance_class=regex("sun-awt-X11-XFramePeer"),
                        title=regex("MATLAB R202[0-9](a|b) - academic use"),
                    ),
                    Match(
                        title=regex("MathWorks Product Installer"),
                    ),
                    Match(
                        wm_class=regex("MATLABWindow"),
                        title=regex("Control System Designer*"),
                    ),
                    Match(
                        wm_class=regex(
                            "VirtualBox Machine|MATLAB R202[0-9](a|b) - academic use|MATLAB R202[0-9][a-b]|MATLABWindow|Matlab-GLEE"
                        ),
                        wm_instance_class=regex("sun-awt-X11-XDialogPeer"),
                    ),
                ],
            ),
            Group(
                name="5",
                position=5,
                layout="max",
                screen_affinity=0,
                matches=[
                    Match(
                        wm_instance_class=regex("MATLABWindow"),
                        wm_class=regex("MATLABWindow"),
                        title=regex("Variable-references - Signal Editor"),
                    ),
                ],
            ),
            Group(
                name="6",
                position=6,
                layout="ratiotile",
                screen_affinity=0,
                matches=[
                    Match(
                        wm_instance_class=regex("sun-awt-X11-XFramePeer"),
                        title=regex(" "),
                    ),
                    Match(
                        wm_instance_class=regex("MATLABWindow"),
                        wm_class=regex("MATLABWindow"),
                        title=regex("Variable-references - Signal Editor"),
                    ),
                    Match(
                        title=regex(
                            "asbQuadcopter/Command/Signal Editor * - Simulink academic use"
                        ),
                        wm_instance_class=regex("Matlab-GLEE"),
                        wm_class=regex("MATLAB R202[0-9](a|b) Update [0-9]+"),
                    ),
                    Match(
                        title=regex("Block Parameters: Position/Attitude Reference"),
                        wm_instance_class=regex("Matlab-GLEE"),
                        wm_class=regex("MATLAB R202[0-9](a|b) Update [0-9]+"),
                    ),
                    Match(
                        title=regex("Block Parameters: Position/Attitude Reference"),
                        wm_instance_class=regex("Matlab-GLEE"),
                        wm_class=regex("MATLAB R202[0-9](a|b) Update [0-9]+"),
                    ),
                    Match(wm_instance_class=regex("sun-awt-X11-XFramePeer")),
                ],
            ),
            Group(
                name="7",
                position=7,
                screen_affinity=0,
                layout="plasma",
            ),
            Group(
                name="8",
                position=8,
                layout="max",
                screen_affinity=0,
                matches=[
                    Match(
                        wm_class=regex(
                            "qBittorrent|calibre-gui|calibre|syncthingtray|qutebrowser",
                        ),
                    ),
                    Match(wm_class= "org.keepassxc.KeePassXC"),
                    Match(
                        wm_class=regex(
                            "teams-for-linux|microsoft teams - preview|Ferdium|KeePassXC",
                        ),
                    ),
                ],
            ),
            Group(
                name="9",
                position=9,
                layout="max",
                screen_affinity=0,
                # exclusive=True,
                matches=Match(wm_class=regex("mpv|Kodi")),
            ),
            Group(
                name="0",
                position=10,
                layout="columns",
            ),
        ]
