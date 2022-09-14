config.load_autoconfig()
# ======================= Imports ============= {{{
import dracula  # User Interface
from os import environ  # User Defined Bindings
# }}}
# ======================= User Defined Bindings ============= {{{
c.bindings.commands["normal"] = {
    ",a": "hint links spawn --detach dmpv aplay '{hint-url}'",
    ",c": "hint links spawn clipf '{hint-url}'",
    ",d": 'config-cycle content.user_stylesheets ~/.config/qutebrowser/dracula.css ""',
    ",e": "hint links spawn --detach dmpv enqueue '{hint-url}'",
    ",g": "hint links spawn --detach gallery-dl '{hint-url}'",
    ",m": "hint links spawn --detach dmpv music '{hint-url}'",
    ",p": "hint links spawn --detach dmpv eplay '{hint-url}'",
    ",s": "hint links spawn --detach streamlink --player mpv '{hint-url}' best",
    ",t": "hint links spawn transadd '{hint-url}'",
    # ",g": 'hint links spawn funnel "{hint-url}"',
    ",z": "zoom 200",
    ",V": 'hint links spawn + environ["BROWSER"] + {hint-url}',
    ",\\": 'spawn dmenuhandler "{url}"',
    "<y><o>": "yank inline [[{url}][{title}]]",
    "td": "config-cycle colors.webpage.darkmode.enabled true false;; restart",
}
# }}}
# ======================= User Interface ============= {{{
dracula.blood(c, {'spacing': {'vertical': 6, 'horizontal': 8}})
c.downloads.location.suggestion = "both"
c.downloads.position = "bottom"
# c.downloads.remove_finished = 20000
c.scrolling.smooth = False
c.session.lazy_restore = True
c.statusbar.show = "in-mode"
c.tabs.last_close = "default-page"
c.tabs.position = "bottom"
c.tabs.show = "switching"
startpage = "file://" + environ["HOME"] + "/.local/src/startpage/index.html"
c.url.default_page = startpage
c.url.start_pages = [startpage]
c.window.title_format = ("{private}{perc}[{scroll_pos}]:{current_url}{title_sep}{current_title}")
# }}}
# ======================= Search Engines ============= {{{
c.url.searchengines = {
    "DEFAULT": "https://searx.be/search?q={}",
    "a": "https://wiki.archlinux.org/?search={}",
    "d": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?q={}",
    "i": "https://searx.be/search?q={}&language=it-IT",
    "s": "https://startpage.com/sp/search?query={}",
    "u": "https://www.urbandictionary.com/define.php?term={}",
    "w": "https://en.wikipedia.org/?search={}",
    "y": "https://yewtu.be/search?q={}",
}
# }}}
# ======================= Redline Insert Mode ============= {{{
# Vim
# c.editor.command = [
#     environ["TERMINAL"],
#     "-e",
#     "nvim",   # environ["EDITOR"],
#     "-f",
#     "{file}",
#     "-c",
#     "normal {line}G{column0}1",
# ]
# Emacs
c.editor.command = [
    "emacsclient",
    "{file}",
    "+{line}:{column0}1",
    "-a", "'emacs'"
]
c.bindings.commands["insert"] = {
    "<Ctrl-h>": "fake-key <Backspace>",
    "<Ctrl-a>": "fake-key <Home>",
    "<Ctrl-e>": "fake-key <End>",
    "<Ctrl-j>": "fake-key <Left>",
    "<Ctrl-k>": "fake-key <Right>",
    "<Ctrl-d>": "fake-key <Delete>",
    "<Ctrl-u>": "fake-key <Shift-Home><Delete>",
    "<Ctrl-i>": "fake-key <Shift-End><Delete>",
    "<Ctrl-x><Ctrl-e>": "edit-text",
}
# }}}
# ======================= Security Settings ============= {{{
c.auto_save.session = True
c.content.autoplay = False
c.content.canvas_reading = False
c.content.cookies.accept = "no-3rdparty"
c.content.geolocation = False
c.content.headers.accept_language = "en-US,en;q=0.5"
c.content.headers.custom = { "accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" }
c.content.headers.user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/105.0.0.0 Safari/537.36"
c.content.javascript.can_access_clipboard = False
c.content.webgl = False
c.content.webrtc_ip_handling_policy = "default-public-interface-only"
c.content.blocking.method = "both"
# }}}
# ======================= Folders & Files ============= {{{
fileselect_cmd = ["st", "-e", "fu", "selection-path {}"]
c.fileselect.folder.command = fileselect_cmd
c.fileselect.multiple_files.command = fileselect_cmd
c.fileselect.single_file.command = fileselect_cmd
# }}}
# ======================= Dark Mode ============= {{{
# c.colors.webpage.darkmode.enabled = True
c.colors.webpage.darkmode.algorithm = "lightness-hsl"
c.colors.webpage.darkmode.contrast = -0.022
c.colors.webpage.darkmode.threshold.text = 150
c.colors.webpage.darkmode.threshold.background = 100
c.colors.webpage.darkmode.policy.images = "always"
c.colors.webpage.darkmode.grayscale.images = 0.35
# }}}
