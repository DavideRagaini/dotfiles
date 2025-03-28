config.load_autoconfig()
# ======================= Imports ============= {{{
import dracula  # User Interface
from os import environ  # User Defined Bindings
# }}}
# ======================= User Defined Bindings ============= {{{
c.bindings.commands["normal"] = {
    ",c": "hint links spawn clipf '{hint-url}'",
    ",dg": "hint links spawn --detach gallery-dl '{hint-url}'",
    ",dt": "hint links spawn transadd '{hint-url}'",
    ",mm": "hint links spawn --detach dmpv music '{hint-url}'",
    ",ms": "hint links spawn --detach dmpv save_music '{hint-url}'",
    ";a": "hint links spawn --detach dmpv append '{hint-url}'",
    ";v": "hint links spawn --detach dmpv aplay '{hint-url}'",
    ",ve": "hint links spawn --detach dmpv enqueue '{hint-url}'",
    ",vp": "hint links spawn --detach dmpv eplay '{hint-url}'",
    ",vs": "hint links spawn --detach streamlink --player mpv '{hint-url}' best",
    ",z": "zoom 200",
    ",V": 'hint links spawn + environ["BROWSER"] + {hint-url}',
    ",\\": 'spawn dmenuhandler "{url}"',
    "<y><o>": "yank inline [[{url}][{title}]]",
    "td": "config-cycle colors.webpage.darkmode.enabled true false",
    # "tdr": "config-cycle colors.webpage.darkmode.enabled true false;; restart",
    "tds": 'config-cycle content.user_stylesheets ~/.config/qutebrowser/dracula.css ""',
    # ",g": 'hint links spawn funnel "{hint-url}"',
    "xs": "config-cycle statusbar.show always never",
    "xt": "config-cycle tabs.show always never",
    "xx": "config-cycle tabs.show always never;; config-cycle statusbar.show always never"
    }
# }}}
# ======================= User Interface ============= {{{
dracula.blood(c, {'spacing': {'vertical': 6, 'horizontal': 8}})
c.downloads.location.directory = environ["HOME"] + "/dwn/qutebrowser"
c.downloads.location.suggestion = "both"
c.downloads.position = "bottom"
c.downloads.remove_finished = 30000
# c.downloads.open_dispatcher = ""
c.scrolling.smooth = False
c.session.lazy_restore = True
c.statusbar.show = "in-mode"
c.tabs.last_close = "default-page"
c.tabs.position = "bottom"
c.tabs.show = "switching"
startpage = "file://" + environ["HOME"] + "/.local/src/startpage/index.html"
c.url.default_page = startpage
c.url.start_pages = [startpage]
c.window.title_format = ("{private}{perc}[{scroll_pos}]:{current_title} {title_sep} {current_url}")
c.window.hide_decoration = True
c.fonts.default_size = "8pt"
c.fonts.default_family = "IosevkaTermSlab Nerd Font Propo"
# }}}
# ======================= Search Engines ============= {{{
c.url.searchengines = {
    "DEFAULT": "https://searx.be/search?q={}",
    "a": "https://wiki.archlinux.org/?search={}",
    "d": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?q={}",
    "i": "https://searx.be/search?q={}&language=it-IT",
    "l": "https://libgen.is/search.php?req={}&open=0&res=100&view=detailed&phrase=1&column=title",
    "no": "https://search.nixos.org/options?packages&query={}",
    "np": "https://search.nixos.org/packages?packages&query={}",
    "nw": "https://nixos.wiki/index.php?search={}&title=Special%3ASearch&profile=default&fulltext=1",
    "hm": "https://mipmip.github.io/home-manager-option-search/?query={}",
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
    "emacsclient", "-c"
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
c.content.blocking.method = "both"
c.content.canvas_reading = False
c.content.cookies.accept = "never"
c.content.geolocation = False
c.content.headers.accept_language = "en-US,en;q=0.5"
c.content.headers.custom = { "accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" }
c.content.headers.user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
c.content.javascript.clipboard = "none"
c.content.pdfjs = False
c.content.webgl = False
c.content.webrtc_ip_handling_policy = "default-public-interface-only"
# }}}
# ======================= Folders & Files ============= {{{
fileselect_cmd = [ environ["TERM"], "-e", "lf", "selection-path {}"]
c.fileselect.folder.command = fileselect_cmd
c.fileselect.multiple_files.command = fileselect_cmd
c.fileselect.single_file.command = fileselect_cmd
# }}}
# ======================= Dark Mode ============= {{{
c.colors.webpage.darkmode.algorithm = "lightness-cielab"
c.colors.webpage.darkmode.contrast = -0.022
c.colors.webpage.darkmode.policy.images = "never"
c.colors.webpage.darkmode.threshold.background = 100
c.colors.webpage.darkmode.threshold.foreground = 220
# }}}
# ======================= Containers ============= {{{
c.aliases['container-open'] = 'spawn --userscript container-open'
c.aliases['container-ls'] = 'spawn --userscript container-ls'
c.aliases['container-add'] = 'spawn --userscript container-add'
c.aliases['container-rm'] = 'spawn --userscript container-rm'

config.bind('C','spawn --userscript container-open')
config.bind('<Alt-c>','cmd-set-text -s :spawn --userscript container-open')
config.bind('<Alt-f>','hint links userscript container-open')
# }}}
