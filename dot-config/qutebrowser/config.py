config.load_autoconfig()
# ======================= Imports ============= {{{
import dracula  # User Interface
import os  # User Defined Bindings

# }}}
# ======================= User Defined Bindings ============= {{{
config.bind(",a", "hint links spawn --detach dmpv aplay '{hint-url}'")
config.bind(",c", "hint links spawn clipf '{hint-url}'")
config.bind(",e", "hint links spawn --detach dmpv enqueue '{hint-url}'")
config.bind(",p", "hint links spawn --detach dmpv eplay '{hint-url}'")
config.bind(
    ",s", 'config-cycle content.user_stylesheets ~/.config/qutebrowser/dracula.css ""'
)
config.bind(",t", "hint links spawn transadd '{hint-url}'")
config.bind(",v", 'hint links spawn funnel "{hint-url}"')
config.bind(",z", "zoom 200")
config.bind(",V", "hint links spawn " + os.environ["BROWSER"] + ' "{hint-url}"')
config.bind(",\\", 'spawn dmenuhandler "{url}"')
config.bind("<y><o>", "yank inline [[{url}][{title}]]")
# }}}
# ======================= User Interface ============= {{{
dracula.blood(c, {"spacing": {"vertical": 4, "horizontal": 4}})
c.downloads.location.suggestion = "both"
c.downloads.position = "bottom"
c.downloads.remove_finished = 20000
c.scrolling.smooth = False
c.session.lazy_restore = True
c.statusbar.show = "in-mode"
c.tabs.last_close = "default-page"
c.tabs.position = "bottom"
c.tabs.show = "multiple"
c.tabs.show_switching_delay = 10000
startpage = os.environ["HOME"] + "/.local/src/startpage/index.html"
c.url.default_page = startpage
c.url.start_pages = [startpage]
c.window.title_format = (
    "{private}{perc}[{scroll_pos}]:{current_url}{title_sep}{current_title}"
)
# }}}
# ======================= Search Engines ============= {{{
c.url.searchengines = {"DEFAULT": "https://searx.nevrlands.de/search?q={}"}
c.url.searchengines = {
    "DEFAULT": "https://searx.nevrlands.de/search?q={}",
    "a": "https://wiki.archlinux.org/?search={}",
    "d": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?q={}",
    "s": "https://startpage.com/sp/search?query={}",
    "w": "https://en.wikipedia.org/?search={}",
    "y": "https://www.youtube.com/results?search_query={}",
}
# }}}
# ======================= Redline Insert Mode ============= {{{
# Awesome way to open vim from qutebrowser
c.editor.command = [
    os.environ["TERMINAL"],
    "-e",
    os.environ["EDITOR"],
    "-f",
    "{file}",
    "-c",
    "normal {line}G{column0}1",
]
config.bind("<Ctrl-h>", "fake-key <Backspace>", "insert")
config.bind("<Ctrl-a>", "fake-key <Home>", "insert")
config.bind("<Ctrl-e>", "fake-key <End>", "insert")
config.bind("<Ctrl-b>", "fake-key <Left>", "insert")
config.bind("<Mod1-b>", "fake-key <Ctrl-Left>", "insert")
config.bind("<Ctrl-f>", "fake-key <Right>", "insert")
config.bind("<Mod1-f>", "fake-key <Ctrl-Right>", "insert")
config.bind("<Ctrl-p>", "fake-key <Up>", "insert")
config.bind("<Ctrl-n>", "fake-key <Down>", "insert")
config.bind("<Mod1-d>", "fake-key <Ctrl-Delete>", "insert")
config.bind("<Ctrl-d>", "fake-key <Delete>", "insert")
config.bind("<Ctrl-w>", "fake-key <Ctrl-Backspace>", "insert")
config.bind("<Ctrl-u>", "fake-key <Shift-Home><Delete>", "insert")
config.bind("<Ctrl-k>", "fake-key <Shift-End><Delete>", "insert")
config.bind("<Ctrl-x><Ctrl-e>", "edit-text", "insert")
# }}}
# ======================= Security Settings ============= {{{
c.auto_save.session = True
c.content.autoplay = False
c.content.canvas_reading = False
c.content.cookies.accept = "no-3rdparty"
c.content.geolocation = False
c.content.headers.accept_language = "en-US,en;q=0.5"
c.content.headers.custom = {
    "accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
}
c.content.headers.user_agent = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:88.0) Gecko/20100101 Firefox/92.0"
)
c.content.javascript.can_access_clipboard = False
c.content.webgl = False
c.content.webrtc_ip_handling_policy = "default-public-interface-only"
# }}}
# ======================= Folders & Files ============= {{{
fileselect_cmd = ["st", "-e", "fu", "selection-path {}"]
c.fileselect.folder.command = fileselect_cmd
c.fileselect.multiple_files.command = fileselect_cmd
c.fileselect.single_file.command = fileselect_cmd
# }}}
