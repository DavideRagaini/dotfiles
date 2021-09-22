import os
import dracula

config.load_autoconfig()
dracula.blood(c, {"spacing": {"vertical": 6, "horizontal": 8}})

config.bind(",a", ":hint links spawn --detach dmpv aplay '{hint-url}'")
config.bind(",e", ":hint links spawn --detach dmpv enqueue '{hint-url}'")
config.bind(",p", ":hint links spawn --detach dmpv eplay '{hint-url}'")
config.bind(",c", ":hint links spawn clipf '{hint-url}'")
config.bind(",t", ":hint links spawn transadd '{hint-url}'")
config.bind("<y><o>", "yank inline [[{url}][{title}]]")
config.bind(",V", "hint links spawn " + os.environ["BROWSER"] + ' "{hint-url}"')
config.bind(",v", 'hint links spawn funnel "{hint-url}"')
config.bind(",\\", 'spawn dmenuhandler "{url}"')

config.bind(',s', 'config-cycle content.user_stylesheets ~/.config/qutebrowser/dracula.css ""')

c.url.searchengines = {
    "DEFAULT": "https://searx.nevrlands.de/search?q={}",
    "a": "https://wiki.archlinux.org/?search={}",
    "d": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?q={}",
    "s": "https://startpage.com/sp/search?query={}",
    "w": "https://en.wikipedia.org/?search={}",
    "y": "https://www.youtube.com/results?search_query={}",
}

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
