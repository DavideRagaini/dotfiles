import dracula

config.load_autoconfig()
dracula.blood(c, {"spacing": {"vertical": 6, "horizontal": 8}})

config.bind(",a", ":hint links spawn --detach dmpv aplay '{hint-url}'")
config.bind(",e", ":hint links spawn --detach dmpv enqueue '{hint-url}'")
config.bind(",p", ":hint links spawn --detach dmpv eplay '{hint-url}'")
config.bind(",c", ":hint links spawn clipf '{hint-url}'")
config.bind(",t", ":hint links spawn transadd '{hint-url}'")

c.url.searchengines = {
    "DEFAULT": "https://searx.nevrlands.de/search?q={}",
    "a": "https://wiki.archlinux.org/?search={}",
    "d": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?q={}",
    "s": "https://startpage.com/sp/search?query={}",
    "w": "https://en.wikipedia.org/?search={}",
    "y": "https://www.youtube.com/results?search_query={}",
}
