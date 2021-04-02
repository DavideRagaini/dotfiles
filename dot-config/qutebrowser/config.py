config.load_autoconfig()

config.bind(",w", ':hint links spawn winmpv play "{hint-url}"')
config.bind(",a", ':hint links spawn mpv "{hint-url}"')
config.bind(",q", ':hint links spawn winmpv queue "{hint-url}"')
config.bind(",da", ':hint links spawn dy audio "{hint-url}"')
config.bind(",dv", ':hint links spawn dy video "{hint-url}"')
config.bind(",dba", ':hint links spawn dy bestaudio "{hint-url}"')
config.bind(",dbv", ':hint links spawn dy bestvideo "{hint-url}"')
config.bind(",t", ':hint links spawn transadd "{hint-url}"')
config.bind(",i", ':hint links spawn getpic "{hint-url}"')

c.url.searchengines = {
    "DEFAULT": "https://searx.nevrlands.de/search?q={}",
    "dg": "https://duckduckgo.com/?q={}",
    "g": "https://www.google.com/search?q={}",
    "w": "https://en.wikipedia.org/?search={}",
    "y": "https://www.youtube.com/results?search_query={}",
    "aw": "https://wiki.archlinux.org/?search={}",
}

import dracula

dracula.blood(c, {"spacing": {"vertical": 6, "horizontal": 8}})
