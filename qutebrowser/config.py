config.load_autoconfig()

c.fonts.default_family = "DejaVu Sans Mono, Noto Sans"
c.fonts.default_size = "12pt"
c.colors.webpage.darkmode.enabled = False
c.colors.webpage.preferred_color_scheme = "dark"

c.url.start_pages = "hidden"
c.url.default_page = "hidden"

c.editor.command = ["st", "-e", "nvim", "{file}"]

# turn up proxy
config.bind('<Ctrl-p>', 'set content.proxy socks5://hidden ;; message-info "Proxy: enabled (SOCKS5 @ hidden)"')

# turn off proxy
config.bind('<Ctrl-Shift-p>', 'set content.proxy system ;; message-info "Proxy: disabled (system)"')


c.url.searchengines = {
    'DEFAULT':      'hidden',

    '!dd':  'https://duckduckgo.com/?ia=web&q={}',
    '!ppx': 'https://www.perplexity.ai/search?q={}',
    '!dru': 'https://ru.wiktionary.org/wiki/{}',
    '!w':   'https://en.wikipedia.org/wiki/{}',

    "!tru": "https://translate.google.com/?sl=auto&tl=ru&text={}&op=translate",
    "!ten": "https://translate.google.com/?sl=auto&tl=en&text={}&op=translate",

    '!gg':  'https://www.google.com/search?q={}',
    '!gm':  'https://www.google.com/maps/search/{}',
    # '!r':       'https://www.reddit.com/search?q={}',

    '!aur': 'https://aur.archlinux.org/packages?O=0&K={}',
    '!pkg': 'https://archlinux.org/packages/?q={}',
    '!gh':  'https://github.com/search?o=desc&q={}&s=stars',
    '!so':  'https://stackoverflow.com/search?q={}',
    '!sx':  'https://stackexchange.com/search?q={}',

    '!yt':  'https://www.youtube.com/results?search_query={}'
}

c.content.autoplay = False        # didn't auto-open video/audio
c.content.javascript.enabled = True
c.content.blocking.enabled = True
c.content.blocking.method = "adblock"

c.content.blocking.adblock.lists = [
        # url here
]


c.content.cookies.accept = 'no-3rdparty' # private
c.content.headers.referer = 'same-domain'
c.content.headers.accept_language = 'en-US,en;q=0.9'


c.confirm_quit = ["downloads"]   
c.downloads.location.directory = "~/Downloads"
c.downloads.position = "bottom"
c.auto_save.session = True       
c.session.lazy_restore = True    


c.input.insert_mode.auto_leave = True
c.input.insert_mode.auto_load = True
c.hints.chars = "asdfghjkl"  # dwm-like keyboard
