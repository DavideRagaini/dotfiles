;;; ../../sources/dotfiles/dot-config/doom/load/rssfeed.el -*- lexical-binding: t; -*-

(require 'elfeed)
(require 'elfeed-goodies)

(elfeed-goodies/setup)

(setq elfeed-use-curl t
      elfeed-curl-max-connections 10
      elfeed-enclosure-default-dir "~/dwn"
      elfeed-sort-order 'descending
      elfeed-search-clipboard-type 'CLIPBOARD
      elfeed-search-title-max-width 70
      elfeed-search-title-min-width 30
      elfeed-search-trailing-width 25
      elfeed-show-truncate-long-urls t
      elfeed-show-truncate-long-urls t
      elfeed-show-unique-buffers t
      elfeed-goodies/entry-pane-size 0.5
      )

(setq
 elfeed-feeds
 '(
   "https://dyn.keepa.com/v2/user/rss/?feed=3s870pff177ohgninibgq9g0jpbu9n5r"
   ;; Blogs
   ( "https://xeiaso.net/blog.rss" Computer )
   ( "https://opensource.com/taxonomy/term/6803/feed" Linux )
   ( "https://www.topbug.net/blog/category/unix-variants/gnu-linux/feed/" Linux )
   ( "https://www.righto.com/feeds/posts/default" Electronics )
   ;; Podcasts
   ( "https://yewtu.be/feed/channel/UCMOiTfbUXxUFqJJtCQGHrrA" Podcasts ) ;; "~Michele Boldrin"
   ( "https://yewtu.be/feed/channel/UCa2C45YKJGkXr9jQV2TYAkA" Podcasts ) ;; "~Mirko Campochiari"
   ( "https://yewtu.be/feed/channel/UCeieWsvQm00yw4GzBNLzJPw" Podcasts STEM ) ;; "~Avvocato dell'atomo"
   ( "https://yewtu.be/feed/channel/UCrdEJmK5bgFte04-UF7o29Q" Podcasts ) ;; "~Liberi Oltre"
   ( "https://yewtu.be/feed/channel/UCNSFH6m4JlzexDWxJSniqMg" Podcasts ) ;; "~Agorà"
   ( "https://yewtu.be/feed/channel/UCWKJtC_ekM-CUg3kB9-5gZw" Podcasts ) ;; "~Stem"
   ( "https://yewtu.be/feed/channel/UCl2mFZoRqjw_ELax4Yisf6w" Podcasts Tech ) ;; "~Luis Rossman"
   ( "https://yewtu.be/feed/channel/UCUkJ4Ue84_RxoRwDY7cctww" Podcasts ) ;; "~ivan"
   ( "https://www.omnycontent.com/d/playlist/b92c52c1-f970-472d-8d97-ac9f008bd59f/2d4af355-a108-4fa6-8c4d-aca00094d203/b8b0f6a9-b865-4117-82d4-aca00098a8c4/podcast.rss" Podcasts )
   ( "https://www.omnycontent.com/d/playlist/b92c52c1-f970-472d-8d97-ac9f008bd59f/2d4af355-a108-4fa6-8c4d-aca00094d203/ac31b694-86b7-4123-953b-acfe008c889c/podcast.rss" Podcasts )
   ;; "---Linux---"
   ( "https://yewtu.be/feed/channel/UC7YOGHUfC1Tb6E4pudI9STA" Linux ) ;; "~Mental Outlaw"
   ( "https://yewtu.be/feed/channel/UCld68syR8Wi-GY_n4CaoJGA" Linux ) ;; "~BrodieRobertson"
   ( "https://yewtu.be/feed/channel/UCtYg149E_wUGVmjGz-TgyNA" Linux ) ;; "~Chris Titus Talk"
   ( "https://yewtu.be/feed/channel/UCg6gPGh8HU2U01vaFCAsvmQ" Linux ) ;; "~Chris Titus"
   ( "https://yewtu.be/feed/channel/UCVls1GmFKf6WlTraIb_IaJg" Linux ) ;; "~DistroTube"
   ( "https://yewtu.be/feed/channel/UCeZyoDTk0J-UPhd7MUktexw" Linux ) ;; "~LibrePhoenix"
   ( "https://yewtu.be/feed/channel/UC2eYFnH61tmytImy1mTYvhA" Linux ) ;; "~Luke Smith"
   ( "https://yewtu.be/feed/channel/UCsnGwSIHyoYN0kiINAGUKxg" Linux ) ;; "~Wolfgang"
   ( "https://yewtu.be/feed/channel/UC5UAwBUum7CPN5buc-_N1Fw" Linux ) ;; "~TLE"
   ( "https://podcast.thelinuxexp.com/@tlenewspodcast/feed.xml" Podcasts Linux ) ;; "~TLE Audio"
   ( "https://yewtu.be/feed/channel/UCmyGZ0689ODyReHw3rsKLtQ" Linux ) ;; "~Michael Tunnel"
   ( "https://yewtu.be/feed/channel/UCZ4HO8or08HUGUzA0w8Tagw" Linux ) ;; "~Flux Harmonic"
   ( "https://yewtu.be/feed/channel/UC9x0AN7BWHpCDHSm9NiJFJQ" Linux ) ;; "~NetworkChuck"
   ( "https://yewtu.be/feed/channel/UCR-DXc1voovS8nhAvccRZhg" Linux ) ;; "~Jeff Geerling"
   ( "https://yewtu.be/feed/channel/UCsd6hP-zzIkCpw8XGw7Osyw" Linux ) ;; "~Geerling Engineering"
   ( "https://yewtu.be/feed/channel/UCnDDucQDLncrauOCmanCIgw" Linux ) ;; "~Morro"
   ( "https://yewtu.be/feed/channel/UCOWcZ6Wicl-1N34H0zZe38w" Linux ) ;; "~Level1Linux"
   ;; "---Emacs---"
   ( "https://twitchrss.appspot.com/vodonly/susampal" Emacs Books )
   ( "https://yewtu.be/feed/channel/UCAiiOTio8Yu69c3XnR7nQBQ" Linux ) ;; "~System Crafters"
   ( "https://systemcrafters.net/rss/news.xml" Linux Blog ) ;; "~System Crafters"
   ( "https://yewtu.be/feed/channel/UCJetJ7nDNLlEzDLXv7KIo0w" Linux ) ;; "~Gavin Freeborn"
   ( "https://sachachua.com/blog/feed" Blog Emacs )
   ( "https://susam.net/maze/feed.xml" Blog Linux )
   ;; "---STEM---"
   ( "https://yewtu.be/feed/channel/UC1yNl2E66ZzKApQdRuTQ4tw" STEM ) ;; "~Sabine Hossenfelder"
   ( "https://yewtu.be/feed/channel/UCHnyfMqiRRG1u-2MsSQLbXA" STEM ) ;; "~Veritasium"
   ( "https://yewtu.be/feed/channel/UCj1VqrHhDte54oLgPG4xpuQ" STEM ) ;; "~Stuff Made Here"
   ( "https://yewtu.be/feed/channel/UCWizIdwZdmr43zfxlCktmNw" STEM ) ;; "~Alec Steele"
   ( "https://yewtu.be/feed/channel/UCYrX5FiWjiPd0JytMh6NX1Q" STEM ) ;; "~Kathy Loves Physics & History"
   ( "https://yewtu.be/feed/channel/UC6107grRI4m0o2-emgoDnAA" STEM ) ;; "~SmarterEveryDay"
   ( "https://yewtu.be/feed/channel/UC8VkNBOwvsTlFjoSnNSMmxw" STEM ) ;; "~SmarterEveryDay2"
   ( "https://yewtu.be/feed/channel/UCy0tKL1T7wFoYcxCe0xjN6Q" STEM ) ;; "~Technology Connections"
   ( "https://yewtu.be/feed/channel/UClRwC5Vc8HrB6vGx6Ti-lhA" STEM ) ;; "~Technology Connextras"
   ( "https://yewtu.be/feed/channel/UCWFKCr40YwOZQx8FHU_ZqqQ" STEM ) ;; "~JerryRigEverything"
   ;; "---Math---"
   ( "https://yewtu.be/feed/channel/UCYO_jab_esuFRV4b17AJtAw" Math ) ;; "~3Blue1Brown"
   ( "https://yewtu.be/feed/channel/UCv0nF8zWevEsSVcmz6mlw6A" Math ) ;; "~vcubingx"
   ( "https://yewtu.be/feed/channel/UCrlZs71h3mTR45FgQNINfrg" Math ) ;; "~Mathemaniac"
   ( "https://yewtu.be/feed/channel/UC1_uAIS3r8Vu6JjXWvastJg" Math ) ;; "~Mathologer"
   ( "https://yewtu.be/feed/channel/UCoxcjq-8xIDTYp3uz647V5A" Math ) ;; "~Numberphile"
   ;; "---Electronics---"
   ( "https://yewtu.be/feed/channel/UCS0N5baNlQWJCUrhCEo8WlA" Electronics ) ;; "~Ben Eater"
   ( "https://yewtu.be/feed/channel/UC6mIxFTvXkWQVEHPsEdflzQ" Electronics ) ;; "~GreatScott"
   ( "https://yewtu.be/feed/channel/UCJ0-OtVpF0wOKEqT2Z1HEtA" Electronics ) ;; "~ElectroBOOM"
   ( "https://yewtu.be/feed/channel/UC1O0jDlG51N3jGf6_9t-9mw" Electronics ) ;; "~Marco Reps"
   ( "https://yewtu.be/feed/channel/UCafxR2HWJRmMfSdyZXvZMTw" Electronics Music ) ;; "~LOOK MOM NO COMPUTER"
   ;; "---Computer Science---"
   ( "https://yewtu.be/feed/channel/UC9-y-6csu5WGm29I7JiwpnA" Computer_Science ) ;; "~Computerphile"
   ( "https://yewtu.be/feed/channel/UC8ENHE5xdFSwx71u3fDH5Xw" Computer_Science ) ;; "~ThePrimeagen"
   ( "https://yewtu.be/feed/channel/UCUyeluBRhGPCW4rPe_UvBZQ" Computer_Science ) ;; "~ThePrimeTime"
   ( "https://yewtu.be/feed/channel/UCVk4b-svNJoeytrrlOixebQ" Computer_Science ) ;; "~TheVimeagen"
   ( "https://yewtu.be/feed/channel/UCZ_cuJGBis0vi6U3bWmvDIg" Computer_Science ) ;; "~FaceDev"
   ( "https://yewtu.be/feed/channel/UCDY981jZta5C5A6kQXioGUg" Computer_Science VIM ) ;; "~denvaar"
   ( "https://yewtu.be/feed/channel/UCIYIsJWfps2RwOzJlhwnoEw" Computer_Science ) ;; "~Vojtěch Pröschl"
   ;; "---Tech---"
   ( "https://yewtu.be/feed/channel/UC4w1YQAJMWOz4qtxinq55LQ" Tech ) ;; "~Level1Tech"
   ( "https://yewtu.be/feed/channel/UChIs72whgZI9w6d6FhwGGHA" Tech ) ;; "~Gamer Nexus"
   ( "https://yewtu.be/feed/channel/UCXuqSBlHAE6Xw-yeJA0Tunw" Tech ) ;; "~Linus Tech Tips"
   ( "https://yewtu.be/feed/channel/UCO8DQrSp5yEP937qNqTooOw" Tech ) ;; "~Strange Parts"
   ( "https://yewtu.be/feed/channel/UCqL9sqfRCcIlqwazHpr9Ohg" Tech ) ;; "~Strange Parts Live YT"
   ;; "---Physics---"
   ( "https://yewtu.be/feed/channel/UCvBqzzvUBLCs8Y7Axb-jZew" Physics ) ;; "~Sixty Symbols"
   ( "https://yewtu.be/feed/channel/UCyE9-Zvq3xxWGS5Okf-TWwg" Physics ) ;; "~Random Physics"
   ;; "---Cuisine---"
   ( "https://yewtu.be/feed/channel/UC8IKIlUPgPZ_NA1jKbsMfXw" Cuisine ) ;; "~Scienza in Cucina"
   ( "https://yewtu.be/feed/channel/UCETyhmgxupv93Ix4VnIiQJQ" Cuisine ) ;; "~Italia Squisita"
   ( "https://yewtu.be/feed/channel/UCVjlpEjEY9GpksqbEesJnNA" Cuisine ) ;; "~Uncle Roger"
   ( "https://yewtu.be/feed/channel/UChBEbMKI1eCcejTtmI32UEw" Cuisine ) ;; "~Joshua Weissman"
   ( "https://yewtu.be/feed/channel/UCnB5HTIi44wDBD56KeT2hNA" Cuisine ) ;; "~Guga"
   ( "# https://yewtu.be/feed/channel/UCEfPHqcy3YcsvSMaBl1UhCQ" Cuisine ) ;; "~Esther Choi"
   ( "# https://yewtu.be/feed/channel/UC9TM3Lrth8MQjHrttZJZiEw" Cuisine ) ;; "~Adam Liaw"
   ( "# https://yewtu.be/feed/channel/UChrcDm7u2mF3II4F7idmXiQ" Cuisine ) ;; "~Chef Wang Gang"
   ( "https://yewtu.be/feed/channel/UCPzFLpOblZEaIx2lpym1l1A" Cuisine ) ;; "~Alex"
   ;; "---Science---"
   ( "https://yewtu.be/feed/channel/UCQPnCKNfzKn4OmPrx1KDWvg" Science ) ;; "~EntropyForLife"
   ( "https://yewtu.be/feed/channel/UC1D3yD4wlPMico0dss264XA" Science ) ;; "~NileBlue"
   ( "https://yewtu.be/feed/channel/UCFhXFikryT4aFcLkLw2LBLA" Science ) ;; "~NileRed"
   ( "https://yewtu.be/feed/channel/UCivA7_KLKWo43tFcCkFvydw" Science ) ;; "~Applied Science"
   ( "https://yewtu.be/feed/channel/UCu6mSoMNzHQiBIOCkHUa2Aw" Science ) ;; "~Cosy's Lab"
   ( "https://yewtu.be/feed/channel/UCJphwa8Wsgzsm1zJS4sm-mA" Science ) ;; "~Dario Bressanini"
   ( "https://yewtu.be/feed/channel/UCH-y44M0pvwaZx2rTq0rJoQ" Science ) ;; "~Barbascura"
   ( "https://yewtu.be/feed/channel/UCHi6Q3Z-5oJUC691WLlSntA" Science ) ;; "~Barbascura eXtra"
   ( "https://yewtu.be/feed/channel/UCqYPhGiB9tkShZorfgcL2lA" Science ) ;; "~What I've Learned"
   ( "https://yewtu.be/feed/channel/UCEIwxahdLz7bap-VDs9h35A" Science ) ;; "~Steve Mould"
   ;; "---Music---"
   ( "https://yewtu.be/feed/channel/UCyDZai57BfE_N0SaBkKQyXg" Music ) ;; "~Rob Scallon"
   ( "https://yewtu.be/feed/channel/UCjewxGh1Gx5i5Uzxn0v-TPw" Music ) ;; "~The Punk Rock MBA"
   ( "https://yewtu.be/feed/channel/UCi5YmM9sdTs7JxzP55D6THQ" Music ) ;; "~Finn Mckenty"
   ( "https://yewtu.be/feed/channel/UCSr_y4ax0ZOf4MNrVnZEH5A" Music ) ;; "~Mavrus"
   ( "http://www.ottonepesante.it/?feed=gigpress" Music ) ;; "~Ottone Pesante"
   ;; "---Cars---"
   ( "https://yewtu.be/feed/channel/UCx8aikojDTsbC_iS-204Y0w" Cars ) ;; "~Angelo Nero"
   ( "https://yewtu.be/feed/channel/UCFNLUhl6K-zVILHawerpDDA" Cars ) ;; "~Passione Motori"
   ;; "---Misc---"
   ( "https://yewtu.be/feed/channel/UClkUhTjFbQbtGfS14h9Vw5g" Chill ) ;;  "~Martijn Doolaard"
   ( "https://yewtu.be/feed/channel/UCxH0MkY6v0sjZzmMgW9TSRQ" Crime ) ;;  "~Elisa True Crime"
   ( "https://yewtu.be/feed/channel/UCtHaxi4GTYDpJgMSGy7AeSw" Random ) ;; "~Michael Reeves"
   ( "https://twitchrss.appspot.com/vodonly/yotobi" Backgrounds  ) ;; "~ytb"
   ( "https://twitchrss.appspot.com/vodonly/enkk" Backgrounds  ) ;; "~nk"
   ( "# https://yewtu.be/feed/channel/UC9RM-iSvTu1uPJb8X5yp3EQ" Misc ) ;; "~Wendover Productions"
   ( "# https://yewtu.be/feed/channel/UCR1D15p_vdP3HkrH8wgjQRw" Misc ) ;; "~Internet Historian"
   ;; # http://export.arxiv.org/api/query?search_query=cat:stat.ML&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending
   ;; # http://export.arxiv.org/api/query?search_query=cat:cs.LG&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending
   ;; # http://export.arxiv.org/api/query?search_query=cat:cs.CL&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending
   ;; "---Software---"
   ( "https://github.com/qtile/qtile/releases.atom" Software News )
   ( "https://keepassxc.org/blog/feed.xml" Software News )
   ;; https://github.com/LukeSmithxyz/voidrice/commits.atom
   ;; http://export.arxiv.org/api/query?search_query=cat:stat.ML&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending
   ;; http://export.arxiv.org/api/query?search_query=cat:cs.LG&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending
   ;; http://export.arxiv.org/api/query?search_query=cat:cs.CL&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending
   )
 )

(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "twitch\\.tv"
                              :add '(Stream))
          (elfeed-make-tagger :feed-url "yewtu\\.be"
                              :add '(Video)))

;;;###autoload
(defun elfeed-open-in-eww (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode) (+evil/window-vsplit-and-follow))
      (eww link)
      (rename-buffer (format "*elfeed eww %s*" link))
      )))

;;;###autoload
(defun elfeed-open-dmpv-append (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (shell-command (format "dmpv append \"%s\"" link))
      )))

;;;###autoload
(defun elfeed-open-dmpv-aplay (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (shell-command (format "dmpv aplay \"%s\"" link))
      )))

(evil-define-key 'normal elfeed-show-mode-map
  (kbd "J") 'elfeed-goodies/split-show-next
  (kbd "K") 'elfeed-goodies/split-show-prev
  (kbd ", e") 'elfeed-open-in-eww
  (kbd ", a") 'elfeed-open-dmpv-append
  (kbd ", A") 'elfeed-open-dmpv-aplay
  )

(evil-define-key 'normal elfeed-search-mode-map
  (kbd ", e") 'elfeed-open-in-eww
  (kbd ", a") 'elfeed-open-dmpv-append
  (kbd ", A") 'elfeed-open-dmpv-aplay
  )

(global-set-key (kbd "C-x w e") 'elfeed)
(global-set-key (kbd "C-x w r") 'elfeed-update)
