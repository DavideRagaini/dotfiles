;;; ../../sources/dotfiles/dot-config/doom/load/rssfeed.el -*- lexical-binding: t; -*-
;; TODO Scoring system
;; https://kitchingroup.cheme.cmu.edu/blog/2017/01/05/Scoring-elfeed-articles/
;; https://yewtu.be/watch?v=rvWbUGx9U5E
;; TODO [[https://gist.github.com/alphapapa/80d2dba33fafcb50f558464a3a73af9a][Elfeed config · GitHub]]
;; [[https://koustuvsinha.com/post/emacs_research_workflow/][A workflow for reading, managing and discovering ML research papers with Emacs | Koustuv Sinha]]
;; [[https://github.com/skeeto/elfeed/issues/222][User Interface suggestions · Issue #222 · skeeto/elfeed · GitHub]]
;; TODO [[https://github.com/sp1ff/elfeed-score][GitHub - sp1ff/elfeed-score: Gnus-style scoring for elfeed]]

(setq elfeed-use-curl t
      elfeed-curl-max-connections 6
      elfeed-enclosure-default-dir "~/dwn"
      elfeed-sort-order 'descending
      elfeed-search-clipboard-type 'CLIPBOARD
      elfeed-search-title-max-width 150
      elfeed-search-title-min-width 100
      elfeed-search-trailing-width 60
      elfeed-show-truncate-long-urls t
      elfeed-show-unique-buffers t
      elfeed-search-filter "@1-month-ago +unread"
      rmh-elfeed-org-files '("~/.config/doom/load/elfeed.org")
      shr-max-image-proportion 0.5
      ;; https://old.reddit.com/r/orgmode/comments/i6hl8b/image_preview_size_in_org_mode/
      ;; shr-inhibit-images t
      )

(defun elfeed-tag-selection-as (mytag)
  "Returns a function that tags an elfeed entry or selection as MYTAG"
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (elfeed-search-toggle-all mytag)
    (forward-line -1)
    (elfeed-search-untag-all-unread)
    ))

(defun elfeed-exclusive-tag-selection-as (mytag)
  "Returns a function that tags an elfeed entry or selection as MYTAG"
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (let ((l '(w x o unread)))
      (while l
        (elfeed-search-untag-all (pop l))
        (forward-line -1)
        ))
    (elfeed-search-toggle-all mytag)))

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
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (call-process-shell-command (format "dmpv append \"%s\"" link) nil 0)
      ;; (async-shell-command (format "dmpv append \"%s\"" link))
      )))

;;;###autoload
(defun elfeed-open-dmpv-aplay (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (call-process-shell-command (format "dmpv aplay \"%s\"" link) nil 0)
      )))

;;;###autoload
(defun elfeed-open-mpv (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (call-process-shell-command (format "mpv \"%s\"" link) nil 0)
      )))

;; prot-elfeed.el
(evil-define-key 'normal elfeed-show-mode-map
  ;; (kbd "J") 'elfeed-goodies/split-show-next
  ;; (kbd "K") 'elfeed-goodies/split-show-prev
  (kbd "; e") 'elfeed-open-in-eww
  (kbd "; a") 'elfeed-open-dmpv-append
  (kbd "; f") 'elfeed-open-dmpv-aplay
  (kbd "; v") 'elfeed-open-mpv
  )

(evil-define-key 'normal elfeed-search-mode-map
  (kbd "C-r") 'elfeed-update
  (kbd "; e") 'elfeed-open-in-eww
  (kbd "; a") 'elfeed-open-dmpv-append
  (kbd "; f") 'elfeed-open-dmpv-aplay
  (kbd "; v") 'elfeed-open-mpv
  ;;
  (kbd "; h") (elfeed-tag-selection-as 'h)
  (kbd "; s") (elfeed-tag-selection-as 's)
  (kbd "; w") (elfeed-exclusive-tag-selection-as 'w)
  (kbd "; o") (elfeed-exclusive-tag-selection-as 'o)
  (kbd "; x") (elfeed-exclusive-tag-selection-as 'x)
  ;;
  (kbd ". b") (lambda () (interactive) (elfeed-search-set-filter "=boldrin"))
  (kbd ". e") (lambda () (interactive) (elfeed-search-set-filter "=nk"))
  (kbd ". y") (lambda () (interactive) (elfeed-search-set-filter "=ytb"))
  (kbd ". a") (lambda () (interactive) (elfeed-search-set-filter "@2-week-ago"))
  (kbd ". r") (lambda () (interactive) (elfeed-search-set-filter "@2-week-ago +unread"))
  (kbd ". p") (lambda () (interactive) (elfeed-search-set-filter "+F"))
  (kbd ". c") (lambda () (interactive) (elfeed-search-set-filter "+chill"))
  (kbd ". h") (lambda () (interactive) (elfeed-search-set-filter "+h"))
  (kbd ". w") (lambda () (interactive) (elfeed-search-set-filter "+w"))
  (kbd ". o") (lambda () (interactive) (elfeed-search-set-filter "+o"))
  (kbd ". s") (lambda () (interactive) (elfeed-search-set-filter "+s"))
  (kbd ". x") (lambda () (interactive) (elfeed-search-set-filter "+x"))
  )

(after! elfeed
  (defun elfeed-search-format-date (date)
    (format-time-string "%y/%m/%d %H:%M" (seconds-to-time date)))

  ;; (add-hook! 'elfeed-show-mode-map #'writeroom-mode)
  (add-hook! 'elfeed-new-entry-hook (elfeed-make-tagger :before "2 month ago" :remove 'unread))

  (defface done-elfeed-entry      '((t :background "#666")) "done") ;; gray
  (push '(x done-elfeed-entry) elfeed-search-face-alist)
  (defface important-elfeed-entry '((t :background "#604")) "important") ;; purple
  (push '(h important-elfeed-entry) elfeed-search-face-alist)
  (defface ongoing-elfeed-entry   '((t :background "#851")) "ongoing") ;; orange
  (push '(o ongoing-elfeed-entry) elfeed-search-face-alist)
  (defface save-elfeed-entry      '((t :background "#330")) "saved") ;; yellow
  (push '(s saved-elfeed-entry) elfeed-search-face-alist)
  (defface watch-elfeed-entry     '((t :background "#262")) "watch") ;; green
  (push '(w watch-elfeed-entry) elfeed-search-face-alist)
  (defface watch-elfeed-entry     '((t :background "#262")) "watch") ;; green
  (push '(w watch-elfeed-entry) elfeed-search-face-alist)
  )

;; [[https://anonymousoverflow.privacyfucking.rocks/exchange/emacs/questions/18008/startup-emacs-daemon-with-elfeed-rss-feed-reader-buffer][Startup Emacs Daemon With Elfeed (RSS Feed Reader) Buffer | AnonymousOverflow]]
(defun dr/start-elfeed ()
  (interactive)
  (progn
    (doom/load-session "~/.config/emacs/.local/etc/workspaces/float")
    (run-at-time nil (* 2 60 60) #'elfeed-update)
    (elfeed)
    )
  )
;; (add-hook 'server-visit-hook 'start-elfeed)
;; (add-hook 'after-make-frame-functions 'start-elfeed)
