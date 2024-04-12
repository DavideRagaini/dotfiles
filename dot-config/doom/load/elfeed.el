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
      elfeed-search-title-max-width 130
      elfeed-search-title-min-width 70
      elfeed-search-trailing-width 40
      elfeed-show-truncate-long-urls t
      elfeed-show-unique-buffers t
      elfeed-search-filter "@2-weeks-ago"
      rmh-elfeed-org-files '("~/.config/doom/load/elfeed.org")
      shr-max-image-proportion 0.5
      ;; shr-inhibit-images t
      )
;; https://old.reddit.com/r/orgmode/comments/i6hl8b/image_preview_size_in_org_mode/
;;

(defun elfeed-tag-selection-as (mytag)
  "Returns a function that tags an elfeed entry or selection as MYTAG"
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (elfeed-search-toggle-all mytag)
    (forward-line -1)
    (elfeed-search-untag-all-unread)
    ))

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

;; (defun elfeed--read-tag (filter)
;;   (interactive)
;;   (elfeed-search-set-filter filter)
;;   (elfeed-search-update :force)
;;   )

(evil-define-key 'normal elfeed-show-mode-map
  ;; (kbd "J") 'elfeed-goodies/split-show-next
  ;; (kbd "K") 'elfeed-goodies/split-show-prev
  (kbd "; e") 'elfeed-open-in-eww
  (kbd "; a") 'elfeed-open-dmpv-append
  (kbd "; f") 'elfeed-open-dmpv-aplay
  )

(evil-define-key 'normal elfeed-search-mode-map
  (kbd "C-r") 'elfeed-update
  (kbd "; e") 'elfeed-open-in-eww
  (kbd "; a") 'elfeed-open-dmpv-append
  (kbd "; f") 'elfeed-open-dmpv-aplay
  ;;
  (kbd "; h") (elfeed-tag-selection-as 'h)
  (kbd "; w") (elfeed-tag-selection-as 'w)
  (kbd "; o") (elfeed-tag-selection-as 'o)
  (kbd "; s") (elfeed-tag-selection-as 's)
  (kbd "; x") (elfeed-tag-selection-as 'x)
  ;;
  (kbd ", p") (lambda () (interactive) (elfeed-search-set-filter "@2-month-ago +unread +F"))
  (kbd ", c") (lambda () (interactive) (elfeed-search-set-filter "@2-month-ago +unread +chill"))
  (kbd ", v") (lambda () (interactive) (elfeed-search-set-filter "@2-month-ago +unread +vid"))
  (kbd ", h") (lambda () (interactive) (elfeed-search-set-filter "@2-month-ago +unread +h"))
  (kbd ", w") (lambda () (interactive) (elfeed-search-set-filter "@2-month-ago +unread +w"))
  (kbd ", o") (lambda () (interactive) (elfeed-search-set-filter "@2-month-ago +unread +o"))
  (kbd ", s") (lambda () (interactive) (elfeed-search-set-filter "@2-month-ago +unread +s"))
  (kbd ", x") (lambda () (interactive) (elfeed-search-set-filter "@2-month-ago +unread +x"))
  )

(after! elfeed
  (defun elfeed-search-format-date (date)
    (format-time-string "%m/%d %H:%M" (seconds-to-time date)))

  ;; (add-hook! 'elfeed-show-mode-map #'writeroom-mode)
  ;; (add-hook! 'elfeed-search-mode-hook #'elfeed-update) ;; auto update when opening
  (add-hook! 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "yewtu\\.com" :add '(vid)))
  (add-hook! 'elfeed-new-entry-hook (elfeed-make-tagger :feed-url "twitchrss\\.appspot\\.com" :add '(str)))

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
  )

;; [[https://anonymousoverflow.privacyfucking.rocks/exchange/emacs/questions/18008/startup-emacs-daemon-with-elfeed-rss-feed-reader-buffer][Startup Emacs Daemon With Elfeed (RSS Feed Reader) Buffer | AnonymousOverflow]]
(defun dr/start-elfeed ()
  (interactive)
  (progn
    (elfeed)
    (run-at-time nil (* 4 60 60) #'elfeed-update)
    )
  )
;; (add-hook 'server-visit-hook 'start-elfeed)
;; (add-hook 'after-make-frame-functions 'start-elfeed)
