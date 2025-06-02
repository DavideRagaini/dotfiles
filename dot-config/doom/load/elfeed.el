;;; ../../sources/dotfiles/dot-config/doom/load/rssfeed.el -*- lexical-binding: t; -*-
;; TODO Scoring system
;; https://kitchingroup.cheme.cmu.edu/blog/2017/01/05/Scoring-elfeed-articles/
;; https://yewtu.be/watch?v=rvWbUGx9U5E
;; TODO [[https://gist.github.com/alphapapa/80d2dba33fafcb50f558464a3a73af9a][Elfeed config · GitHub]]
;; [[https://koustuvsinha.com/post/emacs_research_workflow/][A workflow for reading, managing and discovering ML research papers with Emacs | Koustuv Sinha]]
;; [[https://github.com/skeeto/elfeed/issues/222][User Interface suggestions · Issue #222 · skeeto/elfeed · GitHub]]
;; TODO [[https://github.com/sp1ff/elfeed-score][GitHub - sp1ff/elfeed-score: Gnus-style scoring for elfeed]]

(defun elfeed-prefs ()
  "Set elfeed default preferences"
  (interactive)
  (setq elfeed-use-curl t
        elfeed-curl-max-connections 8
        elfeed-enclosure-default-dir "~/dwn"
        elfeed-sort-order 'descending
        elfeed-search-clipboard-type 'CLIPBOARD
        elfeed-search-title-max-width 150
        elfeed-search-title-min-width 100
        elfeed-search-trailing-width 60
        elfeed-show-truncate-long-urls t
        elfeed-show-unique-buffers t
        elfeed-search-filter "@2-week-ago +unread"
        rmh-elfeed-org-files '("~/.config/doom/load/elfeed.org")
        shr-max-image-proportion 0.5
        flycheck-global-modes '(not . (elfeed-search-mode))
        ;; https://old.reddit.com/r/orgmode/comments/i6hl8b/image_preview_size_in_org_mode/
        ;; shr-inhibit-images t
        )
  )
(elfeed-prefs)

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
    (let ((l '(w x o d unread)))
      (while l
        (elfeed-search-untag-all (pop l))
        (forward-line -1)
        ))
    (elfeed-search-toggle-all mytag)))

(defun elfeed-clear-tags ()
  "Returns a function that tags an elfeed entry or selection as MYTAG"
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (let ((l '(w x o d unread)))
      (while l
        (elfeed-search-untag-all (pop l))
        (forward-line -1)
        ))
    (forward-line 1)
    ))

;;;###autoload
(defun elfeed-open-in-eww (entry)
  "Display the currently selected item in eww."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (when (derived-mode-p 'elfeed-search-mode))
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

;;;###autoload
(defun elfeed-download-audio (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (elfeed-exclusive-tag-selection-as 'd)
      (call-process-shell-command (format "ts yt-dlp --config-locations ~/.config/yt-dlp/config -x \"%s\"" link) nil 0)
      )))

;;;###autoload
(defun elfeed-download-video (entry)
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (let ((link (elfeed-entry-link entry)))
      (elfeed-exclusive-tag-selection-as 'd)
      (call-process-shell-command (format "ts yt-dlp \"%s\"" link) nil 0)
      )))

;; prot-elfeed.el
(evil-define-key 'normal elfeed-show-mode-map
  ;; (kbd "J") 'elfeed-goodies/split-show-next
  ;; (kbd "K") 'elfeed-goodies/split-show-prev
  (kbd "; e") 'elfeed-open-in-eww
  (kbd "; a") 'elfeed-open-dmpv-append
  (kbd "; f") 'elfeed-open-dmpv-aplay
  (kbd "; v") 'elfeed-open-mpv
  (kbd "; d") 'elfeed-download-audio
  (kbd "; D") 'elfeed-download-video
  )

(evil-define-key 'normal elfeed-search-mode-map
  (kbd "C-r") 'elfeed-update
  (kbd "; e") 'elfeed-open-in-eww
  (kbd "; a") 'elfeed-open-dmpv-append
  (kbd "; f") 'elfeed-open-dmpv-aplay
  (kbd "; v") 'elfeed-open-mpv
  (kbd "; d") 'elfeed-download-audio
  (kbd "; D") 'elfeed-download-video
  ;;
  (kbd "; h") (elfeed-tag-selection-as 'h)
  (kbd "; r") (elfeed-clear-tags)
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
  (kbd ". l") (lambda () (interactive) (elfeed-search-set-filter "+l"))
  (kbd ". d") (lambda () (interactive) (elfeed-search-set-filter "+d"))
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
  (defface watch-elfeed-entry     '((t :background "#060")) "watch") ;; green
  (push '(w watch-elfeed-entry) elfeed-search-face-alist)
  (defface to_download-elfeed-entry     '((t :background "#808")) "to_download") ;; magenta
  (push '(l to_download-elfeed-entry) elfeed-search-face-alist)
  (defface download-elfeed-entry     '((t :background "#008")) "downloaded") ;; cyan
  (push '(d download-elfeed-entry) elfeed-search-face-alist)
  )

;; [[https://anonymousoverflow.privacyfucking.rocks/exchange/emacs/questions/18008/startup-emacs-daemon-with-elfeed-rss-feed-reader-buffer][Startup Emacs Daemon With Elfeed (RSS Feed Reader) Buffer | AnonymousOverflow]]
(defun dr/start-elfeed ()
  (interactive)
  (progn
    ;; (doom/load-session "~/.config/emacs/.local/etc/workspaces/float")
    (run-at-time nil (* 1 60 60) #'elfeed-update)
    (elfeed)
    (elfeed-prefs)
    )
  )
;; (add-hook 'server-visit-hook 'start-elfeed)
;; (add-hook 'after-make-frame-functions 'start-elfeed)


;; === Fetch Timings === {{{
;; (add-hook 'elfeed-update-hooks
;;           (lambda (feed)
;;             (elfeed-log 'info "feed: %s: %f"
;;                         feed
;;                         (- (float-time)
;;                            (cdr (assoc feed my/elfeed-update-times))))))

;; (defvar my/elfeed-update-times nil)

;; (cl-loop with elfeed--inhibit-update-init-hooks = t
;;          for feed in (elfeed-feed-list)
;;          for time = (float-time)
;;          do
;;          (push (cons feed time) my/elfeed-update-times)
;;          (elfeed-update-feed feed))
;; }}}


;; === [[https://github.com/skeeto/elfeed/issues/293][Suggestion: Only redraw search buffer after refresh completes · Issue #293 · skeeto/elfeed · GitHub]] === {{{
;; (defvar ap/elfeed-update-complete-hook nil
;;   "Functions called with no arguments when `elfeed-update' is finished.")

;; (defvar ap/elfeed-updates-in-progress 0
;;   "Number of feed updates in-progress.")

;; (defvar ap/elfeed-search-update-filter nil
;;   "The filter when `elfeed-update' is called.")

;; (defun ap/elfeed-update-complete-hook (&rest ignore)
;;   "When update queue is empty, run `ap/elfeed-update-complete-hook' functions."
;;   (when (= 0 ap/elfeed-updates-in-progress)
;;     (run-hooks 'ap/elfeed-update-complete-hook)))

;; (add-hook 'elfeed-update-hooks #'ap/elfeed-update-complete-hook)

;; (defun ap/elfeed-update-message-completed (&rest _ignore)
;;   (message "Feeds updated"))

;; (add-hook 'ap/elfeed-update-complete-hook #'ap/elfeed-update-message-completed)

;; (defun ap/elfeed-search-update-restore-filter (&rest ignore)
;;   "Restore filter after feeds update."
;;   (when ap/elfeed-search-update-filter
;;     (elfeed-search-set-filter ap/elfeed-search-update-filter)
;;     (setq ap/elfeed-search-update-filter nil)))

;; (add-hook 'ap/elfeed-update-complete-hook #'ap/elfeed-search-update-restore-filter)

;; (defun ap/elfeed-search-update-save-filter (&rest ignore)
;;   "Save and change the filter while updating."
;;   (setq ap/elfeed-search-update-filter elfeed-search-filter)
;;   (setq elfeed-search-filter "#0"))

;; ;; NOTE: It would be better if this hook were run before starting the feed updates, but in
;; ;; `elfeed-update', it happens afterward.
;; (add-hook 'elfeed-update-init-hooks #'ap/elfeed-search-update-save-filter)

;; (defun ap/elfeed-update-counter-inc (&rest ignore)
;;   (cl-incf ap/elfeed-updates-in-progress))

;; (advice-add #'elfeed-update-feed :before #'ap/elfeed-update-counter-inc)

;; (defun ap/elfeed-update-counter-dec (&rest ignore)
;;   (cl-decf ap/elfeed-updates-in-progress)
;;   (when (< ap/elfeed-updates-in-progress 0)
;;     ;; Just in case
;;     (setq ap/elfeed-updates-in-progress 0)))

;; (add-hook 'elfeed-update-hooks #'ap/elfeed-update-counter-dec)
;; }}}
