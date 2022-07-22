;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ======================= User Interface ============= {{{
(let ((x (system-name)))
  (cond
   ((string-equal x "VoiD") (setq dr\font-size 30) (global-activity-watch-mode t))
   ((string-equal x "vDR") (setq dr\font-size 16) (global-activity-watch-mode t))
   ((or (string-equal x "void") (string-equal x "NT")) (setq dr\font-size 12))
   ((or (string-equal x "tinkerboard") (string-equal x "bagaro")) (setq dr\font-size 15))))

(setq user-full-name "Davide Ragaini"
      user-mail-address "ragainidavide@gmail.com"
      doom-font (font-spec :family "Iosevka" :size dr\font-size)
      doom-variable-pitch-font (font-spec :family "Liberation Sans" :size (+ dr\font-size 2))
      doom-serif-font (font-spec :family "Liberation Sans" :size (+ dr\font-size 2))
      doom-unicode-font (font-spec :family "Linux Libertine O" :size dr\font-size)
      doom-big-font (font-spec :family "Iosevka" :size (+ dr\font-size 10))
      doom-theme 'doom-dracula
      display-line-numbers-type t
      scroll-margin 2
      confirm-kill-emacs nil)

(global-visual-line-mode t)
(blink-cursor-mode 1)
(global-auto-revert-mode 1)
(global-evil-vimish-fold-mode 1)

(set-face-foreground 'vertical-border "magenta")
(set-frame-parameter (selected-frame) 'alpha '(90))
(add-to-list 'default-frame-alist '(alpha . (80)))

(set-popup-rules!
  '(("^ \\*" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
    ("^\\*"  :slot 1 :vslot -1 :select t)
    ("^\\*Completions" :slot -1 :vslot -2 :ttl 0)
    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)
    ("^\\*Help" :slot -1 :size 0.2 :select t)
    ("^\\*Man" :slot -1 :size 0.4 :side bottom :select t)
    ("^\\*helpful" :slot -1 :size 0.35 :select t)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :size 0.3 :ttl 0 :quit t)
    ("^\\*eww" :slot -1 :side left :quit nil :size 0.5 :select t)
    ("^\\*Python*" :slot -1 :side left :quit nil :size 0.5 :select t)
    ("^\\*doom:"AA :size 0.35 :select t :modeline t :quit t :ttl 5)))
;; }}}
;; ======================= Functions ============= {{{
(defun toggle-theme ()
  "Light theme toggles"
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-dracula)
      (load-theme 'doom-nord-light)
    (load-theme 'doom-dracula)))
(global-set-key [f5] 'toggle-theme)
;; }}}
;; ======================= Dired ============= {{{
(use-package! dired
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;; :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
;; }}}
;; ======================= Org ============= {{{
(setq org-directory "~/Org"
      org-agenda-files
      '("~/Org/Tasks.org"
        "~/Org/Habits.org"
        "~/Org/Learn.org"
        "~/Org/Birthdays.org")
      org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+"))
      org-superstar-headline-bullets-list '("" "" "" "" "" "" "" "" "")
      org-superstar-leading-fallback ?\s);; Hide away leading stars on terminal.

(use-package! org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("❗" "⬆" "⬇" )))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.6))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.4))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.3))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.1))))
 ;; '(org-document-title ((t (:inherit outline-1 :height 1.25))))
 )

(use-package! org
  :commands (org-capture org-agenda)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))
;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 60)

(setq org-tag-alist
      '((:startgroup)
                                        ; Put mutually exclusive tags here
        ("@errand" . ?E)
        ("@home" . ?H)
        ("@work" . ?W)
        (:endgroup)
        (:startgroup)
        ("ME" . ?i)
        ("OTHERS" . ?o)
        (:endgroup)
        ("BATCH" . ?b)
        ("FIX" . ?f)
        ("SETUP" . ?u)
        ("IMPROVE" . ?m)
        ("READ" . ?r)
        ("STUDY" . ?s)
        ("TRY" . ?t)
        ("THINK" . ?T)
        ("WATCH" . ?w)))

(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("Tasks.org" :maxlevel . 1)))
;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; (add-hook 'org-mode-hook 'org-appear-mode)

(setq org-capture-templates
  `(("t" "Tasks / Projects")
    ("tt" "Task" entry (file+olp "~/Org/Tasks.org" "---------- Inbox ----------")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
    ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

    ("n" "Note Entries")
    ("np" "Protocol" entry (file+olp "~/Org/Tasks.org" "---------- Inbox ----------")
        "* TODO %?[[%:link][%:description]] %U\n%i\n" :prepend t)
    ("nl" "Protocol Link" entry (file+olp "~/Org/Tasks.org" "---------- Inbox ----------")
        "* TODO %?[[%:link][%:description]] %U\n" :prepend t)
    ("nx" "Protocol Link from Clipboard" entry (file+olp "~/Org/Tasks.org" "---------- Inbox ----------")
        "* TODO %?%x %U\n" :prepend t)
    ("nb" "Protocol Link Blank" entry (file+olp "~/Org/Tasks.org" "---------- Inbox ----------")
        "* TODO %? %U\n" :prepend t)

    ("j" "Journal Entries")
    ("jj" "Journal" entry
         (file+olp+datetree "~/Org/Journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
    ("jm" "Meeting" entry
         (file+olp+datetree "~/Org/Journal.org")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)


    ("t" "Protocol Link Blank" entry (file+olp "~/Org/Tasks.org" "---------- Inbox ----------")
        "* TODO %? %U\n" :prepend t)
    ;; ("w" "Workflows")
    ;; ("we" "Checking Email" entry (file+olp+datetree ,(dw/get-todays-journal-file-name))
    ;;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
    ("s" "Scramble Capture")
    ("sd" "Dataset" table-line (file+headline "~/Org/Scrambled.org" "Dataset:")
     "| %^{Color} | %^{Time} | %^{Declared} | %^{Extracted} | %U |" :kill-buffer t)
    ("si" "Scrambled Idea" table-line (file+headline "~/Org/Scrambled.org" "Inbox")
         "* IDEA %?  %U\n" :empty-lines 1)
    ("st" "Scrambled Todo" table-line (file+headline "~/Org/Scrambled.org" "Inbox")
         "* TODO %?  %U\n" :empty-lines 1)

    ("m" "Metrics Capture")
    ("mw" "Weight" table-line (file+headline "~/Org/Metrics.org" "Weight")
     "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
;; }}}
;; ======================= Spell Checking ============= {{{
(after! ispell
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_GB,en_US,it_IT")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,en_US,it_IT")
  (setq ispell-personal-dictionary "~/.local/share/hunspell_personal"))
;; }}}

;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
