;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; ======================= User Interface ============= {{{
(cond ((string-equal (system-name) "VoiD")
        (setq dr/font-size 30)
        (global-activity-watch-mode t))
   ((string-equal (system-name) "vDR")
        (setq dr/font-size 16)
        (global-activity-watch-mode t))
   ((or (string-equal (system-name) "void") (string-equal (system-name) "NT"))
        (setq dr/font-size 12
                straight-disable-native-compile t))
   ((string-equal (system-name) "tinkerboard")
        (setq dr/font-size 14
                straight-disable-native-compile t))
   ((string-equal (system-name) "bagaro")
        (setq dr/font-size 14)))

(setq user-full-name "Davide Ragaini"
      user-mail-address "ragainidavide@gmail.com"
      doom-font (font-spec :family "Iosevka" :size dr/font-size)
      doom-variable-pitch-font (font-spec :family "Liberation Sans" :size (+ dr/font-size 2))
      doom-serif-font (font-spec :family "Liberation Sans" :size (+ dr/font-size 2))
      doom-unicode-font (font-spec :family "Linux Libertine O" :size dr/font-size)
      doom-big-font (font-spec :family "Iosevka" :size (+ dr/font-size 10))
      doom-theme 'doom-dracula
      display-line-numbers-type t
      scroll-margin 2
      whitespace-line-column 500
      org-clock-persist 'history
      org-agenda-include-diary t
      ;; org-agenda-include-inactive-timestamps t
      confirm-kill-emacs nil)

(global-visual-line-mode t)
(blink-cursor-mode 1)
(global-auto-revert-mode 1)
(global-evil-vimish-fold-mode 1)
(global-whitespace-mode 1)
(org-clock-persistence-insinuate)

(set-frame-parameter (selected-frame) 'alpha '(92))
(add-to-list 'default-frame-alist '(alpha . (92)))

(set-popup-rules!
  '(("^ \\*" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
    ("^\\*"  :slot 1 :vslot -1 :size #'+popup-shrink-to-fit :select t)
    ("^\\*Completions*"    :slot -1 :vslot -2 :ttl 0)
    ("^\\*Edit Formulas*" :side left   :size 0.35 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Help*"           :side bottom :size 0.25 :quit t   :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Man*"            :side right  :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Org Agenda*"    :side left   :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Python*"        :side left   :size 0.50 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Warnings*"       :side bottom :size 0.30 :quit t   :slot -1 :vslot -2 :ttl 0 :select nil)
    ("^\\*doom:*"        :side bottom :size 0.35 :quit t   :slot  1 :vslot  0 :ttl 5 :select t :modeline t)
    ("^\\*eshell*"        :side bottom :size 0.42 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*eww*"            :side left   :size 0.50 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*helpful*"        :side right  :size 0.33 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :side right :size 0.3 :ttl 5 :quit t)
    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)))
;; }}}
;; ======================= Functions ============= {{{
(defun dr/toggle-theme ()
  "Light theme toggles"
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-dracula)
      (load-theme 'doom-nord-light)
    (load-theme 'doom-dracula)))
(global-set-key [f5] 'dr/toggle-theme)

(defun dr/late-load ()
  (set-face-foreground 'vertical-border "magenta"))
(add-hook! 'doom-load-theme-hook #'dr/late-load)
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
      '("~/Org/Me/Tasks.org"
        "~/Org/Me/Habits.org"
        "~/Org/Me/Learn.org"
        "~/Org/Me/Birthdays.org")
      org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+"))
      org-superstar-headline-bullets-list '("" "" "" "" "" "" "" "" "")
      org-superstar-leading-fallback ?\s;; Hide away leading stars on terminal.
      org-ellipsis " ▾"
      org-hide-emphasis-markers t
      org-habit-graph-column 40
      org-archive-location "Archive/%s_archive::"
      org-refile-targets
      '((org-agenda-files :maxlevel . 3)))

(use-package! org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("❗" "⬆" "⬇" )))

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
 ;; '(org-document-title ((t (:inherit outline-1 :height 1.25))))
 )

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-to-list 'org-modules 'org-habit)

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
;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)
;; }}}
;; ======================= Org Capture ============= {{{
(after! org (setq org-capture-templates
  `(("t" "Tasks / Projects")
    ("tt" "Task" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
         "** TODO %?  %U\n%i" :preappend t :empty-lines 1)
    ("ts" "Clocked Entry Subtask" entry (clock)
         "** TODO %?  %U\n  %a\n  %i" :preappend t :empty-lines 1)
    ("ti" "Interrupt" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "* %T %a :interrupt:\n\n%?\n\n"
          :clock-in :clock-resume :preappend t :empty-lines 1)

    ("n" "Note Entries")
    ("np" "Protocol" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "** NOTE %?[[%:link][%:description]] %U\n%i\n" :prepend t)
    ("nl" "Protocol Link" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "** NOTE %?[[%:link][%:description]] %U\n" :prepend t)
    ("nx" "Protocol Link from Clipboard" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "** NOTE %?%x %U\n" :prepend t)
    ("nb" "Protocol Link Blank" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "** NOTE %? %U\n" :prepend t)

    ("j" "Journal Entries")
    ("jd" "Dream" entry (file+olp+datetree "~/Org/Me/Journal.org")
          "* %T Dream :journal:\n\n%?\n\n"
          :clock-in :clock-resume :empty-lines 1)
    ("jj" "Journal note" entry (file+olp+datetree "~/Org/Me/Journal.org")
          "* %T Journal :journal:\n\n%?\n\n"
          :clock-in :clock-resume :empty-lines 1)
    ("jh" "Hangout" table-line (file+headline "~/Org/Me/Journal.org" "Hangouts")
          "| %^{Activity} | %^{Notes} | %^{With} | %^{Time-Stamp}U |"
          :empty-lines 1)

    ;; ("w" "Workflows")
    ;; ("we" "Checking Email" entry (file+olp+datetree ,(dw/get-todays-journal-file-name))
    ;;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
    ("w" "Workout" table-line (file+headline "~/Org/Me/Habits.org" "Workout")
          "| %^{Type of Workout|Calisthenics|Streatching|Yoga|Swimming} | %^{Exercises} | %^{Time-Stamps}T |"
          :kill-buffer t)

    ("s" "Scramble Capture")
    ("sd" "Dataset" table-line (file+headline "~/Org/Me/Scrambled.org" "Dataset")
          "| %^{Players|2|3|4|5} | %^{Color|B|G|P|W} | %^{Minutes} | %^{Seconds} | %^{Declared} | %^{Extracted} | %^{Time-Stamp}U | %^{Notes} |"
          :kill-budder t)
    ("si" "Scrambled Idea" table-line (file+headline "~/Org/Me/Scrambled.org" "Inbox")
          "* IDEA %?  %U\n" :empty-lines 1)
    ("st" "Scrambled Todo" table-line (file+headline "~/Org/Me/Scrambled.org" "Inbox")
          "* TODO %?  %U\n" :empty-lines 1)

    ("m" "Metrics Capture")
    ("md" "Drink Journal" table-line (file+headline "~/Org/Me/Metrics.org" "Hydro Journal")
          "| | %U | %^{Water|0|200} | %^{The|0|300} | %^{Coffee|0|1} | %^{Beer|0|330} | %^{Drinks|0|400} | %^{Sodas|0|150} | %^{Notes} |"
          :kill-buffer t :prepend t)
    ("mw" "Weight" table-line (file+headline "~/Org/Me/Metrics.org" "Weight")
          "| %U | %^{Weight} |" :preappend t :kill-buffer t))))
;; }}}
;; ======================= Spell Checking ============= {{{
(after! ispell
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_GB,en_US,it_IT,italiano,english"
        ispell-extra-args '("--sug-mode=ultra"))
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,en_US,it_IT,italiano,english")
  (setq ispell-personal-dictionary "~/.local/share/hunspell_personal")
  (remove-hook 'text-mode-hook #'flyspell-mode)
  (remove-hook 'org-mode-hook #'flyspell-mode))
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
