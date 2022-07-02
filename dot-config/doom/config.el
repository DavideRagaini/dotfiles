;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Davide Ragaini"
      user-mail-address "ragainidavide@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;; (setq doom-font (font-spec :family "mononoki Nerd Font Mono" :size 18)
;;       doom-variable-pitch-font (font-spec :family "Luxi Sans" :size 20)
;; doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
;; doom-big-font (font-spec :family "FiraMono Nerd Font" :size 30)
;; )

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
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

(let ((x (system-name)))
  (cond
   ((string-equal x "VoiD")
    (setq custom-font-size 30))
   ((string-equal x "void")
    (setq custom-font-size 12))
   ((string-equal x "vDR")
    (setq custom-font-size 16)))
   (setq doom-font (font-spec :family "Iosevka" :size custom-font-size)
         doom-variable-pitch-font (font-spec :family "Luxi Sans" :size (+ custom-font-size 2))
         doom-serif-font (font-spec :family "Luxi Sans" :size (+ custom-font-size 2))
         doom-unicode-font (font-spec :family "Dejavu Sans" :size custom-font-size)
         doom-big-font (font-spec :family "SpaceMono Nerd Font" :size custom-font-size)
         ))

(defun toggle-theme ()
  "Light theme toggles"
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-dracula)
      (load-theme 'doom-nord-light)
    (load-theme 'doom-dracula)))
(global-set-key [f5] 'toggle-theme)

(defun agenda-layout ()
  "Eagenda layout"
  (progn
    ;; (+workspace/new-named "agenda")
    (find-file (expand-file-name "~/Org/Tasks.org"))
    (split-window-right 65)
    (org-agenda-list 15))
  )

;; Global
(setq scroll-margin 2)
(global-visual-line-mode t)
(blink-cursor-mode 1)
(setq confirm-kill-emacs nil)
(global-auto-revert-mode 1)
(global-activity-watch-mode t)

(set-frame-parameter (selected-frame) 'alpha '(92))
(add-to-list 'default-frame-alist '(alpha . (92)))

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


;; Dired
(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;; :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
(use-package dired-single)


;; ORG ENHANEMENT
;; Automatically change bullet type when indenting
;; Ex: indenting a + makes the bullet a *.
(setq org-list-demote-modify-bullet
      '(("+" . "*") ("*" . "-") ("-" . "+")))
(setq org-superstar-headline-bullets-list
      '("" "" "" "" "" "" "" "" ""))
;; Hide away leading stars on terminal.
(setq org-superstar-leading-fallback ?\s)
(use-package org-fancy-priorities
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

(setq org-agenda-files
      '("~/Org/Tasks.org"
        "~/Org/Habits.org"
        "~/Org/Learn.org"
        "~/Org/Birthdays.org"))

(use-package org
  :commands (org-capture org-agenda)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))
;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; (require 'org-habit)
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

(add-hook 'org-mode-hook 'org-appear-mode)


;; Spell checking
(with-eval-after-load "ispell"
        (setq ispell-program-name "/usr/bin/hunspell")
        (setq ispell-extra-args  '("--sug-mode=ultra"))
        (setq ispell-dictionary "en_GB,en_US,it_IT")
        (ispell-set-spellchecker-params)
        (ispell-hunspell-add-multi-dic "en_GB,en_US,it_IT")
        (setq ispell-personal-dictionary "~/.local/share/hunspell_personal"))
