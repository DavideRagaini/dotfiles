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
(setq doom-font (font-spec :family "mononoki Nerd Font Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Luxi Sans" :size 20)
      ;; doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
      ;; doom-big-font (font-spec :family "FiraMono Nerd Font" :size 30)
)

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


                ;; Global
(setq scroll-margin 2)
(global-visual-line-mode t)
(blink-cursor-mode 1)
(setq confirm-kill-emacs nil)

 ;; (set-frame-parameter (selected-frame) 'alpha '(90))
 ;; (add-to-list 'default-frame-alist '(alpha . (90)))

;; (use-package counsel
;;   :bind (("C-M-j" . 'counsel-switch-buffer)))
         ;; :map minibuffer-local-map
         ;; ("C-M-r" . 'counsel-minibuffer-history)))

(set-popup-rules!
 '(("^ \\*" :slot -1) ; fallback rule for special buffers
   ("^\\*" :select t)
   ("^\\*Completions" :slot -1 :ttl 0)
   ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)
   ("^\\*Help" :slot -1 :size 0.2 :select t)
   ("^\\*doom:"AA :size 0.35 :select t :modeline t :quit t :ttl t)))


                ;; Circadian
(use-package circadian
  :config
  (setq calendar-latitude 43.50)
  (setq calendar-longitude 13.01)
  (setq circadian-themes '((:sunrise . doom-nord-light)
                           (:sunset  . doom-dracula)))
  (circadian-setup))


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

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "sxiv")
                                ("mkv" . "mpv"))))


                ;; ORG ENHANEMENT
;; Automatically change bullet type when indenting
;; Ex: indenting a + makes the bullet a *.
(setq org-list-demote-modify-bullet
      '(("+" . "*") ("*" . "-") ("-" . "+")))

(custom-set-faces
    '(org-level-1 ((t (:inherit outline-1 :height 1.25))))
    ;; '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
    ;; '(org-level-3 ((t (:inherit outline-3 :height 1.15))))
    ;; '(org-level-4 ((t (:inherit outline-4 :height 1.10))))
    ;; '(org-level-5 ((t (:inherit outline-5 :height 1.05))))
    ;; '(org-level-6 ((t (:inherit outline-6 :height 1.05))))
    ;; '(org-document-title ((t (:inherit outline-1 :height 1.25))))
)
;; Stop cycling bullets to emphasize hierarchy of headlines.
(setq org-superstar-cycle-headline-bullets nil)
;; Hide away leading stars on terminal.
(setq org-superstar-leading-fallback ?\s)


                ;; Org Agenda
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("❗" "⬆" "⬇" )))

(setq org-agenda-files
      '("~/Org/Tasks.org"
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
     ("batch" . ?b)
     ("fix" . ?f)
     ("improve" . ?m)
     ("note" . ?n)))

(setq org-refile-targets
  '(("Archive.org" :maxlevel . 1)
    ("Tasks.org" :maxlevel . 1)))
                ;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

(add-hook 'org-mode-hook 'org-appear-mode)

                ;; Spell checking
(setq ispell-personal-dictionary "~/.local/share/hunspell_personal")
