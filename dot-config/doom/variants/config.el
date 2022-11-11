;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; ========= Testing ========= {{{
;; }}}
;; ========= Functions ========= {{{
(defun dr/eagenda()
  (find-file "~/Org/Me/Tasks.org")
  (split-window-horizontally 80)
  (org-agenda-list 21)
  (+workspace:rename "agenda")
)

(defun dr/low-resources()
  (setq display-line-numbers-type nil
        company-idle-delay 0.5
        no-native-compile t)
  (after! org
    (setq org-fontify-quote-and-verse-blocks nil
          org-fontify-whole-heading-line nil
          ;; org-hide-leading-stars nil
          ;; org-startup-indented nil
          )))

(defun dr/high-resources()
  (setq display-line-numbers-type t)
  (dr/ispell-settings)
  (global-activity-watch-mode t)
  )

(defun dr/ispell-settings()
  (after! ispell
    (setq ispell-program-name "hunspell"
          ispell-dictionary "en_GB,en_US,it_IT,italiano,english"
          ispell-extra-args '("--sug-mode=ultra"))
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "en_GB,en_US,it_IT,italiano,english")
    (setq ispell-personal-dictionary "~/.local/share/hunspell_personal")
    ;; (remove-hook 'text-mode-hook #'flyspell-mode)
    ;; (remove-hook 'org-mode-hook #'flyspell-mode)
    ))
;; }}}
;; ========= Bootstraps ========= {{{
(cond
 ((string-equal (system-name) "VoiD")
        (setq dr/font-size 30)
        (dr/high-resources))
 ((string-equal (system-name) "vDR")
        (setq dr/font-size 18)
        (dr/high-resources))
 ((or (string-equal (system-name) "void") (string-equal (system-name) "NT"))
        (dr/low-resources)
        (setq dr/font-size 12))
 ((string-equal (system-name) "tinkerboard")
        (dr/low-resources)
        (setq dr/font-size 17))
 ((string-equal (system-name) "bagaro")
        (dr/high-resources)
        (setq dr/font-size 14)))
;; }}}
;; ========= Common ========= {{{
(setq user-full-name "Davide Ragaini"
      user-mail-address "ragainidavide@gmail.com"
      doom-font (font-spec :family "Fantasque Sans Mono" :size dr/font-size)
      doom-big-font (font-spec :family "Fantasque Sans Mono" :size (* dr/font-size 2))
      doom-serif-font (font-spec :family "Nimbus Mono PS" :size (+ dr/font-size 2))
      doom-variable-pitch-font (font-spec :family "Linux Biolinum O" :size (+ dr/font-size 2))
      doom-unicode-font (font-spec :family "DejaVu Serif" :size dr/font-size)
      doom-theme 'doom-dracula
      ;; whitespace-line-column 500
      whitespace-style '(face trailing newline missing-newline-at-eof empty big-indent space-mark tab-mark newline-mark )
      line-spacing 2
      scroll-margin 3
      delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/"
      auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist)
      confirm-kill-emacs nil)
;; }}}
;; ========= Global Modes ========= {{{
(blink-cursor-mode 1)
(global-auto-revert-mode 1)
(global-evil-vimish-fold-mode 1)
(global-visual-line-mode t)
(global-whitespace-mode 1)
;; }}}
;; ========= Popups Rules ========= {{{
(set-popup-rules!
  '(("^ \\*" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
    ("^\\*"  :slot 1 :vslot -1 :size 0.3 :select t)
    ("^\\*Completions*"     :slot -1 :vslot -2 :ttl 0)
    ("^\\*Edit Formulas*"   :side left   :size 0.35 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Help*"            :side bottom :size 0.25 :quit t   :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Man*"             :side right  :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Org Agenda*"      :side left   :size 0.40 :quit t   :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Python*"          :side left   :size 0.50 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Inferior Octave*" :side left   :size 0.50 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Warnings*"        :side bottom :size 0.30 :quit t   :slot -1 :vslot -2 :ttl 0 :select nil)
    ("^\\*doom:*"           :side bottom :size 0.35 :quit t   :slot  1 :vslot  0 :ttl 5 :select t :modeline t)
    ("^\\*eshell*"          :side bottom :size 0.42 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*eww*"             :side left   :size 0.50 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*helpful*"         :side right  :size 0.33 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*magit-process*"   :side right  :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :side right :size 0.3 :ttl 5 :quit t)
    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)))
;; }}}
;; ========= Hooks ========= {{{
(add-hook! 'doom-load-theme-hook (set-face-foreground 'vertical-border "magenta"))
(add-hook 'org-mode-hook (lambda () visual-line-mode 0))
(add-hook 'mixed-pitch-mode-hook (whitespace-mode nil))
;; }}}
;; ========= Load Config Files ========= {{{
(load! "org.el")
;; }}}
;; ========= Manual ========= {{{
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
;; }}}
