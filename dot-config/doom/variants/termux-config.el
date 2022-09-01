;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; ========= Testing ========= {{{
;; }}}
;; ========= Functions ========= {{{
;; }}}
;; ========= Popups Rules ========= {{{
(set-popup-rules!
  '(("^ \\*" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
    ("^\\*"  :slot 1 :vslot -1 :size #'+popup-shrink-to-fit :select t)
    ("^\\*Completions*"   :slot -1 :vslot -2 :ttl 0)
    ("^\\*Edit Formulas*" :side bottom :size 0.35 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Help*"          :side bottom :size 0.25 :quit t   :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Man*"           :side bottom :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Org Agenda*"    :side bottom :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Python*"        :side bottom :size 0.50 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Warnings*"      :side bottom :size 0.30 :quit t   :slot -1 :vslot -2 :ttl 0 :select nil)
    ("^\\*doom:*"         :side bottom :size 0.35 :quit t   :slot  1 :vslot  0 :ttl 5 :select t :modeline t)
    ("^\\*eshell*"        :side bottom :size 0.42 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*eww*"           :side bottom :size 0.50 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*helpful*"       :side bottom :size 0.33 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :side right :size 0.3 :ttl 5 :quit t)
    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)))
;; }}}
;; ========= Common ========= {{{
(setq user-full-name "Davide Ragaini"
      user-mail-address "ragainidavide@gmail.com"
      ;; doom-font (font-spec :family "Iosevka" :size dr/font-size)
      ;; doom-big-font (font-spec :family "Iosevka" :size (+ dr/font-size 12))
      ;; doom-serif-font (font-spec :family "Liberation Sans" :size (+ dr/font-size 2))
      ;; doom-variable-pitch-font (font-spec :family "Liberation Sans" :size (+ dr/font-size 2))
      ;; doom-unicode-font (font-spec :family "Linux Libertine O" :size dr/font-size)
      display-line-numbers-type nil
      no-native-compile t
      scroll-margin 2
      whitespace-line-column 500
      confirm-kill-emacs nil)

;; (load-theme 'modus-vivendi)
(blink-cursor-mode 1)
(global-auto-revert-mode 1)
(global-evil-vimish-fold-mode 1)
(global-visual-line-mode nil)
(global-whitespace-mode 1)
;; }}}
;; ========= Load Configs ========= {{{
(load! "org.el")
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
