;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; ========= Testing ========= {{{
;; }}}
;; ========= Functions ========= {{{
(defun dr/eagenda()
  (org-agenda-list 21)
  (split-window-horizontally -85)
  (find-file "~/Org/Me/Habits.org")
  (split-window-vertically -15)
  (find-file "~/Org/Me/Tasks.org")
  (split-window-vertically -15)
  (find-file "~/Org/Me/Learn.org")
  (+workspace:rename "agenda")
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

(defun dr/high-resources()
  (setq display-line-numbers-type t)
  (dr/ispell-settings)
  (add-hook! 'org-mode-hook #'turn-on-org-cdlatex)
)

(defun run-with-python ()
  "Set the default comli-command to run the current file with python"
  (setq-local compile-command
              (concat "python "
                      (when buffer-file-name
                        (shell-quote-argument buffer-file-name)))))
(add-hook 'python-mode-hook 'run-with-python)
;; }}}
;; ========= Bootstraps ========= {{{
(setq dr/font-size 14
      display-line-numbers-type nil)
;; (dr/high-resources))
;; }}}
;; ========= Common ========= {{{
(setq user-full-name "Davide Ragaini"
      user-mail-address "ragainidavide@gmail.com"
      doom-font (font-spec :family "Fantasque Sans Propo" :size dr/font-size)
      doom-big-font (font-spec :family "Fantasque Sans Propo" :size (* dr/font-size 2))
      doom-serif-font (font-spec :family "Liberation Serif" :size (+ dr/font-size 2))
      doom-variable-pitch-font (font-spec :family "Liberation Sans" :size (+ dr/font-size 2))
      doom-symbol-font (font-spec :family "DejaVu Serif" :size dr/font-size)
      doom-theme 'modus-vivendi
      ;; whitespace-line-column 500
      compilation-scroll-output t
      whitespace-style '(face trailing newline missing-newline-at-eof empty big-indent space-mark tab-mark newline-mark )
      line-spacing 2
      scroll-margin 3
      delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/"
      auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist)
      confirm-kill-emacs nil)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(map! :leader :desc "Blinking cursor binding" :n "r" #'+nav-flash/blink-cursor)

(set-frame-parameter (selected-frame) 'alpha '(97 . 85))
(add-to-list 'default-frame-alist '(alpha . (97 . 85)))

(setq auth-sources '("~/.local/share/authinfo.gpg"))
;; }}}
;; ========= Global Modes ========= {{{
(blink-cursor-mode 1)
(global-auto-revert-mode 1)
(global-org-modern-mode 1)
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
    ("^magit-process:*"     :side right  :size 0.40 :quit t   :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Org Agenda*"      :side left   :size 0.40 :quit t   :slot  3 :vslot  3 :ttl 0 :select t)
    ("^\\*Python*"          :side left   :size 0.50 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Inferior Octave*" :side left   :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Warnings*"        :side bottom :size 0.30 :quit t   :slot -1 :vslot -2 :ttl 0 :select nil)
    ("^\\*compilation*"     :side right  :size 0.40 :quit t   :slot  1 :vslot  0 :ttl 5 :select t :modeline t)
    ("^\\*doom:*"           :side bottom :size 0.35 :quit t   :slot  1 :vslot  0 :ttl 5 :select t :modeline t)
    ("^\\*eshell*"          :side bottom :size 0.42 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*eww*"             :side left   :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*helpful*"         :side right  :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*magit-process*"   :side right  :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :side right :size 0.3 :ttl 5 :quit t)
    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)))
;; }}}
;; ========= Hooks ========= {{{
(add-hook! 'doom-load-theme-hook (set-face-foreground 'vertical-border "magenta"))
(add-hook! 'org-mode-hook (lambda () visual-line-mode 0))
(add-hook! 'mixed-pitch-mode-hook (whitespace-mode nil))
;; }}}
;; ========= Load Config Files ========= {{{
(load! "load/org.el")
;; (load! "load/calibredb.el")
;; (load! "load/matlab-setup.el")
;; }}}
;; ========= Hydra ========= {{{
(defhydra doom-window-resize-hydra (:hint nil)
  "vim motion evil-window-inc/dec-width/height"
  ("h" evil-window-decrease-width)
  ("j" evil-window-increase-height)
  ("k" evil-window-decrease-height)
  ("l" evil-window-increase-width)
  ("q" nil))
;; }}}
;; ========= Bindings ========= {{{
(map!
 (:prefix "SPC w" :desc "doom-window-resize-hydra/body" :n "SPC" #'doom-window-resize-hydra/body)
 (:prefix "SPC b" :desc "+format/region" :n "f" #'+format/region)
 (:prefix "SPC b" :desc "+format/buffer" :n "F" #'+format/buffer)
 (:prefix "SPC m" :desc "+org-todo-yesterday" :n "y" #'org-todo-yesterday)
)
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
