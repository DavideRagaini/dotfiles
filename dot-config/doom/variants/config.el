;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; ========= Functions ========= {{{
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
  (setq display-line-numbers-type 'relative)
  (dr/ispell-settings)
  (add-hook! 'org-mode-hook #'turn-on-org-cdlatex)
  )

(defun bram85-magit-find-file-as-of (datetime)
  (interactive (list (org-read-date)))
  (let ((rev (format "HEAD@{%s}" datetime)))
    (magit-find-file rev (magit-read-file-from-rev rev "File: "))))

(defun dr/run-with-python ()
  "Set the default comli-command to run the current file with python"
  (setq-local compile-command
              (concat "python "
                      (when buffer-file-name
                        (shell-quote-argument buffer-file-name)))))
(add-hook 'python-mode-hook 'dr/run-with-python)

(defun dr/toggle-theme ()
  "Light/dark theme toggle"
  (interactive)
  (if (eq (car custom-enabled-themes) dr/dark-theme)
      (load-theme dr/light-theme)
    (load-theme dr/dark-theme)))
;; }}}
;; ========= Bootstraps ========= {{{
(setq dr/font-size 13
      dr/main-font-family "IosevkaTerm Nerd Font Mono"

dr/big-font-family "IosevkaTermSlab Nerd Font Mono"
      dr/serif-font-family "Garamond Libre"
      dr/variable-pitch-font-family "Overpass Nerd Font"
      dr/symbol-font-family "JuliaMono"
      dr/light-theme 'leuven
      dr/dark-theme 'modus-vivendi
      )

(cond
 ((string-equal (system-name) "Apollo")
  (setq dr/dark-theme 'modus-vivendi
        dr/font-size 28)
  ;; (global-activity-watch-mode 1)
  (dr/high-resources)
  )
 ((eq system-type 'windows-nt)
  (setq dr/main-font-family "cascadia code"
        dr/big-font-family "cascadia code"
        dr/variable-pitch-font-family "microsoft sans serif"
        dr/symbol-font-family "cascadia code"
        dr/serif-font-family "times new roman"
        dr/variable-pitch-font-family "sagoe print"
        dr/light-theme 'leuven
        dr/dark-theme 'catppuccin
        )
  )
 )
;; }}}
;; ========= Common ========= {{{
(setq user-full-name "Davide Ragaini"
      user-mail-address "ragainidavide@gmail.com"
      ;; whitespace-line-column 500
      doom-font (font-spec :family dr/main-font-family :size dr/font-size)
      doom-big-font (font-spec :family dr/big-font-family :size (* dr/font-size 2))
      doom-serif-font (font-spec :family dr/serif-font-family :size dr/font-size)
      ;; doom-serif-font (font-spec :family "DejaVu Serif" :size dr/font-size)
      doom-variable-pitch-font (font-spec :family dr/variable-pitch-font-family :size dr/font-size)
      doom-symbol-font (font-spec :family dr/symbol-font-family :size dr/font-size)
      doom-theme dr/dark-theme
      compilation-scroll-output t
      whitespace-style '(face trailing newline missing-newline-at-eof empty big-indent space-mark tab-mark)
      line-spacing 2
      scroll-margin 10
      delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/"
      +lookup-open-url-fn #'eww
      which-key-idle-delay 0.3
      confirm-kill-emacs nil)

(add-to-list 'auto-mode-alist
             '("\\.epub\\'" . nov-mode)
             '("\\.m$" . octave-mode))

;; (set-frame-parameter (selected-frame) 'alpha '(98 . 95))
;; (add-to-list 'default-frame-alist '(alpha . (98 . 95)))

(setq auth-sources '("~/.local/share/authinfo.gpg"))
;; }}}
;; ========= Load Config Files ========= {{{
(load! "load/org.el")
;; (load! "load/erc.el")
;; (load! "load/nov.el")
(load! "load/elfeed.el")
;; (load! "load/roam.el")

;; (load! "load/citations.el")
;; (load! "load/emms.el")
;; (load! "load/calibredb.el")
;; (load! "load/matlab-setup.el")
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
  '(("^ \\*"                :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
    ("^\\*"                 :slot 1 :vslot -1 :size 0.3 :select t)
    ("^\\*Completions*"     :slot -1 :vslot -2 :ttl 0)
    ("^\\*Edit Formulas*"   :side left   :size 0.35 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Help*"            :side bottom :size 0.25 :quit t   :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Inferior Octave*" :side left   :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t :modeline t)
    ("^\\*Man*"             :side right  :size 0.30 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Org Agenda*"      :side left   :size 0.40 :quit t   :slot  3 :vslot  3 :ttl 0 :select t :modeline t)
    ("^\\*Python*"          :side left   :size 0.35 :quit nil :slot -1 :vslot  0 :ttl 0 :select t :modeline t)
    ("^\\*Warnings*"        :side bottom :size 0.30 :quit t   :slot -1 :vslot -2 :ttl 0 :select nil)
    ("^\\*WoMan*"           :side right  :size 0.30 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*compilation*"     :side right  :size 0.40 :quit t   :slot  1 :vslot  0 :ttl 5 :select nil)
    ("^\\*doom:*"           :side bottom :size 0.35 :quit t   :slot  1 :vslot  0 :ttl 5 :select t :modeline t)
    ("^\\*elfeed-entry-*"   :side bottom :size 0.42 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*eshell*"          :side bottom :size 0.42 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*eww*"             :side left   :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t :modeline t)
    ("^\\*helpful*"         :side right  :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*image-dired*"     :side bottom :size 0.40 :quit t   :slot  1 :vslot  1 :ttl 5 :select nil)
    ("^*magit-process*"     :side right  :size 0.40 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^etc/NEWS*"           :side right  :size 0.35 :quit nil :slot -1 :vslot  0 :ttl 0 :select t)
    ("^\\*Compil\\(?:ation\\|e-Log\\)" :side right :size 0.3 :quit nil :ttl 5 :quit t)
    ("^\\*\\(?:scratch\\|Messages\\)" :ttl t)))
;; }}}
;; ========= Hooks ========= {{{
(add-hook! 'doom-load-theme-hook (set-face-foreground 'vertical-border "magenta"))
(add-hook! 'org-mode-hook (lambda () visual-line-mode 0))
;; (add-hook! 'mixed-pitch-mode-hook (whitespace-mode nil))
;; }}}
;; ========= use-package! ========= {{{
;; (use-package! magit-todos
;;   :after magit
;;   :config (magit-todos-mode 1))

;; (use-package! pdf-tools
;;   :config
;;   ;; (pdf-tools-install)
;;   (setq-default pdf-view-display-size 'fit-width)
;;   :custom
;;   (pdf-annot-activate-created-annotations t "automatically annotate highlights")
;;   )

;; (after! org-noter
;;   (setq
;;    org-noter-notes-search-path '("~/org/notes")
;;    org-noter-hide-other nil
;;    org-noter-separate-notes-from-heading t
;;    org-noter-always-create-frame nil)

;;   (map!
;;    :map org-noter-doc-mode-map
;;    :leader
;;    :desc "Insert note"         "m i" #'org-noter-insert-note
;;    :desc "Insert precise note" "m p" #'org-noter-insert-precise-note
;;    :desc "Go to previous note" "m k" #'org-noter-sync-prev-note
;;    :desc "Go to next note"     "m j" #'org-noter-sync-next-note
;;    :desc "Create skeleton"     "m s" #'org-noter-create-skeleton
;;    :desc "Kill session"        "m q" #'org-noter-kill-session
;;    )
;;   )

;; }}}
;; ========= Clipboard Wayland ========= {{{
(setq wl-copy-process nil)

(defun wl-copy (text)
  (setq wl-copy-process
        (make-process
         :name "wl-copy"
         :buffer nil
         :command '("wl-copy" "-f" "-n")
         :connection-type 'pipe
         :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ; should return nil if we're the current paste owner
    (shell-command-to-string "wl-paste -n | tr -d \r")))

(setq interprogram-cut-function 'wl-copy)
(setq interprogram-paste-function 'wl-paste)
;; }}}
;; ========= Hydra ========= {{{
(defhydra doom-window-resize-hydra (:hint nil)
  "vim motion evil-window-inc/dec-width/height"
  ("h" evil-window-decrease-width)
  ("j" evil-window-increase-height)
  ("k" evil-window-decrease-height)
  ("l" evil-window-increase-width)
  ("q" nil))

(defhydra workspace-move (:hint nil)
  "vim motion workspace order"
  ("h" +workspace/swap-left)
  ("j" +workspace/swap-left)
  ("k" +workspace/swap-right)
  ("l" +workspace/swap-right)
  ("q" nil))
;; }}}
;; ========= Bindings ========= {{{
(map!
 (:prefix "SPC a"   :desc "blink cursor"                  :n "b"   #'+nav-flash/blink-cursor)
 ;;
 (:prefix "SPC b"   :desc "+format/region"                :n "f"   #'+format/region)
 (:prefix "SPC b"   :desc "+format/buffer"                :n "F"   #'+format/buffer)
 ;;
 (:prefix "SPC d"   :desc "dired"                         :n "d"   #'dired)
 (:prefix "SPC d"   :desc "dired"                         :n "i"   #'image-dired)
 ;;
 (:prefix "SPC h"   :desc "woman"                         :n "z"   #'woman)
 (:prefix "C-h"     :desc "woman"                         :n "z"   #'woman)
 (:prefix "SPC h"   :desc "man"                           :n "  h"   #'man)
 (:prefix "C-h"     :desc "man"                           :n "h"   #'man)
 ;;
 (:prefix "SPC m"   :desc "+org-todo-yesterday"           :n "y"   #'org-todo-yesterday)
 ;;
 (:prefix "SPC o"   :desc "open elfeed"                   :n "n"   #'dr/start-elfeed)
 (:prefix "SPC o"   :desc "elfeed"                        :n "N"   #'elfeed)
 ;;
 (:prefix "SPC w"   :desc "doom-window-resize-hydra/body" :n "SPC" #'doom-window-resize-hydra/body)
 (:prefix "C-w"     :desc "doom-window-resize-hydra/body" :n "SPC" #'doom-window-resize-hydra/body)
 ;;
 (:prefix "SPC t"   :desc "toggle light/dark theme"       :n "t"   #'dr/toggle-theme)
 ;;
 (:prefix "SPC y"   :desc "hint copy link"                :n "y"   #'link-hint-copy-link)
 (:prefix "SPC y"   :desc "hint copy all link"            :n "a"   #'link-hint-copy-all-links)
 (:prefix "SPC y"   :desc "hint copy link at point"       :n "p"   #'link-hint-copy-link-at-point)
 (:prefix "SPC y"   :desc "hint copy multiple link"       :n "m"   #'link-hint-copy-multiple-links)
 ;;
 (:prefix "SPC TAB" :desc "vim motion workspace order"    :n "m"   #'workspace-move/body)
 (:prefix "SPC TAB" :desc "vim motion swap left"          :n "{"   #'+workspace/swap-left)
 (:prefix "SPC TAB" :desc "vim motion swap right"         :n "}"   #'+workspace/swap-right)
 (:prefix "SPC TAB" :desc "last workspace"                :n ";"   #'+workspace/other)
 )

(evil-define-key 'normal dired-mode-map
  (kbd "TAB") 'dired-subtree-toggle
  (kbd "C-TAB") 'dired-subtree-cycle
  (kbd "S-TAB") 'dired-subtree-remove
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
