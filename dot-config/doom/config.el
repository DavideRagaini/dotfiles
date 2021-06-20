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

(setq scroll-margin 3)
(global-visual-line-mode t)
(blink-cursor-mode 1)
(global-whitespace-mode 1)

 (set-frame-parameter (selected-frame) 'alpha '(90 . 80))
 (add-to-list 'default-frame-alist '(alpha . (90 . 80)))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)))
         ;; :map minibuffer-local-map
         ;; ("C-M-r" . 'counsel-minibuffer-history)))

;; ORG ENHANEMENT
;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq org-hide-emphasis-markers t)
(custom-set-faces
    '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
    '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
    '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
    '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
    '(org-level-5 ((t (:inherit outline-5 :height 1.1))))
    '(org-level-6 ((t (:inherit outline-6 :height 1.1))))
    '(org-level-7 ((t (:inherit outline-7 :height 1.1))))
    '(org-level-8 ((t (:inherit outline-8 :height 1.1))))
)

(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(setq org-agenda-files
      '("~/Org/Tasks.org"
        "~/Org/Habits.org"
        "~/Org/Birthdays.org"))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

(use-package org
  ;; :pin org
  :commands (org-capture org-agenda)
  ;; :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)"
                "NEXT(n)"
                "|"
                "DONE(d!)")

      (sequence "BACKLOG(b)"
                "PLAN(p)"
                "READY(r)"
                "ACTIVE(a)"
                "REVIEW(v)"
                "WAIT(w@/!)"
                "HOLD(h)"
                "|"
                "COMPLETED(c)"
                "CANC(k@)")))

(setq org-tag-alist
  '((:startgroup)
     ; Put mutually exclusive tags here
     (:endgroup)
     ("@errand" . ?E)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("planning" . ?p)
     ("publish" . ?P)
     ("batch" . ?b)
     ("note" . ?n)
     ("idea" . ?i)))

;; Configure custom agenda views
(setq org-agenda-custom-commands
 '(("d" "Dashboard"
   ((agenda "" ((org-deadline-warning-days 7)))
  (todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))
    (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

  ("n" "Next Tasks"
   ((todo "NEXT"
      ((org-agenda-overriding-header "Next Tasks")))))

  ("W" "Work Tasks" tags-todo "+work-email")

  ;; Low-effort next actions
  ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
   ((org-agenda-overriding-header "Low Effort Tasks")
    (org-agenda-max-todos 20)
    (org-agenda-files org-agenda-files)))

  ("w" "Workflow Status"
   ((todo "WAIT"
          ((org-agenda-overriding-header "Waiting on External")
           (org-agenda-files org-agenda-files)))
    (todo "REVIEW"
          ((org-agenda-overriding-header "In Review")
           (org-agenda-files org-agenda-files)))
    (todo "PLAN"
          ((org-agenda-overriding-header "In Planning")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "BACKLOG"
          ((org-agenda-overriding-header "Project Backlog")
           (org-agenda-todo-list-sublevels nil)
           (org-agenda-files org-agenda-files)))
    (todo "READY"
          ((org-agenda-overriding-header "Ready for Work")
           (org-agenda-files org-agenda-files)))
    (todo "ACTIVE"
          ((org-agenda-overriding-header "Active Projects")
           (org-agenda-files org-agenda-files)))
    (todo "COMPLETED"
          ((org-agenda-overriding-header "Completed Projects")
           (org-agenda-files org-agenda-files)))
    (todo "CANC"
          ((org-agenda-overriding-header "Cancelled Projects")
           (org-agenda-files org-agenda-files))))))))

(setq org-capture-templates
  `(("t" "Tasks / Projects")
    ("tt" "Task" entry (file+olp "~/Org/Tasks.org" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
    ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

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

    ("m" "Metrics Capture")
    ("mw" "Weight" table-line (file+headline "~/Org/Metrics.org" "Weight")
     "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

(setq org-refile-targets
  '(("Archive.org" :maxlevel . 1)
    ("Tasks.org" :maxlevel . 1)))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)
