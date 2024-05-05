;;; ../../.local/src/dotfiles/dot-config/doom/org.el -*- lexical-binding: t; -*-
(setq org-auto-align-tags nil
      org-startup-folded t
      org-checkbox-hierarchical-statistics nil
      org-image-actual-width 1200
      org-agenda-compact-blocks t
      org-agenda-restore-windows-after-quit t
      org-agenda-default-appointment-duration 30
      org-agenda-span 14
      org-deadline-warning-days 7
      org-agenda-window-setup 'reorganize-frame
      org-agenda-inhibit-startup t
      )

(add-hook 'org-agenda-mode-hook
          (lambda () (local-set-key [tab] 'org-agenda-tree-to-indirect-buffer)))

(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  (setq org-agenda-tags-column (- 4 (window-width)))
  (org-agenda-align-tags))

;; ========= Org Misc ========= {{{
;; Resume clocking task when emacs is restarted
(setq org-directory "~/org"
      ;; org-agenda-start-day "-3d"
      org-agenda-files
      '("~/org/Me/Tasks.org"
        "~/org/Me/Habits.org"
        "~/org/Me/Learn.org"
        "~/org/Others/Birthdays.org")
      org-agenda-log-mode-items '(state closed clock)
      org-agenda-start-with-log-mode t
      org-archive-location "~/org/Archive/%s_archive::"
      org-columns-default-format "%4TODO(ToDo) %40ITEM(Task) %2PRIORITY %6CLOCKSUM(Clock) %8Effort(Estimated Effort){:} %TAGS(Tags)"
      org-agenda-include-diary t
      ;; org-agenda-include-inactive-timestamps t
      ;; org-agenda-show-log 'only
      org-clock-persist t
      org-clock-persist t
      org-clock-in-resume t
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-in-switch-to-state "STRT"
      org-clock-out-when-done t
      org-clock-auto-clock-resolution (quote when-no-clock-is-running)
      org-clock-report-include-clocking-task t
      org-ellipsis " ⤵" ; ▼
      org-export-in-background t
      org-src-fontify-natively t
      org-global-properties
      '(("Effort_ALL" .
         ;;   1    2    3    4    5    6    7    8    9    0
         "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))
      org-hide-emphasis-markers nil
      org-highlight-latex-and-related '(native script entities)
      org-image-actual-width 600
      ;; org-habit-graph-column 40
      org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+"))
      org-log-done 'time
      ;; org-log-into-drawer t
      org-noter-notes-search-path '("~/org/Notes/")
      ;; Use pretty things for the clocktable
      org-pretty-entities nil
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      ;; org-agenda-include-deadlines
      ;; org-agenda-skip-deadline-prewarning-if-scheduled
      ;; org-agenda-todo-ignore-deadlines
      ;; org-agenda-skip-scheduled-if-deadline-is-shown
      ;; org-agenda-skip-deadline-if-done
      ;; org-agenda-skip-scheduled-if-done
      org-tags-column -1)

;; (setq org-todo-keywords
;;   '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "DONE(d)")))

(setq org-agenda-format-date
      (lambda (date) (concat "\n" (make-string (- (window-width) 1) 9472) "\n"
                             (org-agenda-format-date-aligned date))))

(org-clock-persistence-insinuate)

;; (custom-set-faces
;;  '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
;;  '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
;;  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
;;  '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
;;  '(org-level-5 ((t (:inherit outline-5 :height 1.2))))
;;  '(org-level-6 ((t (:inherit outline-6 :height 1.2))))
;;  ;; '(org-document-title ((t (:inherit outline-1 :height 1.25))))
;; )

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
        ("ME" . ?m)
        ("OTHERS" . ?o)
        (:endgroup)
        ("ASK" . ?b)
        ("BATCH" . ?b)
        ("DIY" . ?d)
        ("FIX" . ?f)
        ("SETUP" . ?u)
        ("IMPROVE" . ?i)
        ("READ" . ?r)
        ("REVIEW" . ?R)
        ("STUDY" . ?s)
        ("TEST" . ?t)
        ("THINK" . ?T)
        ("WATCH" . ?w)))
;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)
;; }}}
;; ========= Org Capture ========= {{{
(setq org-capture-templates
      `(("a" "Accounting Capture")
        ("ad" "Debts" table-line (file+olp+datetree "~/org/Me/Accounting.org" "Debts")
         "| %^{Quantity} | %^{Reason} | %^{TimeStamp}U |"
         :tree-type month :kill-buffer t :prepend t)
        ("ag" "Give" table-line (file+olp+datetree "~/org/Me/Accounting.org" "Give")
         "| %^{Quantity} | %^{Reason} | %^{TimeStamp}U |"
         :tree-type month :kill-buffer t :prepend t)
        ("ah" "Have" table-line (file+olp+datetree "~/org/Me/Accounting.org" "Have")
         "| %^{Quantity} | %^{Reason} | %^{TimeStamp}U |"
         :tree-type month :kill-buffer t :prepend t)
        ("as" "Spent" table-line (file+olp+datetree "~/org/Me/Accounting.org" "Spent")
         "| %^{Quantity} | %^{Reason} | %^{TimeStamp}U |"
         :tree-type month :kill-buffer t :prepend t)

        ("b" "Personal Capture")
        ("ba" "Add Anniversary" entry (file+headline "~/org/Others/Recurring.org" "Anniversaries")
         "%%(org-anniversary %^{Year} %^{Month} %^{Day}) %^{Name} %^{Surname} (%d)"
         :kill-buffer t :prepend t)
        ("bb" "Add Birtday" entry (file+headline "~/org/Others/Recurring.org" "Birthdays")
         "%%(org-anniversary %^{Year} %^{Month} %^{Day}) %^{Name} (%d)"
         :kill-buffer t :prepend t)
        ("bm" "Add to Music List" entry (file+headline "~/org/Me/Music.org" "Inbox")
         "* [ ] %^{Title} - %^{Artist}\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %^{Source}\n:LINK: %^{Link}\n:END:"
         :kill-buffer t :prepend t)
        ("br" "Add to Read List" entry (file+headline "~/org/Me/Read.org" "Inbox")
         "* [ ] %^{Title} - %^{Author}\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %^{Source}\n:LINK: %^{Link}\n:END:"
         :kill-buffer t :prepend t)
        ("bt" "Add Bookmark" entry (file+headline "~/org/Me/Bookmarks.org")
         "* %?"
         :kill-buffer t :prepend t)
        ("bw" "Add to Watch List" entry (file+headline "~/org/Me/Watch.org" "Inbox")
         "* [ ] %^{Title} - %^{Author/Director}\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %^{Source}\n:LINK: %^{Link}\n:END:"
         :kill-buffer t :prepend t)

        ("g" "Goals Capture")
        ("gp" "Primary Mission" entry (file+olp "~/org/Me/Goals.org" "Ideals" "Inbox")
         "* %?" :kill-buffer t :prepend t)
        ("gs" "Secondary Mission" entry (file+olp "~/org/Me/Goals.org" "Secondary" "Inbox")
         "* %?" :kill-buffer t :prepend t)

        ("m" "Metrics Capture")
        ("md" "Drink Journal" table-line (file+headline "~/org/Me/Metrics.org" "Hydro Journal")
         "| | %U | %^{Water|0|200} | %^{The|0|300} | %^{Coffee|0|1} | %^{Beer|0|330} | %^{Drinks|0|400} | %^{Sodas|0|150} | %^{Notes} |"
         :kill-buffer t :prepend t)
        ("ms" "Sleep Journal" table-line (file+headline "~/org/Me/Metrics.org" "Sleep Journal")
         "| %^{Sleep TimeStamp}U | %^{Wake TimeStamp}U | | |"
         :kill-buffer t)
        ("mw" "Weight" table-line (file+headline "~/org/Me/Metrics.org" "Weight")
         "| %U | %^{Weight} |"
         :kill-buffer t)

        ("n" "Note Entries")
        ("nb" "Protocol Link Blank" entry (file+headline "~/org/Me/Tasks.org" "Inbox")
         "* NOTE %? %U"
         :prepend t)
        ("nl" "Protocol Link" entry (file+headline "~/org/Me/Tasks.org" "Inbox")
         "* NOTE %?[[%:link][%:description]] %U"
         :prepend t)
        ("np" "Protocol" entry (file+headline "~/org/Me/Tasks.org" "Inbox")
         "* NOTE %?[[%:link][%:description]] %U\n%i"
         :prepend t)
        ("ns" "Shopping Note" entry (file+headline "~/org/Me/Shopping_List.org" "Inbox")
         "* NOTE %? %U"
         :prepend t)
        ("nx" "Protocol Link from Clipboard" entry (file+headline "~/org/Me/Tasks.org" "Inbox")
         "* NOTE %?%x %U"
         :prepend t)

        ("j" "Journal Entries")
        ("jd" "Dream" entry (file+olp+datetree "~/org/Me/Journal.org")
         "* %<%R> Dream :journal:\n%?"
         :clock-in :clock-resume :empty-lines 1 :empty-lines-after 1)
        ("jj" "Journal note" entry (file+olp+datetree "~/org/Me/Journal.org")
         "* %<%R> Journal :journal:\n%?"
         :clock-in :clock-resume :empty-lines 1 :empty-lines-after 1)
        ("jh" "Hangout" table-line (file+headline "~/org/Me/Journal.org" "Hangouts")
         "| %^{Activity} | %^{Notes} | %^{With} | %^{Time-Stamp}U |"
         :prepend t :empty-lines 1 :empty-lines-after 1)
        ("jp" "Daily Planning" entry (file+olp+datetree "~/org/Me/Journal.org")
         "* [ ] %<%R> %?"
         :prepend t :empty-lines 1 :empty-lines-after 1)
        ("jt" "Thoughts" entry (file+olp+datetree "~/org/Me/Thoughts.org")
         "* %?"
         :empty-lines 1 :empty-lines-after 1)

        ("p" "Add new paper" entry (file+headline "~/Uni/Tirocinio/Readings/Readings.org" "Inbox")
         "* [ ] [[%link]]
:PROPERTIES:
:TITLE: %^{Title|Unknown}
:AUTHOR: %^{Author|Unknown}
:CREATED: %U
:SOURCE: %^{Source|Unknown}
:CATEGORY: %^{Category|Unknown}
:LINK: %^{Link|Unknown}
:RELEASE_DATE: %^{Release Date|Unknown}
:PUBLISHER: %^{Publisher|Unknown}
:TAGS: %^{Tags|Unknown}
:PAGES: %^{Pages}
:END:"
         :prepend t)

        ;; ("s" "Tambreet Capture")
        ;; ("sd" "Dataset" table-line (file+headline "~/org/Others/Tambreet.org" "Dataset")
        ;;      "| %^{Players|2|3|4|5} | %^{Color|Blue|Green|Purple|White} | %^{Minutes} | %^{Seconds} | %^{Declared} | %^{Extracted} | %^{Time-Stamp}U | %^{Notes} |"
        ;;      :kill-budder t)
        ;; ("si" "Tambreet Idea" table-line (file+headline "~/org/Others/Tambreet.org" "Inbox")
        ;;      "* IDEA %?  %U\n")
        ;; ("st" "Tambreet Todo" table-line (file+headline "~/org/Others/Tambreet.org" "Inbox")
        ;;      "* TODO %?  %U\n")

        ("t" "Tasks / Projects")
        ("ta" "Scheduled Task" entry (file+headline "~/org/Me/Tasks.org" "Schedule")
         "* %^{Task}\nSCHEDULED: %^{When}t\n:PROPERTIES:\n:CREATED: %U\n:DESCRIPTION: %^{Description}\n:END:\n%?\n"
         :prepend t)
        ("td" "Deadline Task" entry (file+headline "~/org/Me/Tasks.org" "Schedule")
         "* %^{Task}\nDEADLINE: %^{When}t\n:PROPERTIES:\n:CREATED: %U\n:DESCRIPTION: %^{Description}\n:END:\n%?\n"
         :prepend t)
        ("tb" "Remember Task" entry (file+headline "~/org/Me/Tasks.org" "Schedule")
         "* %^{Task}\n%^{When}t\n:PROPERTIES:\n:CREATED: %U\n:DESCRIPTION: %^{Description}\n:END:\n%?\n"
         :prepend t)
        ("tc" "Today's Tasks" entry (file+olp+datetree "~/org/Me/Tasks.org" "Daily")
         "* %?"
         :clock-in :clock-resume :prepend t)
        ("ti" "Interrupt" entry (file+headline "~/org/Me/Tasks.org" "Inbox")
         "* %T %a :INTERRUPT:\n\n%?\n\n"
         :clock-in :clock-resume :prepend t)
        ("tm" "Meeting Entry" entry (file+headline "~/org/Me/Tasks.org" "Schedules")
         "* %^{Purpouse} :MEETING:\nSCHEDULED: %^{When}t\n:PROPERTIES:\n:CREATED: %U\n:WITH: %^{With}\n:DESCRIPTION: %^{Description}\n:END:\n%?\n"
         :prepend t)
        ("tt" "Task" entry (file+headline "~/org/Me/Tasks.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :prepend t)
        ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n%i"
         :prepend t)

        ;; ("w" "Workflows")
        ;; ("we" "Checking Email" entry (file+olp+datetree ,(dw/get-todays-journal-file-name))
        ;;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
        ("w" "Workout" table-line (file+datetree "~/org/Me/Workout.org")
         "| %^{Type of Workout|Calisthenics|Boldering|Stretching|Yoga|Swimming|Others}|%^{Exercises}| | |%^{Start Time-Stamps}U|%^{End Time-Stamps}U|"
         :tree-type month :kill-buffer t)))
;; }}}
