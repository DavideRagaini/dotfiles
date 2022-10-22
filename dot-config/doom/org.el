;;; ../../.local/src/dotfiles/dot-config/doom/org.el -*- lexical-binding: t; -*-
;; ========= Org Misc ========= {{{
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

(setq org-directory "~/Org"
      org-agenda-files
      '("~/Org/Me/Tasks.org"
        "~/Org/Me/Habits.org"
        "~/Org/Me/Learn.org"
        "~/Org/Others/Birthdays.org")
      org-agenda-log-mode-items '(state closed clock)
      org-agenda-start-with-log-mode t
      org-archive-location "~/Org/Archive/%s_archive::"
      org-columns-default-format "%4TODO(ToDo) %40ITEM(Task) %2PRIORITY %6CLOCKSUM(Clock) %8Effort(Estimated Effort){:} %TAGS(Tags)"
      ;; org-agenda-include-diary t
      ;; org-agenda-include-inactive-timestamps t
      ;; org-agenda-show-log 'only
      ;; org-clock-persist 'history
      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      org-clock-persist t
      ;; Resume clocking task on clock-in if the clock is open
      org-clock-in-resume t
      ;; Save clock data and state changes and notes in the LOGBOOK drawer
      org-clock-into-drawer t
      ;; Removes clocked tasks with 0:00 duration
      org-clock-out-remove-zero-time-clocks t
      ;; Clock out when moving task to a done state
      org-clock-out-when-done t
      ;; Enable auto clock resolution for finding open clocks
      org-clock-auto-clock-resolution (quote when-no-clock-is-running)
      ;; Include current clocking task in clock reports
      org-clock-report-include-clocking-task t
      org-ellipsis " ¶"
      org-global-properties
      '(("Effort_ALL" .
      ;;   1    2    3    4    5    6    7    8    9    0
         "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))
      org-hide-emphasis-markers t
      org-habit-graph-column 40
      org-list-demote-modify-bullet '(("+" . "*") ("*" . "-") ("-" . "+"))
      org-log-done 'time
      ;; org-log-into-drawer t
      ;; Use pretty things for the clocktable
      org-pretty-entities t
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      org-tags-column -1
      )

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
    ("ad" "Debts" table-line (file+headline "~/Org/Me/Accounting.org" "Debts")
          "| %^{Quantity} | %^{Reason} | %^{Note} | %^{TimeStamp}U |"
          :kill-buffer t :prepend t)
    ("ag" "Give" table-line (file+headline "~/Org/Me/Accounting.org" "Give")
          "| %^{Quantity} | %^{Reason} | %^{Note} | %^{TimeStamp}U |"
          :kill-buffer t :prepend t)
    ("ah" "Have" table-line (file+headline "~/Org/Me/Accounting.org" "Have")
          "| %^{Quantity} | %^{Reason} | %^{Note} | %^{TimeStamp}U |"
          :kill-buffer t :prepend t)
    ("as" "Spent" table-line (file+headline "~/Org/Me/Accounting.org" "Spent")
          "| %^{Quantity} | %^{Reason} | %^{Note} | %^{TimeStamp}U |"
          :kill-buffer t :prepend t)

    ("b" "Personal Capture")
    ("bb" "Add Birtday" entry (file+headline "~/Org/Others/Birthdays.org" "Inbox")
          "* %? %T\n"
          :kill-buffer t :prepend t)
    ("bm" "Add to Music List" entry (file+headline "~/Org/Me/Music.org" "Inbox")
          "* [ ] %^{Title} - %^{Artist}\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %^{Source}\n:LINK: %^{Link}\n:END:"
          :kill-buffer t :prepend t)
    ("br" "Add to Read List" entry (file+headline "~/Org/Me/Read.org" "Inbox")
          "* [ ] %^{Title} - %^{Author}\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %^{Source}\n:LINK: %^{Link}\n:END:"
          :kill-buffer t :prepend t)
    ("bw" "Add to Watch List" entry (file+headline "~/Org/Me/Watch.org" "Inbox")
          "* [ ] %^{Title} - %^{Author/Director}\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %^{Source}\n:LINK: %^{Link}\n:END:"
          :kill-buffer t :prepend t)

    ("g" "Goals Capture")
    ("gp" "Primary Goals" entry (file+olp "~/Org/Me/Goals.org" "Ideals" "Inbox")
          "* %?" :kill-buffer t :prepend t)
    ("gs" "Secondary Goals" entry (file+olp "~/Org/Me/Goals.org" "Secondary" "Inbox")
          "* %?" :kill-buffer t :prepend t)

    ("m" "Metrics Capture")
    ("md" "Drink Journal" table-line (file+headline "~/Org/Me/Metrics.org" "Hydro Journal")
          "| | %U | %^{Water|0|200} | %^{The|0|300} | %^{Coffee|0|1} | %^{Beer|0|330} | %^{Drinks|0|400} | %^{Sodas|0|150} | %^{Notes} |"
          :kill-buffer t :prepend t)
    ("ms" "Sleep Journal" table-line (file+headline "~/Org/Me/Metrics.org" "Sleep Journal")
          "| %^{Sleep TimeStamp}U | %^{Wake TimeStamp}U | | |"
          :kill-buffer t)
    ("mw" "Weight" table-line (file+headline "~/Org/Me/Metrics.org" "Weight")
          "| %U | %^{Weight} |"
          :kill-buffer t)

    ("n" "Note Entries")
    ("nb" "Protocol Link Blank" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "* NOTE %? %U"
          :prepend t)
    ("nl" "Protocol Link" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "* NOTE %?[[%:link][%:description]] %U"
          :prepend t)
    ("np" "Protocol" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "* NOTE %?[[%:link][%:description]] %U\n%i"
          :prepend t)
    ("ns" "Shopping Note" entry (file+headline "~/Org/Me/Shopping_List.org" "Inbox")
          "* NOTE %? %U"
          :prepend t)
    ("nx" "Protocol Link from Clipboard" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "* NOTE %?%x %U"
          :prepend t)

    ("j" "Journal Entries")
    ("jd" "Dream" entry (file+olp+datetree "~/Org/Me/Journal.org")
          "* %<%R> Dream :journal:\n%?"
          :clock-in :clock-resume :empty-lines 1 :empty-lines-after 1)
    ("jj" "Journal note" entry (file+olp+datetree "~/Org/Me/Journal.org")
          "* %<%R> Journal :journal:\n%?"
          :clock-in :clock-resume :empty-lines 1 :empty-lines-after 1)
    ("jh" "Hangout" table-line (file+headline "~/Org/Me/Journal.org" "Hangouts")
          "| %^{Activity} | %^{Notes} | %^{With} | %^{Time-Stamp}U |"
          :prepend t :empty-lines 1 :empty-lines-after 1)
    ("jp" "Daily Planning" entry (file+olp+datetree "~/Org/Me/Journal.org")
          "* [ ] %<%R> %?"
          :prepend t :empty-lines 1 :empty-lines-after 1)

    ("s" "Tambreet Capture")
    ("sd" "Dataset" table-line (file+headline "~/Org/Others/Tambreet.org" "Dataset")
          "| %^{Players|2|3|4|5} | %^{Color|Blue|Green|Purple|White} | %^{Minutes} | %^{Seconds} | %^{Declared} | %^{Extracted} | %^{Time-Stamp}U | %^{Notes} |"
          :kill-budder t)
    ("si" "Tambreet Idea" table-line (file+headline "~/Org/Others/Tambreet.org" "Inbox")
          "* IDEA %?  %U\n")
    ("st" "Tambreet Todo" table-line (file+headline "~/Org/Others/Tambreet.org" "Inbox")
          "* TODO %?  %U\n")

    ("t" "Tasks / Projects")
    ("tc" "Time Journal" entry (file+olp+datetree "~/Org/Me/Tasks.org" "Daily")
          "* %?"
          :clock-in :clock-resume :prepend t)
    ("ti" "Interrupt" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
          "* %T %a :INTERRUPT:\n\n%?\n\n"
          :clock-in :clock-resume :prepend t)
    ("tm" "Meeting Entry" entry (file+headline "~/Org/Me/Tasks.org" "Schedules")
          "* %^{Purpouse} :MEETING:\nSCHEDULED: %^{When}t\n:PROPERTIES:\n:CREATED: %U\n:WITH: %^{With}\n:DESCRIPTION: %^{Description}\n:END:\n%?\n"
          :prepend t)
    ("tt" "Task" entry (file+headline "~/Org/Me/Tasks.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
         :prepend t)
    ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a\n%i"
         :prepend t)

    ;; ("w" "Workflows")
    ;; ("we" "Checking Email" entry (file+olp+datetree ,(dw/get-todays-journal-file-name))
    ;;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
    ("w" "Workout" table-line (file+headline "~/Org/Me/Habits.org" "Workout")
         "| %^{Type of Workout|Calisthenics|Stretching|Yoga|Swimming|Others}|%^{Exercises}| | |%^{Start Time-Stamps}T|%^{End Time-Stamps}T|"
          :kill-buffer t)))
;; }}}
