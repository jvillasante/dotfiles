(defconst jv-org-packages
    '(org))

(defun jv-org/post-init-org ()
    (with-eval-after-load 'org
        ;; hook
        (add-hook 'org-mode-hook
            (lambda ()
                (spacemacs/toggle-auto-fill-mode-on)
                (set-fill-column 110)))

        ;; org problems
        (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
        (setq org-clock-line-re "^[    ]*CLOCK:")

        ;; settings
        (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|org\\.txt\\)$" . org-mode))
        (setq org-agenda-window-setup (quote current-window)) ;; open agenda in current window
        (setq org-startup-indented t)
        (setq org-indent-mode t)
        (setq org-list-description-max-indent 5) ;; set maximum indentation for description lists
        (setq org-adapt-indentation nil) ;; prevent demoting heading also shifting text inside sections
        (setq org-cycle-separator-lines 2)
        (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
        (setq org-agenda-file-regexp "\\`[^.].*\\.\\(org\\.txt\\|org\\)\\'")
        (setq org-clock-idle-time 15)

        ;; To get the most out of themes
        (setq org-fontify-whole-heading-line t
            org-fontify-done-headline t
            org-fontify-quote-and-verse-blocks t)

        ;; todos
        (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                       (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MIGRATED(m@/!)" "PHONE" "MEETING"))))

        (setq org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                       ("WAITING" ("WAITING" . t))
                       ("MIGRATED" ("MIGRATED" . t))
                       ("HOLD" ("WAITING") ("HOLD" . t))
                       (done ("WAITING") ("HOLD"))
                       ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                       ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                       ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

        ;; organizer directory
        (setq org-directory (concat jv/dropbox-path "/Personal/org/"))
        (setq org-default-notes-file (concat org-directory "inbox.org"))
        (setq jv/org-bookmarks-file (concat org-directory "bookmarks.org"))

        ;; agenda
        (setq org-agenda-files (list org-directory))
        (setq org-agenda-skip-scheduled-if-done t)
        (setq org-agenda-skip-deadline-if-done t)

        ;; tags
        ;; Tags with fast selection keys
        (setq org-tag-alist (quote ((:startgroup)
                                       ("@errand" . ?e)
                                       ("@office" . ?o)
                                       ("@home" . ?H)
                                       (:endgroup)
                                       ("WAITING" . ?w)
                                       ("MIGRATED" . ?M)
                                       ("HOLD" . ?h)
                                       ("IDEA" . ?i)
                                       ("PERSONAL" . ?P)
                                       ("DRAFT" . ?D)
                                       ("WORK" . ?W)
                                       ("NOTE" . ?n)
                                       ("CANCELLED" . ?c)
                                       ("FLAGGED" . ??))))

        ;; capture
        (setq org-capture-templates
            (quote (("t" "todo" entry (file+headline org-default-notes-file "Tasks")
                        ;; "* TODO %?\n%U\n%a\n")
                        "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
                       ("m" "meeting" entry (file+headline org-default-notes-file "Meetings")
                           "* MEETING with %? :MEETING:\n%U")
                       ("i" "idea" entry (file+headline org-default-notes-file "Ideas")
                           "* %? :IDEA:\n%U\n%a\n")
                       ("n" "note" entry (file+headline org-default-notes-file "Notes")
                           "* %? :NOTE:\n%U\n%a\n")
                       ("b" "Bookmark" entry (file+headline jv/org-bookmarks-file "Bookmarks")
                           "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
                       ("h" "habit" entry (file+headline org-default-notes-file "Habits")
                           "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

        ;; refiling
        (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                            (org-agenda-files :maxlevel . 9))))

        ;; pomodoro
        ;; (add-hook 'org-pomodoro-finished-hook (lambda()
        ;;                                           (org-journal-new-entry nil)))
        ))
