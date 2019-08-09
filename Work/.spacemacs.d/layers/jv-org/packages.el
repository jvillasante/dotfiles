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
        (setq org-startup-indented t)
        (setq org-indent-mode t)
        (setq org-list-description-max-indent 5) ;; set maximum indentation for description lists
        (setq org-adapt-indentation nil) ;; prevent demoting heading also shifting text inside sections
        (setq org-cycle-separator-lines 1)
        (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
        (setq org-agenda-file-regexp "\\`[^.].*\\.\\(org\\.txt\\|org\\)\\'")
        (setq org-clock-idle-time 15)
        (setq org-ellipsis " ▼") ;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html

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
        (setq org-default-notes-file (concat org-directory "inbox.org.txt"))
        (setq jv/org-default-habits-file (concat org-directory "habits.org.txt"))

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
            (quote (("t" "todo" entry (file org-default-notes-file)
                        "* TODO %?\n%U\n%a\n")
                       ("m" "meeting" entry (file org-default-notes-file)
                           "* MEETING with %? :MEETING:\n%U")
                       ("i" "idea" entry (file org-default-notes-file)
                           "* %? :IDEA:\n%U\n%a\n")
                       ("n" "note" entry (file org-default-notes-file)
                           "* %? :NOTE:\n%U\n%a\n")
                       ("h" "habit" entry (file rae/org-default-notes-file)
                           "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

        ;; refiling
        (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                            (org-agenda-files :maxlevel . 9))))

        ;; pomodoro
        ;; (add-hook 'org-pomodoro-finished-hook (lambda()
        ;;                                           (org-journal-new-entry nil)))
        ))