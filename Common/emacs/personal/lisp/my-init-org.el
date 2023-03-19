;;; my-init-org.el -*- lexical-binding: t; -*-

(straight-use-package '(org :type built-in))
(straight-use-package 'deft)

;;;; Org mode : Base mode for note taking
;; (use-package org
;;     :config
;;     ;; To get the most out of themes
;;     (setq org-fontify-whole-heading-line t
;;         org-fontify-done-headline t
;;         org-fontify-quote-and-verse-blocks t)
;;     (setq org-startup-indented t)
;;     (setq org-startup-folded t)
;;     (setq org-hide-emphasis-markers t) ;; hide the emphasis markup (e.g. /.../ for italics, *...* for bold, etc.)
;;     ;; Capture templates
;;     (setq org-capture-templates
;;         `(("i" "Inbox (inbox.org)" entry  (file "inbox.org")
;;               ,(concat "* TODO %?\n"
;;                    "/Entered on/ %U"))
;;              ;; ("m" "Meeting (agenda.org)" entry  (file+headline "agenda.org" "Future")
;;              ;;     ,(concat "* %? :meeting:\n"
;;              ;;          "<%<%Y-%m-%d %a %H:00>>"))
;;              ("n" "Note (notes.org)" entry  (file "notes.org")
;;                  ,(concat "* Note (%a)\n"
;;                       "/Entered on/ %U\n" "\n" "%?"))))
;;     ;; Agenda
;;     (setq org-agenda-hide-tags-regexp ".")
;;     (setq org-agenda-prefix-format
;;         '((agenda . " %i %-12:c%?-12t% s")
;;              (todo   . " ")
;;              (tags   . " %i %-12:c")
;;              (search . " %i %-12:c")))
;;     :hook (org-mode . (lambda()
;;                           (display-line-numbers-mode nil) ;; no line numbers in org-mode
;;                           ;; disable auto-pairing of "<" in org-mode
;;                           (setq-local electric-pair-inhibit-predicate
;;                               `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
;;                           (add-to-list 'recentf-exclude ".*org$") ; Ignore org files from recentf due to agenda loading everything
;;                           (variable-pitch-mode)
;;                           (visual-line-mode))))

(use-package org
    :init
    (my/open-map
        :keymaps 'override
        "a" #'org-agenda
        "A" #'my/org-agenda-visited-all-directories
        "c" #'org-capture
        "o" #'org-clock-goto)

    (setq org-directory (expand-file-name "Apps/org" my/dropbox-path)
        org-archive-location (expand-file-name "%s_archive::" (concat org-directory "/archive"))
        org-id-locations-file (file-name-concat org-directory ".orgids")
        org-id-locations-file-relative t
        org-highlight-latex-and-related '(native latex script entities)
        org-special-ctrl-a/e t
        org-indirect-buffer-display 'current-window
        ;; NOTE: inline image link cannot have description.  See org
        ;; manual 12.7 for further instruction if you want to add
        ;; description for a link which is inline image.  By default
        ;; the "remote" means tramp files. Inline http image is not
        ;; supported yet.
        org-display-remote-inline-images 'download
        org-tags-column 0 ;; don't indent tags, put tags directly behind the heading
        org-M-RET-may-split-line nil
        org-return-follows-link t
        org-insert-heading-respect-content t
        org-startup-indented t
        org-enforce-todo-dependencies t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-pretty-entities t ;; display \alpha as utf8 chars in overlay
        org-image-actual-width nil
        org-imenu-depth 6
        ;; Save target buffer after archiving a node.
        org-archive-subtree-save-file-p t
        org-priority-faces '((?A . error)
                                (?B . warning)
                                (?C . success))
        org-startup-indented t
        org-tags-column 0
        ;; Resume when clocking into task with open clock
        org-clock-in-resume t
        org-use-sub-superscripts '{}
        ;; `showeverything' is org's default, but it doesn't respect
        ;; `org-hide-block-startup' (#+startup: hideblocks), archive trees,
        ;; hidden drawers, or VISIBILITY properties. `nil' is equivalent, but
        ;; respects these settings.
        org-startup-folded nil
        ;; Prevent modifications made in invisible sections of an org document, as
        ;; unintended changes can easily go unseen otherwise.
        org-catch-invisible-edits 'smart
        org-preview-latex-image-directory (file-name-concat "~/.cache" "ltximg/")
        org-todo-keywords '((sequence "TODO(t)" "STRT(s)" "WAIT(w)" "HOLD(h)"
                                "|" "DONE(d)" "KILL(k)"))
        ;; don't load those org modules that I never use
        org-modules '(ol-doi ol-bbdb ol-bibtex ol-info ol-eww))

    ;; copied from doomemacs
    (setq org-refile-targets
        '((nil :maxlevel . 5)
             (org-agenda-files :maxlevel . 5))
        ;; Without this, completers like ivy/helm are only given the first level of
        ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
        ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
        ;; target! e.g. FILE/Tasks/heading/subheading
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

    (defface my&org-todo-active '((t :inherit (bold font-lock-constant-face org-todo))) "")
    (defface my&org-todo-onhold '((t :inherit (bold warning org-todo))) "")
    (defface my&org-todo-cancel '((t :inherit (bold error org-todo))) "")

    (setq org-todo-keyword-faces '(("STRT" . my&org-todo-active)
                                      ("WAIT" . my&org-todo-onhold)
                                      ("HOLD" . my&org-todo-onhold)
                                      ("KILL" . my&org-todo-cancel)))

    :config
    (add-to-list 'org-file-apps '(remote . emacs))

    (general-define-key
        :keymaps 'org-mode-map
        "TAB" #'org-cycle
        [remap imenu] #'consult-org-heading
        [remap consult-imenu] #'consult-org-heading)

    (run-with-idle-timer 2 nil #'my/load-org-extensions-idly))

(use-package org-capture
    :config
    (setq my/org-capture-todo-file (file-name-concat "capture" "todo.org")
        my/org-capture-notes-file (file-name-concat "capture" "notes.org")
        my/org-capture-english-note-file (file-name-concat "capture" "english.org")
        my/org-capture-bubble-tea-live-file (file-name-concat "capture" "bubble-tea.org"))

    (setq org-capture-templates
        `(("t" "Personal todo" entry
              (file ,my/org-capture-todo-file)
              "* TODO %?%^g\nSCHEDULED: %t" :prepend t)
             ("n" "Personal notes" entry
                 (file ,my/org-capture-notes-file)
                 "* %u %?\n%i\n%a" :prepend t)
             ("j" "Applied jobs" checkitem
                 (file+olp ,my/org-capture-todo-file "Applied jobs")
                 "- [ ] %?")
             ("i" "clock in(start a timer)" entry
                 (file ,my/org-capture-todo-file)
                 "* TODO %?%^g\nSCHEDULED: %t"
                 :clock-in t :clock-keep t :prepend t)

             ("e" "English notes")
             ("em" "Manually fill" entry
                 (file+olp+datetree ,my/org-capture-english-note-file)
                 "* %^{prompt}\n%i\n\n%a\n:explanation:\n%?\n:END:")
             ("ew" "quick fill with word and sentence under point" entry
                 (file+olp+datetree ,my/org-capture-english-note-file)
                 ,(concat "* %(with-current-buffer (org-capture-get :original-buffer) (current-word))"
                      "\n%(with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'sentence t))"
                      "\n\n%a\n:explanation:\n%?\n:END:"))
             ("er" "quick fill with selected region and sentence under point" entry
                 (file+olp+datetree ,my/org-capture-english-note-file)
                 ,(concat "* %i"
                      "\n%(with-current-buffer (org-capture-get :original-buffer) (thing-at-point 'sentence t))"
                      "\n\n%a\n:explanation:\n%?\n:END:"))))

    (org-capture-put :kill-buffer t) ;; kill org capture buffer by default
    ;; when refiling from org-capture, Emacs prompts to kill the
    ;; underlying, modified buffer. This fixes that.
    (add-hook 'org-after-refile-insert-hook #'save-buffer))

(use-package org-agenda
    :init
    ;; Different colors for different priority levels
    (setq org-agenda-deadline-faces '((1.001 . error)
                                         (1.0 . org-warning)
                                         (0.5 . org-upcoming-deadline)
                                         (0.0 . org-upcoming-distant-deadline))
        org-agenda-skip-unavailable-files t
        org-agenda-span 10
        ;; always start on today
        org-agenda-start-on-weekday nil
        org-agenda-start-day "-3d"
        org-agenda-inhibit-startup t
        org-agenda-window-setup 'other-tab)

    :config
    ;; org-agenda will visit all org files listed
    ;; in `org-agenda-files' to generate the org-agenda view.
    ;; avoid too much files inside this directory.
    (setq  org-agenda-files `(,org-directory
                                 ,@(mapcar
                                       (lambda (x) (file-name-concat org-directory x))
                                       '("capture" "work" "roam"))))

    (advice-add #'org-get-agenda-file-buffer :around #'my/exclude-org-agenda-buffers-from-recentf)
    (add-hook 'org-agenda-finalize-hook #'my/reload-org-agenda-buffers)

    (general-define-key
        :keymaps 'org-agenda-mode-map
        "gt" #'tab-bar-switch-to-next-tab
        "gT" #'tab-bar-switch-to-prev-tab))

;; deft : plain text notes
(use-package deft
    :config
    (setq
        deft-directory (expand-file-name "Apps/org/notes" my/dropbox-path)
        deft-extensions '("org" "md" "txt")
        deft-default-extension "org"
        deft-recursive nil
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                    (nospace . "-")
                                    (case-fn . downcase))
        deft-auto-save-interval 0))

(provide 'my-init-org)
;;; my-init-org.el ends here
