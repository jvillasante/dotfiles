;;; my-init-org.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package org
    :ensure nil ;; emacs built-in
    :preface
    (defun my--org-setup ()
        ;; org wants 8 as tab-width
        (setq-local tab-width 8)
        (setq-local indent-tabs-mode nil)

        ;; disable auto-pairing of "<" in org-mode
        (setq-local electric-pair-inhibit-predicate
                    `(lambda (c)
                         (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))

        ;; setup org-babel languages
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((awk . t)
           (calc .t)
           (C . t)
           (emacs-lisp . t)
           (haskell . t)
           (gnuplot . t)
           (latex . t)
           ;; (ledger . t)
           (js . t)
           (haskell . t)
           ;; (http . t)
           (perl . t)
           (python . t)
           ;; (gnuplot . t)
           ;; org-babel does not currently support php.  That is really sad.
           ;; (php . t)
           (R . t)
           (scheme . t)
           ;; (sh . t)
           (sql . t)
           (sqlite . t))))
    :hook ((org-mode . my--org-setup))
    :config
    (setq org-directory (expand-file-name "Apps/org" my--dropbox-path))
    (setq org-id-locations-file (file-name-concat org-directory ".orgids"))
    (setq org-pretty-entities nil)
    (setq org-fontify-whole-heading-line t)
    (setq org-fontify-done-headline t)
    (setq org-fontify-quote-and-verse-blocks t)
    (setq org-startup-indented t)
    (setq org-startup-folded t)

    ;; Return or left-click with mouse follows link
    (setq org-return-follows-link t)
    (setq org-mouse-1-follows-link t)

    ;; Display links as the description provided
    (setq org-link-descriptive t)

    ;; Hide markup markers
    (setq org-hide-emphasis-markers t)

    ;; Hide the first N-1 stars in a headline.
    (setq org-hide-leading-stars t))

(use-package org-capture
    :ensure nil ;; emacs built-in
    :init
    ;; when refiling from org-capture, Emacs prompts to kill the
    ;; underlying, modified buffer. This fixes that.
    ;; (add-hook 'org-after-refile-insert-hook #'save-buffer)
    :config
    (setq org-capture-templates
          `(("i" "Inbox (inbox.org)" entry  (file "inbox.org")
             ,(concat "* TODO %?\n"
                      "/Entered on/ %U"))
            ;; ("m" "Meeting (agenda.org)" entry  (file+headline "agenda.org" "Future")
            ;;     ,(concat "* %? :meeting:\n"
            ;;          "<%<%Y-%m-%d %a %H:00>>"))
            ("n" "Note (notes.org)" entry  (file "notes.org")
             ,(concat "* Note (%a)\n"
                      "/Entered on/ %U\n" "\n" "%?"))))

    (org-capture-put :kill-buffer t)) ;; kill org capture buffer by default

(use-package org-agenda
    :ensure nil ;; emacs built-in
    :config
    ;; org-agenda will visit all org files listed
    ;; in `org-agenda-files' to generate the org-agenda view.
    ;; avoid too much files inside this directory.
    ;; (setq org-agenda-files (list "inbox.org" "agenda.org" "notes.org"))
    (setq  org-agenda-files `(,org-directory
                              ,@(mapcar
                                 (lambda (x) (file-name-concat org-directory x))
                                 '("inbox" "agenda" "notes"))))

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
    (setq org-agenda-hide-tags-regexp ".")
    (setq org-agenda-prefix-format
          '((agenda . " %i %-12:c%?-12t% s")
            (todo   . " ")
            (tags   . " %i %-12:c")
            (search . " %i %-12:c"))))

(use-package org-superstar
    :after org
    :init (add-hook 'org-mode-hook (lambda () (org-superstar-mode)))
    :config
    (setq org-superstar-remove-leading-stars t)
    (setq org-superstar-leading-bullet ?\s)
    (setq org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●"))
    (setq org-superstar-special-todo-items t))

;; deft : plain text notes
(use-package deft
    :custom ((deft-directory (expand-file-name "Apps/org/notes" my--dropbox-path))
             (deft-extensions '("org" "md" "txt"))
             (deft-default-extension "org")
             (deft-recursive nil)
             (deft-use-filename-as-title nil)
             (deft-use-filter-string-for-filename t)
             (deft-file-naming-rules '((noslash . "-")
                                       (nospace . "-")
                                       (case-fn . downcase)))
             (deft-auto-save-interval 0)))

(provide 'my-init-org)
;;; my-init-org.el ends here
