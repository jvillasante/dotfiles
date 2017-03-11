(defconst jv-org-packages
  '(org))

(defun jv-org/post-init-org ()
  (with-eval-after-load 'org
    ;; org problems
    (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
    (setq org-clock-line-re "^[    ]*CLOCK:")

    (add-hook 'org-mode-hook
              (lambda ()
                (spacemacs/toggle-auto-fill-mode-on)
                (set-fill-column 110)))

    (setq org-startup-indented t)
    (setq org-indent-mode t)

    ;; set maximum indentation for description lists
    (setq org-list-description-max-indent 5)

    ;; prevent demoting heading also shifting text inside sections
    (setq org-adapt-indentation nil)))
