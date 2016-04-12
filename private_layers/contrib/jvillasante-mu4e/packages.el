;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq jvillasante-mu4e-packages
      '(
        mu4e-alert
        ))

;; List of packages to exclude.
(setq jvillasante-mu4e-excluded-packages '())

(defun jvillasante-mu4e/init-mu4e-alert ()
  (use-package mu4e-alert
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (evil-leader/set-key
        "ou" 'mu4e-alert-view-unread-mails)

      (setq mu4e-alert-interesting-mail-query
            (concat
             "flag:unread"
             " AND NOT flag:trashed"
             " AND maildir:"
             "\"/Inbox\""))

      ;; (mu4e-alert-set-default-style 'libnotify)
      ;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
      (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))))
