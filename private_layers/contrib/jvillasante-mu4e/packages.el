;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq jvillasante-mu4e-packages
      '(
        mu4e-alert
        ))

;; List of packages to exclude.
(setq jvillasante-mu4e-excluded-packages '())

(defun jvillasante-mu4e/post-init-mu4e-alert ()
  (evil-leader/set-key
    "ou" 'mu4e-alert-view-unread-mails)

  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread"
         " AND NOT flag:trashed"
         " AND maildir:"
         "\"/Inbox\"")))
