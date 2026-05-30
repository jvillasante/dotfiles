;;; my-init-mail.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(use-package mu4e
    :ensure nil ;; built-in
    :defer t
    :custom
    ;; Some defaults
    (mu4e-modeline-show-global nil)
    (mu4e-hide-index-messages t)
    (mu4e-view-show-images t)
    (mu4e-search-skip-duplicates t)
    ;; Tell Emacs to use an external sendmail program
    (send-mail-function 'message-send-mail-with-sendmail)
    (message-send-mail-function 'message-send-mail-with-sendmail)
    (sendmail-program "msmtp")
    ;; Tell msmtp to read the "From:" field in your email to figure out
    ;; which account to use from your ~/.msmtprc file automatically!
    (message-sendmail-extra-arguments '("--read-envelope-from"))
    (message-sendmail-f-is-evil t))

;; org-msg
(use-package org-msg :defer t)

;; mu4easy : A global minor mode that defines a full working setup for mu4e and mbsync
;; NOTE: mu4easy handles org-msg setup (activation, options, greeting, alternatives, etc.)
(use-package mu4easy
    :defer t
    :preface
    (defun my/change-message-send-mail-function ()
        "Using sendmail"
        (setq message-send-mail-function #'message-send-mail-with-sendmail))
    (defun my/mu4e-action-view-in-firefox (msg)
        "View MSG in Firefox."
        (let ((browse-url-browser-function #'browse-url-firefox))
            (mu4e-action-view-in-browser msg)))
    (defun my/mu4easy-patch-omicron-context ()
        "Inject the Nightly Builds shortcut into the mu4easy-generated Omicron context."
        (let ((shortcut '("/julio.villasante@omicronmedia.com/Nightly" . ?n)))
            (when-let* ((ctx (seq-find (lambda (c) (string= (mu4e-context-name c) "Omicron"))
                                 mu4e-contexts))
                           (vars (mu4e-context-vars ctx))
                           (shortcuts (alist-get 'mu4e-maildir-shortcuts vars)))
                (unless (member shortcut shortcuts)
                    (setf (alist-get 'mu4e-maildir-shortcuts vars)
                        (append shortcuts (list shortcut)))))))
    (defun my/mu4easy-patch-gmail-context ()
        "Drop the Sent shortcut from the Gmail context.
The local Gmail Sent folder is intentionally not synced: Gmail double-labels
sent mail with `[Gmail]/Sent Mail' AND `[Gmail]/All Mail', so syncing both
duplicates the messages on disk.  Sent mail is still browsable under Archive
(which maps to All Mail)."
        (when-let* ((ctx (seq-find (lambda (c) (string= (mu4e-context-name c) "Google"))
                             mu4e-contexts))
                       (vars (mu4e-context-vars ctx)))
            (setf (alist-get 'mu4e-maildir-shortcuts vars)
                (seq-remove (lambda (s) (eq (cdr s) ?s))
                    (alist-get 'mu4e-maildir-shortcuts vars)))))
    (defun my/mu4easy-setup ()
        "Initialize mu4easy and apply post-initialization patches."
        (mu4easy-mode)
        (mu4e-alert-disable-mode-line-display)
        (setq mu4e-update-interval 600)
        (with-eval-after-load 'mu4e
            ;; Patch the Omicron context (add the "Nightly Builds" shortcut)
            (my/mu4easy-patch-omicron-context)

            ;; Patch the Gmail context (drop the dead "Sent" shortcut)
            (my/mu4easy-patch-gmail-context)

            ;; Patch the Bookmarks List
            (unless (seq-find (lambda (b) (eq (plist-get b :key) ?n)) mu4e-bookmarks)
                (setq mu4e-bookmarks
                    (append mu4e-bookmarks
                        '((:query "maildir:\"/julio.villasante@omicronmedia.com/Nightly\" AND date:today..now"
                              :name "Today's Omicron Nightly Builds"
                              :key ?n)))))))
    :hook ((after-init . my/mu4easy-setup)
              (mu4easy-mode . my/change-message-send-mail-function)
              (mu4e-compose-mode . org-msg-edit-mode))
    :bind ("C-c o u" . mu4e)
    :custom
    (mu4easy-signature "---\nRegards,\nJulio")
    (mu4easy-accounts '("Gmail" "iCloud" "Omicron"))
    (mu4easy-headers
        '((:human-date . 18) (:flags . 6) (:short-folder . 18) (:from-or-to . 26)
             (:mailing-list . 10) (:tags . 10) (:subject . 92)))
    (mu4easy-contexts
        '((mu4easy-context
              :c-name  "Google"
              :maildir "jvillasantegomez@gmail.com"
              :mail    "jvillasantegomez@gmail.com"
              :sent-action delete)
             (mu4easy-context
                 :c-name  "iCloud"
                 :maildir "julio.villasante@icloud.com"
                 :mail    "julio.villasante@icloud.com")
             (mu4easy-context
                 :c-name  "Omicron"
                 :maildir "julio.villasante@omicronmedia.com"
                 :mail    "julio.villasante@omicronmedia.com")))
    :config
    (with-eval-after-load 'mu4e
        ;; Add Firefox action (`a' and then `V' when viewing Email)
        (add-to-list 'mu4e-view-actions '("View in Firefox" . my/mu4e-action-view-in-firefox) t)

        ;; Add Custom Header Field (shorter so it fits)
        (add-to-list 'mu4e-header-info-custom
            '(:short-folder .
                 (:name "Folder"
                     :shortname "Folder"
                     :help "Folder name with short account alias"
                     :function (lambda (msg)
                                   (let* ((maildir (mu4e-message-field msg :maildir))
                                             (aliases '(("jvillasantegomez@gmail.com"        . "Gmail")
                                                           ("julio.villasante@icloud.com"       . "iCloud")
                                                           ("julio.villasante@omicronmedia.com" . "Omicron"))))
                                       (if (string-match "^/\\([^/]+\\)/\\(.*\\)" maildir)
                                           (let* ((account (match-string 1 maildir))
                                                     (folder  (match-string 2 maildir))
                                                     (alias   (cdr (assoc account aliases))))
                                               (if alias
                                                   (concat alias "/" folder)
                                                   (concat account "/" folder)))
                                           maildir))))))))

(provide 'my-init-mail)
;;; my-init-mail.el ends here
