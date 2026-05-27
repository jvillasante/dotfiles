;;; my-init-mail.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;; Email setup: mu4e + msmtp + org-msg, three accounts (Gmail, iCloud, Omicron).
;;
;;; Code:

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

;;;
;;; mu4e
;;;
(use-package mu4e
    :ensure nil ;; built-in
    :defer t
    :bind ("C-c o u" . mu4e)
    :hook (mu4e-context-changed . my/mu4e-update-main-buffer)
    :preface
    (defun my/mu4e-compose-hook ()
        "Compose-mode tweaks shared with `org-msg-edit-mode-hook'."
        (set-fill-column 120)
        (turn-on-auto-fill)
        (electric-indent-local-mode -1)
        (turn-on-flyspell))

    (defun my/mu4e-update-main-buffer ()
        "Revert *mu4e-main* on context change so the stats refresh."
        (when (derived-mode-p 'mu4e-main-mode)
            (revert-buffer)))

    (defun my/mu4e-update-mail-and-index ()
        "Run an mu4e update.  With prefix arg, pick a single mbsync group
from `.mbsyncrc' (Gmail, Apple, or Omicron); otherwise sync all."
        (interactive)
        (mu4e-kill-update-mail)
        (let ((mu4e-get-mail-command
                  (if current-prefix-arg
                      (let ((account (completing-read
                                         "Account: "
                                         '("All" "Gmail" "Apple" "Omicron")
                                         nil t nil nil "All")))
                          (format "INSIDE_EMACS=%s mbsync %s"
                              emacs-version
                              (if (equal account "All") "-a" account)))
                      mu4e-get-mail-command)))
            (mu4e-update-mail-and-index nil)))

    (defun my/mu4e-action-view-in-firefox (msg)
        "View MSG in Firefox."
        (let ((browse-url-browser-function #'browse-url-firefox))
            (mu4e-action-view-in-browser msg)))

    (defun my/mu4e-link-description (msg)
        "Generate an org-mode link description for MSG."
        (let ((subject (or (plist-get msg :subject) "No subject"))
                 (date    (or (format-time-string mu4e-headers-date-format
                                  (mu4e-msg-field msg :date))
                              "No date"))
                 (to-from (mu4e~headers-from-or-to msg)))
            (format "%s: %s (%s)" to-from subject date)))

    (defun my/mu4e-make-context (c-name maildir mail &optional sent-action extra-shortcuts)
        "Build an `mu4e' context with the standard /<MAILDIR>/{Inbox,Sent,Trash,
Archive,Drafts,Spam} layout.  C-NAME is the human label, MAIL is
`user-mail-address', SENT-ACTION overrides `mu4e-sent-messages-behavior',
EXTRA-SHORTCUTS is appended to `mu4e-maildir-shortcuts'."
        (let* ((inbox  (concat "/" maildir "/Inbox"))
                  (sent   (concat "/" maildir "/Sent"))
                  (trash  (concat "/" maildir "/Trash"))
                  (refile (concat "/" maildir "/Archive"))
                  (draft  (concat "/" maildir "/Drafts"))
                  (spam   (concat "/" maildir "/Spam"))
                  (shortcuts (append
                                 (list (cons inbox  ?i)
                                     (cons sent   ?s)
                                     (cons trash  ?t)
                                     (cons refile ?a)
                                     (cons draft  ?d)
                                     (cons spam   ?g))
                                 extra-shortcuts)))
            (make-mu4e-context
                :name c-name
                :match-func
                (lambda (msg)
                    (when msg
                        (string-match-p
                            mail
                            (or (plist-get (car (mu4e-message-field msg :to)) :email) ""))))
                :vars `((user-mail-address           . ,mail)
                           (mu4e-sent-folder            . ,sent)
                           (mu4e-drafts-folder          . ,draft)
                           (mu4e-trash-folder           . ,trash)
                           (mu4e-refile-folder          . ,refile)
                           (mu4e-compose-format-flowed  . t)
                           (mu4e-sent-messages-behavior . ,(or sent-action 'sent))
                           (mu4e-maildir-shortcuts      . ,shortcuts)))))

    :custom
    ;; --- sending via msmtp -----------------------------------------------------
    (send-mail-function 'message-send-mail-with-sendmail)
    (message-send-mail-function 'message-send-mail-with-sendmail)
    (sendmail-program "msmtp")
    (message-sendmail-extra-arguments '("--read-envelope-from"))
    (message-sendmail-f-is-evil t)
    ;; --- mu4e core -------------------------------------------------------------
    (mail-user-agent 'mu4e-user-agent)
    (mu4e-mu-binary (or (executable-find "mu") "/usr/local/bin/mu"))
    (mu4e-modeline-show-global nil)
    (mu4e-update-interval 600)
    (mu4e-hide-index-messages t)
    (mu4e-view-show-images t)
    (mu4e-use-fancy-chars t)
    (mu4e-confirm-quit nil)
    (mu4e-main-hide-personal-addresses t)
    (mu4e-main-buffer-name "*mu4e-main*")
    (mu4e-attachment-dir (expand-file-name "~/Downloads"))
    (mu4e-change-filenames-when-moving t)
    (mu4e-completing-read-function 'completing-read)
    ;; --- context selection -----------------------------------------------------
    (mu4e-context-policy 'pick-first)
    (mu4e-compose-context-policy 'ask)
    ;; --- headers view ----------------------------------------------------------
    (mu4e-headers-auto-update t)
    (mu4e-headers-date-format "%d/%m/%Y %H:%M")
    (mu4e-headers-include-related nil)
    (mu4e-headers-skip-duplicates t)
    (mu4e-headers-fields
        '((:human-date . 18) (:flags . 6) (:short-folder . 22) (:from-or-to . 26)
             (:mailing-list . 10) (:tags . 10) (:subject . 92)))
    ;; --- compose ---------------------------------------------------------------
    (mu4e-compose-format-flowed t)
    (mu4e-compose-signature-auto-include nil)
    (mu4e-sent-messages-behavior 'sent)
    ;; --- index -----------------------------------------------------------------
    (mu4e-index-cleanup t)
    (mu4e-index-lazy-check nil)
    ;; --- fetching --------------------------------------------------------------
    (mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -a" emacs-version))
    ;; --- message-mode ----------------------------------------------------------
    (message-signature nil)
    (message-citation-line-function 'message-insert-formatted-citation-line)
    (message-citation-line-format "%N [%Y-%m-%d %a %H:%M] wrote:\n")
    (message-kill-buffer-on-exit t)

    :config
    (require 'mu4e-contrib) ;; provides `mu4e-action-git-apply-mbox'

    ;; --- contexts (Google / Apple / Omicron) -----------------------------------
    (setq mu4e-contexts
        (list
            (my/mu4e-make-context "Google"
                "jvillasantegomez@gmail.com"
                "jvillasantegomez@gmail.com"
                'delete)
            (my/mu4e-make-context "Apple"
                "julio.villasante@icloud.com"
                "julio.villasante@icloud.com")
            (my/mu4e-make-context "Omicron"
                "julio.villasante@omicronmedia.com"
                "julio.villasante@omicronmedia.com"
                nil
                '(("/julio.villasante@omicronmedia.com/Nightly" . ?n)))))

    ;; --- provider-aware trash mark ---------------------------------------------
    ;; Non-Gmail providers move-to-Trash on the server already; setting IMAP +T
    ;; on top causes "half-deleted" messages on Apple/Outlook.  Gmail still needs
    ;; +T so the move actually takes effect server-side.
    (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
             :prompt "dtrash"
             :dyn-target (lambda (_target msg)
                             (let* ((md (plist-get msg :maildir))
                                       (_  (string-match "/\\(.*\\)/.*" md))
                                       (md (match-string 1 md)))
                                 (format "/%s/Trash" md)))
             :action (lambda (docid msg target)
                         (let ((maildir (mu4e-message-field msg :maildir)))
                             (if (string-match-p "Gmail\\|Google" maildir)
                                 (mu4e--server-move docid (mu4e--mark-check-target target) "+T+S-u-N")
                                 (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))))

    ;; --- provider-aware refile (archive) mark ----------------------------------
    ;; Gmail "archive" means stripping the Inbox label, not moving to a folder;
    ;; everyone else moves to /<account>/Archive.
    (setf (alist-get 'refile mu4e-marks)
        '(:char ("r" . "▶")
             :prompt "refile"
             :dyn-target (lambda (_target msg)
                             (let* ((md (plist-get msg :maildir))
                                       (_  (string-match "/\\(.*\\)/.*" md))
                                       (md (match-string 1 md)))
                                 (format "/%s/Archive" md)))
             :action (lambda (docid msg target)
                         (let ((maildir (mu4e-message-field msg :maildir)))
                             (if (string-match-p "Gmail\\|Google" maildir)
                                 (mu4e--server-remove docid)
                                 (mu4e--server-move docid (mu4e--mark-check-target target) "+S-u-N"))))))

    ;; --- bookmarks -------------------------------------------------------------
    (setq mu4e-bookmarks
        '((:name  "Unread"
              :query "flag:new AND maildir:/Inbox/"
              :key   ?u)
             (:name  "Inbox"
                 :query "maildir:/Inbox/"
                 :key   ?i)
             (:name  "Today"
                 :query "date:today..now AND NOT maildir:/Trash/ AND NOT maildir:/Spam/"
                 :key   ?t)
             (:name  "Flagged"
                 :query "flag:flagged"
                 :key   ?f)
             (:name  "Tags"
                 :query "tag://"
                 :key   ?T)
             (:name  "Trash"
                 :query "maildir:/Trash/"
                 :key   ?x
                 :hide-unread t)
             (:name  "Attachments"
                 :query "mime:application/pdf or mime:image/jpg or mime:image/png"
                 :key   ?a
                 :hide-unread t)
             (:name  "Omicron Today's Nightly Builds"
                 :query "maildir:\"/julio.villasante@omicronmedia.com/Nightly\" AND date:today..now"
                 :key   ?n)))

    ;; --- custom :short-folder header field -------------------------------------
    (add-to-list 'mu4e-header-info-custom
        '(:short-folder .
             (:name "Folder"
                 :shortname "Folder"
                 :help "Folder name with short account alias"
                 :function (lambda (msg)
                               (let* ((maildir (mu4e-message-field msg :maildir))
                                         (aliases '(("jvillasantegomez@gmail.com"        . "Gmail")
                                                       ("julio.villasante@icloud.com"       . "Apple")
                                                       ("julio.villasante@omicronmedia.com" . "Omicron"))))
                                   (if (string-match "^/\\([^/]+\\)/\\(.*\\)" maildir)
                                       (let* ((account (match-string 1 maildir))
                                                 (folder  (match-string 2 maildir))
                                                 (alias   (cdr (assoc account aliases))))
                                           (if alias
                                               (concat alias "/" folder)
                                               (concat account "/" folder)))
                                       maildir))))))

    ;; --- view actions ----------------------------------------------------------
    (add-to-list 'mu4e-view-actions
        '("View in Firefox" . my/mu4e-action-view-in-firefox) t)
    (add-to-list 'mu4e-view-actions
        '("Apply Email" . mu4e-action-git-apply-mbox) t)

    ;; --- org link description --------------------------------------------------
    (setq mu4e-org-link-desc-func #'my/mu4e-link-description)

    ;; --- keymap mods -----------------------------------------------------------
    (define-key mu4e-main-mode-map    (kbd "x")         #'bury-buffer)
    (define-key mu4e-main-mode-map    (kbd "I")         #'mu4e-update-index)
    (define-key mu4e-main-mode-map    (kbd "U")         #'my/mu4e-update-mail-and-index)
    (define-key mu4e-view-mode-map    (kbd "<tab>")     #'shr-next-link)
    (define-key mu4e-view-mode-map    (kbd "<backtab>") #'shr-previous-link)
    (define-key mu4e-headers-mode-map (kbd "M")         #'mu4e-headers-mark-all)
    (define-key mu4e-headers-mode-map (kbd "N")         #'mu4e-headers-mark-all-unread-read)

    ;; --- icalendar -------------------------------------------------------------
    (require 'mu4e-icalendar)
    (setq mu4e-icalendar-trash-after-reply nil)
    (setq mu4e-icalendar-diary-file diary-file)
    (mu4e-icalendar-setup))

;;;
;;; mu4e-alert: desktop notifications (no modeline counter)
;;;
(use-package mu4e-alert
    :after mu4e
    :config
    (mu4e-alert-enable-notifications))

;;;
;;; mu4e-column-faces: per-column colorization in headers view
;;;
(use-package mu4e-column-faces
    :after mu4e
    :config
    (mu4e-column-faces-mode))

;;;
;;; org-msg: rich-text composition on top of message-mode
;;;
(use-package org-msg
    :hook ((after-init        . my/org-msg-setup)
              (org-msg-edit-mode . my/mu4e-compose-hook))
    :preface
    (defun my/org-msg-setup ()
        "Load mu4e before enabling org-msg-mode so the integration is wired.
Mu4easy did this implicitly because `mu4easy.el' had `(require \\='mu4e)' at
the top.  Without this, `org-msg-mode' enables but the mu4e branch of its
setup is skipped (since `(featurep \\='mu4e)' is nil), and compose buffers
end up in `org-msg-edit-mode' without the org tree structure."
        (require 'mu4e)
        (org-msg-mode))

    (defun my/org-msg-select-format (alternatives)
        "Force text-only composition when invoked with a prefix argument."
        (if current-prefix-arg '(text) alternatives))

    (defun my/org-msg-strip-text-formatting (orig-fun &rest args)
        "Strip org markup from signature and greeting when composing in text mode."
        (let ((res (apply orig-fun args)))
            (when (equal (cadr args) '(text))
                (setf (alist-get 'signature res)
                    (replace-regexp-in-string "\\([\*/]\\)" "" org-msg-signature))
                (setf (alist-get 'greeting-fmt res) ""))
            res))

    :custom
    (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t tex:imagemagick")
    (org-msg-startup "hidestars indent inlineimages")
    (org-msg-default-alternatives '((new           . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text))))
    (org-msg-signature "---\nRegards,\nJulio")
    (org-msg-greeting-fmt "Hi%s,\n\n")
    (org-msg-posting-style 'top-posting)
    (org-msg-greeting-name-limit 2)
    (org-msg-convert-citation t)

    :config
    (advice-add 'org-msg-composition-parameters :around #'my/org-msg-strip-text-formatting)
    (advice-add 'org-msg-get-alternatives :filter-return #'my/org-msg-select-format))

(provide 'my-init-mail)
;;; my-init-mail.el ends here
