(setq my-mail-pre-extensions
      '(
        ;; pre extension my-email
        ))

(setq my-mail-post-extensions
      '(
        ;; post extension my-mail
        mu4e
        ))

(defun my-mail/init-mu4e ()
  (require 'mu4e-contrib)
  (require 'org-mu4e)
  ;; sending mail -- replace USERNAME with your gmail username
  ;; also, make sure the gnutls command line utils are installed
  ;; package 'gnutls-bin' in Debian/Ubuntu
  (require 'smtpmail)

  ;; General mu4e config
  (setq mu4e-get-mail-command "offlineimap"
        mu4e-update-interval 300
        mu4e-headers-include-related t
        mu4e-view-prefer-html t
        ;;mu4e-html2text-command "html2text"
        mu4e-html2text-command 'mu4e-shr2text
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-keep-self-cc nil
        mu4e-confirm-quit nil
        mu4e-hide-index-messages t
        mu4e-headers-auto-update t
        mu4e-use-fancy-chars t
        mu4e-split-view 'horizontal
        mu4e-headers-visible-lines 20
        mu4e-headers-leave-behavior 'ask
        mu4e~main-buffer-name "*mu4e-main*")

  ;; If you use the mu4e-shr2text, it might be useful to emulate some of the shr key bindings
  (add-hook 'mu4e-view-mode-hook
            (lambda()
              ;; try to emulate some of the eww key-bindings
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))

  ;; How tall to make the headers view when viewing headers+mail as a split.
  (setq mu4e-headers-visible-lines 22)

  ;; Trim down the types of columns we show, to leave more room for the sender & subject.
  (setq mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:from-or-to . 22)
                              (:subject . 74)))

  ;; Trim the number of fields shown in the email view. This is customizable. See mu4e-view.el for a full list.
  (setq mu4e-view-fields '(:from :to :cc :subject :date :tags :attachments))
  (setq mu4e-view-show-addresses t)

  ;; Custom marks
  (setq
   mu4e-headers-new-mark            '("N" . "✉")
   mu4e-headers-empty-parent-prefix '("-" . "○")
   mu4e-headers-first-child-prefix  '("\\" . "┗━❯")
   mu4e-headers-has-child-prefix    '("+" . "┗◉")
   mu4e-headers-duplicate-prefix    '("=" . "⚌")
   mu4e-headers-default-prefix      '("|" . "┃")
   )

  ;; defaults
  (setq mu4e-maildir "~/Maildir")                  ;; top-level Maildir
  (setq mu4e-drafts-folder "/[Gmail].Drafts")      ;; unfinished messages
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")   ;; folder for sent messages
  (setq mu4e-trash-folder  "/[Gmail].Trash")       ;; trashed messages
  (setq mu4e-refile-folder "/Archive")             ;; saved messages

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '(("/Archive"             . ?a)
          ("/INBOX"               . ?i)
          ("/[Gmail].Important"   . ?!)
          ("/[Gmail].Sent Mail"   . ?s)
          ("/[Gmail].Trash"       . ?t)
          ("/[Gmail].Spam"        . ?m)
          ("/[Gmail].Drafts"      . ?d)
          ("/[Gmail].All Mail"    . ?a)))

  ;; Bookmarks
  (add-to-list 'mu4e-bookmarks
               '("size:5M..500M"       "Big messages"     ?b))

  ;; something about ourselves
  (setq
   user-mail-address "jvillasantegomez@gmail.com"
   user-full-name  "Julio C. Villasante"
   mail-user-agent 'mu4e-user-agent
   message-signature nil)

  (if (string< emacs-version "24")
      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
            smtpmail-auth-credentials
            '(("smtp.gmail.com" 587 "jvillasantegomez@gmail.com" nil))
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)

    ;; alternatively, for emacs-24 you can use:
    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)
    )

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Enable images
  (setq mu4e-view-show-images t
        mu4e-show-images t
        mu4e-view-image-max-width 200
        mu4e-view-image-max-height 200)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; date time
  (setq
   mu4e-headers-date-format "%Y-%m-%d"
   mu4e-headers-time-format "%H:%M")

  ;; Config Mesagges
  (setq message-kill-buffer-on-exit t
        message-signature-insert-empty-line t
        message-citation-line-function 'message-insert-formatted-citation-line
        message-citation-line-format "On %Y-%m-%d, %f wrote:\n" ;;message-citation-line-format "%N @ %Y-%m-%d %H:%M %Z:\n" "On %Y-%m-%d, %f wrote:" "On %Y-%m-%d %a at %H:%M %Z, %f wrote:\n"
        )

  ;; Actions
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;;Org config
  (defalias 'org-mail 'org-mu4e-compose-org-mode)
  (setq org-mu4e-convert-to-html t)
  (setq mu4e-org-contacts-file  "~/Documents/org/contacts.org")
  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . distopico:mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . distopico:mu4e-action-add-org-contact) t)
  (add-hook 'message-mode-hook (lambda () (local-set-key "\C-c\M-o" 'org-mime-htmlize)))

  ;; save attackments
  (setq mu4e-attachment-dir "~/Downloads")

  ;; Send async
  (require 'smtpmail-async) ;;async-smtpmail-send-it - smtpmail-send-it
  (setq ;;send-mail-function 'async-smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it)

  ;; queuing mode
  (setq smtpmail-queue-mail nil  ;; do not start in queueing mode
        smtpmail-queue-dir   "~/Maildir/queue/cur")

  ;; mu4e-compose-mode-hook is especially useful for editing-related settings
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (set-fill-column 72)
              (flyspell-mode)))

  ;; send confirmation
  (add-hook 'message-send-hook
            (lambda ()
              (unless (yes-or-no-p "Sure you want to send this?")
                (signal 'quit nil))))

  (evil-leader/set-key
    "om" 'mu4e)
  )
