(defconst jv-mu4e-packages
    '(mu4e))

(defun jv-mu4e/post-init-mu4e ()
    (require 'mu4e-contrib)
    (require 'org-mu4e)
    (require 'gnus-dired)

    ;; call EWW to display HTML messages
    (defun jv/view-in-eww (msg)
        (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))

    ;; search for sender
    (defun jv/search-for-sender (msg)
        "Search for messages sent by the sender of the message at point."
        (mu4e-headers-search
            (concat "from:" (cdar (mu4e-message-field msg :from)))))

    ;; Show number of recipients
    (defun jv/show-number-of-recipients (msg)
        "Display the number of recipients for the message at point."
        (message "Number of recipients: %d"
            (+ (length (mu4e-message-field msg :to))
                (length (mu4e-message-field msg :cc)))))

    (defun jv/mu4e-message-maildir-matches (msg rx)
        (when rx
            (if (listp rx)
                ;; If rx is a list, try each one for a match
                (or (jv/mu4e-message-maildir-matches msg (car rx))
                    (jv/mu4e-message-maildir-matches msg (cdr rx)))
                ;; Not a list, check rx
                (string-match rx (mu4e-message-field msg :maildir)))))

    ;; Choose account label to feed msmtp -a option based on From header
    ;; in Message buffer; This function must be added to
    ;; message-send-mail-hook for on-the-fly change of From address before
    ;; sending message since message-send-mail-hook is processed right
    ;; before sending message.
    (defun jv/choose-msmtp-account ()
        (if (message-mail-p)
            (save-excursion
                (let*
                    ((from (save-restriction
                               (message-narrow-to-headers)
                               (message-fetch-field "from")))
                        (account
                            (cond
                                ((string-match "jvillasantegomez@gmail.com" from) "gmail")
                                ((string-match "julio.villasante@icloud.com" from) "iCloud"))))
                    (setq message-sendmail-extra-arguments (list '"-a" account))))))

    ;; Common Configs
    (setq
        ;; UI symbols
        ;; mu4e-use-fancy-chars t
        ;; mu4e-headers-attach-mark '("" . "")
        ;; mu4e-headers-encrypted-mark '("" . "")
        ;; mu4e-headers-flagged-mark '("+" . "⚑")
        ;; mu4e-headers-list-mark '("" . "")
        ;; mu4e-headers-new-mark '("" . "")
        ;; mu4e-headers-read-mark '("" . "")
        ;; mu4e-headers-replied-mark '("" . "↩")
        ;; mu4e-headers-seen-mark '("" . "")
        ;; mu4e-headers-unseen-mark '("" . "")
        ;; mu4e-headers-unread-mark '("" . "✱")
        ;; mu4e-headers-signed-mark '("" . "")
        ;; mu4e-headers-trashed-mark '("T" . "T")
        ;; mu4e-headers-from-or-to-prefix '("" . "→ ")
        ;; mu4e-headers-default-prefix '(" " . " ─")
        ;; mu4e-headers-duplicate-prefix '("D" . "D")
        ;; mu4e-headers-empty-parent-prefix '("X" . " X")
        ;; mu4e-headers-first-child-prefix '("|" . "╰─")
        ;; mu4e-headers-has-child-prefix '("+" . "╰┬")

        mail-user-agent 'mu4e-user-agent
        mu4e-mu-binary "/usr/local/bin/mu"
        mu4e-maildir "~/.Mail"
        mu4e-get-mail-command "offlineimap -o"
        mu4e-update-interval 300
        mu4e-view-show-images t
        ;; mu4e-use-fancy-chars t
        ;; mu4e-view-prefer-html t
        ;; mu4e-html2text-command 'mu4e-shr2text
        ;; mu4e-html2text-command "html2text -utf8 -nobs -width 72"
        mu4e-html2text-command "w3m -dump -cols 80 -T text/html"
        ;; mu4e-html2text-command "html2markdown --body-width=0 | sed \"s/&nbsp_place_holder;/ /g; /^$/d\""
        mu4e-headers-skip-duplicates t
        mu4e-headers-full-search t
        mu4e-attachment-dir "~/Downloads"
        mu4e-sent-messages-behavior 'delete
        mu4e-hide-index-messages t
        mu4e-compose-signature-auto-include t
        mu4e-headers-include-related t
        mu4e-confirm-quit nil
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-keep-self-cc nil
        mu4e-headers-auto-update t
        mu4e-headers-leave-behavior 'ask
        mu4e-headers-visible-lines 22
        mu4e-view-show-addresses t
        message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
        message-citation-line-function 'message-insert-formatted-citation-line
        mu4e-headers-results-limit 250
        mu4e-completing-read-function 'completing-read
        mu4e-context-policy 'pick-first
        ;; mu4e-index-cleanup nil      ;; don't do a full cleanup check
        ;; mu4e-index-lazy-check t     ;; don't consider up-to-date dirs
        )

    ;; simple compose signature
    (setq mu4e-compose-signature
        (concat
            "Kind Regards,\n"
            "Julio C. Villasante\n"
            "--\n"
            "Sent from GNU Emacs\n"))

    ;; Actions
    (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)
    (add-to-list 'mu4e-view-actions '("Eww view" . jv/view-in-eww) t)
    (add-to-list 'mu4e-view-actions '("xsearch for sender" . jv/search-for-sender) t)
    (add-to-list 'mu4e-headers-actions '("Number of recipients" . jv/show-number-of-recipients) t)

    ;; This hook correctly modifies the \Inbox and \Starred flags on email when they are marked.
    ;; Without it refiling (archiving) and flagging (starring) email won't properly result in
    ;; the corresponding gmail action.
    (add-hook 'mu4e-mark-execute-pre-hook
        (lambda (mark msg)
            (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
                ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
                ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

    ;; compose and view mode hook
    (add-hook 'mu4e-compose-mode-hook
        (lambda ()
            (flyspell-mode 1)
            (auto-fill-mode 0)
            (visual-line-mode 1)))
    (add-hook 'mu4e-view-mode-hook
        (lambda () (visual-line-mode 1)))

    ;; From Ben Maughan: Get some Org functionality in compose buffer
    (add-hook 'message-mode-hook 'turn-on-orgtbl)
    (add-hook 'message-mode-hook 'turn-on-orgstruct++)

    ;; If you use the mu4e-shr2text, it might be useful to emulate some of the shr key bindings
    (add-hook 'mu4e-view-mode-hook
        (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

    ;; mu4e - gpg
    ;; When composing an e-mail, C-c C-e s to sign your message then C-c C-e e to encrypt.
    ;; When receiving a PGP encrypted e-mail: C-c C-e v to verify the signature, and C-c C-e d to decrypt.
    (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
    (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)

    ;; date formats
    (setq
        mu4e-date-format-long "%m/%d/%Y %H:%M:%S"
        mu4e-headers-date-format "%m/%d/%Y"
        mu4e-headers-time-format "%H:%M:%S")

    ;; show full addresses in view message (instead of just names)
    ;; toggle per name with M-RET
    (setq mu4e-view-show-addresses 't)

    ;; store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil)

    ;; the headers to show in the headers list -- a pair of a field
    ;; and its width, with `nil' meaning 'unlimited'
    ;; (better only use that for the last field.
    (setq mu4e-headers-fields
        '((:human-date . 14)     ;; alternatively, use :date
             (:flags      .  6)
             (:from-or-to . 22)
             (:subject    . nil))) ;; alternatively, use :thread-subject

    ;; Trim the number of fields shown in the email view. This is customizable. See mu4e-view.el for a full list.
    (setq mu4e-view-fields '(:from :to :cc :bcc :subject :date :tags :attachments :flags :maildir))
    (setq mu4e-view-show-addresses t)

    ;; This sets up my two different context for my gmail and iCloud emails.
    (setq mu4e-contexts
        `( ,(make-mu4e-context
                :name "gmail"
                :enter-func (lambda () (mu4e-message "Switch to the Gmail context"))
                :match-func (lambda (msg)
                                (when msg
                                    (jv/mu4e-message-maildir-matches msg "^/gmail")))
                :leave-func (lambda () (mu4e-clear-caches))
                :vars '((user-mail-address     . "jvillasantegomez@gmail.com")
                           (user-full-name        . "Julio C. Villasante")
                           (mu4e-sent-folder      . "/jvillasantegomez@gmail.com/[Gmail].Sent Mail")
                           (mu4e-drafts-folder    . "/jvillasantegomez@gmail.com/[Gmail].Drafts")
                           (mu4e-trash-folder     . "/jvillasantegomez@gmail.com/[Gmail].Trash")
                           (mu4e-refile-folder    . "/jvillasantegomez@gmail.com/[Gmail].All Mail")))
             ,(make-mu4e-context
                  :name "iCloud"
                  :enter-func (lambda () (mu4e-message "Switch to the iCloud context"))
                  :match-func (lambda (msg)
                                  (when msg
                                      (jv/mu4e-message-maildir-matches msg "^/icloud")))
                  :leave-func (lambda () (mu4e-clear-caches))
                  :vars '((user-mail-address     . "julio.villasante@icloud.com")
                             (user-full-name        . "Julio C. Villasante")
                             (mu4e-sent-folder      . "/julio.villasante@icloud.com/Sent Messages")
                             (mu4e-drafts-folder    . "/julio.villasante@icloud.com/Drafts")
                             (mu4e-trash-folder     . "/julio.villasante@icloud.com/Junk")
                             (mu4e-refile-folder    . "/julio.villasante@icloud.com/Archive")))))

    ;; Configure sending mail.
    (setq
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/local/bin/msmtp"
        message-sendmail-f-is-evil 't
        user-full-name "Julio C. Villasante")

    ;; Use the correct account context when sending mail based on the from header.
    (setq message-sendmail-envelope-from 'header)
    (add-hook 'message-send-mail-hook 'jv/choose-msmtp-account)

    ;; Bookmarks for common searches that I use.
    (setq mu4e-bookmarks '(("\\\\Inbox" "Inbox" ?i)
                              ("flag:unread" "Unread messages" ?u)
                              ("date:today..now" "Today's messages" ?t)
                              ("date:7d..now" "Last 7 days" ?w)
                              ("mime:image/*" "Messages with images" ?p)
                              ("size:5M..500M" "Big messages" ?b)))
    ;; (add-to-list 'mu4e-bookmarks
    ;;     (make-mu4e-bookmark
    ;;         :name "Inbox"
    ;;         :query "\\\\Inbox"
    ;;         :key ?i))
    (add-to-list 'mu4e-bookmarks
        (make-mu4e-bookmark
            :name "All Inboxes"
            :query "maildir:/jvillasantegomez@gmail.com/INBOX OR maildir:/julio.villasante@icloud.com/INBOX"
            :key ?i))


    ;; mu4e - attachment
    (defun gnus-dired-mail-buffers ()
        "Return a list of active message buffers."
        (let (buffers)
            (save-current-buffer
                (dolist (buffer (buffer-list t))
                    (set-buffer buffer)
                    (when (and (derived-mode-p 'message-mode)
                              (null message-sent-message-via))
                        (push (buffer-name buffer) buffers))))
            (nreverse buffers)))
    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

    ;; make shr/eww readable with dark themes
    (setq shr-color-visible-luminance-min 80)

    ;; mu4e-alert
    (with-eval-after-load 'mu4e-alert
        (setq mu4e-alert-interesting-mail-query
            (concat
                "flag:unread"
                " AND NOT flag:trashed"
                " AND maildir:"
                "\"/Inbox\""))))
