(defconst jv-mu4e-packages
  '(mu4e))

(defun jv-mu4e/post-init-mu4e ()
  ;; mu4e
  (require 'mu4e-contrib)
  (require 'org-mu4e)
  (require 'gnus-dired)

  ;; Call EWW to display HTML messages
  (defun jv-view-in-eww (msg)
    (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))

  ;; search for sender
  (defun jv-search-for-sender (msg)
    "Search for messages sent by the sender of the message at point."
    (mu4e-headers-search
      (concat "from:" (cdar (mu4e-message-field msg :from)))))

  ;; Show number of recipients
  (defun jv-show-number-of-recipients (msg)
    "Display the number of recipients for the message at point."
    (message "Number of recipients: %d"
      (+ (length (mu4e-message-field msg :to))
        (length (mu4e-message-field msg :cc)))))

  ;; Add some actions
  ;; (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("Eww view" . jv-view-in-eww) t)
  (add-to-list 'mu4e-view-actions '("xsearch for sender" . jv-search-for-sender) t)
  (add-to-list 'mu4e-headers-actions '("Number of recipients" . jv-show-number-of-recipients) t)

  ;; hooks
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
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

  ;; Call mu every 5 minutes to update and index Maildir
  (setq mu4e-update-interval 300)

  ;; Set format=flowed
  ;; mu4e sets up visual-line-mode and also fill (M-q) to do the right thing
  ;; each paragraph is a single long line; at sending, emacs will add the
  ;; special line continuation characters.
  ;; (setq mu4e-compose-format-flowed t)

  ;; every new email composition gets its own frame! (window)
  ;;(setq mu4e-compose-in-new-frame t)

  ;; path to our Maildir directory
  (setq mu4e-maildir "~/.Maildir/gmail")

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

  ;; rename files when moving
  ;; NEEDED FOR MBSYNC
  (setq mu4e-change-filenames-when-moving t)

  ;; Try to show images
  (setq mu4e-view-show-images t
    mu4e-show-images t
    mu4e-view-image-max-width 800)

  ;; the next are relative to `mu4e-maildir'
  ;; instead of strings, they can be functions too, see
  ;; their docstring or the chapter 'Dynamic folders'
  (setq mu4e-drafts-folder "/[Gmail].Drafts"
    mu4e-sent-folder   "/[Gmail].Sent Mail"
    mu4e-trash-folder  "/[Gmail].Trash"
    mu4e-refile-folder "/[Gmail].All Mail")

  ;; the maildirs you use frequently; access them with 'j' ('jump')
  (setq mu4e-maildir-shortcuts
    '( ("/Inbox"               . ?i)
       ("/[Gmail].Important"   . ?I)
       ("/[Gmail].Sent Mail"   . ?s)
       ("/[Gmail].Spam"        . ?p)
       ("/[Gmail].Trash"       . ?t)
       ("/[Gmail].Drafts"      . ?d)
       ("/[Gmail].Starred"     . ?S)
       ("/[Gmail].All Mail"    . ?a)))

  ;; bookmarks
  (add-to-list 'mu4e-bookmarks
    '((concat "maildir:/Inbox AND date:"
        (format-time-string "%Y%m%d" (subtract-time (current-time) (days-to-time 7))))
       "Inbox messages in the last 7 days" ?W) t)
  (add-to-list 'mu4e-bookmarks
    '("size:5M..500M" "Big messages" ?b) t)
  (add-to-list 'mu4e-bookmarks
    '("maildir:/Inbox AND flag:flagged" "Flagged Messages" ?f) t)

  ;; Some personal data
  (setq
    user-mail-address "jvillasantegomez@gmail.com"
    user-full-name  "Julio C. Villasante"
    mu4e-compose-signature
    (concat
      "Kind Regards,\n"
      "Julio C. Villasante\n"
      "--\n"
      "Sent from GNU Emacs\n"))

  ;; the headers to show in the headers list -- a pair of a field
  ;; and its width, with `nil' meaning 'unlimited'
  ;; (better only use that for the last field.
  (setq mu4e-headers-fields
    '((:human-date . 14)     ;; alternatively, use :date
       (:flags      .  6)
       (:from-or-to . 22)
       (:subject    . nil))) ;; alternatively, use :thread-subject

  ;; If you get your mail without an explicit command,
  ;; use "true" for the command (this is the default)
  ;; when I press U in the main view, or C-c C-u elsewhere,
  ;; this command is called, followed by the mu indexer
  (setq mu4e-get-mail-command "mbsync gmail") ;; or "mbsync -a"

  ;; mu4e - sending mail
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/bin/msmtp")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Trim the number of fields shown in the email view. This is customizable. See mu4e-view.el for a full list.
  (setq mu4e-view-fields '(:from :to :cc :bcc :subject :date :tags :attachments :flags :maildir))
  (setq mu4e-view-show-addresses t)

  ;; other config
  (setq
    ;; UI symbols
    mu4e-use-fancy-chars t
    mu4e-headers-attach-mark '("" . "")
    mu4e-headers-encrypted-mark '("" . "")
    mu4e-headers-flagged-mark '("+" . "⚑")
    mu4e-headers-list-mark '("" . "")
    mu4e-headers-new-mark '("" . "")
    mu4e-headers-read-mark '("" . "")
    mu4e-headers-replied-mark '("" . "↩")
    mu4e-headers-seen-mark '("" . "")
    mu4e-headers-unseen-mark '("" . "")
    mu4e-headers-unread-mark '("" . "✱")
    mu4e-headers-signed-mark '("" . "")
    mu4e-headers-trashed-mark '("T" . "T")
    mu4e-headers-from-or-to-prefix '("" . "→ ")
    mu4e-headers-default-prefix '(" " . " ─")
    mu4e-headers-duplicate-prefix '("D" . "D")
    mu4e-headers-empty-parent-prefix '("X" . " X")
    mu4e-headers-first-child-prefix '("|" . "╰─")
    mu4e-headers-has-child-prefix '("+" . "╰┬")

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
    mail-user-agent 'mu4e-user-agent
    message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
    message-citation-line-function 'message-insert-formatted-citation-line
    mu4e-headers-results-limit 250
    mu4e-completing-read-function 'completing-read
    ;; mu4e-context-policy 'pick-first
    ;; mu4e-index-cleanup nil      ;; don't do a full cleanup check
    ;; mu4e-index-lazy-check t     ;; don't consider up-to-date dirs
    )

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

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; mu4e-alert
  (with-eval-after-load 'mu4e-alert
    (setq mu4e-alert-interesting-mail-query
      (concat
        "flag:unread"
        " AND NOT flag:trashed"
        " AND maildir:"
        "\"/Inbox\""))))
