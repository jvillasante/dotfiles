(setq jvillasante-mu4e-pre-extensions
      '(
        ))

(setq jvillasante-mu4e-post-extensions
      '(
        ))

;; mu4e -  Set up some common mu4e variables
(require 'mu4e-contrib)
(require 'org-mu4e)
(require 'gnus-dired)

(setq mu4e-maildir "~/.Maildir/gmail"
      mu4e-view-show-images t
      mu4e-view-image-max-width 800
      ;; mu4e-use-fancy-chars t
      ;; mu4e-view-prefer-html t
      ;; mu4e-html2text-command 'mu4e-shr2text
      ;; mu4e-html2text-command "html2text -utf8 -nobs -width 72"
      mu4e-html2text-command "w3m -dump -cols 80 -T text/html"
      ;; mu4e-html2text-command "html2markdown --body-width=0 | sed \"s/&nbsp_place_holder;/ /g; /^$/d\""
      mu4e-headers-skip-duplicates t
      mu4e-headers-full-search t
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval 300
      mu4e-attachment-dir "~/Downloads"
      mu4e-sent-messages-behavior 'delete
      message-kill-buffer-on-exit t
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
      message-citation-line-format "On %m/%d/%Y %H:%M:%S, %f wrote:"
      message-citation-line-function 'message-insert-formatted-citation-line
      mu4e-change-filenames-when-moving t
      mu4e-headers-results-limit 250)

(setq mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder   "/[Gmail].Sent Mail"
      mu4e-trash-folder  "/[Gmail].Trash"
      mu4e-refile-folder "/[Gmail].All Mail")
(setq mu4e-maildir-shortcuts
      '( ("/Inbox"                . ?i)
         ("/[Gmail].Important"   . ?I)
         ("/[Gmail].Sent Mail"   . ?s)
         ("/[Gmail].Spam"        . ?p)
         ("/[Gmail].Trash"       . ?t)
         ("/[Gmail].Drafts"      . ?d)
         ("/[Gmail].Starred"     . ?S)
         ("/[Gmail].All Mail"    . ?a)))
(add-to-list 'mu4e-bookmarks
             '((concat "maildir:/Inbox AND date:"
                       (format-time-string "%Y%m%d" (subtract-time (current-time) (days-to-time 7))))
               "Inbox messages in the last 7 days" ?W) t)
(add-to-list 'mu4e-bookmarks
             '("size:5M..500M" "Big messages" ?b) t)

;; mu4e - something about ourselves
(setq
 user-mail-address "jvillasantegomez@gmail.com"
 user-full-name  "Julio C. Villasante"
 mu4e-compose-signature
 (concat
  "Kind Regards,\n"
  "Julio C. Villasante\n"
  "--\n"
  "Sent from GNU Emacs\n"))

(setq
 mu4e-date-format-long "%m/%d/%Y %H:%M:%S"
 mu4e-headers-date-format "%m/%d/%Y"
 mu4e-headers-time-format "%H:%M:%S")

;; Trim down the types of columns we show, to leave more room for the sender & subject.
(setq mu4e-headers-fields '((:human-date .    12)
                            (:flags      .     6)
                            (:from-or-to .    22)
                            (:subject    . nil)))

;; Trim the number of fields shown in the email view. This is customizable. See mu4e-view.el for a full list.
(setq mu4e-view-fields '(:from :to :cc :bcc :subject :date :tags :attachments :flags :maildir))
(setq mu4e-view-show-addresses t)

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

;; If you use the mu4e-shr2text, it might be useful to emulate some of the shr key bindings
(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(eval-after-load 'mu4e
  '(progn
     ;; mu4e - actions
     (defun search-for-sender (msg)
       "Search for messages sent by the sender of the message at point."
       (mu4e-headers-search
        (concat "from:" (cdar (mu4e-message-field msg :from)))))

     (defun show-number-of-recipients (msg)
       "Display the number of recipients for the message at point."
       (message "Number of recipients: %d"
                (+ (length (mu4e-message-field msg :to))
                   (length (mu4e-message-field msg :cc)))))

     (add-to-list 'mu4e-headers-actions
                  '("Number of recipients" . show-number-of-recipients) t)
     (add-to-list 'mu4e-view-actions
                  '("xsearch for sender" . search-for-sender) t)
     (add-to-list 'mu4e-view-actions
                  '("wView with XWidget" . mu4e-action-view-with-xwidget) t)
     ))

;; mu4e - sending mail
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq message-sendmail-f-is-evil 't)

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (visual-line-mode 1)))
(add-hook 'mu4e-view-mode-hook
          (lambda () (visual-line-mode 1)))

;; mu4e - gpg
;; When composing an e-mail, C-c C-e s to sign your message then C-c C-e e to encrypt.
;; When receiving a PGP encrypted e-mail: C-c C-e v to verify the signature, and C-c C-e d to decrypt.
(add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
(add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
