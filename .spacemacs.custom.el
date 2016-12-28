;; Display Visited File's Path in the Frame Title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq-default user-full-name "Julio C. Villasante"
              user-mail-address "jvillasantegomez@gmail.com"
              paradox-github-token t
              load-prefer-newer t
              fill-column 110                    ; Maximum line width
              truncate-lines t                   ; Don't fold lines
              truncate-partial-width-windows nil ; for vertically-split windows
              split-width-threshold 160          ; Split verticaly by default
              evil-cross-lines t                 ; Make horizontal movement cross lines

              ;; scroll
              scroll-margin 3

              ;; my coding style, bsd but with 2 spaces indentation (and no tab
              ;; characters, only spaces)
              c-basic-indent 2
              c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil
              highlight-tabs t

              ;; Whitespace settings
              whitespace-action '(auto-cleanup)
              whitespace-style '(indentation::space
                                 space-after-tab
                                 space-before-tab
                                 trailing
                                 lines-tail
                                 tab-mark
                                 face
                                 tabs)

              doc-view-continuous t
              helm-echo-input-in-header-line nil

              ;; tramp mode
              tramp-default-method "ssh"

              ;; LaTeX
              font-latex-fontify-script nil
              TeX-newline-function 'reindent-then-newline-and-indent)

;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(setq utf-translate-cjk-mode nil)

(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; backwards compatibility as default-buffer-file-coding-system
;; is deprecated in 23.2.
(if (boundp buffer-file-coding-system)
    (setq buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; multiterm
(setq multi-term-program "/usr/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)))

;; line spacing
(setq-default line-spacing 0.1)

;; Isearch convenience, space matches anything (non-greedy)
(setq search-whitespace-regexp ".*?")

(add-hook 'markdown-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (visual-line-mode 1)))
(add-hook 'prog-mode-hook
          (lambda ()
            (set-fill-column 110)
            (flyspell-prog-mode)))
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'makefile-mode-hook 'whitespace-mode)
(remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

;; use company everywhere
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))

;; use evil-matchit everywhere
(global-evil-matchit-mode 1)

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; use count-words instead of count-words-region as it works on buffer
;; if no region is selected
(global-set-key (kbd "M-=") 'count-words)

;; org
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
  (setq org-adapt-indentation nil))

;; magit
(setq-default git-magit-status-fullscreen t)
(setq magit-completing-read-function 'magit-builtin-completing-read
      magit-push-always-verify nil)

;; company
(add-hook 'c-mode-hook (lambda () (add-to-list 'company-backends '(company-irony-c-headers company-irony))))
(add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends '(company-irony-c-headers company-irony))))

;; deft
(setq deft-directory (concat my-dropbox-path "/Personal/notes")
      deft-default-extension "org"
      deft-extensions '("org")
      deft-recursive t
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-use-filter-string-for-filename t
      deft-file-naming-rules '((noslash . "-")
                               (nospace . "-")
                               (case-fn . downcase))
      deft-auto-save-interval 0)

;; neotree
(setq neo-theme 'ascii)
(custom-set-faces
 '(neo-banner-face ((t . (:inherit shadow :underline nil))) t)
 '(neo-header-face ((t . (:inherit shadow :underline nil))) t)
 '(neo-root-dir-face ((t . (:inherit link-visited :underline nil))) t)
 '(neo-dir-link-face ((t . (:inherit dired-directory :underline nil))) t)
 '(neo-file-link-face ((t . (:inherit default :underline nil))) t)
 '(neo-button-face ((t . (:inherit dired-directory :underline nil))) t)
 '(neo-expand-btn-face ((t . (:inherit button :underline nil))) t))

;; search engine
(setq browse-url-browser-function 'browse-url-generic
      engine/browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; mu4e
(require 'mu4e-contrib)
(require 'org-mu4e)
(require 'gnus-dired)

(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (auto-fill-mode 0)
            (visual-line-mode 1)))

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
      mu4e-headers-results-limit 250
      ;; mu4e-index-cleanup nil      ;; don't do a full cleanup check
      ;; mu4e-index-lazy-check t     ;; don't consider up-to-date dirs
      )

(setq mu4e-drafts-folder "/[Gmail].Drafts"
      mu4e-sent-folder   "/[Gmail].Sent Mail"
      mu4e-trash-folder  "/[Gmail].Trash"
      mu4e-refile-folder "/[Gmail].All Mail")
(setq mu4e-maildir-shortcuts
      '( ("/Inbox"               . ?i)
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

;; mu4e-alert
(with-eval-after-load 'org
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread"
         " AND NOT flag:trashed"
         " AND maildir:"
         "\"/Inbox\"")))

;; xwidget
(evil-set-initial-state 'xwidget-webkit-mode 'emacs)
(add-hook 'xwidget-webkit-mode-hook
          (lambda ()
            (define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
            (define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
            (define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
            (define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
            (define-key xwidget-webkit-mode-map (kbd "C-p") 'xwidget-webkit-scroll-down)
            (define-key xwidget-webkit-mode-map (kbd "C-n") 'xwidget-webkit-scroll-up)
            (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
            (define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

            ;; make xwidget default browser (not for now... maybe in the future!)
            ;; (setq browse-url-browser-function (lambda (url session)
            ;;                                     (other-window 1)
            ;;                                     (xwidget-browse-url-no-reuse url)))

            ;; adapt webkit according to window configuration change automatically
            ;; without this hook, every time you change your window configuration,
            ;; you must press 'a' to adapt webkit content to new window size
            (add-hook 'window-configuration-change-hook (lambda ()
                                                          (when (equal major-mode 'xwidget-webkit-mode)
                                                            (xwidget-webkit-adjust-size-dispatch))))

            ;; by default, xwidget reuses previous xwidget window,
            ;; thus overriding your current website, unless a prefix argument
            ;; is supplied
            ;;
            ;; This function always opens a new website in a new window
            (defun xwidget-browse-url-no-reuse (url &optional sessoin)
              (interactive (progn
                             (require 'browse-url)
                             (browse-url-interactive-arg "xwidget-webkit URL: ")))
              (xwidget-webkit-browse-url url t))))
