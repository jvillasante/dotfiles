;;;  -*- lexical-binding: t; -*-

;;; Config is mostly from https://kkatsuyuki.github.io/notmuch-conf/                 aadsa
;;; This is all modified from
;;; https://github.com/fuxialexander/doom-emacs-private-xfu/

;;;; Notmuch
(def-package! notmuch
  :commands (notmuch
              notmuch-tree
              notmuch-tree-mode
              notmuch-search
              notmuch-search-mode
              notmuch-hello
              notmuch-hello-mode
              notmuch-show
              notmuch-show-mode
              notmuch-message-mode)
  :init
  (map!
    (:leader
      (:prefix "o"
        (:desc "APP: Email" :nmv "e" #'=mail))))
  :config
  (setq notmuch-fcc-dirs nil
    notmuch-show-logo nil
    notmuch-message-headers-visible nil
    message-kill-buffer-on-exit t
    message-send-mail-function 'message-send-mail-with-sendmail
    notmuch-search-oldest-first nil
    send-mail-function 'sendmail-send-it
    ;; sendmail-program "/usr/local/bin/msmtp"
    notmuch-search-result-format '(("date" . "%12s ")
                                    ("count" . "%-7s ")
                                    ("authors" . "%-30s ")
                                    ("subject" . "%-100s ")
                                    ("tags" . "(%s)"))
    notmuch-tag-formats '(("unread"
                            (propertize tag 'face 'notmuch-tag-unread)))
    notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                              notmuch-hello-insert-alltags)
    notmuch-saved-searches '(
                              (:name "inbox"   :query "tag:inbox not tag:trash"                            :key "i")
                              (:name "sent"    :query "tag:sent"                                           :key "s")
                              (:name "archived":query "tag:archived"                                       :key "a")
                              (:name "drafts"  :query "tag:draft"                                          :key "d"))
    notmuch-archive-tags '("-inbox" "-unread" "+archived"))
  (set-evil-initial-state! '(notmuch-hello-mode
                              notmuch-show-mode
                              notmuch-search-mode
                              notmuch-tree-mode
                              notmuch-message-mode) 'normal)
  ;; (add-hook 'notmuch-tree-mode-hook '+mail/buffer-face-mode-notmuch)
  ;; (add-hook 'notmuch-search-mode-hook '+mail/buffer-face-mode-notmuch)
  ;; (add-hook 'notmuch-message-mode-hook '+mail/buffer-face-mode-notmuch)
  (add-hook 'notmuch-message-mode-hook (lambda () (set (make-local-variable 'company-backends) '(notmuch-company (company-ispell :with company-yasnippet)))))
  (add-hook 'notmuch-tree-mode-hook (lambda () (setq-local line-spacing nil)))
  (remove-hook 'message-mode-hook #'turn-on-auto-fill)
  (remove-hook 'notmuch-message-mode-hook #'turn-on-auto-fill)
  (push 'notmuch-tree-mode evil-snipe-disabled-modes)
  (push 'notmuch-hello-mode evil-snipe-disabled-modes)
  (push 'notmuch-search-mode evil-snipe-disabled-modes)
  (push 'notmuch-show-mode evil-snipe-disabled-modes)
  (advice-add #'notmuch-start-notmuch-sentinel :override #'+mail/notmuch-start-notmuch-sentinel)
  (advice-add #'notmuch-show :override #'+mail/notmuch-show-reuse-buffer)
  (advice-add #'notmuch-hello-insert-searches :override #'+mail/notmuch-hello-insert-searches)
  (advice-add #'notmuch-hello-insert-saved-searches :override #'+mail/notmuch-hello-insert-saved-searches)
  (advice-add #'notmuch-hello-insert-buttons :override #'+mail/notmuch-hello-insert-buttons)
  ;; (set! :popup "\\*notmuch-hello\\*" '((size . 20) (side . left)) '((quit . t) (modeline . nil)))
  (set-popup-rule! "\\*offlineimap\\*" :side 'bottom :size 0.4 :quit t)
  (push (lambda (buf) (string-match-p "^\\*notmuch" (buffer-name buf)))
    doom-real-buffer-functions)

  (map! (:after notmuch
          (:map notmuch-show-mode-map
            :nmv "o"     #'ace-link-notmuch-show
            :nmv "i"     #'+mail/open-message-with-mail-app-notmuch-show
            :nmv "I"     #'notmuch-show-view-all-mime-parts
            :nmv "q"     #'notmuch-bury-or-kill-this-buffer
            (:when (featurep! :completion ivy)
              :nmv "s"     #'counsel-notmuch)
            (:when (featurep! :completion helm)
              :nmv "s"     #'helm-notmuch)
            :nmv "t"     #'notmuch-tree-from-show-current-query
            :nmv "N"   #'notmuch-mua-new-mail
            :nmv "n"     #'notmuch-show-next-thread-show
            :nmv "r"     #'notmuch-show-reply
            :nmv "<tab>" #'notmuch-show-toggle-visibility-headers
            :nmv "R"     #'notmuch-show-reply-sender
            :nmv "p"   #'notmuch-show-previous-thread-show)
          (:map notmuch-hello-mode-map
            :nmv "o"   #'ace-link-notmuch-hello
            :nmv "t"   #'notmuch-tree
            :nmv "k"   #'widget-backward
            :nmv "n"   #'notmuch-mua-new-mail
            :nmv "N" #'notmuch-mua-new-mail
            :nmv "j"   #'widget-forward
            (:when (featurep! :completion ivy)
              :nmv "s"     #'counsel-notmuch)
            (:when (featurep! :completion helm)
              :nmv "s"     #'helm-notmuch)
            :nmv "q"   #'+mail/quit
            :nmv "r"   #'notmuch-hello-update)
          (:map notmuch-search-mode-map
            :nmv "j"   #'notmuch-search-next-thread
            :nmv "k"   #'notmuch-search-previous-thread
            :nmv "t"   #'notmuch-tree-from-search-thread
            ;; :nmv "RET"   #'notmuch-tree-from-search-thread
            :nmv "RET" #'notmuch-search-show-thread
            :nmv "N" #'notmuch-mua-new-mail
            :nmv "T"   #'notmuch-tree-from-search-current-query
            :nmv ";"   #'notmuch-search-tag
            :nmv ","   #'notmuch-jump-search
            :nmv "d"   #'+mail/notmuch-search-delete
            :nmv "a"   #'notmuch-search-archive-thread
            ;; :nmv "q"   #'notmuch
            :nmv "q"   #'+mail/quit
            :nmv "R"   #'notmuch-search-reply-to-thread-sender
            :nmv "r"   #'notmuch-search-reply-to-thread
            :nmv "go"  #'+notmuch-exec-offlineimap
            (:when (featurep! :completion ivy)
              :nmv "s"     #'counsel-notmuch)
            (:when (featurep! :completion helm)
              :nmv "s"     #'helm-notmuch)
            :nmv "x"   #'+mail/notmuch-search-spam)
          (:map notmuch-tree-mode-map
            :nmv "j"   #'notmuch-tree-next-message
            :nmv "k"   #'notmuch-tree-prev-message
            :nmv "S"   #'notmuch-search-from-tree-current-query
            (:when (featurep! :completion ivy)
              :nmv "s"     #'counsel-notmuch)
            (:when (featurep! :completion helm)
              :nmv "s"     #'helm-notmuch)
            :nmv "t"   #'notmuch-tree
            :nmv ";"   #'notmuch-tree-tag
            :nmv "RET" #'notmuch-tree-show-message
            :nmv "q"   #'notmuch-tree-quit
            :nmv "s-n" #'notmuch-mua-new-mail
            :nmv "r"   #'notmuch-search-reply-to-thread-sender
            :nmv "a"   #'notmuch-tree-archive-message-then-next
            :nmv "A"   #'notmuch-tree-archive-thread
            :nmv "i"   #'+mail/open-message-with-mail-app-notmuch-tree
            :nmv "d"   #'+mail/notmuch-tree-delete
            :nmv "x"   #'+mail/notmuch-tree-spam)
          (:map notmuch-message-mode-map
            :localleader
            :desc "Send and Exit"       :n doom-localleader-key #'notmuch-mua-send-and-exit
            :desc "Kill Message Buffer" :n "k" #'notmuch-mua-kill-buffer
            :desc "Save as Draft"       :n "s" #'message-dont-send
            :desc "Attach file"         :n "f" #'mml-attach-file))))

;;;; counsel-notmuch
(when (featurep! :completion ivy)
  (def-package! counsel-notmuch
    :commands counsel-notmuch
    :after notmuch))
;;;; helm-notmuch
(when (featurep! :completion helm)
  (def-package! helm-notmuch
    :commands helm-notmuch
    :after notmuch))
;;;; org-mime
(def-package! org-mime
  :after (org notmuch))
:config (setq org-mime-library 'mml)

;;;###autoload
(defun =mail ()
  "Activate (or switch to) `notmuch' in its workspace."
  (interactive)
  (if-let* ((buf (cl-find-if (lambda (it) (string-match-p "^\\*notmuch" (buffer-name (window-buffer it))))
                   (doom-visible-windows))))
    (select-window (get-buffer-window buf))
    (notmuch-search "tag:inbox")))
;; (call-interactively 'notmuch-hello-sidebar)


;;;###autoload
(defun +mail/quit ()
  (interactive)
  ;; (+popup/close (get-buffer-window "*notmuch-hello*"))
  (doom-kill-matching-buffers "^\\*notmuch"))

;;;###autoload
(defun +mail/buffer-face-mode-notmuch-show ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Iosevka" :height 1.2))
  (buffer-face-mode)
  (setq-local line-spacing 0.5))

;;;###autoload
(defun +mail/buffer-face-mode-notmuch ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "SF Mono" :height 1.0))
  (buffer-face-mode)
  (setq-local line-spacing 0.2))



;;;###autoload
(defun +mail/notmuch-search-delete ()
  (interactive)
  (notmuch-search-add-tag
    (list "+trash" "-inbox" "-unread"))
  (notmuch-search-next-thread))

;;;###autoload
(defun +mail/notmuch-tree-delete ()
  (interactive)
  (notmuch-tree-add-tag
    (list "+trash" "-inbox" "-unread"))
  (notmuch-tree-next-message))

;;;###autoload
(defun +mail/notmuch-search-spam ()
  (interactive)
  (notmuch-search-add-tag
    (list "+spam" "-inbox" "-unread"))
  (notmuch-search-next-thread))

;;;###autoload
(defun +mail/notmuch-tree-spam ()
  (interactive)
  (notmuch-tree-add-tag
    (list "+spam" "-inbox" "-unread"))
  (notmuch-tree-next-message))

;;;###autoload
(defun +mail/open-message-with-mail-app-notmuch-tree ()
  (interactive)
  (let* ((msg-path (car (plist-get (notmuch-tree-get-message-properties) :filename)))
          (temp (make-temp-file "notmuch-message-" nil ".eml")))
    (shell-command-to-string (format "cp '%s' '%s'" msg-path temp))
    (start-process-shell-command "email" nil (format "thunderbird '%s'" temp))))

;;;###autoload
(defun +mail/open-message-with-mail-app-notmuch-show ()
  (interactive)
  (let* ((msg-path (car (plist-get (notmuch-show-get-message-properties) :filename)))
          (temp (make-temp-file "notmuch-message-" nil ".eml")))
    (shell-command-to-string (format "cp '%s' '%s'" msg-path temp))
    (start-process-shell-command "email" nil (format "thunderbird '%s'" temp))))




;; Override

;;;###autoload
(defun +mail/notmuch-start-notmuch-sentinel (proc event)
  "Process sentinel function used by `notmuch-start-notmuch'."
  (let* ((err-file (process-get proc 'err-file))
          (err-buffer (or (process-get proc 'err-buffer)
                        (find-file-noselect err-file)))
          (err (when (not (zerop (buffer-size err-buffer)))
                 (with-current-buffer err-buffer (buffer-string))))
          (sub-sentinel (process-get proc 'sub-sentinel))
          (real-command (process-get proc 'real-command)))
    (condition-case err
      (progn
        ;; Invoke the sub-sentinel, if any
        (when sub-sentinel
          (funcall sub-sentinel proc event))
        ;; Check the exit status.  This will signal an error if the
        ;; exit status is non-zero.  Don't do this if the process
        ;; buffer is dead since that means Emacs killed the process
        ;; and there's no point in telling the user that (but we
        ;; still check for and report stderr output below).
        (when (buffer-live-p (process-buffer proc))
          (notmuch-check-async-exit-status proc event real-command err))
        ;; If that didn't signal an error, then any error output was
        ;; really warning output.  Show warnings, if any.
        (let ((warnings
                (when err
                  (with-current-buffer err-buffer
                    (goto-char (point-min))
                    (end-of-line)
                    ;; Show first line; stuff remaining lines in the
                    ;; errors buffer.
                    (let ((l1 (buffer-substring (point-min) (point))))
                      (skip-chars-forward "\n")
                      (cons l1 (unless (eobp)
                                 (buffer-substring (point) (point-max)))))))))
          (when warnings
            (notmuch-logged-error (car warnings) (cdr warnings)))))
      (error
        ;; Emacs behaves strangely if an error escapes from a sentinel,
        ;; so turn errors into messages.
        (message "%s" (error-message-string err))))
    (when err-buffer
      (set-process-query-on-exit-flag (get-buffer-process err-buffer) nil)
      (kill-buffer err-buffer))
    (when err-file (ignore-errors (delete-file err-file)))))

;;;###autoload
(defun +mail/notmuch-show-reuse-buffer (thread-id &optional elide-toggle parent-buffer query-context buffer-name)
  "Run \"notmuch show\" with the given thread ID and display results.
ELIDE-TOGGLE, if non-nil, inverts the default elide behavior.
The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).
The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.
The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used.
Returns the buffer containing the messages, or NIL if no messages
matched."
  (interactive "sNotmuch show: \nP")
  (let ((buffer-name (generate-new-buffer-name
                       (or (concat "*notmuch-" buffer-name "*")
                         (concat "*notmuch-" thread-id "*"))))
         ;; We override mm-inline-override-types to stop application/*
         ;; parts from being displayed unless the user has customized
         ;; it themselves.
         (mm-inline-override-types
           (if (equal mm-inline-override-types
                 (eval (car (get 'mm-inline-override-types 'standard-value))))
             (cons "application/*" mm-inline-override-types)
             mm-inline-override-types)))
    (switch-to-buffer (get-buffer-create buffer-name))
    ;; No need to track undo information for this buffer.
    (setq buffer-undo-list t)

    (notmuch-show-mode)

    ;; Set various buffer local variables to their appropriate initial
    ;; state. Do this after enabling `notmuch-show-mode' so that they
    ;; aren't wiped out.
    (setq notmuch-show-thread-id thread-id
      notmuch-show-parent-buffer parent-buffer
      notmuch-show-query-context (if (or (string= query-context "")
                                       (string= query-context "*"))
                                   nil query-context)

      notmuch-show-process-crypto notmuch-crypto-process-mime
      ;; If `elide-toggle', invert the default value.
      notmuch-show-elide-non-matching-messages
      (if elide-toggle
        (not notmuch-show-only-matching-messages)
        notmuch-show-only-matching-messages))

    (add-hook 'post-command-hook #'notmuch-show-command-hook nil t)
    (jit-lock-register #'notmuch-show-buttonise-links)
    (call-interactively #'+mail/buffer-face-mode-notmuch-show)
    ;; (let ((fill-column 120))
    ;;   (visual-fill-column-mode))
    (notmuch-tag-clear-cache)

    (let ((inhibit-read-only t))
      (if (notmuch-show--build-buffer)
        ;; Messages were inserted into the buffer.
        (current-buffer)

        ;; No messages were inserted - presumably none matched the
        ;; query.
        (kill-buffer (current-buffer))
        (ding)
        (message "No messages matched the query!")
        nil))))

;;;###autoload
(defun +mail/notmuch-hello-insert-searches (title query-list &rest options)
  "Insert a section with TITLE showing a list of buttons made from QUERY-LIST.
QUERY-LIST should ideally be a plist but for backwards
compatibility other forms are also accepted (see
`notmuch-saved-searches' for details).  The plist should
contain keys :name and :query; if :count-query is also present
then it specifies an alternate query to be used to generate the
count for the associated search.
Supports the following entries in OPTIONS as a plist:
:initially-hidden - if non-nil, section will be hidden on startup
:show-empty-searches - show buttons with no matching messages
:hide-if-empty - hide if no buttons would be shown
   (only makes sense without :show-empty-searches)
:filter - This can be a function that takes the search query as its argument and
   returns a filter to be used in conjuction with the query for that search or nil
   to hide the element. This can also be a string that is used as a combined with
   each query using \"and\".
:filter-count - Separate filter to generate the count displayed each search. Accepts
   the same values as :filter. If :filter and :filter-count are specified, this
   will be used instead of :filter, not in conjunction with it."
  (widget-insert (propertize title 'face 'org-agenda-structure))
  (if (and notmuch-hello-first-run (plist-get options :initially-hidden))
    (add-to-list 'notmuch-hello-hidden-sections title))
  (let ((is-hidden (member title notmuch-hello-hidden-sections))
         (widget-push-button-prefix "")
         (widget-push-button-suffix "")
         (start (point)))
    (if is-hidden
      (widget-create 'push-button
        :notify `(lambda (widget &rest ignore)
                   (setq notmuch-hello-hidden-sections
                     (delete ,title notmuch-hello-hidden-sections))
                   (notmuch-hello-update))
        (propertize " +" 'face 'org-agenda-structure))
      (widget-create 'push-button
        :notify `(lambda (widget &rest ignore)
                   (add-to-list 'notmuch-hello-hidden-sections
                     ,title)
                   (notmuch-hello-update))
        " -"))
    (widget-insert "\n")
    (when (not is-hidden)
      (let ((searches (apply 'notmuch-hello-query-counts query-list options)))
        (when (or (not (plist-get options :hide-if-empty))
                searches)
          (widget-insert "\n")
          (notmuch-hello-insert-buttons searches)
          (indent-rigidly start (point) notmuch-hello-indent))))))

;;;###autoload
(defun +mail/notmuch-hello-insert-saved-searches ()
  "Insert the saved-searches section."
  (let ((searches (notmuch-hello-query-counts
                    (if notmuch-saved-search-sort-function
                      (funcall notmuch-saved-search-sort-function
                        notmuch-saved-searches)
                      notmuch-saved-searches)
                    :show-empty-searches notmuch-show-empty-saved-searches)))
    (when searches
      (widget-insert (propertize "Notmuch" 'face 'org-agenda-date-today))
      (widget-insert "\n\n")
      (widget-insert (propertize "Saved searches" 'face 'org-agenda-structure))
      (widget-insert "\n\n")
      (let ((start (point)))
        (notmuch-hello-insert-buttons searches)
        (indent-rigidly start (point) notmuch-hello-indent)))))

;;;###autoload
(defun +mail/notmuch-hello-insert-buttons (searches)
  "Insert buttons for SEARCHES.
SEARCHES must be a list of plists each of which should contain at
least the properties :name NAME :query QUERY and :count COUNT,
where QUERY is the query to start when the button for the
corresponding entry is activated, and COUNT should be the number
of messages matching the query.  Such a plist can be computed
with `notmuch-hello-query-counts'."
  (let* ((widest (notmuch-hello-longest-label searches))
          (tags-and-width (notmuch-hello-tags-per-line widest))
          (tags-per-line (car tags-and-width))
          (column-width (cdr tags-and-width))
          (column-indent 0)
          (count 0)
          (reordered-list (notmuch-hello-reflect searches tags-per-line))
          ;; Hack the display of the buttons used.
          (widget-push-button-prefix "")
          (widget-push-button-suffix ""))
    ;; dme: It feels as though there should be a better way to
    ;; implement this loop than using an incrementing counter.
    (mapc (lambda (elem)
            ;; (not elem) indicates an empty slot in the matrix.
            (when elem
              (if (> column-indent 0)
                (widget-insert (make-string column-indent ? )))
              (let* ((name (plist-get elem :name))
                      (query (plist-get elem :query))
                      (oldest-first (case (plist-get elem :sort-order)
                                      (newest-first nil)
                                      (oldest-first t)
                                      (otherwise notmuch-search-oldest-first)))
                      (search-type (eq (plist-get elem :search-type) 'tree))
                      (msg-count (plist-get elem :count)))
                (widget-insert (format "\n%5s "
                                 (notmuch-hello-nice-number msg-count)))
                (widget-create 'push-button
                  :notify #'notmuch-hello-widget-search
                  :notmuch-search-terms query
                  :notmuch-search-oldest-first oldest-first
                  :notmuch-search-type search-type
                  name)
                (setq column-indent
                  (1+ (max 0 (- column-width (length name)))))))
            (setq count (1+ count))
            (when (eq (% count tags-per-line) 0)
              (setq column-indent 0)
              (widget-insert "\n")))
      reordered-list)

    ;; If the last line was not full (and hence did not include a
    ;; carriage return), insert one now.
    (unless (eq (% count tags-per-line) 0)
      (widget-insert "\n"))))


(defun +notmuch-exec-offlineimap ()
  "Execute offlineimap"
  (interactive)
  (set-process-sentinel
    (start-process-shell-command "offlineimap"
      "*offlineimap*"
      "offlineimap -o")
    '(lambda (process event)
       (notmuch-refresh-all-buffers)
       (message "Done!"))))
