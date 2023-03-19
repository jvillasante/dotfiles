;;; my-utils.el --- -*- lexical-binding: t; -*-

;;; Utils

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;;; UI

(defun my:font-set-small-mono-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 140))

(defun my:font-set-small-variable-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 140 :family "Iosevka Aile"))

(defmacro my/tab-bar-go-to-tab-macro (number)
    (let ((fun (intern (format "my/tab-bar-go-to-tab-%d" number))))
        `(defun ,fun ()
             ,(format "go to tab %d" number)
             (interactive)
             (tab-bar-select-tab ,number))))

(defun my/display-truncation-and-wrap-indicator-as-whitespace ()
    (when (not (char-table-p buffer-display-table))
        (setq buffer-display-table (make-display-table)))
    (set-display-table-slot buffer-display-table 'truncation 32)
    (set-display-table-slot buffer-display-table 'wrap 32))

;;; Completion

(defun my/company-abort ()
    (when company-candidates
        (company-abort)))

;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
(defun my/company-completion-styles (capf-fn &rest args)
    (let ((completion-styles '(basic partial-completion emacs22)))
        (apply capf-fn args)))

;;; Minibuffer

(defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
              (replace-regexp-in-string
                  "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                  crm-separator)
              (car args))
        (cdr args)))

(defun my/completion-in-region (&rest args)
    (apply (if vertico-mode
               #'consult-completion-in-region
               #'completion--in-region)
        args))

;;; VCS

(defun my/project-todos ()
    "Find `hl-todo--regex' items in project using `consult-ripgrep'"
    (interactive)
    (require 'hl-todo)
    (consult-ripgrep nil hl-todo--regexp))

;;; Elisp

(defun my/helpful-display-buffer (buf)
    "If a helpful buffer window is already opened, should use it,
don't occupy other window. Make sure it is a side window, such that
when you press q and want to close the help window), this window will
be completely removed, i.e. the window won't be displayed showing
other buffer."
    (pop-to-buffer buf `((display-buffer-reuse-mode-window display-buffer-in-side-window)
                            (window-height . 0.5)
                            (window-width . 0.5)
                            ;; if there are multiple side window
                            ;; prefer the helpful window to the relatively left position
                            (slot . ,(alist-get 'helpful my/side-window-slots)))))

(defun my/helpful-lookup-symbl-at-point ()
    (interactive)
    (helpful-symbol (symbol-at-point)))

(defun my/elisp-loop-up-symbol (beg end)
    "Look up for the symbol under point, if region is active, use
        the selected region as the symbol" (interactive "r")
    (if (use-region-p)
        (helpful-symbol (intern (buffer-substring beg end)))
        (helpful-symbol (symbol-at-point))))

(defun my/emacs-lisp-outline-level ()
    "Return outline level for comment at point.
Intended to replace `lisp-outline-level'."
    (- (match-end 1) (match-beginning 1)))

(defun my/elisp-setup ()
    (setq-local outline-regexp "[ \t]*;;;\\(;*\\**\\) [^ \t\n]"
        outline-level #'my/emacs-lisp-outline-level)
    (outline-minor-mode)
    (highlight-quoted-mode))

;; DEPRECATED Remove when 28 support is dropped.
(unless (fboundp 'lisp--local-defform-body-p)
    (fset 'lisp--local-defform-body-p #'ignore))

;;; copied from doomemacs
(defun my/lisp-indent-function (indent-point state)
    "A replacement for `lisp-indent-function'.

Indents plists more sensibly. Adapted from
https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned"
    (let ((normal-indent (current-column))
             (orig-point (point))
             ;; TODO Refactor `target' usage (ew!)
             target)
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (cond ((and (elt state 2)
                   (or (eq (char-after) ?:)
                       (not (looking-at-p "\\sw\\|\\s_"))))
                  (if (lisp--local-defform-body-p state)
                      (lisp-indent-defform state indent-point)
                      (unless (> (save-excursion (forward-line 1) (point))
                                  calculate-lisp-indent-last-sexp)
                          (goto-char calculate-lisp-indent-last-sexp)
                          (beginning-of-line)
                          (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
                      (backward-prefix-chars)
                      (current-column)))
            ((and (save-excursion
                      (goto-char indent-point)
                      (skip-syntax-forward " ")
                      (not (eq (char-after) ?:)))
                 (save-excursion
                     (goto-char orig-point)
                     (and (eq (char-after) ?:)
                         (eq (char-before) ?\()
                         (setq target (current-column)))))
                (save-excursion
                    (move-to-column target t)
                    target))
            ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
                       (method (or (function-get (intern-soft function) 'lisp-indent-function)
                                   (get (intern-soft function) 'lisp-indent-hook))))
                 (cond ((or (eq method 'defun)
                            (and (null method)
                                (> (length function) 3)
                                (string-match-p "\\`def" function)))
                           (lisp-indent-defform state indent-point))
                     ((integerp method)
                         (lisp-indent-specform method state indent-point normal-indent))
                     (method
                         (funcall method indent-point state))))))))

;;; Org

(defun my/load-org-extensions-idly ()
    "Some important variables from other org extensions are not autoloaded.
You may feel annoying if you want to use them but find a void variable.
(e.g. you want to call `org-open-at-point' on a timestamp)"
    (let ((org-packages '(org-capture org-agenda)))
        (dolist (pkg org-packages)
            (require pkg))))

(defun my/exclude-org-agenda-buffers-from-recentf (old-fn &rest args)
    "Prevent `org-agenda' buffers from polluting recentf list."
    (let ((recentf-exclude '("\\.org\\'")))
        (apply old-fn args)))

(defun my/reload-org-agenda-buffers ()
    "`org-agenda' creates incomplete `org-mode' buffers to boost its startup speed. Reload those buffers
after `org-agenda' has finalized."
    (run-with-idle-timer
        4 nil
        (lambda ()
            (dolist (buf org-agenda-new-buffers)
                (when (buffer-live-p buf)
                    (with-current-buffer buf
                        (org-mode)))))))

;;; OS

(defun my/macos-cmd-w ()
    "If there is only one tab, close emacs, otherwise close one tab"
    (interactive)
    (if (> (length (tab-bar-tabs)) 1)
        (tab-bar-close-tab)
        (kill-emacs)))

(defun my/tty-setup ()
    ;; Some terminals offer two different cursors: a "visible" static cursor and a
    ;; "very visible" blinking one. By default, Emacs uses the very visible cursor
    ;; and will switch back to it when Emacs is started or resumed. A nil
    ;; `visible-cursor' prevents this.
    (setq visible-cursor nil)

    ;; Enable the mouse in terminal Emacs
    (xterm-mouse-mode)
    ;; the following keys correspond to touchpad gestures.
    (general-define-key [mouse-4] #'scroll-down-line
        [mouse-6] #'scroll-down-line
        [mouse-5] #'scroll-up-line
        [mouse-7] #'scroll-up-line)
    (menu-bar-mode -1)
    ;; BUG: in tty while menu-bar is not displayed, however it is
    ;; still there, that is, when you try to use mouse to click the
    ;; region, they pops up something which suggests its existence.
    ;; Have to turn off it again.
    (xclip-mode 1))

;;; Apps

(defun my/open-html-with-xwidget (url new-session)
    "open local file (html) using xwidget,
prefix with C-u to open the url with a new xwidget session"
    (interactive "G\nP")
    (setq url (concat "file://"
                  (expand-file-name url)))
    (xwidget-webkit-browse-url url new-session))

(defun my/web-search-wrapper (browser-func)
    "search web keywords with browser specified by `browser-func'"
    (lambda (keyword &optional new-session)
        (interactive
            ;; copied and adapted from `browse-url-interactive-arg'
            (list
                (read-string
                    "web search: "
                    ;; short circuiting
                    ;; or/and returns the value itself (not t and nil)
                    (or (and transient-mark-mode mark-active
                            (replace-regexp-in-string
                                "[\t\r\f\n ]+" " "
                                (buffer-substring-no-properties
                                    (region-beginning) (region-end))))
                        (current-word)))))
        ;; referenced from
        ;; URL `https://github.com/manateelazycat/blink-search/blob/master/backend/search_google_suggest.py'
        ;; space is illegal in an url
        (let* ((keyword (replace-regexp-in-string " " "%20" keyword))
                  ;; url is expected to not have Chinese. Should properly encode it.
                  (url (url-encode-url (concat "https://search.brave.com/search?q=" keyword))))
            (funcall browser-func url new-session))))

(defalias #'my/web-search-browser (my/web-search-wrapper #'browse-url-browser)
    "Search web keywords by `eww'.
If region is active, use the region as keyword of initial input, otherwise use `current-word'.")

(defalias #'1my/web-search-eww (my/web-search-wrapper #'eww)
    "Search web keywords by `eww'.
If region is active, use the region as keyword of initial input, otherwise use `current-word'.")

(defalias #'my/web-search-xwidget (my/web-search-wrapper #'xwidget-webkit-browse-url)
    "Search web keywords by `xwidget-webkit-browse-url'.
If region is active, use the region as keyword of initial input, otherwise use `current-word'.")

(defun my/refresh-xwidget-after-eval-python (&rest _)
    (when (xwidget-webkit-last-session)
        (run-with-idle-timer 3 nil #'xwidget-webkit-reload)))

(define-minor-mode my/refresh-xwidget-after-eval-python-mode
    "After evaluating a python command, typically like
`python-shell-send-defun', `python-shell-send-region', refreshing the
xwidget browser. This is useful for interactive usage with web stuffs
like plotly."
    :global t

    (if my/refresh-xwidget-after-eval-python-mode
        (progn
            (advice-add #'python-shell-send-statement
                :after
                #'my/refresh-xwidget-after-eval-python)
            (advice-add #'python-shell-send-region
                :after
                #'my/refresh-xwidget-after-eval-python))
        (progn
            (advice-remove #'python-shell-send-statement
                #'my/refresh-xwidget-after-eval-python)
            (advice-remove #'python-shell-send-region
                #'my/refresh-xwidget-after-eval-python))))

(defvar my/xwidget-side-window-display
    '("\\*xwidget"
         (display-buffer-in-side-window display-buffer-reuse-window))
    "the display action used for xwidget when use it as a side window.")

(defvar my/xwidget-force-display-action
    '(display-buffer-same-window)
    "the display action used for `my/xwidget-force-display-mode'")

(defun my/switch-to-buffer-obey-display-actions (old-fun &rest args)
    (let ((switch-to-buffer-obey-display-actions t))
        (apply old-fun args)))

(defun my/xwidget-force-display (&rest args)
    (if-let ((session (xwidget-webkit-current-session)))
        (display-buffer (xwidget-buffer session)
            my/xwidget-force-display-action)))

(define-minor-mode my/xwidget-side-window-mode
    "`xwidget-webkit-browse-url' doesn't respect
`display-buffer-alist'.  This minor mode advises
`xwidget-webkit-browse-url' to make it respect such. This is helpful
for interactive plotting usage with python/R where you typically want
xwdiget to display plots at the side window."
    :global t

    (if my/xwidget-side-window-mode
        (progn
            (add-to-list 'display-buffer-alist my/xwidget-side-window-display)
            (advice-add #'xwidget-webkit-new-session :around #'my/switch-to-buffer-obey-display-actions)
            (advice-add #'xwidget-webkit-goto-url :around #'my/switch-to-buffer-obey-display-actions))
        (progn
            (setq display-buffer-alist (remove my/xwidget-side-window-display display-buffer-alist))
            (advice-remove #'xwidget-webkit-new-session #'my/switch-to-buffer-obey-display-actions)
            (advice-remove #'xwidget-webkit-goto-url #'my/switch-to-buffer-obey-display-actions))))

(define-minor-mode my/xwidget-force-display-mode
    "`xwidget-webkit-browse-url' won't display its buffer in current
frame when the xwidget session exists and no window is displaying that
session.  This minor mode advises `xwidget-webkit-browse-url' to
ensure such behavior. This is helpful for viewing web contents with
`mu4e', `notmuch', and `elfeed'"
    :global t

    (if my/xwidget-force-display-mode
        (progn
            (advice-add #'xwidget-webkit-goto-url :after #'my/xwidget-force-display))
        (progn
            (advice-remove #'xwidget-webkit-goto-url #'my/xwidget-force-display))))

(defun my:elfeed-delete-window-after-kill-buffer (&rest args)
    (delete-window (selected-window)))

(defun my:elfeed-open-entry-via-xwidget (&optional new-session)
    "if point is under a url, then open this url via `xwidget',
otherwise open the current visited elfeed entry via `xwidget'.  If
with a prefix \\[universal-argument] create a new `xwidget' session
otherwise use the existed one"
    (interactive "P")
    (if-let ((link-at-point (get-text-property (point) 'shr-url)))
        (xwidget-webkit-browse-url link-at-point new-session)
        (xwidget-webkit-browse-url
            (elfeed-entry-link elfeed-show-entry)
            new-session)))

(defun my:elfeed-open-entry-via-eww (&optional new-session)
    "if point is under a url, then open this url via `eww',
otherwise open the current visited elfeed entry via `eww'.  If
with a prefix \\[universal-argument] create a new `eww' session
otherwise use the existed one"
    (interactive "P")
    (if-let ((link-at-point (get-text-property (point) 'shr-url)))
        (eww link-at-point new-session)
        (eww
            (elfeed-entry-link elfeed-show-entry)
            new-session)))

;;; Misc

(defun my/vterm ()
    "open vterm at project root, if no root is found, open at the default-directory"
    (interactive)
    (let ((default-directory (my/project-root-or-default-dir)))
        (call-interactively #'vterm)))

(defun my/project-root-or-default-dir ()
    "If a project root is found, return it. Otherwise return `default-directory'."
    (if-let ((proj (project-current)))
        (project-root proj)
        default-directory))

(defun my/ibuffer-vc-setup ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))

(defun my/set-scratch-directory (old-fun &rest args)
    "After creating a new tab, the default buffer to be displayed is
scratch buffer whose directory is set to where emacs is initialized.
Change it to the directory of previous buffer where `tab-bar-new-tab'
is called."
    (let ((current-dir default-directory))
        (apply old-fun args)
        (setq-local default-directory current-dir)))

(defun my/switch-to-messages-buffer ()
    "Stolen from spacemacs."
    (interactive)
    (with-current-buffer (messages-buffer)
        (goto-char (point-max))
        (switch-to-buffer (current-buffer))))

(defun my/dos2unix ()
    "Replace DOS eolns CR LF with Unix eolns CR"
    (interactive)
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t) (replace-match "")))

(defun my/hide-dos-eol ()
    "Hide ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

(defun my/show-dos-eol ()
    "Show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M ?\^M))

(defun my/switch-theme (theme)
    "This interactive call is taken from `load-theme'."
    (interactive
        (list
            (intern (completing-read "Load custom theme: "
                        (mapcar 'symbol-name
                            (custom-available-themes))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))

(defun my/save-all ()
    "Save buffers without prompt"
    (interactive)
    (let ((modified-count
              (length (cl-loop for buf in (buffer-list)
                          when (and (buffer-file-name buf) (buffer-modified-p buf))
                          collect buf))))
        (save-excursion
            (save-some-buffers t)
            ;; (message "%d buffer(s) saved" modified-count)
            )))

(defun my/comment-auto-fill ()
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))

(defun my/fill-or-unfill ()
    ;; https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html.
    "Like `fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
              (if (eq last-command 'my/fill-or-unfill)
                  (progn (setq this-command nil)
                      (point-max))
                  fill-column)))
        (call-interactively #'fill-paragraph)))

(defun my/org-fill-or-unfill ()
    ;; https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html.
    "Like `org-fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
              (if (eq last-command 'my/org-fill-or-unfill)
                  (progn (setq this-command nil)
                      (point-max))
                  fill-column)))
        (call-interactively #'org-fill-paragraph)))

(defun my/fill-buffer ()
    (interactive)
    (save-excursion
        (save-restriction
            (widen)
            (fill-region (point-min) (point-max)))))

(defun my/unfill-paragraph (&optional region)
    ;; https://www.emacswiki.org/emacs/UnfillParagraph.
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
             ;; This would override `fill-column' if it's an integer.
             (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

(defun my/unfill-region (beg end)
    ;; https://www.emacswiki.org/emacs/UnfillRegion.
    "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'"
    (interactive "*r")
    (let ((fill-column (point-max)))
        (fill-region beg end)))

(defun my/unfill-buffer ()
    ;; https://www.emacswiki.org/emacs/UndoFilling.
    "Undo filling for all paragraphs."
    (interactive)
    (goto-char (point-min))
    (let ((fill-column 99999))
        (fill-paragraph nil)
        (while (< (point) (point-max))
            (forward-paragraph)
            (fill-paragraph nil))))

(defun my/toggle-window-split ()
    ;; https://www.emacswiki.org/emacs/ToggleWindowSplit
    "Vertical split shows more of each line, horizontal split
 shows more lines. This code toggles between them. It only works
 for frames with exactly two windows."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
                  (next-win-buffer (window-buffer (next-window)))
                  (this-win-edges (window-edges (selected-window)))
                  (next-win-edges (window-edges (next-window)))
                  (this-win-2nd (not (and (<= (car this-win-edges)
                                              (car next-win-edges))
                                         (<= (cadr this-win-edges)
                                             (cadr next-win-edges)))))
                  (splitter
                      (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                          'split-window-horizontally
                          'split-window-vertically)))
            (delete-other-windows)
            (let ((first-win (selected-window)))
                (funcall splitter)
                (if this-win-2nd (other-window 1))
                (set-window-buffer (selected-window) this-win-buffer)
                (set-window-buffer (next-window) next-win-buffer)
                (select-window first-win)
                (if this-win-2nd (other-window 1))))))

(defun my/new-scratch-buffer-in-markdown ()
    "Make a temporary buffer in markdown-mode and switch to it"
    (interactive)
    (switch-to-buffer (make-temp-name "scratch-"))
    (markdown-mode))

(defun my/comment-or-uncomment ()
    "Comments or uncomments the current line or region."
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region
            (region-beginning)(region-end))
        (comment-or-uncomment-region
            (line-beginning-position)(line-end-position))))

(provide 'my-utils)
;;; my-utils.el ends here
