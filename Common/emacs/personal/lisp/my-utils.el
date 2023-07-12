;;; my-utils.el --- -*- lexical-binding: t; -*-

;;; Utils

(defconst IS-MAC (eq system-type 'darwin))
(defconst IS-LINUX (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defmacro setq! (&rest settings)
    "A more sensible `setopt' for SETTINGS customizable variables.

This can be used as a drop-in replacement for `setq' and *should* be used
instead of `setopt'.  Unlike `setq', this triggers custom setters on variables.
Unlike `setopt', this won't needlessly pull in dependencies."
    (macroexp-progn
        (cl-loop for (var val) on settings by 'cddr
            collect `(funcall (or (get ',var 'custom-set) #'set-default-toplevel-value)
                         ',var ,val))))

(defmacro delq! (elt list &optional fetcher)
    "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
    `(setq ,list (delq ,(if fetcher
                            `(funcall ,fetcher ,elt ,list)
                            elt)
                     ,list)))

(defun my/clone-buffer-in-new-window-readonly ()
    "Clone the current buffer in a new window.

Make it readonly, and set up a keybinding (q) to close the window."
    (interactive)
    (let ((clone-buffer (clone-indirect-buffer (buffer-name) t)))
        (with-current-buffer clone-buffer
            (read-only-mode t)
            (let ((map (make-sparse-keymap)))
                (define-key map (kbd "q") (lambda ()
                                              (interactive)
                                              (kill-buffer-and-window)))
                (use-local-map map)))
        (switch-to-buffer-other-window clone-buffer)))

(defun my/file-is-root-p (name)
    "Check whether tramp su/sudo method is used for opening filepath NAME."
    ;; Adopted from https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-File-Lock-and-Backup.html
    (let ((method (file-remote-p name 'method)))
        (when (stringp method)
            (member method '("su" "sudo")))))

(defun my/file-is-not-root-p (name)
    "Check whether tramp su/sudo method is not used for opening filepath NAME."
    (not (my/file-is-root-p name)))

;;; UI

(defun my/font-installed-p (font-name)
    "Check if a font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))

(defun my/font-set-small-mono-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 145))

(defun my/font-set-small-variable-font ()
    "Set the default font to a smaller sized font for current buffer."
    (face-remap-add-relative 'default :height 145 :family "Iosevka Aile"))

(defun my/display-truncation-and-wrap-indicator-as-whitespace ()
    "Remove those ugly Emacs default truncation."
    (when (not (char-table-p buffer-display-table))
        (setq buffer-display-table (make-display-table)))
    (set-display-table-slot buffer-display-table 'truncation 32)
    (set-display-table-slot buffer-display-table 'wrap 32))

;;; VCS

(defun my/project-todos ()
    "Find `hl-todo--regex' items in project using `consult-ripgrep'."
    (interactive)
    (require 'hl-todo)
    (consult-ripgrep nil hl-todo--regexp))

;;; Elisp

(defun my/helpful-lookup-symbl-at-point ()
    (interactive)
    (helpful-symbol (symbol-at-point)))

(defun my/elisp-look-up-symbol (beg end)
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
    (outline-minor-mode +1))

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
    (menu-bar-mode -1))

;;; Apps

(defun my/switch-to-buffer-obey-display-actions (old-fun &rest args)
    (let ((switch-to-buffer-obey-display-actions t))
        (apply old-fun args)))

(defun my/elfeed-delete-window-after-kill-buffer (&rest args)
    (delete-window (selected-window)))

(defun my/elfeed-open-entry-via-eww (&optional new-session)
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
            (message "%d buffer(s) saved" modified-count))))

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

;; Behave like vi's o command
(defun open-next-line (arg)
    "Move to the next line and then opens ARG lines."
    (interactive "p")
    (end-of-line)
    (open-line arg)
    (forward-line 1)
    (when electric-indent-mode
        (indent-according-to-mode)))

;; Behave like vi's O command
(defun open-previous-line (arg)
    "Open ARG lines before the current one."
    (interactive "p")
    (beginning-of-line)
    (open-line arg)
    (when electric-indent-mode
        (indent-according-to-mode)))

(provide 'my-utils)
;;; my-utils.el ends here
