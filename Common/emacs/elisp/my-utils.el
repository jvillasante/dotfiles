;;; my-utils.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(require 'cl-lib)

;;; Constants

(defconst my/os-mac (eq system-type 'darwin))
(defconst my/os-linux (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst my/os-windows (memq system-type '(cygwin windows-nt ms-dos)))

;;; Macros

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

;;; UI

(defun my/toggle-continuation-fringe-indicator ()
    "Toggling the continuation indicator."
    (interactive)
    (setq-default
     fringe-indicator-alist
     (if (assq 'continuation fringe-indicator-alist)
             (delq (assq 'continuation fringe-indicator-alist) fringe-indicator-alist)
         (cons '(continuation right-curly-arrow left-curly-arrow) fringe-indicator-alist))))

;;; VCS

(defun my/project-todos ()
    "Find `hl-todo--regex' items in project using `consult-ripgrep'."
    (interactive)
    (defvar hl-todo--regexp)
    (when (and (featurep 'consult) (fboundp 'consult-ripgrep))
        (consult-ripgrep nil hl-todo--regexp)))

;;; Elisp

(defun my/helpful-lookup-symbl-at-point ()
    "Look up for the symbol under point."
    (interactive)
    (when (and (featurep 'helpful) (fboundp 'helpful-symbol))
        (helpful-symbol (symbol-at-point))))

(defun my/elisp-look-up-symbol (beg end)
    "Look up for the symbol under point.
If region (BEG to END) is active, use the selected region as the symbol."
    (interactive "r")
    (when (and (featurep 'helpful) (fboundp 'helpful-symbol))
        (if (use-region-p)
                (helpful-symbol (intern (buffer-substring beg end)))
            (helpful-symbol (symbol-at-point)))))

;;; OS

(defun my/server-shutdown ()
    "Save buffers, Quit, and Shutdown (kill) server."
    (interactive)
    (setq confirm-kill-emacs nil)
    (save-buffers-kill-emacs))

(defun my/macos-cmd-w ()
    "If there is only one tab, close EMACS, otherwise close one tab."
    (interactive)
    (if (> (length (tab-bar-tabs)) 1)
            (tab-bar-close-tab)
        (kill-emacs)))

;;; Browser

;; Open link in eww or browser
(defun my/open-link-at-point-or-minibuffer-with-choice ()
    "Use `consult` to select link at point.
Then choose to open it in default browser or eww.
If 'eww' is chosen, the link is opened in a window that occupies 80% of the frame height below the current one."
    (interactive)
    (let* ((url (or (thing-at-point 'url)                             ;; Check if there's a URL at point
                    ;; (consult--read (thing-at-point--list 'url))       ;; Use consult to select a URL from the buffer
                    ;; (consult-omni)                                    ;; Use consult-omni results to select a link
                    (read-string "Enter URL: "))))                    ;; Fall back to manual URL entry
        (if url
                (let ((choice (completing-read "Open link in: " '("Browser" "eww"))))
                    (cond
                     ((string-equal choice "Browser")
                      (shell-command (concat browse-url-generic-program " " (shell-quote-argument url))))
                     ((string-equal choice "eww")
                      ;; Calculate the height for the top window (20% of the frame height)
                      (let ((window (selected-window))
                            (top-window-height (floor (* 0.2 (window-total-height)))))
                          ;; Split window with 20% height on top and 80% height for eww on the bottom
                          (select-window (split-window window top-window-height))
                          (eww url)))))
            (message "No URL provided."))))

;;; Misc

;; better `keyword-quit'
(defun my/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))

;; close buffer and window
(defun my/close-buffer-and-window ()
    "Close the current buffer and the window it's in."
    (interactive)
    (let ((window (selected-window)))
        (kill-buffer)                  ;; Kill the current buffer
        (when (window-live-p window)   ;; Check if the window is still live
            (delete-window))))         ;; Delete the window if it exists

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

(defun my/project-root-or-default-dir ()
    "If a project root is found, return it. Otherwise return `default-directory'."
    (when (fboundp 'project-root)
        (if-let ((proj (project-current)))
                (project-root proj)
            default-directory)))

(defun my/switch-to-messages-buffer ()
    "Stolen from spacemacs."
    (interactive)
    (with-current-buffer (messages-buffer)
        (goto-char (point-max))
        (switch-to-buffer (current-buffer))))

(defun my/dos2unix ()
    "Replace DOS eolns CR LF with Unix eolns CR."
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
    "This interactive call is taken from `load-theme'.
Switch the current theme to THEME"
    (interactive
     (list
      (intern (completing-read "Load custom theme: "
                               (mapcar 'symbol-name
                                       (custom-available-themes))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))

(defun my/save-all ()
    "Save all buffers without prompt."
    (interactive)
    (let ((modified-count
           (length (cl-loop for buf in (buffer-list)
                            when (and (buffer-file-name buf) (buffer-modified-p buf))
                            collect buf))))
        (save-excursion
            (save-some-buffers t)
            (message "%d buffer(s) saved" modified-count))))

(defun my/fill-or-unfill ()
    "Like `fill-paragraph', but `unfill' if used twice.
Taken from: https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html."
    (interactive)
    (let ((fill-column
           (if (eq last-command 'my/fill-or-unfill)
                   (progn (setq this-command nil)
                          (point-max))
               fill-column)))
        (call-interactively #'fill-paragraph)))

(defun my/org-fill-or-unfill ()
    "Like `org-fill-paragraph', but `unfill' if used twice.
Taken from: https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html."
    (interactive)
    (when (fboundp 'org-fill-paragraph)
        (let ((fill-column
               (if (eq last-command 'my/org-fill-or-unfill)
                       (progn (setq this-command nil)
                              (point-max))
                   fill-column)))
            (call-interactively #'org-fill-paragraph))))

(defun my/fill-buffer ()
    "Fill the entire current buffer."
    (interactive)
    (save-excursion
        (save-restriction
            (widen)
            (fill-region (point-min) (point-max)))))

(defun my/unfill-paragraph (&optional region)
    "Takes a REGION and make it into a single line of text.
Taken from: https://www.emacswiki.org/emacs/UnfillParagraph."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

(defun my/unfill-region (beg end)
    "Unfill the region (BEG - END).
Joining text paragraphs into a single
logical line.  This is useful, e.g., for use with `visual-line-mode'.
Taken from: https://www.emacswiki.org/emacs/UnfillRegion."
    (interactive "*r")
    (let ((fill-column (point-max)))
        (fill-region beg end)))

(defun my/unfill-buffer ()
    "Undo filling for all paragraphs.
Taken from: https://www.emacswiki.org/emacs/UndoFilling."
    (interactive)
    (goto-char (point-min))
    (let ((fill-column 99999))
        (fill-paragraph nil)
        (while (< (point) (point-max))
            (forward-paragraph)
            (fill-paragraph nil))))

(defun my/toggle-window-split ()
    "Toggle between vertical and horizontal split.
Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows. Taken from:
https://www.emacswiki.org/emacs/ToggleWindowSplit"
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
    "Make a temporary buffer in markdown mode and switch to it."
    (interactive)
    (when (fboundp 'markdown-mode)
        (switch-to-buffer (make-temp-name "scratch-"))
        (markdown-mode)))

(defun my/comment-or-uncomment ()
    "Comments or uncomments the current line or region."
    (interactive)
    (if (region-active-p)
            (comment-or-uncomment-region
             (region-beginning)(region-end))
        (comment-or-uncomment-region
         (line-beginning-position)(line-end-position))))

;; Behave like vi's o command
(defun my/open-next-line (arg)
    "Move to the next line and then opens ARG lines."
    (interactive "p")
    (end-of-line)
    (open-line arg)
    (forward-line 1)
    (when electric-indent-mode
        (indent-according-to-mode)))

;; Behave like vi's O command
(defun my/open-previous-line (arg)
    "Open ARG lines before the current one."
    (interactive "p")
    (beginning-of-line)
    (open-line arg)
    (when electric-indent-mode
        (indent-according-to-mode)))

;; Movement

(defun my/move-character-backward ()
    "Move character backward."
    (interactive)
    (transpose-chars 1)
    (backward-char 2))

(defun my/move-character-forward ()
    "Move character forward."
    (interactive)
    (forward-char 1)
    (transpose-chars 1)
    (backward-char))

(defun my/move-word-backwards ()
    "Move word backwards."
    (interactive)
    (backward-to-word 1)
    (transpose-words 1)
    (backward-word-strictly 2))

(defun my/move-word-forward ()
    "Move word forward."
    (interactive)
    (forward-to-word 1)
    (transpose-words 1)
    (backward-word))

(defun my/move-sentence-backward ()
    "Move sentence backward."
    (interactive)
    (transpose-sentences 1)
    (backward-sentence 2))

(defun my/move-sentence-forward ()
    "Move sentence forward."
    (interactive)
    (forward-sentence 1)
    (transpose-sentences 1)
    (backward-sentence))

(defun my/move-paragraph-backward ()
    "Move paragraph up."
    (interactive)
    (transpose-paragraphs -1)
    (backward-paragraph)
    (forward-line))

(defun my/move-paragraph-forward ()
    "Move paragraph down."
    (interactive)
    (transpose-paragraphs 1)
    (backward-paragraph)
    (forward-line))

(defun my/move-sexp-backward ()
    "Move sexp backward."
    (interactive)
    (transpose-sexps 1)
    (backward-sexp 2))

(defun my/move-sexp-forward ()
    "Move sexp forward."
    (interactive)
    (forward-sexp 1)
    (transpose-sexps 1)
    (backward-sexp))

(defun my/move-line-backward ()
    "Move line up."
    (interactive)
    (transpose-lines 1)
    (forward-line -2))

(defun my/move-line-forward ()
    "Move line down."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1))

(defun my/move-line-up ()
    "Move up the current line."
    (interactive)
    (transpose-lines 1)
    (forward-line -2)
    (when electric-indent-mode
        (indent-according-to-mode)))

(defun my/move-line-down ()
    "Move down the current line."
    (interactive)
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (when electric-indent-mode
        (indent-according-to-mode)))

(provide 'my-utils)
;;; my-utils.el ends here
