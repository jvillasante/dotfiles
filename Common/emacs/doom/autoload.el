;;;  -*- lexical-binding: t; -*-

;;;###autoload
(defmacro setc (&rest settings)
    "A stripped-down `customize-set-variable' with the syntax of `setq'.
Like `setq', multiple variables can be set at once; SETTINGS should consist of
variable value pairs. Some variables have a custom setter (specified with
`defcustom' and :set) that is used to run code necessary for changes to take
effect (e.g. `auto-revert-interval'). If a package has already been loaded, and
the user uses `setq' to set one of these variables, the :set code will not
run (e.g. in the case of `auto-revert-interval', the timer will not be updated).
Like with `customize-set-variable', `general-setq' will use the custom :set
setter when it exists. If the package defining the variable has not yet been
loaded, the custom setter will not be known, but it will still be run upon
loading the package. Unlike `customize-set-variable', `general-setq' does not
attempt to load any dependencies for the variable and does not support giving
variables comments. It also falls back to `set' instead of `set-default', so
that like `setq' it will change the local value of a buffer-local variable
instead of the default value."
    `(progn
         ,@(cl-loop for (var val) on settings by 'cddr
               collect `(funcall (or (get ',var 'custom-set) #'set)
                            ',var ,val))))

;;;###autoload
(defun +my/switch-to-messages-buffer ()
    "Stolen from spacemacs."
    (interactive)
    (with-current-buffer (messages-buffer)
        (goto-char (point-max))
        (switch-to-buffer (current-buffer))))

;;;###autoload
(defun +my/dos2unix ()
    "Replace DOS eolns CR LF with Unix eolns CR"
    (interactive)
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t) (replace-match "")))

;;;###autoload
(defun +my/hide-dos-eol ()
    "Hide ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setc buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

;;;###autoload
(defun +my/show-dos-eol ()
    "Show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setc buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M ?\^M))

;;;###autoload
(defun +my/switch-theme (theme)
    "This interactive call is taken from `load-theme'."
    (interactive
        (list
            (intern (completing-read "Load custom theme: "
                        (mapcar 'symbol-name
                            (custom-available-themes))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))

(defun +my/save-all ()
    "Save buffers without prompt"
    (interactive)
    (let ((modified-count
              (length (cl-loop for buf in (buffer-list)
                          when (and (buffer-file-name buf) (buffer-modified-p buf))
                          collect buf))))
        (save-some-buffers t)
        (message "%d buffer(s) saved" modified-count)))

;;;###autoload
(defun +my/comment-auto-fill ()
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode 1))

;;;###autoload
(defun +my/fill-or-unfill ()
    ;; https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html.
    "Like `fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
              (if (eq last-command '+my/fill-or-unfill)
                  (progn (setc this-command nil)
                      (point-max))
                  fill-column)))
        (call-interactively #'fill-paragraph)))

;;;###autoload
(defun +my/org-fill-or-unfill ()
    ;; https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html.
    "Like `org-fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
              (if (eq last-command '+my/org-fill-or-unfill)
                  (progn (setc this-command nil)
                      (point-max))
                  fill-column)))
        (call-interactively #'org-fill-paragraph)))

;;;###autoload
(defun +my/fill-buffer ()
    (interactive)
    (save-excursion
        (save-restriction
            (widen)
            (fill-region (point-min) (point-max)))))

;;;###autoload
(defun +my/unfill-paragraph (&optional region)
    ;; https://www.emacswiki.org/emacs/UnfillParagraph.
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
             ;; This would override `fill-column' if it's an integer.
             (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

;;;###autoload
(defun +my/unfill-region (beg end)
    ;; https://www.emacswiki.org/emacs/UnfillRegion.
    "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'"
    (interactive "*r")
    (let ((fill-column (point-max)))
        (fill-region beg end)))

;;;###autoload
(defun +my/unfill-buffer ()
    ;; https://www.emacswiki.org/emacs/UndoFilling.
    "Undo filling for all paragraphs."
    (interactive)
    (goto-char (point-min))
    (let ((fill-column 99999))
        (fill-paragraph nil)
        (while (< (point) (point-max))
            (forward-paragraph)
            (fill-paragraph nil))))

;;;###autoload
(defun +my/toggle-window-split ()
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

;;;###autoload
(defun +my/new-scratch-buffer-in-markdown ()
    "Make a temporary buffer in markdown-mode and switch to it"
    (interactive)
    (switch-to-buffer (make-temp-name "scratch-"))
    (markdown-mode))
