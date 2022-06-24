;;;  -*- lexical-binding: t; -*-

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
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

;;;###autoload
(defun +my/show-dos-eol ()
    "Show ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M ?\^M))

;;;###autoload
(defun +my/comment-box (b e)
    "Draw a box comment around the region but arrange for the region to extend to
at least the fill column. Place the point after the comment box. http://irreal.org/blog/?p=374 "
    (interactive "r")
    (let ((e (copy-marker e t)))
        (goto-char b)
        (end-of-line)
        (insert-char ?  (- fill-column (current-column)))
        (comment-box b e 1)
        (goto-char e)
        (set-marker e nil)))

;;;###autoload
(defun +my/bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings "
    (when (and
              (buffer-live-p buffer)
              (string-match "compilation" (buffer-name buffer))
              (string-match "finished" string)
              (not
                  (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "warning" nil t))))
        (run-with-timer 2 nil
            (lambda (buf)
                (bury-buffer buf)
                (delete-windows-on buf))
            buffer)))

;;;###autoload
(defun +my/current-minor-modes ()
    "Return the list of minor modes enabled in the current buffer."
    (interactive)
    (delq nil
        (mapcar (lambda (mode)
                    (if (and (boundp mode) (symbol-value mode))
                        mode))
            minor-mode-list)))

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

;;;###autoload
(defun +my/fill-or-unfill ()
    ;; https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html.
    "Like `fill-paragraph', but unfill if used twice."
    (interactive)
    (let ((fill-column
              (if (eq last-command '+my/fill-or-unfill)
                  (progn (setq this-command nil)
                      (point-max))
                  fill-column)))
        (call-interactively #'fill-paragraph)))

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
