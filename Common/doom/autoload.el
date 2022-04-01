;;;  -*- lexical-binding: t; -*-

;;;###autoload
(defun +my/switch-to-messages-buffer ()
    "Stolen from spacemacs."
    (interactive)
    (with-current-buffer (messages-buffer)
        (goto-char (point-max))
        (switch-to-buffer (current-buffer))))

;;;###autoload
(defun +my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
        ('light (load-theme 'modus-operandi t))
        ('dark (load-theme 'modus-vivendi t))))

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
(global-set-key (kbd "C-c b b") '+my/comment-box)

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
