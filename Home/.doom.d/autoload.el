;;;  -*- lexical-binding: t; -*-

;;;###autoload
(defun +my/better-font()
    "Changing font to better."
    (interactive)
    ;; english font
    (if (display-graphic-p)
        (progn
            (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Source Code Pro" 17)) ;; 11 13 17 19 23
            ;; chinese font
            (dolist (charset '(kana han symbol cjk-misc bopomofo))
                (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 20)))))) ;; 14 16 20 22 28

;;;###autoload
(defun +my--support-format-p()
    (and (featurep! :editor format)
        (memq major-mode '(c-mode c++-mode emacs-lisp-mode java-mode python-mode))))

;;;###autoload
(defun +my/indent-buffer()
    "Indent the currently visited buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

;;;###autoload
(defun +my/rename-this-file-and-buffer (new-name)
    "Rename both current buffer and file it's visiting to NEW_NAME"
    (interactive "sNew name: ")
    (let ((name (buffer-name))
             (filename (buffer-file-name)))
        (unless filename
            (error "Buffer '%s' is not visiting a file" name))
        (progn
            (when (file-exists-p filename)
                (rename-file filename new-name 1))
            (set-visited-file-name new-name)
            (rename-buffer new-name))))

;;;###autoload
(defun +my--replace-pairs(content pairs &optional to-direction)
    "replace pairs in `content'"
    (when (or (not to-direction) (string= to-direction "auto"))
        (setq to-direction
            (if (catch 'break
                    (mapc
                        (lambda (pair)
                            (if (string-match (aref pair 0) content)
                                (throw 'break t))) pairs)
                    nil)
                "positive"
                "negative")))
    (with-temp-buffer
        (insert content)
        (mapc
            (lambda (pair)
                (goto-char (point-min))
                (while (search-forward-regexp (elt pair 0) nil "noerrro")
                    (replace-match (elt pair 1))))
            (cond
                ((string= to-direction "positive")
                    pairs)
                ((string= to-direction "negative")
                    (mapcar
                        (lambda(pair)
                            (vector (elt pattern 1) (elt pattern 0))) pairs))
                (t
                    (user-error "Your 3rd argument %s is invalid" to-direction))))
        (buffer-string)))


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
(global-set-key (kbd "C-c b b") 'jv/comment-box)

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
                ;; (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                (delete-windows-on buf))
            buffer)))
(add-hook 'compilation-finish-functions '+my/bury-compile-buffer-if-successful)
;; (remove-hook 'compilation-finish-functions '+my/bury-compile-buffer-if-successful)

(defun +my/elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

(defun +my/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

(defun +my/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))
