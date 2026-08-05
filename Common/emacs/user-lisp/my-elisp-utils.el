;;; my-elisp-utils.el --- Elisp Utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;;;###autoload
(defun my/auto-compile-elisp ()
    "Automatically compile Emacs Lisp files in watched directories on save."
    (when (and (eq major-mode 'emacs-lisp-mode)
              buffer-file-name)

        (let* ((current-file (expand-file-name buffer-file-name))
                  ;; List the variables holding your directory paths.
                  (target-vars '(my/user-directory my/lisp-dir my/lisp-experiments))

                  ;; Safely get the paths only if the variables are actually defined
                  (target-dirs (delq nil (mapcar (lambda (var)
                                                     (when (boundp var)
                                                         (expand-file-name (symbol-value var))))
                                             target-vars))))

            ;; If the current file starts with any of those directory paths, compile it.
            ;; Files with `no-byte-compile: t` will be gracefully skipped by Emacs.
            (when (seq-some (lambda (dir) (string-prefix-p dir current-file)) target-dirs)
                (byte-compile-file current-file)))))

(provide 'my-elisp-utils)
;;; my-elisp-utils.el ends here
