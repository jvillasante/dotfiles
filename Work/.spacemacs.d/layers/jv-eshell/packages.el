(defconst jv-eshell-packages
    '(eshell))

(defun jv-eshell/post-init-eshell ()
    (setq eshell-highlight-prompt nil)
    (setq-default eshell-prompt-function #'eshell/eshell-local-prompt-function)

    (setq ;; eshell-buffer-shorthand t ...  Can't see Bug#19391
        eshell-scroll-to-bottom-on-input 'all
        eshell-error-if-no-glob t
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-prefer-lisp-functions nil
        eshell-destroy-buffer-when-process-dies t)

    ;; Visual Executables
    (add-hook 'eshell-mode-hook
        (lambda ()
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")
            (add-to-list 'eshell-visual-commands "top")))

    ;; Aliases
    (add-hook 'eshell-mode-hook
        (lambda ()
            (eshell/alias "e" "find-file $1")
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "emacs" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")

            (eshell/alias "gd" "magit-diff-unstaged")
            (eshell/alias "gds" "magit-diff-staged")
            (eshell/alias "d" "dired $1")

            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ll" (concat ls " -AlohG --color=always")))))

    )
