;;; my-init-shell.el -*- lexical-binding: t; -*-

;; ielm : elisp shell
(use-package ielm
    :ensure nil ;; emacs built-in
    :init
    (add-hook 'ielm-mode-hook 'eldoc-mode))

;; eshell : the emacs shell
(use-package eshell-prompt-extras)
(use-package eshell
    :ensure nil ;; emacs built-in
    :init
    ;; Prompt
    (with-eval-after-load 'esh-opt
        (autoload 'epe-theme-lambda "eshell-prompt-extras")
        (setq! eshell-highlight-prompt nil)
        (setq! eshell-prompt-function 'epe-theme-lambda))

    (add-hook 'eshell-mode-hook
        (lambda()
            ;; visual commands
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")
            (add-to-list 'eshell-visual-commands "top")

            ;; aliases
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ls" (concat ls " --group-directories-first --color"))
                (eshell/alias "ll" (concat ls " -AlFh --group-directories-first --color")))
            (eshell/alias "ff" "find-file $1")
            (eshell/alias "e" "find-file-other-window $1")
            (eshell/alias "d" "dired $1")))
    :config
    (setq eshell-highlight-prompt nil)
    (setq eshell-scroll-to-bottom-on-input nil)
    (setq eshell-scroll-to-bottom-on-output nil)
    (setq eshell-prefer-lisp-functions nil)
    (setq eshell-error-if-no-glob t)
    (setq eshell-hist-ignoredups t)
    (setq eshell-save-history-on-exit t)
    (setq eshell-destroy-buffer-when-process-dies t))

(use-package eat
    :disabled t
    :init
    ;; For `eat-eshell-mode'.
    (add-hook 'eshell-load-hook #'eat-eshell-mode)

    ;; For `eat-eshell-visual-command-mode'.
    (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(use-package vterm
    :defer t
    :config
    (add-to-list 'vterm-tramp-shells '("ssh" "/bin/sh"))
    (setq vterm-shell "/usr/bin/bash")
    (setq vterm-max-scrollback 5000)
    (add-hook 'vterm-mode-hook
        (lambda ()
            (setq-local mode-line-format nil)
            (setq-local confirm-kill-processes nil)
            (setq-local hscroll-margin 0))))

(provide 'my-init-shell)
;;; my-init-shell.el ends here
