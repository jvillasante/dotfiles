;;; my-init-shell.el -*- lexical-binding: t; -*-

;; ielm : elisp shell
(use-package ielm
    :ensure nil ;; emacs built-in
    :init (add-hook 'ielm-mode-hook 'eldoc-mode))

;; eshell : the emacs shell
(use-package eshell-prompt-extras)
(use-package eshell
    :ensure nil ;; emacs built-in
    :init
    ;; Prompt
    (with-eval-after-load 'esh-opt
        (autoload 'epe-theme-lambda "eshell-prompt-extras")
        (setq eshell-highlight-prompt nil)
        (setq eshell-prompt-function 'epe-theme-lambda))

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
    (setq eshell-scroll-to-bottom-on-input 'this)
    (setq eshell-scroll-to-bottom-on-output nil)
    (setq eshell-prefer-lisp-functions nil)
    (setq eshell-error-if-no-glob t)
    (setq eshell-hist-ignoredups t)
    (setq eshell-save-history-on-exit t)
    (setq eshell-destroy-buffer-when-process-dies t))

;; eat: Emulate A Terminal
(use-package eat
    :preface
    (defun my/eat ()
        "open `eat' at project root, if no root is found, open at the default-directory"
        (interactive)
        (let ((default-directory (my/project-root-or-default-dir)))
            (call-interactively #'eat)))
    :init
    (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
    (add-hook 'eshell-load-hook #'eat-eshell-mode)
    (add-hook 'eat-mode-hook
        (lambda ()
            (setq-local scroll-margin 0)
            (setq-local mode-line-format nil)
            (setq-local confirm-kill-processes nil)
            (setq-local hscroll-margin 0))))

(use-package vterm
    :disabled t
    :defer t
    :preface
    (defun my/vterm ()
        "open vterm at project root, if no root is found, open at the default-directory"
        (interactive)
        (let ((default-directory (my/project-root-or-default-dir)))
            (call-interactively #'vterm)))
    :config
    (add-to-list 'vterm-tramp-shells '("ssh" "/bin/sh"))
    (setq vterm-shell "/usr/bin/bash")
    (setq vterm-max-scrollback 5000)
    (add-hook 'vterm-mode-hook
        (lambda ()
            (setq-local scroll-margin 0)
            (setq-local mode-line-format nil)
            (setq-local confirm-kill-processes nil)
            (setq-local hscroll-margin 0))))

;; dwim-shell-command : Bring command-line utilities to your Emacs workflows
;; noweb templates operate on drawn files using either the following:
;;   <<f>>   (file path)
;;   <<fne>> (file path without extension)
;;   <<e>>   (extension)
;;   <<td>>  (generate a temporary directory)
;;   <<*>>   (all files joined)
;;   <<cb>>  (clipboard)
(use-package dwim-shell-command
    :bind (([remap shell-command] . dwim-shell-command)
              :map dired-mode-map
              ([remap dired-do-async-shell-command] . dwim-shell-command)
              ([remap dired-do-shell-command] . dwim-shell-command)
              ([remap dired-smart-shell-command] . dwim-shell-command))
    :config
    (require 'dwim-shell-commands)
    (defun my/dwim-shell-command-convert-to-gif ()
        "Convert all marked videos to optimized gif(s)."
        (interactive)
        (dwim-shell-command-on-marked-files
            "Convert to gif"
            "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
            :utils "ffmpeg")))

;; emamux : Interact with tmux from Emacs.
(use-package emamux)

(use-package chatgpt-shell
    :custom
    ((chatgpt-shell-openai-key
         (lambda ()
             (string-trim (nth 4 (process-lines "pass" "show" "Logins/openai.com")))))))

(provide 'my-init-shell)
;;; my-init-shell.el ends here
