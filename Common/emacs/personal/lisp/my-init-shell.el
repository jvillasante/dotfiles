;;; my-init-shell.el -*- lexical-binding: t; -*-

;; ielm : elisp shell
(use-package ielm
    :ensure nil ;; emacs built-in
    :bind (("C-c o i" . #'ielm))
    :init (add-hook 'ielm-mode-hook 'eldoc-mode))

;; eshell : the emacs shell
(use-package eshell-prompt-extras)
(use-package eshell
    :ensure nil ;; emacs built-in
    :bind (("C-c o e" . #'eshell))
    :init
    (with-eval-after-load 'esh-opt
        (autoload 'epe-theme-lambda "eshell-prompt-extras")
        (setq eshell-highlight-prompt nil)
        (setq eshell-prompt-function 'epe-theme-lambda))

    (add-hook 'eshell-mode-hook
              (lambda ()
                  ;; visual commands
                  (add-to-list 'eshell-visual-commands "top")
                  (add-to-list 'eshell-visual-commands "htop")
                  (add-to-list 'eshell-visual-commands "neofetch")
                  (add-to-list 'eshell-visual-commands "ssh")
                  (add-to-list 'eshell-visual-commands "tail")
                  (add-to-list 'eshell-visual-commands "lynx")
                  (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))

                  ;; aliases
                  (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                                    "/usr/local/bin/gls"
                                "/bin/ls")))
                      (eshell/alias "ls" (concat ls " --group-directories-first --color"))
                      (eshell/alias "ll" (concat ls " -AlFh --group-directories-first --color")))
                  (eshell/alias "ff" "find-file $1")
                  (eshell/alias "e" "find-file-other-window $1")
                  (eshell/alias "d" "dired $1")

                  ;; eat
                  (when (featurep 'eat)
                      (setq eshell-visual-commands nil)
                      (eat-eshell-mode +1)
                      (eat-eshell-visual-command-mode +1))))
    :config
    (setq eshell-scroll-to-bottom-on-input 'this)
    (setq eshell-scroll-to-bottom-on-output nil)
    (setq eshell-prefer-lisp-functions nil)
    (setq eshell-error-if-no-glob t)
    (setq eshell-hist-ignoredups t)
    (setq eshell-save-history-on-exit t)
    (setq eshell-destroy-buffer-when-process-dies t))

;; eat: Emulate A Terminal (https://codeberg.org/akib/emacs-eat)
(use-package eat
    :bind (("C-c o t" . #'eat)
           ("C-c o T" . #'eat-other-window)
           :map project-prefix-map
           ("t" . #'eat-project)
           ("T" . #'eat-project-other-window))
    :init
    (add-to-list 'project-switch-commands '(eat-project "Eat terminal") t)
    (add-to-list 'project-switch-commands '(eat-project-other-window "Eat terminal other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . eat-mode))
    :config
    (setq eat-kill-buffer-on-exit t)
    (setq eat-shell-prompt-annotation-failure-margin-indicator "")
    (setq eat-shell-prompt-annotation-running-margin-indicator "")
    (setq eat-shell-prompt-annotation-success-margin-indicator ""))

;; vterm : fully-fledged terminal emulator inside GNU emacs
(use-package vterm
    :disabled t
    :preface
    (defun my--vterm ()
        "Open vterm at project root, if no root is found, open at the default-directory"
        (interactive)
        (let ((default-directory (my--project-root-or-default-dir)))
            (if (equal current-prefix-arg nil) ; no C-u
                    (call-interactively #'vterm-other-window)
                (call-interactively #'vterm))))
    (defun my--vterm-project ()
        "Open vterm at project root with name <project>-vterm"
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
               (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
               (vterm-buffer (get-buffer vterm-buffer-name)))
            (if (and vterm-buffer (not current-prefix-arg))
                    (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
                (if (equal current-prefix-arg nil) ; no C-u
                        (call-interactively #'vterm-other-window)
                    (call-interactively #'vterm)))))
    :bind (("C-c o t" . my--vterm)
           :map project-prefix-map
           ("t" . my--vterm-project)
           :map vterm-mode-map
           ([return] . #'vterm-send-return)
           ("C-q" . #'vterm-send-next-key)
           ("M-[" . #'vterm-copy-mode)
           ("C-y" . #'vterm-yank)
           ("C-g" . #'vterm-send-escape)
           :map vterm-copy-mode-map
           ("M-w" . #'vterm-copy-mode-done)
           ("C-g" . #'vterm-copy-mode-done))
    :init
    (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
    (setq vterm-always-compile-module t)
    (add-to-list 'project-switch-commands '(my--project-vterm "vTerm") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
    (add-hook 'vterm-mode-hook
              (lambda ()
                  (setq-local scroll-margin 0)
                  (setq-local confirm-kill-processes nil)
                  (setq-local hscroll-margin 0)))
    :config
    (setq vterm-copy-mode-remove-fake-newlines t)
    (setq vterm-kill-buffer-on-exit t)
    (setq vterm-copy-exclude-prompt t)
    (setq vterm-max-scrollback 100000)
    (setq vterm-shell (executable-find "bash"))
    (setq vterm-tramp-shells '(("ssh" "/bin/sh")
                               ("podman" "/bin/bash"))))

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
    :init
    (require 'dwim-shell-commands)
    (defun my--dwim-shell-command-convert-to-gif ()
        "Convert all marked videos to optimized gif(s)."
        (interactive)
        (dwim-shell-command-on-marked-files
         "Convert to gif"
         "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
         :utils "ffmpeg"))
    (defun my--dwim-shell-command-copy-to-www-sm2 ()
        "Copfy files to SM2's www directory"
        (interactive)
        (dwim-shell-command-on-marked-files
         "Copy marked files to SM2's www directory"
         "scp -Or <<f>> dmxs.sm2.lan:/opt/dmxs/www"
         :utils "scp"))
    (defun my--dwim-shell-command-copy-to-bin-sm2 ()
        "Copy files to SM2's www directory"
        (interactive)
        (dwim-shell-command-on-marked-files
         "Copy marked files to SM2's www directory"
         "scp -Or <<f>> dmxs.sm2.lan:/opt/dmxs/app/bin"
         :utils "scp")))

;; emamux : Interact with tmux from Emacs.
(use-package emamux
    :disabled t)

(use-package chatgpt-shell
    :custom
    ((chatgpt-shell-openai-key
      (lambda ()
          (string-trim (nth 4 (process-lines "pass" "show" "Logins/openai.com")))))))

(provide 'my-init-shell)
;;; my-init-shell.el ends here
