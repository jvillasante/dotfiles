;;; my-init-shell.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; ielm : elisp shell
(use-package ielm
    :ensure nil ;; emacs built-in
    :init (add-hook 'ielm-mode-hook 'eldoc-mode))

;; eshell : the emacs shell
(use-package eshell-prompt-extras)
(use-package eshell
    :ensure nil ;; emacs built-in
    :preface
    (defun my--eshell-other-window ()
        "Open a `eshell' in a new window."
        (interactive)
        (let ((buf (eshell)))
            (switch-to-buffer (other-buffer buf))
            (switch-to-buffer-other-window buf)))
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

;; shell : shell in emacs
(use-package shell
    :ensure nil ;; emacs built-in
    :preface
    (defun my--shell-other-window ()
        "Open a `shell' in a new window."
        (interactive)
        (let ((buf (shell)))
            (switch-to-buffer (other-buffer buf))
            (switch-to-buffer-other-window buf))))

;; eat: Emulate A Terminal (https://codeberg.org/akib/emacs-eat)
(use-package eat
    :disabled t
    :init
    (add-to-list 'project-switch-commands '(eat-project "Eat terminal") t)
    (add-to-list 'project-switch-commands '(eat-project-other-window "Eat terminal other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . eat-mode))
    :config
    (setq process-adaptive-read-buffering nil) ; makes EAT a lot quicker!
    (setq eat-term-name "xterm-256color") ; https://codeberg.org/akib/emacs-eat/issues/119"
    (setq eat-kill-buffer-on-exit t)
    (setq eat-shell-prompt-annotation-failure-margin-indicator "")
    (setq eat-shell-prompt-annotation-running-margin-indicator "")
    (setq eat-shell-prompt-annotation-success-margin-indicator ""))

;; vterm : fully-fledged terminal emulator inside GNU emacs
(use-package vterm
    :preface
    (defun my--vterm-project ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
               (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
               (vterm-buffer (get-buffer vterm-buffer-name)))
            (if (and vterm-buffer (not current-prefix-arg))
                    (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
                (vterm))))
    (defun my--vterm-project-other-window ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
               (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
               (vterm-buffer (get-buffer vterm-buffer-name)))
            (if (and vterm-buffer (not current-prefix-arg))
                    (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
                (vterm-other-window))))
    :init
    (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
    (setq vterm-always-compile-module t)
    (add-to-list 'project-switch-commands '(my--vterm-project "vTerm") t)
    (add-to-list 'project-switch-commands '(my--vterm-project-other-window "vTerm other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
    :config
    (setq vterm-copy-mode-remove-fake-newlines t)
    (setq vterm-kill-buffer-on-exit t)
    (setq vterm-copy-exclude-prompt t)
    (setq vterm-max-scrollback 100000)
    (setq vterm-shell (executable-find "bash"))
    (setq vterm-tramp-shells '(("ssh" "/bin/bash")
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

(provide 'my-init-shell)
;;; my-init-shell.el ends here
