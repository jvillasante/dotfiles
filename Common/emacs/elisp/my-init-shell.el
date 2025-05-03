;;; my-init-shell.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; ielm : elisp shell
(use-package ielm
    :ensure nil ;; emacs built-in
    :init (add-hook 'ielm-mode-hook 'eldoc-mode))

;; eshell : the emacs shell
(use-package eshell
    :ensure nil ;; emacs built-in
    :defer t
    :preface
    (defun my/eshell-other-window ()
        "Open a `eshell' in a new window."
        (interactive)
        (let ((buf (eshell)))
            (switch-to-buffer (other-buffer buf))
            (switch-to-buffer-other-window buf)))
    :hook
    (eshell-mode . (lambda ()
                       ;; visual commands
                       (add-to-list 'eshell-visual-commands "top")
                       (add-to-list 'eshell-visual-commands "htop")
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
                       (eshell/alias "f" "find-file $1")
                       (eshell/alias "fd" "find-dired $PWD ''")
                       (eshell/alias "e" "find-file-other-window $1")
                       (eshell/alias "d" "dired $1")
                       (eshell/alias "clear" "clear-scrollback")))
    :custom
    (eshell-scroll-to-bottom-on-input 'this)
    (eshell-scroll-to-bottom-on-output nil)
    (eshell-prefer-lisp-functions nil)
    (eshell-error-if-no-glob t)
    (eshell-hist-ignoredups t)
    (eshell-save-history-on-exit t)
    (eshell-destroy-buffer-when-process-dies t))

;; shell : shell in emacs
(use-package shell
    :ensure nil ;; emacs built-in
    :preface
    (defun my/shell-other-window ()
        "Open a `shell' in a new window."
        (interactive)
        (let ((buf (shell)))
            (switch-to-buffer (other-buffer buf))
            (switch-to-buffer-other-window buf))))

;; eat: Emulate A Terminal (https://codeberg.org/akib/emacs-eat)
(use-package eat
    :disabled t
    :preface
    (defun my/eat-open (file)
        "Helper function to open files from eat terminal."
        (interactive)
        (if (file-exists-p file)
                (find-file-other-window file t)
            (warn "File doesn't exist")))
    :init
    (add-to-list 'project-switch-commands '(eat-project "Eat terminal") t)
    (add-to-list 'project-switch-commands '(eat-project-other-window "Eat terminal other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . eat-mode))
    :config
    (add-to-list 'eat-message-handler-alist (cons "open" 'my/eat-open))
    (setq process-adaptive-read-buffering nil) ; makes EAT a lot quicker!
    (setq eat-term-name "xterm-256color") ; https://codeberg.org/akib/emacs-eat/issues/119"
    (setq eat-kill-buffer-on-exit t)
    (setq eat-shell-prompt-annotation-failure-margin-indicator "")
    (setq eat-shell-prompt-annotation-running-margin-indicator "")
    (setq eat-shell-prompt-annotation-success-margin-indicator ""))

;; vterm : fully-fledged terminal emulator inside GNU emacs
(use-package vterm
    :defer t
    :preface
    (defun my/vterm-copy-mode-cancel ()
        (interactive)
        (vterm-copy-mode -1))
    (defun my/vterm-project ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
               (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
               (vterm-buffer (get-buffer vterm-buffer-name)))
            (vterm)))
    (defun my/vterm-project-other-window ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
               (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
               (vterm-buffer (get-buffer vterm-buffer-name)))
            (vterm-other-window)))
    :hook ((vterm-copy-mode . (lambda ()
                                  (set-buffer-modified-p (not (buffer-modified-p)))
                                  (force-mode-line-update))))
    :config
    (add-to-list 'vterm-eval-cmds '("dired" dired))
    (add-to-list 'project-switch-commands '(my/vterm-project "vTerm") t)
    (add-to-list 'project-switch-commands '(my/vterm-project-other-window "vTerm other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
    (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")
    (setq vterm-always-compile-module t)
    (setq vterm-timer-delay 0.01)
    (setq vterm-copy-exclude-prompt t)
    (setq vterm-copy-mode-remove-fake-newlines t)
    (setq vterm-kill-buffer-on-exit t)
    (setq vterm-max-scrollback 100000)
    (setq vterm-shell (executable-find "bash"))
    (setq vterm-tramp-shells '(("ssh" "/bin/bash")
                               ("docker" "/bin/bash")
                               ("podman" "/bin/bash"))))

;; dwim-shell-command : Bring command-line utilities to your Emacs workflows
;; noweb templates operate on drawn files using either the following:
;;   <<f>>   (file path)
;;   <<fne>> (file path without extension)
;;   <<e>>   (extension)
;;   <<td>>  (generate a temporary directory)
;;   <<*>>   (all files joined)
;;   <<cb>>  (clipboard)
;;   <<n>>, <<1n>>, or <<An>> (for current iteration)
(use-package dwim-shell-command
    :init
    (require 'dwim-shell-commands)
    (defun my/dwim-shell-command-extract ()
        "Extract all marked archives (of any kind) using `atool'."
        (interactive)
        (dwim-shell-command-on-marked-files
         "Extract" "atool --extract --subdir --explain '<<f>>'"
         :utils "atool"))
    (defun my/dwim-shell-command-convert-to-gif ()
        "Convert all marked videos to optimized gif(s)."
        (interactive)
        (dwim-shell-command-on-marked-files
         "Convert to gif"
         "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
         :utils "ffmpeg")))

;; emamux : Interact with tmux from Emacs.
(use-package emamux
    :disabled t)

(provide 'my-init-shell)
;;; my-init-shell.el ends here
