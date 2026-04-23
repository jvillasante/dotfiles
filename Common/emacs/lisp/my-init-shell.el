;;; my-init-shell.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; ielm : elisp shell
(use-package ielm
    :ensure nil ;; emacs built-in
    :defer t
    :hook (ielm-mode . eldoc-mode))

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
    :bind (("C-c o e" . eshell)
              ("C-c o E" . my/eshell-other-window))
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
                       (eshell/alias "c" "clear-scrollback")))
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
            (switch-to-buffer-other-window buf)))
    :bind (("C-c o s" . shell)
              ("C-c o S" . my/shell-other-window)))

;; eat: Emulate A Terminal (https://codeberg.org/akib/emacs-eat)
(use-package eat
    :disabled t
    :defer t
    :preface
    (defun my/eat-open (file)
        "Helper function to open files from eat terminal."
        (interactive)
        (if (file-exists-p file)
            (find-file-other-window file t)
            (warn "File doesn't exist")))
    :hook ((eshell-mode . eat-eshell-mode)
              (eat-exit . (lambda (&rest _) (kill-buffer-and-window))))
    :bind (("C-c o t" . eat)
              ("C-c o T" . eat-other-window)
              :map project-prefix-map
              ("t" . eat-project)
              ("T" . eat-project-other-window))
    :config
    (add-to-list 'project-switch-commands '(eat-project "Eat terminal") t)
    (add-to-list 'project-switch-commands '(eat-project-other-window "Eat terminal other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . eat-mode))
    (add-to-list 'eat-message-handler-alist (cons "open" #'my/eat-open))
    (setq eat-term-name "xterm-256color") ; https://codeberg.org/akib/emacs-eat/issues/119"
    (setq eat-kill-buffer-on-exit t)
    (setq eat-term-scrollback-size 500000)
    (setq eat-shell-prompt-annotation-failure-margin-indicator "")
    (setq eat-shell-prompt-annotation-running-margin-indicator "")
    (setq eat-shell-prompt-annotation-success-margin-indicator ""))

;; ghostel : Emacs terminal emulator powered by libghostty-vt
(use-package ghostel
    :disabled t
    :defer t
    :pin melpa
    :preface
    (defun my/ghostel-other-window ()
        "Open a `ghostel' terminal in another window."
        (interactive)
        (display-buffer-override-next-command
            (lambda (buffer alist)
                (cons (display-buffer-pop-up-window buffer alist) 'window)))
        (ghostel))
    (defun my/ghostel-project-other-window ()
        "Open a `ghostel' terminal in the project root in another window."
        (interactive)
        (display-buffer-override-next-command
            (lambda (buffer alist)
                (cons (display-buffer-pop-up-window buffer alist) 'window)))
        (ghostel-project))
    :config
    (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
    (add-to-list 'project-switch-commands '(my/ghostel-project-other-window "Ghostel other window") t)
    :bind (("C-c o t" . ghostel)
              ("C-c o T" . my/ghostel-other-window)
              :map ghostel-copy-mode-map
              ("<return>" . ghostel-copy-mode-exit)
              ("RET"      . ghostel-copy-mode-exit)
              :map ghostel-mode-map
              ("M-[" . ghostel-copy-mode)
              :map project-prefix-map
              ("t" . ghostel-project)
              ("T" . my/ghostel-project-other-window))
    :custom
    (ghostel-module-auto-install 'download) ; TODO: Change this to compile!
    (ghostel-tramp-shells
        '(("ssh" login-shell)
             ("scp" login-shell)
             ("docker" "/bin/bash")
             ("podman" "/bin/bash"))))

;; vterm : fully-fledged terminal emulator inside GNU emacs
(use-package vterm
    :defer t
    :pin melpa
    :preface
    (defun my/vterm-copy-mode-cancel ()
        (interactive)
        (vterm-copy-mode -1))
    (defun my/vterm-project ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
                  (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
            (vterm)))
    (defun my/vterm-project-other-window ()
        (interactive)
        (defvar vterm-buffer-name)
        (let* ((default-directory (project-root (project-current t)))
                  (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
            (vterm-other-window)))
    :bind (("C-c o t" . vterm)
              ("C-c o T" . vterm-other-window)
              :map vterm-copy-mode-map
              ("<return>" . my/vterm-copy-mode-cancel)
              ("RET"      . my/vterm-copy-mode-cancel)
              :map vterm-mode-map
              ("<insert>" . ignore)
              ("C-g"      . vterm-send-escape)
              ("M-["      . vterm-copy-mode)
              ("C-q"      . vterm-send-next-key)
              :map project-prefix-map
              ("t" . my/vterm-project)
              ("T" . my/vterm-project-other-window))
    :config
    (add-to-list 'vterm-eval-cmds '("find-file-other-window" find-file-other-window))
    (add-to-list 'vterm-eval-cmds '("dired" dired))
    (add-to-list 'vterm-eval-cmds '("dired-other-window" dired-other-window))
    (add-to-list 'project-switch-commands '(my/vterm-project "vTerm") t)
    (add-to-list 'project-switch-commands '(my/vterm-project-other-window "vTerm other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . vterm-mode))
    (setq vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=On -DCMAKE_BUILD_TYPE=Release")
    (setq vterm-always-compile-module nil)
    (setq vterm-timer-delay 0.01)
    (setq vterm-copy-exclude-prompt t)
    (setq vterm-copy-mode-remove-fake-newlines t)
    (setq vterm-kill-buffer-on-exit t)
    (setq vterm-max-scrollback 500000)
    (setq vterm-shell (executable-find "bash"))
    (setq vterm-tramp-shells '(("ssh" login-shell)
                                  ("scp" login-shell)
                                  ("docker" "/bin/bash")
                                  ("podman" "/bin/bash"))))

(provide 'my-init-shell)
;;; my-init-shell.el ends here
