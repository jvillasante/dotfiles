;;; my-init-misc.el -*- lexical-binding: t; -*-

(straight-use-package '(emacs :type built-in))
(straight-use-package '(isearch :type built-in))
(straight-use-package '(saveplace :type built-in))
(straight-use-package '(savehist :type built-in))
(straight-use-package '(recentf :type built-in))
(straight-use-package '(project :type built-in))
(straight-use-package '(whitespace :type built-in))
(straight-use-package '(eshell :type built-in))
(straight-use-package '(tramp :type built-in))
(straight-use-package 'rainbow-delimiters)
(straight-use-package 'vterm)
(straight-use-package 'ibuffer-vc)
(straight-use-package 'crux)
(straight-use-package 'persistent-scratch)
(straight-use-package 'editorconfig)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'avy)
(straight-use-package 'super-save)
(straight-use-package 'undo-tree)
(straight-use-package 'hydra)
(straight-use-package 'expand-region)
(straight-use-package 'whole-line-or-region)
(straight-use-package 'dwim-shell-command)

;; dired
(straight-use-package '(dired :type built-in))
(straight-use-package 'dired-sidebar)
(straight-use-package 'dired-rsync)
(straight-use-package 'diredfl)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'dired-git-info)

(use-package emacs
    :config
    (remove-hook 'text-mode-hook 'turn-on-auto-fill)     ;; auto-fill insert hard line breaks
    (add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; ... visual-line-mode is much better
    (add-hook 'prog-mode-hook 'my/comment-auto-fill)     ;; ... but add comment auto-fill in prog-mode
    (dolist (hook '(special-mode-hook
                       term-mode-hook
                       comint-mode-hook
                       compilation-mode-hook
                       minibuffer-setup-hook))
        (add-hook hook
            (lambda () (setq show-trailing-whitespace nil)))))

(use-package isearch
    :config
    (setq isearch-resume-in-command-history t) ; use history for isearch as well
    (setq search-whitespace-regexp ".*?") ; isearch convenience, space matches anything (non-greedy)
    (setq isearch-lax-whitespace t)
    (setq isearch-allow-motion t) ; enable Emacs28 isearch motions
    :init
    (defadvice isearch-search (after isearch-no-fail activate)
        (unless isearch-success
            (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
            (ad-activate 'isearch-search)
            (isearch-repeat (if isearch-forward 'forward))
            (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
            (ad-activate 'isearch-search))))

;; saveplace : remembers your location in a file when saving files
(use-package saveplace
    :config
    (setq save-place-file (expand-file-name "saveplace" my/savefile-dir))
    (save-place-mode +1))

;; savehist : save minibuffer history
(use-package savehist
    :config
    (setq savehist-additional-variables
        '(search-ring regexp-search-ring) ;; search entries
        savehist-save-minibuffer-history t
        savehist-autosave-interval 60 ;; save every minute
        savehist-file (expand-file-name "savehist" my/savefile-dir)) ;; keep the home clean
    (savehist-mode +1))

;; recentf : recent files
(use-package recentf
    :init
    (my/run-hook-once pre-command-hook recentf-mode)
    :config
    (setq recentf-save-file (expand-file-name "recentf" my/savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
    (push (list (expand-file-name ".emacs.chemacs2/" my/dotfiles-path)) recentf-exclude)
    (push (list (expand-file-name ".emacs.crafted/" my/dotfiles-path)) recentf-exclude)
    (push (list (expand-file-name ".emacs.d/" my/dotfiles-path)) recentf-exclude)
    (push (list (expand-file-name ".emacs.doom/" my/dotfiles-path)) recentf-exclude)
    (push "~/.mail" recentf-exclude)
    (push "\\.git" recentf-exclude)
    (push "/tmp/" recentf-exclude)
    (push "/ssh:" recentf-exclude)
    (push "~/\\.emacs\\.d/.local" recentf-exclude)
    (push "~/mail" recentf-exclude)
    (push "/var" recentf-exclude)
    (push "/etc" recentf-exclude)
    (push "/usr" recentf-exclude)
    (push "\\.?ido\\.last$" recentf-exclude)
    (push "^/nix/store/" recentf-exclude)
    (push ".+\\.mp3$" recentf-exclude))

;; project.el : default project manager
(use-package project
    :config
    (setq project-list-file (expand-file-name "projects" my/savefile-dir))
    (add-to-list 'project-switch-commands
        '(project-dired "Dired at root")))

;; eshell : the emacs shell
(use-package eshell
    :hook ((eshell-mode-hook . (lambda()
                                   (display-line-numbers-mode nil) ;; no line numbers
                                   (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                                                 "/usr/local/bin/gls"
                                                 "/bin/ls")))
                                       (eshell/alias "ls" (concat ls " --group-directories-first --color"))
                                       (eshell/alias "ll" (concat ls " -AlFh --group-directories-first --color")))
                                   (eshell/alias "ff" "find-file $1")
                                   (eshell/alias "e" "find-file-other-window $1")
                                   (eshell/alias "d" "dired $1"))))
    :config
    (setq eshell-highlight-prompt nil)
    (setq eshell-scroll-to-bottom-on-input nil)
    (setq eshell-scroll-to-bottom-on-output nil)
    (setq eshell-prefer-lisp-functions nil)
    (setq eshell-error-if-no-glob t)
    (setq eshell-hist-ignoredups t)
    (setq eshell-save-history-on-exit t)
    (setq eshell-destroy-buffer-when-process-dies t))

;; tramp : Transparent Remote Access, Multiple Protocols
(use-package tramp
    :init
    (setq tramp-verbose 2)
    (setq tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
    (setq tramp-password-prompt-regexp   ; Add verification code support.
        (concat
            "^.*"
            (regexp-opt
                '("passphrase" "Passphrase"
                     "password" "Password"
                     "Verification code")
                t)
            ".*:\0? *")))

(use-package hideshow
    :hook (prog-mode . hs-minor-mode))

(use-package elec-pair
    :init
    (my/run-hook-once after-init-hook electric-pair-mode)

    :config
    ;; more conservative on whether should also insert ) when typing
    ;; (, for example, prevent from inserting ) when point is on a
    ;; word.
    (setq electric-pair-inhibit-predicate #'electric-pair-conservative-inhibit))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(use-package vterm
    :config
    (add-to-list 'vterm-tramp-shells '("ssh" "/bin/sh"))
    (setq vterm-shell "/usr/bin/bash")
    (setq vterm-max-scrollback 5000)
    (add-to-list 'display-buffer-alist
        `("\\*vterm\\*"
             (display-buffer-in-side-window)
             (window-height . 0.4)
             (window-width .0.5)
             (slot . ,(alist-get 'vterm my/side-window-slots))))

    (general-define-key
        :keymaps 'vterm-mode-map
        "C-c <escape>" #'vterm-send-escape)

    (add-hook 'vterm-mode-hook (my/setq-locally confirm-kill-processes nil))
    (add-hook 'vterm-mode-hook (my/setq-locally hscroll-margin 0))
    (add-hook 'vterm-mode-hook (my/turn-off-mode display-line-numbers-mode)))

(use-package auto-revert
    :init
    (my/run-hook-once pre-command-hook global-auto-revert-mode))

(use-package ibuffer-vc
    :init
    (add-hook 'ibuffer-hook #'my/ibuffer-vc-setup))

;; crux : A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
    :demand t)

;; persistent-scratch : preserve scratch buffer across sessions
(use-package persistent-scratch
    :demand t
    :config
    (setq persistent-scratch-save-file (expand-file-name "persistent-scratch" my/savefile-dir))
    (persistent-scratch-setup-default)
    (persistent-scratch-autosave-mode 1))

;; editorconfig : editorconfig for Emacs
(use-package editorconfig
    :demand t
    :config
    (editorconfig-mode 1))

;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :demand t
    :config
    (when (daemonp)
        (exec-path-from-shell-initialize)))

;; avy : GNU Emacs package for jumping to visible text using a char-based decision tree
(use-package avy
    :demand t
    :config
    (setq avy-all-windows t)
    (setq avy-background t))

;; super-save : auto-saves your buffers, when certain events happen
(use-package super-save
    :demand t
    :config
    (add-to-list 'super-save-triggers 'ace-window) ;; add integration with ace-window
    (super-save-mode +1))

;; undo-tree : treat undo history as a tree
(use-package undo-tree
    :demand t
    :config
    ;; autosave the undo-tree history
    (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
    (setq undo-tree-auto-save-history t)
    (global-undo-tree-mode +1))

;; hydra : Keybindings combinations
(use-package hydra :demand t)

;; Expand Region : expand or contract selection
(use-package expand-region
    :demand t)

;; better C-w and M-w
(use-package whole-line-or-region
    :config
    (whole-line-or-region-global-mode))

;; better shell commands
(use-package dwim-shell-command
    :demand
    :bind
    (([remap shell-command] . dwim-shell-command)
        ([remap async-shell-command] . dwim-shell-command)
        :map dired-mode-map
        ([remap dired-do-async-shell-command] . dwim-shell-command)
        ([remap dired-do-shell-command] . dwim-shell-command)
        ([remap dired-smart-shell-command] . dwim-shell-command))
    :init
    (require 'dwim-shell-commands)
    :config
    (defun my/dwim-shell-command-convert-to-gif ()
        "Convert all marked videos to optimized gif(s)."
        (interactive)
        (dwim-shell-command-on-marked-files
            "Convert to gif"
            "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
            :utils "ffmpeg")))

;; Dired : built-in navigation of folders
(use-package dired
    :config
    (setq dired-ls-F-marks-symlinks t) ;; mark symlinks
    (setq dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
    (setq dired-dwim-target t) ;; makes dired guess the target directory
    (setq dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (setq dired-hide-details-hide-symlink-targets nil
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; Disable the prompt about whether I want to kill the Dired buffer for a
        ;; deleted directory. Of course I do!
        dired-clean-confirm-killing-deleted-buffers nil)

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alh"
                                     "-alhvF --group-directories-first"))

    ;; Make dired use the same buffer for viewing directory
    (if (< emacs-major-version 28)
        (progn
            (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))) ; was dired-up-directory
        (progn
            (setq dired-kill-when-opening-new-dired-buffer t)))

    ;; enable some really cool extensions like C-x C-j(dired-jump)
    (if (< emacs-major-version 28)
        (progn
            (require 'dired-x))
        nil))

(use-package diredfl
    :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-sidebar
    :init
    (add-hook 'dired-sidebar-mode-hook (my/turn-off-mode display-line-numbers-mode))
    (add-hook 'dired-sidebar-mode-hook 'my:font-set-small-mono-font)
    :config
    (setq dired-sidebar-display-alist
        `((window-width . 0.25)
             (side . ,(alist-get 'dired-sidebar my/side-window-sides))
             (slot . ,(alist-get 'dired-sidebar my/side-window-slots)))
        dired-sidebar-use-term-integration t
        dired-sidebar-resize-on-open nil
        dired-sidebar-window-fixed nil
        dired-sidebar-theme 'ascii
        dired-sidebar-use-custom-modeline nil))

(provide 'my-init-misc)
;;; my-init-misc.el ends here
