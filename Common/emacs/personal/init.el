;;; init.el --- personal init -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Package management
;;;; Optimize garbage collection : improves the startup time
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()(setq gc-cons-threshold 800000)))

;;;; Enable MELPA : Add the main user repository of packages
;; cf Getting Started https://melpa.org/
;; ELPA, the default repository, has much less available
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                   (not (gnutls-available-p))))
          (proto (if no-ssl "http" "https")))
    (when no-ssl
        (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
    ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
    (add-to-list 'package-archives (cons "melpa" (concat "http" "://melpa.org/packages/")) t)
    ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
    (when (< emacs-major-version 24)
        ;; For important compatibility libraries like cl-lib
        (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;;; use-package : Use package will be used as a package loader in this file
;; Install use-package if not installed yet
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

;; Install the package if not available yet
(use-package use-package
    :custom
    (use-package-always-ensure t) ; Download missing packages by default
    (use-package-always-defer t)) ; Lazy load by default, use :demand otherwise

;;;; Misc
(defconst +my/savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p +my/savefile-dir)
    (make-directory +my/savefile-dir)) ;; create the savefile dir if it doesn't exist

;; frame title
(setq-default frame-title-format
    '(:eval
         (format "%s@%s: %s"
             (or (file-remote-p default-directory 'user)
                 user-real-login-name)
             (or (file-remote-p default-directory 'host)
                 system-name)
             (cond
                 (buffer-file-truename
                     (concat buffer-file-truename))
                 (dired-directory
                     (concat dired-directory))
                 (t (buffer-name))))))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
    '(try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill
         try-complete-file-name-partially
         try-complete-file-name
         try-expand-all-abbrevs
         try-expand-list
         try-expand-line
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol))

(progn
    (setq user-full-name "Julio C. Villasante"
        user-mail-address "jvillasantegomez@gmail.com"
        user-login-name "jvillasante")
    (setq load-prefer-newer t) ;; Always load newest byte code
    (tool-bar-mode -1) ; Disable the toolbar in GUI mode
    (menu-bar-mode -1) ; Hide Menu bar
    (blink-cursor-mode -1) ;; the blinking cursor is nothing, but an annoyance
    (scroll-bar-mode -1) ; Disable the scroll bar in GUI mode
    (setq inhibit-startup-screen t) ; Hide the startup screen
    (setq auto-window-vscroll nil  ; fast scrolling
        fast-but-imprecise-scrolling t
        scroll-margin 2
        scroll-conservatively 101
        scroll-preserve-screen-position t)
    (when (fboundp 'pixel-scroll-precision-mode)
        (pixel-scroll-precision-mode t))
    (if (boundp 'use-short-answers) ;; Use "y" and "n" to confirm/negate prompt
        (setq use-short-answers t)
        (advice-add 'yes-or-no-p :override #'y-or-n-p))
    (setq large-file-warning-threshold 100000000) ;; warn when opening files bigger than 100MB
    (setq confirm-kill-processes nil) ;; quit Emacs directly even if there are running processes
    (savehist-mode) ; Save history for commands
    (setq isearch-resume-in-command-history t) ; Use history for isearch as well
    (setq-default auto-revert-verbose t) ; show message when file changes
    (setq-default auto-revert-avoid-polling t) ; use save signal
    (global-auto-revert-mode t) ; Refresh files automatically when modified from outside emacs
    (setq enable-local-eval t) ; Enable eval blocks in .dir-locals.el
    (setq enable-local-variables :all) ; Enable by default variables in .dir-locals.el
    (setq ring-bell-function 'ignore) ; Disable the bell for emacs
    (setq debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
    (setq completions-detailed t) ; Detailed description for the built in describe symbol etc
    (column-number-mode t) ; Display column numbers in the status line
    (global-display-line-numbers-mode t) ; Display line numbers on the left
    (line-number-mode t) ; Display line number
    (size-indication-mode t) ; Display size indication
    (delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
    (save-place-mode +1) ; Remember point in files
    (electric-pair-mode +1) ; auto-insert matching parenteses
    (show-paren-mode +1) ; Highlight the matching parenthesis
    (global-so-long-mode +1) ; long files
    (setq kill-do-not-save-duplicates t) ; Do not save duplicates in kill-ring
    (setq auto-save-default nil) ; Don't autosave files with default Emacs package (we'll use `super-save' pacakge isntead)
    (setq search-whitespace-regexp ".*?") ;; Isearch convenience, space matches anything (non-greedy)
    (setq next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
    (setq use-short-answers t) ; Abreviate Yes/No to y or n
    (setq require-final-newline t) ;; Newline at end of file
    (setq-default fill-column 135) ;; Wrap lines at 135 characters
    (setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
    (setq-default tab-width 4)            ;; but maintain correct appearance
    (setq indent-line-function 'insert-tab) ;; indent the current line
    (setq standard-indent 4)
    (setq-default c-basic-offset  4) ; Base indent size when indented automatically
    (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
    (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
    (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
    (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
    (setq make-backup-files nil) ; Do not use backup files (filename~)
    (setq create-lockfiles nil)) ; Do not use lock files (.#filename)

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; Auto-save on focus lost - https://www.emacswiki.org/emacs/AutoSave
(add-function :after after-focus-change-function
    (lambda () (unless (frame-focus-state) (save-some-buffers t))))

;; Default font
(if (daemonp)
    (add-hook 'after-make-frame-functions
        (lambda (frame)
            (with-selected-frame frame
                (progn
                    (set-face-font 'default "Iosevka 16")
                    (set-face-font 'variable-pitch "Iosevka 16")
                    (copy-face 'default 'fixed-pitch)))))
    (progn
        (set-face-font 'default "Iosevka 16")
        (set-face-font 'variable-pitch "Iosevka 22")
        (copy-face 'default 'fixed-pitch)))

;; TODO: from Steve Yegge: Bind `kill-region' to "C-x C-k" and "C-c C-k"
;; TODO: from Steve Yegge: Bind `backward-kill-word' to "C-w"
(use-package emacs
    :ensure nil  ; emacs built-in
    :hook ((text-mode-hook . (lambda() (visual-line-mode))))
    :config
    (dolist (hook '(special-mode-hook
                       term-mode-hook
                       comint-mode-hook
                       compilation-mode-hook
                       minibuffer-setup-hook))
        (add-hook hook
            (lambda () (setq show-trailing-whitespace nil)))))

;; hl-line : highlight the current line
(use-package hl-line
    :ensure nil  ; emacs built-in
    :config
    (global-hl-line-mode +1))

;; saveplace : remembers your location in a file when saving files
(use-package saveplace
    :ensure nil  ; emacs built-in
    :config
    (setq save-place-file (expand-file-name "saveplace" +my/savefile-dir))
    (save-place-mode +1))

;; savehist : save minibuffer history
(use-package savehist
    :ensure nil  ; emacs built-in
    :config
    (setq savehist-additional-variables
        '(search-ring regexp-search-ring) ;; search entries
        savehist-autosave-interval 60 ;; save every minute
        savehist-file (expand-file-name "savehist" +my/savefile-dir)) ;; keep the home clean
    (savehist-mode +1))

;; recentf : recent files
(use-package recentf
    :ensure nil  ; emacs built-in
    :config
    (setq recentf-save-file (expand-file-name "recentf" +my/savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
    (recentf-mode +1))

;;;; projectile : project interaction library for Emacs.
(use-package projectile
    :demand
    :init
    (setq projectile-project-search-path '("~/Workspace/Private/Projects/" "~/Workspace/Public/" "~/Workspace/Work/Projects")
        projectile-switch-project-action 'projectile-dired
        projectile-require-project-root t
        projectile-project-root-files-bottom-up '(".projectile" ".git")
        projectile-sort-order 'recentf
        projectile-indexing-method 'hybrid)
    :config
    (projectile-mode +1))

;;;; Dired : built-in navigation of folders
(use-package dired
    :ensure nil  ; emacs built-in
    :config
    (setq dired-ls-F-marks-symlinks t) ;; mark symlinks
    (setq dired-recursive-copies 'always) ;; Never prompt for recursive copies of a directory
    (setq dired-recursive-deletes 'always) ;; Never prompt for recursive deletes of a directory
    (setq dired-dwim-target t) ;; makes dired guess the target directory
    (setq dired-auto-revert-buffer t) ;; auto-revert dired buffers if file changed on disk
    (setq projectile-switch-project-action 'projectile-dired) ;; dired loads on project switch

    ;; Dired listing switches
    ;;  -a : Do not ignore entries starting with .
    ;;  -l : Use long listing format.
    ;;  -h : Human-readable sizes like 1K, 234M, ..
    ;;  -v : Do natural sort .. so the file names starting with . will show up first.
    ;;  -F : Classify filenames by appending '*' to executables, '/' to directories, etc.
    (setq dired-listing-switches (if (eq system-type 'windows-nt)
                                     "-alh"
                                     "-alhvF --group-directories-first"))

    (require 'dired-x)) ;; enable some really cool extensions like C-x C-j(dired-jump)

;;;; whitespace : visualize blanks (tabs, spaces, newline, etc)
(use-package whitespace
    :ensure nil  ; emacs built-in
    :init
    (add-hook 'phyton-mode-hook #'whitespace-mode)
    (add-hook 'makefile-mode-hook #'whitespace-mode)
    (add-hook 'before-save-hook #'whitespace-cleanup)
    :config
    (setq show-trailing-whitespace t)
    (setq whitespace-action '(auto-cleanup))
    (setq whitespace-line-column 135) ;; limit line length
    (setq whitespace-style '(face tabs empty trailing lines-tail)))

;;;; editorconfig : editorconfig for Emacs
(use-package editorconfig
    :demand
    :config
    (editorconfig-mode 1))

;;;; exec-path-from-shell : Sane environment variables
(use-package exec-path-from-shell
    :demand
    :config
    (when (daemonp)
        (exec-path-from-shell-initialize)))

;;;; Main color theme : modus-themes
(use-package modus-themes
    :demand
    :init
    ;; Add all your customizations prior to loading the themes
    (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-region '(bg-only no-extend))

    ;; Load the theme files before enabling a theme
    (modus-themes-load-themes)
    :config
    (if (daemonp)
        (add-hook 'after-make-frame-functions
            (lambda (frame)
                (with-selected-frame frame
                    (modus-themes-load-operandi)))) ;; OR (modus-themes-load-vivendi)
        (modus-themes-load-operandi))) ;; OR (modus-themes-load-vivendi))

;;;; modeline : mini-modeline
(use-package mini-modeline
    :demand
    :init
    (setq mode-line-position (list "%l:%c %p"))
    (setq mode-line-modes (list "%m"))
    (setq mini-modeline-enhance-visual t)
    (setq mini-modeline-display-gui-line t)
    (setq mini-modeline-echo-duration 10)
    (setq mini-modeline-right-padding 1)
    (setq mini-modeline-l-format nil)
    (setq mini-modeline-r-format
        '("%e"
             mode-line-front-space
             mode-line-position
             " "
             mode-line-mule-info     ; Information on character sets, encodings, and other human-language details
             mode-line-client        ; Identifies frames created by emacsclient
             mode-line-modified      ; Modified and read-only status
             mode-line-remote        ; At-sign (@) for buffers visiting remote files, otherwise a dash
             " "
             mode-line-modes))
    :config
    (if (daemonp)
        (add-hook 'after-make-frame-functions
            (lambda (frame)
                (with-selected-frame frame
                    (mini-modeline-mode t))))
        (mini-modeline-mode t)))

;;;; pulsar : Pulse highlight line on demand or after running select functions
(use-package pulsar
    :demand
    :config
    (setq pulsar-pulse-on-window-change t)
    (setq pulsar-pulse t)
    (setq pulsar-delay 0.055)
    (setq pulsar-iterations 10)
    (setq pulsar-face 'pulsar-magenta)
    (setq pulsar-highlight-face 'pulsar-yellow)
    (pulsar-global-mode 1))

;;;; diminish : Hide the mode line string for modes (called the lighter)
(use-package diminish
    :demand
    :config
    (diminish 'eldoc-mode)
    (diminish 'flycheck-mode)
    (diminish 'flyspell-mode)
    (diminish 'flyspell-prog-mode)
    (diminish 'abbrev-mode))

(use-package guru-mode
    :demand
    :config
    (setq guru-warn-only t)
    ;; (add-to-list 'guru-affected-bindings-list '("<C-left>" "M-b" left-word))
    (guru-global-mode +1))

;;;; avy : GNU Emacs package for jumping to visible text using a char-based decision tree
(use-package avy
    :demand
    :config
    (setq avy-all-windows t)
    (setq avy-background t))

;;;; super-save : auto-saves your buffers, when certain events happen
(use-package super-save
    :demand
    :config
    ;; add integration with ace-window
    (add-to-list 'super-save-triggers 'ace-window)
    (super-save-mode +1)
    (diminish 'super-save-mode))

;;;; diff-hl : highlights uncommitted changes on the left side of the window
(use-package diff-hl
    :demand
    :config
    (global-diff-hl-mode +1)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;;; ace-window : GNU Emacs package for selecting a window to switch to
(use-package ace-window
    :demand)

;;;; crux : A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
    :demand)

;;;; vterm : terminal emulator
(use-package vterm
    :demand
    :hook (vterm-mode . (lambda()
                            (setq-local global-hl-line-mode nil)
                            (display-line-numbers-mode 0)))
    :config
    (setq vterm-shell "/usr/bin/bash"))

;;;; anzy : displays current match and total matches information in the mode-line in various search modes
(use-package anzu
    :demand
    :config
    (global-anzu-mode))

;;;; easy-kill : kill things easily
(use-package easy-kill
    :demand)

;;;; magit : Git front end (amazing!)
(use-package magit
    :config
    ;; Have magit-status go full screen and quit to previous configuration.
    ;; Taken from http://whattheemacsd.com/setup-magit.el-01.html#comment-748135498
    ;; and http://irreal.org/blog/?p=2253
    (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))
    (defadvice magit-quit-window (after magit-restore-screen activate)
        (jump-to-register :magit-fullscreen))

    (setq git-commit-summary-max-length 80
        vc-handled-backends (delq 'Git vc-handled-backends)))

;;;; which-key : Displays command shortcuts when typing commands
(use-package which-key
    :demand
    :config (which-key-mode +1)
    (diminish 'which-key-mode))

;;;; undo-tree : treat undo history as a tree
(use-package undo-tree
    :demand
    :config
    ;; autosave the undo-tree history
    (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
    (setq undo-tree-auto-save-history t)
    (global-undo-tree-mode +1)
    (diminish 'undo-tree-mode))

;;;; hydra : Keybindings combinations
(use-package hydra)

;;;; Vertico : Completion for commands in a vertical way
(use-package vertico
    :demand
    :init (vertico-mode)
    :custom
    (vertico-cycle t)
    (vertico-count 15))

;;;; Marginalia : Display additional completion data (doc strings, file permissions...)
(use-package marginalia
    :demand
    :config (marginalia-mode))

;;;; Orderless : Matching of several patterns without order in completion
(use-package orderless
    :demand
    :custom
    ((completion-styles '(orderless basic))
        (completion-category-defaults nil)
        (completion-category-overrides '((file (styles partial-completion))))))

;;;; Consult : a collection of commands that improve emacs defaults
(use-package consult
    :demand
    ;; Enable automatic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)
    :init
    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

    :config
    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
        consult-theme
        :preview-key '(:debounce 0.2 any)
        consult-ripgrep consult-git-grep consult-grep
        consult-bookmark consult-recent-file consult-xref
        consult--source-bookmark consult--source-recent-file
        consult--source-project-recent-file
        :preview-key (kbd "M-."))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<")) ;; (kbd "C-+")

;;;; embard : Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
    :demand
    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
        '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
             nil
             (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
    :after (embark consult)
    :demand
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;; Expand Region : expand or contract selection
(use-package expand-region
    :demand)

;;;; Helpful : nice looking and more complete help buffers
(use-package helpful
    :demand)

;;;; Org mode : Base mode for note taking
(use-package org
    :config
    ;; To get the most out of themes
    (setq org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t)
    (setq org-startup-indented t)
    (setq org-startup-folded t)
    (setq org-hide-emphasis-markers t) ;; hide the emphasis markup (e.g. /.../ for italics, *...* for bold, etc.)
    :hook (org-mode . (lambda()
                          ;; disable auto-pairing of "<" in org-mode
                          (setq-local electric-pair-inhibit-predicate
                              `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))
                          (add-to-list 'recentf-exclude ".*org$") ; Ignore org files from recentf due to agenda loading everything
                          (variable-pitch-mode)
                          (visual-line-mode))))

;;;; Org bullets : Pretty mode for org
(use-package org-bullets
    :hook (org-mode . org-bullets-mode))

;;;; org-appear : toggle visibility of hidden elements
(use-package org-appear
    :hook (org-mode . org-appear-mode))

;;;; encryption
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(progn
    (require 'epa-file)
    (epa-file-enable)
    (setq
        epa-file-encrypt-to user-mail-address
        epa-file-select-keys 'silent
        epa-file-cache-passphrase-for-symmetric-encryption nil)

    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq org-crypt-disable-auto-save nil
        org-tags-exclude-from-inheritance (quote ("crypt"))
        org-crypt-key nil
        org-crypt-key user-mail-address))

;;; Development packages and options
;;;; ag : Front end for the CLI utility ag
(use-package ag
    :custom (ag-highlight-search t))

;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

;;;; neotree : A Emacs tree plugin like NerdTree for Vim.
(use-package neotree
    :config
    (setq neo-theme 'ascii
        neo-window-width 42
        neo-smart-open t
        neo-create-file-auto-open nil
        neo-show-updir-line t
        neo-show-hidden-files t
        neo-auto-indent-point t
        neo-vc-integration nil
        neo-autorefresh nil
        projectile-switch-project-action 'neotree-projectile-action
        neo-hidden-regexp-list '(;; vcs folders
                                    "^\\.\\(?:git\\|hg\\|svn\\)$"
                                    ;; compiled files
                                    "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
                                    ;; generated files, caches or local pkgs
                                    "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
                                    ;; org-mode folders
                                    "^\\.\\(?:sync\\|export\\|attach\\)$"
                                    ;; temp files
                                    "~$"
                                    "^#.*#$"
                                    ;; Others
                                    "^\\.\\(cache\\|tox\\|coverage\\)$"
                                    "^\\.\\(DS_Store\\|python\\-version\\)"
                                    "^\\(htmlcov\\)$" "\\.elcs$"
                                    "^\\.coverage\\..*" "\\.ipynb.*$" "\\.py[cod]$"
                                    "^\\.#.*$" "^__pycache__$"
                                    "\\.gcda$" "\\.gcov$" "\\.gcno$" "\\.lo$" "\\.o$" "\\.so$"
                                    "^\\.cproject$" "^\\.project$" "^\\.projectile$"
                                    "\\.egg\-info$")))

;;;; deft : plain text notes
(use-package deft
    :config
    (setq
        deft-directory "~/Dropbox/Apps/org/notes"
        deft-extensions '("org" "md" "txt")
        deft-default-extension "org"
        deft-recursive nil
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-file-naming-rules '((noslash . "-")
                                    (nospace . "-")
                                    (case-fn . downcase))
        deft-auto-save-interval 0))

;;;; elfeed : rss
(use-package elfeed
    :config
    (setq elfeed-search-title-min-width 60)
    (setq elfeed-search-title-max-width 100)
    (setq elfeed-search-trailing-width 0)
    (setq elfeed-search-filter "@6-months-ago +unread")
    (setq elfeed-db-directory "~/Dropbox/Apps/elfeed/elfeed_db"))

;;;; elfeed-org : rss
(use-package elfeed-org
    :init (elfeed-org)
    :config
    (setq rmh-elfeed-org-files (list "~/Dropbox/Apps/elfeed/elfeed.org")))

;;;; markdown-mode : edit markdown-formatted text
(use-package markdown-mode
    :mode (("\\.md\\'" . gfm-mode)
              ("\\.markdown\\'" . gfm-mode))
    :config
    (setq markdown-fontify-code-blocks-natively t)
    :preface
    (defun jekyll-insert-image-url ()
        (interactive)
        (let* ((files (directory-files "../assets/images"))
                  (selected-file (completing-read "Select image: " files nil t)))
            (insert (format "![%s](/assets/images/%s)" selected-file selected-file))))

    (defun jekyll-insert-post-url ()
        (interactive)
        (let* ((project-root (projectile-project-root))
                  (posts-dir (expand-file-name "_posts" project-root))
                  (default-directory posts-dir))
            (let* ((files (remove "." (mapcar #'file-name-sans-extension (directory-files "."))))
                      (selected-file (completing-read "Select article: " files nil t)))
                (insert (format "{%% post_url %s %%}" selected-file))))))

;;;; adoc-mode : ascii docs
(use-package adoc-mode
    :mode "\\.adoc\\'")

;;;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(use-package csv-mode
    :mode "\\.csv\\'")

;;;; yaml-mode : Support gitlab-ci.yml
(use-package yaml-mode
    :mode "\\.yml\\'")

;;;; web-mode : Support various web files
(use-package web-mode
    :preface
    (defun +my/match-buffer-extension(&rest extensions)
        "Returns t if the current buffer has an extension in EXTENSIONS"
        (if (member (file-name-extension (buffer-name)) extensions)
            t))
    :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.vue\\'")
    :hook (web-mode . (lambda () (when (+my/match-buffer-extension "ts" "js" "vue")
                                     (setq-local lsp-auto-format t))))
    :custom
    (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
    (web-mode-markup-indent-offset 2)) ; For html : use an indent of size 2 (default is 4)

;;;; prettier-js : Formatting on save, used by my-ts-mode for .js and .ts files
(use-package prettier-js
    :custom
    (prettier-js-show-errors nil)
    (prettier-js-args '("--semi" "false"
                           "--single-quote" "false"
                           "--tab-width" "4"
                           "--trailing-comma" "all"
                           "--print-width" "150")))

;;;; c++ mode
(use-package c++-mode
    :ensure nil  ; Part of emacs
    :mode ("\\.h\\'" "\\.cpp\\'" "\\.hpp\\'" "\\.hxx\\'" "\\.cxx\\'")
    :config
    (advice-add 'c-update-modeline :override #'ignore)) ;; Don't use a modeline suffix (i.e C++//l)

;;;; company : Completion frontend, used by lsp
(use-package company
    :demand
    :config
    (setq company-idle-delay 0.1)
    (setq company-show-quick-access t)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 2)
    (setq company-tooltip-align-annotations t)
    (global-company-mode)
    (diminish 'company-mode))

;;;; hl-todo : highlight TODO and similar keywords in comments and strings
(use-package hl-todo
    :demand
    :config
    (setq hl-todo-highlight-punctuation ":")
    (global-hl-todo-mode))

;;;; flyspell : on-the-fly spell checker
(use-package flyspell
    :ensure nil  ; Part of emacs
    :config
    (when (eq system-type 'windows-nt)
        (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
    (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode))

;;;; yasnippet : Dependency used by lsp to insert snippets. Used by some lsp commands like completion
(use-package yasnippet
    :hook (lsp-mode . (lambda()
                          (yas-minor-mode)
                          (diminish 'yas-minor-mode))))

;;;; flycheck : Syntax highlighting, used by lsp
(use-package flycheck
    :demand
    :config
    (setq flycheck-temp-prefix "flycheck_tmp")
    (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eldev
    :demand)

;;;; lsp-mode : Completion and syntax highlighting backend API, available for most languages
(use-package lsp-mode
    :hook ((lsp-mode . lsp-enable-which-key-integration)
              ((c-mode             ; clangd
                   c++-mode        ; clangd
                   c-or-c++-mode   ; clangd
                   ;; java-mode       ; eclipse-jdtls
                   js-mode         ; ts-ls (tsserver wrapper)
                   js-jsx-mode     ; ts-ls (tsserver wrapper)
                   typescript-mode ; ts-ls (tsserver wrapper)
                   python-mode     ; pyright
                   web-mode        ; ts-ls/HTML/CSS
                   ;; haskell-mode    ; haskell-language-server
                   ) . lsp-deferred))
    :config
    (setq lsp-restart 'ignore
        lsp-headerline-breadcrumb-enable nil
        lsp-enable-symbol-highlighting t
        lsp-enable-indentation nil
        lsp-eldoc-enable-hover t
        lsp-eldoc-render-all nil
        lsp-signature-render-documentation nil
        lsp-signature-auto-activate nil
        lsp-signature-doc-lines 1
        lsp-auto-guess-root nil
        lsp-enable-file-watchers nil
        lsp-enable-on-type-formatting nil)

    ;; Zig
    ;; (setq lsp-zig-zls-executable (expand-file-name "zig/zls/zig-out/bin/zls" +my/software-path))

    ;; C++
    (with-eval-after-load "c++-mode"
        (setq lsp-clients-clangd-args
            '("-j=4"
                 "--malloc-trim"
                 "--log=error"
                 "--background-index"
                 "--clang-tidy"
                 "--cross-file-rename"
                 "--completion-style=detailed"
                 "--pch-storage=memory"
                 "--header-insertion=never"
                 "--header-insertion-decorators=0"))))

;;;; lsp-ui : annoying ui for lsp
(use-package lsp-ui
    :config
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-doc-header t)
    (setq lsp-ui-doc-include-signature t)
    (setq lsp-ui-doc-border (face-foreground 'default))
    (setq lsp-ui-sideline-show-code-actions t)
    (setq lsp-ui-sideline-delay 0.05))

;;;; lsp-pyright : An LSP backend for python
(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

;;;; rustic : blazingly fast
(use-package rustic
    :config
    (setq
        rustic-lsp-server 'rust-analyzer
        rustic-format-on-save nil
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-inlay-hints-mode nil
        lsp-rust-analyzer-server-display-inlay-hints nil))

(use-package format-all
    :hook ((c-mode . format-all-mode)
              (c++-mode . format-all-mode)
              (python-mode . format-all-mode)
              (format-all-mode . format-all-ensure-formatter))
    :config
    (custom-set-variables
        '(format-all-formatters (quote (("C++" clang-format)
                                           ("Python" black))))))

;;;; general : binding keys
(use-package general
    :demand
    :config
    ;; * Global Keybindings
    ;; `general-define-key' acts like `global-set-key' when :keymaps is not
    ;; specified (because ":keymaps 'global" is the default)
    ;; kbd is not necessary and arbitrary amount of key def pairs are allowed
    (general-define-key
        ;;;; general keys
        "C-c u" 'browse-url-at-point ; simple browse url
        "C-x k" 'kill-this-buffer ; kill buffer without prompt
        "C-x K" 'kill-buffer ; prompt for buffer to kill
        "M-/" 'hippie-expand ; use hippie-expand instead of debbrev
        [remap list-buffers] 'ibuffer ; ibuffer is better than list-buffers

        ;;;; easy-kill
        [remap kill-ring-save] 'easy-kill

        ;;;; avy
        "M-j" 'avy-goto-char-timer ; most usefull avy function

        ;;;; ace-window
        [remap other-window] 'ace-window ; better other window

        ;;;; anzy
        "M-%" 'anzu-query-replace
        "C-M-%" 'anzu-query-replace-regexp

        ;;;; expand-region
        "C-=" 'er/expand-region

        ;;;; magit
        "C-x g" 'magit-status

        ;;;; embark
        "C-." 'embark-act        ; pick some comfortable binding
        "C-;" 'embark-dwim       ; good alternative: "M-."
        "C-h B" 'embark-bindings ; alternative for `describe-bindings'

        ;;;; projectile
        "C-c p" 'projectile-command-map

        ;;;; helpful
        ;; (define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
        [remap describe-command] 'helpful-command
        [remap describe-function] 'helpful-callable
        [remap describe-key] 'helpful-key
        [remap describe-symbol] 'helpful-symbol
        [remap describe-variable] 'helpful-variable
        "C-h F" 'helpful-function
        "C-h K" 'describe-keymap

        ;;;; consult
        ;; C-c bindings (mode-specific-map)
        "C-c h" 'consult-history
        "C-c m" 'consult-mode-command
        ;; "C-c k" 'consult-kmacro
        ;; C-x bindings (Ctl-x-map)
        "C-x M-:" 'consult-complex-command     ; orig. repeat-complex-command
        "C-x b" 'consult-buffer                ; orig. switch-to-buffer
        "C-x 4 b" 'consult-buffer-other-window ; orig. switch-to-buffer-other-window
        "C-x 5 b" 'consult-buffer-other-frame  ; orig. switch-to-buffer-other-frame
        "C-x r b" 'consult-bookmark            ; orig. bookmark-jump
        "C-x p b" 'consult-project-buffer      ; orig. project-switch-to-buffer
        ;; Custom M-# bindings for fast register access
        "M-#" 'consult-register-load
        "M-'" 'consult-register-store          ; orig. abbrev-prefix-mark (unrelated)
        "C-M-#" 'consult-register
        ;; Other custom bindings
        "M-y" 'consult-yank-pop                ; orig. yank-pop
        "<help> a" 'consult-apropos            ; orig. apropos-command
        ;; M-g bindings (goto-map)
        "M-g e" 'consult-compile-error
        "M-g f" 'consult-flymake               ; Alternative: consult-flycheck
        "M-g g" 'consult-goto-line             ; orig. goto-line
        "M-g M-g" 'consult-goto-line           ; orig. goto-line
        "M-g o" 'consult-outline               ; Alternative: consult-org-heading
        "M-g m" 'consult-mark
        "M-g k" 'consult-global-mark
        "M-g i" 'consult-imenu
        "M-g I" 'consult-imenu-multi
        ;; M-s bindings (search-map)
        "M-s d" 'consult-find
        "M-s D" 'consult-locate
        "M-s g" 'consult-grep
        "M-s G" 'consult-git-grep
        "M-s r" 'consult-ripgrep
        "M-s l" 'consult-line
        "M-s L" 'consult-line-multi
        "M-s m" 'consult-multi-occur
        "M-s k" 'consult-keep-lines
        "M-s u" 'consult-focus-lines
        ;; Isearch integration
        "M-s e" 'consult-isearch-history

        ;;;; crux
        ;; "C-c o" 'crux-open-with
        ;; "C-c u" 'crux-view-url
        "C-o" 'crux-smart-open-line
        "M-o" 'crux-smart-open-line-above
        "C-x C-r" 'crux-recentf-find-file
        "C-c f" 'crux-recentf-find-file
        "C-c F" 'crux-recentf-find-directory
        ;; "C-c n" 'crux-cleanup-buffer-or-region
        "C-M-z" 'crux-indent-defun
        "C-c e" 'crux-eval-and-replace
        "C-c w" 'crux-swap-windows
        "C-c D" 'crux-delete-file-and-buffer
        "C-c r" 'crux-rename-buffer-and-file
        "C-c t" 'crux-visit-term-buffer
        "C-c k" 'crux-kill-other-buffers
        "C-c TAB" 'crux-indent-rigidly-and-copy-to-clipboard
        "C-c I" 'crux-find-user-custom-file
        "C-c S" ' crux-find-shell-init-file
        "C-^" 'crux-top-join-line
        "C-c s" 'crux-ispell-word-then-abbrev
        "C-k" 'crux-smart-kill-line
        "C-<backspace>" 'crux-kill-line-backwards
        "C-x 4 t" 'crux-transpose-windows
        "C-x C-u" 'crux-upcase-region
        "C-x C-l" 'crux-downcase-region
        "C-x M-c" 'crux-capitalize-region
        [remap move-beginning-of-line] 'crux-move-beginning-of-line
        [shift return] 'crux-smart-open-line
        [control shift return] 'crux-smart-open-line-above
        [remap kill-whole-line] 'crux-kill-whole-line)

    ;; * Prefix Keybindings
    ;; :prefix can be used to prevent redundant specification of prefix keys
    (general-define-key :prefix "C-c c" ; code
        "d" 'lsp-describe-thing-at-point
        "f" 'format-all-buffer)
    (general-define-key :prefix "C-c n" ; notes
        "d" 'deft-find-file
        "D" 'deft)
    (general-define-key :prefix "C-c o" ; open
        "c" 'calc
        "C" 'quick-calc
        "e" 'elfeed
        "n" 'neotree-toggle
        "v" 'vterm)

    ;; * Mode Keybindings
    ;; `general-define-key' is comparable to `define-key' when :keymaps is specified
    (general-define-key :keymaps 'dired-mode-map
        "C-c o" 'crux-open-with)
    (general-define-key :keymaps 'isearch-mode-map
        "M-e" 'consult-isearch-history         ; orig. isearch-edit-string
        "M-s e" 'consult-isearch-history       ; orig. isearch-edit-string
        "M-s l" 'consult-line                  ; needed by consult-line to detect isearch
        "M-s L" 'consult-line-multi)           ; needed by consult-line to detect isearch
    (general-define-key :keymaps 'minibuffer-local-map
        "M-s" 'consult-history                 ; orig. next-matching-history-element
        "M-r" 'consult-history)                ; orig. previous-matching-history-element
    )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(format-all-formatters '(("C++" clang-format) ("Python" black)) t)
 '(package-selected-packages
    '(moody lsp-ui format-all general elfeed-org elfeed guru-mode org-appear pulsar neotree yasnippet yaml-mode which-key web-mode vertico use-package undo-tree super-save rainbow-delimiters projectile prettier-js org-bullets orderless modus-themes marginalia magit lsp-treemacs lsp-pyright hl-todo helpful flycheck-eldev expand-region exec-path-from-shell editorconfig easy-kill diminish diff-hl csv-mode crux consult company anzu ag adoc-mode))
 '(tab-stop-list
    '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
