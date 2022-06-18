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
(setq backup-directory-alist
    `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
    `((".*" ,temporary-file-directory t)))

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
    (tool-bar-mode 0) ; Disable the toolbar in GUI mode
    (menu-bar-mode -1) ; Hide Menu bar
    (blink-cursor-mode -1) ;; the blinking cursor is nothing, but an annoyance
    (when (display-graphic-p) (scroll-bar-mode -1)) ; Disable the scroll bar in GUI mode
    (setq inhibit-startup-screen t) ; Hide the startup screen
    (setq scroll-margin 0 ;; nice scrolling
        scroll-conservatively 100000
        scroll-preserve-screen-position 1)
    (when (fboundp 'pixel-scroll-precision-mode)
        (pixel-scroll-precision-mode t))
    (fset 'yes-or-no-p 'y-or-n-p) ;; enable y/n answers
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
    (save-place-mode 1) ; Remember point in files
    (show-paren-mode 1) ; Highlight the matching parenthesis
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
                (set-frame-font "Iosevka 16" nil t))))
    (set-frame-font "Iosevka 16" nil t))

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
         (lambda () (setq show-trailing-whitespace nil))))
    :bind (;; kill buffers
              ("C-x k" . kill-this-buffer)
              ("C-x K" . kill-buffer)
              ;; use hippie-expand instead of debbrev
              ("M-/" . hippie-expand)
              ;; ibuffer is better than list-buffers
              ([remap list-buffers] . ibuffer)))

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
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
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
        (modus-themes-load-operandi)) ;; OR (modus-themes-load-vivendi)
    :bind ("<f5>" . modus-themes-toggle))

;;;; mini-modeline
;; modeline
(use-package mini-modeline
    :demand
    :init
    (setq mode-line-position (list "%l:%c %p"))
    (setq mode-line-modes (list "%m"))
    (setq mini-modeline-enhance-visual t)
    (when (display-graphic-p) (setq mini-modeline-display-gui-line t))
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

;;;; diminish : Hide the mode line string for modes (called the lighter)
(use-package diminish
    :demand
    :config
    (diminish 'eldoc-mode)
    (diminish 'flycheck-mode)
    (diminish 'flyspell-mode)
    (diminish 'flyspell-prog-mode)
    (diminish 'abbrev-mode))

;;;; avy : GNU Emacs package for jumping to visible text using a char-based decision tree
(use-package avy
    :demand
    :bind (("M-j" . avy-goto-char-timer)
              ("C-c ." . avy-goto-word-or-subword-1)
              ("C-c ," . avy-goto-char)
              ("M-g f" . avy-goto-line)
              ("M-g w" . avy-goto-word-or-subword-1))
    :config
    (setq avy-all-windows t)
    (setq avy-background t))

;;;; smartparens : minor mode for dealing with pairs in Emacs
(use-package smartparens
  :demand
  :config
    (require 'smartparens-config)
    (show-smartparens-global-mode +1)
    (smartparens-global-mode 1)
    (show-paren-mode t))

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
  :demand
  :config
  (global-set-key [remap other-window] 'ace-window))

;;;; crux : A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
    :demand
    :bind (("C-c o" . crux-open-with)
              ("C-o" . crux-smart-open-line)
              ("M-o" . crux-smart-open-line-above)
              ("C-x C-r" . crux-recentf-find-file)
              ("C-c f" . crux-cleanup-buffer-or-region)
              ("C-M-z" . crux-indent-defun)
              ("C-c u" . crux-view-url)
              ("C-c e" . crux-eval-and-replace)
              ("C-c w" . crux-swap-windows)
              ("C-c D" . crux-delete-file-and-buffer)
              ("C-c r" . crux-rename-buffer-and-file)
              ("C-c t" . crux-visit-term-buffer)
              ("C-c k" . crux-kill-other-buffers)
              ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
              ("C-c I" . crux-find-user-init-file)
              ("C-c S" . crux-find-shell-init-file)
              ("C-^" . crux-top-join-line)
              ;; TODO: ("C-k" . crux-kill-whole-line)
              ("C-<backspace>" . crux-kill-line-backwards)
              ([remap move-beginning-of-line] . crux-move-beginning-of-line)
              ([(shift return)] . crux-smart-open-line)
              ([(control shift return)] . crux-smart-open-line-above)
              ([remap kill-whole-line] . crux-kill-whole-line)
              ("C-c s" . crux-ispell-word-then-abbrev)))

;;;; vterm : terminal emulator
(use-package vterm
  :demand
  :hook (vterm-mode . (lambda()
                        (setq-local global-hl-line-mode nil)
                        (display-line-numbers-mode 0)))
  :config
  (setq vterm-shell "/usr/bin/bash")
  (global-set-key (kbd "C-c v") 'vterm))

;;;; anzy : displays current match and total matches information in the mode-line in various search modes
(use-package anzu
    :demand
    :bind (("M-%" . anzu-query-replace)
              ("C-M-%" . anzu-query-replace-regexp))
    :config
    (global-anzu-mode))

;;;; easy-kill : kill things easily
(use-package easy-kill
  :demand
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;;;; magit : Git front end (amazing!)
(use-package magit
    :bind (("C-x g" . magit-status))
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
    :init (vertico-mode)
    :custom
    (vertico-cycle t)
    (vertico-count 15))

;;;; Marginalia : Display additional completion data (doc strings, file permissions...)
(use-package marginalia
    :init (marginalia-mode))

;;;; Orderless : Matching of several patterns without order in completion
(use-package orderless
    :custom
    ((completion-styles '(orderless basic))
        (completion-category-defaults nil)
        (completion-category-overrides '((file (styles partial-completion))))))

;;;; Consult : a collection of commands that improve emacs defaults
(use-package consult
    :demand
    :bind (;; C-x bindings (ctl-x-map)
              ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
              ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
              ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
              ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
              ;; Custom M-# bindings for fast register access
              ("M-#" . consult-register-load)
              ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
              ("C-M-#" . consult-register)
              ;; Other custom bindings
              ("M-y" . consult-yank-pop)                ;; orig. yank-pop
              ("<help> a" . consult-apropos)            ;; orig. apropos-command
              ;; M-g bindings (goto-map)
              ("M-g e" . consult-compile-error)
              ("M-g f" . consult-flycheck)
              ("M-g g" . consult-goto-line)             ;; orig. goto-line
              ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
              ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
              ("M-g m" . consult-mark)
              ("M-g k" . consult-global-mark)
              ("M-g i" . consult-imenu)
              ("M-g I" . consult-imenu-multi)
              ;; M-s bindings (search-map)
              ("M-s f" . consult-find)
              ("M-s F" . consult-locate)
              ("M-s g" . consult-grep)
              ("M-s G" . consult-git-grep)
              ("M-s r" . consult-ripgrep)
              ("M-s l" . consult-line)
              ("M-s L" . consult-line-multi)
              ("M-s m" . consult-multi-occur)
              ("M-s k" . consult-keep-lines)
              ("M-s u" . consult-focus-lines)))

;;;; embard : Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :demand
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

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
  :demand
  :bind ("C-=" . er/expand-region))

;;;; Helpful : nice looking and more complete help buffers
(use-package helpful)

;;;; Org mode : Base mode for note taking
(use-package org
  :config
  ;; Latex previews in org-mode
  (plist-put org-format-latex-options :background 'default)

  ;; To get the most out of themes
  (setq
   org-fontify-whole-heading-line t
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t)

  ;; settings
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|org\\.txt\\)$" . org-mode))
  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-cycle-separator-lines 2)
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))
  (setq org-agenda-file-regexp "\\`[^.].*\\.\\(org\\.txt\\|org\\)\\'")
  (setq org-log-done t)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-imenu-depth 8)
  (setq org-hide-emphasis-markers t) ;; hide the emphasis markup (e.g. /.../ for italics, *...* for bold, etc.)
  (setq org-adapt-indentation nil ;; prevent demoting heading also shifting text inside sections
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 2)

  ;; Show the daily agenda by default.
  (setq org-agenda-span 'day)
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " ")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-hide-tags-regexp ".") ;; ask the agenda to hide any tag (.) that may be present.
  (setq org-capture-templates            ;; set our capture templates
        `(("i" "Inbox" entry (file "inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
          ("n" "Note" entry (file "notes.org")
           ,(concat "* Note (%a)\n"
                    "/Entered on/ %U\n" "\n" "%?"))))
  :hook (org-mode . (lambda()
                      (require 'org-tempo) ; For templates like <sTAB to insert a code block
                      (require 'recentf)
                      (add-to-list 'recentf-exclude ".*org$") ; Ignore org files from recentf due to agenda loading everything
                      (org-indent-mode) ; Auto indent lines according to depth
                      (visual-line-mode))))

;;;; Org bullets : Pretty mode for org
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

;;;; encryption
;; https://orgmode.org/worg/org-tutorials/encrypting-files.html
(require 'epa-file)
(progn
    (epa-file-enable)
    (setq
        epa-file-encrypt-to user-mail-address
        epa-file-select-keys 'silent
        epa-file-cache-passphrase-for-symmetric-encryption nil))
(require 'org-crypt)
(progn
    (org-crypt-use-before-save-magic)
    (setq org-crypt-disable-auto-save nil) ;; don't ask to disable auto-save
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    (setq org-crypt-key nil)
    (setq org-crypt-key user-mail-address))

;;; Development packages and options
;;;; ag : Front end for the CLI utility ag
(use-package ag
    :custom (ag-highlight-search t))

;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

;;;; neotree : A Emacs tree plugin like NerdTree for Vim.
(use-package neotree
  :bind (("C-c n" . neotree-toggle))
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
    neo-hidden-regexp-list
    '(;; vcs folders
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
                           (lsp-deferred)
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
    :hook (c++-mode . lsp-deferred)
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
    ;; invert the navigation direction if the the completion popup-isearch-match
    ;; is displayed on top (happens near the bottom of windows)
    (setq company-tooltip-flip-when-above t)
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
    :hook (lsp-mode . lsp-enable-which-key-integration)
    :init (setq lsp-keymap-prefix "C-c l")
    :bind (("C-h l" . lsp-describe-thing-at-point))
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
             "--header-insertion-decorators=0")))

;;;; lsp-format-and-save : format on save if lsp-auto-format is not nil
(defcustom lsp-auto-format nil
    "If not nil, lsp-format-and-save will format the buffer before saving."
    :type 'boolean)

(defun lsp-format-and-save()
    "Save the current buffer and formats it if lsp-format-on-save is not nil."
    (interactive)
    (when (and (not buffer-read-only) lsp-auto-format)
        (lsp-format-buffer))
    (save-buffer))

;;;; lsp-pyright : An LSP backend for python
(use-package lsp-pyright
    :hook (python-mode . (lambda()
                             (require 'lsp-pyright)
                             (lsp-deferred))))

;;;; rustic : blazingly fast
(use-package rustic
  :config
    (setq
        rustic-lsp-server 'rust-analyzer
        rustic-format-on-save nil
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-inlay-hints-mode nil
        lsp-rust-analyzer-server-display-inlay-hints nil))

;;;; Hydra search text
(defhydra search(:exit t :columns 2)
  "Tex
t search related commands"
    ("o" consult-line "Occurences in file")
    ("s" isearch-forward "Next occurence in file")
    ("w" isearch-forward-symbol-at-point "Next occurence in file of word")
    ("r" query-replace "Next occurence in file")
    ("a" (lambda() (interactive) (consult-grep default-directory)) "Grep in current directory")
    ("p" projectile-ag "Grep in current project")
    ("b" multi-occur-in-matching-buffers "Occur in all buffers"))
;; (define-key ijkl-local-mode-map "s" 'search/body)

;;;; Hydra find
(defhydra find(:exit t :columns 2)
    "Search related commands"
    ("d" dired-jump "Open current directory in dired")
    ("f" find-file "Find file by URL")
    ("e" flycheck-list-errors "Errors current file (flycheck + LSP)")
    ("t" lsp-treemacs-errors-list "Errors current project (LSP treemacs)")
    ("r" lsp-find-references "LSP find references")
    ("o" ff-find-other-file "switch header/cpp")
    ("p" project-find-file "project-find-file")
    ("P" projectile-find-file-other-window "projectile-find-file-other-window"))
;; (define-key ijkl-local-mode-map "f" 'find/body)

;;;; Hydra go
(defhydra go(:exit t :columns 3)
    "Jump to destination in text"
    ("l" goto-line "Go to line n°")
    ("b" bookmark-jump "Bookmark jump")
    ("r" jump-to-register "Jump to register (see point-to-register)")
    ("j" lsp-find-definition "LSP jump to definition")
    ("g" flycheck-next-error "Next error (Flycheck)")
    ("e" flycheck-next-error "Next error (Flycheck)")
    ("E" flycheck-previous-error "Previous error (Flycheck)")
    ("n" forward-sexp  "Go to the closing parenthesis/bracket")
    ("," org-roam-node-find "Go to an org roam file")
    ("p" backward-sexp "Go to the opening parenthesis/bracket"))
;; (define-key ijkl-local-mode-map "g" 'go/body)

;;;; Magit hydra
(defhydra magit(:exit t :columns 1)
    "Magit commands"
    ("s" magit-status "Status (Home)")
    ("f" magit-file-dispatch "File commands")
    ("v" magit-dispatch "Global Commands"))
;; (define-key ijkl-local-mode-map "v" 'magit/body)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(neotree yasnippet yaml-mode which-key web-mode vertico use-package undo-tree super-save rainbow-delimiters projectile prettier-js org-bullets orderless modus-themes marginalia magit lsp-treemacs lsp-pyright hl-todo helpful flycheck-eldev expand-region exec-path-from-shell editorconfig easy-kill diminish diff-hl csv-mode crux consult company anzu ag adoc-mode))
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
