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

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
    '((:eval (if (buffer-file-name)
                 (abbreviate-file-name (buffer-file-name))
                 "%b"))))

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
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
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
    (setq-default show-trailing-whitespace t) ; Show in red the spaces forgotten at the end of lines
    (setq next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
    (setq use-short-answers t) ; Abreviate Yes/No to y or n
    (setq require-final-newline t) ;; Newline at end of file
    (setq-default fill-column 135) ;; Wrap lines at 135 characters
    (setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
    (setq-default tab-width 4)            ;; but maintain correct appearance
    (setq indent-line-function 'insert-tab) ;; indent the current line
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
(set-frame-font "Iosevka 16" nil t)

;; from Steve Yegge
;; TODO: Bind `kill-region' to "C-x C-k" and "C-c C-k"
;; TODO: Bind `backward-kill-word' to "C-w"
(use-package emacs
    :ensure nil  ; emacs built-in
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
    ;; activate it for all buffers
    (setq-default save-place t))

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

;;;; Dired : built-in navigation of folders
(use-package dired
    :ensure nil  ; emacs built-in
    ;; :bind (:map dired-mode-map ("u" . dired-up-directory))
    :config
    ;; reuse current buffer by pressing 'a'
    (put 'dired-find-alternate-file 'disabled nil)
    ;; always delete and copy recursively
    (setq dired-recursive-deletes 'always)
    (setq dired-recursive-copies 'always)
    ;; if there is a dired buffer displayed in the next window, use its
    ;; current subdir, instead of the current subdir of this dired buffer
    (setq dired-dwim-target t)
    ;; enable some really cool extensions like C-x C-j(dired-jump)
    (require 'dired-x)
    :custom
    (dired-kill-when-opening-new-dired-buffer t)) ; Auto close previous folder buffer

;;;; whitespace : visualize blanks (tabs, spaces, newline, etc)
(use-package whitespace
    :ensure nil  ; emacs built-in
    :init
    (dolist (hook '(prog-mode-hook text-mode-hook))
        (add-hook hook #'whitespace-mode))
    (add-hook 'before-save-hook #'whitespace-cleanup)
    :config
    (setq whitespace-line-column 135) ;; limit line length
    (setq whitespace-style '(face tabs empty trailing lines-tail)))

;;;; editorconfig : editorconfig for Emacs
(use-package editorconfig
    :ensure t
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
    ;; Load the theme of your choice:
    (modus-themes-load-operandi) ;; OR (modus-themes-load-vivendi)
    :bind ("<f5>" . modus-themes-toggle))

;;;; diminish : Hide the mode line string for modes (called the lighter)
(use-package diminish
    :demand
    :config
    (diminish 'eldoc-mode)
    (diminish 'flycheck-mode)
    (diminish 'flyspell-mode)
    (diminish 'flyspell-prog-mode)
    (diminish 'abbrev-mode))

;;; avy : GNU Emacs package for jumping to visible text using a char-based decision tree
(use-package avy
    :demand
    :bind (("M-j" . avy-goto-char-timer)
              ("C-c ." . avy-goto-word-or-subword-1)
              ("C-c ," . avy-goto-char)
              ("M-g f" . avy-goto-line)
              ("M-g w" . avy-goto-word-or-subword-1))
    :config
    (setq avy-background t))

;; super-save : auto-saves your buffers, when certain events happen
(use-package super-save
  :demand
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1)
  (diminish 'super-save-mode))

;;;; diff-hl : highlights uncommitted changes on the left side of the window
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;;; ace-window : GNU Emacs package for selecting a window to switch to
(use-package ace-window
  :demand
  :config
  (global-set-key (kbd "M-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

;;;; crux : A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
    :demand
    :bind (("C-c o" . crux-open-with)
              ("M-o" . crux-smart-open-line)
              ("C-c n" . crux-cleanup-buffer-or-region)
              ("C-c f" . crux-recentf-find-file)
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
              ("s-r" . crux-recentf-find-file)
              ("s-j" . crux-top-join-line)
              ("C-^" . crux-top-join-line)
              ("s-k" . crux-kill-whole-line)
              ("C-<backspace>" . crux-kill-line-backwards)
              ("s-o" . crux-smart-open-line-above)
              ([remap move-beginning-of-line] . crux-move-beginning-of-line)
              ([(shift return)] . crux-smart-open-line)
              ([(control shift return)] . crux-smart-open-line-above)
              ([remap kill-whole-line] . crux-kill-whole-line)
              ("C-c s" . crux-ispell-word-then-abbrev)))

;; FIXME: Figure out why the vterm module stopped compiling properly
;; (use-package vterm
;;   :ensure t
;;   :config
;;   (setq vterm-shell "/bin/bash")
;;   ;; macOS
;;   (global-set-key (kbd "s-v") 'vterm)
;;   ;; Linux
;;   (global-set-key (kbd "C-c v") 'vterm))

;;;; anzy : displays current match and total matches information in the mode-line in various search modes
(use-package anzu
    :ensure t
    :bind (("M-%" . anzu-query-replace)
              ("C-M-%" . anzu-query-replace-regexp))
    :config
    (global-anzu-mode))

;;;; easy-kill : kill things easily
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

;;;; projectile : project interaction library for Emacs.
(use-package projectile
  :demand
  :init
  ;; TODO: (setq projectile-project-search-path '("~/projects/" "~/work/"))
  :config
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;;;; magit : Git front end (amazing!)
(use-package magit
    :bind (("C-x g" . magit-status))
    :custom
    (ediff-split-window-function 'split-window-horizontally)) ; Make ediff split side by side

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

;;;; Expand Region : expand or contract selection
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;;;; Helpful : nice looking and more complete help buffers
(use-package helpful)

;;;; Org mode : Base mode for note taking
(use-package org
    :custom ((org-agenda-files '("~/.org_roam")) ; TODO: For autopopulating todos from notes
                (org-agenda-span 'month) ; To have a monthly view by default
                (org-agenda-start-on-weekday 1) ; Agenda starts on monday in agenda
                (calendar-week-start-day 1) ; Date picker starts on monday
                (org-capture-bookmark nil)) ; To disable adding a bookmark on each org capture
    :hook (org-mode . (lambda()
                          (require 'org-tempo) ; For templates like <sTAB to insert a code block
                          (require 'recentf)
                          (add-to-list 'recentf-exclude ".*org$") ; Ignore org files from recentf due to agenda loading everything
                          (org-indent-mode) ; Auto indent lines according to depth
                          (auto-fill-mode)))) ; Wrap lines when longer than fill column

;;;; Org bullets : Pretty mode for org
(use-package org-bullets
    :hook (org-mode . org-bullets-mode))

;;; Development packages and options
;;;; ag : Front end for the CLI utility ag
(use-package ag
    :custom (ag-highlight-search t))

;;;; rainbow-delimiters : Parenthesis color based on depth
(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

;;;; treemacs : Displays the current project on the left as in an IDE
(use-package treemacs
    :custom (treemacs-no-delete-other-windows nil))

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
  :hook
  (web-mode . (lambda () (when (+my/match-buffer-extension "ts" "js" "vue")
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

;;;; lsp-treemacs : treemacs style views for various lsp results
(use-package lsp-treemacs
    :demand)

;;;; company : Completion frontend, used by lsp
(use-package company
    :demand
    :config
    (setq company-idle-delay 0.5)
    (setq company-show-numbers t)
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
  :ensure t
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
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-eldev
  :demand)

;;;; lsp-mode : Completion and syntax highlighting backend API, available for most languages
;; The following packages need to be installed according to the language
;; Python : pip install pyright flake8
;; c++ : pacman -S clang bear (or jq)
;; vue.js, javascript, typescript : sudo npm install -g vls typescript-language-server
(use-package lsp-mode
    :hook
    (lsp-mode . lsp-enable-which-key-integration)
    :init (setq lsp-keymap-prefix "C-c l")
    :bind (("C-h l" . lsp-describe-thing-at-point))
    :custom
    ;; Formatting options for vue.js (.vue files)
    (lsp-enable-links nil) ; Make links non clickable
    (lsp-vetur-format-default-formatter-html "js-beautify-html")
    (lsp-vetur-format-default-formatter-options
        '((js-beautify-html
              (wrap_attributes . "preserve")
              (indent_size . 2)
              (wrap_attributes_indent_size . 2))
             (prettier
                 (singleQuote . :json-false)
                 (printWidth . 100)
                 (tabWidth . 4)
                 (trailingComma . "all")
                 (vueIndentScriptAndStyle . :json-false)
                 (semi . :json-false))))
    :config
    (setq lsp-headerline-arrow ">")) ; Material design icon not working on windows

;;;; lsp-format-and-save : format on save if lsp-auto-format is not nil
(defcustom lsp-auto-format nil
    "If not nil, lsp-format-and-save will format the buffer before saving"
    :type 'boolean)

(defun lsp-format-and-save()
    "Saves the current buffer and formats it if lsp-format-on-save is not nil"
    (interactive)
    (when (and (not buffer-read-only) lsp-auto-format)
        (lsp-format-buffer))
    (save-buffer))

;;;; lsp-pyright : An LSP backend for python
(use-package lsp-pyright
    :hook (python-mode . (lambda()
                             (require 'lsp-pyright)
                             (lsp-deferred))))

;;;; Hydra search text
(defhydra search(:exit t :columns 2)
    "Text search related commands"
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
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
