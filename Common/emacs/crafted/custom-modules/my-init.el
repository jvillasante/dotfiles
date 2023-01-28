;;; my-init.el --- Custom Init Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Init Customizations

;;; Code:

(setq! user-full-name "Julio C. Villasante")
(setq! user-mail-address "jvillasantegomez@gmail.com")
(setq! user-login-name "jvillasante")
(setq! +my/home-path (expand-file-name "~/"))
(setq! +my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path))
(setq! +my/software-path (expand-file-name "Workspace/Software/" +my/home-path))
(setq! +my/dropbox-path (expand-file-name "Dropbox/" +my/home-path))

(cond
    ((string-equal system-type "windows-nt")
        (progn
            (message "Emacs Running on Microsoft Windows (which is super weird!)")))
    ((string-equal system-type "darwin")
        (progn
            (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
            (setq! browse-url-browser-function 'browse-url-generic)
            (setq! browse-url-generic-program "open")
            (setq! +my/clang-path "/usr/local/opt/llvm/bin/clang")
            (setq! +my/mu-path "/usr/local/bin/mu")
            (setq! +my/msmtp-path "/usr/local/bin/msmtp")
            (setq! vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
            (setq! ns-use-proxy-icon nil)
            (setq! ns-use-thin-smoothing t)
            (setq! ns-alternate-modifier nil)
            (setq! mac-command-modifier 'meta)
            (setq! mac-option-modifier 'alt)
            (setq! mac-right-option-modifier 'alt)))
    ((string-equal system-type "gnu/linux")
        (progn
            (setq! browse-url-browser-function 'browse-url-generic)
            (setq! browse-url-generic-program "xdg-open")
            (setq! +my/clang-path "/usr/bin/clang")
            (setq! +my/mu-path "/usr/bin/mu")
            (setq! +my/msmtp-path "/usr/bin/msmtp")
            (setq! vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes"))))

;; store all backup and autosave files in the tmp dir
(setq! backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq! auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; org-directory needs to be set early
(setq! org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(setq! inhibit-compacting-font-caches t)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; encryption : https://orgmode.org/worg/org-tutorials/encrypting-files.html
(progn
    (require 'epa-file)
    (epa-file-enable)
    (setq! epa-file-encrypt-to user-mail-address)
    (setq! epa-file-select-keys 'silent)
    (setq! epa-file-cache-passphrase-for-symmetric-encryption nil)

    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq! org-crypt-disable-auto-save nil)
    (setq! org-tags-exclude-from-inheritance (quote ("crypt")))
    (setq! org-crypt-key nil)
    (setq! org-crypt-key user-mail-address))

;; Defaults
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; Hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; Disable the scroll bar
(blink-cursor-mode -1) ; the blinking cursor is nothing, but an annoyance
(setq! visible-cursor nil) ; make it work in terminal too
(setq! delete-by-moving-to-trash t) ; Delete files to trash
(setq! window-combination-resize t) ; take new window space from all other windows (not just current)
(setq! delete-trailing-lines t) ; `M-x delete-trailing-whitespace' deletes trailing lines
(setq! x-stretch-cursor nil) ; Stretch cursor to the glyph width
(setq! undo-limit 80000000) ; Raise undo-limit to 80Mb
(setq! suggest-key-bindings nil) ; very annoying
(setq! auto-save-default t) ; Nobody likes to loose work, I certainly don't
(setq! password-cache-expiry nil) ; I can trust my computers ... can't I?
(setq! major-mode 'fundamental-mode)
(setq! use-dialog-box nil)
(setq! load-prefer-newer t)
(setq! truncate-lines t) ; Don't fold lines
(setq! truncate-partial-width-windows nil) ; for vertically-split windows
(setq! split-width-threshold 160) ; Split verticaly by default
(setq! uniquify-buffer-name-style 'forward) ; Uniquify buffer names
(setq! indent-tabs-mode t) ; use space to indent by default
(setq! large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
(setq! confirm-kill-processes nil) ; quit Emacs directly even if there are running processes
(setq! enable-local-eval t) ; Enable eval blocks in .dir-locals.el
(setq! enable-local-variables :all) ; Enable by default variables in .dir-locals.el
(setq! debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
(setq! completions-detailed t) ; Detailed description for the built in describe symbol etc
(setq! delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
(setq! save-place-mode 1) ; Remember point in files
(setq! global-so-long-mode 1) ; long files
(setq! kill-do-not-save-duplicates t) ; Do not save duplicates in kill-ring
(setq! search-whitespace-regexp ".*?") ; Isearch convenience, space matches anything (non-greedy)
(setq! next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
(setq! require-final-newline t) ; Newline at end of file
(setq! make-backup-files nil) ; Do not use backup files (filename~)
(setq! create-lockfiles nil) ; Do not use lock files (.#filename)
(setq! read-process-output-max (* 1024 1024)) ; Maximum number of bytes to read from subprocess in a single chunk.

;; tabs and indent
(setq! indent-tabs-mode nil) ; don't use tabs to indent
(setq! tab-width 4) ; but maintain correct appearance
(setq! indent-line-function 'insert-tab) ; indent the current line
(setq! standard-indent 4)
(setq! c-basic-offset  4) ; Base indent size when indented automatically
(c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
(c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
(c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
(c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent

;; Save history for commands
(savehist-mode)

;; scrolling
(setq! auto-window-vscroll nil)  ; fast scrolling
(setq! fast-but-imprecise-scrolling t)
(setq! scroll-margin 2)
(setq! scroll-conservatively 101)
(setq! scroll-preserve-screen-position t)
(when (fboundp 'pixel-scroll-precision-mode) (pixel-scroll-precision-mode t))

;; Use "y" and "n" to confirm/negate prompt
(if (boundp 'use-short-answers)
    (setq! use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; set appearance of a tab that is represented by 4 spaces
(setq! tab-width 4)
(setq! standard-indent 4)
(setq! highlight-tabs t)  ; show those ugly tabs

;; Whitespace settings
(setq! whitespace-line-column 132)
(setq! show-trailing-whitespace nil)
(setq! whitespace-action '(cleanup auto-cleanup))
(setq! whitespace-style '(indentation::space
                             space-after-tab
                             space-before-tab
                             trailing
                             lines-tail
                             tab-mark
                             face
                             tabs))

(setq! doc-view-continuous t)

;; LaTeX
(setq! font-latex-fontify-script nil)
(setq! TeX-newline-function 'reindent-then-newline-and-indent)

;; vc
(setq! vc-follow-symlinks t)
(setq! vc-handled-backends '(Git))

;; other defaults
(setq! sp-escape-quotes-after-insert nil)

;; I do not know what this is :)
(setq! max-specpdl-size 5000)
(setq! url-queue-timeout 30)

;; enable repeat-mode, see: `describe-repeat-maps'
(unless (version< emacs-version "28")
    (repeat-mode 1))

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; write over selected text on input... like all modern editors do
(delete-selection-mode t)

;; auto revert mode; automatically update a buffer if a file changes on disk
(setq! auto-revert-verbose t) ; show message when file changes
(setq! auto-revert-avoid-polling t) ; use save signal
(global-auto-revert-mode t)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq! world-clock-list '(("UTC" "UTC")
                             ("America/New_York" "Tampa")
                             ("Europe/Ljubljana" "Slovenia")
                             ("Asia/Calcutta" "India")
                             ("America/Havana" "Havana")))
(setq! world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(setq! line-spacing 0.1)

;; Toggle visualization of matching parens
(setq! show-paren-mode 1)

;; filling
(setq! fill-column 132)

;; hippie expand is dabbrev expand on steroids
(setq! hippie-expand-try-functions-list
    '(yas-hippie-try-expand
         try-expand-dabbrev
         try-expand-dabbrev-all-buffers
         try-expand-dabbrev-from-kill
         try-complete-file-name-partially
         try-complete-file-name
         try-expand-all-abbrevs
         try-expand-list
         try-expand-line
         try-complete-lisp-symbol-partially
         try-complete-lisp-symbol))

;; compilation buffer
(defun +my/bury-compile-buffer (buffer msg)
    "Bury compilation buffer if succeeded without warnings."
    (if (and
            (buffer-live-p buffer)
            (string-match "^finished" msg)
            (not (with-current-buffer buffer
                     (goto-char (point-min))
                     (search-forward "warning" nil t))))
        (progn
            (run-at-time "1 sec" nil 'delete-windows-on buffer)
            (message "Compilation Successful :-)"))
        (message "Compilation Failed :-("))
    (setq! current-frame (car (car (cdr (current-frame-configuration)))))
    (select-frame-set-input-focus current-frame))
(add-to-list 'compilation-finish-functions '+my/bury-compile-buffer)

;; macros
(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

;; Prevent killing scratch buffer
(with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

;; Prevent killing Messages buffer
(with-current-buffer "*Messages*"
    (emacs-lock-mode 'kill))

;; Auto-save on focus lost - https://www.emacswiki.org/emacs/AutoSave
(add-function :after after-focus-change-function
    (lambda () (unless (frame-focus-state) (save-some-buffers t))))

;; Hooks
(add-hook 'phyton-mode-hook 'whitespace-mode)
(add-hook 'makefile-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)    ;; auto-fill insert hard line breaks
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; ... visual-line-mode is much better
(add-hook 'prog-mode-hook '+my/comment-auto-fill)    ;; ... but add comment auto-fill in prog-mode

;; Minibuffer setup hook
(add-hook
    'minibuffer-setup-hook (lambda ()
                               (setq! show-trailing-whitespace nil)
                               (setq! line-spacing 1)))

;; trailing whitespace
(dolist (hook '(special-mode-hook
                   term-mode-hook
                   comint-mode-hook
                   compilation-mode-hook
                   minibuffer-setup-hook))
    (add-hook hook
        (lambda () (setq! show-trailing-whitespace nil))))

(provide 'my-init)
;;; my-init.el ends here
