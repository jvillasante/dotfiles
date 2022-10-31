;;; my-init.el --- Custom Init Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Init Customizations

;;; Code:

(customize-set-variable 'user-full-name "Julio C. Villasante")
(customize-set-variable 'user-mail-address "jvillasantegomez@gmail.com")
(customize-set-variable 'user-login-name "jvillasante")
(customize-set-variable '+my/home-path (expand-file-name "~/"))
(customize-set-variable '+my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path))
(customize-set-variable '+my/software-path (expand-file-name "Workspace/Software/" +my/home-path))
(customize-set-variable '+my/dropbox-path (expand-file-name "Dropbox/" +my/home-path))

(cond
    ((string-equal system-type "windows-nt")
        (progn
            (message "Emacs Running on Microsoft Windows (which is super weird!)")))
    ((string-equal system-type "darwin")
        (progn
            (message "Emacs Running on MacOS")
            (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
            (customize-set-variable 'browse-url-browser-function 'browse-url-generic)
            (customize-set-variable 'browse-url-generic-program "open")
            (customize-set-variable '+my/clang-path "/usr/local/opt/llvm/bin/clang")
            (customize-set-variable '+my/mu-path "/usr/local/bin/mu")
            (customize-set-variable '+my/msmtp-path "/usr/local/bin/msmtp")
            (customize-set-variable 'vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
            (customize-set-variable 'ns-use-proxy-icon nil)
            (customize-set-variable 'ns-use-thin-smoothing t)
            (customize-set-variable 'ns-alternate-modifier nil)
            (customize-set-variable 'mac-command-modifier 'meta)
            (customize-set-variable 'mac-option-modifier 'alt)
            (customize-set-variable 'mac-right-option-modifier 'alt)))
    ((string-equal system-type "gnu/linux")
        (progn
            (message "Emacs Running in Linux")
            (customize-set-variable 'browse-url-browser-function 'browse-url-generic)
            (customize-set-variable 'browse-url-generic-program "xdg-open")
            (customize-set-variable '+my/clang-path "/usr/bin/clang")
            (customize-set-variable '+my/mu-path "/usr/bin/mu")
            (customize-set-variable '+my/msmtp-path "/usr/bin/msmtp")
            (customize-set-variable 'vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes"))))

;; store all backup and autosave files in the tmp dir
(customize-set-variable 'backup-directory-alist `((".*" . ,temporary-file-directory)))
(customize-set-variable 'auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Add system-wide mu4e installation
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; org-directory needs to be set early
(customize-set-variable 'org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(customize-set-variable 'inhibit-compacting-font-caches t)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; emacs does not need a pager
(setenv "PAGER" "cat")

;; encryption : https://orgmode.org/worg/org-tutorials/encrypting-files.html
(progn
    (require 'epa-file)
    (epa-file-enable)
    (customize-set-variable 'epa-file-encrypt-to user-mail-address)
    (customize-set-variable 'epa-file-select-keys 'silent)
    (customize-set-variable 'epa-file-cache-passphrase-for-symmetric-encryption nil)

    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (customize-set-variable 'org-crypt-disable-auto-save nil)
    (customize-set-variable 'org-tags-exclude-from-inheritance (quote ("crypt")))
    (customize-set-variable 'org-crypt-key nil)
    (customize-set-variable 'org-crypt-key user-mail-address))

;; Defaults
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; Hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; Disable the scroll bar
(blink-cursor-mode -1) ; the blinking cursor is nothing, but an annoyance
(customize-set-variable 'visible-cursor nil) ; make it work in terminal too
(customize-set-variable 'delete-by-moving-to-trash t) ; Delete files to trash
(customize-set-variable 'window-combination-resize t) ; take new window space from all other windows (not just current)
(customize-set-variable 'delete-trailing-lines t) ; `M-x delete-trailing-whitespace' deletes trailing lines
(customize-set-variable 'x-stretch-cursor nil) ; Stretch cursor to the glyph width
(customize-set-variable 'undo-limit 80000000) ; Raise undo-limit to 80Mb
(customize-set-variable 'suggest-key-bindings nil) ; very annoying
(customize-set-variable 'auto-save-default t) ; Nobody likes to loose work, I certainly don't
(customize-set-variable 'password-cache-expiry nil) ; I can trust my computers ... can't I?
(customize-set-variable 'major-mode 'fundamental-mode)
(customize-set-variable 'use-dialog-box nil)
(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'truncate-lines t) ; Don't fold lines
(customize-set-variable 'truncate-partial-width-windows nil) ; for vertically-split windows
(customize-set-variable 'split-width-threshold 160) ; Split verticaly by default
(customize-set-variable 'uniquify-buffer-name-style 'forward) ; Uniquify buffer names
(customize-set-variable 'indent-tabs-mode t) ; use space to indent by default
(customize-set-variable 'large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
(customize-set-variable 'confirm-kill-processes nil) ; quit Emacs directly even if there are running processes
(customize-set-variable 'enable-local-eval t) ; Enable eval blocks in .dir-locals.el
(customize-set-variable 'enable-local-variables :all) ; Enable by default variables in .dir-locals.el
(customize-set-variable 'debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
(customize-set-variable 'completions-detailed t) ; Detailed description for the built in describe symbol etc
(delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
(save-place-mode +1) ; Remember point in files
;; (electric-pair-mode +1) ; auto-insert matching parenteses
;; (show-paren-mode +1) ; Highlight the matching parenthesis
(global-so-long-mode +1) ; long files
(customize-set-variable 'kill-do-not-save-duplicates t) ; Do not save duplicates in kill-ring
(customize-set-variable 'search-whitespace-regexp ".*?") ; Isearch convenience, space matches anything (non-greedy)
(customize-set-variable 'next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
(customize-set-variable 'require-final-newline t) ; Newline at end of file
(customize-set-variable 'make-backup-files nil) ; Do not use backup files (filename~)
(customize-set-variable 'create-lockfiles nil) ; Do not use lock files (.#filename)

;; tabs and indent
(customize-set-variable 'indent-tabs-mode nil) ; don't use tabs to indent
(customize-set-variable 'tab-width 4) ; but maintain correct appearance
(customize-set-variable 'indent-line-function 'insert-tab) ; indent the current line
(customize-set-variable 'standard-indent 4)
(customize-set-variable 'c-basic-offset  4) ; Base indent size when indented automatically
(c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
(c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
(c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
(c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent

;; Save history for commands
(savehist-mode)

;; scrolling
(customize-set-variable 'auto-window-vscroll nil)  ; fast scrolling
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-margin 2)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-preserve-screen-position t)
(when (fboundp 'pixel-scroll-precision-mode) (pixel-scroll-precision-mode t))

;; Use "y" and "n" to confirm/negate prompt
(if (boundp 'use-short-answers)
    (customize-set-variable 'use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; set appearance of a tab that is represented by 4 spaces
(customize-set-variable 'tab-width 4)
(customize-set-variable 'standard-indent 4)
(customize-set-variable 'highlight-tabs t)  ; show those ugly tabs

;; Whitespace settings
(customize-set-variable 'whitespace-line-column 132)
(customize-set-variable 'show-trailing-whitespace nil)
(customize-set-variable 'whitespace-action '(auto-cleanup))
(customize-set-variable 'whitespace-style
    '(indentation::space
         space-after-tab
         space-before-tab
         trailing
         lines-tail
         tab-mark
         face
         tabs))

(customize-set-variable 'doc-view-continuous t)

;; LaTeX
(customize-set-variable 'font-latex-fontify-script nil)
(customize-set-variable 'TeX-newline-function 'reindent-then-newline-and-indent)

;; vc
(customize-set-variable 'vc-follow-symlinks t)
(customize-set-variable 'vc-handled-backends '(Git))

;; other defaults
(customize-set-variable 'sp-escape-quotes-after-insert nil)

;; I do not know what this is :)
(customize-set-variable 'max-specpdl-size 5000)
(customize-set-variable 'url-queue-timeout 30)

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
(customize-set-variable 'auto-revert-verbose t) ; show message when file changes
(customize-set-variable 'auto-revert-avoid-polling t) ; use save signal
(global-auto-revert-mode t)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(customize-set-variable 'world-clock-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")
         ("Asia/Calcutta" "India")
         ("America/Havana" "Havana")))
(customize-set-variable 'world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(customize-set-variable 'line-spacing 0.1)

;; Toggle visualization of matching parens
(customize-set-variable 'show-paren-mode 1)

;; filling
(customize-set-variable 'fill-column 132)

;; hippie expand is dabbrev expand on steroids
(customize-set-variable 'hippie-expand-try-functions-list
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
    (customize-set-variable 'current-frame (car (car (cdr (current-frame-configuration)))))
    (select-frame-set-input-focus current-frame))
(add-to-list 'compilation-finish-functions '+my/bury-compile-buffer)

;; macros
(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

;; Prevent killing scratch buffer
(with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

;; Auto-save on focus lost - https://www.emacswiki.org/emacs/AutoSave
(add-function :after after-focus-change-function
    (lambda () (unless (frame-focus-state) (save-some-buffers t))))

;; Hooks
(add-hook 'phyton-mode-hook #'whitespace-mode)
(add-hook 'makefile-mode-hook #'whitespace-mode)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)    ;; auto-fill insert hard line breaks
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; ... visual-line-mode is much better
(add-hook 'prog-mode-hook '+my/comment-auto-fill)    ;; ... but add comment auto-fill in prog-mode

;; Minibuffer setup hook
(add-hook
    'minibuffer-setup-hook (lambda ()
                               (customize-set-variable 'show-trailing-whitespace nil)
                               (customize-set-variable 'line-spacing 1)))

;; trailing whitespace
(dolist (hook '(special-mode-hook
                   term-mode-hook
                   comint-mode-hook
                   compilation-mode-hook
                   minibuffer-setup-hook))
    (add-hook hook
        (lambda () (customize-set-variable 'show-trailing-whitespace nil))))

(provide 'my-init)
;;; my-init.el ends here
