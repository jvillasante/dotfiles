;;; my-init.el --- Custom Init Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Init Customizations

;;; Code:

(setc user-full-name "Julio C. Villasante")
(setc user-mail-address "jvillasantegomez@gmail.com")
(setc user-login-name "jvillasante")
(setc +my/home-path (expand-file-name "~/"))
(setc +my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path))
(setc +my/software-path (expand-file-name "Workspace/Software/" +my/home-path))
(setc +my/dropbox-path (expand-file-name "Dropbox/" +my/home-path))

(cond
    ((string-equal system-type "windows-nt")
        (progn
            (message "Emacs Running on Microsoft Windows (which is super weird!)")))
    ((string-equal system-type "darwin")
        (progn
            (message "Emacs Running on MacOS")
            (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
            (setc browse-url-browser-function 'browse-url-generic)
            (setc browse-url-generic-program "open")
            (setc +my/clang-path "/usr/local/opt/llvm/bin/clang")
            (setc +my/mu-path "/usr/local/bin/mu")
            (setc +my/msmtp-path "/usr/local/bin/msmtp")
            (setc vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
            (setc ns-use-proxy-icon nil)
            (setc ns-use-thin-smoothing t)
            (setc ns-alternate-modifier nil)
            (setc mac-command-modifier 'meta)
            (setc mac-option-modifier 'alt)
            (setc mac-right-option-modifier 'alt)))
    ((string-equal system-type "gnu/linux")
        (progn
            (message "Emacs Running in Linux")
            (setc browse-url-browser-function 'browse-url-generic)
            (setc browse-url-generic-program "xdg-open")
            (setc +my/clang-path "/usr/bin/clang")
            (setc +my/mu-path "/usr/bin/mu")
            (setc +my/msmtp-path "/usr/bin/msmtp")
            (setc vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes"))))

;; store all backup and autosave files in the tmp dir
(setc backup-directory-alist `((".*" . ,temporary-file-directory)))
(setc auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Add system-wide mu4e installation
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; org-directory needs to be set early
(setc org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(setc inhibit-compacting-font-caches t)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; emacs does not need a pager
(setenv "PAGER" "cat")

;; encryption : https://orgmode.org/worg/org-tutorials/encrypting-files.html
(progn
    (require 'epa-file)
    (epa-file-enable)
    (setc epa-file-encrypt-to user-mail-address)
    (setc epa-file-select-keys 'silent)
    (setc epa-file-cache-passphrase-for-symmetric-encryption nil)

    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setc org-crypt-disable-auto-save nil)
    (setc org-tags-exclude-from-inheritance (quote ("crypt")))
    (setc org-crypt-key nil)
    (setc org-crypt-key user-mail-address))

;; Defaults
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; Hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; Disable the scroll bar
(blink-cursor-mode -1) ; the blinking cursor is nothing, but an annoyance
(setc visible-cursor nil) ; make it work in terminal too
(setc delete-by-moving-to-trash t) ; Delete files to trash
(setc window-combination-resize t) ; take new window space from all other windows (not just current)
(setc delete-trailing-lines t) ; `M-x delete-trailing-whitespace' deletes trailing lines
(setc x-stretch-cursor nil) ; Stretch cursor to the glyph width
(setc undo-limit 80000000) ; Raise undo-limit to 80Mb
(setc suggest-key-bindings nil) ; very annoying
(setc auto-save-default t) ; Nobody likes to loose work, I certainly don't
(setc password-cache-expiry nil) ; I can trust my computers ... can't I?
(setc major-mode 'fundamental-mode)
(setc use-dialog-box nil)
(setc load-prefer-newer t)
(setc truncate-lines t) ; Don't fold lines
(setc truncate-partial-width-windows nil) ; for vertically-split windows
(setc split-width-threshold 160) ; Split verticaly by default
(setc uniquify-buffer-name-style 'forward) ; Uniquify buffer names
(setc indent-tabs-mode t) ; use space to indent by default
(setc large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
(setc confirm-kill-processes nil) ; quit Emacs directly even if there are running processes
(setc enable-local-eval t) ; Enable eval blocks in .dir-locals.el
(setc enable-local-variables :all) ; Enable by default variables in .dir-locals.el
(setc debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
(setc completions-detailed t) ; Detailed description for the built in describe symbol etc
(delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
(save-place-mode +1) ; Remember point in files
(electric-pair-mode +1) ; auto-insert matching parenteses
(show-paren-mode +1) ; Highlight the matching parenthesis
(global-so-long-mode +1) ; long files
(setc kill-do-not-save-duplicates t) ; Do not save duplicates in kill-ring
(setc search-whitespace-regexp ".*?") ; Isearch convenience, space matches anything (non-greedy)
(setc next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
(setc require-final-newline t) ; Newline at end of file
(setc make-backup-files nil) ; Do not use backup files (filename~)
(setc create-lockfiles nil) ; Do not use lock files (.#filename)

;; tabs and indent
(setc indent-tabs-mode nil) ; don't use tabs to indent
(setc tab-width 4) ; but maintain correct appearance
(setc indent-line-function 'insert-tab) ; indent the current line
(setc standard-indent 4)
(setc c-basic-offset  4) ; Base indent size when indented automatically
(c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
(c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
(c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
(c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent

;; Save history for commands
(savehist-mode)

;; scrolling
(setc auto-window-vscroll nil)  ; fast scrolling
(setc fast-but-imprecise-scrolling t)
(setc scroll-margin 2)
(setc scroll-conservatively 101)
(setc scroll-preserve-screen-position t)
(when (fboundp 'pixel-scroll-precision-mode) (pixel-scroll-precision-mode t))

;; Use "y" and "n" to confirm/negate prompt
(if (boundp 'use-short-answers)
    (setc use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; set appearance of a tab that is represented by 4 spaces
(setc tab-width 4)
(setc standard-indent 4)
(setc highlight-tabs t)  ; show those ugly tabs

;; Whitespace settings
(setc show-trailing-whitespace t)
(setc whitespace-action '(auto-cleanup))
(setc whitespace-style '(indentation::space
                            space-after-tab
                            space-before-tab
                            trailing
                            lines-tail
                            tab-mark
                            face
                            tabs))

(setc doc-view-continuous t)

;; LaTeX
(setc font-latex-fontify-script nil)
(setc TeX-newline-function 'reindent-then-newline-and-indent)

;; vc
(setc vc-follow-symlinks t)
(setc vc-handled-backends '(Git))

;; other defaults
(setc sp-escape-quotes-after-insert nil)

;; I do not know what this is :)
(setc max-specpdl-size 5000)
(setc url-queue-timeout 30)

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
(setc auto-revert-verbose t) ; show message when file changes
(setc auto-revert-avoid-polling t) ; use save signal
(global-auto-revert-mode t)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setc world-clock-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")
         ("Asia/Calcutta" "India")
         ("America/Havana" "Havana")))
(setc world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(setc line-spacing 0.1)

;; Toggle visualization of matching parens
(setc show-paren-mode 1)

;; filling
(setc fill-column 132)

;; hippie expand is dabbrev expand on steroids
(setc hippie-expand-try-functions-list
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
    (setc current-frame (car (car (cdr (current-frame-configuration)))))
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
                               (setc show-trailing-whitespace nil)
                               (setc line-spacing 1)))

;; trailing whitespace
(dolist (hook '(special-mode-hook
                   term-mode-hook
                   comint-mode-hook
                   compilation-mode-hook
                   minibuffer-setup-hook))
    (add-hook hook
        (lambda () (setc show-trailing-whitespace nil))))

(provide 'my-init)
;;; my-init.el ends here
