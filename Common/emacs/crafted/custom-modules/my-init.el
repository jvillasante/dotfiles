;;; my-init.el --- Custom Init Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; Init Customizations

;;; Code:

(csetq user-full-name "Julio C. Villasante")
(csetq user-mail-address "jvillasantegomez@gmail.com")
(csetq user-login-name "jvillasante")
(csetq +my/home-path (expand-file-name "~/"))
(csetq +my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path))
(csetq +my/software-path (expand-file-name "Workspace/Software/" +my/home-path))
(csetq +my/dropbox-path (expand-file-name "Dropbox/" +my/home-path))

(cond
 ((string-equal system-type "windows-nt")
  (progn
    (message "Emacs Running on Microsoft Windows (which is super weird!)")))
 ((string-equal system-type "darwin")
  (progn
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (csetq browse-url-browser-function 'browse-url-generic)
    (csetq browse-url-generic-program "open")
    (csetq +my/clang-path "/usr/local/opt/llvm/bin/clang")
    (csetq +my/mu-path "/usr/local/bin/mu")
    (csetq +my/msmtp-path "/usr/local/bin/msmtp")
    (csetq vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
    (csetq ns-use-proxy-icon nil)
    (csetq ns-use-thin-smoothing t)
    (csetq ns-alternate-modifier nil)
    (csetq mac-command-modifier 'meta)
    (csetq mac-option-modifier 'alt)
    (csetq mac-right-option-modifier 'alt)))
 ((string-equal system-type "gnu/linux")
  (progn
    (csetq browse-url-browser-function 'browse-url-generic)
    (csetq browse-url-generic-program "xdg-open")
    (csetq +my/clang-path "/usr/bin/clang")
    (csetq +my/mu-path "/usr/bin/mu")
    (csetq +my/msmtp-path "/usr/bin/msmtp")
    (csetq vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes"))))

;; store all backup and autosave files in the tmp dir
(csetq backup-directory-alist `((".*" . ,temporary-file-directory)))
(csetq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Add system-wide mu4e installation
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

;; org-directory needs to be set early
(csetq org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(csetq inhibit-compacting-font-caches t)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; encryption : https://orgmode.org/worg/org-tutorials/encrypting-files.html
(progn
  (require 'epa-file)
  (epa-file-enable)
  (csetq epa-file-encrypt-to user-mail-address)
  (csetq epa-file-select-keys 'silent)
  (csetq epa-file-cache-passphrase-for-symmetric-encryption nil)

  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (csetq org-crypt-disable-auto-save nil)
  (csetq org-tags-exclude-from-inheritance (quote ("crypt")))
  (csetq org-crypt-key nil)
  (csetq org-crypt-key user-mail-address))

;; Defaults
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable the toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; Hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; Disable the scroll bar
(blink-cursor-mode -1) ; the blinking cursor is nothing, but an annoyance
(csetq visible-cursor nil) ; make it work in terminal too
(csetq delete-by-moving-to-trash t) ; Delete files to trash
(csetq window-combination-resize t) ; take new window space from all other windows (not just current)
(csetq delete-trailing-lines t) ; `M-x delete-trailing-whitespace' deletes trailing lines
(csetq x-stretch-cursor nil) ; Stretch cursor to the glyph width
(csetq undo-limit 80000000) ; Raise undo-limit to 80Mb
(csetq suggest-key-bindings nil) ; very annoying
(csetq auto-save-default t) ; Nobody likes to loose work, I certainly don't
(csetq password-cache-expiry nil) ; I can trust my computers ... can't I?
(csetq major-mode 'fundamental-mode)
(csetq use-dialog-box nil)
(csetq load-prefer-newer t)
(csetq truncate-lines t) ; Don't fold lines
(csetq truncate-partial-width-windows nil) ; for vertically-split windows
(csetq split-width-threshold 160) ; Split verticaly by default
(csetq uniquify-buffer-name-style 'forward) ; Uniquify buffer names
(csetq indent-tabs-mode t) ; use space to indent by default
(csetq large-file-warning-threshold 100000000) ; warn when opening files bigger than 100MB
(csetq confirm-kill-processes nil) ; quit Emacs directly even if there are running processes
(csetq enable-local-eval t) ; Enable eval blocks in .dir-locals.el
(csetq enable-local-variables :all) ; Enable by default variables in .dir-locals.el
(csetq debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
(csetq completions-detailed t) ; Detailed description for the built in describe symbol etc
(csetq delete-selection-mode 1) ; If text is selected, we expect that typing will replace the selection
(csetq save-place-mode 1) ; Remember point in files
(csetq global-so-long-mode 1) ; long files
(csetq kill-do-not-save-duplicates t) ; Do not save duplicates in kill-ring
(csetq search-whitespace-regexp ".*?") ; Isearch convenience, space matches anything (non-greedy)
(csetq next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
(csetq require-final-newline t) ; Newline at end of file
(csetq make-backup-files nil) ; Do not use backup files (filename~)
(csetq create-lockfiles nil) ; Do not use lock files (.#filename)
(csetq read-process-output-max (* 1024 1024)) ; Maximum number of bytes to read from subprocess in a single chunk.

;; tabs and indent
(csetq indent-tabs-mode nil) ; don't use tabs to indent
(csetq tab-width 4) ; but maintain correct appearance
(csetq indent-line-function 'insert-tab) ; indent the current line
(csetq standard-indent 4)
(csetq c-basic-offset  4) ; Base indent size when indented automatically
(c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
(c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
(c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
(c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent

;; Save history for commands
(savehist-mode)

;; scrolling
(csetq auto-window-vscroll nil)  ; fast scrolling
(csetq fast-but-imprecise-scrolling t)
(csetq scroll-margin 2)
(csetq scroll-conservatively 101)
(csetq scroll-preserve-screen-position t)
(when (fboundp 'pixel-scroll-precision-mode) (pixel-scroll-precision-mode t))

;; Use "y" and "n" to confirm/negate prompt
(if (boundp 'use-short-answers)
    (csetq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; set appearance of a tab that is represented by 4 spaces
(csetq tab-width 4)
(csetq standard-indent 4)
(csetq highlight-tabs t)  ; show those ugly tabs

;; Whitespace settings
(csetq whitespace-line-column 132)
(csetq show-trailing-whitespace nil)
(csetq whitespace-action '(cleanup auto-cleanup))
(csetq whitespace-style '(indentation::space
                          space-after-tab
                          space-before-tab
                          trailing
                          lines-tail
                          tab-mark
                          face
                          tabs))

(csetq doc-view-continuous t)

;; LaTeX
(csetq font-latex-fontify-script nil)
(csetq TeX-newline-function 'reindent-then-newline-and-indent)

;; vc
(csetq vc-follow-symlinks t)
(csetq vc-handled-backends '(Git))

;; other defaults
(csetq sp-escape-quotes-after-insert nil)

;; I do not know what this is :)
(csetq max-specpdl-size 5000)
(csetq url-queue-timeout 30)

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
(csetq auto-revert-verbose t) ; show message when file changes
(csetq auto-revert-avoid-polling t) ; use save signal
(global-auto-revert-mode t)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(csetq world-clock-list '(("UTC" "UTC")
                          ("America/New_York" "Tampa")
                          ("Europe/Ljubljana" "Slovenia")
                          ("Asia/Calcutta" "India")
                          ("America/Havana" "Havana")))
(csetq world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(csetq line-spacing 0.1)

;; Toggle visualization of matching parens
(csetq show-paren-mode 1)

;; filling
(csetq fill-column 132)

;; hippie expand is dabbrev expand on steroids
(csetq hippie-expand-try-functions-list
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
  (csetq current-frame (car (car (cdr (current-frame-configuration)))))
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
                          (csetq show-trailing-whitespace nil)
                          (csetq line-spacing 1)))

;; trailing whitespace
(dolist (hook '(special-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook))
  (add-hook hook
            (lambda () (csetq show-trailing-whitespace nil))))

(provide 'my-init)
;;; my-init.el ends here
