;;; my-init-early.el -*- lexical-binding: t; -*-

;; some defaults
(setq mac-right-option-modifier 'meta)
(setq mac-option-modifier 'meta)
(setq warning-minimum-level :error)

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq world-clock-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")))
(setq world-clock-time-format "%a, %d %b %I:%M %p %Z")

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

;; macros
(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

;; the mark ring
(setq-default set-mark-command-repeat-pop t)

;; enable repeat-mode if available, see: `describe-repeat-maps'
(when (fboundp 'repeat-mode)
    (repeat-mode +1))

;; enable pixel-scroll-precision if available
(when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode +1))

;; Don't compact font caches during GC.
(setq inhibit-compacting-font-caches t)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; emacs does not need a pager
(setenv "PAGER" "cat")

;; Prevent killing Messages buffer
(with-current-buffer "*Messages*"
    (emacs-lock-mode 'kill))

;; Prevent killing scratch buffer
(with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

;; Printer
(setq printer-name "Brother_HL_L2370DW_series")

;; Emacs28 dictionary lookup
(when (fboundp 'dictionary-lookup-definition)
    (setq dictionary-server "dict.org"))

;; Some Defaults
(setq user-full-name "Julio C. Villasante"
    user-mail-address "jvillasantegomez@gmail.com"
    user-login-name "jvillasante")
(setq scroll-margin 3
    scroll-step 1
    scroll-conservatively 10000
    auto-window-vscroll nil)
(setq load-prefer-newer t) ;; Always load newest byte code
(setq visible-cursor nil) ;; make it work in terminal too
(setq inhibit-startup-screen t) ; Hide the startup screen
(if (boundp 'use-short-answers) ;; Use "y" and "n" to confirm/negate prompt
    (setq use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p))
(setq large-file-warning-threshold 100000000) ;; warn when opening files bigger than 100MB
(setq confirm-kill-processes nil) ;; quit Emacs directly even if there are running processes
(savehist-mode +1) ; Save history for commands
(setq-default auto-revert-verbose t) ; show message when file changes
(setq-default auto-revert-avoid-polling t) ; use save signal
(global-auto-revert-mode t) ; Refresh files automatically when modified from outside emacs
(setq enable-local-eval t) ; Enable eval blocks in .dir-locals.el
(setq enable-local-variables :all) ; Enable by default variables in .dir-locals.el
(setq ring-bell-function 'ignore) ; Disable the bell for emacs
(setq debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
(setq completions-detailed t) ; Detailed description for the built in describe symbol etc
(column-number-mode t) ; Display column numbers in the status line
(size-indication-mode t) ; Display size indication
(delete-selection-mode +1) ; If text is selected, we expect that typing will replace the selection
(save-place-mode +1) ; Remember point in files
(electric-pair-mode +1) ; auto-insert matching parenteses
(show-paren-mode +1) ; Highlight the matching parenthesis
(global-so-long-mode +1) ; long files
(setq kill-do-not-save-duplicates t) ; Do not save duplicates in kill-ring
(setq next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
(setq use-short-answers t) ; Abreviate Yes/No to y or n
(setq require-final-newline t) ;; Newline at end of file
(setq-default fill-column 132) ;; Wrap lines at 132 characters
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance
(setq indent-line-function 'insert-tab) ;; indent the current line
(setq standard-indent 4)
(setq line-spacing 0.1) ;; line spacing
(setq completion-ignore-case nil)              ;; do NOT ignore case by default
(setq read-file-name-completion-ignore-case t) ;; ... but, ignore case when completing filenames
(setq read-buffer-completion-ignore-case t)    ;; ... and, ignore case whem completing buffers
(setq delete-by-moving-to-trash t) ;; use the system trash

;; backups
(setq make-backup-files t         ; backup of a file the first time it is saved.
    backup-by-copying t           ; don't clobber symlinks
    version-control t             ; version numbers for backup files
    delete-old-versions t         ; delete excess backup files silently
    kept-old-versions 2           ; oldest versions to keep when a new numbered backup is made (default: 2)
    kept-new-versions 6)          ; newest versions to keep when a new numbered backup is made (default: 2)

;; backup all files
(setq backup-directory-alist
    `(("." . ,(no-littering-expand-var-file-name "backup/"))))

;; ... do not backup some
(setq backup-enable-predicate
    (lambda (name)
        (and (normal-backup-enable-predicate name)
            (not (s-starts-with? "/dev/shm" name))
            (not (s-contains? "password-store" name))
            (my/file-is-not-root-p name))))

;; autosave
(setq auto-save-default t         ; auto-save every buffer that visits a file
    auto-save-timeout 20          ; number of seconds idle time before auto-save (default: 30)
    auto-save-interval 200)       ; number of keystrokes between auto-saves (default: 300)
(auto-save-visited-mode +1)

;; auto-save files
(setq auto-save-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; disable auto-save on certain tramp profiles
(connection-local-set-profile-variables
    'no-remote-auto-save-profile
    '((buffer-auto-save-file-name . nil)
         (remote-file-name-inhibit-auto-save-visited . t)
         (remote-file-name-inhibit-auto-save . t)))

;; disable auto-save for specific protocols
(dolist (protocol '("sudo" "doas" "su" "sudoedit" "ssh"))
    (connection-local-set-profiles
        `(:application tramp :protocol ,protocol 'no-remote-auto-save-profile)))

;; lock files
(setq lock-file-name-transforms
    `((".*" ,(no-littering-expand-var-file-name "lock-file/") t)))

(when IS-MAC
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (setq browse-url-browser-function 'browse-url-generic)
    (setq browse-url-generic-program "open")
    (setq my/clang-path "/usr/local/opt/llvm/bin/clang")
    (setq my/mu-path "/usr/local/bin/mu")
    (setq my/msmtp-path "/usr/local/bin/msmtp")
    (setq vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
    (setq ns-use-proxy-icon nil)
    (setq ns-use-thin-smoothing t)
    (setq ns-alternate-modifier nil)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'alt)
    (setq mac-right-option-modifier 'alt)

    ;; Use spotlight search backend as a default for M-x locate (and helm/ivy
    ;; variants thereof), since it requires no additional setup.
    (setq locate-command "mdfind"
        ;; Visit files opened outside of Emacs in existing frame, not a new one
        ns-pop-up-frames nil))

(when IS-LINUX
    (setq browse-url-browser-function 'browse-url-generic)
    (setq browse-url-generic-program "xdg-open")
    (setq my/clang-path "/usr/bin/clang")
    (setq my/mu-path "/usr/bin/mu")
    (setq my/msmtp-path "/usr/bin/msmtp")
    (setq vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes"))

(add-hook 'tty-setup-hook #'my/tty-setup)

(provide 'my-init-early)
;;; my-init-basics ends here
