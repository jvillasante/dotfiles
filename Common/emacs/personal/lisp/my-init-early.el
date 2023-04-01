;;; my-init-early.el -*- lexical-binding: t; -*-

;; ???
(setq mac-right-option-modifier 'meta)
(setq mac-option-modifier 'meta)
(setq warning-minimum-level :error)

;; try to disable the auto backup behavior
;; (setq make-backup-files nil
;;       create-lockfiles nil
;;       auto-save-default nil)
;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq world-clock-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")
         ("America/Havana" "Havana")))
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

;; Auto-save on focus lost - https://www.emacswiki.org/emacs/AutoSave
(add-function :after after-focus-change-function
    (lambda () (unless (frame-focus-state) (save-some-buffers t))))

;; Prevent killing scratch buffer
(with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

;; Printer
(setq printer-name "Brother_HL_L2370DW_series")

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
(savehist-mode) ; Save history for commands
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
(setq-default c-basic-offset  4) ; Base indent size when indented automatically
(c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
(c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
(c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
(c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
(setq make-backup-files nil) ; Do not use backup files (filename~)
(setq create-lockfiles nil) ; Do not use lock files (.#filename)
(setq line-spacing 0.1) ;; line spacing
(setq completion-ignore-case t)                ;; ignore case
(setq read-file-name-completion-ignore-case t) ;; ignore case when completing filenames
(setq read-buffer-completion-ignore-case t)    ;; ignore case whem completing buffers

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

;;; save & shutdown when we get an "end of session" signal on dbus
(progn
    (require 'dbus)
    (defun my/register-signals (client-path)
        "Register for the 'QueryEndSession' and 'EndSession' signals from
Gnome SessionManager.

When we receive 'QueryEndSession', we just respond with
'EndSessionResponse(true, \"\")'.  When we receive 'EndSession', we
append this EndSessionResponse to kill-emacs-hook, and then call
kill-emacs.  This way, we can shut down the Emacs daemon cleanly
before we send our 'ok' to the SessionManager."
        (setq my/gnome-client-path client-path)
        (let ( (end-session-response (lambda (&optional arg)
                                         (dbus-call-method-asynchronously
                                             :session "org.gnome.SessionManager" my/gnome-client-path
                                             "org.gnome.SessionManager.ClientPrivate" "EndSessionResponse" nil
                                             t "") ) ) )
            (dbus-register-signal
                :session "org.gnome.SessionManager" my/gnome-client-path
                "org.gnome.SessionManager.ClientPrivate" "QueryEndSession"
                end-session-response )
            (dbus-register-signal
                :session "org.gnome.SessionManager" my/gnome-client-path
                "org.gnome.SessionManager.ClientPrivate" "EndSession"
                `(lambda (arg)
                     (add-hook 'kill-emacs-hook ,end-session-response t)
                     (kill-emacs) ) ) ) )

    ;; DESKTOP_AUTOSTART_ID is set by the Gnome desktop manager when emacs
    ;; is autostarted.  We can use it to register as a client with gnome
    ;; SessionManager.
    (dbus-call-method-asynchronously
        :session "org.gnome.SessionManager"
        "/org/gnome/SessionManager"
        "org.gnome.SessionManager" "RegisterClient" 'my/register-signals
        "Emacs server" (getenv "DESKTOP_AUTOSTART_ID")))

(provide 'my-init-early)
;;; my-init-basics ends here
