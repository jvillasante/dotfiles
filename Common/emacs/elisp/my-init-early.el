;;; my-init-early.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; make IO better
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024))

;; everything is UTF8
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(require 'time)
(setq world-clock-list
      '(("UTC" "UTC")
        ("America/New_York" "Tampa")
        ("Europe/Ljubljana" "Slovenia")
        ("Europe/Paris" "France")))
(setq world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; input method to switch to when `toggle-input-method'
(setq default-input-method "spanish-postfix")

;; macros
(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

;; Make Emacs repeat the C-u C-SPC command (`set-mark-command') by
;; following it up with another C-SPC.  It is faster to type
;; C-u C-SPC, C-SPC, C-SPC, than C-u C-SPC, C-u C-SPC, C-u C-SPC...
(setq-default set-mark-command-repeat-pop t)

;; enable repeat-mode if available, see: `describe-repeat-maps'
(when (fboundp 'repeat-mode)
    (repeat-mode +1))

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
;; (setq lpr-command "lp")
;; (setq lpr-switches '("-d" "Brother_HL_L2370DW_series"))

;; Emacs28 dictionary lookup
(when (fboundp 'dictionary-lookup-definition)
    (defvar dictionary-server)
    (setq dictionary-server "dict.org"))

;; Some Defaults
(setq user-full-name "Julio C. Villasante"
      user-mail-address "jvillasantegomez@gmail.com"
      user-login-name "jvillasante")
(setq visible-cursor nil) ;; make it work in terminal too
(setq inhibit-startup-screen t) ; Hide the startup screen
(if (boundp 'use-short-answers) ;; Use "y" and "n" to confirm/negate prompt
        (setq use-short-answers t)
    (advice-add 'yes-or-no-p :override #'y-or-n-p))
(setq large-file-warning-threshold 100000000) ;; warn when opening files bigger than 100MB
(setq confirm-kill-processes nil) ;; quit Emacs directly even if there are running processes
(setq enable-local-eval t) ; Enable eval blocks in .dir-locals.el
(setq enable-local-variables :all) ; Enable by default variables in .dir-locals.el
(setq ring-bell-function 'ignore) ; Disable the bell for emacs
(setq debug-on-error nil) ; Display the stacktrace if error encountered in one of the lisp method
(setq completions-detailed t) ; Detailed description for the built in describe symbol etc
(column-number-mode t) ; Display column numbers in the status line
(size-indication-mode t) ; Display size indication
(setq show-paren-mode +1 ; Highlight the matching parenthessi
      show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)
(global-so-long-mode +1) ; long files
(setq kill-do-not-save-duplicates t) ; Do not save duplicates in kill-ring
(setq next-error-message-highlight t) ; When jumping between errors, occurs, etc, highlight the current line
(setq use-short-answers t) ; Abreviate Yes/No to y or n
(setq require-final-newline t) ;; Newline at end of file
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 4)            ;; but maintain correct appearance
(setq indent-line-function 'insert-tab) ;; indent the current line
(setq standard-indent 4)
(setq line-spacing 0.1) ;; line spacing
(setq completion-ignore-case nil)              ;; do NOT ignore case by default
(setq read-file-name-completion-ignore-case t) ;; ... but, ignore case when completing filenames
(setq read-buffer-completion-ignore-case t)    ;; ... and, ignore case whem completing buffers
(setq delete-by-moving-to-trash t) ;; use the system trash
(setq sentence-end-double-space nil) ;; Nobody ends sentences with double space!
(put 'narrow-to-region 'disabled nil) ; enable narrow to region (disabled by default)
(setq undo-limit 100000000) ; raise Emacs undo memory to 10 MB
(setq select-enable-clipboard t) ; Non-nil means cutting and pasting uses the clipboard.

;;; compile-angel : Natively compile all the things
(use-package compile-angel
    :demand t
    :custom
    (compile-angel-verbose nil)
    :config
    (compile-angel-on-load-mode)
    (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; s.el : The long lost Emacs string manipulation library.
(use-package s)

;; delsel : delete selection if you insert
(use-package delsel
    :ensure nil ; emacs built-in
    :hook (after-init . delete-selection-mode))

;; backups and auto-save
(require 'my-utils)
(use-package emacs
    :ensure nil ;; emacs built-in
    :config
    (progn ;; backups
        (setq create-lockfiles nil      ; avoid generating lockfiles
              make-backup-files t       ; backup of a file the first time it is saved.
              backup-by-copying t       ; don't clobber symlinks
              version-control t         ; version numbers for backup files
              delete-old-versions t     ; delete excess backup files silently
              kept-old-versions 5       ; oldest versions to keep when a new numbered backup is made (default: 2)
              kept-new-versions 5       ; newest versions to keep when a new numbered backup is made (default: 2)
              vc-make-backup-files nil) ; Do not backup version controlled files

        ;; backup all files
        (setq backup-directory-alist
              `(("." . ,(expand-file-name "backup" my/var-dir))))
        (setq tramp-backup-directory-alist backup-directory-alist)

        ;; ... do not backup some
        (setq backup-enable-predicate
              (lambda (name)
                  (and (normal-backup-enable-predicate name)
                       (not (s-starts-with? "/dev/shm" name))
                       (not (s-contains? "password-store" name))
                       (my/file-is-not-root-p name)))))

    (progn ;; autosave
        (setq auto-save-default t      ; auto-save every buffer that visits a file
              auto-save-timeout 20     ; number of seconds idle time before auto-save (default: 30)
              auto-save-interval 200)  ; number of keystrokes between auto-saves (default: 300)

        ;; Do not auto-disable auto-save after deleting large chunks of
        ;; text. The purpose of auto-save is to provide a fail-safe, and
        ;; disabling it contradicts this objective.
        (setq auto-save-include-big-deletions t)

        ;; Auto save options
        (setq kill-buffer-delete-auto-save-files t)

        ;; auto-save files
        ;; (setq auto-save-file-name-transforms
        ;;       `((".*" ,(expand-file-name "auto-save" my/var-dir) t)))
        (setq auto-save-list-file-prefix
              (expand-file-name "auto-save/" my/var-dir))
        (setq tramp-auto-save-directory
              (expand-file-name "tramp-autosave/" my/var-dir))

        ;; disable auto-save on certain tramp profiles
        (connection-local-set-profile-variables
         'no-remote-auto-save-profile
         '((buffer-auto-save-file-name . nil)
           (remote-file-name-inhibit-auto-save-visited . t)
           (remote-file-name-inhibit-auto-save . t)))

        ;; disable auto-save for specific protocols
        (dolist (protocol '("sudo" "doas" "su" "sudoedit" "ssh"))
            (connection-local-set-profiles
             `(:application tramp :protocol ,protocol 'no-remote-auto-save-profile))))

    (progn ;; bookmarks
        (setq bookmark-default-file
              (expand-file-name "bookmarks.el" my/var-dir))
        (setq bookmark-save-flag 1)
        (setq bookmark-set-fringe-mark nil))

    (progn ;; make `save-some-buffers` show diff on demand
        (add-to-list 'save-some-buffers-action-alist
                     (list "d"
                           (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                           "show diff between the buffer and its file")))

    (when my/os-mac
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (setq browse-url-browser-function 'browse-url-generic)
        (setq browse-url-generic-program "open")
        (setq ns-use-proxy-icon nil)
        (setq ns-use-thin-smoothing t)
        (setq ns-alternate-modifier nil)
        (setq mac-command-key-is-meta nil
              mac-command-modifier 'super
              mac-option-key-is-meta t
              mac-option-modifier 'meta)
        (setq default-input-method "MacOSX")
        (defconst my/clang-path "/usr/local/opt/llvm/bin/clang")
        (defconst my/mu-path "/usr/local/bin/mu")
        (defconst my/msmtp-path "/usr/local/bin/msmtp")

        ;; Use spotlight search backend as a default for M-x locate (and helm/ivy
        ;; variants thereof), since it requires no additional setup.
        (setq locate-command "mdfind"
              ;; Visit files opened outside of Emacs in existing frame, not a new one
              ns-pop-up-frames nil))

    (when my/os-linux
        (setq x-super-keysym 'meta)
        (setq browse-url-browser-function 'browse-url-generic)
        (setq browse-url-generic-program "xdg-open")
        (defconst my/clang-path "/usr/bin/clang")
        (defconst my/mu-path "/usr/bin/mu")
        (defconst my/msmtp-path "/usr/bin/msmtp")))

(provide 'my-init-early)
;;; my-init-early.el ends here
