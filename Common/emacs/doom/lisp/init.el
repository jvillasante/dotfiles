;;; lisp/init.el -*- lexical-binding: t; -*-

(setq! user-full-name "Julio C. Villasante")
(setq! user-mail-address "jvillasantegomez@gmail.com")
(setq! user-login-name "jvillasante")
(setq! +my/home-path (expand-file-name "~/"))
(setq! +my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path))
(setq! +my/software-path (expand-file-name "Workspace/Software/" +my/home-path))
(setq! +my/dropbox-path (expand-file-name "Dropbox/" +my/home-path))
(setq! +my/splash-path (expand-file-name "Misc/splash/emacs-logo.png" +my/dotfiles-path))

(cond
    (IS-MAC
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
        (setq! mac-right-option-modifier 'alt))
    (IS-LINUX
        (setq! browse-url-browser-function 'browse-url-generic)
        (setq! browse-url-generic-program "xdg-open")
        (setq! +my/clang-path "/usr/bin/clang")
        (setq! +my/mu-path "/usr/bin/mu")
        (setq! +my/msmtp-path "/usr/bin/msmtp")
        (setq! vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")))

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; org-directory needs to be set early
(setq! org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(setq! inhibit-compacting-font-caches t)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; emacs does not need a pager
(setenv "PAGER" "cat")

;; encryption
(progn
    (require 'epa-file)
    (epa-file-enable)
    (setq! epa-file-encrypt-to user-mail-address)
    (setq! epa-file-select-keys 'silent)
    (setq! epa-file-cache-passphrase-for-symmetric-encryption nil))

;; aspell
(advice-add #'ispell-init-process :around #'doom-shut-up-a)
(setq! ispell-dictionary "en_US") ;; default dictionary

;; no comments on RET while writing comments
(setq! +default-want-RET-continue-comments nil)

;; Some default
(setq! delete-by-moving-to-trash t)      ; Delete files to trash
(setq! window-combination-resize t)      ; take new window space from all other windows (not just current)
(setq! delete-trailing-lines t)          ; `M-x delete-trailing-whitespace' deletes trailing lines
(setq! x-stretch-cursor nil)             ; Stretch cursor to the glyph width
(setq! undo-limit 80000000)             ; Raise undo-limit to 80Mb
(setq! suggest-key-bindings nil)        ; very annoying
(setq! auto-save-default t)             ; Nobody likes to loose work, I certainly don't
(setq! password-cache-expiry nil)       ; I can trust my computers ... can't I?
(setq! major-mode 'fundamental-mode)
(setq! use-dialog-box nil)
(setq! load-prefer-newer t)
(setq! truncate-lines t)                   ; Don't fold lines
(setq! truncate-partial-width-windows nil) ; for vertically-split windows
(setq! split-width-threshold 160)          ; Split verticaly by default
(setq! scroll-margin 3)
(setq! uniquify-buffer-name-style 'forward)   ; Uniquify buffer names
(setq! indent-tabs-mode t)                    ; use space to indent by default

;; set appearance of a tab that is represented by 4 spaces
(setq! tab-width 4)
(setq! standard-indent 4)
(setq! highlight-tabs t)  ; show those ugly tabs

;; Whitespace settings
(setq! show-trailing-whitespace t)
(setq! whitespace-action '(auto-cleanup))
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
(setq! auto-window-vscroll nil)
(setq! sp-escape-quotes-after-insert nil)

;; I do not know what this is :)
(setq! max-specpdl-size 5000)
(setq! url-queue-timeout 30)

;; Default Encoding
(prefer-coding-system 'utf-8)
(setq! locale-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq! buffer-file-coding-system 'utf-8) ; utf-8
(setq! save-buffer-coding-system 'utf-8) ; nil
(setq! process-coding-system-alist
    (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; write over selected text on input... like all modern editors do
(delete-selection-mode t)

;; no whitespace-mode, just enabled for makefiles
(advice-add #'doom-highlight-non-default-indentation-h :override #'ignore)

;; auto revert mode; automatically update a buffer if a file changes on disk
(global-auto-revert-mode t)

;; remove doom advice, I don't need deal with comments when newline
(advice-remove #'newline-and-indent #'doom*newline-indent-and-continue-comments)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq! world-clock-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")
         ("Asia/Calcutta" "India")
         ("America/Havana" "Havana")))
(setq! world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(setq! line-spacing 0.1)

;; Isearch
(progn
    (setq! search-whitespace-regexp ".*?") ;; Isearch convenience, space matches anything (non-greedy)
    (setq! isearch-lax-whitespace t)
    (setq! isearch-allow-motion t)) ;; Enable Emacs 28 isearch motions

;; Color compilation buffer
(require 'xterm-color)
(setq! compilation-environment '("TERM=xterm-256color"))
(defun +my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'+my/advice-compilation-filter)

;; Workaround for terminal buffer scroll
(setq! term-char-mode-point-at-process-mark nil)

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

;; compilation stuff
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
;; (add-to-list 'compilation-finish-functions '+my/bury-compile-buffer)

;; Add online search engines for +lookup/online
(add-to-list '+lookup-provider-url-alist '("cppreference" "https://en.cppreference.com/w/?search=%s"))

;; macros
(require 'kmacro)
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(define-key kmacro-keymap (kbd "I") #'kmacro-insert-macro)

;; Hooks
(add-hook 'phyton-mode-hook #'whitespace-mode)
(add-hook 'makefile-mode-hook #'whitespace-mode)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)    ;; auto-fill insert hard line breaks
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ;; ... visual-line-mode is much better
(add-hook 'prog-mode-hook '+my/comment-auto-fill)    ;; ... but add comment auto-fill in prog-mode

;; Minibuffer setup
(add-hook 'minibuffer-setup-hook (lambda ()
                                     (setq! show-trailing-whitespace nil)
                                     (setq! line-spacing 1)))
