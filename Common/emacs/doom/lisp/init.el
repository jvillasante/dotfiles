;;; lisp/init.el -*- lexical-binding: t; -*-

(customize-set-variable 'user-full-name "Julio C. Villasante")
(customize-set-variable 'user-mail-address "jvillasantegomez@gmail.com")
(customize-set-variable 'user-login-name "jvillasante")
(customize-set-variable '+my/home-path (expand-file-name "~/"))
(customize-set-variable '+my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path))
(customize-set-variable '+my/software-path (expand-file-name "Workspace/Software/" +my/home-path))
(customize-set-variable '+my/dropbox-path (expand-file-name "Dropbox/" +my/home-path))
(customize-set-variable '+my/splash-path (expand-file-name "Misc/splash/emacs-logo.png" +my/dotfiles-path))

(cond
    (IS-MAC
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (customize-set-variable 'browse-url-browser-function 'browse-url-generic)
        (customize-set-variable 'browse-url-generic-program "open")
        (customize-set-variable '+my/clang-path "/usr/local/opt/llvm/bin/clang")
        (customize-set-variable '+my/mu-path "/usr/local/bin/mu")
        (customize-set-variable '+my/msmtp-path "/usr/local/bin/msmtp")
        (customize-set-variable 'vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
        (customize-set-variable 'ns-use-proxy-icon nil)
        (customize-set-variable ns-use-thin-smoothing     t)
        (customize-set-variable ns-alternate-modifier     nil)
        (customize-set-variable mac-command-modifier      'meta)
        (customize-set-variable mac-option-modifier       'alt)
        (customize-set-variable mac-right-option-modifier 'alt))
    (IS-LINUX
        (customize-set-variable 'browse-url-browser-function 'browse-url-generic)
        (customize-set-variable 'browse-url-generic-program "xdg-open")
        (customize-set-variable '+my/clang-path "/usr/bin/clang")
        (customize-set-variable '+my/mu-path "/usr/bin/mu")
        (customize-set-variable '+my/msmtp-path "/usr/bin/msmtp")
        (customize-set-variable 'vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")))

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; org-directory needs to be set early
(customize-set-variable 'org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(customize-set-variable 'inhibit-compacting-font-caches t)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; emacs does not need a pager
(setenv "PAGER" "cat")

;; encryption
(progn
    (require 'epa-file)
    (epa-file-enable)
    (customize-set-variable 'epa-file-encrypt-to user-mail-address)
    (customize-set-variable 'epa-file-select-keys 'silent)
    (customize-set-variable 'epa-file-cache-passphrase-for-symmetric-encryption nil))

;; aspell
(advice-add #'ispell-init-process :around #'doom-shut-up-a)
(customize-set-variable 'ispell-dictionary "en_US") ;; default dictionary

;; no comments on RET while writing comments
(customize-set-variable '+default-want-RET-continue-comments nil)

;; Some default
;; TODO
(customize-set-variable 'delete-by-moving-to-trash t)      ; Delete files to trash
(customize-set-variable 'window-combination-resize t)      ; take new window space from all other windows (not just current)
(customize-set-variable 'delete-trailing-lines t)          ; `M-x delete-trailing-whitespace' deletes trailing lines
(customize-set-variable 'x-stretch-cursor nil)             ; Stretch cursor to the glyph width
(customize-set-variable 'undo-limit 80000000)             ; Raise undo-limit to 80Mb
(customize-set-variable 'suggest-key-bindings nil)        ; very annoying
(customize-set-variable 'auto-save-default t)             ; Nobody likes to loose work, I certainly don't
(customize-set-variable 'password-cache-expiry nil)       ; I can trust my computers ... can't I?
(customize-set-variable 'major-mode 'fundamental-mode)
(customize-set-variable 'use-dialog-box nil)
(customize-set-variable 'load-prefer-newer t)
(customize-set-variable 'truncate-lines t)                   ; Don't fold lines
(customize-set-variable 'truncate-partial-width-windows nil) ; for vertically-split windows
(customize-set-variable 'split-width-threshold 160)          ; Split verticaly by default
(customize-set-variable 'scroll-margin 3)
(customize-set-variable 'uniquify-buffer-name-style 'forward)   ; Uniquify buffer names
(customize-set-variable 'indent-tabs-mode t)                    ; use space to indent by default

;; set appearance of a tab that is represented by 4 spaces
(customize-set-variable 'tab-width 4)
(customize-set-variable 'standard-indent 4)
(customize-set-variable 'highlight-tabs t)  ; show those ugly tabs

;; Whitespace settings
(customize-set-variable 'show-trailing-whitespace t)
(customize-set-variable 'whitespace-action '(auto-cleanup))
(customize-set-variable 'whitespace-style '(indentation::space
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
(customize-set-variable 'auto-window-vscroll nil)
(customize-set-variable 'sp-escape-quotes-after-insert nil)

;; I do not know what this is :)
(customize-set-variable 'max-specpdl-size 5000)
(customize-set-variable 'url-queue-timeout 30)

;; Default Encoding
(prefer-coding-system 'utf-8)
(customize-set-variable 'locale-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(customize-set-variable 'buffer-file-coding-system 'utf-8) ; utf-8
(customize-set-variable 'save-buffer-coding-system 'utf-8) ; nil
(customize-set-variable 'process-coding-system-alist
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
(customize-set-variable 'world-clock-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")
         ("Asia/Calcutta" "India")
         ("America/Havana" "Havana")))
(customize-set-variable 'world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(customize-set-variable 'line-spacing 0.1)

;; Isearch
(progn
    (customize-set-variable 'search-whitespace-regexp ".*?") ;; Isearch convenience, space matches anything (non-greedy)
    (customize-set-variable 'isearch-lax-whitespace t)
    (customize-set-variable 'isearch-allow-motion t)) ;; Enable Emacs 28 isearch motions

;; Color compilation buffer
(require 'xterm-color)
(customize-set-variable 'compilation-environment '("TERM=xterm-256color"))
(defun +my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'+my/advice-compilation-filter)

;; Workaround for terminal buffer scroll
(customize-set-variable 'term-char-mode-point-at-process-mark nil)

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
    (customize-set-variable 'current-frame (car (car (cdr (current-frame-configuration)))))
    (select-frame-set-input-focus current-frame))
(add-to-list 'compilation-finish-functions '+my/bury-compile-buffer)

;; Add online search engines for +lookup/online
(add-to-list '+lookup-provider-url-alist '("cppreference" "https://en.cppreference.com/w/?search=%s"))

;; Add system-wide mu4e installation
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

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
(add-hook
    'minibuffer-setup-hook (lambda ()
                               (customize-set-variable 'show-trailing-whitespace nil)
                               (customize-set-variable 'line-spacing 1)))
