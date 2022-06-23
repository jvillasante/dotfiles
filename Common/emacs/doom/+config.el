;;; +config.el -*- lexical-binding: t; -*-

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; frame title
(setq-default frame-title-format
    '(:eval
         (format "%s@%s: %s"
             (or (file-remote-p default-directory 'user)
                 user-real-login-name)
             (or (file-remote-p default-directory 'host)
                 system-name)
             (cond
                 (buffer-file-truename
                     (concat buffer-file-truename))
                 (dired-directory
                     (concat dired-directory))
                 (t (buffer-name))))))

;; encryption
(require 'epa-file)
(progn
    (epa-file-enable)
    (setq epa-file-encrypt-to user-mail-address
        epa-file-select-keys 'silent
        epa-file-cache-passphrase-for-symmetric-encryption nil))

;; Minibuffer setup
(setq-hook! 'minibuffer-setup-hook
    show-trailing-whitespace nil
    line-spacing 1)

;; aspell
(advice-add #'ispell-init-process :around #'doom-shut-up-a)
(setq ispell-dictionary "en_US") ;; default dictionary

;; Don't autosave files with default Emacs package (we'll use super-save package instead)
(setq auto-save-default nil)

;;
;; Some default
;;

(setq-default
    delete-by-moving-to-trash t      ; Delete files to trash
    window-combination-resize t      ; take new window space from all other windows (not just current)
    x-stretch-cursor nil)            ; Stretch cursor to the glyph width

(setq-default
    fill-column 120
    delete-trailing-lines t)

(setq
    undo-limit 80000000               ; Raise undo-limit to 80Mb
    suggest-key-bindings nil          ; very annoying
    auto-save-default t               ; Nobody likes to loose work, I certainly don't
    password-cache-expiry nil)        ; I can trust my computers ... can't I?

;; defaults
(setq
    major-mode 'fundamental-mode
    use-dialog-box nil
    vc-follow-symlinks t
    load-prefer-newer t
    truncate-lines t                   ; Don't fold lines
    truncate-partial-width-windows nil ; for vertically-split windows
    split-width-threshold 160          ; Split verticaly by default
    scroll-margin 3
    uniquify-buffer-name-style 'forward   ; Uniquify buffer names
    indent-tabs-mode nil                  ; use space to indent by default

    ;; set appearance of a tab that is represented by 4 spaces
    tab-width 4
    standard-indent 4
    highlight-tabs t  ; show those ugly tabs

    ;; Whitespace settings
    show-trailing-whitespace t
    whitespace-action '(auto-cleanup)
    whitespace-style '(indentation::space
                          space-after-tab
                          space-before-tab
                          trailing
                          lines-tail
                          tab-mark
                          face
                          tabs)

    doc-view-continuous t

    ;; LaTeX
    font-latex-fontify-script nil
    TeX-newline-function 'reindent-then-newline-and-indent

    ;; other defaults
    auto-window-vscroll nil
    sp-escape-quotes-after-insert nil

    ;; I do not know what this is :)
    max-specpdl-size 5000
    url-queue-timeout 30)

;; Default Encoding
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8
(setq save-buffer-coding-system 'utf-8) ; nil
(setq process-coding-system-alist
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
(setq world-clock-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")
         ("Asia/Calcutta" "India")
         ("America/Havana" "Havana")))
(setq world-clock-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(setq-default line-spacing 0.1)

;; Isearch
(progn
    (setq search-whitespace-regexp ".*?") ;; Isearch convenience, space matches anything (non-greedy)
    (setq isearch-lax-whitespace t)
    (setq isearch-allow-motion t)) ;; Enable Emacs 28 isearch motions

;; Color compilation buffer
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun +my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'+my/advice-compilation-filter)

;; Workaround for terminal buffer scroll
(setq term-char-mode-point-at-process-mark nil)

;; Toggle visualization of matching parens
(setq show-paren-mode 1)

;; no auto-fill-mode
(turn-off-auto-fill)

;; Add online search engines for +lookup/online
(add-to-list '+lookup-provider-url-alist '("cppreference" "https://en.cppreference.com/w/?search=%s"))

;; Hooks
(add-hook 'phyton-mode-hook #'whitespace-mode)
(add-hook 'makefile-mode-hook #'whitespace-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
;; (add-hook 'compilation-finish-functions #'+my/bury-compile-buffer-if-successful)
