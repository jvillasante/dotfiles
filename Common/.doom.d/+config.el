;;; +config.el -*- lexical-binding: t; -*-

(cond
    (IS-MAC
        (setq
            browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "open"
            ns-use-thin-smoothing t)
        (setq
            ns-alternate-modifier     nil
            mac-command-modifier      'meta
            mac-option-modifier       'alt
            mac-right-option-modifier 'alt)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (add-to-list 'default-frame-alist '(ns-appearance . dark)))
    (IS-LINUX
        (executable-find "firefox")))

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (pushnew! default-frame-alist '(undecorated . t) '(fullscreen . maximized))

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

;; encryption
(require 'epa-file)
(progn
    (epa-file-enable)
    (setq
        epa-file-select-keys nil
        epa-file-cache-passphrase-for-symmetric-encryption t))

;; Minibuffer setup
(setq-hook! 'minibuffer-setup-hook
    show-trailing-whitespace nil
    line-spacing 1)

;; use aspell
(advice-add #'ispell-init-process :around #'doom-shut-up-a)

;; Don't Autosave files with default Emacs package (we'll use super-save package instead)
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

(setq undo-limit 80000000            ; Raise undo-limit to 80Mb
    evil-want-fine-undo t            ; By default while in insert all changes are one big blob. Be more granular
    auto-save-default t)             ; Nobody likes to loose work, I certainly don't

;; (global-subword-mode 1)              ; Iterate through CamelCase words

;; shell
(setq-default
    multi-term-program +my/zsh-path
    shell-file-name +my/zsh-path)

;; frame title
(setq frame-title-format
    '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs"))

;; defaults
(setq
    major-mode 'text-mode
    use-dialog-box nil
    vc-follow-symlinks t
    load-prefer-newer t
    truncate-lines t                   ; Don't fold lines
    truncate-partial-width-windows nil ; for vertically-split windows
    split-width-threshold 160          ; Split verticaly by default
    evil-cross-lines t                 ; Make horizontal movement cross lines
    scroll-margin 3
    uniquify-buffer-name-style 'forward   ; Uniquify buffer names
    indent-tabs-mode nil                  ; use space to indent by default

    ;; set appearance of a tab that is represented by 4 spaces
    evil-shift-width 4
    tab-width 4
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
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-locale-environment "en_US.UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8) ; included by set-selection-coding-system
(set-keyboard-coding-system 'utf-8) ; configured by prefer-coding-system
(set-terminal-coding-system 'utf-8) ; configured by prefer-coding-system
(setq buffer-file-coding-system 'utf-8) ; utf-8-unix
(setq save-buffer-coding-system 'utf-8-unix) ; nil
(setq process-coding-system-alist
    (cons '("grep" utf-8 . utf-8) process-coding-system-alist))

;; write over selected text on input... like all modern editors do
(delete-selection-mode t)

;; no whitespace-mode, just enabled for makefiles
(advice-add #'doom-highlight-non-default-indentation-h :override #'ignore)

;; ???
(global-auto-revert-mode t)

;; remove doom advice, I don't need deal with comments when newline
(advice-remove #'newline-and-indent #'doom*newline-indent-and-continue-comments)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq display-time-world-list
    '(("UTC" "UTC")
         ("America/New_York" "Tampa")
         ("Europe/Ljubljana" "Slovenia")
         ("Asia/Calcutta" "India")
         ("America/Havana" "Havana")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;; line spacing
(setq-default line-spacing 0.1)

;; Isearch convenience, space matches anything (non-greedy)
(setq search-whitespace-regexp ".*?")

;; Color compilation buffer
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun +my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'+my/advice-compilation-filter)

;; comment auto-fill
(defun +my/comment-auto-fill ()
      (setq-local comment-auto-fill-only-comments t)
      (auto-fill-mode 1))

;; Hooks
(add-hook! text-mode #'turn-on-auto-fill)
(add-hook! prog-mode #'+my/comment-auto-fill)
(add-hook! phyton-mode #'whitespace-mode)
(add-hook! makefile-mode #'whitespace-mode)
(add-hook 'compilation-finish-functions #'+my/bury-compile-buffer-if-successful)
;; (remove-hook 'compilation-finish-functions #'+my/bury-compile-buffer-if-successful)
(add-hook! markdown-mode
    (toggle-word-wrap nil)
    (auto-fill-mode -1))

;; No highligh persisten on evil search
(setq evil-ex-search-persistent-highlight nil)

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; Workaround for terminal buffer scroll
(setq term-char-mode-point-at-process-mark nil)

;; ???
(show-paren-mode 1)

;; Add online search engines for +lookup/online
(add-to-list '+lookup-provider-url-alist '("cppreference" "https://en.cppreference.com/w/?search=%s"))
