;;; +config.el -*- lexical-binding: t; -*-

(cond
    (IS-MAC
        (setq
            browse-url-browser-function 'browse-url-generic
            engine/browser-function 'browse-url-generic
            browse-url-generic-program "open"
            ns-use-thin-smoothing t)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (add-to-list 'default-frame-alist '(ns-appearance . dark)))
    (IS-LINUX
        (executable-find "firefox")))

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (pushnew! default-frame-alist '(undecorated . t) '(fullscreen . maximized))

;; encryption
(require 'epa-file)
(epa-file-enable)

;; Minibuffer setup
(setq-hook! 'minibuffer-setup-hook
    show-trailing-whitespace nil
    line-spacing 1)

;; No more 'Starting new Ispell process aspell with default dictionary...done'
(advice-add #'ispell-init-process :around #'doom-shut-up-a)

;;
;; Some default
;;

(setq-default
    delete-by-moving-to-trash t      ; Delete files to trash
    window-combination-resize t      ; take new window space from all other windows (not just current)
    x-stretch-cursor nil)              ; Stretch cursor to the glyph width

(setq undo-limit 80000000            ; Raise undo-limit to 80Mb
    evil-want-fine-undo t            ; By default while in insert all changes are one big blob. Be more granular
    auto-save-default t)             ; Nobody likes to loose work, I certainly don't

(global-subword-mode 1)              ; Iterate through CamelCase words

(setq
    major-mode 'text-mode
    use-dialog-box nil
    vc-follow-symlinks t
    load-prefer-newer t
    fill-column 110                    ; Maximum line width
    truncate-lines t                   ; Don't fold lines
    truncate-partial-width-windows nil ; for vertically-split windows
    split-width-threshold 160          ; Split verticaly by default
    evil-cross-lines t                 ; Make horizontal movement cross lines
    frame-title-format "%f"
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
    ispell-program-name "aspell"
    ispell-dictionary "english"
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

;; multiterm
(setq multi-term-program +my/zsh-path)

;; line spacing
(setq-default line-spacing 0.1)

;; Isearch convenience, space matches anything (non-greedy)
(setq search-whitespace-regexp ".*?")

;; Hooks
(add-hook 'term-mode-hook
    (lambda ()
        (setq term-buffer-maximum-size 10000)))
(add-hook 'prog-mode-hook #'goto-address-mode) ;; Linkify links!
(add-hook 'prog-mode-hook
    (lambda ()
        (set-fill-column 110)
        (flyspell-prog-mode)))
(add-hook 'text-mode-hook #'turn-on-flyspell)
(add-hook 'phyton-mode-hook #'whitespace-mode)
(add-hook 'makefile-mode-hook #'whitespace-mode)
(add-hook 'compilation-finish-functions #'+my/bury-compile-buffer-if-successful)
;; (remove-hook 'compilation-finish-functions #'+my/bury-compile-buffer-if-successful)
(add-hook! 'markdown-mode-hook
    (progn
        (toggle-word-wrap nil)
        (auto-fill-mode -1)))
(add-hook 'ibuffer-hook
    (lambda ()
        (ibuffer-vc-set-filter-groups-by-vc-root)
        (unless (eq ibuffer-sorting-mode 'alphabetic)
            (ibuffer-do-sort-by-alphabetic))))

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
