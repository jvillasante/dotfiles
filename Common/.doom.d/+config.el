;;; +config.el -*- lexical-binding: t; -*-

(cond
    (IS-MAC
        (setq
            browse-url-browser-function 'browse-url-generic
            engine/browser-function 'browse-url-generic
            browse-url-generic-program "open")

        (setq ns-use-thin-smoothing t)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (add-to-list 'default-frame-alist '(ns-appearance . dark)))
    (IS-LINUX
        (executable-find "firefox")))

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (pushnew! default-frame-alist '(undecorated . t) '(fullscreen . maximized))

;; Minibuffer setup
(setq-hook! 'minibuffer-setup-hook
    show-trailing-whitespace nil
    line-spacing 1)

;; No more 'Starting new Ispell process aspell with default dictionary...done'
(advice-add #'ispell-init-process :around #'doom-shut-up-a)

;; Some default configs
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

    ;; scroll
    scroll-margin 3

    ;; use space to indent by default
    indent-tabs-mode nil

    ;; set appearance of a tab that is represented by 4 spaces
    evil-shift-width 4
    tab-width 4

    ;; show those ugly tabs
    highlight-tabs t

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

    ;; tramp mode
    tramp-default-method "ssh"

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

;; ???
(global-auto-revert-mode t)

;; remove doom advice, I don't need deal with comments when newline
(advice-remove #'newline-and-indent #'doom*newline-indent-and-continue-comments)

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq display-time-world-list '(("UTC" "UTC")
                                   ("America/New_York" "Tampa")
                                   ("Europe/Ljubljana" "Slovenia")
                                   ("Asia/Calcutta" "India")
                                   ("America/Havana" "Havana")))

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
(add-hook 'focus-out-hook
    (lambda ()
        (save-some-buffers t)))
(add-hook 'prog-mode-hook #'goto-address-mode) ;; Linkify links!
(add-hook 'prog-mode-hook
    (lambda ()
        (set-fill-column 110)
        (flyspell-prog-mode)))
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'makefile-mode-hook 'whitespace-mode)
;; (remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)
;; (remove-hook 'rust-mode-hook 'adaptive-wrap-prefix-mode)
(add-hook 'compilation-finish-functions '+my/bury-compile-buffer-if-successful)
;; (remove-hook 'compilation-finish-functions '+my/bury-compile-buffer-if-successful)
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
;; (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;; (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; Workaround for terminal buffer scroll
(setq term-char-mode-point-at-process-mark nil)

;; ???
(show-paren-mode 1)

;; popup rules
;; (set-popup-rules!
;;   '((\"^ \\*\" :slot 1 :vslot -1 :size #'+popup-shrink-to-fit)
;;     (\"^\\*\"  :slot 1 :vslot -1 :select t))
;;   '((\"^\\*Completions\" :slot -1 :vslot -2 :ttl 0)
;;     (\"^\\*Compil\\(?:ation\\|e-Log\\)\" :size 0.3 :ttl 0 :quit t)))"
