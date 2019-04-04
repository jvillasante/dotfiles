;;; -*- lexical-binding: t; -*-

(when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))

;; check OS type
(cond
    ((string-equal system-type "windows-nt") ; Microsoft Windows
        (progn
            (message "Microsoft Windows Not Supported!!!!")))
    ((string-equal system-type "darwin") ; Mac OS X
        (progn
            (setq
                jv/dropbox-path "~/Dropbox"
                jv/zsh-path "/usr/local/bin/zsh"
                jv/clang-path "/usr/local/opt/llvm/bin/clang")

            (setq
                browse-url-browser-function 'browse-url-generic
                engine/browser-function 'browse-url-generic
                browse-url-generic-program "open")))
    ((string-equal system-type "gnu/linux") ; linux
        (progn
            (setq
                jv/dropbox-path "~/Dropbox"
                jv/zsh-path "/usr/bin/zsh"
                jv/clang-path "/usr/bin/clang")

            (executable-find "google-chrome"))))

;; Some setq-defaults
(setq-default
    user-full-name "Julio C. Villasante"
    user-mail-address "jvillasantegomez@gmail.com"

    major-mode 'text-mode
    use-dialog-box nil
    vc-follow-symlinks t
    load-prefer-newer t
    fill-column 110                    ; Maximum line width
    truncate-lines t                   ; Don't fold lines
    truncate-partial-width-windows nil ; for vertically-split windows
    split-width-threshold 160          ; Split verticaly by default
    evil-cross-lines t                 ; Make horizontal movement cross lines

    ;; scroll
    scroll-margin 3

    ;; my coding style, bsd with 4 spaces indentation (and no tab characters)
    evil-shift-width 4
    tab-width 4
    indent-tabs-mode nil

    ;; Whitespace settings
    whitespace-action '(auto-cleanup)
    whitespace-style '(indentation::space
                          space-after-tab
                          space-before-tab
                          trailing
                          lines-tail
                          tab-mark
                          face
                          tabs)
    ;; tramp mode
    tramp-default-method "ssh")

;; Some more defaults
(setq
    ;; No highligh persisten on evil search
    evil-ex-search-persistent-highlight nil

    flycheck-check-syntax-automatically '(mode-enabled save)

    ;; recentf exclude folders/files
    recentf-exclude '("~/Hacking/workspace/dotfiles/.emacs.d")

    ispell-program-name "aspell"
    auto-window-vscroll nil
    sp-escape-quotes-after-insert nil

    ;; I do not know what this is :)
    max-specpdl-size 5000
    url-queue-timeout 30)

;; Load snippets
;; (after! yasnippet
;;     (push (expand-file-name "snippets/" doom-private-dir) yas-snippet-dirs))

;; Modules
(load! "+ui")      ;; My ui mods. Also contains ligature stuff.
