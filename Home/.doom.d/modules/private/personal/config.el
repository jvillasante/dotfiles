;;; -*- lexical-binding: t; -*-

(when (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode))

;; OS types
(when IS-MAC
    (setq
        jv/dotfiles-path (file-truename "~/Hacking/workspace/dotfiles")
        jv/software-path (file-truename "~/Hacking/software")
        jv/dropbox-path (file-truename "~/Dropbox")
        jv/zsh-path "/usr/local/bin/zsh"
        jv/clang-path "/usr/local/opt/llvm/bin/clang")

    (setq
        browse-url-browser-function 'browse-url-generic
        engine/browser-function 'browse-url-generic
        browse-url-generic-program "open"))

(when IS-LINUX
    (setq
        jv/dotfiles-path (file-truename "~/Hacking/Software/dotfiles")
        jv/software-path (file-truename "~/Hacking/Software")
        jv/dropbox-path (file-truename "~/Dropbox")
        jv/zsh-path "/usr/bin/zsh"
        jv/clang-path "/usr/bin/clang")

    (executable-find "google-chrome"))

;; Minibuffer setup
(setq-hook! 'minibuffer-setup-hook
    show-trailing-whitespace nil
    ;; room for icons
    line-spacing 1)

;; seems slow, barely works
;; (global-auto-revert-mode)

(setq
    flycheck-check-syntax-automatically '(mode-enabled save))

;; https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
(setq display-time-world-list '(("UTC" "UTC")
                                   ("US/Eastern" "Miami")
                                   ("America/Havana" "Habana")
                                   ("America/New_York" "New York")
                                   ("Europe/Amsterdam" "Amsterdam")
                                   ("Europe/Copenhagen" "Denmark")
                                   ("Asia/Shanghai" "China")
                                   ("Asia/Calcutta" "India")))

;; recentf exclude folders/files
(setq recentf-exclude (list (concat jv/dotfiles-path "/.emacs.d")))

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

    ispell-program-name "aspell"
    auto-window-vscroll nil
    sp-escape-quotes-after-insert nil

    ;; I do not know what this is :)
    max-specpdl-size 5000
    url-queue-timeout 30)

;; Modules
(load! "+config-packages")
(load! "+ui")
(load! "+bindings")
(load! "+bindings-spacemacs")
(load! "+hooks")
