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
                jv/dotfiles-path (file-truename "~/Hacking/workspace/dotfiles")
                jv/software-path (file-truename "~/Hacking/software")
                jv/dropbox-path (file-truename "~/Dropbox")
                jv/zsh-path "/usr/local/bin/zsh"
                jv/clang-path "/usr/local/opt/llvm/bin/clang")

            (setq
                browse-url-browser-function 'browse-url-generic
                engine/browser-function 'browse-url-generic
                browse-url-generic-program "open")))
    ((string-equal system-type "gnu/linux") ; linux
        (progn
            (setq
                jv/dotfiles-path (file-truename "~/Hacking/Software/dotfiles")
                jv/software-path (file-truename "~/Hacking/Software")
                jv/dropbox-path (file-truename "~/Dropbox")
                jv/zsh-path "/usr/bin/zsh"
                jv/clang-path "/usr/bin/clang")

            (executable-find "google-chrome"))))

(global-auto-revert-mode t)

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

;; I do not know what this is :)
(setq max-specpdl-size 5000)
(setf url-queue-timeout 30))

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
(load! "+spacemacs-bindings")
(load! "+ui")
(load! "+packages_custom")
