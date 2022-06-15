;;; personal/preload/init.el --- Prelude personal preload init -*- lexical-binding: t; -*-
;;;

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq prelude-theme nil)
(setq prelude-super-keybindings nil)
(setq prelude-whitespace nil)

(setq user-full-name "Julio C. Villasante"
    user-mail-address "jvillasantegomez@gmail.com"
    user-login-name "jvillasante"
    +my/home-path (expand-file-name "~/")
    +my/dotfiles-path (expand-file-name "Workspace/Public/dotfiles/" +my/home-path)
    +my/software-path (expand-file-name "Workspace/Software/" +my/home-path)
    +my/dropbox-path (expand-file-name "Dropbox/" +my/home-path)
    +my/splash-path (expand-file-name "Misc/splash/emacs-logo.png" +my/dotfiles-path))

(cond
    (IS-MAC
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "open")
        (setq +my/zsh-path "/usr/local/bin/zsh"
            +my/clang-path "/usr/local/opt/llvm/bin/clang"
            +my/mu-path "/usr/local/bin/mu"
            +my/msmtp-path "/usr/local/bin/msmtp"
            vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")
        (setq ns-use-proxy-icon         nil
            ns-use-thin-smoothing     t
            ns-alternate-modifier     nil
            mac-command-modifier      'meta
            mac-option-modifier       'alt
            mac-right-option-modifier 'alt))
    (IS-LINUX
        (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "xdg-open")
        (setq +my/zsh-path "/usr/bin/zsh"
            +my/clang-path "/usr/bin/clang"
            +my/mu-path "/usr/bin/mu"
            +my/msmtp-path "/usr/bin/msmtp"
            vterm-module-cmake-args " -DUSE_SYSTEM_LIBVTERM=yes")))

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; org-directory needs to be set early
(setq org-directory (expand-file-name "Apps/org" +my/dropbox-path))

;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)
