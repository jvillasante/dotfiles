;;; +early-init.el -*- lexical-binding: t; -*-

(setq
    user-full-name "Julio C. Villasante"
    user-mail-address "jvillasantegomez@gmail.com"
    user-login-name "jvillasante")

(setq
    +my/home-path (expand-file-name "~/")
    +my/dotfiles-path (expand-file-name "Workspace/Others/dotfiles/" +my/home-path)
    +my/software-path (expand-file-name "Workspace/Software/" +my/home-path)
    +my/dropbox-path (expand-file-name "Dropbox/" +my/home-path))

(cond
    (IS-MAC
        (setq
            +my/zsh-path "/usr/local/bin/zsh"
            +my/clang-path "/usr/local/opt/llvm/bin/clang"
            vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes"))
    (IS-LINUX
        (setq
            +my/zsh-path "/usr/bin/zsh"
            +my/clang-path "/usr/bin/clang"
            vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=no")))

(if (and (fboundp 'native-comp-available-p)
        (native-comp-available-p))
    (progn
        (message "Native compilation is available")
        (setq comp-deferred-compilation t)
        (setq comp-async-report-warnings-errors nil))
    (message "Native compilation is *not* available"))

(if (functionp 'json-serialize)
    (message "Native JSON is available")
    (message "Native JSON is *not* available"))

;;
;; (when (string= (system-name) "your.ubuntu.host")
;;   (color-theme-initialize))
