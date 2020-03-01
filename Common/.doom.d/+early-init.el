;;; +early-init.el -*- lexical-binding: t; -*-

(setq
    user-full-name "Julio C. Villasante"
    user-mail-address "jvillasantegomez@gmail.com")

(setq
    +my/home-path (file-truename "~")
    +my/dotfiles-path (file-truename "~/Workspace/Others/dotfiles")
    +my/software-path (file-truename "~/Workspace/Software")
    +my/dropbox-path (file-truename "~/Dropbox"))

(cond
    (IS-MAC
        (setq
            +my/zsh-path "/usr/local/bin/zsh"
            +my/clang-path "/usr/local/opt/llvm/bin/clang"))
    (IS-LINUX
        (setq
            +my/zsh-path "/usr/bin/zsh"
            +my/clang-path "/usr/bin/clang")))
