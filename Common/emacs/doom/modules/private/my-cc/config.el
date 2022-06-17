;;; private/my-cc/config.el -*- lexical-binding: t; -*-

(after! cc
    (setq-default flycheck-c/c++-clang-executable +my/clang-path)
    (setq-default flycheck-clang-standard-library "libc++")
    (setq-default flycheck-clang-language-standard "c++20"))

(after! ccls
    (unless (executable-find "ccls")
        (setq ccls-executable (expand-file-name "ccls/Release/ccls" +my/software-path)))

    (setq ccls-initialization-options
        `(:cache (:directory ,(expand-file-name ".emacs.d/.local/.cache/lsp-ccls" +my/dotfiles-path)))))
