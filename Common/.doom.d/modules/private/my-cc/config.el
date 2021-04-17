;;; private/my-cc/config.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(add-hook! c-mode-common
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'innamespace 0) ;; Do not indent namespaces.
    (c-set-offset 'arglist-intro '+) ;; indent function args properly
    (c-set-offset 'arglist-cont-nonempty '+)
    (c-toggle-hungry-state 1)          ;; use hungry delete.
    (setq c-basic-offset tab-width)
    (setq c-default-style "stroustrup"))

(after! company
    (setq company-clang-executable +my/clang-path))

(after! cc-mode
    (add-hook! c++-mode
        (progn
            (setq-default flycheck-c/c++-clang-executable +my/clang-path)
            (setq-default flycheck-clang-standard-library "libc++")
            (setq-default flycheck-clang-language-standard "c++20")
            (setq company-clang-arguments '("-std=c++20")))))

(use-package! modern-cpp-font-lock
    :config
    (modern-c++-font-lock-global-mode t))

(after! ccls
    (unless (executable-find "ccls")
        (setq ccls-executable (expand-file-name "ccls/Release/ccls" +my/software-path)))

    (setq ccls-initialization-options
        `(:cache (:directory ,(expand-file-name ".emacs.d/.local/.cache/lsp-ccls" +my/dotfiles-path)))))
