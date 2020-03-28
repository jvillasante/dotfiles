;;; private/my-cc/config.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))

(defun +my/c-mode-common-hook ()
    (setq
        c-default-style "stroustrup"
        c-basic-offset 4
        c-basic-indent 4)
    (c-set-offset 'substatement-open 0))
(add-hook 'c-mode-common-hook '+my/c-mode-common-hook)

(defvar +my/cpp-default-mode-for-headers 'c++-mode
    "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

;; (spacemacs|define-jump-handlers c++-mode)

(after! smartparens
    (sp-with-modes '(c-mode c++-mode)
        ;; messes with std::cout << ...
        ;; (sp-local-pair "<" ">")
        (sp-local-pair "<" ">" :actions nil)

        ;; when you press RET, the curly braces automatically add another newline
        (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
        (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                     ("* ||\n[i]" "RET")))))

(after! cc-mode
    (add-hook 'c++-mode-hook
        (lambda ()
            (progn
                (setq-default flycheck-c/c++-clang-executable +my/clang-path)
                (setq-default flycheck-clang-standard-library "libc++")
                (setq-default flycheck-clang-language-standard "c++17")
                (setq company-clang-arguments '("-std=c++17"))))))

(after! company
    (setq company-clang-executable +my/clang-path)
    (setq company-idle-delay 0))

(use-package! modern-cpp-font-lock
    :config
    (modern-c++-font-lock-global-mode t))

(after! ccls
    (unless (executable-find "ccls")
        (setq ccls-executable (expand-file-name "ccls/Release/ccls" +my/software-path)))

    (setq ccls-initialization-options
        `(:cache (:directory ,(expand-file-name ".emacs.d/.cache/lsp-ccls" +my/dotfiles-path)))))
