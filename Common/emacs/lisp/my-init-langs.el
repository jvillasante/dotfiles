;;; my-init-langs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; treesit : tree-sitter utilities
(use-package treesit
    :ensure nil ;; emacs built-in
    :bind (("M-<up>" . treesit-beginning-of-defun)
              ("M-<down>" . treesit-end-of-defun))
    :custom
    (treesit-extra-load-path
        `(,(expand-file-name "tree-sitter" my/var-dir)))
    (treesit-auto-install-grammar 'ask)
    (treesit-enabled-modes t)
    (treesit-font-lock-level 4))

(use-package elisp-mode
    :ensure nil ;; emacs built-in
    :defer t
    :bind (("C-h ." . helpful-at-point))
    :custom (lisp-body-indent 4))

(use-package elisp-demos
    :after elisp-mode
    :init
    ;; (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

;; c++
(use-package c++-ts-mode
    :ensure nil ;; emacs built-in
    :defer t
    :preface
    (defun my/c-ts-indent-style()
        "Override the built-in BSD indentation style with some additional rules.
         Docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
         Notes: `treesit-explore-mode' can be very useful to see where you're at in the tree-sitter tree,
                especially paired with `(setq treesit--indent-verbose t)' to debug what rules is being
                applied at a given point."
        (let ((my/rules '(;; do not indent preprocessor statements
                             ((node-is "preproc") column-0 0)
                             ;; do not indent namespace children
                             ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0))))
            (if (>= emacs-major-version 31)
                ;; Emacs 31+: c-ts-mode--simple-indent-rules
                (let ((bsd-rules (cdar (c-ts-mode--simple-indent-rules 'cpp 'bsd))))
                    `((cpp ,@my/rules ,@bsd-rules)))
                ;; Emacs 30: c-ts-mode--indent-styles
                (let ((bsd-rules (alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
                    `(,@my/rules ,@bsd-rules)))))
    :mode ("\\.h\\'"
              "\\.H\\'"
              "\\.hpp\\'"
              "\\.HPP\\'"
              "\\.c\\'"
              "\\.C\\'"
              "\\.cpp\\'"
              "\\.CPP\\'"
              "\\.inc\\'")
    :bind (:map c-ts-base-mode-map
              ("C-x C-o" . ff-find-other-file))
    :custom
    (c-ts-mode-indent-offset 4)
    (c-ts-mode-indent-style #'my/c-ts-indent-style))

;; cmake
(use-package cmake-mode :defer t)

;; adoc-mode : ascii docs
(use-package adoc-mode
    :defer t
    :mode "\\.adoc\\'")

;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(use-package csv-mode
    :defer t
    :mode "\\.csv\\'"
    :hook ((csv-mode . csv-align-mode)
              (csv-mode . (lambda () (toggle-truncate-lines nil)))))

;; yaml-mode : Support gitlab-ci.yml
(use-package yaml-mode
    :defer t
    :mode "\\.yml\\'" "\\.yaml\\'" "\\.clangd\\'")

;; web-mode : Support various web files
(use-package web-mode
    :defer t
    :mode ("\\.html?\\'"
              "\\.html\\.twig\\'"
              "\\.phtml\\'"
              "\\.tpl\\.php\\'")
    :custom ((web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
                (web-mode-markup-indent-offset 2)
                (web-mode-css-indent-offset 2)
                (web-mode-code-indent-offset 2)))

;; php-mode
(use-package php-mode
    :defer t
    :mode ("\\.php\\'")
    :custom ((php-mode-coding-style 'psr2)
                (php-mode-template-compatibility nil)
                (php-imenu-generic-expression 'php-imenu-generic-expression-simple)))

(use-package python
    :defer t
    :config
    (defvar my/python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    (when my/python-enable-ipython
        (setq python-shell-interpreter "ipython3")
        (setq python-shell-interpreter-args "-i --simple-prompt --no-color-info")))

(use-package markdown-mode
    :defer t
    :mode (("\\.[Rr]md\\'" . markdown-mode)
              ("\\.qmd\\'" . markdown-mode))
    :bind (:map markdown-mode-map
              ("TAB" . markdown-cycle))
    :init (setq markdown-command "multimarkdown")
    :config (setq markdown-fontify-code-blocks-natively t
                markdown-fontify-whole-heading-line t
                markdown-enable-math t))

(use-package dockerfile-mode
    :defer t
    :custom (dockerfile-mode-command "podman"))

(use-package go-mode
    :defer t
    :preface
    (defun my/go-setup ()
        (setq-local c-basic-offset 4) ; Base indent size when indented automatically
        (setq-local tab-width 4)
        (setq-local indent-tabs-mode nil))
    :hook ((go-mode . my/go-setup)
              (go-ts-mode . my/go-setup)))

(use-package sql
    :defer t
    :hook (sql-mode . (lambda () (setq-local tab-width 4))))

;; rust-mode : blazingly fast
(use-package rust-mode
    :defer t
    :config (setq rust-format-on-save nil))

;; zig
(use-package zig-mode :defer t)

;; JSON Support
(use-package json-mode :defer t)

;; Lua Support
(use-package lua-mode :defer t)

;; js is everywhere
(use-package js2-mode
    :defer t
    :init
    (add-hook 'js2-mode-hook
        (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)))
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    :config
    (setq-default js2-basic-indent 2
        js2-basic-offset 2
        js2-auto-indent-p t
        js2-cleanup-whitespace t
        js2-enter-indents-newline t
        js2-indent-on-enter-key t
        js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute"
                               "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location"
                               "__dirname" "console" "JSON" "jQuery" "$")))

(provide 'my-init-langs)
;;; my-init-langs.el ends here
