;;; my-init-langs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package treesit
    :ensure nil ;; emacs built-in
    :config (setq treesit-font-lock-level 4))

;; treesit-auto : Automatically install and use tree-sitter major modes in Emacs 29+
(use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (when (version<= "29.0" emacs-version)
        (add-to-list 'treesit-auto-recipe-list
                     (make-treesit-auto-recipe
                      :lang 'cpp
                      :ts-mode 'c++-ts-mode
                      :remap 'c++-mode
                      :url "https://github.com/tree-sitter/tree-sitter-cpp"
                      :revision "v0.22.0" ;; BUG: newer grammar breaks syntax highlighting in `c++-ts-mode'
                      :ext "\\.cpp\\'"))
        (setq major-mode-remap-alist
              (treesit-auto--build-major-mode-remap-alist))) ;; BUG: https://github.com/renzmann/treesit-auto/issues/76
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

;; elisp
(use-package elisp-mode
    :ensure nil ;; emacs built-in
    :config (setq lisp-body-indent 4))

(use-package elisp-demos
    :init
    (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

;; c++ treesiter
(use-package c-ts-mode
    :ensure nil ;; emacs built-in
    :preface
    (defun my--c-ts-indent-style()
        "Override the built-in BSD indentation style with some additional rules.
         Docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
         Notes: `treesit-explore-mode' can be very useful to see where you're at in the tree-sitter tree,
                especially paired with `(setq treesit--indent-verbose t)' to debug what rules is being
                applied at a given point."
        `(;; do not indent preprocessor statements
          ((node-is "preproc") column-0 0)
          ;; do not indent namespace children
          ;; ((n-p-gp nil nil "namespace_definition") grand-parent 0)
          ((n-p-gp nil "declaration_list" "namespace_definition") parent-bol 0)
          ;; append to bsd style
          ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
    :config
    (setq c-ts-mode-indent-offset 4)
    (setq c-ts-mode-indent-style #'my--c-ts-indent-style))

;; cmake
(use-package cmake-mode)

;; adoc-mode : ascii docs
(use-package adoc-mode
    :mode "\\.adoc\\'")

;; csv-mode : Support for csv files (use csv-align-mode for alignment)
(use-package csv-mode
    :mode "\\.csv\\'")

;; yaml-mode : Support gitlab-ci.yml
(use-package yaml-mode
    :mode "\\.yml\\'")

;; web-mode : Support various web files
(use-package web-mode
    :mode ("\\.css\\'"
           "\\.html?\\'"
           "\\.html\\.twig\\'"
           "\\.phtml\\'"
           "\\.tpl\\.php\\'"
           "\\.ts\\'"
           "\\.js\\'"
           "\\.jsx?$"
           "\\.vue\\'")
    :custom ((web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
             (web-mode-markup-indent-offset 2)
             (web-mode-css-indent-offset 2)
             (web-mode-code-indent-offset 2)))

;; php-mode
(use-package php-mode
    :custom ((php-mode-coding-style 'psr2)
             (php-mode-template-compatibility nil)
             (php-imenu-generic-expression 'php-imenu-generic-expression-simple)))

(use-package python
    :config
    (defvar my--python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    (when my--python-enable-ipython
        (setq python-shell-interpreter "ipython3")
        (setq python-shell-interpreter-args "-i --simple-prompt --no-color-info")))

(use-package markdown-mode
    :mode (("\\.[Rr]md\\'" . markdown-mode)
           ("\\.qmd\\'" . markdown-mode))
    ;; :init (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)
    :config (setq markdown-fontify-code-blocks-natively t
                  markdown-fontify-whole-heading-line t
                  markdown-enable-math t))

(use-package go-mode
    :preface
    (defun my--go-setup ()
        (setq-local c-basic-offset 4) ; Base indent size when indented automatically
        (setq-local tab-width 4)
        (setq-local indent-tabs-mode nil))
    :hook ((go-mode . my--go-setup)
           (go-ts-mode . my--go-setup)))

(use-package sql
    :config (add-hook 'sql-mode-hook (setq-local tab-width 4)))

;; rust-mode : blazingly fast
(use-package rust-mode
    :config (setq rust-format-on-save nil))

;; zig
(use-package zig-mode)

;; JSON Support
(use-package json-mode)

;; Lua Support
(use-package lua-mode)

;; js is everywhere
(use-package js2-mode
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

(use-package csv-mode
    :ensure nil ;; emacs built-in
    :init
    (add-hook 'csv-mode-hook 'csv-align-mode)
    (add-hook 'csv-mode-hook (lambda () (interactive) (toggle-truncate-lines nil))))

(provide 'my-init-langs)
;;; my-init-langs.el ends here
