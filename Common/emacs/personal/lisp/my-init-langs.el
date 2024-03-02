;;; my-init-langs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; treesit : treesitter for Emacs
(use-package treesit
    :ensure nil ;; emacs built-in
    :custom
    (treesit-font-lock-level 4))

;; treesit-auto : Automatically install and use tree-sitter major modes in Emacs 29+.
(use-package treesit-auto
    :custom
    (treesit-auto-install 'prompt)
    :config
    (treesit-auto-add-to-auto-mode-alist 'all)

    ;; Workaround for https://github.com/renzmann/treesit-auto/issues/76
    (setq major-mode-remap-alist (treesit-auto--build-major-mode-remap-alist))

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
(use-package c++-ts-mode
    :ensure nil ;; emacs built-in
    :preface (defun my--c-ts-indent-style()
                 "Override the built-in BSD indentation style with some additional rules.
         Docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Parser_002dbased-Indentation.html
         Notes: `treesit-explore-mode' can be very useful to see where you're at in the tree-sitter tree,
                especially paired with `(setq treesit--indent-verbose t)' to debug what rules is being
                applied at a given point."
                 `(
                   ;; do not indent preprocessor statements
                   ((node-is "preproc") column-0 0)
                   ;; do not indent namespace children
                   ((n-p-gp nil nil "namespace_definition") grand-parent 0)
                   ;; append to bsd style
                   ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))
    :bind (:map c++-ts-mode-map
                ("M-<up>" . treesit-beginning-of-defun)
                ("M-<down>" . treesit-end-of-defun))
    :config
    (setq c-ts-mode-indent-offset 4)
    (setq c-ts-mode-indent-style #'my--c-ts-indent-style))

;; c/c++ mode
(use-package cc-mode
    :ensure nil ;; emacs built-in
    :preface
    (defun my--c-indent-then-complete ()
        (interactive)
        (if (= 0 (c-indent-line-or-region))
                (completion-at-point)))
    :config
    (dolist (map (list c-mode-map c++-mode-map))
        (define-key map (kbd "<tab>") #'my--c-indent-then-complete))
    (add-hook 'c-mode-common-hook
              (lambda ()
                  (c-set-style "stroustrup")
                  (c-set-offset 'innamespace [0]) ; Do not indent in namespaces
                  (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
                  (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
                  (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
                  (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
                  (setq-local c-basic-offset 4) ; Base indent size when indented automatically
                  (setq-local tab-width 4)
                  (setq-local indent-tabs-mode nil)))
    :init
    (add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.C\\'"   . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode)))

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
    :disabled t
    :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.jsx?$" "\\.vue\\'")
    :custom
    (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
    (web-mode-markup-indent-offset 2)
    (web-mode-css-indent-offset 2)
    (web-mode-code-indent-offset 2))

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
    :bind ((:map markdown-mode-map
                 ("TAB" . 'markdown-cycle)))
    :init (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)
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
    :init (setq rust-format-on-save nil))

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

(provide 'my-init-langs)
;;; my-init-langs.el ends here
