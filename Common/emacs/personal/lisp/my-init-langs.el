;;; my-init-langs.el -*- lexical-binding: t; -*-

;; treesit-auto : Automatically install and use tree-sitter major modes in Emacs 29+.
(use-package treesit-auto
    :init
    (dolist (mapping '((python-mode     . python-ts-mode)
                       (sh-mode         . bash-ts-mode)
                       (cmake-mode      . cmake-ts-mode)
                       (css-mode        . css-ts-mode)
                       (typescript-mode . tsx-ts-mode)
                       (json-mode       . json-ts-mode)
                       (js-mode         . js-ts-mode)
                       (css-mode        . css-ts-mode)
                       (yaml-mode       . yaml-ts-mode)
                       (c-mode          . c-ts-mode)
                       (c++-mode        . c++-ts-mode)
                       (c-or-c++-mode   . c-or-c++-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
    :config
    (setq treesit-font-lock-level 4)
    (setq treesit-auto-install 'prompt)
    (global-treesit-auto-mode))

;; elisp
(use-package elisp-mode
    :ensure nil ;; emacs built-in
    :config (setq lisp-body-indent 4))

(use-package elisp-demos
    :init
    (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

;; c/c++ mode
(use-package cc-mode
    :ensure nil ;; emacs built-in
    :preface
    (defun my/c-indent-then-complete ()
        (interactive)
        (if (= 0 (c-indent-line-or-region))
                (completion-at-point)))
    :config
    (dolist (map (list c-mode-map c++-mode-map))
        (define-key map (kbd "<tab>") #'my/c-indent-then-complete))
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

;; modern-cpp-font-lock : Syntax highlighting support for "Modern C++"
(use-package modern-cpp-font-lock
    :disabled t
    :config (modern-c++-font-lock-global-mode t))

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
    :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.jsx?$" "\\.vue\\'")
    :custom
    (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
    (web-mode-markup-indent-offset 2)
    (web-mode-css-indent-offset 2)
    (web-mode-code-indent-offset 2))

(use-package python
    :config
    (defvar my/python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    (when my/python-enable-ipython
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
    (defun my/go-setup ()
        (setq-local c-basic-offset 4) ; Base indent size when indented automatically
        (setq-local tab-width 4)
        (setq-local indent-tabs-mode nil))
    :hook ((go-mode . my/go-setup)
           (go-ts-mode . my/go-setup)))

(use-package sql
    :config (add-hook 'sql-mode-hook (setq-local tab-width 4)))

;; rustic : blazingly fast
(use-package rustic
    :config
    (setq rustic-lsp-server 'rust-analyzer
          rustic-format-on-save nil
          rustic-lsp-client 'eglot))

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
