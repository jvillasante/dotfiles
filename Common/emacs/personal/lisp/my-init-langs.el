;;; my-init-langs.el -*- lexical-binding: t; -*-

;; elisp
(use-package elisp-mode
    :ensure nil ;; emacs built-in
    :init (add-hook 'emacs-lisp-mode-hook #'my/elisp-setup)
    :config
    (setq lisp-body-indent 4
        lisp-indent-function #'my/lisp-indent-function))

(use-package elisp-demos
    :init
    (advice-add #'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
    (advice-add #'helpful-update :after #'elisp-demos-advice-helpful-update))

;; c/c++ mode
(use-package cc-mode
    :ensure nil ;; emacs built-in
    :config
    (add-to-list 'auto-mode-alist '("\\.h\\'"   . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cc\\'"  . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.C\\'"   . c++-mode))
    (add-hook 'c-mode-common-hook
        (lambda ()
            (c-set-style "stroustrup")
            (c-set-offset 'cpp-macro 0 nil) ; Indent C/C++ macros as normal code
            (c-set-offset 'substatement-open 0) ; Align braces with the if/for statement. If not set, a half indent will be used
            (c-set-offset 'arglist-intro '+) ; Align multiline arguments with a standard indent (instead of with parenthesis)
            (c-set-offset 'arglist-close 0) ; Align the parenthesis at the end of the arguments with the opening statement indent
            (setq c-basic-offset 4) ; Base indent size when indented automatically
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            (eglot-ensure))))

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
    :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.vue\\'")
    :custom
    (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
    (web-mode-markup-indent-offset 2)) ; For html : use an indent of size 2 (default is 4)

(use-package python
    :config
    (add-to-list 'python-mode-hook #'eglot-ensure)
    (defvar my/python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    (when my/python-enable-ipython
        (setq python-shell-interpreter "ipython3")
        (setq python-shell-interpreter-args "-i --simple-prompt --no-color-info"))

    (when (and (display-graphic-p)
              (featurep 'xwidget-internal))
        (add-hook 'python-mode-hook #'my/xwidget-side-window-mode)
        (add-hook 'python-mode-hook #'my/refresh-xwidget-after-eval-python-mode)))

(use-package markdown-mode
    :mode (("\\.[Rr]md\\'" . markdown-mode)
              ("\\.qmd\\'" . markdown-mode))
    :init
    (setq markdown-fontify-code-blocks-natively t
        markdown-fontify-whole-heading-line t
        markdown-enable-math t)
    :bind ((:map markdown-mode-map
               ("TAB" . 'markdown-cycle))))

(use-package go-mode
    :config
    (add-to-list 'go-mode-hook #'eglot-ensure)
    (add-hook 'go-mode-hook (setq-local tab-width 4)))

(use-package sql
    :config
    (add-to-list 'sql-mode-hook #'eglot-ensure)
    (setq sqlind-basic-offset 4)
    (add-hook 'sql-mode-hook (setq-local tab-width 4)))

;; rustic : blazingly fast
(use-package rustic
    :config
    (add-to-list 'rustic-mode-hook #'eglot-ensure)
    (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'eglot
        rustic-format-on-save nil))

;; js is everywhere
;; TODO: Add LSP support
(use-package js2-mode
    :init
    (add-hook 'js2-mode-hook
        (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)))

    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

    :config
    (setq js-basic-indent 2)
    (setq-default js2-basic-indent 2
        js2-basic-offset 2
        js2-auto-indent-p t
        js2-cleanup-whitespace t
        js2-enter-indents-newline t
        js2-indent-on-enter-key t
        js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$")))

(provide 'my-init-langs)
;;; my-init-langs.el ends here
