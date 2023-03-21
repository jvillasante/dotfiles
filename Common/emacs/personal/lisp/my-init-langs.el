;;; my-init-langs.el -*- lexical-binding: t; -*-

(straight-use-package 'adoc-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'web-mode)
(straight-use-package '(c++-mode :type built-in))
(straight-use-package 'json-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'go-mode)
(straight-use-package 'sql-indent)
(straight-use-package 'rustic)
(straight-use-package 'js2-mode)

(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)
(c-set-offset 'comment-intro 0)

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

;; c/c++ mode
(use-package c++-mode
    :init (add-to-list 'c-mode-common-hook #'eglot-ensure)
    :mode ("\\.h\\'" "\\.cpp\\'" "\\.hpp\\'" "\\.hxx\\'" "\\.cxx\\'" "\\.cc\\'" "\\.C\\'"))

(use-package python
    :config
    (add-to-list 'python-mode-hook #'eglot-ensure)
    (defvar my/python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    (when my/python-enable-ipython
        (setq python-shell-interpreter "ipython3")
        (setq python-shell-interpreter-args "-i --simple-prompt --no-color-info"))

    (add-to-list 'display-buffer-alist
        `("^\\*[pP]ython"
             (display-buffer-reuse-window display-buffer-in-side-window)
             (window-width . 0.5)
             (window-height . 0.4)
             (side . bottom)
             (slot . ,(alist-get 'python my/side-window-slots))))

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

    :config
    (general-define-key
        :keymaps 'markdown-mode-map
        "TAB" #'markdown-cycle))

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
