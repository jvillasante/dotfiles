;;; my-init-langs.el -*- lexical-binding: t; -*-

(straight-use-package 'adoc-mode)
(straight-use-package 'csv-mode)
(straight-use-package 'yaml-mode)
(straight-use-package 'web-mode)
(straight-use-package '(c++-mode :type built-in))
(straight-use-package 'prettier-js)
(straight-use-package 'json-mode)
(straight-use-package 'markdown-mode)
(straight-use-package 'go-mode)
(straight-use-package 'sql-indent)
(straight-use-package 'rustic)
(straight-use-package 'format-all)

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
    :preface
    (defun my/match-buffer-extension(&rest extensions)
        "Returns t if the current buffer has an extension in EXTENSIONS"
        (if (member (file-name-extension (buffer-name)) extensions)
            t))
    :mode ("\\.css\\'" "\\.html\\'" "\\.ts\\'" "\\.js\\'" "\\.vue\\'")
    :hook (web-mode . (lambda () (when (my/match-buffer-extension "ts" "js" "vue")
                                     (setq-local lsp-auto-format t))))
    :custom
    (web-mode-script-padding 0) ; For vue.js SFC : no initial padding in the script section
    (web-mode-markup-indent-offset 2)) ; For html : use an indent of size 2 (default is 4)

;; prettier-js : Formatting on save, used by my-ts-mode for .js and .ts files
(use-package prettier-js
    :custom
    (prettier-js-show-errors nil)
    (prettier-js-args '("--semi" "false"
                           "--single-quote" "false"
                           "--tab-width" "4"
                           "--trailing-comma" "all"
                           "--print-width" "150")))

;; c/c++ mode
(add-to-list 'c-mode-common-hook #'eglot-ensure)
(use-package c++-mode
    :mode ("\\.h\\'" "\\.cpp\\'" "\\.hpp\\'" "\\.hxx\\'" "\\.cxx\\'" "\\.cc\\'" "\\.C\\'")
    :config
    (advice-add 'c-update-modeline :override #'ignore)) ;; Don't use a modeline suffix (i.e C++//l)

(use-package python
    :init
    (defvar my/python-enable-ipython t
        "use ipython as the embedded REPL.")
    (setq python-indent-offset 4)

    :config
    (add-to-list 'python-mode-hook #'eglot-ensure)

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
    (add-hook 'go-mode-hook (setq-local tab-width 4))
    (add-to-list 'go-mode-hook #'eglot-ensure))

(use-package sql
    :init
    (setq sqlind-basic-offset 4)

    :config
    (add-hook 'sql-mode-hook (setq-local tab-width 4))
    (add-to-list 'sql-mode-hook #'eglot-ensure))

;; rustic : blazingly fast
(use-package rustic
    :config
    (add-to-list 'rustic-mode-hook #'eglot-ensure)
    (setq
        rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'eglot
        rustic-format-on-save nil))

;;;; format-all :
(use-package format-all
    :hook ((c-mode . format-all-mode)
              (c++-mode . format-all-mode)
              (python-mode . format-all-mode)
              (format-all-mode . format-all-ensure-formatter))
    :config
    (custom-set-variables
        '(format-all-formatters (quote (("C++" clang-format)
                                           ("Python" black))))))

(provide 'my-init-langs)
;;; my-init-langs.el ends here
