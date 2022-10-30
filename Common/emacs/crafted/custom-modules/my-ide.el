;;; my-ide.el --- Custom IDE Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; IDE Customizations

;;; Code:

;; Eglot
(when (crafted-package-installed-p 'eglot)
    (setc eldoc-echo-area-use-multiline-p nil)
    (setc eglot-extend-to-xref t)
    (setc eglot-ignored-server-capabilities
        (quote (:documentFormattingProvider :documentRangeFormattingProvider)))
    (add-to-list 'eglot-server-programs
        `(rust-mode . ("rust-analyzer"))
        `(c-mode c++-mode
             . ("/usr/bin/clangd"
                   "-j=4"
                   "--malloc-trim"
                   "--log=error"
                   "--background-index"
                   "--clang-tidy"
                   "--cross-file-rename"
                   "--completion-style=detailed"
                   "--pch-storage=memory"
                   "--header-insertion=never"
                   "--header-insertion-decorators=0"))))

;; C++
(progn
    (setc c-default-style "linux")
    (setc c-basic-offset 4)
    (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
    (add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))
    (add-hook 'c-mode-common-hook
        (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
                (eglot-ensure)))))

;; rustic : rust mode
(crafted-package-install-package 'rustic)
(progn
    (setc rustic-format-on-save nil)
    (add-hook 'rustic-mode-hook #'eglot-ensure))

;; format-all : auto format source code
(crafted-package-install-package 'format-all)
(progn
    (add-hook 'c-mode-hook #'format-all-mode)
    (add-hook 'c++-mode-hook #'format-all-mode)
    (add-hook 'python-mode-hook #'format-all-mode)
    (add-hook 'rustic-mode-hook #'format-all-mode)
    (add-hook 'format-all-mode-hook #'format-all-ensure-formatter)
    (custom-set-variables
        '(format-all-formatters (quote (("C++" clang-format)
                                           ("Python" black))))))

(provide 'my-ide)
;;; my-ide.el ends here
