;;; my-init-langtools.el -*- lexical-binding: t; -*-

;; hideshow
(use-package hideshow
    :ensure nil ;; emacs built-in
    :preface
    (defun my/toggle-fold ()
        (interactive)
        (save-excursion
            (end-of-line)
            (hs-toggle-hiding)))
    :init
    (add-hook 'prog-mode-hook 'hs-minor-mode))

;; flymake
(use-package flymake
    :ensure nil ;; emacs built-in
    :hook ((prog-mode . (lambda () (flymake-mode +1))))
    :config
    (custom-set-faces ;; Underline warnings and errors from Flymake
        '(flymake-errline ((((class color)) (:underline "red"))))
        '(flymake-warnline ((((class color)) (:underline "yellow")))))

    (custom-set-variables ;; Display error and warning messages in minibuffer.
        '(help-at-pt-timer-delay 0.5)
        '(help-at-pt-display-when-idle '(flymake-overlay))))

(use-package eldoc
    :ensure nil ;; emacs built-in
    :init
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package eglot
    :ensure nil ;; emacs built-in
    :preface
    (defun my/eglot-eldoc ()
        (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))
    :hook ((eglot-managed-mode . my/eglot-eldoc)
              (c-mode-common . eglot-ensure)
              (rustic-mode . eglot-ensure)
              (js-mode . eglot-ensure)
              (python-mode . eglot-ensure)
              (go-mode . elot-ensure)
              (sql-mode . eglot-ensure))
    :init
    (setq eglot-extend-to-xref t)
    (setq eglot-autoshutdown t)
    (setq eglot-ignored-server-capabilities
        (quote (:documentFormattingProvider :documentRangeFormattingProvider :inlayHintProvider)))
    (setq  read-process-output-max (* 1024 1024))
    :config
    (add-to-list 'eglot-server-programs
        '(python-mode . ("pyright-langserver" "--stdio")))

    (add-to-list 'eglot-server-programs
        '(sql-mode . ("sqls")))

    (add-to-list 'eglot-server-programs
        '(go-mode . ("gopls")))

    (add-to-list 'eglot-server-programs
        '(rustic-mode . ("rust-analyzer")))

    (add-to-list 'eglot-server-programs
        '(c++-mode
             . ("clangd"
                   "-j=8"
                   "--log=error"
                   "--malloc-trim"
                   "--background-index"
                   "--clang-tidy"
                   "--cross-file-rename"
                   "--completion-style=detailed"
                   "--pch-storage=memory"
                   "--header-insertion=never"
                   "--header-insertion-decorators=0"))))

;; cmake
(use-package cmake-mode)

;; apheleia : Good code is automatically formatted
(use-package apheleia
    :preface
    (defun my/disable-apheleia ()
        (apheleia-mode -1))
    :init
    (apheleia-global-mode +1)
    (add-hook 'html-mode-hook 'my/disable-apheleia))

(use-package compile
    :ensure nil ; Emacs built in
    :custom
    (compilation-always-kill t))

(use-package fancy-compilation
    :commands (fancy-compilation-mode)
    :init
    (custom-set-faces
        '(fancy-compilation-default-face ((t (:inherit nil :background unspecified)))))
    (with-eval-after-load 'compile
        (fancy-compilation-mode)))

(provide 'my-init-langtools)
;;; my-init-langtools.el ends here
