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
    :hook ((prog-mode . (lambda () (flymake-mode +1)))))

(use-package eldoc
    :ensure nil ;; emacs built-in
    :init
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package eglot
    :ensure nil ;; emacs built-in
    :preface
    (defun my/eglot-eldoc ()
        ;; Show flymake diagnostics first.
        (setq eldoc-documentation-functions
            (cons #'flymake-eldoc-function
                (remove #'flymake-eldoc-function eldoc-documentation-functions)))
        ;; Show all eldoc feedback.
        (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

    ;; Making a Cape Super Capf for Eglot
    (defun my/eglot-capf ()
        (setq-local completion-at-point-functions
            (list (cape-super-capf
                      #'eglot-completion-at-point
                      ;; #'tempel-expand
                      #'cape-file))))
    :hook ((eglot-managed-mode . my/eglot-eldoc)
              (c-mode-common . eglot-ensure)
              (rustic-mode . eglot-ensure)
              ;; (js-mode . eglot-ensure)
              (python-mode . eglot-ensure)
              (go-mode . eglot-ensure)
              (sql-mode . eglot-ensure))
    :init
    (setq eglot-extend-to-xref t)
    (setq eglot-autoshutdown t)
    (setq read-process-output-max (* 1024 1024))
    (setq eglot-ignored-server-capabilities
        (quote (:documentFormattingProvider :documentRangeFormattingProvider :inlayHintProvider)))

    ;; Configure Corfu for Eglot
    (setq completion-category-overrides '((eglot (styles orderless))))
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

    :config
    (add-to-list 'eglot-stay-out-of 'eldoc-documentation-strategy)
    ;; (add-to-list 'eglot-stay-out-of 'flymake)

    ;; workspace
    (setq-default eglot-workspace-configuration
        '((:gopls .
              ((staticcheck . t)
                  (usePlaceholders . t)))))

    ;; python
    (add-to-list 'eglot-server-programs
        '(python-mode . ("pyright-langserver" "--stdio")))

    ;; js
    (add-to-list 'eglot-server-programs
        '(js-mode . ("typescript-language-server" "--stdio")))

    ;; web
    (add-to-list 'eglot-server-programs
        '(web-mode . ("typescript-language-server" "--stdio")))

    ;; sql
    (add-to-list 'eglot-server-programs
        '(sql-mode . ("sqls")))

    ;; go
    (add-to-list 'eglot-server-programs
        '(go-mode . ("gopls")))

    ;; rust
    (add-to-list 'eglot-server-programs
        '(rustic-mode . ("rust-analyzer")))

    ;; C++
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
