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

;; (use-package eldoc-box
;;     :init
;;     (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

(use-package eglot
    :ensure nil ;; emacs built-in
    :if (eq my/lsp-backend 'eglot)
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
              (js-mode . eglot-ensure)
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

    ;; workspace
    (setq-default eglot-workspace-configuration
        '((:gopls .
              ((staticcheck . t)
                  (usePlaceholders . t)))))

    ;; python
    (add-to-list 'eglot-server-programs
        '(python-mode . ("pyright-langserver" "--stdio")))

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

(use-package lsp-mode
    :if (eq my/lsp-backend 'lsp-mode)
    :hook ((lsp-mode . lsp-enable-which-key-integration)
              (c-mode-common . lsp)
              (rustic-mode . lsp)
              (js-mode . lsp)
              (python-mode . lsp)
              (go-mode . lsp)
              (sql-mode . lsp))
    :init
    ;; Rust hack!
    (cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
        (-let* (((&hash "value") contents)
                   (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
                   (sig_group (if (s-equals? "```rust" (car (-third-item groups)))
                                  (-third-item groups)
                                  (car groups)))
                   (sig (--> sig_group
                            (--drop-while (s-equals? "```rust" it) it)
                            (--take-while (not (s-equals? "```" it)) it)
                            (--map (s-trim it) it)
                            (s-join " " it))))
            (lsp--render-element (concat "```rust\n" sig "\n```"))))

    ;; customizations
    (setq lsp-keymap-prefix "C-c l") ;; set prefix for lsp-command-keymap
    (setq lsp-session-file (expand-file-name ".lsp-session" no-littering-var-directory))
    (setq lsp-idle-delay 0.5)
    (setq lsp-file-watch-threshold 15000)
    (setq lsp-auto-guess-root t)
    (setq lsp-log-io nil)
    (setq lsp-restart 'auto-restart)
    (setq lsp-enable-symbol-highlighting t)
    (setq lsp-lens-enable nil)
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-modeline-code-actions-enable t)
    (setq lsp-modeline-diagnostics-enable t)
    (setq lsp-eldoc-enable-hover t)
    (setq lsp-signature-auto-activate t)
    (setq lsp-signature-render-documentation nil)
    (setq lsp-completion-show-detail t)
    (setq lsp-completion-show-kind nil)
    (setq read-process-output-max (* 1024 1024)) ;; 1MB

    ;; Rust
    (setq lsp-rust-analyzer-cargo-watch-command "clippy")
    (setq lsp-rust-analyzer-completion-auto-import-enable nil)

    ;; Zig
    (setq lsp-zig-zls-executable
        (expand-file-name "zig/zls/zig-out/bin/zls" my/software-path))

    ;; C++
    (setq lsp-clients-clangd-args
        '("-j=8"
             "--log=error"
             "--malloc-trim"
             "--background-index"
             "--clang-tidy"
             "--cross-file-rename"
             "--completion-style=detailed"
             "--pch-storage=memory"
             "--header-insertion=never"
             "--header-insertion-decorators=0"))

    ;; clangd
    ;; (with-eval-after-load 'lsp-clangd (set-lsp-priority! 'clangd 2))
    )

(use-package lsp-mode
    :if (eq my/lsp-backend 'lsp-mode)
    :init
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-doc-show-with-cursor nil)
    (setq lsp-ui-doc-show-with-mouse nil)
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-ui-sideline-show-code-actions nil)
    (setq lsp-ui-sideline-show-hover nil))

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
