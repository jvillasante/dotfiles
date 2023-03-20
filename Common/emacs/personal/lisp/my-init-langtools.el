;;; my-init-langtools.el -*- lexical-binding: t; -*-

(straight-use-package 'eglot)
(straight-use-package 'consult-eglot)
(straight-use-package 'apheleia)

;; hideshow
(progn
    (add-hook 'prog-mode-hook 'hs-minor-mode)
    (defun +my/toggle-fold ()
        (interactive)
        (save-excursion
            (end-of-line)
            (hs-toggle-hiding))))

;; flymake
(progn
    ;; Underline warnings and errors from Flymake
    (custom-set-faces
        '(flymake-errline ((((class color)) (:underline "red"))))
        '(flymake-warnline ((((class color)) (:underline "yellow")))))

    ;; Display error and warning messages in minibuffer.
    (custom-set-variables
        '(help-at-pt-timer-delay 0.5)
        '(help-at-pt-display-when-idle '(flymake-overlay))))

(use-package eldoc
    :init
    (setq eldoc-echo-area-use-multiline-p nil
        eldoc-documentation-strategy #'eldoc-documentation-compose)
    ;; eglot has 3 eldoc functions: `eglot-hover-eldoc-function', and
    ;; `eglot-signature-eldoc-function', using the default strategy
    ;; will only show one information, setting to the following option
    ;; allows the possibility to show both information in eldoc
    ;; buffer.

    :config
    (add-to-list 'display-buffer-alist
        `("\\*eldoc\\*"
             (display-buffer-reuse-window display-buffer-in-side-window)
             (window-width . 0.5)
             (window-height . 0.4)
             (slot . ,(alist-get 'eldoc my/side-window-slots)))))

(use-package eglot
    :init
    (setq eglot-stay-out-of '(company)
        eglot-autoshutdown t
        eglot-extend-to-xref t
        eglot-workspace-configuration '(:pyright (:useLibraryCodeForTypes t :openFilesOnly :json-false)
                                           :r (:lsp (:diagnostics :json-false)))
        eglot-ignored-server-capabilities (quote (:documentFormattingProvider :documentRangeFormattingProvider))
        read-process-output-max (* 1024 1024))

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
        '(c-mode c++-mode
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
                   "--header-insertion-decorators=0")))

    (add-hook 'eglot-managed-mode-hook
        (setq-local eldoc-documentation-function #'eldoc-documentation-compose))

    ;; format on save
    ;; (add-hook 'c-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
    ;; (add-hook 'c++-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
    ;; (add-hook 'python-mode-hook '(lambda() (add-hook 'before-save-hook 'eglot-format-buffer nil t)))

    (general-create-definer my/lsp-map
        :prefix "C-c c"
        :prefix-map 'my/lsp-map)

    (my/lsp-map
        :keymaps 'eglot-mode-map
        "" '(:ignore t :which-key "lsp")
        "f" #'eglot-format
        "s" #'consult-eglot-symbols
        "a" #'eglot-code-actions
        "e" #'consult-flymake
        "n" #'eglot-rename
        "t" #'eglot-find-typeDefinition
        "i" #'eglot-find-implementation
        "[" #'xref-go-back
        "]" #'xref-go-forward
        "d" #'xref-find-definitions
        "D" #'xref-find-references))

;; apheleia : Good code is automatically formatted
(use-package apheleia
    :init
    (apheleia-global-mode +1))

(provide 'my-init-langtools)
;;; my-init-langtools.el ends here
