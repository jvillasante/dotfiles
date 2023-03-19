;;; my-init-langtools.el -*- lexical-binding: t; -*-

(straight-use-package 'eglot)
(straight-use-package 'consult-eglot)
(straight-use-package 'edit-indirect)

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
        eglot-workspace-configuration
        '(:pyright (:useLibraryCodeForTypes t :openFilesOnly :json-false)
             :r (:lsp (:diagnostics :json-false)))
        read-process-output-max (* 1024 1024))

    :config
    (add-to-list 'eglot-server-programs
        '(python-mode . ("pyright-langserver" "--stdio")))

    (add-to-list 'eglot-server-programs
        '(sql-mode . ("sqls")))

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

    (general-create-definer my/lsp-map
        :prefix "C-c l"
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
        "]" #'xref-go-forward)

    (general-define-key
        :keymaps 'eglot-mode-map
        "] d" #'flymake-goto-next-error
        "[ d" #'flymake-goto-prev-error
        ;; jump to next/prev location containing the references.
        "[ r" (my/xref-move-in-original-src-macro xref-prev-line)
        "] r" (my/xref-move-in-original-src-macro xref-next-line)
        ;; jump to next/prev file containing the references.
        "[ R" (my/xref-move-in-original-src-macro xref-prev-group)
        "] R" (my/xref-move-in-original-src-macro xref-next-group)
        "K" #'my/eldoc-buffer-dwim
        "gd" #'xref-find-definitions
        "gr" #'xref-find-references))

(use-package edit-indiret
    :init
    (add-hook 'edit-indirect-after-creation-hook #'my/markdown-src-lsp-setup)
    (add-to-list 'display-buffer-alist
        '("\\*edit-indirect"
             (display-buffer-at-bottom)
             (window-height . 0.8))))

(provide 'my-init-langtools)
;;; my-init-langtools.el ends here
