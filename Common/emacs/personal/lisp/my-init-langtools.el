;;; my-init-langtools.el -*- lexical-binding: t; -*-

(straight-use-package 'citre)
(straight-use-package 'eglot)
(straight-use-package 'consult-eglot)
(straight-use-package 'edit-indirect)

(use-package citre
    :init
    (require 'citre-config)
    (setq citre-tags-completion-case-sensitive nil)

    (add-hook 'emacs-lisp-mode-hook (my/setq-locally citre-enable-imenu-integration nil))
    (add-hook 'emacs-lisp-mode-hook (my/setq-locally citre-enable-capf-integration nil))
    (add-hook 'emacs-lisp-mode-hook (my/setq-locally citre-enable-xref-integration nil))
    (add-hook 'org-mode-hook (my/setq-locally citre-enable-imenu-integration nil))
    (add-hook 'org-mode-hook (my/setq-locally citre-enable-capf-integration nil))
    (add-hook 'markdown-mode-hook (my/setq-locally citre-enable-imenu-integration nil))

    :config
    (general-define-key
        :keymaps 'citre-mode-map
        "C-]" #'citre-jump
        "C-t" #'citre-jump-back)

    (general-define-key
        :keymaps 'citre-mode-map
        "C-w ]" #'citre-peek)

    (general-define-key
        :prefix "C-c w"
        :keymaps 'citre-mode-map
        "]" #'citre-peek)

    (general-define-key
        :keymaps 'citre-peek-keymap
        "q" #'citre-peek-abort
        "RET" #'citre-peek-jump
        "[t" #'citre-peek-prev-definition
        "]t" #'citre-peek-next-definition
        "M-[" #'citre-peek-prev-line
        "M-]" #'citre-peek-next-line
        "M-{" #'citre-peek-prev-branch
        "M-}" #'citre-peek-next-branch)

    (add-hook 'citre-after-jump-hook #'better-jumper-set-jump))

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

    (add-hook
        'eglot-managed-mode-hook #'my/toggle-citre-eglot-capf)

    (add-hook 'eglot-managed-mode-hook
        (my/setq-locally eldoc-documentation-function #'eldoc-documentation-compose))

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
