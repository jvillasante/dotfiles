;;; my-init-langtools.el -*- lexical-binding: t; -*-

(straight-use-package '(hideshow :type built-in))
(straight-use-package '(flymake :type built-in))
(straight-use-package '(eldoc :type built-in))
(straight-use-package '(eglot :type built-in))
(straight-use-package 'consult-eglot)
(straight-use-package 'apheleia)
(straight-use-package 'fancy-compilation)

;; hideshow
(use-package hideshow
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
    :demand t
    :config
    (custom-set-faces ;; Underline warnings and errors from Flymake
        '(flymake-errline ((((class color)) (:underline "red"))))
        '(flymake-warnline ((((class color)) (:underline "yellow")))))

    (custom-set-variables ;; Display error and warning messages in minibuffer.
        '(help-at-pt-timer-delay 0.5)
        '(help-at-pt-display-when-idle '(flymake-overlay))))

(use-package eldoc
    :init
    (setq eldoc-echo-area-use-multiline-p nil)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    :config
    (add-to-list 'display-buffer-alist
        `("\\*eldoc\\*"
             (display-buffer-reuse-window display-buffer-in-side-window)
             (window-width . 0.5)
             (window-height . 0.4)
             (slot . ,(alist-get 'eldoc my/side-window-slots)))))

(use-package eglot
    :preface
    (defun my/eglot-eldoc ()
        (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
    :hook ((eglot-managed-mode . my/eglot-eldoc))
    :init
    (setq eglot-sync-connect 1
        eglot-extend-to-xref t
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-auto-display-help-buffer nil)
    ;; (setq eglot-stay-out-of '(flymake))
    (setq eglot-ignored-server-capabilities
        (quote (:documentFormattingProvider :documentRangeFormattingProvider)))
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
                   "--header-insertion-decorators=0")))

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

(use-package fancy-compilation
    :commands (fancy-compilation-mode)
    :init
    (custom-set-faces
        '(fancy-compilation-default-face ((t (:inherit nil :background unspecified)))))
    (with-eval-after-load 'compile
        (fancy-compilation-mode)))

(provide 'my-init-langtools)
;;; my-init-langtools.el ends here
