(defconst jv-lsp-packages
    '(lsp-mode
         lsp-ui
         ccls))

(defun jv-lsp/post-init-lsp-mode ()
    (defun jv-lsp/setup-lsp-mode ()
        (setq lsp-enable-file-watchers nil)
        (setq lsp-restart 'auto-restart)
        (setq lsp-remap-xref-keybindings nil)
        (setq lsp-enable-on-type-formatting nil)
        (setq lsp-navigation 'both))

    (spacemacs|diminish lsp-mode " Ⓛ" " l")

    (add-hook 'c-mode-hook 'jv-lsp/setup-lsp-mode)
    (add-hook 'c++-mode-hook 'jv-lsp/setup-lsp-mode)
    (add-hook 'rust-mode-hook 'jv-lsp/setup-lsp-mode)
    (add-hook 'go-mode-hook 'jv-lsp/setup-lsp-mode))

(defun jv-lsp/post-init-lsp-ui ()
    (defun jv-lsp/setup-lsp-ui-mode ()
        (setq lsp-ui-doc-enable t)
        (setq lsp-ui-doc-include-signature nil)
        (setq lsp-ui-sideline-enable nil)
        (setq lsp-ui-sideline-show-symbol nil)
        (setq lsp-ui-sideline-ignore-dupliate nil))

    (defun jv-lsp/setup-lsp-ui-mode-no-doc ()
        (setq lsp-ui-doc-enable nil)
        (setq lsp-ui-doc-include-signature nil)
        (setq lsp-ui-sideline-enable nil)
        (setq lsp-ui-sideline-show-symbol nil)
        (setq lsp-ui-sideline-ignore-dupliate nil))

    (add-hook 'c-mode-hook 'jv-lsp/setup-lsp-ui-mode-no-doc)
    (add-hook 'c++-mode-hook 'jv-lsp/setup-lsp-ui-mode-no-doc)
    (add-hook 'rust-mode-hook 'jv-lsp/setup-lsp-ui-mode-no-doc)
    (add-hook 'go-mode-hook 'jv-lsp/setup-lsp-ui-mode-no-doc))

(defun jv-lsp/post-init-ccls ()
    (unless (executable-find "ccls")
        (setq ccls-executable (concat jv/software-path "/ccls/Release/ccls")))

    (setq ccls-initialization-options
        `(:cache (:directory ,(concat jv/dotfiles-path "/.emacs.d/.cache/lsp-ccls")))))
