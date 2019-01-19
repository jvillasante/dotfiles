(defconst jv-lsp-packages
  '(lsp-mode
     lsp-ui))

(defun jv-lsp/post-init-lsp-mode ()
  (spacemacs|diminish lsp-mode " Ⓛ" " L")

  (add-hook 'c++-mode-hook
    (lambda () (progn
                 (setq lsp-remap-xref-keybindings nil)
                 (setq lsp-navigation 'both)))))

(defun jv-lsp/post-init-lsp-ui ()
  (add-hook 'c++-mode-hook
    (lambda () (progn
                 (setq lsp-ui-doc-enable t)
                 (setq lsp-ui-doc-include-signature nil)
                 (setq lsp-ui-sideline-enable nil)
                 (setq lsp-ui-sideline-show-symbol nil)
                 (setq lsp-ui-sideline-ignore-dupliate nil)))))
