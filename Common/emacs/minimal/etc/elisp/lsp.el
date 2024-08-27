;;; lsp.el --- UI -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eglot
    :ensure nil
    :defer t
    :commands (eglot
               eglot-rename
               eglot-ensure
               eglot-rename
               eglot-format-buffer)

    :custom
    (eglot-report-progress nil)  ; Prevent minibuffer spam

    :config
    ;; Optimizations
    (fset #'jsonrpc--log-event #'ignore)
    (setq jsonrpc-event-hook nil)

    (setq-default eglot-workspace-configuration
                  `(:pylsp (:plugins
                            (;; Fix imports and syntax using `eglot-format-buffer`
                             :isort (:enabled t)
                             :autopep8 (:enabled t)

                             ;; Syntax checkers (works with Flymake)
                             :pylint (:enabled t)
                             :pycodestyle (:enabled t)
                             :flake8 (:enabled t)
                             :pyflakes (:enabled t)
                             :pydocstyle (:enabled t)
                             :mccabe (:enabled t)

                             :yapf (:enabled :json-false)
                             :rope_autoimport (:enabled :json-false)))))

    (add-hook 'python-mode-hook #'eglot)
    (add-hook 'python-ts-mode-hook #'eglot))

(use-package corfu
    :ensure t
    :defer t
    :commands (corfu-mode global-corfu-mode)

    :hook ((prog-mode . corfu-mode)
           (shell-mode . corfu-mode)
           (eshell-mode . corfu-mode))

    :custom
    ;; Hide commands in M-x which do not apply to the current mode.
    (read-extended-command-predicate #'command-completion-default-include-p)
    ;; Disable Ispell completion function. As an alternative try `cape-dict'.
    (text-mode-ispell-word-completion nil)
    (tab-always-indent 'complete)

    ;; Enable Corfu
    :config
    (global-corfu-mode))

(use-package cape
    :ensure t
    :defer t
    :commands (cape-dabbrev cape-file cape-elisp-block)
    :bind ("C-c p" . cape-prefix-map)
    :init
    ;; Add to the global default value of `completion-at-point-functions' which is
    ;; used by `completion-at-point'.
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-elisp-block))

(provide 'lsp)
;;; lsp.el ends here
