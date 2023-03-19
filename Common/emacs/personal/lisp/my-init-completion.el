;;; my-init-completion.el -*- lexical-binding: t; -*-

(straight-use-package 'company)
(straight-use-package 'company-box)
(straight-use-package 'cape)
(straight-use-package 'yasnippet)

(use-package company
    :init
    (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-dabbrev-minimum-length 3
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-files-exclusions '(".git/" ".DS_Store")
        company-global-modes '(not vterm-mode)
        company-frontends '(company-pseudo-tooltip-frontend
                               ;; always show candidates in overlay tooltip
                               company-echo-metadata-frontend)
        company-backends '((company-files company-yasnippet company-capf :separate company-dabbrev-code))
        company-auto-commit nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil
        company-selection-wrap-around t)

    (add-hook 'after-init-hook 'global-company-mode)
    (add-hook 'after-init-hook 'company-tng-mode)
    :config
    (unless (display-graphic-p)
        ;; Don't persist company popups when switching back to normal mode.
        ;; `company-box' aborts on mode switch so it doesn't need this.
        (add-hook 'after-init-hook #'my/company-abort))

    (with-eval-after-load 'company-files
        ;; Fix `company-files' completion for org file:* links
        (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)"))

    (when (display-graphic-p)
        (add-hook 'company-mode-hook #'company-box-mode))

    (general-define-key
        :keymaps 'company-active-map
        "C-e" #'company-abort
        ;; use C-y to enter yasnippet expansion
        ;; without input of additional character.
        "C-y" #'company-complete-selection)

    (general-define-key
        :keymaps 'company-mode-map
        ;; manually invoke the completion
        "M-i" #'company-complete)

    (advice-add #'company-capf :around #'my/company-completion-styles)
    (yas-global-mode))

(use-package company-box
    :config
    (setq company-box-max-candidates 50
        company-frontends '(company-tng-frontend company-box-frontend)
        company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package yasnippet
    :init
    ;; TODO: (push (expand-file-name "snippets/" user-emacs-directory) yas-snippet-dirs)
    (setq yas-verbosity 2))

(provide 'my-init-completion)
;;; my-init-completion.el ends here
