;;; my-init-completion.el -*- lexical-binding: t; -*-

(straight-use-package 'corfu)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(use-package corfu
    :demand t
    :preface
    (defun corfu-send-shell (&rest _)
        "Send completion candidate when inside comint/eshell."
        (cond
            ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
                (eshell-send-input))
            ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
                (comint-send-input))))
    :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("TAB" . nil)
              ("RET" . corfu-insert))
    :config
    (setq corfu-cycle t
        corfu-auto t
        corfu-quit-no-match 'separator)
    :init
    (global-corfu-mode)
    (global-set-key (kbd "M-i") #'completion-at-point)
    (add-hook 'eshell-mode-hook
        (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))
    (advice-add #'corfu-insert :after #'corfu-send-shell))

(use-package yasnippet
    :init
    (yas-global-mode +1)
    :config
    (yas-reload-all)
    (push (expand-file-name "snippets/" user-emacs-directory) yas-snippet-dirs))

(provide 'my-init-completion)
;;; my-init-completion.el ends here
