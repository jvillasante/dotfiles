;;; my-init-icons.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package nerd-icons)

(use-package nerd-icons-completion
    :after marginalia
    :config (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
    :after corfu
    :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
    :hook (dired-mode . nerd-icons-dired-mode))

(provide 'my-init-icons)
;;; my-init-icons.el ends here
