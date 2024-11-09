;;; my-init-shell.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Code:

(use-package vterm
    :ensure t
    :defer t
    :commands vterm
    :config
    ;; Speed up vterm
    (setq vterm-timer-delay 0.01))

(provide 'my-init-shell)
;;; my-init-shell.el ends here
