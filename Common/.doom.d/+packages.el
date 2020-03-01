;;; +packages.el -*- lexical-binding: t; -*-

(use-package! pinentry
    :init
    (setq epa-pinentry-mode 'loopback))

(use-package! crux
    :bind (("C-c o" . crux-open-with)))

(use-package! visual-regexp
    :commands (vr/query-replace vr/replace))
