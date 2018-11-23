;;; editor/lispyville/config.el -*- lexical-binding: t; -*-

(def-package! lispy
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'lispy-mode))

(def-package!
  lispyville
  :after lispy
  :when (featurep! :feature evil)
  :init
  (setq lispyville-key-theme
    '(operators
       c-w
       prettify
       text-objects
       additional-motions
       wrap
       mark-toggle))
  :config
  (push 'emacs-lisp-mode evil-escape-excluded-major-modes)
  (add-hook 'lispy-mode-hook #'lispyville-mode))
