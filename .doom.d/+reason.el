;;;  -*- lexical-binding: t; -*-

(def-package! reason-mode
  :mode "\\.rei?$"
  :commands (reason-mode)
  :config
  (let* (
          (refmt-bin (executable-find "refmt"))
          (merlin-bin (executable-find "ocamlmerlin"))
          (merlin-base-dir (when merlin-bin
                             (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    ;; Add npm merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
      (setq merlin-command merlin-bin))

    (when refmt-bin
      (setq refmt-command refmt-bin))
    (require 'merlin)
    (add-hook! reason-mode
      (add-hook 'before-save-hook #'refmt-before-save nil t)
      (merlin-mode))
    (setq-hook! reason-mode
      indent-region-function #'apply-refmt)
    (set-electric! 'some-mode :chars '(?|))
    (set-lookup-handlers! 'reason-mode
      :definition #'merlin-locate
      :references #'merlin-occurrences
      :documentation #'merlin-document)
    (set-company-backend! 'reason-mode 'merlin-company-backend)))
