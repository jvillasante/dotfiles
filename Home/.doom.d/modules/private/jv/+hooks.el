;;; private/jv/+hooks.el -*- lexical-binding: t; -*-

(add-hook! 'markdown-mode-hook
    (progn
        (toggle-word-wrap nil)
        (auto-fill-mode -1)))

(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
