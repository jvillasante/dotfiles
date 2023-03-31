;;; my-init-vcs.el -*- lexical-binding: t; -*-

;; always follow symlinks
(setq vc-follow-symlinks t)

(use-package magit
    :init
    (setq git-commit-summary-max-length 50
        magit-diff-refine-hunk t ; show granular diffs in selected hunk.
        ;; Don't autosave repo buffers. This is too magical, and
        ;; saving can trigger a bunch of unwanted side-effects, like
        ;; save hooks and formatters. Trust the user to know what
        ;; they're doing.
        magit-save-repository-buffers nil
        magit-define-global-key-bindings nil)
    :hook
    ((git-commit-mode . (lambda () (setq-local fill-column 72))))
    :config
    (add-to-list 'display-buffer-alist
        '("magit:" ;; the main magit dashboard
             ;; don't create new window if there is one magit window
             ;; and create new tab if there isn't one magit window.
             (display-buffer-reuse-window display-buffer-in-new-tab))))

(use-package git-gutter
    :commands (git-gutter:revert-hunk git-gutter:stage-hunk)
    :hook ((prog-mode . git-gutter-mode)
              (conf-mode . git-gutter-mode))
    :config
    (add-to-list 'display-buffer-alist
        '("\\*git-gutter"
             (display-buffer-below-selected)
             (window-height . 0.3))))

(use-package hl-todo
    :hook ((prog-mode . hl-todo-mode)
              (conf-mode . hl-todo-mode))
    :init
    (setq hl-todo-highlight-punctuation ":"))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
