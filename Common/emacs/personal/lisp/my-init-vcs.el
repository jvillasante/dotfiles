;;; my-init-vcs.el -*- lexical-binding: t; -*-

;; always follow symlinks
(setq vc-follow-symlinks t)

(use-package magit
    :preface (defun my--magit-kill-buffers ()
                 "Restore window configuration and kill all Magit buffers."
                 (interactive)
                 (let ((buffers (magit-mode-get-buffers)))
                     (magit-restore-window-configuration)
                     (mapc #'kill-buffer buffers)))
    :bind (:map magit-status-mode-map
                ("q" . #'my--magit-kill-buffers)
                ("C-x k" . #'my--magit-kill-buffers))
    :custom ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
             (git-commit-summary-max-length 50)
             (magit-diff-refine-hunk t) ; show granular diffs in selected hunk.
             (magit-save-repository-buffers nil)
             (magit-define-global-key-bindings nil)))

(use-package git-gutter
    :commands (git-gutter:revert-hunk git-gutter:stage-hunk)
    :hook ((prog-mode . git-gutter-mode)
           (conf-mode . git-gutter-mode))
    :config (add-to-list 'display-buffer-alist
                         '("\\*git-gutter"
                           (display-buffer-below-selected)
                           (window-height . 0.3))))

(use-package hl-todo
    :hook ((prog-mode . hl-todo-mode)
           (conf-mode . hl-todo-mode))
    :config (setq hl-todo-highlight-punctuation ":"))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
