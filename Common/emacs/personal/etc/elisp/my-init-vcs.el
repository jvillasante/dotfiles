;;; my-init-vcs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; always follow symlinks
(setq vc-follow-symlinks t)

(use-package magit
    :preface
    (defun my--magit-kill-buffers ()
        "Restore window configuration and kill all Magit buffers."
        (interactive)
        (let ((buffers (magit-mode-get-buffers)))
            (magit-restore-window-configuration)
            (mapc #'kill-buffer buffers)))
    :hook ((git-commit-setup . git-commit-turn-on-auto-fill))
    :custom ((magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
             (git-commit-summary-max-length 50)
             (git-commit-fill-column 72)
             (magit-diff-refine-hunk 'all) ; show granular diffs in selected hunk.
             (magit-save-repository-buffers nil)
             (magit-define-global-key-bindings nil)))

(use-package diff-hl
    :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
           (magit-post-refresh . diff-hl-magit-post-refresh)
           (dired-mode . diff-hl-dir-mode))
    :config (global-diff-hl-mode))

(use-package hl-todo
    :hook ((prog-mode . hl-todo-mode)
           (conf-mode . hl-todo-mode))
    :config (setq hl-todo-highlight-punctuation ":"))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
