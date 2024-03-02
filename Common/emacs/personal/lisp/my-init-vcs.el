;;; my-init-vcs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; always follow symlinks
(setq vc-follow-symlinks t)

(use-package magit
    :defines magit-status-mode-map
    :functions magit-status magit-restore-window-configuration magit-mode-get-buffers
    :bind
    (("C-x g" . #'magit-status))
    :custom
    ((magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
     (magit-bury-buffer-function #'magit-restore-window-configuration)
     (git-commit-summary-max-length 50)
     (magit-diff-refine-hunk t) ; show granular diffs in selected hunk.
     (magit-save-repository-buffers nil)
     (magit-define-global-key-bindings nil)))

(use-package diff-hl
    :functions global-diff-hl-mode
    :hook
    ((magit-pre-refresh . diff-hl-magit-pre-refresh)
     (magit-post-refresh . diff-hl-magit-post-refresh)
     (dired-mode . diff-hl-dir-mode))
    :config
    (global-diff-hl-mode))

(use-package hl-todo
    :defines hl-todo-highlight-punctuation
    :hook
    ((prog-mode . hl-todo-mode)
     (conf-mode . hl-todo-mode))
    :config
    (setq hl-todo-highlight-punctuation ":"))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
