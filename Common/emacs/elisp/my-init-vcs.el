;;; my-init-vcs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package vc
    :bind ("C-x v R" . my-vc-git-reflog)
    :custom
    (vc-handled-backends '(Git))
    (vc-follow-symlinks t)                   ; always follow symlinks
    (vc-git-diff-switches '("--histogram"))  ; use a different diff option
    (vc-ignore-dir-regexp                    ; make sure vc stuff is not making tramp slower
     (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))

(use-package magit
    :preface
    (defun my-magit-kill-buffers ()
        "Restore window configuration and kill all Magit buffers."
        (interactive)
        (let ((buffers (magit-mode-get-buffers)))
            (magit-restore-window-configuration)
            (mapc #'kill-buffer buffers)))
    :custom ((git-commit-summary-max-length 50)
             (git-commit-fill-column 72)
             (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
             (magit-define-global-key-bindings nil)
             (magit-diff-refine-hunk 'all) ; show word-granularity differences within diff hunks.
             (magit-save-repository-buffers nil)
             (magit-define-global-key-bindings nil)))

;; eldoc-diffstat : provides a way to display VCS diffstat information via eldoc.
(use-package eldoc-diffstat
    :hook (after-init . global-eldoc-diffstat-mode))

;; diff-hl : highlights uncommitted changes on the left side
(use-package diff-hl
    :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
           ;; (dired-mode . diff-hl-dired-mode-unless-remote)
           (after-init . global-diff-hl-mode))
    :custom (diff-hl-disable-on-remote t))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
