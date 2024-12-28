;;; my-init-vcs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; always follow symlinks
(setq vc-follow-symlinks t)
;; make sure vc stuff is not making tramp slower
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
;; use a different diff option
(setq vc-git-diff-switches '("--histogram"))

(use-package magit
    :preface
    (defun my/magit-kill-buffers ()
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

;; eldoc-diffstat : provides a way to display VCS diffstat information via eldoc.
(use-package eldoc-diffstat
    :hook (after-init . global-eldoc-diffstat-mode))

;; diff-hl : highlights uncommitted changes on the left side
(use-package diff-hl
    :preface
    (defun my/diff-hl-set-reference ()
        "Set the reference revision for showing `diff-hl' changes.
Do so buffer-locally."
        (interactive)
        (setq-local
         diff-hl-reference-revision
         (read-string
          (format "Set reference revision (buffer %s): "
                  (buffer-name))))
        (diff-hl-update))
    :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
           (magit-post-refresh . diff-hl-magit-post-refresh)
           ;; (dired-mode . diff-hl-dired-mode-unless-remote)
           (after-init . global-diff-hl-mode))
    :custom (diff-hl-disable-on-remote t))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
