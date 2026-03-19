;;; my-init-vcs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; vc : builtin emacs version control
(use-package vc
    :ensure nil ;; emacs built-in
    :bind ("C-x v R" . my-vc-git-reflog)
    :custom
    (vc-handled-backends '(Git))
    (vc-follow-symlinks t)                   ; always follow symlinks
    (vc-git-diff-switches '("--histogram"))  ; use a different diff option
    (vc-ignore-dir-regexp                    ; make sure vc stuff is not making tramp slower
     (format "\\(%s\\)\\|\\(%s\\)"
             vc-ignore-dir-regexp
             tramp-file-name-regexp)))

;; magit : A Git Porcelain inside Emacs
(use-package magit
    :defer t
    :preface
    (defun my-magit-kill-buffers ()
        "Restore window configuration and kill all Magit buffers."
        (interactive)
        (let ((buffers (magit-mode-get-buffers)))
            (magit-restore-window-configuration)
            (mapc #'kill-buffer buffers)))
    :bind (("C-x g" . magit-status)
           :map magit-status-mode-map
           ("q"     . my-magit-kill-buffers)
           ("C-x k" . my-magit-kill-buffers)
           :map project-prefix-map
           ("m" . magit-project-status))
    :custom ((git-commit-summary-max-length 50)
             (git-commit-fill-column 72)
             (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
             (magit-diff-refine-hunk 'all) ; show word-granularity differences within diff hunks.
             (magit-save-repository-buffers nil)
             (magit-define-global-key-bindings nil))
    :config
    (add-hook 'magit-status-sections-hook #'magit-insert-worktrees t))

;; nntp worktree helper - create a worktree and seed it with personal config
;; files that are not tracked by git.
(defvar my-nntp-worktree-base (expand-file-name "Projects/nntp" my-work-path)
    "Parent directory where nntp worktrees live.")

(defvar my-nntp-dotfiles-dir (expand-file-name "Misc/work/nntp" my-dotfiles-path)
    "Directory containing personal config files to copy into new worktrees.")

(defvar my-nntp-config-files '((".clang-tidy"               . ".clang-tidy")
                                (".dir-locals.el"           . ".dir-locals.el")
                                ("compile_flags.fedora.txt" . "compile_flags.txt")
                                (".scripts"                 . ".scripts"))
    "Alist of (SOURCE . DEST) for symlinks from dotfiles into each new worktree.
SOURCE is relative to `my-nntp-dotfiles-dir', DEST is relative to the worktree.")

;; Make the master worktree read-only to prevent accidental edits.
(dir-locals-set-class-variables 'nntp-master-readonly
    '((nil . ((buffer-read-only . t)))))
(dir-locals-set-directory-class
    (expand-file-name "master" my-nntp-worktree-base) 'nntp-master-readonly)

(defun my-nntp-worktree-add (name start-point &optional detach)
    "Create an nntp worktree called NAME based on START-POINT.
With prefix argument DETACH, use detached HEAD (useful for code reviews)."
    (interactive
     (let ((n (read-string "Worktree name: "))
           (sp (magit-read-branch-or-commit "Starting point")))
         (list n sp current-prefix-arg)))
    (let* ((base (expand-file-name my-nntp-worktree-base))
           (master (expand-file-name "master" base))
           (wt-path (expand-file-name name base))
           (dotfiles (expand-file-name my-nntp-dotfiles-dir))
           (args (if detach
                     (list "worktree" "add" wt-path start-point)           ;; detached HEAD for reviews
                   (list "worktree" "add" "-b" name wt-path start-point)))) ;; new branch for development
        (when (file-exists-p wt-path)
            (user-error "Worktree %s already exists" wt-path))
        ;; Create the worktree
        (let ((default-directory master))
            (apply #'magit-run-git args)
            (message "Created worktree at %s" wt-path))
        ;; Set upstream for easy rebasing (only for branches, not detached)
        (unless detach
            (let ((default-directory wt-path))
                (magit-run-git "branch" (format "--set-upstream-to=%s" start-point) name)))
        ;; Symlink personal config files
        (dolist (entry my-nntp-config-files)
            (let ((src (expand-file-name (car entry) dotfiles))
                  (dst (expand-file-name (cdr entry) wt-path)))
                (make-symbolic-link src dst)))
        (message "Symlinked config files into %s" wt-path)))

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
