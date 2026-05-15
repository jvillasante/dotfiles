;;; my-init-vcs.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(require 'cl-lib)

;; vc : builtin emacs version control
(use-package vc
    :ensure nil ;; emacs built-in
    :bind ("C-x v R" . my/vc-git-reflog)
    :custom
    (vc-handled-backends '(Git))
    (vc-follow-symlinks t)                   ; always follow symlinks
    (vc-git-diff-switches '("--histogram"))  ; use a different diff option
    (vc-ignore-dir-regexp                    ; make sure vc stuff is not making tramp slower
        (format "\\(%s\\)\\|\\(%s\\)"
            vc-ignore-dir-regexp
            tramp-file-name-regexp)))

;; eldoc-diffstat : provides a way to display VCS diffstat information via eldoc.
(use-package eldoc-diffstat
    :disabled t
    :hook (after-init . global-eldoc-diffstat-mode))

;; diff-hl : highlights uncommitted changes on the left side
(use-package diff-hl
    :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
              (after-init . (lambda ()
                                (global-diff-hl-mode 1)
                                (unless (display-graphic-p)
                                    (diff-hl-margin-mode 1)))))
    :custom (diff-hl-disable-on-remote t))

;; magit : A Git Porcelain inside Emacs
(use-package magit
    :preface
    (defun my/magit-kill-buffers ()
        "Restore window configuration and kill all Magit buffers."
        (interactive)
        (let ((buffers (magit-mode-get-buffers)))
            (magit-restore-window-configuration)
            (mapc #'kill-buffer buffers)))
    :bind (("C-x g" . magit-status)
              :map magit-status-mode-map
              ("q"     . my/magit-kill-buffers)
              ("C-x k" . my/magit-kill-buffers)
              :map project-prefix-map
              ("m" . magit-project-status))
    :custom ((git-commit-summary-max-length 50)
                (git-commit-fill-column 72)
                (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
                (magit-diff-refine-hunk 'all) ; show word-granularity differences within diff hunks.
                (magit-save-repository-buffers nil)
                (magit-define-global-key-bindings nil))
    :config
    (add-hook 'magit-status-sections-hook #'magit-insert-worktrees t))

;; Generic worktree-with-activities workflow.
;;
;; A "worktree family" describes one project that uses git worktrees under a
;; shared base directory.  Each registered family gets:
;;   * `my/<NAME>-worktree-add' - interactive command to create a worktree,
;;     symlink personal (non-tracked) config files into it, and open it
;;     inside a new "<NAME>/<worktree>" activity (when `activities' loads).
;;   * Refusal to delete worktrees listed in the family's `protected' field.
;;   * Activity cleanup when a worktree is deleted.
;;   * RET on a worktree section in magit-status routed to the corresponding
;;     activity (resumed or created on first visit), unless the worktree
;;     basename is in the family's `skip-visit' field.
;;   * A suffix under the `magit-worktree' transient with the family's
;;     `transient-key' / `transient-label'.
;;
;; Adding another family is a single `my/worktree-register-family' call;
;; see the nntp registration at the bottom of this section as a template.

(cl-defstruct my/worktree-family
    "Per-project configuration for the worktree-with-activities workflow."
    name              ;; string, also used as the activity-name prefix
    base-dir          ;; parent directory containing the worktrees
    dotfiles-dir      ;; directory holding personal (non-tracked) config files
    config-files      ;; alist of (SOURCE . DEST) symlinks, paths relative to dotfiles-dir / worktree
    source-worktree   ;; basename of an existing worktree to run `git worktree add' from
    protected         ;; list of basenames that must not be deleted (e.g. ".bare", "master")
    skip-visit        ;; list of basenames where RET falls through to magit's default
    transient-key     ;; key string under `magit-worktree' transient for the add command
    transient-label)  ;; label shown for that transient suffix

(defvar my/worktree-families nil
    "List of registered `my/worktree-family' records.")

(defun my/worktree-find-family (worktree)
    "Return the registered family whose base contains WORKTREE, or nil."
    (let ((wt (file-name-as-directory (expand-file-name worktree))))
        (cl-find-if
            (lambda (f)
                (string-prefix-p
                    (file-name-as-directory
                        (expand-file-name (my/worktree-family-base-dir f)))
                    wt))
            my/worktree-families)))

(defun my/worktree-add (family name start-point &optional detach)
    "Create a worktree under FAMILY named NAME based on START-POINT.
With non-nil DETACH, check out a detached HEAD (useful for code reviews)
instead of creating a new branch.

When `activities' is loaded, also create an activity named
\"<FAMILY-NAME>/NAME\" and switch to the project at the worktree root
inside it."
    (let* ((base (my/worktree-family-base-dir family))
              (source (expand-file-name (my/worktree-family-source-worktree family) base))
              (wt-path (expand-file-name name base))
              (dotfiles (my/worktree-family-dotfiles-dir family))
              (activity-name (and (featurep 'activities)
                                 (format "%s/%s" (my/worktree-family-name family) name)))
              (args (if detach
                        (list "worktree" "add" wt-path start-point)
                        (list "worktree" "add" "-b" name wt-path start-point))))
        ;; Pre-flight: bail before any side effects so we never leave a
        ;; half-created worktree on disk if a later step would fail.
        (when (file-exists-p wt-path)
            (user-error "Worktree %s already exists" wt-path))
        (when (and activity-name (activities-named activity-name))
            (user-error "Activity %s already exists" activity-name))
        ;; Create the worktree (new branch for development, or detached
        ;; HEAD for reviews).
        (let ((default-directory source))
            (apply #'magit-run-git args)
            (message "Created worktree at %s" wt-path))
        ;; Set upstream on new branches so plain `git rebase' works.
        (unless detach
            (let ((default-directory wt-path))
                (magit-run-git "branch" (format "--set-upstream-to=%s" start-point) name)))
        ;; Symlink personal config files into the worktree (intentionally
        ;; not tracked by the upstream repo).
        (dolist (entry (my/worktree-family-config-files family))
            (let ((src (expand-file-name (car entry) dotfiles))
                     (dst (expand-file-name (cdr entry) wt-path)))
                (make-symbolic-link src dst)))
        (message "Symlinked config files into %s" wt-path)
        ;; Open the worktree as a project, in a dedicated activity when
        ;; activities is available.
        (cond
            (activity-name
                (activities-new activity-name)
                (project-switch-project wt-path)
                (message "Created activity %s on %s" activity-name wt-path))
            (t
                (project-switch-project wt-path)
                (message "Created worktree %s (activities not available)" wt-path)))))

(defun my/worktree-protect (worktree &rest _)
    "Refuse to delete WORKTREE if its basename is in its family's `protected' list.
Intended as `:before' advice on `magit-worktree-delete'.  No-op for
worktrees outside any registered family."
    (when-let* ((family (my/worktree-find-family worktree)))
        (let ((wt-name (file-name-nondirectory (directory-file-name worktree))))
            (when (member wt-name (my/worktree-family-protected family))
                (user-error "Refusing to delete protected %s worktree: %s"
                    (my/worktree-family-name family) wt-name)))))

(defun my/worktree-discard-activity (worktree &rest _)
    "Discard the activity associated with WORKTREE in its family, if any.
Intended as `:after' advice on `magit-worktree-delete'.  No-op when the
activities package is not loaded or WORKTREE is not in a registered family."
    (when (featurep 'activities)
        (when-let* ((family (my/worktree-find-family worktree)))
            (let* ((wt-name (file-name-nondirectory (directory-file-name worktree)))
                      (activity-name (format "%s/%s" (my/worktree-family-name family) wt-name))
                      (activity (activities-named activity-name)))
                (when activity
                    (when (activities-activity-active-p activity)
                        (activities-close activity))
                    (setf activities-activities
                        (map-delete activities-activities activity-name))
                    (activities--persist)
                    (message "Discarded activity %s" activity-name))))))

(defun my/worktree-visit (orig-fn worktree &rest args)
    "Around advice for `magit-worktree-status' with parameters ORIG-FN, WORKTREE and ARGS.
For worktrees in a registered family (and not in its `skip-visit' list),
switch to the corresponding activity, resuming an existing one or
creating it on first visit.  All other worktrees fall through to
ORIG-FN so RET behaves as normal magit there."
    (let* ((family (my/worktree-find-family worktree))
              (wt-name (file-name-nondirectory (directory-file-name worktree))))
        (if (and (featurep 'activities)
                family
                (not (member wt-name (my/worktree-family-skip-visit family))))
            (let* ((activity-name (format "%s/%s" (my/worktree-family-name family) wt-name))
                      (activity (activities-named activity-name)))
                (cond
                    (activity
                        (activities-resume activity)
                        (message "Switched to activity %s" activity-name))
                    (t
                        (activities-new activity-name)
                        (project-switch-project worktree)
                        (message "Created activity %s on %s" activity-name worktree))))
            (apply orig-fn worktree args))))

(defun my/worktree-register-family (family)
    "Register FAMILY.
Store it, define an interactive add command, and append a suffix to the `magit-worktree'
transient.  Replaces any prior registration with the same name (idempotent on re-eval)."
    (let* ((fam-name (my/worktree-family-name family))
              (cmd (intern (format "my/%s-worktree-add" fam-name)))
              (doc (format "Create a %s worktree.
Auto-generated wrapper around `my/worktree-add' for the `%s' family." fam-name fam-name)))
        ;; Replace any previous family registration with this name.
        (setq my/worktree-families
            (cons family
                (cl-remove fam-name my/worktree-families
                    :key #'my/worktree-family-name :test #'equal)))
        ;; Define an interactive M-x-able add command bound to this family.
        ;; The closure captures FAMILY by lexical scope.
        (defalias cmd
            (lambda (n sp &optional detach)
                (interactive
                    (list (read-string "Worktree name: ")
                        (magit-read-branch-or-commit "Starting point")
                        current-prefix-arg))
                (my/worktree-add family n sp detach))
            doc)
        ;; Append (or replace) the family's suffix under `magit-worktree'.
        (with-eval-after-load 'magit
            (ignore-errors
                (transient-remove-suffix 'magit-worktree
                    (my/worktree-family-transient-key family)))
            (transient-append-suffix 'magit-worktree "c"
                (list (my/worktree-family-transient-key family)
                    (my/worktree-family-transient-label family)
                    cmd)))))

(with-eval-after-load 'magit-worktree
    (advice-add 'magit-worktree-delete :before #'my/worktree-protect)
    (advice-add 'magit-worktree-delete :after  #'my/worktree-discard-activity)
    (advice-add 'magit-worktree-status :around #'my/worktree-visit))

;; Register the `nntp' family.  To add a new project, copy this block,
;; change every field, and pick a `transient-key' that doesn't collide
;; with an existing magit-worktree binding.
(my/worktree-register-family
    (make-my/worktree-family
        :name "nntp"
        :base-dir (expand-file-name "Projects/nntp" my/work-path)
        :dotfiles-dir (expand-file-name "Misc/work/nntp" my/dotfiles-path)
        :config-files '((".clang-tidy"               . ".clang-tidy")
                           (".dir-locals.el"           . ".dir-locals.el")
                           (".clangd"                  . ".clangd")
                           (".scripts"                 . ".scripts"))
        :source-worktree "master"
        :protected '(".bare" "master")
        :skip-visit '(".bare")
        :transient-key "a"
        :transient-label "Add nntp worktree"))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
