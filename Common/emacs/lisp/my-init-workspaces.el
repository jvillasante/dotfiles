;;; my-init-workspaces.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; otpp : One tab per project, with unique names
(use-package otpp
    :after project
    :preface
    (defun my/otpp-project-name (dir)
        "Get project name for DIR, with git worktree support.
For worktree checkouts, returns \"project » worktree\".
Otherwise, falls back to the default `otpp-project-name'."
        (let* ((default-name (otpp-project-name dir))
                  (proj (project-current nil dir))
                  (root (and proj (expand-file-name (project-root proj))))
                  (git-file (and root (expand-file-name ".git" root))))
            (if (and git-file (file-regular-p git-file))
                ;; .git is a file, so this is a worktree checkout
                (let* ((worktree-name (file-name-nondirectory (directory-file-name root)))
                          (git-content (with-temp-buffer
                                           (insert-file-contents git-file)
                                           (buffer-string)))
                          (gitdir (and (string-match "gitdir: \\(.+\\)" git-content)
                                      (expand-file-name (match-string 1 git-content) root)))
                          ;; Navigate from .git/worktrees/<name> up to the main repo
                          (main-git-dir (and gitdir (expand-file-name "../.." gitdir)))
                          (main-root (and main-git-dir
                                         (or
                                             ;; Bare repo: .bare is in the project root
                                             (let ((bare-parent (file-name-directory (directory-file-name main-git-dir))))
                                                 (and (file-exists-p (expand-file-name ".bare" bare-parent))
                                                     bare-parent))
                                             ;; Standard worktree: .git dir is inside the main working tree
                                             (file-name-directory (directory-file-name main-git-dir)))))
                          (project-name (and main-root
                                            (file-name-nondirectory (directory-file-name main-root)))))
                    (if project-name
                        (format "%s » %s" project-name worktree-name)
                        (or default-name (file-name-nondirectory (directory-file-name root)))))
                (or default-name (and root (file-name-nondirectory (directory-file-name root)))))))
    :custom
    (otpp-project-name-function #'my/otpp-project-name)
    :hook (after-init . (lambda ()
                            ;; Enable `otpp-mode` globally
                            (otpp-mode 1)
                            ;; If you want to advice the commands in `otpp-override-commands`
                            ;; to be run in the current's tab (so, current project's) root directory
                            (otpp-override-mode 1))))

;; tabspaces : create buffer-isolated workspaces
(use-package tabspaces
    :disabled t
    :preface
    (defun my/tabspace-setup ()
        "Set up tabspace at startup."
        ;; Add *Messages* and *splash* to Tab \`Default\'
        (tabspaces-mode 1)
        (progn
            (tab-bar-rename-tab "Default")
            (when (get-buffer "*Messages*")
                (set-frame-parameter nil
                    'buffer-list
                    (cons (get-buffer "*Messages*")
                        (frame-parameter nil 'buffer-list))))
            (when (get-buffer "*splash*")
                (set-frame-parameter nil
                    'buffer-list
                    (cons (get-buffer "*splash*")
                        (frame-parameter nil 'buffer-list))))))

    (defun my/consult-tabspaces ()
        "Deactivate isolated buffers when not using tabspaces."
        (require 'consult)
        (cond (tabspaces-mode
                  ;; hide full buffer list (still available with "b")
                  (consult-customize consult--source-buffer :hidden t :default nil)
                  (add-to-list 'consult-buffer-sources 'consult--source-workspace))
            (t
                ;; reset consult-buffer to show all buffers
                (consult-customize consult--source-buffer :hidden nil :default t)
                (setq consult-buffer-sources (remove #'consult--source-workspace consult-buffer-sources)))))
    :commands
    (tabspaces-switch-or-create-workspace
        tabspaces-open-or-create-project-and-workspace)
    :hook
    ((after-init . my/tabspace-setup)
        (tabspaces-mode . my/consult-tabspaces))
    :init
    (with-eval-after-load 'consult
        ;; hide full buffer list (still available with "b" prefix)
        (consult-customize consult--source-buffer :hidden t :default nil)
        ;; set consult-workspace buffer list
        (defvar consult--source-workspace
            (list :name     "Workspace Buffers"
                :narrow   ?w
                :history  'buffer-name-history
                :category 'buffer
                :state    #'consult--buffer-state
                :default  t
                :items    (lambda () (consult--buffer-query
                                         :predicate #'tabspaces--local-buffer-p
                                         :sort 'visibility
                                         :as #'buffer-name)))

            "Set workspace buffer list for consult-buffer.")
        (add-to-list 'consult-buffer-sources 'consult--source-workspace))
    :custom
    (tabspaces-keymap-prefix "C-c TAB")
    (tabspaces-use-filtered-buffers-as-default t)
    (tabspaces-default-tab "Default")
    (tabspaces-remove-to-default t)
    (tabspaces-include-buffers '("*scratch*"))
    (tabspaces-initialize-project-with-todo nil)
    (tabspaces-todo-file-name "project-todo.org")
    ;; sessions
    (tabspaces-session nil)
    (tabspaces-session-auto-restore nil)
    (tab-bar-new-tab-choice "*scratch*"))

(provide 'my-init-workspaces)
;;; my-init-workspaces.el ends here
