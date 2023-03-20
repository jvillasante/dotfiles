;;; my-init-vcs.el -*- lexical-binding: t; -*-

(straight-use-package 'magit)
(straight-use-package 'git-gutter)
(straight-use-package 'hl-todo)

;; always follow symlinks
(setq vc-follow-symlinks t)

(general-create-definer my/git-map
    :prefix "C-c g"
    :prefix-map 'my/git-map)

(use-package magit
    :init
    (my/git-map
        :keymaps 'override
        "" '(:ignore t :which-key "git")
        "g" #'magit
        "a" #'magit-file-dispatch
        "A" #'magit-dispatch)

    (setq git-commit-summary-max-length 50
        magit-diff-refine-hunk t ; show granular diffs in selected hunk.
        ;; Don't autosave repo buffers. This is too magical, and
        ;; saving can trigger a bunch of unwanted side-effects, like
        ;; save hooks and formatters. Trust the user to know what
        ;; they're doing.
        magit-save-repository-buffers nil
        magit-define-global-key-bindings nil)

    :config
    (add-to-list 'display-buffer-alist
        '("magit:" ;; the main magit dashboard
             ;; don't create new window if there is one magit window
             ;; and create new tab if there isn't one magit window.
             (display-buffer-reuse-window display-buffer-in-new-tab)))

    (general-define-key
        :keymaps 'magit-status-mode-map
        "gt" #'tab-bar-switch-to-next-tab
        ;; emulate C-g
        "<escape>" #'transient-quit-one))

(use-package git-gutter
    :commands (git-gutter:revert-hunk git-gutter:stage-hunk)
    :hook ((prog-mode . git-gutter-mode)
              (conf-mode . git-gutter-mode))

    :init
    (my/git-map
        :keymaps 'override
        "r" #'git-gutter:revert-hunk
        "s" #'git-gutter:stage-hunk)

    :config
    ;; (general-define-key
    ;;     "]h" #'git-gutter:next-hunk
    ;;     "[h" #'git-gutter:previous-hunk)

    (add-to-list 'display-buffer-alist
        '("\\*git-gutter"
             (display-buffer-below-selected)
             (window-height . 0.3))))

(use-package hl-todo
    :hook ((prog-mode . hl-todo-mode)
              (conf-mode . hl-todo-mode))
    :init
    (setq hl-todo-highlight-punctuation ":")
    (my/git-map
        :keymaps 'override
        "t" #'my/project-todos))

(provide 'my-init-vcs)
;;; my-init-vcs.el ends here
