(with-eval-after-load 'magit
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (setq-default git-magit-status-fullscreen t)
    (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil)

    (setq
        ;; magit-repository-directories '(("~/work" . 2))
        ;; magit-commit-arguments '("--gpg-sign=5F6C0EA160557395")
        ;; magit-rebase-arguments '("--autostash" "--gpg-sign=5F6C0EA160557395")
        ;; magit-pull-arguments   '("--rebase" "--autostash" "--gpg-sign=5F6C0EA160557395")
        +magit-hub-features t
        git-commit-summary-max-length 80
        vc-handled-backends (delq 'Git vc-handled-backends))

    ;; Temporary workaround for +magit/quit hang with lots of buffers
    (define-key magit-status-mode-map [remap magit-mode-bury-buffer] nil))
