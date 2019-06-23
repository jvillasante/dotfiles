(with-eval-after-load 'magit
    (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
    (setq-default git-magit-status-fullscreen t)
    (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil))