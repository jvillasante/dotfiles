(defconst jv-magit-packages
    '(magit))

(defun jv-magit/post-init-magit ()
    (with-eval-after-load 'magit
        (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
        (setq-default git-magit-status-fullscreen t)
        (setq magit-completing-read-function 'magit-builtin-completing-read
            magit-push-always-verify nil))

    (evil-leader/set-key
        "gg" 'magit-status))