;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package chatgpt-shell
    :disabled t
    :defer t
    :custom
    ((chatgpt-shell-model-version "gemini-3-flash-preview")
        (chatgpt-shell-google-key
            (lambda ()
                (password-store-get-field "Work/Omicron/Gemini" "API Key")))))

(use-package agent-shell
    :defer t
    :preface
    (defun my/agent-shell-dot-subdir (subdir)
        (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
                  (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
            (expand-file-name subdir (expand-file-name
                                         (concat "agent-shell/" sanitized) my/var-dir))))
    :bind (("C-c a s" . agent-shell)
              ("C-c a t" . agent-shell-toggle)
              ("C-c a b" . agent-shell-switch-buffer)
              ("C-c a n" . agent-shell-new-shell)
              ("C-c a o" . agent-shell-other-buffer))
    :hook
    (agent-shell-viewport-edit-mode . turn-off-auto-fill)
    :custom
    (agent-shell-session-strategy 'prompt)
    (agent-shell-session-restore-verbosity 'last)
    (agent-shell-prefer-viewport-interaction t)
    (agent-shell-dot-subdir-function #'my/agent-shell-dot-subdir)
    (agent-shell-preferred-agent-config 'claude-code)
    (agent-shell-anthropic-default-model-id "opus[1m]")
    (agent-shell-anthropic-default-session-mode-id "default")
    (agent-shell-screenshot-command '("spectacle" "--region" "--background" "--nonotify" "--output"))
    (agent-shell-show-usage-at-turn-end t)
    :init
    (setq switch-to-buffer-obey-display-actions t)
    (add-to-list 'display-buffer-alist
        '("Agent @"
             (display-buffer-in-side-window)
             (side . right)
             (slot . 0)
             (window-width . 0.4)
             (dedicated . t)
             (window-parameters . ((no-delete-other-windows . t))))))

(use-package claude-code-ide
    :disabled t
    :defer t
    :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
    :bind (("C-c C-'" . claude-code-ide-menu)
              ("C-c a t" . claude-code-ide-toggle)
              ("C-c a s" . claude-code-ide-send-prompt)
              ("C-c a @" . claude-code-ide-insert-at-mentioned)
              ("C-c a r" . claude-code-ide-resume)
              ("C-c a c" . claude-code-ide-continue)
              ("C-c a k" . claude-code-ide-stop)
              ("C-c a l" . claude-code-ide-list-sessions)
              ("C-c a b" . claude-code-ide-switch-to-buffer))
    :custom
    (claude-code-ide-terminal-backend 'ghostel)
    (claude-code-ide-cli-extra-flags "--model opus")
    (claude-code-ide-use-side-window t)
    (claude-code-ide-window-side 'right)
    (claude-code-ide-window-width 90)
    (claude-code-ide-focus-on-open t)
    (claude-code-ide-focus-claude-after-ediff nil)
    (claude-code-ide-diagnostics-backend 'flymake)
    (claude-code-ide-use-ide-diff t)                    ; use ediff instead of terminal diff
    (claude-code-ide-show-claude-window-in-ediff nil)   ; hide Claude window while reviewing in ediff
    (claude-code-ide-switch-tab-on-ediff t)             ; yank me to Claude's tab when ediff opens
    :config
    (claude-code-ide-emacs-tools-setup))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
