;;; my-init-ai.el --- AI Stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package agent-shell
    :disabled t
    :preface
    (defun my/agent-shell-dot-subdir (subdir)
        (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
                  (sanitized (replace-regexp-in-string "/" "-"
                                 (string-remove-prefix "/" cwd))))
            (expand-file-name subdir (expand-file-name
                                         (concat "agent-shell/" sanitized)
                                         my/var-dir))))
    :bind (("C-c a s" . agent-shell)
              ("C-c a t" . agent-shell-toggle)
              ("C-c a b" . agent-shell-switch-buffer)
              ("C-c a n" . agent-shell-new-shell)
              ("C-c a o" . agent-shell-other-buffer)
              ("C-c a r" . agent-shell-resume-session)
              ("C-c a c" . agent-shell-prompt-compose)
              ("C-c a f" . agent-shell-fork)
              ("C-c a k" . agent-shell-interrupt)
              ("C-c a w" . agent-shell-new-worktree-shell))
    :hook
    (agent-shell-viewport-edit-mode . turn-off-auto-fill)
    :custom
    (agent-shell-session-strategy 'prompt)
    (agent-shell-session-restore-verbosity 'last)
    (agent-shell-prefer-viewport-interaction t)
    (agent-shell-display-action
        '(display-buffer-in-side-window
             (side . right)
             (slot . 0)
             (dedicated . t)
             (window-width . 0.4)))
    (agent-shell-dot-subdir-function #'my/agent-shell-dot-subdir)
    (agent-shell-preferred-agent-config '(auto . claude-code))
    (agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))
    (agent-shell-anthropic-default-model-id "opus[1m]")
    (agent-shell-anthropic-default-session-mode-id "default")
    (agent-shell-screenshot-command '("spectacle" "--region" "--background"
                                         "--nonotify" "--output"))
    (agent-shell-show-usage-at-turn-end t))

;; eca :
(use-package eca
    :bind (("C-c a s"    . eca)                               ; start eca
              ("C-c a t" . eca-chat-toggle-window)            ; toggle chat window
              ("C-c a b" . eca-switch-to-chat)                ; switch between chat buffers
              ("C-c a n" . eca-chat-new)                      ; new chat
              ("C-c a o" . eca-chat-select)                   ; select/switch chats (annotated)
              ("C-c a r" . eca-chat-resume)                   ; resume persisted chat
              ("C-c a c" . eca-chat-send-prompt)              ; send prompt from minibuffer
              ("C-c a f" . eca-chat-fork)                     ; fork current chat
              ("C-c a k" . eca-chat-stop-prompt)              ; stop/interrupt running prompt
              ("C-c a w" . eca-rewrite)                       ; rewrite region/defun with AI
              ("C-c a @" . eca-chat-add-context-to-user-prompt) ; add file/region context
              ("C-c a l" . eca-workspaces)                    ; open workspaces dashboard
              ("C-c C-'" . eca-transient-menu)                  ; discoverable transient menu
              :map eca-chat-mode-map
              ("M-p" . eca-chat-go-to-prev-expandable-block)  ; jump to prev tool call / block
              ("M-n" . eca-chat-go-to-next-expandable-block)) ; jump to next tool call / block
    :hook
    (eca-chat-mode . turn-off-auto-fill)
    :custom
    ;; --- Chat window (mirrors agent-shell display-action) ---
    (eca-chat-use-side-window t)
    (eca-chat-window-side 'right)
    (eca-chat-window-width 0.40)
    (eca-chat-focus-on-open t)
    ;; --- Diff: ediff takes over the frame for full-screen review ---
    (eca-chat-diff-tool 'ediff)                               ; full-screen side-by-side ediff
    (eca-rewrite-diff-tool 'ediff)                            ; ediff for inline rewrites too
    ;; --- Chat behavior ---
    (eca-chat-read-only-history t)
    (eca-chat-hide-markdown-markup t)
    (eca-chat-tab-line t)
    (eca-chat-table-beautify t)
    (eca-chat-expand-pending-approval-tools t)
    (eca-chat-shrink-called-tools t)
    ;; --- Context ---
    (eca-chat-auto-add-cursor t)                              ; auto-track cursor position
    (eca-chat-auto-asdd-repomap nil)                           ; don't auto-include repomap
    ;; --- Completion ---
    (eca-completion-idle-delay 0.2)
    (eca-completion-syntax-highlight t))

(use-package claude-code-ide
    :disabled t
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
