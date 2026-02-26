;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :disabled t
    :defer t
    :vc (:url "git@github.com:karthink/gptel.git"
              :rev :newest)
    :hook ((gptel-post-stream . gptel-auto-scroll)
           (gptel-mode . visual-line-mode))
    :bind (:map gptel-mode-map
                ("C-c C-c" . gptel-send))
    :custom
    (gptel-default-mode 'org-mode)
    (gptel-expert-commands t)
    (gptel-track-media t)
    (gptel-include-reasoning 'ignore)
    :config
    (setq
     gptel-model 'gemini-3-flash-preview
     gptel-backend (gptel-make-gemini "Gemini"
                                      :key (lambda ()
                                               (password-store-get-field "Work/Omicron/Gemini" "API Key"))
                                      :stream t)))

(use-package chatgpt-shell
    :disabled t
    :defer t
    :vc (:url "git@github.com:xenodium/chatgpt-shell.git"
              :rev :newest)
    :custom
    ((chatgpt-shell-model-version "gemini-3-flash-preview")
     (chatgpt-shell-google-key
      (lambda ()
          (password-store-get-field "Work/Omicron/Gemini" "API Key")))))

(use-package agent-shell
    :disabled t
    :defer t
    :vc (:url "https://github.com/xenodium/agent-shell.git"
              :rev :newest)
    :preface
    (defun my-agent-shell-transcript-file-path ()
        "Generate a transcript file path."
        (let* ((dir (expand-file-name "agent-shell/transcripts" my-var-dir))
               (filename (format-time-string "%F-%H-%M-%S.md"))
               (filepath (expand-file-name filename dir)))
            filepath))
    :custom
    (agent-shell-transcript-file-path-function
     #'my-agent-shell-transcript-file-path)
    (agent-shell-preferred-agent-config
     (agent-shell-anthropic-make-claude-code-config))
    (agent-shell-anthropic-authentication
     (agent-shell-anthropic-make-authentication :login t)))

(use-package agent-shell-sidebar
    :disabled t
    :defer t
    :vc (:url "https://github.com/cmacrae/agent-shell-sidebar"
              :rev :newest)
    :bind (("C-c a s" . agent-shell-sidebar-toggle)
           ("C-c a f" . agent-shell-sidebar-toggle-focus))
    :custom
    (agent-shell-sidebar-width "30%")
    (agent-shell-sidebar-minimum-width 80)
    (agent-shell-sidebar-maximum-width "50%")
    (agent-shell-sidebar-position 'right)
    (agent-shell-sidebar-locked t)
    (agent-shell-sidebar-default-config
     (agent-shell-anthropic-make-claude-code-config)))

(use-package claude-code-ide
    :defer t
    :vc (:url "https://github.com/manzaltu/claude-code-ide.el"
              :rev :newest)
    :bind (("C-c C-'" . claude-code-ide-menu)
           ("C-c a t" . claude-code-ide-toggle)
           ("C-c a s" . claude-code-ide-send-prompt)
           ("C-c a @" . claude-code-ide-insert-at-mentioned)
           ("C-c a r" . claude-code-ide-resume)
           ("C-c a c" . claude-code-ide-continue))
    :custom
    (claude-code-ide-window-side 'right)
    (claude-code-ide-window-width 90)
    (claude-code-ide-focus-on-open t)
    (claude-code-ide-diagnostics-backend 'flycheck)
    (claude-code-ide-use-ide-diff t)                    ; use ediff instead of terminal diff
    (claude-code-ide-show-claude-window-in-ediff nil)   ; hide Claude window while reviewing in ediff
    (claude-code-ide-focus-claude-after-ediff nil)      ; no Claude window to focus while ediff is open
    :config
    (claude-code-ide-emacs-tools-setup))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
