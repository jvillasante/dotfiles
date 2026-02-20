;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
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
    :defer t
    :vc (:url "git@github.com:xenodium/chatgpt-shell.git"
              :rev :newest)
    :custom
    ((chatgpt-shell-model-version "gemini-3-flash-preview")
     (chatgpt-shell-google-key
      (lambda ()
          (password-store-get-field "Work/Omicron/Gemini" "API Key")))))

(use-package agent-shell
    :defer t
    :config
    (setq agent-shell-transcript-file-path-function)
    (setq agent-shell-preferred-agent-config
          (agent-shell-anthropic-make-claude-code-config))
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication :login t)))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
