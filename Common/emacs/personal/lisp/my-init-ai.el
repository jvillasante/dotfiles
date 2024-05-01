;;; my-init-ai.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :custom ((gptel-default-mode 'org-mode))
    :config
    (setq gptel-expert-commands t)
    (setq gptel-model "llama3:latest"
          gptel-backend (gptel-make-ollama "Ollama"
                            :host "localhost:11434"
                            :stream t
                            :models '("llama3:latest"))))

(use-package chatgpt-shell
    :disabled t
    :defer t
    :custom
    ((chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))))

(use-package copilot
    :disabled t
    :preface
    (defun my--maybe-start-copilot ()
        "Exclude some modes from copilot."
        (let ((disabled-modes '(lisp-interaction-mode)))
            (unless (apply 'derived-mode-p disabled-modes)
                (copilot-mode))))
    :hook
    ((prog-mode . my--maybe-start-copilot)
     (copilot-mode . (lambda ()
                         (setq-local copilot--indent-warning-printed-p t)))))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
