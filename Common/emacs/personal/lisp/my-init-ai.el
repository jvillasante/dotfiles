;;; my-init-ai.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :custom
    ((gptel-default-mode 'org-mode)))

(use-package chatgpt-shell
    :defer t
    :custom
    ((chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))))

(use-package copilot
    :hook (prog-mode . copilot-mode)
    :defer t
    :config
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
