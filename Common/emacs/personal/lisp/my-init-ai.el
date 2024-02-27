;;; my-init-ai.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :bind (:map gptel-mode-map
                ("C-c C-c" . 'gptel-send))
    :custom
    ((gptel-default-mode 'org-mode)))

(use-package chatgpt-shell
    :defer t
    :custom
    ((chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))))

(use-package copilot
    :hook ((prog-mode . copilot-mode)
           (copilot-mode . (lambda ()
                               (setq-local copilot--indent-warning-printed-p t))))
    :bind (:map copilot-completion-map
                ("<tab>" . 'copilot-accept-completion)
                ("TAB" . 'copilot-accept-completion)))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
