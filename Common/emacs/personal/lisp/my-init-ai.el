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
    :disabled t
    :defer t
    :custom
    ((chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))))

(use-package copilot
    :preface
    (defun my--maybe-start-copilot ()
        "Exlude some modes from copilot."
        (let ((disabled-modes '(lisp-interaction-mode)))
            (unless (apply 'derived-mode-p disabled-modes)
                (copilot-mode))))
    :hook (;; (prog-mode . my--maybe-start-copilot)
           (copilot-mode . (lambda ()
                               (setq-local copilot--indent-warning-printed-p t))))
    :bind (:map copilot-completion-map
                ("<tab>" . 'copilot-accept-completion)
                ("TAB" . 'copilot-accept-completion)))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
