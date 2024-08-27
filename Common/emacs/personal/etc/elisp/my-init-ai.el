;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :custom ((gptel-default-mode 'org-mode)
             (gptel-expert-commands t)
             (gptel-model "gpt-4o")))

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
