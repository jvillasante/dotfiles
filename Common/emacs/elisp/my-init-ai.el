;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :custom
    (gptel-default-mode 'markdown-mode)
    (gptel-expert-commands t)
    :config
    (gptel-make-gemini "Gemini"
        :key (lambda () (password-store-get-field "Work/Omicron/Gemini" "API Key"))
        :stream t)
    ;; (setq gptel-model 'gpt-4o)
    (setq gptel-model 'gpt-4o-mini)
    (setq gptel-api-key
          (lambda ()
              (password-store-get-field "Logins/openai.com" "API Key"))))

(use-package gptel-quick
    :disabled t
    :defer t
    :after gptel
    :vc (:url "git@github.com:karthink/gptel-quick.git"
              :rev :newest)
    :config
    (keymap-set embark-general-map "?" #'gptel-quick))

(use-package chatgpt-shell
    :disabled t
    :defer t
    :custom
    ((chatgpt-shell-openai-key
      (lambda ()
          (password-store-get-field "Logins/openai.com" "API Key")))))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
