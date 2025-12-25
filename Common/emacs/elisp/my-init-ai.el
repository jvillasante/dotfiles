;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :vc (:url "git@github.com:karthink/gptel.git"
              :rev :newest)
    :hook (gptel-post-stream . gptel-auto-scroll)
    :bind (:map gptel-mode-map
                ("C-c C-c" . gptel-send))
    :custom
    (gptel-default-mode 'org-mode)
    (gptel-expert-commands t)
    (gptel-track-media t)
    (gptel-include-reasoning 'ignore)
    :config
    (setq
     gptel-model 'gemini-2.5-pro
     gptel-backend (gptel-make-gemini "Gemini"
                       :key (lambda ()
                                (password-store-get-field "Work/Omicron/Gemini" "API Key"))
                       :stream t))
    (setq
     ;; gptel-model 'gpt-40
     gptel-api-key
     (lambda ()
         (password-store-get-field "Logins/openai.com" "API Key"))))

(use-package chatgpt-shell
    :disabled t
    :defer t
    :custom
    ((chatgpt-shell-model-version "gemini-2.5-pro")
     (chatgpt-shell-google-key
      (lambda ()
          (password-store-get-field "Work/Omicron/Gemini" "API Key")))
     (chatgpt-shell-openai-key
      (lambda ()
          (password-store-get-field "Logins/openai.com" "API Key")))))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
