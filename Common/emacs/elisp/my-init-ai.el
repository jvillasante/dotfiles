;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :vc (:url "git@github.com:karthink/gptel.git"
              :rev :newest)
    :custom
    (gptel-default-mode 'org-mode)
    (gptel-expert-commands t)
    (gptel-track-media t)
    (gptel-include-reasoning 'ignore)
    :hook
    (gptel-post-stream . gptel-auto-scroll)
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
    ((chatgpt-shell-model-version "gemini-2.5-flash-preview-04-17")
     (chatgpt-shell-google-key
      (lambda ()
          (password-store-get-field "Work/Omicron/Gemini" "API Key")))
     (chatgpt-shell-openai-key
      (lambda ()
          (password-store-get-field "Logins/openai.com" "API Key")))))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
