;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :preface
    (defvar my--gptel-define-word-prompt
        "Please give a short definition of this word or phrase. Then, provide 3 usage examples, synonyms and antonyms"
        "The ChatGPT style prompt used define a word.")

    (defun my--gptel-stash-response (buffer prompt response)
        "Store a response in a well known buffer we can look at if we want"
        (let ((buffer (get-buffer-create buffer)))
            (with-current-buffer buffer
                (erase-buffer)
                (insert prompt)
                (insert "\n\n-->\n\n")
                (insert response))))

    (defun my--gptel-define-word (start end)
        "Use ChatGPT to define the current word of the region."
        (interactive "r")
        (unless (region-active-p)
            (error "You must have a region set"))
        (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
            (gptel-request nil
                :callback (lambda (response info)
                              (my--gptel-stash-response "*Last Definition*" (plist-get info :context) response)
                              (message response))
                :system my--gptel-define-word-prompt
                :context input)))
    :custom
    (gptel-default-mode 'org-mode)
    (gptel-expert-commands t)
    :config
    (setq gptel-model "gpt-4o")
    (setq gptel-api-key
          (password-store-get-field "Logins/openai.com" "API Key")))

(use-package gptel-quick
    :vc (:url "git@github.com:karthink/gptel-quick.git"
              :rev :newest)
    :config
    (keymap-set embark-general-map "?" #'gptel-quick))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
