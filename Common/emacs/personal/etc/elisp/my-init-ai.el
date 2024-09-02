;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :custom
    (gptel-default-mode 'org-mode)
    (gptel-expert-commands t)
    :config
    ;; (setq gptel-model "gpt-4o")
    (setq
     gptel-model "claude-3-sonnet-20240229"
     gptel-backend (gptel-make-anthropic "Claude"
                       :stream t
                       :key (auth-source-pick-first-password :host "console.claude.com"))))

(use-package gptel-quick
    :defer t
    :vc (:url "https://github.com/karthink/gptel-quick"
              :rev :newest)
    :config
    (keymap-set embark-general-map "?" #'gptel-quick))

;; Define word or phrase
(use-package gptel
    :defer t
    :config
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
                :context input))))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
