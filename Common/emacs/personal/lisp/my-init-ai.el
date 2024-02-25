;;; my-init-ai.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package chatgpt-shell
    :custom
    ((chatgpt-shell-openai-key
      (lambda ()
          (string-trim (nth 4 (process-lines "pass" "show" "Logins/openai.com")))))))

(provide 'my-init-ai)
;;; my-init-ai.el ends here
