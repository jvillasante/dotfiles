;;; my-init-ai.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(use-package gptel
    :defer t
    :preface
    (defvar my/gptel-define-word-prompt
        "Please give a short definition of this word or phrase. Then, provide 3 usage examples, synonyms and antonyms"
        "The ChatGPT style prompt used define a word.")

    (defun my/gptel-stash-response (buffer prompt response)
        "Store a response in a well known buffer we can look at if we want"
        (let ((buffer (get-buffer-create buffer)))
            (with-current-buffer buffer
                (erase-buffer)
                (insert prompt)
                (insert "\n\n-->\n\n")
                (insert response))))

    (defun my/gptel-define-word (start end)
        "Use ChatGPT to define the current word of the region."
        (interactive "r")
        (unless (region-active-p)
            (error "You must have a region set"))
        (let ((input (buffer-substring-no-properties (region-beginning) (region-end))))
            (gptel-request nil
                           :callback (lambda (response info)
                                         (my/gptel-stash-response "*Last Definition*" (plist-get info :context) response)
                                         (message response))
                           :system my/gptel-define-word-prompt
                           :context input)))

    (defun my/parse-prompt-file (prompt-file)
        "Parse a single prompt file and return its description and content."
        (with-temp-buffer
            (insert-file-contents prompt-file)
            (let ((prompt-description "NO DESCRIPTION"))
                ;; nab the description - single-line descriptions only!
                (goto-char (point-min))
                (when (re-search-forward "#\\+description: \\(.*?\\) *--> *$" nil t)
                    (setq prompt-description (match-string 1)))
                ;; remove all comments
                (delete-matching-lines "^ *<!--" (point-min) (point-max))
                ;; remove leading blank lines
                (goto-char (point-min))
                (while (and (looking-at "^$") (not (eobp)))
                    (delete-char 1))
                ;; return the description and content
                (list prompt-description (buffer-substring-no-properties (point-min) (point-max))))))

    (defun my/gptel-build-directives (promptdir)
        "Build `gptel-directives' from Markdown files in PROMPTDIR."
        (let* ((prompt-files (directory-files promptdir t "md$")))
            (mapcar (lambda (prompt-file)
                        (let ((parsed-prompt (my/parse-prompt-file prompt-file)))
                            (cons (intern (f-base prompt-file))  ; gptel-directives key
                                  (nth 1 parsed-prompt))))       ; prompt content
                    prompt-files)))
    :custom
    (gptel-default-mode 'markdown-mode)
    (gptel-expert-commands t)
    :config
    ;; (setq gptel-directives
    ;;       (my/gptel-build-directives
    ;;        (expand-file-name "Common/emacs/personal/etc/gptel-prompts" my/dotfiles-path)))
    ;; (setq gptel-model 'gpt-4o)
    (setq gptel-model 'gpt-4o-mini)
    (setq gptel-api-key (lambda ()
                            (password-store-get-field "Logins/openai.com" "API Key"))))

(use-package gptel-quick
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
