(defconst jv-slack-packages
  '(slack))

(defun jv-slack/post-init-slack ()
  ;; (setq request-backend 'url-retrieve)
  (setq slack-request-timeout 15)
  (setq request-backend 'curl)
  ;; (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  (setq alert-default-style 'message)
  (slack-register-team
   :name "cpplang"
   :default t
   :client-id (shell-command-to-string "secret-tool lookup user jvillasante service slack-client-id domain slack.com")
   :client-secret (shell-command-to-string "secret-tool lookup user jvillasante service slack-client-secret domain slack.com")
   :token (shell-command-to-string "secret-tool lookup user jvillasante service slack-client-token domain slack.com")
   :subscribed-channels '(general random)))
