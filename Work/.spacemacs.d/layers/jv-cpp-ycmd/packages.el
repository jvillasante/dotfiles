(defconst jv-cpp-ycmd-packages
  '(ycmd
     ))

(defun jv-cpp-ycmd/pre-init-ycmd ()
  (setq ycmd-server-command (list "python" (file-truename "~/Hacking/Software/ycmd/ycmd"))))

(defun jv-cpp-ycmd/post-init-ycmd ()
  (spacemacs|diminish ycmd-mode " Ⓨ" " Y")
  (setq request-message-level -1)
  ;; (setq ycmd-extra-conf-whitelist '("~/Hacking/workspace/cpp/*"))
  (setq ycmd-force-semantic-completion t))
