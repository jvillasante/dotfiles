(defconst jv-cpp-ycmd-packages
  '(ycmd
     ))

(defun jv-cpp-ycmd/pre-init-ycmd ()
  (setq ycmd-server-command (list "python" (file-truename "~/Hacking/software/ycmd/ycmd"))))

(defun jv-cpp-ycmd/post-init-ycmd ()
  (setq ycmd-force-semantic-completion t))
