;;; config.el --- jv-lisp Layer config File for Spacemacs
(setf slime-default-lisp 'sbcl)
(setq slime-net-coding-system 'utf-8-unix)

(setf slime-lisp-implementations
  `((roswell ("ros" "-Q" "run"))
     (sbcl ("ros" "-L" "sbcl" "+R" "-l" "~/.sbclrc" "run"))))
