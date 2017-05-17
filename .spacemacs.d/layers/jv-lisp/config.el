;;; config.el --- jv-lisp Layer config File for Spacemacs

(setq inferior-lisp-program "ros -L sbcl -Q -l ~/.sbclrc run")
(setf slime-lisp-implementations
  `((sbcl ("sbcl" "--dynamic-space-size" "2000"))
     (roswell ("ros" "-Q" "run"))))
(setf slime-default-lisp 'roswell)
(setq slime-net-coding-system 'utf-8-unix)
