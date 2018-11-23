(with-eval-after-load 'common-lisp
  (load (expand-file-name "~/.roswell/helper.el")))

(setf slime-lisp-implementations
  `((roswell ("ros" "-Q" "run"))
     (sbcl ("ros" "-L" "sbcl" "+R" "-l" "~/.sbclrc" "run"))))

(setf slime-default-lisp 'sbcl)
(setq slime-net-coding-system 'utf-8-unix)
