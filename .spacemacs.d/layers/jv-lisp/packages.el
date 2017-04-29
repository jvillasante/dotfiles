(defconst jv-lisp-packages
  '(common-lisp))

(defun jv-lisp/post-init-common-lisp ()
  (with-eval-after-load 'common-lisp
    ;; SLIME - SBCL (Common Lisp)
    (setq inferior-lisp-program my-cl-path)
    (setf slime-lisp-implementations
          `((sbcl ("ros" "-Q" "-l" "~/.sbclrc" "-L" "sbcl" "run"))
            (ccl  ("ros" "-Q" "-l" "~/.ccl-init.lisp" "-L" "ccl-bin" "run"))))
    (setf slime-default-lisp 'sbcl)
    (setq slime-net-coding-system 'utf-8-unix)))
