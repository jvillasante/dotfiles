(setq slime-lisp-implementations
  '((cmucl ("cmucl" "-quiet"))
     (sbcl ("/usr/local/bin/sbcl" "+R" "-l" "~/.sbclrc" "run") :coding-system utf-8-unix)))

(setq slime-default-lisp 'sbcl)
(setq slime-net-coding-system 'utf-8-unix)
