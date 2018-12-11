(setq inferior-lisp-program "/usr/local/bin/sbcl")

(setq slime-lisp-implementations
  `((ccl ("~/.cim/bin/ccl-1.9") :coding-system utf-8-unix)
     (alisp ("/usr/local/bin/alisp") :coding-system utf-8-unix)
     (ecl ("/usr/local/bin/ecl"))  ; :coding-system utf-8-unix)
     (cmucl ("/usr/local/bin/cmucl") :coding-system utf-8-unix)
     (sbcl ("/usr/local/bin/sbcl" "+R" "-l" "~/.sbclrc") :coding-system utf-8-unix)
     (abcl ("~/.cim/bin/abcl-1.3.1" "-XX:MaxPermSize=256m" "-Dfile.encoding=UTF-8") :coding-system utf-8-unix)
     (clisp ("/usr/local/bin/clisp") :coding-system utf-8-unix)))

(setq slime-default-lisp 'sbcl)
(setq slime-net-coding-system 'utf-8-unix)
