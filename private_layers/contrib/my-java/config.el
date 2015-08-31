;; Define the buffer local company backend variable
(spacemacs|defvar-company-backends java-mode)

;; Command prefixes
(setq my-java/key-binding-prefixes '(("me" . "errors")
                                  ("mf" . "find")
                                  ("mg" . "goto")
                                  ("mr" . "refactor")
                                  ("mh" . "documentation")
                                  ("mm" . "maven")
                                  ("ma" . "ant")
                                  ("mp" . "project")
                                  ("mt" . "test")))

(mapc (lambda(x) (spacemacs/declare-prefix-for-mode
                  'java-mode (car x) (cdr x)))
      my-java/key-binding-prefixes)
