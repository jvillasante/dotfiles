;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
((c++-mode .
     ((flycheck-clang-language-standard . "c++20"))))

((c++-mode .
     ((lsp-clients-clangd-args . ("-j=2"
                                     "--log=error"
                                     ;; "--compile-commands-dir=build"
                                     "--background-index"
                                     "--clang-tidy"
                                     "--completion-style=detailed"
                                     "--pch-storage=memory"
                                     "--header-insertion=never"
                                     "--header-insertion-decorators=0")))))
