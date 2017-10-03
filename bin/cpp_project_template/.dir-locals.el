;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")
((c++-mode
  (flycheck-clang-language-standard . "c++17")
  (eval add-hook 'before-save-hook #'clang-format-buffer nil t)))
