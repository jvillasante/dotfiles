;; -*- no-byte-compile: t; -*-
;;; editor/lispyville/packages.el

(when (package! lispy)
  (when (featurep! :feature evil)
    (package! lispyville)))
