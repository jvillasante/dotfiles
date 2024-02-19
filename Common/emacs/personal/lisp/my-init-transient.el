;;; my-init-transient.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(require 'transient)

(use-package transient-dwim
    :bind ("M-s =" . transient-dwim-dispatch))

;; Transient menu for isearch.
(transient-define-prefix my--isearch-menu-transient ()
    [["Edit Search String"
      ("e"
       "Edit the search string (recursive)"
       isearch-edit-string
       :transient nil)
      ("w"
       "Pull next word or character word from buffer"
       isearch-yank-word-or-char
       :transient nil)
      ("s"
       "Pull next symbol or character from buffer"
       isearch-yank-symbol-or-char
       :transient nil)
      ("l"
       "Pull rest of line from buffer"
       isearch-yank-line
       :transient nil)
      ("y"
       "Pull string from kill ring"
       isearch-yank-kill
       :transient nil)
      ("t"
       "Pull thing from buffer"
       isearch-forward-thing-at-point
       :transient nil)]

     ["Replace"
      ("q"
       "Start ‘query-replace’"
       isearch-query-replace
       :if-nil buffer-read-only
       :transient nil)
      ("x"
       "Start ‘query-replace-regexp’"
       isearch-query-replace-regexp
       :if-nil buffer-read-only
       :transient nil)]]

    [["Toggle"
      ("X"
       "Regexp searching"
       isearch-toggle-regexp
       :transient nil)
      ("S"
       "Symbol searching"
       isearch-toggle-symbol
       :transient nil)
      ("W"
       "Word searching"
       isearch-toggle-word
       :transient nil)
      ("F"
       "Case fold"
       isearch-toggle-case-fold
       :transient nil)
      ("L"
       "Lax whitespace"
       isearch-toggle-lax-whitespace
       :transient nil)]

     ["Misc"
      ("o"
       "occur"
       isearch-occur
       :transient nil)
      ("h"
       "highlight"
       isearch-highlight-regexp
       :transient nil)
      ("H"
       "highlight lines"
       isearch-highlight-lines-matching-regexp
       :transient nil)]])
(define-key isearch-mode-map (kbd "M-s =") 'my--isearch-menu-transient)

(provide 'my-init-transient)
;;; my-init-transient.el ends here
