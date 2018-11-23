;;
;; Have a look at https://github.com/joaotavora/eglot
;;

(defconst jv-rust-packages
  '(eglot
     ))

(defun jv-rust/init-eglot ()
  (use-package eglot
    :defer t
    :init
    (spacemacs/declare-prefix "ae" "eglot")
    (spacemacs/set-leader-keys "aes" 'eglot
      "aeR" 'eglot-reconnect
      "aeq" 'eglot-shutdown
      "ae=" 'eglot-format
      "aeh" 'eglot-help-at-point
      "aeO" 'eglot-events-buffer
      "aeE" 'eglot-stderr-buffer
      "aer" 'eglot-rename
      "aee" 'eglot-code-actions)))
