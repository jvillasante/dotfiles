(defun jv-cpp/rtags-find-symbol-at-point-other-file ()
  (interactive)
  (let((current-prefix-arg '(4)))
    (call-interactively 'rtags-find-symbol-at-point)))
