;; emacs bindings
(define-configuration buffer
    ((default-modes (append '(emacs-mode) %slot-default%))))

;; Keybindings overrides
(define-configuration buffer
    ((override-map (let ((map (make-keymap "override-map")))
                       (define-key map
                           "M-x" 'execute-command)))))
