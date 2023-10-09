(in-package #:nyxt-user)

;; Emacs please
(define-configuration (web-buffer prompt-buffer panel-buffer
                                  nyxt/mode/editor:editor-buffer)
    ((default-modes (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

;; Enable blocker-mode
(define-configuration web-buffer
    ((default-modes
         (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))

(define-configuration browser
    ((restore-session-on-startup-p nil)))

(define-configuration document-buffer
    ((search-always-auto-complete-p nil)))
