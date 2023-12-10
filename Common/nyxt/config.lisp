(in-package #:nyxt-user)

;; Emacs please
(define-configuration buffer
    ((default-modes
         (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))

;; Keybindings
(define-configuration input-buffer
    ((override-map
      (let ((map (make-keymap "override-map")))
          (define-key map "M-x" 'execute-command "C-space" 'nothing)))))

;; Enable blocker-mode
(define-configuration web-buffer
    ((default-modes
         (pushnew 'nyxt/mode/blocker:blocker-mode %slot-value%))))

;; start fresh
(define-configuration browser
    ((restore-session-on-startup-p nil)))

;; do not complete searches
(define-configuration document-buffer
    ((search-always-auto-complete-p nil)))
