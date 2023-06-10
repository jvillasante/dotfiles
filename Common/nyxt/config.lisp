;; Flathub: $HOME/.var/app/engineer.atlas.Nyxt/config/nyxt

;; emacs bindings
(define-configuration buffer
    ((default-modes
         (pushnew 'nyxt/mode/emacs:emacs-mode %slot-value%))))
