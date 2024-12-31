;;; init.el --- Init -*- no-byte-compile: t; lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; package.el
(progn
    (require 'package)

    ;; Initialize and refresh package contents again if needed
    (package-initialize)
    (unless package-archive-contents
        (package-refresh-contents))

    ;; Install use-package if necessary
    (unless (package-installed-p 'use-package)
        (package-install 'use-package))

    ;; Ensure use-package is available at compile time
    (eval-when-compile
        (require 'use-package)
        (use-package use-package
            :ensure nil ;; emacs built-in
            :custom ((use-package-verbose t)
                     (use-package-always-ensure t)
                     (use-package-expand-minimally t)))))

(setq make-backup-files nil)

(use-package activities
    :init
    (activities-mode)
    (activities-tabs-mode)
    (setq edebug-inhibit-emacs-lisp-mode-bindings t)
    ;; :custom
    ;; (activities-bookmark-store nil)
    ;; (activities-always-persist nil)
    :bind
    (("C-x C-a C-n" . activities-new)
     ("C-x C-a C-d" . activities-define)
     ("C-x C-a C-a" . activities-resume)
     ("C-x C-a C-s" . activities-suspend)
     ("C-x C-a C-k" . activities-kill)
     ("C-x C-a RET" . activities-switch)
     ("C-x C-a b"   . activities-switch-buffer)
     ("C-x C-a g"   . activities-revert)
     ("C-x C-a l"   . activities-list)))

(provide 'init)

;;; init.el ends here
