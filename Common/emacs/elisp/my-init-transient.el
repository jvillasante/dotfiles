;;; my-init-transient.el --- -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; disproject : dispatch various project-related commands via Transient menus
(use-package disproject
    :disabled t
    ;; Replace `project-prefix-map' with `disproject-dispatch'.
    :bind (:map ctl-x-map
                ("p" . disproject-dispatch)))

;; TODO: Configure other casual packages
;; casual : A collection of opinionated Transient-based keyboard-driven user
;;          interfaces for various built-in Emacs modes.
(use-package casual
    :config
    ;; dired
    (keymap-set dired-mode-map "?" #'casual-dired-tmenu)
    (keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu) ; optional
    (keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu)) ; optional

(provide 'my-init-transient)
;;; my-init-transient.el ends here
