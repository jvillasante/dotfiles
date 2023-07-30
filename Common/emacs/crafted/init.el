;;; init.el -*- lexical-binding: t; -*-

;;; Initial phase.

;; Load the custom file if it exists.  Among other settings, this will
;; have the list `package-selected-packages', so we need to load that
;; before adding more packages.  The value of the `custom-file'
;; variable must be set appropriately, by default the value is nil.
;; This can be done here, or in the early-init.el file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

;; Adds crafted-emacs modules to the `load-path', sets up a module
;; writing template, sets the `crafted-emacs-home' variable.
(load (convert-standard-filename
          (expand-file-name ".emacs.crafted/modules/crafted-init-config" my/dotfiles-path)))

;;; Packages phase

(require 'crafted-completion-packages)

;; Install the packages listed in the `package-selected-packages' list.
(package-install-selected-packages :noconfirm)

;;; Configuration phase

;; Some example modules to configure Emacs. Don't blindly copy these,
;; they are here for example purposes.  Find the modules which work
;; for you and add them here.
(require 'crafted-defaults-config)
(require 'crafted-startup-config)
(require 'crafted-completion-config)
(unless crafted-startup-inhibit-splash
  ;; Setting the initial-buffer-choice to the function to show the
  ;; Crafted Emacs startup screen when Emacs is started.
  (setq initial-buffer-choice #'crafted-startup-screen))

;;; Optional configuration

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Crafted Emacs loaded in %s."
                     (emacs-init-time))))

;; Set default coding system (especially for Windows)
(set-default-coding-systems 'utf-8)

(provide 'init)
;;; init.el ends here
