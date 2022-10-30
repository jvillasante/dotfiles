;;; my-ui.el --- Custom UI Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; UI Customizations

;;; Code:

;; Set the default face.
(add-hook 'emacs-startup-hook
    (lambda ()
        (custom-set-faces
            `(default ((t (:font "Iosevka 16"))))
            `(fixed-pitch ((t (:inherit (default)))))
            `(fixed-pitch-serif ((t (:inherit (default)))))
            `(variable-pitch ((t (:font "Iosevka Aile 16")))))))

;; frame title
(customize-set-variable 'frame-title-format
    '(:eval
         (format "%s@%s: %s"
             (or (file-remote-p default-directory 'user)
                 user-real-login-name)
             (or (file-remote-p default-directory 'host)
                 system-name)
             (cond
                 (buffer-file-truename
                     (concat buffer-file-truename))
                 (dired-directory
                     (concat dired-directory))
                 (t (buffer-name))))))

;; no splash
(customize-set-variable 'crafted-startup-inhibit-splash t)

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Hide the startup screen
(customize-set-variable 'inhibit-startup-screen t)

;; Ignore xressources
(customize-set-variable 'inhibit-x-resources t)

;; all-the-icons
(customize-set-variable 'all-the-icons-scale-factor 1.1)

;;; no bell
(customize-set-variable 'visible-bell nil)
(customize-set-variable 'ring-bell-function 'ignore)

;;; line numbers on
(customize-set-variable 'crafted-ui-display-line-numbers t)

;; theme
(defun +my/switch-theme (theme)
    "This interactive call is taken from `load-theme'."
    (interactive
        (list
            (intern (completing-read "Load custom theme: "
                        (mapcar 'symbol-name
                            (custom-available-themes))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t))

(crafted-package-install-package 'modus-themes)
(progn
    (customize-set-variable 'modus-themes-mode-line '(borderless (padding 1) (height 0.9)))
    (customize-set-variable 'modus-themes-bold-constructs nil)
    (customize-set-variable 'modus-themes-italic-constructs t)
    (customize-set-variable 'modus-themes-fringes 'subtle)
    (customize-set-variable 'modus-themes-tabs-accented t)
    (customize-set-variable 'modus-themes-subtle-line-numbers t)
    (customize-set-variable 'modus-themes-diffs 'desaturated)
    (customize-set-variable 'modus-themes-region '(bg-only no-extend))
    (customize-set-variable 'modus-themes-headings
        '((1 . (monochrome variable-pitch 1.3))
             (2 . (monochrome variable-pitch 1.2))
             (3 . (monochrome variable-pitch 1.1))
             (t . (monochrome))))

    (modus-themes-load-themes) ;; Load the theme files before enabling a theme
    (+my/switch-theme 'modus-operandi))

;; modeline
(progn
    (customize-set-variable 'doom-modeline-icon nil)
    (customize-set-variable 'doom-modeline-height 1)
    (customize-set-variable 'doom-modeline-lsp t))

;; column number display mode in the modeline
(add-hook 'after-init-hook 'column-number-mode)

;; minions : menu that lists enabled minor-modes
(crafted-package-install-package 'minions)
(progn
  (add-hook 'after-init-hook 'minions-mode))

;; anzy : displays current match and total matches information in the mode-line in various search modes
(crafted-package-install-package 'anzu)
(progn
    (global-anzu-mode))

(provide 'my-ui)
;;; my-ui.el ends here
