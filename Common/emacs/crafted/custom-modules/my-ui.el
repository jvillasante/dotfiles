;;; my-ui.el --- Custom UI Configs -*- lexical-binding: t; -*-

;;; Commentary:
;;; UI Customizations

;;; Code:

;; Set the default face.
(add-hook
 'emacs-startup-hook
 (lambda ()
   (custom-set-faces
    `(default ((t (:font "Iosevka 16"))))
    `(fixed-pitch ((t (:inherit (default)))))
    `(fixed-pitch-serif ((t (:inherit (default)))))
    `(variable-pitch ((t (:font "Iosevka Aile 16")))))))

;; frame title
(csetq frame-title-format
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
(csetq crafted-startup-inhibit-splash t)

;; Start maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Select and raise the frame, always
(select-frame-set-input-focus (selected-frame))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; Hide the startup screen
(csetq inhibit-startup-screen t)

;; Ignore xressources
(csetq inhibit-x-resources t)

;; all-the-icons
(csetq all-the-icons-scale-factor 1.1)

;;; no bell
(csetq visible-bell nil)
(csetq ring-bell-function 'ignore)

;;; line numbers on
(csetq crafted-ui-display-line-numbers t)

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
  (csetq modus-themes-variable-pitch-ui t)
  (csetq modus-themes-mode-line '(borderless (padding 1) (height 0.9)))
  (csetq modus-themes-bold-constructs t)
  (csetq modus-themes-italic-constructs nil)
  (csetq modus-themes-paren-match '(bold intense))
  (csetq modus-themes-fringes 'subtle)
  (csetq modus-themes-tabs-accented t)
  (csetq modus-themes-subtle-line-numbers t)
  (csetq modus-themes-diffs 'desaturated)
  (csetq modus-themes-region '(bg-only no-extend))
  (csetq modus-themes-mixed-fonts t)
  (csetq modus-themes-section-headings t)
  (csetq modus-themes-completions '((matches . (extrabold background intense))
                                    (selection . (extrabold accented intense))
                                    (popup . (accented))))
  (csetq modus-themes-headings
         '((1 . (monochrome variable-pitch 1.3))
           (2 . (monochrome variable-pitch 1.2))
           (3 . (monochrome variable-pitch 1.1))
           (t . (monochrome))))

  (modus-themes-load-themes) ;; Load the theme files before enabling a theme
  (+my/switch-theme 'modus-operandi))

;; modeline
(progn
  (csetq crafted-ui-use-doom-modeline nil)
  (with-eval-after-load 'doom-modeline
      (csetq doom-modeline-icon nil)
      (csetq doom-modeline-height 1)
      (csetq doom-modeline-lsp t)))

;; column number display mode in the modeline
(add-hook 'after-init-hook 'column-number-mode)

;; minions : menu that lists enabled minor-modes
(crafted-package-install-package 'minions)
(progn
  (with-eval-after-load 'minions
    (push 'flycheck-mode minions-prominent-modes)
    (push 'overwrite-mode minions-prominent-modes))
  (add-hook 'after-init-hook 'minions-mode))

;; anzu : displays current match and total matches information in the mode-line in various search modes
(crafted-package-install-package 'anzu)
(progn
  (global-anzu-mode))

(provide 'my-ui)
;;; my-ui.el ends here
