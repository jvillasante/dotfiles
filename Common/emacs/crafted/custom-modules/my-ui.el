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
(setq-default frame-title-format
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

;; Emacs 29 improved scrolling
(when (> emacs-major-version 28)
  (pixel-scroll-precision-mode))

;; Ignore xressources
(setq inhibit-x-resources t)

;; all-the-icons
(setq all-the-icons-scale-factor 1.1)

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
    (setq
        modus-themes-mode-line '(borderless (padding 1) (height 0.9))
        modus-themes-bold-constructs nil
        modus-themes-italic-constructs t
        modus-themes-fringes 'subtle
        modus-themes-tabs-accented t
        modus-themes-subtle-line-numbers t
        modus-themes-diffs 'desaturated
        modus-themes-region '(bg-only no-extend)
        modus-themes-headings
        '((1 . (monochrome variable-pitch 1.3))
             (2 . (monochrome variable-pitch 1.2))
             (3 . (monochrome variable-pitch 1.1))
             (t . (monochrome))))

    (modus-themes-load-themes) ;; Load the theme files before enabling a theme
    (+my/switch-theme 'modus-operandi))

;; modeline
(progn
    (setq doom-modeline-icon t)
    (setq doom-modeline-height 1)
    (setq doom-modeline-lsp t)
    (custom-set-faces
        '(mode-line ((t (:height 0.9))))
        '(mode-line-active ((t (:height 0.9))))
        '(mode-line-inactive ((t (:height 0.9))))))

(provide 'my-ui)
;;; my-ui.el ends here
