;;; jv-ui.el --- Custom UI Configs -*- lexical-binding: t; -*-

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

;; no splash
(customize-set-variable 'crafted-startup-inhibit-splash t)

;; all-the-icons
(setq all-the-icons-scale-factor 1.1)

;;; line numbers on
(setq crafted-ui-display-line-numbers t)

;; theme
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
    (disable-theme 'deeper-blue) ; first turn off the deeper-blue theme
    (modus-themes-load-themes))

;; modeline
(progn
    (setq doom-modeline-icon t)
    (setq doom-modeline-height 1)
    (setq doom-modeline-lsp t)
    (custom-set-faces
        '(mode-line ((t (:height 0.9))))
        '(mode-line-active ((t (:height 0.9))))
        '(mode-line-inactive ((t (:height 0.9))))))


;;; Themes are color customization packages which coordinate the
;;; various colors, and in some cases, font-sizes for various aspects
;;; of text editing within Emacs, toolbars, tabbars and
;;; modeline. Several themes are built-in to Emacs, by default,
;;; Crafted Emacs uses the `deeper-blue' theme. Here is an example of
;;; loading a different theme from the venerable Doom Emacs project.
(crafted-package-install-package 'doom-themes)
(progn
    (disable-theme 'deeper-blue)          ; first turn off the deeper-blue theme
    (load-theme 'doom-one-light t))       ; load the doom-palenight theme

(provide 'jv-ui)
;;; jv-ui.el ends here
