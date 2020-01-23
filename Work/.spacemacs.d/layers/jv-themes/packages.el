(defconst jv-themes-packages
    '(solaire-mode
         material-theme
         zenburn-theme
         solarized-theme
         doom-themes))

(defun jv-themes/init-solaire-mode ()
    (use-package solaire-mode
        :hook
        ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
        (minibuffer-setup . solaire-mode-in-minibuffer)
        :config
        (solaire-global-mode +1)
        (solaire-mode-swap-bg)))

(defun jv-themes/init-material-theme ()
    (use-package material-theme
        :ensure t
        :defer t
        :init
        (defun jv/material-theme-hook ()
            (set-face-attribute 'which-key-key-face nil :foreground
                (face-attribute 'error :foreground))
            (loop for n from 1 to 8
                do (set-face-attribute (intern-soft (format "org-level-%s" n))
                       nil
                       :height     'unspecified
                       :background 'unspecified
                       :box        'unspecified)))
        (jv/add-theme-hook 'material       #'jv/material-theme-hook)
        (jv/add-theme-hook 'material-light #'jv/material-theme-hook)))

(defun jv-themes/init-zenburn-theme ()
    (use-package zenburn-theme
        :ensure t
        :defer t
        :init
        (jv/add-theme-hook 'zenburn #'jv/zenburn-theme-hook)))

(defun jv-themes/init-solarized-theme ()
    (use-package solarized-theme
        :ensure t
        :defer t
        :init
        (jv/add-theme-hook 'solarized-dark  #'jv/solarized-dark-theme-hook)
        (jv/add-theme-hook 'solarized-light #'jv/solarized-light-theme-hook)
        :config
        (setq
            solarized-use-variable-pitch nil
            solarized-use-less-bold t
            solarized-use-more-italic nil
            solarized-distinct-doc-face t
            solarized-high-contrast-mode-line t
            solarized-height-minus-1 1.0
            solarized-height-plus-1 1.0
            solarized-height-plus-2 1.0
            solarized-height-plus-3 1.0
            solarized-height-plus-4 1.0)))

(defun jv-themes/init-doom-themes ()
    "
    https://github.com/hlissner/emacs-doom-themes
"
    (use-package doom-themes
        ;; :init
        ;; (jv/add-theme-hook 'doom-solarized-dark  #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-solarized-light #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-one #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-one-light #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-nord #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-nord-light #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-opera #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-opera-light #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-molokai #'jv/doom-themes-hook)
        ;; (jv/add-theme-hook 'doom-spacegrey #'jv/doom-themes-hook)
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
            doom-themes-enable-italic t)   ; if nil, italics is universally disabled

        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)

        ;; Enable custom neotree theme (all-the-icons must be installed!)
        ;; (doom-themes-neotree-config)
        ;; or for treemacs users
        ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        ;; (doom-themes-treemacs-config)

        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config)))
