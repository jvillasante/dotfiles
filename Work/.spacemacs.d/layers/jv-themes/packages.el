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
        (solaire-global-mode t)
        (solaire-mode-swap-bg)))

(defun jv-themes/init-material-theme ()
    (use-package material-theme
        :ensure t
        :defer t
        :init
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
        (jv/add-theme-hook 'solarized-dark  #'jv/solarized-theme-hook)
        (jv/add-theme-hook 'solarized-light #'jv/solarized-theme-hook)
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
        :init
        (jv/add-theme-hook 'doom-solarized-dark  #'jv/doom-solarized-theme-hook)
        (jv/add-theme-hook 'doom-solarized-light #'jv/doom-solarized-theme-hook)
        (jv/add-theme-hook 'doom-one #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-one-light #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-nord #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-nord-light #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-opera #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-opera-light #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-tomorrow-night #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-tomorrow-day #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-vibrant #'jv/doom-theme-hook)
        (jv/add-theme-hook 'doom-molokai #'jv/doom-theme-hook)
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
            doom-themes-enable-italic t)   ; if nil, italics is universally disabled

        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)

        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config)))