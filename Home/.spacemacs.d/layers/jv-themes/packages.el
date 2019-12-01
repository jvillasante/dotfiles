(defconst jv-themes-packages
    '(material-theme
         zenburn-theme
         solarized-theme))

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
        ;; load initial theme
        (load-theme 'solarized-light t)
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
