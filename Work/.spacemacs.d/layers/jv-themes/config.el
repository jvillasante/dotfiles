;;; set global key for hydra
(global-set-key (kbd "C-c w t") 'jv/themes-hydra/body)

;;; load default theme
(add-hook 'after-init-hook
    (lambda () (load-theme 'doom-solarized-light t)))