;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-zeal-packages
      '(
        zeal-at-point
        ))

;; List of packages to exclude.
(setq my-zeal-excluded-packages '())

(defun my-zeal/init-zeal-at-point ()
  (use-package zeal-at-point
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (evil-leader/set-key
        "oz" 'zeal-at-point))
    :config ; :config is called after the package is actually loaded with defer
    ;; You can put stuff that relies on the package like function calls here
    (message "zeal mode was actually loaded!")))
