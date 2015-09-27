;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-sunshine-packages
      '(
        sunshine
        ))

;; List of packages to exclude.
(setq my-sunshine-excluded-packages '())

(defun my-sunshine/init-sunshine ()
  (use-package sunshine
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (setq sunshine-location "33126,USA")
      (setq sunshine-show-icons t)
      (setq sunshine-units 'metric)
      (evil-leader/set-key
        "osf" 'sunshine-forecast
        "oss" 'sunshine-quick-forecast
        "osq" 'sunshine-quit))
    :config ; :config is called after the package is actually loaded with defer
    ;; You can put stuff that relies on the package like function calls here
    (message "sunshine mode was actually loaded!")))
