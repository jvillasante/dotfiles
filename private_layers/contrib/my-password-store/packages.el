;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-password-store-packages
    '(
      password-store
      ))

;; List of packages to exclude.
(setq my-password-store-excluded-packages '())

(defun my-password-store/init-password-store ()
  (use-package password-store
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (evil-leader/set-key
        "opc" 'password-store-copy
        "ope" 'password-store-edit))
    :config ; :config is called after the package is actually loaded with defer
      ;; You can put stuff that relies on the package like function calls here
      (message "password-store mode was actually loaded!")))
