;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-deft-packages
      '(
        deft
        ))

;; List of packages to exclude.
(setq my-deft-excluded-packages '())

(defun my-deft/init-deft ()
  (use-package deft
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (setq deft-directory "~/Dropbox/Personal/Notes")
      (setq deft-extension "org")
      (setq deft-text-mode 'org-mode)
      (setq deft-use-filename-as-title t)
      (setq deft-use-filter-string-for-filename t)
      (setq deft-auto-save-interval 0)

      ;; launch deft
      (evil-leader/set-key
        "od" 'deft))
    :config ; :config is called after the package is actually loaded with defer
    ;; You can put stuff that relies on the package like function calls here
    (message "deft mode was actually loaded!")))
