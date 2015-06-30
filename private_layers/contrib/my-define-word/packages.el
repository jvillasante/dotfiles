;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-define-word-packages
      '(
        define-word
        ))

;; List of packages to exclude.
(setq my-define-word-excluded-packages '())

(defun my-define-word/init-define-word ()
  (use-package define-word
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    :config ; :config is called after the package is actually loaded with defer
    ;; You can put stuff that relies on the package like function calls here
    (message "define-word mode was actually loaded!")))
