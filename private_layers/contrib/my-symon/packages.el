;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-symon-packages
      '(
        symon
        ))

;; List of packages to exclude.
(setq my-symon-excluded-packages '())

(defun my-symon/init-symon ()
  (use-package symon
    :ensure t
    :init
    (setq symon-sparkline-type 'bounded)
    :config
    (symon-mode)
    (message "symon mode was actually loaded!")))
