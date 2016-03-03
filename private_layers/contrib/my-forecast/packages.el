;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-forecast-packages
      '(
        forecast
        ))

;; List of packages to exclude.
(setq my-forecast-excluded-packages '())

(defun my-forecast/init-forecast ()
  (use-package forecast
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (setq forecast-latitude 25.6400320
            forecast-longitude -80.3385390
            forecast-city "Miami"
            forecast-country "USA"
            forecast-api-key "96a8f25d9ec2a623b6606f079bbd2f5f")

      (evil-leader/set-key
        "of" 'forecast))
    :config ; :config is called after the package is actually loaded with defer
    ;; You can put stuff that relies on the package like function calls here
    (message "forecast.el was actually loaded!")))
