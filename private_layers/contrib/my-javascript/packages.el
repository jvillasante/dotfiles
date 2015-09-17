;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-javascript-packages
      '(
        nodejs-repl
        ))

;; List of packages to exclude.
(setq my-javascript-excluded-packages '())

(defun my-javascript/init-nodejs-repl ()
  (use-package nodejs-repl
    ;; :defer t activates lazy loading which makes startup faster
    :defer t
    ;; The code in :init is always run, use it to set up config vars and key bindings
    :init
    (progn ; :init only takes one expression so use "progn" to combine multiple things
      (defun my-send-region-to-nodejs-repl-process (start end)
        "Send region to `nodejs-repl' process."
        (interactive "r")
        (save-selected-window
          (save-excursion (nodejs-repl)))
        (comint-send-region (get-process nodejs-repl-process-name)
                            start end))

      (evil-leader/set-key
        "ojs" 'my-send-region-to-nodejs-repl-process
        "ojn" 'nodejs-repl))

    :config ; :config is called after the package is actually loaded with defer
    ;; You can put stuff that relies on the package like function calls here
    (message "my-javascript was actually loaded!")))
