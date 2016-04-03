;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq my-irony-packages
      '(
        irony
        company-irony
        flycheck-irony
        irony-eldoc
        ))

;; List of packages to exclude.
(setq my-irony-excluded-packages
      '(auto-complete-clang))

(defun my-irony/init-irony ()
  (use-package irony
    :defer t
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)
    ;; replace the `completion-at-point' and `complete-symbol' bindings in
    ;; irony-mode's buffers by irony-mode's function
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  ;; (use-package irony
  ;;   :defer t
  ;;   :init
  ;;   (progn
  ;;     (add-hook 'c++-mode-hook 'irony-mode)
  ;;     (add-hook 'c-mode-hook 'irony-mode)
  ;;     (add-hook 'objc-mode-hook 'irony-mode)
  ;;     (add-hook 'irony-mode-hook
  ;;               (lambda ()
  ;;                 (define-key irony-mode-map [remap completion-at-point]
  ;;                   'irony-completion-at-point-async)
  ;;                 (define-key irony-mode-map [remap complete-symbol]
  ;;                   'irony-completion-at-point-async)))
  ;;     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;;     (spacemacs|diminish irony-mode " Ⓘ" " I")))
  )


(defun my-irony/init-company-irony ()
  (use-package company-irony
    :init
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))
    ;; (optional) adds CC special commands to `company-begin-commands' in order to
    ;; trigger completion at interesting places, such as after scope operator
    ;;     std::|
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

  ;; (use-package company-irony
  ;;   :defer t
  ;;   :init
  ;;   (progn
  ;;     (eval-after-load 'company
  ;;       '(add-to-list 'company-backends 'company-irony))
  ;;     (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  ;;     (add-hook 'irony-mode-hook 'company-mode)))
  )

(defun my-irony/init-flycheck-irony ()
  (use-package flycheck-irony
    :init
    (defun setup-flycheck-irony ()
      (when (featurep 'flycheck)
        (flycheck-irony-setup)))
    (add-to-hook 'setup-flycheck-irony '(c-mode-hook c++-mode-hook)))

  ;; (use-package flycheck-irony
  ;;   ;; :defer t                            ; fix this ???
  ;;   :init
  ;;   (progn
  ;;     (eval-after-load 'flycheck
  ;;       '(add-to-list 'flycheck-checkers 'irony))
  ;;     (add-hook 'irony-mode-hook 'flycheck-mode)))
  )

(defun my-irony/init-irony-eldoc ()
  (use-package flycheck-irony
    :init
    (progn
      (add-hook 'irony-mode-hook 'irony-eldoc))))
