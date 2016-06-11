(setq jvillasante-packages
      '(magit
        flycheck
        flyspell-lazy
        irony
        irony-eldoc
        company-irony
        company-irony-c-headers
        flycheck-irony
        forecast
        password-store
        zeal-at-point
        ;; modern-cpp-font-lock
        ))

;; List of packages to exclude.
(setq jvillasante-excluded-packages '())

(defun jvillasante/post-init-magit ()
  (setq-default git-magit-status-fullscreen t)
  (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil))

(defun jvillasante/post-init-flycheck ()
  (add-hook 'c++-mode-hook (lambda ()
                             (setq flycheck-clang-language-standard "c++11"))))

(defun jvillasante/init-flyspell-lazy ()
  (use-package flyspell-lazy
    :init
    (add-hook 'prog-mode-hook 'flyspell-lazy-mode))
  )

(defun jvillasante/init-irony ()
  (use-package irony
    :defer t
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
        'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
        'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (spacemacs|diminish irony-mode "I" "I")))

(defun jvillasante/init-irony-eldoc ()
  (use-package irony-eldoc
    :defer t
    :init
    (add-hook 'c-mode-hook 'eldoc-mode)
    (add-hook 'c++-mode-hook 'eldoc-mode)
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun jvillasante/init-company-irony ()
  (use-package company-irony
    :init
    (add-hook 'c-mode-hook (lambda () (add-to-list 'company-backends 'company-irony)))
    (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends 'company-irony)))
    ;; (optional) adds CC special commands to `company-begin-commands' in order to
    ;; trigger completion at interesting places, such as after scope operator
    ;;     std::|
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(defun jvillasante/init-flycheck-irony ()
  (use-package flycheck-irony
    :init
    (defun setup-flycheck-irony ()
      (when (featurep 'flycheck)
        (flycheck-irony-setup)))
    (add-hook 'c-mode-hook 'setup-flycheck-irony)
    (add-hook 'c++-mode-hook 'setup-flycheck-irony)))

(defun jvillasante/init-modern-cpp-font-lock ()
    (use-package modern-cpp-font-lock
      :defer t
      :init
      (modern-c++-font-lock-global-mode t)))

(defun jvillasante/init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :init
    (evil-leader/set-key
      "oz" 'zeal-at-point)))

(defun jvillasante/init-password-store ()
  (use-package password-store
    :defer t
    :init
    (setq password-store-password-length 25)
    (evil-leader/set-key
      "opc" 'password-store-copy
      "ope" 'password-store-edit
      "opg" 'password-store-generate)))

(defun jvillasante/init-forecast ()
  (use-package forecast
    :defer t
    :init
    (setq forecast-latitude 25.6400320
          forecast-longitude -80.3385390
          forecast-city "Miami"
          forecast-country "USA"
          forecast-api-key "96a8f25d9ec2a623b6606f079bbd2f5f")
    (evil-leader/set-key
      "of" 'forecast)))
