(setq jvillasante-packages
      '(sr-speedbar
        flyspell-lazy
        irony
        irony-eldoc
        company-irony
        company-irony-c-headers
        flycheck-irony
        forecast
        password-store
        helm-dash
        ;; helm-gtags
        zeal-at-point
        xwidgete
        modern-cpp-font-lock
        ))

(defun jvillasante/init-sr-speedbar ()
  (use-package sr-speedbar
    :defer t
    :init
    (spacemacs/set-leader-keys
      "sr" 'sr-speedbar-toggle)))

(defun jvillasante/init-flyspell-lazy ()
  (use-package flyspell-lazy
    :init
    (add-hook 'prog-mode-hook 'flyspell-lazy-mode)))

(defun jvillasante/init-irony ()
  (use-package irony
    :init
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
    (spacemacs|diminish irony-mode "I" "I")))

(defun jvillasante/init-irony-eldoc ()
  (use-package irony-eldoc
    :init
    (add-hook 'c-mode-hook 'eldoc-mode)
    (add-hook 'c++-mode-hook 'eldoc-mode)
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun jvillasante/init-company-irony ()
  (use-package company-irony
    :init
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(defun jvillasante/init-company-irony-c-headers ()
  (use-package company-irony-c-headers))

(defun jvillasante/init-flycheck-irony ()
  (use-package flycheck-irony
    :init
    (progn
      (eval-after-load 'flycheck
        '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))))

(defun jvillasante/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :init
    (modern-c++-font-lock-global-mode t)
    (spacemacs|diminish modern-c++-font-lock-mode "M" "M")))

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
    (setq forecast-latitude 27.395712
          forecast-longitude -82.4427907
          forecast-city "Bradenton, FL"
          forecast-country "USA"
          forecast-api-key "96a8f25d9ec2a623b6606f079bbd2f5f")
    (evil-leader/set-key
      "of" 'forecast)))

(defun jvillasante/init-zeal-at-point ()
  (use-package zeal-at-point
    :defer t
    :init
    (spacemacs/set-leader-keys
      "dd" 'zeal-at-point
      "dD" 'zeal-at-point-set-docset)))

(defun jvillasante/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init
    ;; (setq helm-dash-browser-func 'eww)
    ;; (setq helm-dash-browser-func 'xwidget-webkit-browse-url)
    (setq helm-dash-docsets-path my-docsets-path)

    (defun my-c-doc-hook ()
      (interactive)
      (setq-local helm-dash-docsets '("C" "C++")))
    (add-hook 'c-mode-common-hook 'my-c-doc-hook)

    (spacemacs/set-leader-keys
      "dh" 'helm-dash-at-point
      "dH" 'helm-dash)))

;; (defun jvillasante/post-init-helm-gtags ()
;;   (spacemacs|diminish helm-gtags-mode "HG" "HG"))

(defun jvillasante/init-xwidgete ()
  (use-package xwidgete
    :defer t
    :init
    (evil-set-initial-state 'xwidget-webkit-mode 'emacs)
    (spacemacs/set-leader-keys
      "ox" 'xwidget-webkit-browse-url)))
