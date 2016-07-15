(setq jvillasante-packages
      '(magit
        flyspell-lazy
        irony
        irony-eldoc
        company-irony
        company-irony-c-headers
        flycheck-irony
        forecast
        password-store
        org
        esqlite
        zeal-at-point
        helm-dash
        xwidget-webkit
        modern-cpp-font-lock))

;; List of packages to exclude.
(setq jvillasante-excluded-packages '())

(defun jvillasante/post-init-magit ()
  (setq-default git-magit-status-fullscreen t)
  (setq magit-completing-read-function 'magit-builtin-completing-read
        magit-push-always-verify nil))

(defun jvillasante/init-flyspell-lazy ()
  (use-package flyspell-lazy
    :init
    (add-hook 'prog-mode-hook 'flyspell-lazy-mode)))

(defun jvillasante/init-irony ()
  (use-package irony
    :defer t
    :init
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)

    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))

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
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(defun jvillasante/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :init
    (add-hook 'c-mode-hook (lambda () (add-to-list 'company-backends 'company-irony-c-headers)))
    (add-hook 'c++-mode-hook (lambda () (add-to-list 'company-backends 'company-irony-c-headers)))))

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

(defun jvillasante/post-init-org ()
  ;; (add-hook 'org-mode-hook 'yas-minor-mode)

  (with-eval-after-load 'org
    ;; org problems
    (setq org-planning-line-re "^[    ]*\\(\\(?:CLOSED\\|DEADLINE\\|SCHEDULED\\):\\)")
    (setq org-clock-line-re "^[    ]*CLOCK:")

    (setq org-startup-indented t)
    (setq org-indent-mode t)

    ;; set maximum indentation for description lists
    (setq org-list-description-max-indent 5)

    ;; prevent demoting heading also shifting text inside sections
    (setq org-adapt-indentation nil)))

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
    (setq helm-dash-browser-func 'eww)
    (setq helm-dash-docsets-path my-docsets-path)

    (defun c-doc-hook ()
      (interactive)
      (setq-local helm-dash-docsets '("C" "C++")))
    (add-hook 'c-mode-common-hook 'c-doc-hook)

    (spacemacs/set-leader-keys
      "dh" 'helm-dash-at-point
      "dH" 'helm-dash)))

(defun jvillasante/post-init-xwidget-webkit ()
  ;; WebKit
  ;; make these keys behave like normal browser
  (define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
  (define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
  (define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
  (define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
  (define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
  (define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

  ;; adapt webkit according to window configuration change automatically
  ;; without this hook, every time you change your window configuration,
  ;; you must press 'a' to adapt webkit content to new window size
  (add-hook 'window-configuration-change-hook (lambda ()
                                                (when (equal major-mode 'xwidget-webkit-mode)
                                                  (xwidget-webkit-adjust-size-dispatch))))

  ;; by default, xwidget reuses previous xwidget window,
  ;; thus overriding your current website, unless a prefix argument
  ;; is supplied
  ;;
  ;; This function always opens a new website in a new window
  (defun xwidget-browse-url-no-reuse (url &optional sessoin)
    (interactive (progn
                   (require 'browse-url)
                   (browse-url-interactive-arg "xwidget-webkit URL: "
                                               )))
    (xwidget-webkit-browse-url url t))

  ;; make xwidget default browser
  (setq browse-url-browser-function (lambda (url session)
                                      (other-window 1)
                                      (xwidget-browse-url-no-reuse url))))
