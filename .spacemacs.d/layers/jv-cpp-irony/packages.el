(defconst jv-cpp-irony-packages
  '(irony
     company-irony
     company-irony-c-headers
     flycheck-irony
     irony-eldoc))

(defun jv-cpp-irony/init-irony ()
  (use-package irony
    :defer t
    :init
    (progn
      ;; If irony server was never installed, install it.
      ;; (unless (irony--find-server-executable) (call-interactively #'irony-install-server))

      (spacemacs|diminish irony-mode " Ⓘ" " I")

      (defun jv-irony/enable-irony-mode-if-server-found ()
        (let* ((exec-path (cons (expand-file-name "bin" irony-server-install-prefix) exec-path)))
          (if (executable-find "irony-server") (irony-mode))))
      (add-hook 'c-mode-hook 'jv-irony/enable-irony-mode-if-server-found)
      (add-hook 'c++-mode-hook 'jv-irony/enable-irony-mode-if-server-found)

      (defun jv-irony/irony-mode-hook ()
        (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
        (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
      (add-hook 'irony-mode-hook 'jv-irony/irony-mode-hook)
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
    :config nil))

(defun jv-cpp-irony/init-company-irony ()
  (use-package company-irony
    :if
    (and
      (configuration-layer/layer-usedp 'auto-completion)
      (configuration-layer/package-usedp 'company))
    :defer t
    :commands company-irony
    :init
    (progn
      ;; (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))
      ;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      ;; (add-hook 'irony-mode-hook 'company-mode)
      (push 'company-irony company-backends-c-mode-common)
      (spacemacs|use-package-add-hook irony
        :post-config (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))
    :config nil))

(defun jv-cpp-irony/init-company-irony-c-headers ()
  (use-package company-irony-c-headers
    :if
    (and
      (configuration-layer/layer-usedp 'auto-completion)
      (configuration-layer/package-usedp 'company))
    :defer t
    :commands company-irony-c-headers
    :init
    (progn
      (push 'company-irony-c-headers company-backends-c-mode-common))))

(defun jv-cpp-irony/init-flycheck-irony ()
  (use-package flycheck-irony
    :if
    (and
      (configuration-layer/layer-usedp 'syntax-checking)
      (configuration-layer/package-usedp 'flycheck))
    :defer t
    :init
    (progn
      (add-hook 'irony-mode-hook 'flycheck-irony-setup)
      (eval-after-load 'flycheck '(add-to-list 'flycheck-checkers 'irony))
      (add-hook 'irony-mode-hook 'flycheck-mode))))

(defun jv-cpp-irony/init-irony-eldoc ()
  (use-package irony-eldoc
    :commands (irony-eldoc)
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc)))
