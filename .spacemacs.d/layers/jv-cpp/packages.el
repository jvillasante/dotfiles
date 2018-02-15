(defconst jv-cpp-packages
  '(cc-mode
     ;; semantic
     company
     irony
     company-irony
     company-irony-c-headers
     flycheck-irony
     irony-eldoc
     rtags))

(defun jv-cpp/post-init-cc-mode ()
  (add-hook 'c++-mode-hook
    (lambda () (progn
                 (setq-default flycheck-c/c++-clang-executable jv/clang-path)
                 (setq-default flycheck-clang-standard-library "libc++")
                 (setq-default flycheck-clang-language-standard "c++17")
                 (setq company-clang-arguments '("-std=c++17"))))))

;; (defun jv-cpp/post-init-semantic ()
;;   (with-eval-after-load 'semantic
;;     (setq semantic-default-submodes
;;       (remove 'global-semantic-stickyfunc-mode semantic-default-submodes))))

(defun jv-cpp/post-init-company ()
  (setq company-clang-executable jv/clang-path)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (setq company-backends (delete 'company-semantic company-backends)))

(defun jv-cpp/init-irony ()
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

(defun jv-cpp/init-company-irony ()
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

(defun jv-cpp/init-company-irony-c-headers ()
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

(defun jv-cpp/init-flycheck-irony ()
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

(defun jv-cpp/init-irony-eldoc ()
  (use-package irony-eldoc
    :commands (irony-eldoc)
    :init
    (add-hook 'irony-mode-hook 'irony-eldoc)))

(defun jv-cpp/init-rtags ()
  (use-package rtags
    :defer f
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "d" 'rtags-find-symbol-at-point
        "D" 'jv-cpp/rtags-find-symbol-at-point-other-file
        "/" 'rtags-find-symbol
        "R" 'rtags-find-references-at-point
        ;; "r" 'rtags-find-references-at-point-in-file to implement
        "v" 'rtags-find-virtuals-at-point
        "i" 'rtags-imenu
        "C-r" 'rtags-rename-symbol

        ;; print prefix
        "pt" 'rtags-print-class-hierarchy
        "pe" 'rtags-print-enum-value-at-point
        "pi" 'rtags-print-dependencies
        "ps" 'rtags-print-symbol-info
        "pp" 'rtags-preprocess-file

        ;; TODO: planned micro state
        ;; "o" (rtags-occurence-transient state)
        ;; "n" 'rtags-next-match
        ;; "N/p" 'rtags-previous-match
        )
      (add-hook 'rtags-jump-hook 'evil-set-jump)
      (add-to-list 'spacemacs-jump-handlers-c++-mode '(rtags-find-symbol-at-point :async t)))
    :config
    (progn
      (require 'rtags-helm)
      (setq rtags-jump-to-first-match nil)
      (setq rtags-use-helm t))))
